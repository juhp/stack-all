{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Extra
import Data.Either
import Data.Ini.Config
import Data.List.Extra
import Data.Maybe
import Data.Tuple (swap)
import Text.Read (readMaybe)
import qualified Data.Text.IO as T
import SimpleCmd
import SimpleCmdArgs
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Process

import Paths_stack_all (version)

-- FIXME allow specific snapshots?
-- FIXME lts latest
data Snapshot = LTS Int | Nightly
  deriving (Eq, Ord)

-- readCompactSnap "lts16"
readCompactSnap :: String -> Maybe Snapshot
readCompactSnap "nightly" = Just Nightly
readCompactSnap snap =
  if "lts" `isPrefixOf` snap then
    case readMaybe (dropPrefix "lts" snap) of
      Just major -> Just (LTS major)
      Nothing -> error' $ "couldn't parse compact " ++ snap ++  " (expected ltsXX)"
  else Nothing

eitherReadSnap :: String -> Either String Snapshot
eitherReadSnap cs =
  case maybeReadSnap cs of
    Just s -> Right s
    _ -> Left cs

maybeReadSnap :: String -> Maybe Snapshot
maybeReadSnap "nightly" = Just Nightly
maybeReadSnap snap =
  if "lts" `isPrefixOf` snap then
    case readMaybe (dropPrefix "lts-" snap) <|> readMaybe (dropPrefix "lts" snap) of
      Just major -> Just (LTS major)
      Nothing -> Nothing
  else Nothing

-- readSnap "lts-16"
readSnap :: String -> Snapshot
readSnap "nightly" = Nightly
readSnap snap =
  case maybeReadSnap snap of
    Just s -> s
    Nothing ->
      error' $ "couldn't parse " ++ snap ++ " (expected lts-XX or ltsXX)"

showSnap :: Snapshot -> String
showSnap Nightly = "nightly"
showSnap (LTS ver) = "lts-" ++ show ver

defaultOldest :: Snapshot
defaultOldest = LTS 11

allSnaps :: [Snapshot]
allSnaps = [Nightly, LTS 17, LTS 16, LTS 14, LTS 13, LTS 12, LTS 11,
            LTS 10, LTS 9, LTS 8, LTS 6, LTS 5, LTS 4, LTS 2, LTS 1]

data VersionLimit = DefaultLimit | Oldest Snapshot | AllVersions

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  simpleCmdArgs' (Just version) "Build over Stackage versions"
    "stack-all builds projects easily across different Stackage versions" $
    run <$>
    switchWith 'c' "create-config" "Create a project .stack-all file" <*>
    switchWith 'd' "debug" "Verbose stack build output on error" <*>
    optional (readSnap <$> strOptionWith 'n' "newest" "lts-MAJOR" "Newest LTS release to build from") <*>
    (Oldest . readSnap <$> strOptionWith 'o' "oldest" "lts-MAJOR" "Oldest compatible LTS release" <|>
     flagWith DefaultLimit AllVersions 'a' "all-lts" "Try to build back to LTS 1 even") <*>
    many (strArg "SNAPSHOT... [COMMAND...]")

run :: Bool -> Bool -> Maybe Snapshot -> VersionLimit -> [String] -> IO ()
run createconfig debug mnewest verlimit verscmd = do
  findStackProjectDir Nothing
  if createconfig then
    case verlimit of
      Oldest oldest -> createStackAll oldest
      _ -> error' "creating .stack-all requires --oldest LTS"
    else do
    (versions, cs) <- getVersionsCmd
    configs <- mapMaybe readStackConf <$> listDirectory "."
    let newestFilter = maybe id (filter . (>=)) mnewest
    mapM_ (stackBuild configs debug cs) (newestFilter versions)
  where
    findStackProjectDir :: Maybe FilePath -> IO ()
    findStackProjectDir mcwd = do
      haveStackYaml <- doesFileExist "stack.yaml"
      if haveStackYaml
        then return ()
        else do
        cwdir <- getCurrentDirectory
        if cwdir /= "/"
          then setCurrentDirectory ".." >>
               findStackProjectDir (if isJust mcwd then mcwd else Just cwdir)
          else do
          putStrLn "stack.yaml not found"
          whenJust mcwd setCurrentDirectory
          haveCabalFile <- fileWithExtension_ "." ".cabal"
          if haveCabalFile
            then do
            unlessM (cmdBool "stack" ["init"]) $
              writeFile "stack.yaml" "resolver: lts-16.31\n"
            else error' "no package/project found"

    readStackConf :: FilePath -> Maybe Snapshot
    readStackConf "stack-lts.yaml" = error' "unversioned stack-lts.yaml is unsupported"
    readStackConf f =
      stripPrefix "stack-" f >>= stripSuffix ".yaml" >>= readCompactSnap

    getVersionsCmd :: IO ([Snapshot],[String])
    getVersionsCmd = do
      let partitionSnaps = swap . partitionEithers . map eitherReadSnap
          (verlist,cmds) = partitionSnaps verscmd
      versions <-
        if null verlist then
          case verlimit of
            DefaultLimit -> do
              oldest <- fromMaybeM (return defaultOldest) readOldestLTS
              return $ case mnewest of
                         Just newest ->
                           if newest < oldest
                           then filter (newest >=) allSnaps
                           else filter (\ s ->  s >= oldest && newest >= s) allSnaps
                         Nothing -> filter (>= oldest) allSnaps
            AllVersions -> return allSnaps
            Oldest ver -> return $ filter (>= ver) allSnaps
        else return verlist
      return (versions,if null cmds then ["build"] else cmds)

stackAllFile :: FilePath
stackAllFile = ".stack-all"

createStackAll :: Snapshot -> IO ()
createStackAll snap = do
  exists <- doesFileExist stackAllFile
  if exists then error' $ stackAllFile ++ " already exists"
    else do
    let older =
          let molder = listToMaybe $ dropWhile (>= snap) allSnaps
          in maybe "" (\s -> showSnap s ++ " too old") molder
    writeFile stackAllFile $
      "[versions]\n# " ++ older ++ "\noldest = " ++ showSnap snap ++ "\n"

readOldestLTS :: IO (Maybe Snapshot)
readOldestLTS = do
  haveConfig <- doesFileExist stackAllFile
  if haveConfig then
    Just . readSnap <$> readIniConfig stackAllFile rcParser id
    else return Nothing
  where
    rcParser :: IniParser String
    rcParser =
      section "versions" $
      fieldOf "oldest" string

    readIniConfig :: FilePath -> IniParser a -> (a -> b) -> IO b
    readIniConfig inifile iniparser fn = do
      ini <- T.readFile inifile
      return $ either error fn $ parseIniFile ini iniparser

stackBuild :: [Snapshot] -> Bool -> [String] -> Snapshot -> IO ()
stackBuild configs debug command snap = do
  let config =
        case sort (filter (snap <=) configs) of
          [] -> []
          (cfg:_) -> ["--stack-yaml", showConfig cfg]
      opts = ["-v" | debug] ++ ["--resolver", showSnap snap] ++
             config
  if debug
    then debugBuild $ opts ++ command
    else do
    ok <- cmdBool "stack" $ opts ++ command
    unless ok $ do
      putStr "\nsnapshot-pkg-db: "
      cmd_ "stack" $ "--silent" : opts ++ ["path", "--snapshot-pkg-db"]
      error' $ "failed for " ++ showSnap snap
  putStrLn ""
  where
    showConfig :: Snapshot -> FilePath
    showConfig sn = "stack-" ++ compactSnap sn <.> "yaml"
      where
        compactSnap :: Snapshot -> String
        compactSnap Nightly = "nightly"
        compactSnap (LTS ver) = "lts" ++ show ver

    debugBuild :: [String] -> IO ()
    debugBuild args = do
      putStr $ "stack " ++ unwords args
      (ret,out,err) <- readProcessWithExitCode "stack" args ""
      putStrLn "\n"
      unless (null out) $ putStrLn out
      unless (ret == ExitSuccess) $ do
        -- stack verbose includes info line with all stackages (> 500kbytes)
        mapM_ putStrLn $ filter ((<10000) . length) . lines $ err
        error' $ showSnap snap ++ " build failed"


-- taken from cabal-rpm FileUtils:

filesWithExtension :: FilePath -- directory
                   -> String   -- file extension
                   -> IO [FilePath]
filesWithExtension dir ext =
  filter (\ f -> takeExtension f == ext) <$> getDirectoryContents dir

-- looks in dir for a unique file with given extension
fileWithExtension :: FilePath -- directory
                  -> String   -- file extension
                  -> IO (Maybe FilePath)
fileWithExtension dir ext = do
  files <- filesWithExtension dir ext
  case files of
       [file] -> return $ Just $ dir </> file
       [] -> return Nothing
       _ -> putStrLn ("More than one " ++ ext ++ " file found!") >> return Nothing

-- looks in current dir for a unique file with given extension
fileWithExtension_ :: FilePath -> String -> IO Bool
fileWithExtension_ dir ext =
  isJust <$> fileWithExtension dir ext
