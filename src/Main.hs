{-# LANGUAGE OverloadedStrings, CPP #-}

import Control.Monad.Extra
import Data.Either
import Data.Ini.Config
import Data.List.Extra
import Data.Maybe
import Data.Tuple (swap)
import SimpleCmd
import SimpleCmdArgs
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Process
import qualified Data.Text.IO as T

import Paths_stack_all (version)
import Snapshots
import Types

eitherReadSnap :: String -> Either String Snapshot
eitherReadSnap cs =
  case maybeReadSnap cs of
    Just s -> Right s
    _ -> Left cs

defaultOldest :: Snapshot
defaultOldest = LTS 11

data VersionLimit = DefaultLimit | Oldest Snapshot | AllVersions

data Command = CreateConfig | MakeStackLTS | DefaultRun

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  simpleCmdArgs' (Just version) "Build over Stackage versions"
    "stack-all builds projects easily across different Stackage versions" $
    run <$>
    (flagWith' CreateConfig 'c' "create-config" "Create a project .stack-all file" <|>
     flagWith DefaultRun MakeStackLTS 's' "make-lts" "Create a stack-ltsXX.yaml file") <*>
    switchWith 'd' "debug" "Verbose stack build output on error" <*>
    optional (readSnap <$> strOptionWith 'n' "newest" "lts-MAJOR" "Newest LTS release to build from") <*>
    (Oldest . readSnap <$> strOptionWith 'o' "oldest" "lts-MAJOR" "Oldest compatible LTS release" <|>
     flagWith DefaultLimit AllVersions 'a' "all-lts" "Try to build back to LTS 1 even") <*>
    many (strArg "SNAPSHOT... [COMMAND...]")

run :: Command -> Bool -> Maybe Snapshot -> VersionLimit -> [String] -> IO ()
run command debug mnewest verlimit verscmd = do
  findStackProjectDir Nothing
  case command of
    CreateConfig ->
      case verlimit of
        Oldest oldest -> createStackAll oldest
        _ -> error' "creating .stack-all requires --oldest LTS"
    MakeStackLTS -> do
      (versions, _) <- getVersionsCmd
      if null versions
        then error' "--make-lts needs an LTS major version"
        else makeStackLTS versions
    DefaultRun -> do
      (versions, cargs) <- getVersionsCmd
      configs <- mapMaybe readStackConf <$> listDirectory "."
      let newestFilter = maybe id (filter . (>=)) mnewest
      mapM_ (stackBuild configs debug cargs) (newestFilter versions)
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
          haveCabalFile <- doesFileExistWithExtension "." ".cabal"
          if haveCabalFile
            then do
            -- FIXME take suggested extra-deps into stack.yaml
            -- FIXME stack init content too verbose
            unlessM (cmdBool "stack" ["init"]) $
              writeFile "stack.yaml" "resolver: lts-16.31\n"
            else error' "no package/project found"

    getVersionsCmd :: IO ([Snapshot],[String])
    getVersionsCmd = do
      let partitionSnaps = swap . partitionEithers . map eitherReadSnap
          (verlist,cmds) = partitionSnaps verscmd
      allSnaps <- getAllSnaps
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

readStackConf :: FilePath -> Maybe Snapshot
readStackConf "stack-lts.yaml" = error' "unversioned stack-lts.yaml is unsupported"
readStackConf f =
  stripPrefix "stack-" f >>= stripSuffix ".yaml" >>= readCompactSnap

stackAllFile :: FilePath
stackAllFile = ".stack-all"

createStackAll :: Snapshot -> IO ()
createStackAll snap = do
  exists <- doesFileExist stackAllFile
  if exists then error' $ stackAllFile ++ " already exists"
    else do
    allSnaps <- getAllSnaps
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

makeStackLTS :: [Snapshot] -> IO ()
makeStackLTS snaps = do
  configs <- mapMaybe readStackConf <$> listDirectory "."
  forM_ snaps $ \ snap -> do
    if snap `elem` configs
      then error' $ showConfig snap ++ " already exists!"
      else do
      let mcurrentconfig =
            listToMaybe $ sort (filter (snap <=) configs)
      case mcurrentconfig of
        Nothing -> copyFile "stack.yaml" (showConfig snap)
        Just conf -> copyFile (showConfig conf) (showConfig snap)
      whenJustM (latestSnapshot snap) $ \latest ->
        cmd_ "sed" ["-i", "-e", "s/\\(resolver:\\) .*/\\1 " ++ latest ++ "/", showConfig snap]

showConfig :: Snapshot -> FilePath
showConfig sn = "stack-" ++ compactSnap sn <.> "yaml"
  where
    compactSnap :: Snapshot -> String
    compactSnap Nightly = "nightly"
    compactSnap (LTS ver) = "lts" ++ show ver

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
  filter (ext `isExtensionOf`) <$> listDirectory dir

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
doesFileExistWithExtension :: FilePath -> String -> IO Bool
doesFileExistWithExtension dir ext =
  isJust <$> fileWithExtension dir ext

#if !MIN_VERSION_filepath(1,4,2)
isExtensionOf :: String -> FilePath -> Bool
isExtensionOf ext@('.':_) = isSuffixOf ext . takeExtensions
isExtensionOf ext         = isSuffixOf ('.':ext) . takeExtensions
#endif
