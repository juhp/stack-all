{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Extra
import Data.Ini.Config
import Data.List.Extra
import Data.Maybe
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

-- maybeReadSnap "lts16"
readCompactSnap :: String -> Maybe Snapshot
readCompactSnap "nightly" = Just Nightly
readCompactSnap snap =
  if "lts" `isPrefixOf` snap then
    case readMaybe (dropPrefix "lts" snap) of
      Just major -> Just (LTS major)
      Nothing -> error' $ "couldn't parse compact " ++ snap ++  " (expected ltsXX)"
  else Nothing

-- readSnap "lts-16"
readSnap :: String -> Snapshot
readSnap "nightly" = Nightly
readSnap snap =
  if "lts" `isPrefixOf` snap then
    case readMaybe (dropPrefix "lts-" snap) of
      Just major -> LTS major
      Nothing -> error' $ "couldn't parse " ++ snap ++ " (expected lts-XX)"
  else error' $ "malformed snapshot " ++ snap

showSnap :: Snapshot -> String
showSnap Nightly = "nightly"
showSnap (LTS ver) = "lts-" ++ show ver

defaultOldest :: Snapshot
defaultOldest = LTS 11

allSnaps :: [Snapshot]
allSnaps = [Nightly, LTS 17, LTS 16, LTS 14, LTS 13, LTS 12, LTS 11,
            LTS 10, LTS 9, LTS 8, LTS 6, LTS 5, LTS 4, LTS 2, LTS 1]

data VersionSpec = DefaultVersions | Oldest Snapshot | AllVersions | VersionList [Snapshot]

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  unlessM (doesFileExist "stack.yaml") $
    error' "no stack.yaml found"
  simpleCmdArgs (Just version) "Build over Stackage versions"
    "stack-all builds projects easily across different Stackage versions" $
    run <$>
    switchWith 'c' "create-config" "Create a project .stack-all file" <*>
    switchWith 'd' "debug" "Verbose stack build output on error" <*>
    optional (readSnap <$> strOptionWith 'n' "newest" "lts-MAJOR" "Newest LTS release to build from") <*>
    optional (strOptionWith 'C' "cmd" "COMMAND" "Specify a stack command [default: build]") <*>
    versionSpec
  where
    versionSpec =
      Oldest . readSnap <$> strOptionWith 'o' "oldest" "lts-MAJOR" "Oldest compatible LTS release" <|>
      VersionList . map readSnap <$> some (strArg "LTS") <|>
      flagWith DefaultVersions AllVersions 'a' "all-lts" "Try to build back to LTS 1 even"

run :: Bool -> Bool -> Maybe Snapshot -> Maybe String -> VersionSpec -> IO ()
run createConfig debug mnewest mcmd versionSpec = do
  if createConfig then
    case versionSpec of
      Oldest oldest -> createStackAll oldest
      _ -> error' "creating .stack-all requires --oldest LTS"
    else do
    versions <-
      case versionSpec of
        DefaultVersions -> do
          oldest <- fromMaybeM (return defaultOldest) readOldestLTS
          return $ case mnewest of
                     Just newest | newest < oldest -> filter (newest >=) allSnaps
                     _ -> filter (>= oldest) allSnaps
        AllVersions -> return allSnaps
        Oldest ver -> return $ filter (>= ver) allSnaps
        VersionList vers -> return vers
    configs <- mapMaybe readStackConf <$> listDirectory "."
    let newestFilter = maybe id (filter . (>=)) mnewest
    mapM_ (stackBuild configs debug mcmd) (newestFilter versions)
  where
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

stackBuild :: [Snapshot] -> Bool -> Maybe String -> Snapshot -> IO ()
stackBuild configs debug mcmd snap = do
  let command = maybe ["build"] words mcmd
      config =
        case sort (filter (snap <=) configs) of
          [] -> []
          (cfg:_) -> ["--stack-yaml", showConfig cfg]
      args = ["-v" | debug] ++ ["--resolver", showSnap snap] ++
             config ++ command
  if debug
    then debugBuild args
    else cmd_ "stack" args
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
