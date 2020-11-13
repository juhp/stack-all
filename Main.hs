{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Extra
import Data.Ini.Config
import Data.List.Extra
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

readSnap :: String -> Snapshot
readSnap "nightly" = Nightly
readSnap snap =
  if "lts" `isPrefixOf` snap then
    let major = read (dropPrefix "lts-" snap) in LTS major
  else error' $ "malformed snapshot " ++ snap

showSnap :: Snapshot -> String
showSnap Nightly = "nightly"
showSnap (LTS ver) = "lts-" ++ show ver

defaultOldest :: Snapshot
defaultOldest = LTS 11

allSnaps :: [Snapshot]
allSnaps = [Nightly, LTS 16, LTS 14, LTS 13, LTS 12, LTS 11,
            LTS 10, LTS 9, LTS 8, LTS 6, LTS 5, LTS 4, LTS 2, LTS 1]

data VersionSpec = DefaultVersions | Oldest Snapshot | AllVersions | VersionList [Snapshot]

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  unlessM (doesFileExist "stack.yaml") $
    error' "no stack.yaml found"
  simpleCmdArgs (Just version) "Build over Stackage versions"
    "stack-all builds projects easily across different Stackage versions" $
    main' <$>
    switchWith 'c' "create-config" "Create a project .stack-all file" <*>
    switchWith 'd' "debug" "Verbose stack build output on error" <*>
    optional (strOptionWith 'C' "cmd" "COMMAND" "Specify a stack command [default: build]") <*>
    versionSpec
  where
    versionSpec =
      Oldest . readSnap <$> strOptionWith 'o' "oldest" "lts-MAJOR" "Oldest compatible LTS release" <|>
      VersionList . map readSnap <$> some (strArg "LTS") <|>
      flagWith DefaultVersions AllVersions 'a' "all-lts" "Try to build back to LTS 1 even"

main' :: Bool -> Bool -> Maybe String -> VersionSpec -> IO ()
main' createConfig debug mcmd versionSpec = do
  if createConfig then
    case versionSpec of
      Oldest oldest -> createStackAll oldest
      _ -> error' "creating .stack-all requires --oldest LTS"
    else do
    versions <-
      case versionSpec of
        DefaultVersions -> do
          oldest <- fromMaybeM (return defaultOldest) getOldestLTS
          return $ filter (>= oldest) allSnaps
        AllVersions -> return allSnaps
        Oldest ver -> return $ filter (>= ver) allSnaps
        VersionList vers -> return vers
    configs <- filter isStackConf <$> listDirectory "."
    mapM_ (stackBuild configs debug mcmd) versions
  where
    isStackConf :: FilePath -> Bool
    isStackConf f = "stack-" `isPrefixOf` f && "yaml" `isExtensionOf` f

stackAllFile :: FilePath
stackAllFile = ".stack-all"

createStackAll :: Snapshot -> IO ()
createStackAll snap = do
  exists <- doesFileExist stackAllFile
  if exists then error' $ stackAllFile ++ " already exists"
    else do
    writeFile stackAllFile $
      "[versions]\n# reason comment\noldest = " ++ showSnap snap ++ "\n"

getOldestLTS :: IO (Maybe Snapshot)
getOldestLTS = do
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

stackBuild :: [FilePath] -> Bool -> Maybe String -> Snapshot -> IO ()
stackBuild configs debug mcmd snap = do
  let command = maybe ["build"] words mcmd
      config =
        case sort (filter (snapConfig <=) configs) of
          [] -> []
          (cfg:_) -> ["--stack-yaml", cfg]
      args = ["-v" | debug] ++ ["--resolver", showSnap snap] ++
             config ++ command
  if debug
    then debugBuild args
    else cmd_ "stack" args
  putStrLn ""
  where
    snapConfig :: FilePath
    snapConfig = "stack-" ++ compactSnap snap <.> "yaml"
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
