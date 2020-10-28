{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Extra
import Data.Ini.Config
import Data.List.Extra
import qualified Data.Text.IO as T
import SimpleCmd
import SimpleCmdArgs
import System.Directory
import System.FilePath

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

defaultSnaps, allSnaps :: [Snapshot]
defaultSnaps = [Nightly, LTS 16, LTS 14, LTS 13, LTS 12, LTS 11]

allSnaps = defaultSnaps ++
           [LTS 10, LTS 9, LTS 8, LTS 6, LTS 5, LTS 4, LTS 2, LTS 1]

main :: IO ()
main = do
  unlessM (doesFileExist "stack.yaml") $
    error' "no stack.yaml found"
  simpleCmdArgs (Just version) "Build over Stackage versions"
    "stack-all builds projects easily across different Stackage versions" $
    main' <$>
    switchWith 'c' "create-config" "Create a project .stack-all file" <*>
    optional (readSnap <$> strOptionWith 'o' "oldest" "lts-MAJOR" "Oldest compatible LTS release") <*>
    switchWith 'a' "all-lts" "Try to build back to LTS 1 even"

main' :: Bool -> Maybe Snapshot -> Bool -> IO ()
main' createConfig moldest allLTS = do
  configs <- filter isStackConf <$> listDirectory "."
  if createConfig then createStackAll moldest
    else do
    moldestLTS <- maybe getOldestLTS (return . Just) moldest
    mapM_ (stackBuild configs) $
      case moldestLTS of
        Just oldest ->
          filter (>= oldest) (if allLTS then allSnaps else defaultSnaps)
        Nothing -> defaultSnaps
  where
    isStackConf :: FilePath -> Bool
    isStackConf f = "stack-" `isPrefixOf` f && "yaml" `isExtensionOf` f

stackAllFile :: FilePath
stackAllFile = ".stack-all"

createStackAll :: Maybe Snapshot -> IO ()
createStackAll Nothing = error' "creating .stack-all requires --oldest LTS"
createStackAll (Just snap) = do
  exists <- doesFileExist stackAllFile
  if exists then error' $ stackAllFile ++ " already exists"
    else do
    writeFile stackAllFile $
      "[versions]\noldest = " ++ showSnap snap ++ "\n"

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

stackBuild :: [FilePath] -> Snapshot -> IO ()
stackBuild configs snap = do
  let resolver = showSnap snap
      config =
        case sort (filter (snapConfig <=) configs) of
          [] -> []
          (cfg:_) -> ["--stack-yaml", cfg]
  cmd_ "stack" $ ["--resolver", resolver, "build"] ++ config
  putStrLn ""
  where
    snapConfig :: FilePath
    snapConfig = "stack-" ++ compactSnap snap <.> "yaml"
      where
        compactSnap :: Snapshot -> String
        compactSnap Nightly = "nightly"
        compactSnap (LTS ver) = "lts" ++ show ver
