{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Extra
import Data.Ini.Config
import Data.List.Extra
import qualified Data.Text.IO as T
import SimpleCmd
import System.Directory
import System.FilePath

-- FIXME allow specific snapshots?
-- FIXME lts latest
data Snapshot = LTS Int | Nightly
  deriving (Eq, Ord)

defaultSnaps :: [Snapshot]
defaultSnaps = [Nightly, LTS 16, LTS 14, LTS 13, LTS 12, LTS 11]

readSnap :: String -> Snapshot
readSnap "nightly" = Nightly
readSnap snap =
  if "lts" `isPrefixOf` snap then
    let major = read (dropPrefix "lts-" snap) in LTS major
  else error' $ "malformed snapshot " ++ snap

showSnap :: Snapshot -> String
showSnap Nightly = "nightly"
showSnap (LTS ver) = "lts-" ++ show ver

main :: IO ()
main = do
  unlessM (doesFileExist "stack.yaml") $
    error' "no stack.yaml found"
  configs <- filter isStackConf <$> listDirectory "."
  moldest <- getOldestLTS
  mapM_ (stackBuild configs) $
    case moldest of
      Just oldest -> filter (>= oldest) defaultSnaps
      Nothing -> defaultSnaps
  where
    isStackConf :: FilePath -> Bool
    isStackConf f = "stack-" `isPrefixOf` f && "yaml" `isExtensionOf` f

getOldestLTS :: IO (Maybe Snapshot)
getOldestLTS = do
  let configFile = ".stack-all"
  haveConfig <- doesFileExist configFile
  if haveConfig then
    Just . readSnap <$> readIniConfig configFile rcParser id
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
