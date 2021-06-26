{-# LANGUAGE OverloadedStrings #-}

module Snapshots (
  getMajorVers,
  latestSnapshot
  )
where

import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List.Extra
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Network.HTTP.Query
import SimpleCmd (error')
import System.Directory
import System.Environment.XDG.BaseDir
import System.FilePath

import MajorVer

excludedMajors :: [MajorVer]
excludedMajors = [LTS 15, LTS 7, LTS 3, LTS 0]

getMajorVers :: IO [MajorVer]
getMajorVers = do
  obj <- getSnapshots
  return $ reverse . sort $ map (readMajor . T.unpack) (H.keys obj \\ ["lts"]) \\ excludedMajors

latestSnapshot :: MajorVer -> IO (Maybe String)
latestSnapshot ver = do
  lookupKey (T.pack (showMajor ver)) <$> getSnapshots

-- FIXME handle network failure
getSnapshots :: IO Object
getSnapshots = do
  file <- getUserCacheFile "stack-all" "snapshots.json"
  exists <- doesFileExist file
  unless exists $ do
    putStrLn $ "Creating " ++ file ++ " ..."
    createDirectoryIfMissing True (takeDirectory file)
  recent <- do
    if exists then do
      ts <- getModificationTime file
      t <- getCurrentTime
      -- < 3 hours
      return $ diffUTCTime t ts < 10000
      else return False
  if recent
    then do
    eobj <- eitherDecode <$> B.readFile file
    case eobj of
      Left err -> error' err
      Right obj -> return obj
    else do
    obj <- webAPIQuery "http://haddock.stackage.org/snapshots.json" []
    B.writeFile file $ encode obj
    return obj
