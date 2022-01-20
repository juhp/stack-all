{-# LANGUAGE CPP, OverloadedStrings #-}

module Snapshots (
  getMajorVers,
  latestSnapshot,
  latestLTS,
  latestLtsSnapshot,
  resolveMajor
  )
where

import Data.Aeson
import Data.List.Extra
#if MIN_VERSION_aeson(2,0,0)
import Data.Aeson.Key (toText)
import qualified Data.Aeson.KeyMap as M
#else
import qualified Data.HashMap.Lazy as M
#endif
import qualified Data.Text as T
import Network.HTTP.Query
import SimpleCmd (error')
import System.Cached.JSON

import MajorVer

excludedMajors :: [MajorVer]
excludedMajors = [LTS 17, LTS 15, LTS 7, LTS 3, LTS 0]

getMajorVers :: IO [MajorVer]
getMajorVers = do
  obj <- getSnapshots
  return $ reverse . sort $ map (readMajor . T.unpack . toText) (M.keys obj \\ ["lts"]) \\ excludedMajors
#if !MIN_VERSION_aeson(2,0,0)
  where toText = id
#endif

latestSnapshot :: MajorVer -> IO (Maybe String)
latestSnapshot ver = do
  lookupKey (T.pack (showMajor ver)) <$> getSnapshots

getSnapshots :: IO Object
getSnapshots =
  getCachedJSON "stackage-snapshots" "snapshots.json" "http://haddock.stackage.org/snapshots.json" 200

latestLTS :: IO MajorVer
latestLTS = do
  msnap <- latestSnapshot LatestLTS
  case msnap of
    Nothing ->
      error' "failed to determine latest lts snapshot"
    Just snap -> case breakOn "." snap of
      ("",_) -> error' $ "bad snapshot " ++ snap
      (lts,_minor) -> return $ readMajor lts

latestLtsSnapshot :: IO String
latestLtsSnapshot = do
  msnap <- latestSnapshot LatestLTS
  case msnap of
    Nothing ->
      error' "failed to determine latest lts snapshot"
    Just snap -> return snap

-- converts 'lts' to actual version
resolveMajor :: MajorVer -> IO MajorVer
resolveMajor ver =
  if ver == LatestLTS then latestLTS else return ver
