{-# LANGUAGE OverloadedStrings #-}

module Snapshots (
  getMajorVers,
  latestSnapshot
  )
where

import Data.Aeson
import Data.List.Extra
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import Network.HTTP.Query
import System.Cached.JSON

import MajorVer

excludedMajors :: [MajorVer]
excludedMajors = [LTS 17, LTS 15, LTS 7, LTS 3, LTS 0]

getMajorVers :: IO [MajorVer]
getMajorVers = do
  obj <- getSnapshots
  return $ reverse . sort $ map (readMajor . T.unpack) (H.keys obj \\ ["lts"]) \\ excludedMajors

latestSnapshot :: MajorVer -> IO (Maybe String)
latestSnapshot ver = do
  lookupKey (T.pack (showMajor ver)) <$> getSnapshots

getSnapshots :: IO Object
getSnapshots =
  getCachedJSON "stackage-snapshots" "snapshots.json" "http://haddock.stackage.org/snapshots.json" 200
