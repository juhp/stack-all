{-# LANGUAGE OverloadedStrings #-}

module Snapshots (
  getAllSnaps,
  latestSnapshot
  )
where

import Data.Aeson.Types
import Data.List.Extra
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import Network.HTTP.Query

import Types

excludedSnaps :: [Snapshot]
excludedSnaps = [LTS 15, LTS 7, LTS 3, LTS 0]

readSnapshots :: IO Object
readSnapshots =
  webAPIQuery "http://haddock.stackage.org/snapshots.json" []

-- FIXME cache json daily
getAllSnaps :: IO [Snapshot]
getAllSnaps = do
  obj <- readSnapshots
  return $ reverse . sort $ map (readSnap . T.unpack) (H.keys obj \\ ["lts"]) \\ excludedSnaps

latestSnapshot :: Snapshot -> IO (Maybe String)
latestSnapshot snap = do
  lookupKey (T.pack (showSnap snap)) <$> readSnapshots
