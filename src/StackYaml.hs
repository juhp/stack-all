{-# LANGUAGE OverloadedStrings #-}

module StackYaml (readStackYaml)
where

import Control.Applicative ((<|>))
import Data.Yaml (decodeFileThrow)
import Network.HTTP.Query (lookupKey)

import MajorVer (MajorVer, snapMajorVer)

readStackYaml :: FilePath -> IO (Maybe MajorVer)
readStackYaml file = do
  yaml <- decodeFileThrow file
  return $ snapMajorVer <$>
    (lookupKey "resolver" yaml <|> lookupKey "snapshot" yaml)
