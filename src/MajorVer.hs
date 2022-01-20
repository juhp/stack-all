module MajorVer (
  MajorVer(..),
  eitherReadMajor,
  maybeReadMajor,
  readCompactMajor,
  readMajor,
  showMajor,
  showCompact
  )
where

import Control.Applicative
import Data.List.Extra
import SimpleCmd (error')
import Text.Read (readMaybe)

-- FIXME allow specific snapshots?
data MajorVer = LatestLTS | LTS Int | Nightly
  deriving (Eq, Ord)

maybeReadMajor :: String -> Maybe MajorVer
maybeReadMajor "nightly" = Just Nightly
maybeReadMajor "lts" = Just LatestLTS
maybeReadMajor ver =
  if "lts" `isPrefixOf` ver then
    case readMaybe (dropPrefix "lts-" ver) <|> readMaybe (dropPrefix "lts" ver) of
      Just major -> Just (LTS major)
      Nothing -> Nothing
  else Nothing

-- readMajor "lts-16"
readMajor :: String -> MajorVer
readMajor "nightly" = Nightly
readMajor "lts" = LatestLTS
readMajor ver =
  case maybeReadMajor ver of
    Just s -> s
    Nothing ->
      error' $! "couldn't parse " ++ ver ++ " (expected lts-XX or ltsXX)"

-- readCompactMajor "lts16"
readCompactMajor :: String -> Maybe MajorVer
readCompactMajor "nightly" = Just Nightly
readCompactMajor "lts" = Just LatestLTS
readCompactMajor ver =
  if "lts" `isPrefixOf` ver then
    case readMaybe (dropPrefix "lts" ver) of
      Just major -> Just (LTS major)
      Nothing -> error' $! "couldn't parse compact " ++ ver ++  " (expected ltsXX)"
  else Nothing

eitherReadMajor :: String -> Either String MajorVer
eitherReadMajor cs =
  case maybeReadMajor cs of
    Just s -> Right s
    _ -> Left cs

showMajor :: MajorVer -> String
showMajor Nightly = "nightly"
showMajor LatestLTS = "lts"
showMajor (LTS ver) = "lts-" ++ show ver

showCompact :: MajorVer -> String
showCompact = filter (/= '-') . showMajor
