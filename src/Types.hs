module Types (
  Snapshot(..),
  maybeReadSnap,
  readCompactSnap,
  readSnap,
  showSnap
  )
where

import Control.Applicative
import Data.List.Extra
import SimpleCmd (error')
import Text.Read (readMaybe)

-- FIXME allow specific snapshots?
-- FIXME lts latest
data Snapshot = LTS Int | Nightly
  deriving (Eq, Ord)

maybeReadSnap :: String -> Maybe Snapshot
maybeReadSnap "nightly" = Just Nightly
maybeReadSnap snap =
  if "lts" `isPrefixOf` snap then
    case readMaybe (dropPrefix "lts-" snap) <|> readMaybe (dropPrefix "lts" snap) of
      Just major -> Just (LTS major)
      Nothing -> Nothing
  else Nothing

-- readSnap "lts-16"
readSnap :: String -> Snapshot
readSnap "nightly" = Nightly
readSnap snap =
  case maybeReadSnap snap of
    Just s -> s
    Nothing ->
      error' $ "couldn't parse " ++ snap ++ " (expected lts-XX or ltsXX)"

-- readCompactSnap "lts16"
readCompactSnap :: String -> Maybe Snapshot
readCompactSnap "nightly" = Just Nightly
readCompactSnap snap =
  if "lts" `isPrefixOf` snap then
    case readMaybe (dropPrefix "lts" snap) of
      Just major -> Just (LTS major)
      Nothing -> error' $ "couldn't parse compact " ++ snap ++  " (expected ltsXX)"
  else Nothing

showSnap :: Snapshot -> String
showSnap Nightly = "nightly"
showSnap (LTS ver) = "lts-" ++ show ver
