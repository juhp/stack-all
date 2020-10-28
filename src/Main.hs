import Control.Monad.Extra
import Data.List.Extra
import SimpleCmd
import System.Directory
import System.FilePath

main :: IO ()
main = do
  unlessM (doesFileExist "stack.yaml") $
    error' "no stack.yaml found"
  configs <- filter isStackConf <$> listDirectory "."
  mapM_ (stackBuild configs) [Nightly, LTS 16, LTS 14, LTS 13, LTS 12]

isStackConf :: FilePath -> Bool
isStackConf f = "stack-" `isPrefixOf` f && "yaml" `isExtensionOf` f

-- FIXME allow specific snapshots?
-- FIXME lts latest
data Snapshot = Nightly | LTS Int

showSnap :: Snapshot -> String
showSnap Nightly = "nightly"
showSnap (LTS ver) = "lts-" ++ show ver

compactSnap :: Snapshot -> String
compactSnap Nightly = "nightly"
compactSnap (LTS ver) = "lts" ++ show ver

snapToConfig :: Snapshot -> FilePath
snapToConfig snap = "stack-" ++ compactSnap snap <.> "yaml"

-- configToSnap :: FilePath -> Snapshot
-- configToSnap f =
--   if isStackConf f then
--     let snap = takeBaseName $ dropPrefix "stack-" f
--     in if snap == "nightly" then Nightly
--        else if "lts" `isPrefixOf` snap then
--               let major = read (dropPrefix "lts" snap) in LTS major
--             else error' $ "malformed snapshot config file " ++ f
--   else error' ("malformed stack config filename " ++ f)

stackBuild :: [FilePath] -> Snapshot -> IO ()
stackBuild configs snap = do
  let resolver = showSnap snap
      config =
        case sort (filter (snapToConfig snap <=) configs) of
          [] -> []
          (cfg:_) -> ["--stack-yaml", cfg]
  cmd_ "stack" $ ["--resolver", resolver, "build"] ++ config
  putStrLn ""
