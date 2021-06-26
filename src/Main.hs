{-# LANGUAGE OverloadedStrings, CPP #-}

import Control.Monad.Extra
import Data.Either
import Data.Ini.Config
import Data.List.Extra
import Data.Maybe
import Data.Tuple (swap)
import SimpleCmd
import SimpleCmdArgs
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Process
import qualified Data.Text.IO as T

import MajorVer
import Paths_stack_all (version)
import Snapshots

defaultOldest :: MajorVer
defaultOldest = LTS 11

data VersionLimit = DefaultLimit | Oldest MajorVer | AllVersions

data Command = CreateConfig | MakeStackLTS | DefaultRun

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  simpleCmdArgs' (Just version) "Build over Stackage versions"
    "stack-all builds projects easily across different Stackage versions" $
    run <$>
    (flagWith' CreateConfig 'c' "create-config" "Create a project .stack-all file" <|>
     flagWith DefaultRun MakeStackLTS 's' "make-lts" "Create a stack-ltsXX.yaml file") <*>
    switchWith 'd' "debug" "Verbose stack build output on error" <*>
    optional (readMajor <$> strOptionWith 'n' "newest" "MAJOR" "Newest LTS release to build from") <*>
    (Oldest . readMajor <$> strOptionWith 'o' "oldest" "MAJOR" "Oldest compatible LTS release" <|>
     flagWith DefaultLimit AllVersions 'a' "all-lts" "Try to build back to LTS 1 even") <*>
    many (strArg "MAJORVER... [COMMAND...]")

run :: Command -> Bool -> Maybe MajorVer -> VersionLimit -> [String] -> IO ()
run command debug mnewest verlimit verscmd = do
  findStackProjectDir Nothing
  case command of
    CreateConfig ->
      case verlimit of
        Oldest oldest -> createStackAll oldest
        _ -> error' "creating .stack-all requires --oldest LTS"
    MakeStackLTS -> do
      (versions, _) <- getVersionsCmd
      if null versions
        then error' "--make-lts needs an LTS major version"
        else makeStackLTS versions
    DefaultRun -> do
      (versions, cargs) <- getVersionsCmd
      configs <- mapMaybe readStackConf <$> listDirectory "."
      let newestFilter = maybe id (filter . (>=)) mnewest
      mapM_ (stackBuild configs debug cargs) (newestFilter versions)
  where
    findStackProjectDir :: Maybe FilePath -> IO ()
    findStackProjectDir mcwd = do
      haveStackYaml <- doesFileExist "stack.yaml"
      if haveStackYaml
        then return ()
        else do
        cwdir <- getCurrentDirectory
        if cwdir /= "/"
          then setCurrentDirectory ".." >>
               findStackProjectDir (if isJust mcwd then mcwd else Just cwdir)
          else do
          putStrLn "stack.yaml not found"
          whenJust mcwd setCurrentDirectory
          haveCabalFile <- doesFileExistWithExtension "." ".cabal"
          if haveCabalFile
            then do
            -- FIXME take suggested extra-deps into stack.yaml
            -- FIXME stack init content too verbose
            unlessM (cmdBool "stack" ["init"]) $
              -- FIXME determine latest stable snapshot automatically
              writeFile "stack.yaml" "resolver: lts-17.15\n"
            else error' "no package/project found"

    getVersionsCmd :: IO ([MajorVer],[String])
    getVersionsCmd = do
      let partitionMajors = swap . partitionEithers . map eitherReadMajor
          (verlist,cmds) = partitionMajors verscmd
      allMajors <- getMajorVers
      versions <-
        if null verlist then
          case verlimit of
            DefaultLimit -> do
              oldest <- fromMaybeM (return defaultOldest) readOldestLTS
              return $ case mnewest of
                         Just newest ->
                           if newest < oldest
                           then filter (newest >=) allMajors
                           else filter (\ s ->  s >= oldest && newest >= s) allMajors
                         Nothing -> filter (>= oldest) allMajors
            AllVersions -> return allMajors
            Oldest ver -> return $ filter (>= ver) allMajors
        else return verlist
      return (versions,if null cmds then ["build"] else cmds)

readStackConf :: FilePath -> Maybe MajorVer
readStackConf "stack-lts.yaml" = error' "unversioned stack-lts.yaml is unsupported"
readStackConf f =
  stripPrefix "stack-" f >>= stripSuffix ".yaml" >>= readCompactMajor

stackAllFile :: FilePath
stackAllFile = ".stack-all"

createStackAll :: MajorVer -> IO ()
createStackAll ver = do
  exists <- doesFileExist stackAllFile
  if exists then error' $ stackAllFile ++ " already exists"
    else do
    allMajors <- getMajorVers
    let older =
          let molder = listToMaybe $ dropWhile (>= ver) allMajors
          in maybe "" (\s -> showMajor s ++ " too old") molder
    writeFile stackAllFile $
      "[versions]\n# " ++ older ++ "\noldest = " ++ showMajor ver ++ "\n"

readOldestLTS :: IO (Maybe MajorVer)
readOldestLTS = do
  haveConfig <- doesFileExist stackAllFile
  if haveConfig then
    Just . readMajor <$> readIniConfig stackAllFile rcParser id
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

makeStackLTS :: [MajorVer] -> IO ()
makeStackLTS vers = do
  configs <- mapMaybe readStackConf <$> listDirectory "."
  forM_ vers $ \ ver -> do
    if ver `elem` configs
      then error' $ showConfig ver ++ " already exists!"
      else do
      let mcurrentconfig =
            listToMaybe $ sort (filter (ver <=) configs)
      case mcurrentconfig of
        Nothing -> copyFile "stack.yaml" (showConfig ver)
        Just conf -> copyFile (showConfig conf) (showConfig ver)
      whenJustM (latestSnapshot ver) $ \latest ->
        cmd_ "sed" ["-i", "-e", "s/\\(resolver:\\) .*/\\1 " ++ latest ++ "/", showConfig ver]

showConfig :: MajorVer -> FilePath
showConfig sn = "stack-" ++ compactMajor sn <.> "yaml"
  where
    compactMajor :: MajorVer -> String
    compactMajor Nightly = "nightly"
    compactMajor (LTS ver) = "lts" ++ show ver

stackBuild :: [MajorVer] -> Bool -> [String] -> MajorVer -> IO ()
stackBuild configs debug command ver = do
  let config =
        case sort (filter (ver <=) configs) of
          [] -> []
          (cfg:_) -> ["--stack-yaml", showConfig cfg]
  latest <- latestSnapshot ver
  case latest of
    Nothing -> error' $ "no snapshot not found for " ++ showMajor ver
    Just minor -> do
      let opts = ["-v" | debug] ++ ["--resolver", minor] ++ config
      putStrLn $ "# " ++ minor
      if debug
        then debugBuild $ opts ++ command
        else do
        ok <- cmdBool "stack" $ opts ++ command
        unless ok $ do
          putStr "\nsnapshot-pkg-db: "
          cmd_ "stack" $ "--silent" : opts ++ ["path", "--snapshot-pkg-db"]
          error' $ "failed for " ++ showMajor ver
      putStrLn ""
  where
    debugBuild :: [String] -> IO ()
    debugBuild args = do
      putStr $ "stack " ++ unwords args
      (ret,out,err) <- readProcessWithExitCode "stack" args ""
      putStrLn "\n"
      unless (null out) $ putStrLn out
      unless (ret == ExitSuccess) $ do
        -- stack verbose includes info line with all stackages (> 500kbytes)
        mapM_ putStrLn $ filter ((<10000) . length) . lines $ err
        error' $ showMajor ver ++ " build failed"


-- taken from cabal-rpm FileUtils:

filesWithExtension :: FilePath -- directory
                   -> String   -- file extension
                   -> IO [FilePath]
filesWithExtension dir ext =
  filter (ext `isExtensionOf`) <$> listDirectory dir

-- looks in dir for a unique file with given extension
fileWithExtension :: FilePath -- directory
                  -> String   -- file extension
                  -> IO (Maybe FilePath)
fileWithExtension dir ext = do
  files <- filesWithExtension dir ext
  case files of
       [file] -> return $ Just $ dir </> file
       [] -> return Nothing
       _ -> putStrLn ("More than one " ++ ext ++ " file found!") >> return Nothing

-- looks in current dir for a unique file with given extension
doesFileExistWithExtension :: FilePath -> String -> IO Bool
doesFileExistWithExtension dir ext =
  isJust <$> fileWithExtension dir ext

#if !MIN_VERSION_filepath(1,4,2)
isExtensionOf :: String -> FilePath -> Bool
isExtensionOf ext@('.':_) = isSuffixOf ext . takeExtensions
isExtensionOf ext         = isSuffixOf ('.':ext) . takeExtensions
#endif
