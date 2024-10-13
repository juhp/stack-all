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
import StackYaml (readStackYaml)

defaultOldestLTS :: MajorVer
defaultOldestLTS = LTS 18

data VersionLimit = DefaultLimit | Oldest MajorVer | AllVersions

data Command = CreateConfig
             | SetDefaultResolver MajorVer
             | UpdateDefaultResolver
             | MakeStackLTS MajorVer
             | RunCmd [String]
  deriving Eq

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  simpleCmdArgs' (Just version) "Build project over Stackage major versions"
    "https://github.com/juhp/stack-all#readme" $
    run
    <$> switchWith 'k' "keep-going" "Keep going even if an LTS fails"
    <*> switchWith 'D' "debug" "Verbose stack build output on error"
    <*> switchLongWith "refresh-cache" "Force refresh of stackage snapshots.json cache"
    <*> optional (readMajor <$> strOptionWith 'n' "newest" "MAJOR" "Newest LTS release to build from")
    <*> (Oldest . readMajor <$> strOptionWith 'o' "oldest" "MAJOR" "Oldest compatible LTS release" <|>
         flagWith DefaultLimit AllVersions 'a' "all-lts" "Try to build back to LTS 1 even")
    <*>
    (flagWith' CreateConfig 'c' "create-config" "Create a project .stack-all file" <|>
     (SetDefaultResolver . readMajor <$> strOptionWith 'd' "default-resolver" "MAJOR" ("Set" +-+ stackYaml +-+ "resolver")) <|>
     flagWith' UpdateDefaultResolver 'u' "update-resolver" ("Update" +-+ stackYaml +-+ "resolver") <|>
     (MakeStackLTS . readMajor <$> strOptionWith 's' "make-lts" "MAJOR" "Create a stack-ltsXX.yaml file") <|>
     RunCmd <$> many (strArg "MAJORVER... COMMAND..."))

stackYaml :: FilePath
stackYaml = "stack.yaml"

run :: Bool ->Bool -> Bool -> Maybe MajorVer -> VersionLimit -> Command
    -> IO ()
run keepgoing debug refresh mnewest verlimit com = do
  whenJustM findStackProjectDir setCurrentDirectory
  case com of
    CreateConfig ->
      case verlimit of
        Oldest oldest -> createStackAll (Just oldest) mnewest
        _ -> createStackAll Nothing mnewest
    SetDefaultResolver ver -> stackDefaultResolver $ Just ver
    UpdateDefaultResolver -> stackDefaultResolver Nothing
    MakeStackLTS ver -> makeStackLTS refresh ver
    RunCmd verscmd -> do
      (versions, cargs) <- getVersionsCmd verscmd
      configs <- readStackConfigs
      let newestFilter = maybe id (filter . (>=)) mnewest
      mapM_ (runStack configs keepgoing debug refresh $ if null cargs then ["build"] else cargs) (newestFilter versions)
  where
    findStackProjectDir :: IO (Maybe FilePath)
    findStackProjectDir = do
      haveStackYaml <- doesFileExist stackYaml
      cwdir <- getCurrentDirectory
      if haveStackYaml
        then return $ Just cwdir
        else do
        haveCabalPackageFile <-
          doesFileExistWithExtension "." ".cabal" ||^
          doesFileExist "package.yaml" ||^
          doesFileExist "cabal.project"
        if haveCabalPackageFile
          then do
          putStrLn $ "creating" +-+ stackYaml +-+ "in" +-+ cwdir
          -- FIXME take suggested extra-deps into stack.yaml
          -- FIXME stack init content too verbose
          unlessM (cmdBool "stack" ["init"]) $ do
            snap <- latestLtsSnapshot refresh
            writeFile stackYaml $ "resolver: " ++ snap ++ "\n"
          return $ Just cwdir
          else
          if cwdir /= "/"
          then withCurrentDirectory ".." findStackProjectDir
          else do
          putStrLn "no package/project found"
          return Nothing

    getVersionsCmd :: [String] -> IO ([MajorVer],[String])
    getVersionsCmd verscmd = do
      let partitionMajors = swap . partitionEithers . map eitherReadMajorAlias
          (verlist,cmds) = partitionMajors verscmd
      allMajors <- getMajorVers
      versions <-
        if null verlist then
          case verlimit of
            DefaultLimit -> do
              (newestLTS, oldestLTS) <- readNewestOldestLTS
              return $ case mnewest of
                         Just newest ->
                           if newest < oldestLTS
                           then filter (inRange newest (LTS 1)) allMajors
                           else filter (inRange newest oldestLTS) allMajors
                         Nothing -> filter (inRange newestLTS oldestLTS) allMajors
            AllVersions -> return allMajors
            Oldest ver -> return $ filter (inRange Nightly ver) allMajors
        else nub <$> mapM resolveMajor verlist
      return (versions,cmds)
      where
        inRange :: MajorVer -> MajorVer -> MajorVer -> Bool
        inRange newest oldest v = v >= oldest && v <= newest

readStackConfigs :: IO [MajorVer]
readStackConfigs = do
  sort . mapMaybe readStackConf <$> listDirectory "."
  where
    readStackConf :: FilePath -> Maybe MajorVer
    readStackConf "stack-lts.yaml" =
      error' "unversioned stack-lts.yaml is unsupported"
    readStackConf f =
      stripPrefix "stack-" f >>= stripSuffix ".yaml" >>= readCompactMajor

readNewestOldestLTS :: IO (MajorVer,MajorVer)
readNewestOldestLTS = do
  haveConfig <- doesFileExist stackAllFile
  if haveConfig then
    readIniConfig stackAllFile $
    section "versions" $ do
    mnewest <- fmap readMajor <$> fieldMbOf "newest" string
    moldest <- fmap readMajor <$> fieldMbOf "oldest" string
    return (fromMaybe Nightly mnewest, fromMaybe defaultOldestLTS moldest)
    else return (Nightly, defaultOldestLTS)
  where
    readIniConfig :: FilePath -> IniParser a -> IO a
    readIniConfig inifile iniparser = do
      ini <- T.readFile inifile
      return $ either error id $ parseIniFile ini iniparser

stackAllFile :: FilePath
stackAllFile = ".stack-all"

createStackAll :: Maybe MajorVer -> Maybe MajorVer -> IO ()
createStackAll Nothing Nothing =
  error' "creating .stack-all requires --oldest LTS and/or --newest LTS"
createStackAll moldest mnewest = do
  exists <- doesFileExist stackAllFile
  if exists then error' $ stackAllFile ++ " already exists"
    else do
    allMajors <- getMajorVers
    writeFile stackAllFile $
      "[versions]\n" ++
      case mnewest of
        Nothing -> ""
        Just newest ->
          "newest = " ++ showMajor newest ++ "\n"
      ++
      case moldest of
        Nothing -> ""
        Just oldest ->
          let older =
                let molder = listToMaybe $ dropWhile (>= oldest) allMajors
                in maybe "" (\s -> showMajor s ++ " too old") molder
          in "# " ++ older ++ "\noldest = " ++ showMajor oldest ++ "\n"

stackDefaultResolver :: Maybe MajorVer -> IO ()
stackDefaultResolver mver = do
  unlessM (doesFileExist stackYaml) $
    error' $ "no" +-+ stackYaml +-+ "present"
  case mver of
    Nothing -> do
      mdef <- readStackYaml stackYaml
      case mdef of
        Nothing -> error' $ "could not determine major version of" +-+ stackYaml
        Just ver -> stackDefaultResolver $ Just ver
    Just ver ->
      whenJustM (latestMajorSnapshot False ver) $ \latest ->
      cmd_ "sed" ["-i", "-e", "s/\\(resolver:\\) .*/\\1 " ++ latest ++ "/", stackYaml]

makeStackLTS :: Bool -> MajorVer -> IO ()
makeStackLTS refresh ver = do
  configs <- readStackConfigs
  let newfile = configFile ver
  if ver `elem` configs
    then do
    error' $ newfile ++ " already exists!"
    else do
    let mcurrentconfig = find (ver <=) (delete Nightly configs)
    case mcurrentconfig of
      Nothing -> copyFile stackYaml newfile
      Just conf -> do
        let origfile = configFile conf
        copyFile origfile newfile
    whenJustM (latestMajorSnapshot refresh ver) $ \latest ->
      cmd_ "sed" ["-i", "-e", "s/\\(resolver:\\) .*/\\1 " ++ latest ++ "/", newfile]

configFile :: MajorVer -> FilePath
configFile ver = "stack-" ++ showCompact ver <.> "yaml"

runStack :: [MajorVer] -> Bool -> Bool -> Bool -> [String]
           -> MajorVer -> IO ()
runStack configs keepgoing debug refresh command ver = do
  let mcfgver =
        case ver of
          Nightly | Nightly `elem` configs -> Just Nightly
          _ ->
            case sort (filter (ver <=) (delete Nightly configs)) of
              [] -> Nothing
              (cfg:_) -> Just cfg
  latest <- latestMajorSnapshot refresh ver
  case latest of
    Nothing -> error' $ "no snapshot not found for " ++ showMajor ver
    Just minor -> do
      let opts = ["-v" | debug] ++ ["--resolver", minor] ++
                 maybe [] (\f -> ["--stack-yaml", configFile f]) mcfgver
      putStrLn $ "# " ++ minor
      if debug
        then debugBuild $ opts ++ command
        else do
        ok <- cmdBool "stack" $ opts ++ command
        unless (ok || keepgoing) $ do
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


#if !MIN_VERSION_simple_cmd(0,2,4)
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
#endif

-- looks in current dir for a unique file with given extension
doesFileExistWithExtension :: FilePath -> String -> IO Bool
doesFileExistWithExtension dir ext = do
  mf <- fileWithExtension dir ext
  case mf of
    Nothing -> return False
    Just f -> doesFileExist f

#if !MIN_VERSION_filepath(1,4,2)
isExtensionOf :: String -> FilePath -> Bool
isExtensionOf ext@('.':_) = isSuffixOf ext . takeExtensions
isExtensionOf ext         = isSuffixOf ('.':ext) . takeExtensions
#endif
