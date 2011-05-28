module Distribution.App
  (App(..),
   addAppHooks,
   ibtoolProgram
  )
  where

import Data.Maybe
import Distribution.Compat.Filesystem
import Distribution.Compat.ReadP
import Distribution.PackageDescription
import Distribution.ParseUtils
import Distribution.ReadE
import Distribution.Simple
import Distribution.Simple.Build
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Simple.Setup
import Distribution.Simple.Utils
import Distribution.System
import Distribution.Verbosity
import System.Directory
import System.FilePath (replaceExtension)


data App = App {
              appCHeaders :: [FilePath],
              appCSources :: [FilePath],
              appName :: String,
              appInfoPlist :: FilePath,
              appResourceDirectory :: Maybe FilePath,
              appXIBs :: [FilePath],
              appOtherResources :: [FilePath]
            }
            deriving (Show)


addAppHooks :: UserHooks -> UserHooks
addAppHooks userHooks =
  let oldHookedPrograms = hookedPrograms userHooks
      oldBuildHook = buildHook userHooks
  in userHooks {
         hookedPrograms = [touchProgram, ibtoolProgram] ++ oldHookedPrograms,
         buildHook = appBuildHook oldBuildHook
       }


touchProgram :: Program
touchProgram = simpleProgram "touch"


ibtoolProgram :: Program
ibtoolProgram = simpleProgram "ibtool"


appBuildHook :: (PackageDescription
                 -> LocalBuildInfo
                 -> UserHooks
                 -> BuildFlags
                 -> IO ())
             -> PackageDescription
             -> LocalBuildInfo
             -> UserHooks
             -> BuildFlags
             -> IO ()
appBuildHook oldBuildHook
             packageDescription
             localBuildInfo
             userHooks
             buildFlags = do
  oldBuildHook packageDescription localBuildInfo userHooks buildFlags
  if buildOS == OSX
    then withExeLBI packageDescription
                    localBuildInfo
                    (\executable componentLocalBuildInfo -> do
                       maybeApp <- getApp buildFlags executable
                       case maybeApp of
                         Nothing -> return ()
                         Just app -> buildApp buildFlags
                                              localBuildInfo
                                              executable
                                              app)
    else return ()


getApp :: BuildFlags -> Executable -> IO (Maybe App)
getApp buildFlags executable = do
  let verbosity = fromFlagOrDefault normal $ buildVerbosity buildFlags
      fields = customFieldsBI $ buildInfo executable
      getParse fieldName parser = do
        case lookup fieldName fields of
          Nothing -> return Nothing
          Just string -> do
            case [x | (x, "") <- readP_to_S parser string] of
              [result] -> return $ Just result
              [] -> fail $ "No parse of " ++ fieldName ++ "."
              _ -> fail $ "Ambiguous parse of " ++ fieldName ++ "."
      getListParse fieldName parser = do
        result <- getParse fieldName $ parseCommaList parser
        case result of
          Nothing -> return []
          Just items -> return items
  maybeAppInfoPlist <- getParse "x-app-info-plist" parseFilePathQ
  case maybeAppInfoPlist of
    Nothing -> return Nothing
    Just appInfoPlist -> do
      appCHeaders <- getListParse "x-app-c-headers" parseFilePathQ
      appCSources <- getListParse "x-app-c-sources" parseFilePathQ
      let appName = exeName executable
      appResourceDirectory <-
        getParse "x-app-resource-dir" parseFilePathQ
        >>= return . fmap pathCoerceToDirectory
      appXIBs <- getListParse "x-app-xibs" parseFilePathQ
      appOtherResources <- getListParse "x-app-other-resources" parseFilePathQ
      return $ Just App {
                        appCHeaders = appCHeaders,
                        appCSources = appCSources,
                        appName = appName,
                        appInfoPlist = appInfoPlist,
                        appResourceDirectory = appResourceDirectory,
                        appXIBs = appXIBs,
                        appOtherResources = appOtherResources
                      }


buildApp :: BuildFlags -> LocalBuildInfo -> Executable -> App -> IO ()
buildApp buildFlags localBuildInfo executable app = do
  let distPref = fromFlagOrDefault defaultDistPref $ buildDistPref buildFlags
      verbosity = fromFlagOrDefault normal $ buildVerbosity buildFlags
      appPath = distPref </> "build" </> (appName app ++ ".app")
      contentsPath = appPath </> "Contents"
      executableDirectoryPath = contentsPath </> "MacOS"
      resourcesPath = contentsPath </> "Resources"
  (touchConfiguredProgram, _) <-
    requireProgram verbosity touchProgram $ withPrograms localBuildInfo
  (ibtoolConfiguredProgram, _) <-
    requireProgram verbosity ibtoolProgram $ withPrograms localBuildInfo
  
  info verbosity $ "Building " ++ appName app ++ ".app bundle..."
  
  removeDirectoryRecursiveSilently appPath
  
  createDirectoryIfMissing False appPath
  writeFileAtomic (appPath </> "PkgInfo") "APPL????"
  
  createDirectoryIfMissing False contentsPath
  let infoPlistSource = appInfoPlist app
      infoPlistDestination = contentsPath </> "Info.plist"
  installOrdinaryFile verbosity infoPlistSource infoPlistDestination
  
  createDirectoryIfMissing False executableDirectoryPath
  let executableSource =
        buildDir localBuildInfo </> exeName executable </> exeName executable
      executableDestination =
        executableDirectoryPath </> appName app
  installExecutableFile verbosity executableSource executableDestination
  
  createDirectoryIfMissing False resourcesPath
  let sourceResourcePath relativePath =
        case appResourceDirectory app of
          Nothing -> relativePath
          Just resourceDirectory -> resourceDirectory </> relativePath
  mapM_ (\xib -> do
           let xibPath = sourceResourcePath xib
               nibPath = resourcesPath </> replaceExtension xib ".nib"
           runProgram verbosity
                      ibtoolConfiguredProgram
                      ["--warnings",
                       "--errors",
                       "--output-format=human-readable-text",
                       "--compile",
                       nibPath,
                       xibPath])
        $ appXIBs app
  mapM_ (\resource -> do
           let resourceSource = sourceResourcePath resource
               resourceDestination = resourcesPath </> resource
           installOrdinaryFile verbosity resourceSource resourceDestination)
        $ appOtherResources app
  
  runProgram verbosity touchConfiguredProgram [appPath]
