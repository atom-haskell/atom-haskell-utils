{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
module Main (main, parseDotCabal, getComponentFromFile) where

-- base
import Data.Aeson
import Data.List
import System.FilePath ((</>), normalise)

-- ghcjs
import GHCJS.Marshal
import GHCJS.Marshal.Pure
import GHCJS.Foreign.Callback
import GHCJS.Types
-- import GHCJS.Prim (JSException)

-- Cabal
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
-- import Distribution.Verbosity
import Distribution.Text
import Distribution.Package
import Distribution.ModuleName (toFilePath, ModuleName)

foreign import javascript safe "$1($2);"
  invokeCallback :: Callback (JSVal -> IO ()) -> JSVal -> IO ()

main :: IO ()
main = putStrLn "Dummy main"

parseDotCabal :: JSVal -> Callback (JSVal -> IO ()) -> IO ()
parseDotCabal (pFromJSVal -> input) callback =
    case parsePackageDescription input of
      ParseFailed _err ->
        invokeCallback callback nullRef
      ParseOk _warnings gpkg -> do
        let pkg     = package (packageDescription gpkg)
            name    = display $ pkgName    pkg
            version = display $ pkgVersion pkg

        let --targets :: [PureJSRef (JSRef ())]
            targets = concat [
                case condLibrary gpkg of
                  Nothing -> []
                  Just _  -> [object [
                      "type" .= ("library" :: String)
                    , "name" .= name
                    , "target" .= ("lib:" ++ name)
                    ]]
              , flip map (condExecutables gpkg) $ \(exe, _) -> object [
                      "type"   .= ("executable" :: String)
                    , "name"   .= exe
                    , "target" .= ("exe:" ++ exe)
                    ]
              , flip map (condTestSuites gpkg) $ \(test, _) -> object [
                      "type"   .= ("test-suite" :: String)
                    , "name"   .= test
                    , "target" .= ("test:" ++ test)
                    ]
              , flip map (condBenchmarks gpkg) $ \(bench, _) -> object [
                      "type"   .= ("benchmark" :: String)
                    , "name"   .= bench
                    , "target" .= ("bench:" ++ bench)
                    ]
              ]

            descr = object [
                "name"    .= name
              , "version" .= version
              , "targets" .= targets
              ]

        invokeCallback callback =<< toJSVal descr

lookupFile :: FilePath -> [FilePath] -> [ModuleName] -> [FilePath] -> Bool
lookupFile file sourceDirs modules files =
     nf `elem` concatMap genFP files
  || (`isPrefixOf` nf) `any` concatMap (genFP . toFilePath) modules
  where
    genFP = (`map` sourceDirs) . (normalise .) . flip (</>)
    nf = normalise file

getComponentFromFile :: JSVal -> JSVal -> Callback (JSVal -> IO ()) -> IO ()
getComponentFromFile (pFromJSVal -> input) (pFromJSVal -> file) callback =
    case parsePackageDescription input of
      ParseFailed _err ->
        invokeCallback callback nullRef
      ParseOk _warnings gpkg -> do
        let pkg     = package (packageDescription gpkg)
            name    = display $ pkgName    pkg

        let list = concat $ concat [
                      case condLibrary gpkg of
                        Nothing -> []
                        Just lib  -> let
                            exposedModules' = exposedModules $ condTreeData lib
                            otherModules' = otherModules $ libBuildInfo $ condTreeData lib
                            sourceDirs' = hsSourceDirs $ libBuildInfo $ condTreeData lib
                            in
                              [["lib:" ++ name | lookupFile file sourceDirs' (exposedModules' ++ otherModules') []]]
                    , flip map (condExecutables gpkg) $ \(exe, info) ->
                        let
                          modulePath' = modulePath $ condTreeData info
                          otherModules' = otherModules $ buildInfo $ condTreeData info
                          sourceDirs' = hsSourceDirs $ buildInfo $ condTreeData info
                        in
                          [ "exe:" ++ exe | lookupFile file sourceDirs' otherModules' [modulePath']]
                    , flip map (condTestSuites gpkg) $ \(test, info) ->
                        let
                          modulePaths' =
                            case testInterface $ condTreeData info of
                              TestSuiteExeV10 _version fp -> [fp]
                              _ -> []
                          mainModules' =
                            case testInterface $ condTreeData info of
                              TestSuiteLibV09 _version mp -> [mp]
                              _ -> []
                          otherModules' = otherModules $ testBuildInfo $ condTreeData info
                          sourceDirs' = hsSourceDirs $ testBuildInfo $ condTreeData info
                        in
                          [ "test:" ++ test | lookupFile file sourceDirs' (mainModules' ++ otherModules') modulePaths']
                    , flip map (condBenchmarks gpkg) $ \(bench, info) ->
                        let
                          modulePaths' =
                            case benchmarkInterface $ condTreeData info of
                              BenchmarkExeV10 _version fp -> [fp]
                              _ -> []
                          otherModules' = otherModules $ benchmarkBuildInfo $ condTreeData info
                          sourceDirs' = hsSourceDirs $ benchmarkBuildInfo $ condTreeData info
                        in
                          [ "bench:" ++ bench | lookupFile file sourceDirs' otherModules' modulePaths']
                    ]
        invokeCallback callback =<< toJSVal list
