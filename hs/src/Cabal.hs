{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, ViewPatterns, TupleSections #-}
module Cabal where

-- base
import Control.Arrow
import Control.Monad (join)
import Data.Maybe (isJust, maybeToList)
import Data.Aeson
import Data.List
import System.FilePath ((</>), normalise)

-- ghcjs
import GHCJS.Marshal
import GHCJS.Marshal.Pure
import GHCJS.Foreign.Callback
import GHCJS.Types

-- Cabal
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
-- import Distribution.Verbosity
import Distribution.Text
import Distribution.Package
import Distribution.ModuleName (toFilePath)
import Distribution.Simple.PreProcess.Unlit (unlit)

import Foreign (invokeCallback2)

unlitSync :: JSVal -> JSVal -> IO JSVal
unlitSync (pFromJSVal -> file) (pFromJSVal -> src) =
  case unlit file src of
    Left plainsrc -> toJSVal plainsrc
    Right err -> toJSVal $ object ["error" .= err]

unlitAsync :: JSVal -> JSVal -> Callback (JSVal -> JSVal -> IO ()) -> IO ()
unlitAsync (pFromJSVal -> file) (pFromJSVal -> src) callback = do
  args <- case unlit file src of
            Left plainsrc -> (nullRef,) <$> toJSVal plainsrc
            Right err -> (,nullRef) <$> toJSVal err
  uncurry (invokeCallback2 callback) args

parseDotCabal :: JSVal -> IO JSVal
parseDotCabal
  (parsePackageDescription . pFromJSVal -> ParseOk _warnings gpkg)
  = do
    let pkg     = package (packageDescription gpkg)
        name    = display $ pkgName    pkg
        version = display $ pkgVersion pkg
        mkobj :: String -> String -> String -> Value
        mkobj p t n = object [
            "type" .= t
          , "name" .= n
          , "target" .= (p ++ n)
          ]
        mkobj' p t = mkobj p t . fst
        targets = concat [
            [mkobj "lib:" "library" name | isJust $ condLibrary gpkg]
          , mkobj' "exe:" "executable" `map` condExecutables gpkg
          , mkobj' "test:" "test-suite" `map` condTestSuites gpkg
          , mkobj' "bench:" "benchmark" `map` condBenchmarks gpkg
          ]

        descr = object [
            "name"    .= name
          , "version" .= version
          , "targets" .= targets
          ]

    toJSVal descr
parseDotCabal _ = return nullRef

getComponentFromFile :: JSVal -> JSVal -> IO JSVal
getComponentFromFile
  (parsePackageDescription . pFromJSVal -> ParseOk _warnings gpkg)
  (normalise . pFromJSVal -> file)
  = do
    let pkg     = package (packageDescription gpkg)
        name    = display $ pkgName    pkg
        lookupFile prefix fjoin ffirst finfo (name', tree) =
          let
            (modules, sourceDirs) =
                  ffirst &&& (finfo >>> otherModules &&& hsSourceDirs)
              >>> second fst &&& snd . snd -- assoc :: (a,(b,c)) -> ((a,b),c)
              >>> first (fjoin >>> uncurry (++))
              $   condTreeData tree
            check = any (
              either (any (`isPrefixOf` file) . genFP . toFilePath)
                     (elem file . genFP))
              modules
              where
                genFP f = map (normalise . (</> f)) sourceDirs
            in
              [prefix ++ name' | check]
        list = concat $ concat [
                  flip map (maybeToList $ condLibrary gpkg) $
                    lookupFile "lib:" (join (***) $ map Left) exposedModules libBuildInfo . (name,)
                , flip map (condExecutables gpkg) $
                    lookupFile "exe:" (return . Right *** map Left) modulePath buildInfo
                , flip map (condTestSuites gpkg) $
                    let
                      ti (TestSuiteExeV10 _version fp) = [Right fp]
                      ti (TestSuiteLibV09 _version mp) = [Left mp]
                      ti (TestSuiteUnsupported _) = []
                    in lookupFile "test:" (second (map Left)) (testInterface >>> ti) testBuildInfo
                , flip map (condBenchmarks gpkg) $
                    let
                      bi (BenchmarkExeV10 _version fp) = [Right fp]
                      bi (BenchmarkUnsupported _) = []
                    in lookupFile "bench:" (second (map Left)) (benchmarkInterface >>> bi) benchmarkBuildInfo
                ]
    toJSVal list
getComponentFromFile _ _ = return nullRef
