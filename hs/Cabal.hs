{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, ViewPatterns, TupleSections #-}
module Main (main, parseDotCabal, getComponentFromFile) where

-- base
import Control.Arrow
import Control.Monad (join)
import Data.Maybe (isJust, maybeToList)
import Data.Coerce
import Data.Aeson
import Data.List
import System.FilePath ((</>), normalise)

-- ghcjs
import GHCJS.Marshal
import GHCJS.Marshal.Pure
import GHCJS.Foreign.Callback
import GHCJS.Foreign.Callback.Internal
import GHCJS.Types
-- import GHCJS.Prim (JSException)

-- Cabal
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
-- import Distribution.Verbosity
import Distribution.Text
import Distribution.Package
import Distribution.ModuleName (toFilePath)

foreign import javascript safe "$1($2);"
  invokeCallback :: Callback (JSVal -> IO ()) -> JSVal -> IO ()

foreign import javascript unsafe "exports[$1] = $2;"
  setExport :: JSString -> Callback a -> IO ()

main :: IO ()
main = do
  setExport "parseDotCabalSync" =<< syncCallback1' parseDotCabal
  setExport "getComponentFromFileSync" =<< syncCallback2' getComponentFromFile
  setExport "parseDotCabal" =<< asyncCallback2 (mkAsync2 parseDotCabal)
  setExport "getComponentFromFile" =<< asyncCallback3 (mkAsync3 getComponentFromFile)

mkAsync2 :: (JSVal -> IO JSVal) -> JSVal -> JSVal -> IO()
mkAsync2 f a1 = coerce $ (f a1 >>=) . invokeCallback
mkAsync3 :: (JSVal -> JSVal -> IO JSVal) -> JSVal -> JSVal -> JSVal -> IO()
mkAsync3 f a1 a2 = coerce $ (f a1 a2 >>=) . invokeCallback

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
            check = or $
              either (any (`isPrefixOf` file) . genFP . toFilePath)
                     (elem file . genFP)
                `map` modules
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
