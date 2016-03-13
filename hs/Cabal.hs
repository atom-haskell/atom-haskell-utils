{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, ViewPatterns, TupleSections #-}
module Main (main) where

-- base
import Control.Arrow
import Control.Monad (join)
import Data.Maybe (isJust, maybeToList)
import Data.Coerce
import Data.Aeson
import qualified Data.Aeson.Types as JST
import Data.List
import System.FilePath ((</>), normalise)

-- ghcjs
import GHCJS.Marshal
import GHCJS.Marshal.Pure
import GHCJS.Foreign.Callback
import GHCJS.Foreign.Callback.Internal
import GHCJS.Types

-- Cabal
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
-- import Distribution.Verbosity
import Distribution.Text
import Distribution.Package
import Distribution.ModuleName (toFilePath)
import Distribution.Simple.PreProcess.Unlit (unlit)

-- parser
import qualified Language.Haskell.Exts.Parser as HSP
import qualified Language.Haskell.Exts.Syntax as S

toJsonOpts :: JST.Options
toJsonOpts = JST.defaultOptions {JST.sumEncoding=JST.ObjectWithSingleField}

instance ToJSON S.ModuleName where
  toJSON     = genericToJSON toJsonOpts
instance ToJSON S.ImportDecl where
  toJSON     = genericToJSON toJsonOpts
instance ToJSON S.ImportSpec where
  toJSON     = genericToJSON toJsonOpts
instance ToJSON S.Name where
  toJSON     = genericToJSON toJsonOpts
instance ToJSON S.CName where
  toJSON     = genericToJSON toJsonOpts
instance ToJSON S.Namespace where
  toJSON     = genericToJSON toJsonOpts
instance ToJSON S.SrcLoc where
  toJSON     = genericToJSON toJsonOpts

foreign import javascript safe "$1($2);"
  invokeCallback :: Callback (JSVal -> IO ()) -> JSVal -> IO ()

foreign import javascript safe "$1($2, $3);"
  invokeCallback2 :: Callback (JSVal -> JSVal -> IO ()) -> JSVal -> JSVal -> IO ()

foreign import javascript unsafe "exports[$1] = $2;"
  setExport :: JSString -> Callback a -> IO ()

main :: IO ()
main = do
  setExport "parseDotCabalSync" =<< syncCallback1' parseDotCabal
  setExport "getComponentFromFileSync" =<< syncCallback2' getComponentFromFile
  setExport "parseDotCabal" =<< asyncCallback2 (mkAsync2 parseDotCabal)
  setExport "getComponentFromFile" =<< asyncCallback3 (mkAsync3 getComponentFromFile)
  setExport "unlitSync" =<< syncCallback2' unlitSync
  setExport "unlit" =<< asyncCallback3 (coerce unlitAsync)
  setExport "parseHsModuleImports" =<< asyncCallback2 (mkAsync2 parseHsModuleImports)
  setExport "parseHsModuleImportsSync" =<< syncCallback1' parseHsModuleImports

mkAsync2 :: (JSVal -> IO JSVal) -> JSVal -> JSVal -> IO()
mkAsync2 f a1 = coerce $ (f a1 >>=) . invokeCallback
mkAsync3 :: (JSVal -> JSVal -> IO JSVal) -> JSVal -> JSVal -> JSVal -> IO()
mkAsync3 f a1 a2 = coerce $ (f a1 a2 >>=) . invokeCallback

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

parseHsModuleImports :: JSVal -> IO JSVal
parseHsModuleImports = toJSVal . enc . HSP.unNonGreedy . HSP.fromParseResult . HSP.parse . pFromJSVal
  where
    enc (HSP.ModuleHeadAndImports _pragma (name, _warning, _exports) imports) =
      object
        [ "name" .= name
        , "imports" .= imports
        ]
