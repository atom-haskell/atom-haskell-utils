{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE OverloadedStrings, ViewPatterns, TupleSections #-}
module Main (main) where

-- base
import Data.Coerce

-- ghcjs
import GHCJS.Foreign.Callback
import GHCJS.Foreign.Callback.Internal

import Foreign
import Cabal
import Parser

main :: IO ()
main = do
  setExport "parseDotCabalSync" =<< syncCallback1' parseDotCabal
  setExport "getComponentFromFileSync" =<< syncCallback2' getComponentFromFile
  setExport "parseDotCabal" =<< asyncCallback2 (mkAsync2 parseDotCabal)
  setExport "getComponentFromFile" =<< asyncCallback3 (mkAsync3 getComponentFromFile)
  setExport "unlitSync" =<< syncCallback2' unlitSync
  setExport "unlit" =<< asyncCallback3 (coerce unlitAsync)
  setExport "parseHsModuleImports" =<< asyncCallback2 (mkAsync2 parseHsModuleImportsNoThrow)
  setExport "parseHsModuleImportsSync" =<< syncCallback1' parseHsModuleImports
