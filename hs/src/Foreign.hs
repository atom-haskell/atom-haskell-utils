{-# OPTIONS_GHC -Wall #-}
module Foreign where

import Data.Coerce
import GHCJS.Foreign.Callback
import GHCJS.Foreign.Callback.Internal
import GHCJS.Types

foreign import javascript safe "$1($2);"
  invokeCallback :: Callback (JSVal -> IO ()) -> JSVal -> IO ()

foreign import javascript safe "$1($2, $3);"
  invokeCallback2 :: Callback (JSVal -> JSVal -> IO ()) -> JSVal -> JSVal -> IO ()

foreign import javascript unsafe "exports[$1] = $2;"
  setExport :: JSString -> Callback a -> IO ()

mkAsync2 :: (JSVal -> IO JSVal) -> JSVal -> JSVal -> IO()
mkAsync2 f a1 = coerce $ (f a1 >>=) . invokeCallback
mkAsync3 :: (JSVal -> JSVal -> IO JSVal) -> JSVal -> JSVal -> JSVal -> IO()
mkAsync3 f a1 a2 = coerce $ (f a1 a2 >>=) . invokeCallback
