module Util where

import GHCJS.Marshal
import GHCJS.Marshal.Pure
import GHCJS.Foreign.Callback
import GHCJS.Types

hsEscapeString :: JSVal -> IO JSVal
hsEscapeString = toJSVal . (show :: String -> String) . pFromJSVal
