module Util where

import           Foreign.C
import           System.IO (utf8)
import qualified GHC.Foreign as GHC

withCString :: String -> (CString -> IO a) -> IO a
withCString s f = GHC.withCString utf8 s f

peekCStringLen :: CStringLen -> IO String
peekCStringLen s = GHC.peekCStringLen utf8 s
