module Util (
  CInt (..)
, CString
, CStringLen
, withCString
, withCStringLen
, peekCStringLen
) where

import           Foreign.C (CInt (..), CString, CStringLen)
import           System.IO (utf8)
import qualified GHC.Foreign as GHC

withCStringLen :: String -> (CStringLen -> IO a) -> IO a
withCStringLen s f = GHC.withCStringLen utf8 s f

withCString :: String -> (CString -> IO a) -> IO a
withCString s f = GHC.withCString utf8 s f

peekCStringLen :: CStringLen -> IO String
peekCStringLen s = GHC.peekCStringLen utf8 s
