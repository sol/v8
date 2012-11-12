{-# LANGUAGE ForeignFunctionInterface #-}
module Foreign.JavaScript.V8 (
  runScript
, Arguments (..)
, Value (..)
, StringValue (..)
) where

import Control.Applicative
import Foreign.C hiding (withCString, peekCString, peekCStringLen)
import Foreign.Ptr
import Foreign.Marshal.Alloc
import qualified GHC.Foreign as GHC
import           System.IO (utf8)

withCString :: String -> (CString -> IO a) -> IO a
withCString s f = GHC.withCString utf8 s f

peekCStringLen :: CStringLen -> IO String
peekCStringLen s = GHC.peekCStringLen utf8 s

runScript :: String -> IO ()
runScript input = do
  ptr <- mkInvocationCallback jsPrint
  withCString input (c_runScript ptr)
  freeHaskellFunPtr ptr
  where
    jsPrint :: Arguments -> IO Value
    jsPrint args = do
      argumentsGet 0 args >>= toString >>= getStringValue >>= putStrLn
      mkUndefined

foreign import ccall "runScript" c_runScript :: InvocationCallback -> CString -> IO ()

newtype Value = Value (Ptr ())

foreign import ccall "mkUndefined" mkUndefined :: IO Value

newtype Arguments = Arguments (Ptr ())

type InvocationCallback = FunPtr (Arguments -> IO Value)

foreign import ccall "wrapper" mkInvocationCallback :: (Arguments -> IO Value) -> IO InvocationCallback

foreign import ccall "argumentsGet" argumentsGet :: CInt -> Arguments -> IO Value

newtype StringValue = StringValue (Ptr ())

getStringValue :: StringValue -> IO String
getStringValue src = do
  n <- fromIntegral <$> stringUtf8Length src
  allocaBytes n $ \dst -> c_getStringValue src dst >> peekCStringLen (dst, n)

foreign import ccall "toString" toString :: Value -> IO StringValue
foreign import ccall "stringUtf8Length" stringUtf8Length :: StringValue -> IO CInt
foreign import ccall "getStringValue" c_getStringValue :: StringValue -> CString -> IO ()
