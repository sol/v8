module Foreign.JavaScript.V8.Value (
  Value (..)
, withHandleScope
, mkUndefined
, mkString
, toString
) where

import           Control.Applicative
import           Control.Exception (SomeException, bracket, try, throwIO)
import           Data.IORef
import           Foreign
import           Foreign.C (CInt (..), CString)

import           Util

newtype Value = Value (Ptr ())

foreign import ccall mkUndefined :: IO Value

mkString :: String -> IO Value
mkString str = withCString str c_mkString
foreign import ccall c_mkString :: CString -> IO Value

data StringValueToken
type StringValue = Ptr StringValueToken

toString :: Value -> IO String
toString value = withHandleScope $ do
  src <- valueToString value
  n <- fromIntegral <$> stringUtf8Length src
  allocaBytes n $ \dst -> stringUtf8Value src dst >> peekCStringLen (dst, n)

foreign import ccall valueToString :: Value -> IO StringValue
foreign import ccall stringUtf8Length :: StringValue -> IO CInt
foreign import ccall stringUtf8Value :: StringValue -> CString -> IO ()

type ActionCallback = FunPtr (IO ())
foreign import ccall "wrapper" mkActionCallback :: IO () -> IO ActionCallback
foreign import ccall c_withHandleScope :: ActionCallback -> IO ()

withHandleScope :: IO a -> IO a
withHandleScope action = do
  ref <- newIORef (error "withHandleScope: empty IORef")
  bracket
    (mkActionCallback $ try action >>= writeIORef ref)
    freeHaskellFunPtr
    c_withHandleScope
  readIORef ref >>= liftE
  where
    liftE :: Either SomeException a -> IO a
    liftE = either throwIO return
