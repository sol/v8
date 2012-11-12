{-# LANGUAGE ForeignFunctionInterface #-}
module Foreign.JavaScript.V8 (
  withHandleScope
, runScript

, Context (..)
, withContext
, withContext_

, ContextScope (..)
, enterContext
, leaveContext

, Arguments (..)
, Value (..)
, toString
, mkUndefined
) where

import           Control.Applicative
import           Control.Exception (bracket)
import           Data.IORef
import           Foreign
import           Foreign.C (CString, CStringLen, CInt(..))
import qualified GHC.Foreign as GHC
import           System.IO (utf8)

withCString :: String -> (CString -> IO a) -> IO a
withCString s f = GHC.withCString utf8 s f

peekCStringLen :: CStringLen -> IO String
peekCStringLen s = GHC.peekCStringLen utf8 s

newtype Context = Context (Ptr ())
foreign import ccall contextNew :: InvocationCallback -> IO Context
foreign import ccall contextDispose :: Context -> IO ()

newtype ContextScope = ContextScope (Ptr ())
foreign import ccall enterContext :: Context -> IO ContextScope
foreign import ccall leaveContext :: ContextScope -> IO ()

type ActionCallback = FunPtr (IO ())
foreign import ccall "wrapper" mkActionCallback :: IO () -> IO ActionCallback
foreign import ccall c_withHandleScope :: ActionCallback -> IO ()

withHandleScope :: IO a -> IO a
withHandleScope action = do
  ref <- newIORef undefined
  bracket
    (mkActionCallback (action >>= writeIORef ref))
    freeHaskellFunPtr
    c_withHandleScope
  readIORef ref

-- | Create a new `Context` and run given action within that context.
withContext :: (Context -> IO a) -> IO a
withContext action = do
  bracket (mkInvocationCallback jsPrint) freeHaskellFunPtr $ \ptr -> do
    bracket (contextNew ptr) contextDispose $ \context -> do
      bracket (enterContext context) leaveContext $ \_ -> do
        action context
  where
    jsPrint :: Arguments -> IO Value
    jsPrint args = do
      argumentsGet 0 args >>= toString >>= putStrLn
      mkUndefined

-- | Create a new `Context` and run given action within that context.
withContext_ :: IO a -> IO a
withContext_ action = withContext $ \_ -> action

-- | Run a script.
--
-- This requires an active `Context`.
runScript :: String -> IO Value
runScript input = withCString input c_runScript
foreign import ccall c_runScript :: CString -> IO Value

newtype Value = Value (Ptr ())

foreign import ccall "mkUndefined" mkUndefined :: IO Value

newtype Arguments = Arguments (Ptr ())

type InvocationCallback = FunPtr (Arguments -> IO Value)
foreign import ccall "wrapper" mkInvocationCallback :: (Arguments -> IO Value) -> IO InvocationCallback

foreign import ccall "argumentsGet" argumentsGet :: CInt -> Arguments -> IO Value

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
