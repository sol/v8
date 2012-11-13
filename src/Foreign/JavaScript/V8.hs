{-# LANGUAGE ForeignFunctionInterface #-}
module Foreign.JavaScript.V8 (
  withHandleScope
, runScript

, Context (..)
, withContext
, withContext_

, contextAddFunction

, contextEnter
, contextExit

, Arguments (..)
, argumentsGet

, Value
, toString
, mkUndefined
, mkString
) where

import           Control.Exception (bracket, bracket_)
import           Foreign
import           Foreign.C (CString, CInt(..))

import           Util
import           Foreign.JavaScript.V8.Value

newtype Context = Context (Ptr ())
foreign import ccall contextNew :: InvocationCallback -> IO Context
foreign import ccall contextDispose :: Context -> IO ()

foreign import ccall contextEnter :: Context -> IO ()
foreign import ccall contextExit :: Context -> IO ()

-- | Create a new `Context` and run given action within that context.
withContext :: (Context -> IO a) -> IO a
withContext action = do
  bracket (mkInvocationCallback jsPrint) freeHaskellFunPtr $ \ptr -> do
    bracket (contextNew ptr) contextDispose $ \context -> do
      bracket_ (contextEnter context) (contextExit context) $ do
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


newtype Arguments = Arguments (Ptr ())

type InvocationCallback = FunPtr (Arguments -> IO Value)
foreign import ccall "wrapper" mkInvocationCallback :: (Arguments -> IO Value) -> IO InvocationCallback

foreign import ccall "argumentsGet" argumentsGet :: CInt -> Arguments -> IO Value

-- |
-- This returns a finalizer.  It should be called after the given context has
-- been disposed to reclaim memory.
contextAddFunction :: Context -> String -> (Arguments -> IO Value) -> IO (IO ())
contextAddFunction context name f = do
  ptr <- mkInvocationCallback f
  withCString name $ \name_ -> c_contextAddFunction context name_ ptr
  return (freeHaskellFunPtr ptr)

foreign import ccall c_contextAddFunction :: Context -> CString -> InvocationCallback -> IO ()
