module Foreign.JavaScript.V8.Context (
  Context (..)
, contextNew
, contextDispose
, contextEnter
, contextExit

, ObjectTemplate (..)
, mkObjectTemplate
, objectTemplateAddFunction

, Arguments (..)
, argumentsGet

, contextAddFunction
) where

import           Foreign

import           Util
import           Foreign.JavaScript.V8.Value

newtype Context = Context (Ptr ())

newtype Arguments = Arguments (Ptr ())

type InvocationCallback = FunPtr (Arguments -> IO Value)
foreign import ccall "wrapper" mkInvocationCallback :: (Arguments -> IO Value) -> IO InvocationCallback

foreign import ccall "argumentsGet" argumentsGet :: CInt -> Arguments -> IO Value

foreign import ccall contextNew :: ObjectTemplate -> IO Context
foreign import ccall contextDispose :: Context -> IO ()

foreign import ccall contextEnter :: Context -> IO ()
foreign import ccall contextExit :: Context -> IO ()

-- |
-- This returns a finalizer.  It should be called after the given context has
-- been disposed to reclaim memory.
contextAddFunction :: Context -> String -> (Arguments -> IO Value) -> IO (IO ())
contextAddFunction context name f = do
  ptr <- mkInvocationCallback f
  withCString name $ \name_ -> c_contextAddFunction context name_ ptr
  return (freeHaskellFunPtr ptr)
foreign import ccall c_contextAddFunction :: Context -> CString -> InvocationCallback -> IO ()

newtype ObjectTemplate = ObjectTemplate (Ptr ())
foreign import ccall mkObjectTemplate :: IO ObjectTemplate

objectTemplateAddFunction :: ObjectTemplate -> String -> (Arguments -> IO Value) -> IO (IO ())
objectTemplateAddFunction t name f = do
  ptr <- mkInvocationCallback f
  withCString name $ \name_ -> c_objectTemplateAddFunction t name_ ptr
  return (freeHaskellFunPtr ptr)

foreign import ccall c_objectTemplateAddFunction :: ObjectTemplate -> CString -> InvocationCallback -> IO ()
