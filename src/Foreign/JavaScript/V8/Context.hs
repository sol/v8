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

import           Control.Applicative
import           Foreign

import           Util
import           Foreign.JavaScript.V8.Value
import           Foreign.JavaScript.V8.Finalizer

newtype Context = Context (Ptr ())

newtype Arguments = Arguments (Ptr ())

type InvocationCallback = FunPtr (Arguments -> IO Value)
foreign import ccall "wrapper" mkInvocationCallback :: (Arguments -> IO Value) -> IO InvocationCallback

foreign import ccall "argumentsGet" argumentsGet :: CInt -> Arguments -> IO Value


contextNew :: ObjectTemplate -> IO Context
contextNew (ObjectTemplate ptr fin) = c_contextNew ptr
foreign import ccall c_contextNew :: Ptr ObjectTemplateToken -> IO Context

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

data ObjectTemplateToken
data ObjectTemplate = ObjectTemplate (Ptr ObjectTemplateToken) Finalizer

mkObjectTemplate :: IO ObjectTemplate
mkObjectTemplate = ObjectTemplate <$> c_mkObjectTemplate <*> finalizerNew
foreign import ccall c_mkObjectTemplate :: IO (Ptr ObjectTemplateToken)

objectTemplateAddFunction :: ObjectTemplate -> String -> (Arguments -> IO Value) -> IO ()
objectTemplateAddFunction (ObjectTemplate t fin) name f = do
  ptr <- mkInvocationCallback f
  withCString name $ \name_ -> c_objectTemplateAddFunction t name_ ptr
  finalizerAdd fin (freeHaskellFunPtr ptr)

foreign import ccall c_objectTemplateAddFunction :: Ptr ObjectTemplateToken -> CString -> InvocationCallback -> IO ()

instance Disposable ObjectTemplate where
  dispose (ObjectTemplate _ fin) = dispose fin
