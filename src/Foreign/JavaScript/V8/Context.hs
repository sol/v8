module Foreign.JavaScript.V8.Context (
  Context (..)
, contextNew
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

data ContextToken
data Context = Context (Ptr ContextToken) Finalizer
type ContextPtr = Ptr ContextToken

newtype Arguments = Arguments (Ptr ())

type InvocationCallback = FunPtr (Arguments -> IO Value)
foreign import ccall "wrapper" mkInvocationCallback :: (Arguments -> IO Value) -> IO InvocationCallback

foreign import ccall "argumentsGet" argumentsGet :: CInt -> Arguments -> IO Value

-- |
-- The context takes ownership of the passed `ObjectTemplate`, it is disposed
-- when the context is disposed.  You must not call `dispose` on an
-- `ObjectTemplate` that you have passed to `contextNew`!
contextNew :: ObjectTemplate -> IO Context
contextNew (ObjectTemplate t fin) = Context <$> c_contextNew t <*> pure fin
foreign import ccall c_contextNew :: Ptr ObjectTemplateToken -> IO ContextPtr

instance Disposable Context where
  dispose (Context c fin) = contextDispose c >> finalize fin

foreign import ccall contextDispose :: ContextPtr -> IO ()

contextEnter :: Context -> IO ()
contextEnter (Context c _) = c_contextEnter c
foreign import ccall c_contextEnter :: ContextPtr -> IO ()

contextExit :: Context -> IO ()
contextExit (Context c _) = c_contextExit c
foreign import ccall c_contextExit :: ContextPtr -> IO ()

-- |
-- This returns a finalizer.  It should be called after the given context has
-- been disposed to reclaim memory.
contextAddFunction :: Context -> String -> (Arguments -> IO Value) -> IO ()
contextAddFunction (Context c fin) name f = do
  ptr <- mkInvocationCallback f
  withCString name $ \name_ -> c_contextAddFunction c name_ ptr
  finalizerAdd fin (freeHaskellFunPtr ptr)
foreign import ccall c_contextAddFunction :: ContextPtr -> CString -> InvocationCallback -> IO ()

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
  dispose (ObjectTemplate _ fin) = finalize fin
