{-# LANGUAGE ForeignFunctionInterface #-}
module Foreign.JavaScript.V8 (
  withHandleScope
, runScript

, Context
, contextNew
, contextDispose
, withContext
, withContext_

, mkObjectTemplate
, objectTemplateAddFunction

, contextAddFunction

, contextEnter
, contextExit

, Arguments
, argumentsGet

, Value
, toString
, mkUndefined
, mkString

, module Foreign.JavaScript.V8.Finalizer
) where

import           Control.Exception (bracket, bracket_)

import           Util
import           Foreign.JavaScript.V8.Value
import           Foreign.JavaScript.V8.Context
import           Foreign.JavaScript.V8.Finalizer

-- | Create a new `Context` and run given action within that context.
withContext :: (Context -> IO a) -> IO a
withContext action = do
  t <- mkObjectTemplate
  objectTemplateAddFunction t "print" jsPrint
  r <- bracket (contextNew t) contextDispose $ \context -> do
    bracket_ (contextEnter context) (contextExit context) $ do
      action context
  dispose t
  return r
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
