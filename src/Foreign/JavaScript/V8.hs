{-# LANGUAGE ForeignFunctionInterface #-}
module Foreign.JavaScript.V8 (
  withHandleScope
, runScript

, Context
, withContext
, withContext_

, contextAddFunction

, contextEnter
, contextExit

, Arguments
, argumentsGet

, Value
, toString
, mkUndefined
, mkString
) where

import           Control.Exception (bracket, bracket_)
import           Foreign

import           Util
import           Foreign.JavaScript.V8.Value
import           Foreign.JavaScript.V8.Context

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
