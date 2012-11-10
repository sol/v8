{-# LANGUAGE ForeignFunctionInterface #-}
module Foreign.JavaScript.V8 (runScript) where

import Foreign.C
import Foreign.Ptr
import Data.Int

runScript :: String -> IO ()
runScript input = do
  ptr <- mkInvocationCallback jsPrint
  withCString input (c_runScript ptr)
  freeHaskellFunPtr ptr
  where
    jsPrint :: Arguments -> IO Value
    jsPrint _ = do
      putStrLn "foo"
      mkUndefined

foreign import ccall "runScript" c_runScript :: InvocationCallback -> CString -> IO ()

type Value = Int64

foreign import ccall "mkUndefined" mkUndefined :: IO Value

data ArgumentsToken
type Arguments = Ptr ArgumentsToken

type InvocationCallback = FunPtr (Arguments -> IO Value)

foreign import ccall "wrapper" mkInvocationCallback :: (Arguments -> IO Value) -> IO InvocationCallback
