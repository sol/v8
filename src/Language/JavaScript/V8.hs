module Language.JavaScript.V8 (run) where

import           Control.Applicative
import           Control.Exception
import           System.FilePath

import           Foreign.JavaScript.V8

run :: FilePath -> String -> IO ()
run path source = do
  withHandleScope $ do
    bracket (mkModuleContext path) dispose $ \c -> do
      withContextScope c $ do
        runScript source >> pure ()

mkModuleContext :: FilePath -> IO Context
mkModuleContext path = do
  t <- mkObjectTemplate
  objectTemplateAddFunction t "print" jsPrint
  objectTemplateAddFunction t "require" (jsRequire path)
  contextNew t

loadModule :: FilePath -> String -> IO Value
loadModule path name = withHandleScope $ do
  source <- readFile (path </> (name ++ ".js"))
  c <- mkModuleContext path
  v <- withContextScope c $ do
    _ <- runScript "var exports = new Object()"
    _ <- runScript source
    runScript "exports"
  dispose c
  return v

jsRequire :: FilePath -> Arguments -> IO Value
jsRequire path args = do
  name <- argumentsGet 0 args >>= toString
  loadModule path name

jsPrint :: Arguments -> IO Value
jsPrint args = do
  argumentsGet 0 args >>= toString >>= putStrLn
  mkUndefined
