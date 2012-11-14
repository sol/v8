module Language.JavaScript.V8 (
  run
, run_
, ModuleLoader
) where

import           Control.Applicative
import           Control.Exception
import           System.FilePath

import           Foreign.JavaScript.V8

-- | A mapping from module name to module source.
type ModuleLoader = String -> IO String

fileModuleLoader :: FilePath -> ModuleLoader
fileModuleLoader path name = readFile (path </> (name ++ ".js"))

run :: FilePath -> String -> IO ()
run path = run_ (fileModuleLoader path)

run_ :: ModuleLoader -> String -> IO ()
run_ loader source = do
  withHandleScope $ do
    bracket (mkModuleContext loader) dispose $ \c -> do
      withContextScope c $ do
        runScript source >> pure ()

mkModuleContext :: ModuleLoader -> IO Context
mkModuleContext loader = do
  t <- mkObjectTemplate
  objectTemplateAddFunction t "print" jsPrint
  objectTemplateAddFunction t "require" (jsRequire loader)
  contextNew t

loadModule :: ModuleLoader -> String -> IO Value
loadModule loader name = withHandleScope $ do
  source <- loader name
  c <- mkModuleContext loader
  v <- withContextScope c $ do
    _ <- runScript "var exports = new Object()"
    _ <- runScript source
    runScript "exports"
  dispose c
  return v

jsRequire :: ModuleLoader -> Arguments -> IO Value
jsRequire loader args = do
  name <- argumentsGet 0 args >>= toString
  loadModule loader name

jsPrint :: Arguments -> IO Value
jsPrint args = do
  argumentsGet 0 args >>= toString >>= putStrLn
  mkUndefined
