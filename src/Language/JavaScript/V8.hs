module Language.JavaScript.V8 (
  run
, run_
, SourceLoader
) where

import           Prelude hiding (mapM_)
import           Control.Applicative
import           Control.Exception
import           Data.IORef
import           Data.Map   (Map)
import qualified Data.Map as Map
import           Data.Foldable (mapM_)
import           System.FilePath

import           Foreign.JavaScript.V8

-- | A mapping from module name to module source.
type SourceLoader = String -> IO String

fileModuleLoader :: FilePath -> SourceLoader
fileModuleLoader path name = readFile (path </> (name ++ ".js"))

run :: FilePath -> String -> IO ()
run path = run_ (fileModuleLoader path)

run_ :: SourceLoader -> String -> IO ()
run_ sourceLoader source = withHandleScope $ do
  bracket (mkModuleLoader sourceLoader) dispose $ \loader -> do
    bracket (mkModuleContext loader) dispose $ \c -> do
      withContextScope c $ do
        runScript source >> pure ()

mkModuleContext :: ModuleLoader -> IO Context
mkModuleContext loader = do
  t <- mkObjectTemplate
  objectTemplateAddFunction t "print" jsPrint
  objectTemplateAddFunction t "require" (jsRequire loader)
  contextNew t

-- | A mapping from module names to loaded modules.
type ModuleCache = IORef (Map String Context)

moduleCacheLookup :: ModuleCache -> String -> IO (Maybe Context)
moduleCacheLookup cache name = Map.lookup name <$> readIORef cache

moduleCacheInsert :: ModuleCache -> String -> Context -> IO ()
moduleCacheInsert cache name c = modifyIORef cache (Map.insert name c)

data ModuleLoader = ModuleLoader SourceLoader ModuleCache

instance Disposable ModuleLoader where
  dispose (ModuleLoader _ cache) = readIORef cache >>= mapM_ dispose

mkModuleLoader :: SourceLoader -> IO ModuleLoader
mkModuleLoader sourceLoader = ModuleLoader <$> pure sourceLoader <*> newIORef Map.empty

loadModule :: ModuleLoader -> String -> IO Context
loadModule loader@(ModuleLoader sourceLoader cache) name = do
  moduleCacheLookup cache name >>= maybe load return
  where
    load = do
      source <- sourceLoader name
      c <- mkModuleContext loader
      _ <- withContextScope c $ do
        runScript "var exports = new Object()" >> runScript source
      moduleCacheInsert cache name c
      return c

jsRequire :: ModuleLoader -> Arguments -> IO Value
jsRequire loader args = do
  name <- argumentsGet 0 args >>= toString
  c <- loadModule loader name
  withContextScope c (runScript "exports")

jsPrint :: Arguments -> IO Value
jsPrint args = do
  argumentsGet 0 args >>= toString >>= putStrLn
  mkUndefined
