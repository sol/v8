{-# LANGUAGE OverloadedStrings #-}
module Language.JavaScript.V8Spec (main, spec) where

import           Test.Hspec
import           System.IO.Silently

import           Language.JavaScript.V8
import           Data.String.Builder

import           Control.Exception (throwIO, ErrorCall(..))
import           Control.Monad.Trans.State
import           Data.Map (Map)
import qualified Data.Map as M

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "run" $ do
    it "can run a script that uses require" $ do
      capture_ (readFile "resources/trivial/program.js" >>= run "resources/trivial/")
        `shouldReturn` "inc(1) = 2\n"

    it "can use native functions from other modules" $ do
      let loader = fakeLoader $ do
            moduleSource "sys" $ do
              "exports.puts = function(str) {print(str)};"

      capture_ . run_ loader . build $ do
        "require('sys').puts(23);"
      `shouldReturn` "23\n"

fakeLoader :: FakeLoader -> ModuleLoader
fakeLoader m name = maybe noSource return (M.lookup name m_)
  where
    m_ = execState m M.empty
    noSource = throwIO (ErrorCall $ "no source for " ++ show name)

type FakeLoader = State (Map String String) ()

moduleSource :: String -> Builder -> FakeLoader
moduleSource name source = modify $ M.insert name (build source)
