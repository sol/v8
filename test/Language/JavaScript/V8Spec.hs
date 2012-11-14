module Language.JavaScript.V8Spec (main, spec) where

import           Test.Hspec
import           System.IO.Silently

import           Language.JavaScript.V8

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "run" $ do
    it "can run a script that uses require" $ do
      capture_ (readFile "resources/trivial/program.js" >>= run "resources/trivial/")
        `shouldReturn` "inc(1) = 2\n"
