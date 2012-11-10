module Foreign.JavaScript.V8Spec (main, spec) where

import           Test.Hspec
import           System.IO.Silently

import           Foreign.JavaScript.V8

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "global built-ins" $ do
    describe "print" $ do
      it "can print a string" $ do
        capture_ (runScript "print('foo')") `shouldReturn` "foo\n"
