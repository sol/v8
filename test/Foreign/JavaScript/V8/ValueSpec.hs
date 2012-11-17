module Foreign.JavaScript.V8.ValueSpec (main, spec) where

import           Test.Hspec.Experimental

import           Control.Exception (ArithException(..), evaluate)
import           Foreign.JavaScript.V8
import           Foreign.JavaScript.V8.Value

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "withHandleScope" $ do
    it "propagates exceptions" $ do
      withHandleScope $ do
        evaluate (1 `div` 0 :: Int)
      `shouldThrow` (== DivideByZero)

  describe "numberOfHandles" $ do
    it "counts the number of allocated handles" $ do
      withHandleScope $ do
        numberOfHandles `shouldReturn` 0
        _ <- mkString "foo"
        numberOfHandles `shouldReturn` 1

  describe "mkString" $ do
    it "creates a string value" $ do
      withHandleScope $ do
        (mkString "foo" >>= toString) `shouldReturn` "foo"

    it "works for arbitrary strings" $
      \str -> withHandleScope $ do
        (mkString str >>= toString) `shouldReturn` str

  describe "toString" $ do
    it "can convert undefined to string" $ withHandleScope . withContext_ $ do
      (mkUndefined >>= toString) `shouldReturn` "undefined"

  describe "mkUndefined" $ do
    it "dose not add to the number of allocated handles" $ do
      withHandleScope $ do
        numberOfHandles `shouldReturn` 0
        _ <- mkUndefined
        numberOfHandles `shouldReturn` 0
