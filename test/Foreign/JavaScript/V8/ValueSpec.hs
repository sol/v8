module Foreign.JavaScript.V8.ValueSpec (main, spec) where

import           Test.Hspec

import           Foreign.JavaScript.V8

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "toString" $ do
    it "can convert undefined to string" $ withHandleScope . withContext_ $ do
      (mkUndefined >>= toString) `shouldReturn` "undefined"
