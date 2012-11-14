module Foreign.JavaScript.V8.DisposableSpec (main, spec) where

import           Test.Hspec
import           System.IO.Silently

import           Foreign.JavaScript.V8.Disposable

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Finalizer" $ do
    it "can be used to manage finalization code" $ do
      fin <- finalizerNew
      finalizerAdd fin $ putStrLn "foo"
      finalizerAdd fin $ putStrLn "bar"
      capture_ (finalize fin) `shouldReturn` "foo\nbar\n"

    it "throws AlreadyDisposed if called twice" $ do
      fin <- finalizerNew
      finalize fin
      finalize fin `shouldThrow` (== AlreadyDisposed)
