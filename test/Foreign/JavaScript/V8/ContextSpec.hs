module Foreign.JavaScript.V8.ContextSpec (main, spec) where

import           Test.Hspec

import           Control.Exception (bracket, bracket_)

import           Foreign.JavaScript.V8

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Context" $ do
    describe "contextNew" $ do
      it "takes a template for the global object" $ do
        withHandleScope $ do
          t <- mkObjectTemplate
          objectTemplateAddFunction t "reverse" jsReverse
          objectTemplateAddFunction t "concat"  jsConcat
          bracket (contextNew t) dispose $ \c -> do
            bracket_ (contextEnter c) (contextExit c) $ do
              (runScript "reverse('foo')" >>= toString) `shouldReturn` "oof"
              (runScript "concat('foo', 'bar')" >>= toString) `shouldReturn` "foobar"

    describe "dispose" $ do
      it "disposes the associated object template" $ do
        withHandleScope $ do
          t <- mkObjectTemplate
          contextNew t >>= dispose
          dispose t `shouldThrow` (== AlreadyDisposed)
  where
    jsReverse args = do
      s <- argumentsGet 0 args >>= toString
      mkString (reverse s)

    jsConcat args = do
      a <- argumentsGet 0 args >>= toString
      b <- argumentsGet 1 args >>= toString
      mkString (a ++ b)
