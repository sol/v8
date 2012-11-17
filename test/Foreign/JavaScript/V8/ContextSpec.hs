{-# LANGUAGE OverloadedStrings #-}
module Foreign.JavaScript.V8.ContextSpec (main, spec) where

import           Test.Hspec
import           Data.String.Builder (build)

import           Control.Exception (bracket)

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
            withContextScope c $ do
              (runScript "reverse('foo')" >>= toString) `shouldReturn` "oof"
              (runScript "concat('foo', 'bar')" >>= toString) `shouldReturn` "foobar"

    describe "dispose" $ do
      it "disposes the associated object template" $ do
        withHandleScope $ do
          t <- mkObjectTemplate
          contextNew t >>= dispose
          dispose t `shouldThrow` (== AlreadyDisposed)

    it "can share values with other contexts" $ do
      withHandleScope $ do
        c1 <- mkObjectTemplate >>= contextNew
        v <- withContextScope c1 $ do
          runScript "'bar'"

        t <- mkObjectTemplate
        objectTemplateAddFunction t "foo" $ \_ -> do
          return v

        bracket (contextNew t) dispose $ \c2 -> do
          withContextScope c2 $ do
            (runScript "foo()" >>= toString) `shouldReturn` "bar"
        dispose c1

    it "can share objects with other contexts" $ do
      withHandleScope $ do
        c1 <- mkObjectTemplate >>= contextNew
        v <- withContextScope c1 $ do
          runScript . build $ do
            "var foo = new Object()"
            "foo.bar = 23"
            "foo"

        t <- mkObjectTemplate
        objectTemplateAddFunction t "foo" $ \_ -> do
          return v

        bracket (contextNew t) dispose $ \c2 -> do
          withContextScope c2 $ do
            (runScript "foo().bar" >>= toString) `shouldReturn` "23"
        dispose c1
  where
    jsReverse args = do
      s <- argumentsGet 0 args >>= toString
      mkString (reverse s)

    jsConcat args = do
      a <- argumentsGet 0 args >>= toString
      b <- argumentsGet 1 args >>= toString
      mkString (a ++ b)
