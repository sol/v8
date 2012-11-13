module Foreign.JavaScript.V8Spec (main, spec) where

import           Test.Hspec.Experimental
import           System.IO.Silently

import           Control.Monad
import           Control.Applicative

import           Foreign.JavaScript.V8

main :: IO ()
main = hspec spec

escapeString :: String -> String
escapeString s = go s ""
  where
    go :: String -> ShowS
    go ""     = id
    go (x:xs) = escap x . go xs

    escap :: Char -> ShowS
    escap c = case c of
      '\NUL' -> showString "\\u0000"
      '\\'   -> showString "\\\\"
      '\r'   -> showString "\\r"
      '\n'   -> showString "\\n"
      '\''   -> showString "\\'"
      _      -> showChar c

-- | Create a new `Context` and run a script.
runScript_ :: String -> IO ()
runScript_ input = withHandleScope $ do
  withContext_ (runScript input >> pure ())

spec :: Spec
spec = do
  describe "runScript" $ do
    it "can return a number" $ withHandleScope . withContext_ $ do
      (runScript "23" >>= toString) `shouldReturn` "23"

    it "can return a string" $ withHandleScope . withContext_ $ do
      (runScript "'foo'" >>= toString) `shouldReturn` "foo"

    it "can return undefined" $ withHandleScope . withContext_ $ do
      (runScript "undefined" >>= toString) `shouldReturn` "undefined"

    it "can do basic math" $ withHandleScope . withContext_ $ do
      (runScript "1 + 2" >>= toString) `shouldReturn` "3"

    it "can create global variables" $ withHandleScope . withContext_ $ do
      (runScript "var foo = 'bar';" >>= toString) `shouldReturn` "undefined"
      (runScript "foo" >>= toString) `shouldReturn` "bar"

  describe "contextAddFunction" $ do
    it "adds function to context" $ do
      join . withHandleScope . withContext $ \c -> do
        fin <- contextAddFunction c "concat" $ \args -> do
          a <- argumentsGet 0 args >>= toString
          b <- argumentsGet 1 args >>= toString
          mkString (a ++ b)
        (runScript "concat('foo', 'bar')" >>= toString) `shouldReturn` "foobar"
        return fin

    it "works with unicode names" $ do
      join . withHandleScope . withContext $ \c -> do
        fin <- contextAddFunction c "foo\955bar" $ \_ -> do
          mkString "foo"
        (runScript "foo\955bar()" >>= toString) `shouldReturn` "foo"
        return fin

  describe "global built-ins" $ do
    describe "print" $ do
      it "can print a number" $ do
        capture_ (runScript_ "print(23)") `shouldReturn` "23\n"

      it "can print a string" $ do
        capture_ (runScript_ "print('foo')") `shouldReturn` "foo\n"

      it "can print a string that contains NUL characters" $ do
        capture_ (runScript_ "print('foo-\\u0000-bar')") `shouldReturn` "foo-\NUL-bar\n"

      it "works for arbitrary strings" $ \str -> do
        capture_ (runScript_ $ "print('" ++ escapeString str ++ "')") `shouldReturn` str ++ "\n"

      it "can print undefined" $ do
        capture_ (runScript_ "print(undefined)") `shouldReturn` "undefined\n"

      it "prints undefined if called without arguments" $ do
        capture_ (runScript_ "print()") `shouldReturn` "undefined\n"

