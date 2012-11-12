module Foreign.JavaScript.V8Spec (main, spec) where

import           Test.Hspec.Experimental
import           System.IO.Silently

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

spec :: Spec
spec = do
  describe "global built-ins" $ do
    describe "print" $ do
      it "can print a number" $ do
        capture_ (runScript "print(23)") `shouldReturn` "23\n"

      it "can print a string" $ do
        capture_ (runScript "print('foo')") `shouldReturn` "foo\n"

      it "can print a string that contains NUL characters" $ do
        capture_ (runScript "print('foo-\\u0000-bar')") `shouldReturn` "foo-\NUL-bar\n"

      it "works for arbitrary strings" $ \str -> do
        capture_ (runScript $ "print('" ++ escapeString str ++ "')") `shouldReturn` str ++ "\n"

      it "can print `undefined`" $ do
        capture_ (runScript "print(undefined)") `shouldReturn` "undefined\n"

      it "prints `undefined` if called without arguments" $ do
        capture_ (runScript "print()") `shouldReturn` "undefined\n"
