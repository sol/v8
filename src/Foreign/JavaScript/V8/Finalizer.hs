{-# LANGUAGE DeriveDataTypeable #-}
module Foreign.JavaScript.V8.Finalizer (
  Disposable (..)
, AlreadyDisposed (..)
, Finalizer
, finalizerNew
, finalizerAdd
, finalize
) where

import           Control.Applicative
import           Control.Monad
import           Data.IORef

import qualified Control.Exception as E
import           Data.Typeable

data AlreadyDisposed = AlreadyDisposed
  deriving (Eq, Show, Typeable)

instance E.Exception AlreadyDisposed

newtype Finalizer = Finalizer (IORef (IO ()))

-- | Create empty finalizer.
finalizerNew :: IO Finalizer
finalizerNew = Finalizer <$> newIORef (pure ())

-- | Add action to finalizer.
finalizerAdd :: Finalizer -> IO () -> IO ()
finalizerAdd (Finalizer fin) action = modifyIORef fin (>> action)

-- | Run finalizer.
finalize :: Finalizer -> IO ()
finalize (Finalizer fin) = do
  join (readIORef fin)
  writeIORef fin (E.throwIO AlreadyDisposed)

class Disposable a where
  dispose :: a -> IO ()

instance Disposable Finalizer where
  dispose = finalize
