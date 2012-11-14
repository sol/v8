module Foreign.JavaScript.V8.Finalizer (
  Disposable (..)
, Finalizer
, finalizerNew
, finalizerAdd
, finalize
) where

import           Control.Applicative
import           Control.Monad
import           Data.IORef

newtype Finalizer = Finalizer {getFinalizer :: IORef (IO ())}

-- | Create empty finalizer.
finalizerNew :: IO Finalizer
finalizerNew = Finalizer <$> newIORef (pure ())

-- | Add action to finalizer.
finalizerAdd :: Finalizer -> IO () -> IO ()
finalizerAdd (Finalizer fin) action = modifyIORef fin (>> action)

-- | Run finalizer.
finalize :: Finalizer -> IO ()
finalize = join . readIORef . getFinalizer

class Disposable a where
  dispose :: a -> IO ()

instance Disposable Finalizer where
  dispose = finalize
