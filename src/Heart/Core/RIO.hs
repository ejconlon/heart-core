{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Heart.Core.RIO
  ( HasStateRef (..)
  , RIO (..)
  , withRIO
  , runRIO
  ) where

import Control.Monad.IO.Unlift (withUnliftIO)
import Control.Monad.Reader (ReaderT (..))
import Heart.Core.Prelude
import UnliftIO.IORef (IORef, readIORef, writeIORef)

newtype RIO env a = RIO { unRIO :: ReaderT env IO a }
  deriving (Functor, Applicative, Monad, MonadReader env, MonadIO, MonadThrow, MonadFail, MonadCatch, MonadMask)

instance MonadUnliftIO (RIO env) where
  askUnliftIO = RIO (ReaderT (\r -> withUnliftIO (\u -> return (UnliftIO (unliftIO u . flip runReaderT r . unRIO)))))

withRIO :: (env -> env') -> RIO env' a -> RIO env a
withRIO f m = do
  env <- ask
  let env' = f env
  runRIO env' m

liftRIO :: (MonadIO m, MonadReader env m) => RIO env a -> m a
liftRIO rio = do
  env <- ask
  runRIO env rio

runRIO :: MonadIO m => env -> RIO env a -> m a
runRIO r m = liftIO (runReaderT (unRIO m) r)

class HasStateRef st env | env -> st where
  stateRefL :: Lens' env (IORef st)

instance HasStateRef st env => MonadState st (RIO env) where
  get = do
    ref <- view stateRefL
    readIORef ref
  put st = do
    ref <- view stateRefL
    writeIORef ref st
