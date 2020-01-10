module Heart.Core.RIO
  ( RIO (..)
  , withRIO
  , runRIO
  ) where

import Control.Monad.IO.Unlift (withUnliftIO)
import Control.Monad.Reader (ReaderT (..))
import Heart.Core.Prelude

newtype RIO env a = RIO { unRIO :: ReaderT env IO a }
  deriving (Functor, Applicative, Monad, MonadReader env, MonadIO, MonadThrow, MonadFail, MonadCatch, MonadMask)

instance MonadUnliftIO (RIO env) where
  askUnliftIO = RIO (ReaderT (\r -> withUnliftIO (\u -> return (UnliftIO (unliftIO u . flip runReaderT r . unRIO)))))

withRIO :: (env -> env') -> RIO env' a -> RIO env a
withRIO f m = do
  env <- ask
  let env' = f env
  runRIO env' m

runRIO :: MonadIO m => env -> RIO env a -> m a
runRIO r m = liftIO (runReaderT (unRIO m) r)
