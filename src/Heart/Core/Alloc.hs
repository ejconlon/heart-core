{-# LANGUAGE ExistentialQuantification #-}

module Heart.Core.Alloc
  ( Alloc
  , newAlloc
  , newEnumAlloc
  , incAlloc
  ) where

import Heart.Core.Prelude
import UnliftIO.IORef

data Alloc e = forall x. Alloc
  { _allocStep :: !(x -> x)
  , _allocRef:: !(IORef x)
  , _allocExtract :: !(x -> e)
  }

instance Functor Alloc where
  fmap f (Alloc step ref extract) = Alloc step ref (f . extract)

newAlloc :: MonadIO m => (e -> e) -> e -> m (Alloc e)
newAlloc f e = fmap (\r -> Alloc f r id) (newIORef e)

newEnumAlloc :: (MonadIO m, Enum e) => m (Alloc e)
newEnumAlloc = newAlloc succ (toEnum 0)

incAlloc :: MonadIO m => Alloc e -> m e
incAlloc (Alloc step ref extract) = atomicModifyIORef' ref (\e -> (step e, extract e))
