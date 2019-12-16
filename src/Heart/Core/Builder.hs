{-# LANGUAGE ExistentialQuantification #-}

module Heart.Core.Builder
  ( Builder
  , premapBuilder
  , newBuilder
  , newMonoidBuilder
  , addBuilder
  , addAllBuilder
  , consumeBuilder
  , drainBuilder
  , runBuilder
  , evalBuilder
  , SeqBuilder
  , newSeqBuilder
  , runSeqBuilder
  , evalSeqBuilder
  , MapBuilder
  , newMapBuilder
  , runMapBuilder
  , evalMapBuilder
  , SetBuilder
  , newSetBuilder
  , runSetBuilder
  , evalSetBuilder
  , HashMapBuilder
  , newHashMapBuilder
  , runHashMapBuilder
  , evalHashMapBuilder
  , HashSetBuilder
  , newHashSetBuilder
  , runHashSetBuilder
  , evalHashSetBuilder
  , OrdMultiMapBuilder
  , newOrdMultiMapBuilder
  , runOrdMultiMapBuilder
  , evalOrdMultiMapBuilder
  , HashMultiMapBuilder
  , newHashMultiMapBuilder
  , runHashMultiMapBuilder
  , evalHashMultiMapBuilder
  ) where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Heart.Core.MultiMap
import Heart.Core.Prelude
import qualified ListT
import UnliftIO.IORef

data Builder a b = forall x. Builder
  { _builderStep :: !(x -> a -> x)
  , _builderRef :: !(IORef x)
  , _builderExtract :: !(x -> b)
  }

instance Functor (Builder a) where
  fmap f (Builder step ref extract) = Builder step ref (f . extract)

premapBuilder :: (z -> a) -> Builder a b -> Builder z b
premapBuilder f (Builder step ref extract) = Builder (\x z -> step x (f z)) ref extract

newBuilder :: MonadIO m => (b -> a -> b) -> b -> m (Builder a b)
newBuilder f b = fmap (\ref -> Builder f ref id) (newIORef b)

newMonoidBuilder :: (MonadIO m, Monoid b) => m (Builder b b)
newMonoidBuilder = newBuilder mappend mempty

addBuilder :: MonadIO m => Builder a b -> a -> m ()
addBuilder (Builder step ref _) b = modifyIORef' ref (`step` b)

addAllBuilder :: (MonadIO m, Foldable f) => Builder a b -> f a -> m ()
addAllBuilder (Builder step ref _) as = modifyIORef' ref (\a -> foldl step a as)

consumeBuilder :: MonadIO m => Builder a b -> ListT m a -> m ()
consumeBuilder b = ListT.traverse_ (addBuilder b)

drainBuilder :: MonadIO m => Builder a b -> m b
drainBuilder (Builder _ ref extract) = fmap extract (readIORef ref)

rawRunBuilder :: MonadIO m => Builder a b -> (Builder a b -> m c) -> m (b, c)
rawRunBuilder s h = do
  c <- h s
  a' <- drainBuilder s
  pure (a', c)

runBuilder :: MonadIO m => (b -> a -> b) -> b -> (Builder a b -> m c) -> m (b, c)
runBuilder f a h = do
  s <- newBuilder f a
  rawRunBuilder s h

evalBuilder :: MonadIO m => (b -> a -> b) -> b -> (Builder a b -> m ()) -> m b
evalBuilder f a h = fmap fst (runBuilder f a h)

type SeqBuilder a = Builder a (Seq a)

newSeqBuilder :: MonadIO m => m (SeqBuilder a)
newSeqBuilder = newBuilder (:|>) Empty

runSeqBuilder :: MonadIO m => (SeqBuilder a -> m c) -> m (Seq a, c)
runSeqBuilder h = newSeqBuilder >>= flip rawRunBuilder h

evalSeqBuilder :: MonadIO m => (SeqBuilder a -> m ()) -> m (Seq a)
evalSeqBuilder h = fmap fst (runSeqBuilder h)

type MapBuilder k v = Builder (k, v) (Map k v)

newMapBuilder :: (MonadIO m, Ord k) => m (MapBuilder k v)
newMapBuilder = newBuilder (\m (k, v) -> Map.insert k v m) Map.empty

runMapBuilder :: (MonadIO m, Ord k) => (MapBuilder k v -> m c) -> m (Map k v, c)
runMapBuilder h = newMapBuilder >>= flip rawRunBuilder h

evalMapBuilder :: (MonadIO m, Ord k) => (MapBuilder k v -> m ()) -> m (Map k v)
evalMapBuilder h = fmap fst (runMapBuilder h)

type SetBuilder k = Builder k (Set k)

newSetBuilder :: (MonadIO m, Ord k) => m (SetBuilder k)
newSetBuilder = newBuilder (flip Set.insert) Set.empty

runSetBuilder :: (MonadIO m, Ord k) => (SetBuilder k -> m c) -> m (Set k, c)
runSetBuilder h = newSetBuilder >>= flip rawRunBuilder h

evalSetBuilder :: (MonadIO m, Ord k) => (SetBuilder k -> m ()) -> m (Set k)
evalSetBuilder h = fmap fst (runSetBuilder h)

type HashMapBuilder k v = Builder (k, v) (HashMap k v)

newHashMapBuilder :: (MonadIO m, Eq k, Hashable k) => m (HashMapBuilder k v)
newHashMapBuilder = newBuilder (\m (k, v) -> HashMap.insert k v m) HashMap.empty

runHashMapBuilder :: (MonadIO m, Eq k, Hashable k) => (HashMapBuilder k v -> m c) -> m (HashMap k v, c)
runHashMapBuilder h = newHashMapBuilder >>= flip rawRunBuilder h

evalHashMapBuilder :: (MonadIO m, Eq k, Hashable k) => (HashMapBuilder k v -> m ()) -> m (HashMap k v)
evalHashMapBuilder h = fmap fst (runHashMapBuilder h)

type HashSetBuilder k = Builder k (HashSet k)

newHashSetBuilder :: (MonadIO m, Eq k, Hashable k) => m (HashSetBuilder k)
newHashSetBuilder = newBuilder (flip HashSet.insert) HashSet.empty

runHashSetBuilder :: (MonadIO m, Eq k, Hashable k) => (HashSetBuilder k -> m c) -> m (HashSet k, c)
runHashSetBuilder h = newHashSetBuilder >>= flip rawRunBuilder h

evalHashSetBuilder :: (MonadIO m, Eq k, Hashable k) => (HashSetBuilder k -> m ()) -> m (HashSet k)
evalHashSetBuilder h = fmap fst (runHashSetBuilder h)

type OrdMultiMapBuilder k v = Builder (k, v) (OrdMultiMap k v)

newOrdMultiMapBuilder :: (MonadIO m, Ord k, Ord v) => m (OrdMultiMapBuilder k v)
newOrdMultiMapBuilder = newBuilder (\m (k, v) -> insertOrdMultiMap k v m) Map.empty

runOrdMultiMapBuilder :: (MonadIO m, Ord k, Ord v) => (OrdMultiMapBuilder k v -> m c) -> m (OrdMultiMap k v, c)
runOrdMultiMapBuilder h = newOrdMultiMapBuilder >>= flip rawRunBuilder h

evalOrdMultiMapBuilder :: (MonadIO m, Ord k, Ord v) => (OrdMultiMapBuilder k v -> m ()) -> m (OrdMultiMap k v)
evalOrdMultiMapBuilder h = fmap fst (runOrdMultiMapBuilder h)

type HashMultiMapBuilder k v = Builder (k, v) (HashMultiMap k v)

newHashMultiMapBuilder :: (MonadIO m, Eq k, Hashable k, Eq v, Hashable v) => m (HashMultiMapBuilder k v)
newHashMultiMapBuilder = newBuilder (\m (k, v) -> insertHashMultiMap k v m) HashMap.empty

runHashMultiMapBuilder :: (MonadIO m, Eq k, Hashable k, Eq v, Hashable v) => (HashMultiMapBuilder k v -> m c) -> m (HashMultiMap k v, c)
runHashMultiMapBuilder h = newHashMultiMapBuilder >>= flip rawRunBuilder h

evalHashMultiMapBuilder :: (MonadIO m, Eq k, Hashable k, Eq v, Hashable v) => (HashMultiMapBuilder k v -> m ()) -> m (HashMultiMap k v)
evalHashMultiMapBuilder h = fmap fst (runHashMultiMapBuilder h)
