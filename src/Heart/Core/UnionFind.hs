module Heart.Core.UnionFind
  ( PartitionAlloc
  , Term
  , newPartitionAlloc
  , newTerm
  , findTermRank
  , findTerm
  , unionTerm
  , decideEqTerm
  , hashSplitClasses
  , ordSplitClasses
  ) where

import Data.Function (on)
import Heart.Core.Alloc
import Heart.Core.MultiMap
import Heart.Core.Prelude
import UnliftIO.IORef (IORef, newIORef, readIORef, writeIORef)

newtype Partition = Partition { unPartition :: Int } deriving (Eq, Ord, Enum, Show, Hashable)

-- From Content/Term encoding simplified from
--- https://github.com/ekmett/guanxi/blob/master/src/Equality.hs
data Content
  = Root !Int
  | Child !Term

data Term = Term
  { _termPartition :: !Partition
  , _termParent :: !(IORef Content)
  }

instance Eq Term where
  (==) = (==) `on` _termPartition

instance Ord Term where
  (<=) = (<=) `on` _termPartition

instance Hashable Term where
  hash = hash . _termPartition
  hashWithSalt d = hashWithSalt d . _termPartition

newtype PartitionAlloc = PartitionAlloc { unPartitionAlloc :: Alloc Partition }

newPartitionAlloc :: MonadIO m => m PartitionAlloc
newPartitionAlloc = fmap PartitionAlloc newEnumAlloc

newTerm :: MonadIO m => PartitionAlloc -> m Term
newTerm (PartitionAlloc alloc) = do
  p <- incAlloc alloc
  r <- newIORef (Root 0)
  pure (Term p r)

findTermRank :: MonadIO m => Term -> m (Int, Term)
findTermRank t@(Term _ r) = do
  y <- readIORef r
  case y of
    Root i -> pure (i, t)
    Child s -> do
      z@(_, q) <- findTermRank s
      writeIORef r (Child q)
      pure z

findTerm :: MonadIO m => Term -> m Term
findTerm = fmap snd . findTermRank

unionTerm :: MonadIO m => Term -> Term -> m ()
unionTerm m n = do
  (mrank, mroot) <- findTermRank m
  (nrank, nroot) <- findTermRank n
  let mref = _termParent mroot
      nref = _termParent nroot
  case compare mrank nrank of
    LT -> do
      writeIORef mref (Child nroot)
      writeIORef nref (Root nrank)
    GT -> do
      writeIORef nref (Child mroot)
      writeIORef mref (Root mrank)
    EQ -> do
      writeIORef mref (Child nroot)
      writeIORef nref (Root (nrank + 1))

decideEqTerm :: MonadIO m => Term -> Term -> m Bool
decideEqTerm m n = (==) <$> findTerm m <*> findTerm n

hashSplitClasses :: (Eq k, Hashable k, MonadIO m) => HashMap k Term -> m (HashMultiMap Term k)
hashSplitClasses = invertHashMapWith findTerm

ordSplitClasses :: (Ord k, MonadIO m) => Map k Term -> m (OrdMultiMap Term k)
ordSplitClasses = invertOrdMapWith findTerm
