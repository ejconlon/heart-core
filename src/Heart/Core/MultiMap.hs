module Heart.Core.MultiMap
  ( OrdMultiMap
  , insertOrdMultiMap
  , invertOrdMapWith
  , invertOrdMap
  , HashMultiMap
  , insertHashMultiMap
  , invertHashMapWith
  , invertHashMap
  ) where

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Heart.Core.Prelude

type OrdMultiMap k v = Map k (Set v)

insertOrdMultiMap :: (Ord k, Ord v) => k -> v -> OrdMultiMap k v -> OrdMultiMap k v
insertOrdMultiMap k v m =
  M.insert k (maybe (S.singleton v) (S.insert v) (M.lookup k m)) m

invertOrdMapWith :: (Monad m, Ord k, Ord u) => (v -> m u) -> Map k v -> m (OrdMultiMap u k)
invertOrdMapWith f m = foldM go M.empty (M.toList m) where
  go acc (k, v) = fmap (\u -> insertOrdMultiMap u k acc) (f v)

invertOrdMap :: (Ord k, Ord v) => Map k v -> OrdMultiMap v k
invertOrdMap = runIdentity . invertOrdMapWith Identity

type HashMultiMap k v = HashMap k (HashSet v)

insertHashMultiMap :: (Eq k, Hashable k, Eq v, Hashable v) => k -> v -> HashMultiMap k v -> HashMultiMap k v
insertHashMultiMap k v m =
  HM.insert k (maybe (HS.singleton v) (HS.insert v) (HM.lookup k m)) m

invertHashMapWith :: (Monad m, Eq k, Hashable k, Eq u, Hashable u) => (v -> m u) -> HashMap k v -> m (HashMultiMap u k)
invertHashMapWith f m = foldM go HM.empty (HM.toList m) where
  go acc (k, v) = fmap (\u -> insertHashMultiMap u k acc) (f v)

invertHashMap :: (Eq k, Hashable k, Eq v, Hashable v) => HashMap k v -> HashMultiMap v k
invertHashMap = runIdentity . invertHashMapWith Identity
