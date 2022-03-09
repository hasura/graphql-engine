module Data.HashMap.Strict.InsOrd.Extended
  ( module OMap,
    catMaybes,
    groupTuples,
    groupListWith,
    partition,
    alterF,
  )
where

import Data.HashMap.Strict.InsOrd as OMap
import Data.Hashable (Hashable)
import Data.List qualified as L
import Data.Sequence.NonEmpty qualified as NE
import Prelude

catMaybes :: InsOrdHashMap k (Maybe v) -> InsOrdHashMap k v
catMaybes = OMap.mapMaybe id

groupTuples ::
  (Eq k, Hashable k, Foldable t) =>
  t (k, v) ->
  OMap.InsOrdHashMap k (NE.NESeq v)
groupTuples =
  L.foldl' groupFlds OMap.empty
  where
    groupFlds m (k, v) =
      OMap.insertWith (flip (<>)) k (NE.singleton v) m

groupListWith ::
  (Eq k, Hashable k, Foldable t, Functor t) =>
  (v -> k) ->
  t v ->
  OMap.InsOrdHashMap k (NE.NESeq v)
groupListWith f l =
  groupTuples $ fmap (\v -> (f v, v)) l

partition :: (Eq k, Hashable k) => (v -> Bool) -> OMap.InsOrdHashMap k v -> (OMap.InsOrdHashMap k v, OMap.InsOrdHashMap k v)
partition predicate =
  OMap.foldlWithKey'
    ( \(left, right) key val ->
        if (predicate val)
          then (OMap.insert key val left, right)
          else (left, OMap.insert key val right)
    )
    (mempty, mempty)

-- | Alter a hashmap using a function that can fail, in which case the entire operation fails.
-- (Maybe a version with the key also being passed to the function could be useful.)
alterF ::
  (Functor f, Eq k, Hashable k) =>
  (Maybe v -> f (Maybe v)) ->
  k ->
  InsOrdHashMap k v ->
  f (InsOrdHashMap k v)
alterF f k m = alter' <$> f (OMap.lookup k m)
  where
    alter' = \case
      Nothing -> OMap.delete k m
      Just v -> OMap.insert k v m
