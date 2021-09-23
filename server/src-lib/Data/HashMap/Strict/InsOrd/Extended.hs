module Data.HashMap.Strict.InsOrd.Extended
  ( module OMap,
    groupTuples,
    groupListWith,
    partition,
  )
where

import Data.HashMap.Strict.InsOrd as OMap
import Data.Hashable (Hashable)
import Data.List qualified as L
import Data.Sequence.NonEmpty qualified as NE
import Prelude

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
