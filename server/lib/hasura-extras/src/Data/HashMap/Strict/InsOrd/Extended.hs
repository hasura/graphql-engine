{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.HashMap.Strict.InsOrd.Extended
  ( module InsOrdHashMap,
    catMaybes,
    partition,
    alterF,
  )
where

import Data.HashMap.Strict.InsOrd as InsOrdHashMap
import Data.Hashable (Hashable)
import Witherable (Filterable (..))
import Prelude

instance Filterable (InsOrdHashMap.InsOrdHashMap k) where
  mapMaybe = InsOrdHashMap.mapMaybe
  filter = InsOrdHashMap.filter

partition :: (Hashable k) => (v -> Bool) -> InsOrdHashMap.InsOrdHashMap k v -> (InsOrdHashMap.InsOrdHashMap k v, InsOrdHashMap.InsOrdHashMap k v)
partition predicate =
  InsOrdHashMap.foldlWithKey'
    ( \(left, right) key val ->
        if (predicate val)
          then (InsOrdHashMap.insert key val left, right)
          else (left, InsOrdHashMap.insert key val right)
    )
    (mempty, mempty)

-- | Alter a hashmap using a function that can fail, in which case the entire operation fails.
-- (Maybe a version with the key also being passed to the function could be useful.)
alterF ::
  (Functor f, Hashable k) =>
  (Maybe v -> f (Maybe v)) ->
  k ->
  InsOrdHashMap k v ->
  f (InsOrdHashMap k v)
alterF f k m = alter' <$> f (InsOrdHashMap.lookup k m)
  where
    alter' = \case
      Nothing -> InsOrdHashMap.delete k m
      Just v -> InsOrdHashMap.insert k v m
