{-# OPTIONS_HADDOCK hide #-}

-- | Helper functions for working with maps of maps.
module System.Metrics.Prometheus.Internal.Map2
  ( fromList,
    delete,
    nonEmptyMap,
    lookup,
  )
where

import Data.List (foldl')
import qualified Data.Map.Strict as M
import Prelude hiding (lookup)

fromList :: (Ord k1, Ord k2) => [(k1, k2, a)] -> M.Map k1 (M.Map k2 a)
fromList =
  foldl'
    (\m (name, labels, a) -> insert name labels a m)
    M.empty

insert ::
  (Ord k1, Ord k2) =>
  k1 ->
  k2 ->
  a ->
  M.Map k1 (M.Map k2 a) ->
  M.Map k1 (M.Map k2 a)
insert k1 k2 a = M.alter insert_ k1
  where
    insert_ Nothing = Just $ M.singleton k2 a
    insert_ (Just m) = Just $ M.insert k2 a m

-- | Delete a key from a map-within-a-map. If this would cause the map
-- to become empty, remove the map.
delete ::
  (Ord k1, Ord k2) =>
  k1 ->
  k2 ->
  M.Map k1 (M.Map k2 a) ->
  M.Map k1 (M.Map k2 a)
delete k1 k2 = M.update (nonEmptyMap . M.delete k2) k1

nonEmptyMap :: M.Map k a -> Maybe (M.Map k a)
nonEmptyMap m
  | M.null m = Nothing
  | otherwise = Just m

lookup ::
  (Ord k1, Ord k2) => k1 -> k2 -> M.Map k1 (M.Map k2 a) -> Maybe a
lookup k1 k2 m = M.lookup k1 m >>= M.lookup k2
