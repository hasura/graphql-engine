{-# OPTIONS_HADDOCK hide #-}

-- | Helper functions for working with metric samples.
module System.Metrics.Prometheus.Internal.Sample
  ( fromList,
    insert,
    delete,
    lookup,
    member,
  )
where

import Data.List (foldl')
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import Prelude hiding (lookup)

fromList :: (Ord k1, Ord k2) => [(k1, k2, a, b)] -> M.Map k1 (a, M.Map k2 b)
fromList =
  foldl'
    (\m (k1, k2, a, b) -> insert k1 k2 a b m)
    M.empty

insert ::
  (Ord k1, Ord k2) =>
  k1 ->
  k2 ->
  a ->
  b ->
  M.Map k1 (a, M.Map k2 b) ->
  M.Map k1 (a, M.Map k2 b)
insert k1 k2 a b = M.alter insert_ k1
  where
    insert_ Nothing = Just $ (a, M.singleton k2 b)
    insert_ (Just (_, m)) = Just $ (a, M.insert k2 b m) -- Replace 'a'

-- | Delete a key from a sample. If this would cause a map to become empty,
-- remove it.
delete ::
  (Ord k1, Ord k2) =>
  k1 ->
  k2 ->
  M.Map k1 (a, M.Map k2 b) ->
  M.Map k1 (a, M.Map k2 b)
delete k1 k2 = M.update (traverse (nonEmptyMap . M.delete k2)) k1

nonEmptyMap :: M.Map k a -> Maybe (M.Map k a)
nonEmptyMap m
  | M.null m = Nothing
  | otherwise = Just m

lookup ::
  (Ord k1, Ord k2) => k1 -> k2 -> M.Map k1 (a, M.Map k2 b) -> Maybe b
lookup k1 k2 m = M.lookup k1 m >>= M.lookup k2 . snd

member ::
  (Ord k1, Ord k2) => k1 -> k2 -> M.Map k1 (a, M.Map k2 b) -> Bool
member k1 k2 m = isJust $ lookup k1 k2 m
