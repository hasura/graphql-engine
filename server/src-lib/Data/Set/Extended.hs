module Data.Set.Extended
  ( module S,
    intersects,
  )
where

import Data.Set as S
import Prelude

-- | Returns a boolean indicating whether the two sets intersect, i.e. whether
-- they have at least one element in common.
intersects :: Ord a => S.Set a -> S.Set a -> Bool
intersects a b = not $ S.null $ S.intersection a b
