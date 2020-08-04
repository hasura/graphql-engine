module Data.List.Extended
       ( duplicates
       , uniques
       , module L
       ) where

import           Data.Hashable       (Hashable)
import           Prelude

import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet        as Set
import qualified Data.List           as L
import qualified Data.List.NonEmpty  as NE

duplicates :: (Eq a, Hashable a) => [a] -> Set.HashSet a
duplicates =
  Set.fromList . Map.keys . Map.filter (> 1) . Map.fromListWith (+) . map (,1::Int)

uniques :: Eq a => [a] -> [a]
uniques = map NE.head . NE.group
