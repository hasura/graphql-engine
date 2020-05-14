module Data.List.Extended
       ( duplicates
       , module L
       ) where

import           Data.Hashable       (Hashable)
import           Prelude

import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet        as Set
import qualified Data.List           as L

duplicates :: (Eq a, Hashable a) => [a] -> Set.HashSet a
duplicates =
  Set.fromList . Map.keys . Map.filter (> 1) . Map.fromListWith (+) . map (,1::Int)
