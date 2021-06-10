module Data.List.Extended
       ( duplicates
       , uniques
       , getDifference
       , getDifferenceOn
       , getOverlapWith
       , module L
       ) where

import           Data.Function                (on)
import           Data.Hashable                (Hashable)
import           Prelude

import qualified Data.HashMap.Strict.Extended as Map
import qualified Data.HashSet                 as Set
import qualified Data.List                    as L
import qualified Data.List.NonEmpty           as NE

duplicates :: (Eq a, Hashable a) => [a] -> Set.HashSet a
duplicates =
  Set.fromList . Map.keys . Map.filter (> 1) . Map.fromListWith (+) . map (,1::Int)

uniques :: Eq a => [a] -> [a]
uniques = map NE.head . NE.group

getDifference :: (Eq a, Hashable a) => [a] -> [a] -> Set.HashSet a
getDifference = Set.difference `on` Set.fromList

getDifferenceOn :: (Eq k, Hashable k) => (v -> k) -> [v] -> [v] -> [v]
getDifferenceOn f l = Map.elems . Map.differenceOn f l

getOverlapWith :: (Eq k, Hashable k) => (v -> k) -> [v] -> [v] -> [(v, v)]
getOverlapWith getKey left right =
  Map.elems $ Map.intersectionWith (,) (mkMap left) (mkMap right)
  where
    mkMap = Map.fromList . map (\v -> (getKey v, v))
