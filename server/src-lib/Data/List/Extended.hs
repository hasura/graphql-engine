module Data.List.Extended
  ( duplicates,
    uniques,
    getDifference,
    getDifferenceOn,
    getOverlapWith,
    longestCommonPrefix,
    appendToNonEmpty,
    module L,
  )
where

import Data.Containers.ListUtils (nubOrd)
import Data.Function (on)
import Data.HashMap.Strict.Extended qualified as Map
import Data.HashSet qualified as Set
import Data.Hashable (Hashable)
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Prelude

duplicates :: (Eq a, Hashable a) => [a] -> Set.HashSet a
duplicates =
  Set.fromList . Map.keys . Map.filter (> 1) . Map.fromListWith (+) . map (,1 :: Int)

-- | Remove duplicates from a list. Like 'nub' but runs in @O(n * log(n))@
--   time and requires 'Ord' instances.
-- >>> uniques [1,3,2,9,4,1,5,7,3,3,1,2,5,4,3,2,1,0]
-- [0,1,2,3,4,5,7,9]
uniques :: (Ord a) => [a] -> [a]
uniques = nubOrd

getDifference :: (Eq a, Hashable a) => [a] -> [a] -> Set.HashSet a
getDifference = Set.difference `on` Set.fromList

getDifferenceOn :: (Eq k, Hashable k) => (v -> k) -> [v] -> [v] -> [v]
getDifferenceOn f l = Map.elems . Map.differenceOn f l

getOverlapWith :: (Eq k, Hashable k) => (v -> k) -> [v] -> [v] -> [(v, v)]
getOverlapWith getKey left right =
  Map.elems $ Map.intersectionWith (,) (mkMap left) (mkMap right)
  where
    mkMap = Map.fromList . map (\v -> (getKey v, v))

-- | Returns the longest prefix common to all given lists. Returns an empty list on an empty list.
--
-- >>> longestCommonPrefix ["abcd", "abce", "abgh"]
-- "ab"
--
-- >>> longestCommonPrefix []
-- []
longestCommonPrefix :: Eq a => [[a]] -> [a]
longestCommonPrefix [] = []
longestCommonPrefix (x : xs) = foldr prefix x xs
  where
    prefix l1 l2 = map fst $ takeWhile (uncurry (==)) $ zip l1 l2

appendToNonEmpty :: NE.NonEmpty a -> [a] -> NE.NonEmpty a
appendToNonEmpty (neHead NE.:| neList) list =
  neHead NE.:| (neList <> list)
