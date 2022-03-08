module Data.List.Extended
  ( duplicates,
    uniques,
    getDifference,
    getDifferenceOn,
    getOverlapWith,
    hasNoDuplicates,
    longestCommonPrefix,
    appendToNonEmpty,
    singleton,
    module L,
  )
where

import Data.Function (on)
import Data.HashMap.Strict.Extended qualified as Map
import Data.HashSet qualified as Set
import Data.Hashable (Hashable)
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Set qualified as S
import Prelude

duplicates :: (Eq a, Hashable a) => [a] -> Set.HashSet a
duplicates =
  Set.fromList . Map.keys . Map.filter (> 1) . Map.fromListWith (+) . map (,1 :: Int)

uniques :: (Ord a) => [a] -> [a]
uniques = S.toList . S.fromList

getDifference :: (Eq a, Hashable a) => [a] -> [a] -> Set.HashSet a
getDifference = Set.difference `on` Set.fromList

getDifferenceOn :: (Eq k, Hashable k) => (v -> k) -> [v] -> [v] -> [v]
getDifferenceOn f l = Map.elems . Map.differenceOn f l

getOverlapWith :: (Eq k, Hashable k) => (v -> k) -> [v] -> [v] -> [(v, v)]
getOverlapWith getKey left right =
  Map.elems $ Map.intersectionWith (,) (mkMap left) (mkMap right)
  where
    mkMap = Map.fromList . map (\v -> (getKey v, v))

hasNoDuplicates :: (Eq a, Hashable a) => [a] -> Bool
hasNoDuplicates xs = Set.size (Set.fromList xs) == length xs

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

-- | As of base-4.15.0.0 (GHC > 9) singleton now exists in Data.List so we should be able to remove this when we upgrade.
singleton :: a -> [a]
singleton = pure
