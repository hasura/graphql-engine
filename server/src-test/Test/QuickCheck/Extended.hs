module Test.QuickCheck.Extended
  ( -- * Helpers
    distinct,
    distinct1,
    arbitraryExcluding,
    distinctExcluding,
    distinctExcluding1,
    sublistOf1,

    -- * Re-exports
    module QuickCheck,
  )
where

-------------------------------------------------------------------------------

import Data.Containers.ListUtils (nubOrd)
import Test.QuickCheck as QuickCheck
import Prelude

-------------------------------------------------------------------------------

-- Quickcheck helpers

-- | Generates a list of random length with all duplicate elements removed.
--
-- NOTE: This will generate the list /and then/ remove the values without
-- making any guarantees as to the size or distribution of the final list.
distinct :: (Arbitrary a, Ord a) => Gen [a]
distinct = nubOrd <$> arbitrary

-- | Generates a non-empty list of random length with all duplicate elements
-- removed.
--
-- NOTE: This will generate the list /and then/ remove the values without
-- making any guarantees as to the size or distribution of the final list.
distinct1 :: (Arbitrary a, Ord a) => Gen [a]
distinct1 = nubOrd <$> listOf1 arbitrary

-- | Generates a value, exclusive of any values in the given list.
arbitraryExcluding :: (Arbitrary a, Eq a) => [a] -> Gen a
arbitraryExcluding exclusions = arbitrary `suchThat` (`notElem` exclusions)

-- | Generates a list of random length, with all duplicate elements removed,
-- exclusive of any values in the given list.
--
-- NOTE: This will generate the list /and then/ remove the values without
-- making any guarantees as to the size or distribution of the final list.
distinctExcluding :: (Arbitrary a, Ord a) => [a] -> Gen [a]
distinctExcluding = fmap nubOrd . listOf . arbitraryExcluding

-- | Generates a non-empty list of random length, with all duplicate elements
-- removed, exclusive of any values in the given list.
--
-- NOTE: This will generate the list /and then/ remove the values without
-- making any guarantees as to the size or distribution of the final list.
distinctExcluding1 :: (Arbitrary a, Ord a) => [a] -> Gen [a]
distinctExcluding1 = fmap nubOrd . listOf1 . arbitraryExcluding

-- | Generates a random, non-empty subsequence of the given list.
sublistOf1 :: [a] -> Gen [a]
sublistOf1 xs = sublistOf xs `suchThat` (not . null)
