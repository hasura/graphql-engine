module Test.QuickCheck.Arbitrary.Extended
       ( arbitraryExcluding
       , distinctExcluding
       , distinctExcluding1
       , sublistOf1
       ) where

import           Data.Containers.ListUtils (nubOrd)
import           Hasura.Prelude
import           Test.QuickCheck

arbitraryExcluding :: (Arbitrary a, Eq a) => [a] -> Gen a
arbitraryExcluding exclusions = arbitrary `suchThat` (`notElem` exclusions)

distinctExcluding :: (Arbitrary a, Ord a) => [a] -> Gen [a]
distinctExcluding = fmap nubOrd . listOf . arbitraryExcluding

distinctExcluding1 :: (Arbitrary a, Ord a) => [a] -> Gen [a]
distinctExcluding1 = fmap nubOrd . listOf1 . arbitraryExcluding

sublistOf1 :: [a] -> Gen [a]
sublistOf1 xs = sublistOf xs `suchThat` (not . null)
