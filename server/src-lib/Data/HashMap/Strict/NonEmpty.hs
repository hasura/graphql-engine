-- | Non-empty hash maps.
module Data.HashMap.Strict.NonEmpty
  ( -- * Type
    NEHashMap,

    -- * Construction and conversions
    singleton,
    fromHashMap,
    fromList,
    fromNonEmpty,
    toHashMap,
    toList,
    toNonEmpty,

    -- * Basic interface
    lookup,
    (!?),
    keys,

    -- * Combine
    union,
    unionWith,

    -- * Transformations
    mapKeys,

    -- * Predicates
    isInverseOf,
  )
where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as M
import Data.HashMap.Strict.Extended qualified as Extended
import Data.Hashable (Hashable)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Prelude hiding (lookup)

-------------------------------------------------------------------------------

-- | A non-empty hashmap is a wrapper around a normal hashmap, that
-- only provides a restricted set of functionalities. It doesn't
-- provide a 'Monoid' instance, nor an 'empty' function.
newtype NEHashMap k v = NEHashMap {unNEHashMap :: HashMap k v}
  deriving newtype (Show, Eq, FromJSON, Hashable, NFData, Ord, Semigroup, ToJSON)
  deriving stock (Functor, Foldable, Traversable)

-------------------------------------------------------------------------------

-- | Construct a non-empty map with a single element.
singleton :: Hashable k => k -> v -> NEHashMap k v
singleton k v = NEHashMap $ M.singleton k v

-- | Construct a non-empty map with the supplied mappings.
-- Returns 'Nothing' if the provided 'HashMap' is empty.
fromHashMap :: HashMap k v -> Maybe (NEHashMap k v)
fromHashMap m
  | M.null m = Nothing
  | otherwise = Just $ NEHashMap m

-- | Construct a non-empty map with the supplied mappings as follows:
--
-- * if the provided list contains duplicate mappings, the later mappings take
--   precedence;
-- * if the provided list is empty, returns 'Nothing'.
fromList :: (Eq k, Hashable k) => [(k, v)] -> Maybe (NEHashMap k v)
fromList [] = Nothing
fromList v = Just $ NEHashMap $ M.fromList v

-- | A variant of 'fromList' that uses 'NonEmpty' inputs.
fromNonEmpty :: (Eq k, Hashable k) => NonEmpty (k, v) -> NEHashMap k v
fromNonEmpty (x NE.:| xs) = NEHashMap (M.fromList (x : xs))

-- | Convert a non-empty map to a 'HashMap'.
toHashMap :: NEHashMap k v -> HashMap k v
toHashMap = unNEHashMap

-- | Convert a non-empty map to a non-empty list of key/value pairs. The closed
-- operations of 'NEHashMap' guarantee that this operation won't fail.
toNonEmpty :: NEHashMap k v -> NonEmpty (k, v)
toNonEmpty = NE.fromList . M.toList . unNEHashMap

-- | Convert a non-empty map to a list of key/value pairs.
toList :: NEHashMap k v -> [(k, v)]
toList = M.toList . unNEHashMap

-------------------------------------------------------------------------------

-- | Return the value to which the specified key is mapped, or 'Nothing' if
-- this map contains no mapping for the key.
lookup :: (Eq k, Hashable k) => k -> NEHashMap k v -> Maybe v
lookup k (NEHashMap m) = M.lookup k m

-- | Return the value to which the specified key is mapped, or 'Nothing' if
-- this map contains no mapping for the key.
--
-- This is a flipped version of 'lookup'.
(!?) :: (Eq k, Hashable k) => NEHashMap k v -> k -> Maybe v
(!?) = flip lookup

-- | Return a list of this map's keys.
keys :: NEHashMap k v -> [k]
keys = M.keys . unNEHashMap

-------------------------------------------------------------------------------

-- | The union of two maps.
--
-- If a key occurs in both maps, the left map @m1@ (first argument) will be
-- preferred.
union :: (Eq k, Hashable k) => NEHashMap k v -> NEHashMap k v -> NEHashMap k v
union (NEHashMap m1) (NEHashMap m2) = NEHashMap $ M.union m1 m2

-- | The union of two maps using a given value-wise union function.
--
-- If a key occurs in both maps, the provided function (first argument) will be
-- used to compute the result.
unionWith :: (Eq k, Hashable k) => (v -> v -> v) -> NEHashMap k v -> NEHashMap k v -> NEHashMap k v
unionWith fun (NEHashMap m1) (NEHashMap m2) = NEHashMap $ M.unionWith fun m1 m2

-------------------------------------------------------------------------------

-- | @'mapKeys' f s@ is the map obtained by applying @f@ to each key of @s@.
--
-- The size of the result may be smaller if f maps two or more distinct keys to
-- the same new key. In this case there is no guarantee which of the associated
-- values is chosen for the conflicting key.
mapKeys :: (Eq k2, Hashable k2) => (k1 -> k2) -> NEHashMap k1 v -> NEHashMap k2 v
mapKeys fun (NEHashMap m) = NEHashMap $ M.mapKeys fun m

-------------------------------------------------------------------------------

-- | Determines whether the left-hand-side and the right-hand-side are inverses of each other.
--
-- More specifically, for two maps @A@ and @B@, 'isInverseOf' is satisfied when both of the
-- following are true:
-- 1. @∀ key ∈ A. A[key] ∈  B ∧ B[A[key]] == key@
-- 2. @∀ key ∈ B. B[key] ∈  A ∧ A[B[key]] == key@
isInverseOf ::
  (Eq k, Hashable k, Eq v, Hashable v) => NEHashMap k v -> NEHashMap v k -> Bool
lhs `isInverseOf` rhs = toHashMap lhs `Extended.isInverseOf` toHashMap rhs
