-- | HashMap to multiple values.
module Data.HashMap.Strict.Multi
  ( -- * Type
    MultiMap,

    -- * Construction and conversions
    singleton,
    fromMap,
    toMap,
    fromList,
    toList,

    -- * Basic interface
    lookup,
    insert,
    keys,
    elems,
  )
where

import Data.Aeson (ToJSON)
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe)
import Data.Set qualified as S
import Prelude hiding (lookup)

-------------------------------------------------------------------------------

-- | Map from keys to sets of values
newtype MultiMap k v = MultiMap
  { unMultiMap :: HashMap.HashMap k (S.Set v)
  }
  deriving newtype (Eq, Show, ToJSON)

instance (Hashable k, Ord v) => Semigroup (MultiMap k v) where
  MultiMap m0 <> MultiMap m1 = MultiMap $ HashMap.unionWith S.union m0 m1

instance (Hashable k, Ord v) => Monoid (MultiMap k v) where
  mempty = MultiMap mempty

-------------------------------------------------------------------------------

-- | Construct a 'MmultiMap' with a single key, to which only one
-- value is associated.
singleton :: (Hashable k) => k -> v -> MultiMap k v
singleton k v = MultiMap $ HashMap.singleton k (S.singleton v)

-- | Construct a 'MultiMap' with the supplied mappings.
fromMap :: HashMap.HashMap k (S.Set v) -> MultiMap k v
fromMap = MultiMap

-- | Convert a 'MultiMap' to a 'HashMap'.
toMap :: MultiMap k v -> HashMap.HashMap k (S.Set v)
toMap = unMultiMap

-- | Creates a 'MultiMap' from an association list.
--
-- If the provided list constains duplicate mappings, the resulting
-- 'MultiMap' will store the set of all mapped values for each
-- duplicate key.
fromList :: (Hashable k, Ord v) => [(k, v)] -> MultiMap k v
fromList l = MultiMap $ HashMap.fromListWith (S.union) $ map (fmap S.singleton) l

-- | Creates an association list from a 'MultiMap'.
--
-- Each set of values associated with a given key is transformed back
-- into a list.
toList :: MultiMap k v -> [(k, [v])]
toList (MultiMap m) = HashMap.toList $ fmap (S.toList) m

-------------------------------------------------------------------------------

-- | Return the value to which the specified key is mapped, or 'Nothing' if
-- this map contains no mapping for the key.
lookup :: (Hashable k) => k -> MultiMap k v -> S.Set v
lookup k (MultiMap m) = fromMaybe S.empty $ HashMap.lookup k m

-- | Associate the specified value with the specified key in this map.
--
-- If this map previously contained a mapping for the key, the new value is
-- inserted in the set, and does not replace the previous mapping.
insert :: (Hashable k, Ord v) => k -> v -> MultiMap k v -> MultiMap k v
insert k v (MultiMap m) = MultiMap $ HashMap.insertWith (S.union) k (S.singleton v) m

-- | Returns a list of this map's keys.
keys :: MultiMap k v -> [k]
keys = HashMap.keys . unMultiMap

-- | Returns a list of this map's set of values.
elems :: MultiMap k v -> [S.Set v]
elems = HashMap.elems . unMultiMap
