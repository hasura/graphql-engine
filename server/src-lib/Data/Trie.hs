-- | Prefix trees on arbitrary keys.
module Data.Trie
  ( -- * Type
    Trie (..),

    -- * Construction
    empty,
    singleton,

    -- * Basic interface
    lookup,
    insert,
    insertWith,
    elems,
  )
where

import Data.Aeson (ToJSON, ToJSONKey)
import Data.HashMap.Strict qualified as M
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Prelude hiding (lookup)

-------------------------------------------------------------------------------

-- | Data structure for storing a value @v@ keyed on a sequence of @k@s
data Trie k v = Trie
  { trieMap :: M.HashMap k (Trie k v),
    trieData :: Maybe v
  }
  deriving stock (Eq, Show, Ord, Generic)

-- | Semigroup via union.
-- The resulting 'Trie' will contain all paths present in either tries. If both
-- tries contain a value at a given path, we use the value's semigroup instance
-- to compute the resulting value.
instance (Eq k, Hashable k, Semigroup v) => Semigroup (Trie k v) where
  Trie m0 v0 <> Trie m1 v1 = Trie (M.unionWith (<>) m0 m1) (v0 <> v1)

instance (Eq k, Hashable k, Semigroup v) => Monoid (Trie k v) where
  mempty = empty

instance (ToJSONKey a, ToJSON v) => ToJSON (Trie a v)

-------------------------------------------------------------------------------

-- | Construct an empty trie.
empty :: Trie k v
empty = Trie M.empty Nothing

-- | Creates a trie from a path and a value
--
-- >>> singleton ["a", "b"] 5
-- Trie (fromList [("a", Trie (fromList [("b", Trie (fromList []) (Just 5))]) Nothing)]) Nothing
--
-- >>> singleton [] 5
-- Trie (fromList []) (Just 5)
singleton :: (Hashable k) => [k] -> v -> Trie k v
singleton ps v = foldr (\p t -> Trie (M.singleton p t) Nothing) (Trie M.empty (Just v)) ps

-------------------------------------------------------------------------------

-- | Find a value at the given path, if any.
lookup :: (Eq k, Hashable k) => [k] -> Trie k v -> Maybe v
lookup [] (Trie _ value) = value
lookup (p : ps) (Trie tmap _) = lookup ps =<< M.lookup p tmap

-- | Insert the given value at the given path.
--
-- If there's already a value at the given path, it is replaced.
insert :: (Eq k, Hashable k) => [k] -> v -> Trie k v -> Trie k v
insert = insertWith const

-- | Insert the value at the given path.
--
-- If there's already a value at the given path, the old value is replaced by
-- the result of applying the given function to the new and old value.
insertWith :: (Eq k, Hashable k) => (v -> v -> v) -> [k] -> v -> Trie k v -> Trie k v
insertWith fun path newValue t = go t path
  where
    go (Trie tmap value) = \case
      [] -> Trie tmap $
        Just $ case value of
          Nothing -> newValue
          Just oldValue -> fun newValue oldValue
      (p : ps) -> Trie (M.alter (step ps) p tmap) value
    step ps = \case
      -- this path did not exist and must be created
      Nothing -> Just $ go empty ps
      -- we found the path
      Just st -> Just $ go st ps

-- | Extract all values of the trie, discarding any path information.
elems :: Trie k v -> [v]
elems (Trie m v) =
  let subElems = concatMap elems (M.elems m)
   in case v of
        Nothing -> subElems
        Just val -> val : subElems
