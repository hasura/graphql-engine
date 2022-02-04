module Data.HashMap.Strict.Extended
  ( module M,
    catMaybes,
    fromListOn,
    unionsAll,
    groupOn,
    groupOnNE,
    differenceOn,
    lpadZip,
    mapKeys,
    unionsWith,
    isInverseOf,
  )
where

import Data.Align qualified as A
import Data.Foldable qualified as F
import Data.Function
import Data.HashMap.Strict as M
import Data.Hashable
import Data.List.NonEmpty (NonEmpty (..))
import Data.These
import Prelude

catMaybes :: HashMap k (Maybe v) -> HashMap k v
catMaybes = M.mapMaybe id

fromListOn :: (Eq k, Hashable k) => (v -> k) -> [v] -> HashMap k v
fromListOn f = fromList . Prelude.map (\v -> (f v, v))

-- | Like 'M.unions', but keeping all elements in the result.
unionsAll ::
  (Eq k, Hashable k, Foldable t) => t (HashMap k v) -> HashMap k (NonEmpty v)
unionsAll = F.foldl' (\a b -> unionWith (<>) a (fmap (:| []) b)) M.empty

-- | Given a 'Foldable' sequence of values and a function that extracts a key from each value,
-- returns a 'HashMap' that maps each key to a list of all values in the sequence for which the
-- given function produced it.
--
-- >>> groupOn (take 1) ["foo", "bar", "baz"]
-- fromList [("f", ["foo"]), ("b", ["bar", "baz"])]
groupOn :: (Eq k, Hashable k, Foldable t) => (v -> k) -> t v -> HashMap k [v]
groupOn f = fmap F.toList . groupOnNE f

groupOnNE ::
  (Eq k, Hashable k, Foldable t) => (v -> k) -> t v -> HashMap k (NonEmpty v)
groupOnNE f =
  Prelude.foldr
    (\v -> M.alter (Just . (v :|) . maybe [] F.toList) (f v))
    M.empty

differenceOn ::
  (Eq k, Hashable k, Foldable t) => (v -> k) -> t v -> t v -> HashMap k v
differenceOn f = M.difference `on` (fromListOn f . F.toList)

-- | Analogous to 'A.lpadZip', but on 'HashMap's instead of lists.
lpadZip ::
  (Eq k, Hashable k) => HashMap k a -> HashMap k b -> HashMap k (Maybe a, b)
lpadZip left =
  catMaybes . flip
    A.alignWith
    left
    \case
      This _ -> Nothing
      That b -> Just (Nothing, b)
      These a b -> Just (Just a, b)

-- | @'mapKeys' f s@ is the map obtained by applying @f@ to each key of @s@.
--
-- The size of the result may be smaller if @f@ maps two or more distinct
-- keys to the same new key.  In this case the value at the greatest of the
-- original keys is retained.
--
-- > mapKeys (+ 1) (fromList [(5,"a"), (3,"b")])                        == fromList [(4, "b"), (6, "a")]
-- > mapKeys (\ _ -> 1) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) == singleton 1 "c"
-- > mapKeys (\ _ -> 3) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) == singleton 3 "c"
--
-- copied from https://hackage.haskell.org/package/containers-0.6.4.1/docs/src/Data.Map.Internal.html#mapKeys
mapKeys :: (Ord k2, Hashable k2) => (k1 -> k2) -> HashMap k1 a -> HashMap k2 a
mapKeys f = fromList . foldrWithKey (\k x xs -> (f k, x) : xs) []

-- | The union of a list of maps, with a combining operation:
--   (@'unionsWith' f == 'Prelude.foldl' ('unionWith' f) 'empty'@).
--
-- > unionsWith (++) [(fromList [(5, "a"), (3, "b")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "A3"), (3, "B3")])]
-- >     == fromList [(3, "bB3"), (5, "aAA3"), (7, "C")]
--
-- copied from https://hackage.haskell.org/package/containers-0.6.4.1/docs/src/Data.Map.Internal.html#unionsWith
unionsWith ::
  (Foldable f, Hashable k, Ord k) =>
  (a -> a -> a) ->
  f (HashMap k a) ->
  HashMap k a
unionsWith f ts = F.foldl' (unionWith f) empty ts

-- | Determines whether the left-hand-side and the right-hand-side are inverses of each other.
--
-- More specifically, for two maps @A@ and @B@, 'isInverseOf' is satisfied when both of the
-- following are true:
-- 1. @∀ key ∈ A. A[key] ∈  B ∧ B[A[key]] == key@
-- 2. @∀ key ∈ B. B[key] ∈  A ∧ A[B[key]] == key@
isInverseOf ::
  (Eq k, Hashable k, Eq v, Hashable v) => HashMap k v -> HashMap v k -> Bool
lhs `isInverseOf` rhs = lhs `invertedBy` rhs && rhs `invertedBy` lhs
  where
    invertedBy ::
      forall s t.
      (Eq s, Eq t, Hashable t) =>
      HashMap s t ->
      HashMap t s ->
      Bool
    a `invertedBy` b = and $ do
      (k, v) <- M.toList a
      pure $ M.lookup v b == Just k
