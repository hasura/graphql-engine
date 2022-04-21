module Data.HashMap.Strict.Extended
  ( module M,
    catMaybes,
    fromListOn,
    groupOn,
    groupOnNE,
    differenceOn,
    lpadZip,
    insertWithM,
    isInverseOf,
    unionWithM,
    unionsAll,
    unionsWith,
    homogenise,
  )
where

import Control.Monad (foldM)
import Data.Align qualified as A
import Data.Foldable qualified as F
import Data.Function (on)
import Data.HashMap.Strict as M
import Data.HashSet (HashSet)
import Data.HashSet qualified as S
import Data.Hashable (Hashable)
import Data.List qualified as L
import Data.List.NonEmpty (NonEmpty (..))
import Data.These (These (That, These, This))
import Prelude

catMaybes :: HashMap k (Maybe v) -> HashMap k v
catMaybes = M.mapMaybe id

fromListOn :: (Eq k, Hashable k) => (v -> k) -> [v] -> HashMap k v
fromListOn f = fromList . Prelude.map (\v -> (f v, v))

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

-- | Monadic version of https://hackage.haskell.org/package/unordered-containers-0.2.18.0/docs/Data-HashMap-Internal.html#v:insertWith
insertWithM :: (Monad m, Hashable k, Eq k) => (v -> v -> m v) -> k -> v -> HashMap k v -> m (HashMap k v)
insertWithM f k v m =
  sequence $
    M.insertWith
      ( \a b -> do
          x <- a
          y <- b
          f x y
      )
      k
      (return v)
      (return <$> m)

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

-- | The union of two maps.
--
-- If a key occurs in both maps, the provided function (first argument) will be
-- used to compute the result. Unlike 'unionWith', 'unionWithA' performs the
-- computation in an arbitratry monad.
unionWithM ::
  (Monad m, Eq k, Hashable k) =>
  (v -> v -> m v) ->
  HashMap k v ->
  HashMap k v ->
  m (HashMap k v)
unionWithM f m1 m2 = foldM step m1 (toList m2)
  where
    step m (k, new) = case M.lookup k m of
      Nothing -> pure $ insert k new m
      Just old -> do
        combined <- f new old
        pure $ insert k combined m

-- | Like 'M.unions', but keeping all elements in the result.
unionsAll ::
  (Eq k, Hashable k, Foldable t) => t (HashMap k v) -> HashMap k (NonEmpty v)
unionsAll = F.foldl' (\a b -> unionWith (<>) a (fmap (:| []) b)) M.empty

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

-- | Homogenise maps, such that all maps range over the full set of
-- keys, inserting a default value as needed.
homogenise :: (Hashable a, Eq a) => b -> [HashMap a b] -> (HashSet a, [HashMap a b])
homogenise defaultValue maps =
  let ks = S.unions $ L.map keysSet maps
      defaults = fromList [(k, defaultValue) | k <- S.toList ks]
   in (ks, L.map (<> defaults) maps)
