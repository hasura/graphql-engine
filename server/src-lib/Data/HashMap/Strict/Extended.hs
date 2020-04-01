module Data.HashMap.Strict.Extended
  ( module M
  , catMaybes
  , fromListOn
  , unionsAll
  , groupOn
  , groupOnNE
  , differenceOn
  , lpadZip
  ) where

import           Prelude

import qualified Data.Align          as A
import qualified Data.Foldable       as F

import           Data.Function
import           Data.Hashable
import           Data.HashMap.Strict as M
import           Data.List.NonEmpty  (NonEmpty (..))
import           Data.These

catMaybes :: HashMap k (Maybe v) -> HashMap k v
catMaybes = M.mapMaybe id

fromListOn :: (Eq k, Hashable k) => (v -> k) -> [v] -> HashMap k v
fromListOn f = fromList . Prelude.map (\v -> (f v, v))

-- | Like 'M.unions', but keeping all elements in the result.
unionsAll :: (Eq k, Hashable k, Foldable t) => t (HashMap k v) -> HashMap k (NonEmpty v)
unionsAll = F.foldl' (\a b -> unionWith (<>) a (fmap (:|[]) b)) M.empty

-- | Given a 'Foldable' sequence of values and a function that extracts a key from each value,
-- returns a 'HashMap' that maps each key to a list of all values in the sequence for which the
-- given function produced it.
--
-- >>> groupOn (take 1) ["foo", "bar", "baz"]
-- fromList [("f", ["foo"]), ("b", ["bar", "baz"])]
groupOn :: (Eq k, Hashable k, Foldable t) => (v -> k) -> t v -> HashMap k [v]
groupOn f = fmap F.toList . groupOnNE f

groupOnNE :: (Eq k, Hashable k, Foldable t) => (v -> k) -> t v -> HashMap k (NonEmpty v)
groupOnNE f = Prelude.foldr (\v -> M.alter (Just . (v:|) . maybe [] F.toList) (f v)) M.empty

differenceOn :: (Eq k, Hashable k, Foldable t) => (v -> k) -> t v -> t v -> HashMap k v
differenceOn f = M.difference `on` (fromListOn f . F.toList)

-- | Analogous to 'A.lpadZip', but on 'HashMap's instead of lists.
lpadZip :: (Eq k, Hashable k) => HashMap k a -> HashMap k b -> HashMap k (Maybe a, b)
lpadZip left = catMaybes . flip A.alignWith left \case
  This  _   -> Nothing
  That    b -> Just (Nothing, b)
  These a b -> Just (Just a,  b)
