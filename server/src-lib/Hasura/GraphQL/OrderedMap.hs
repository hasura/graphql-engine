{-# LANGUAGE NoImplicitPrelude #-}

-- TODO: use insert-ordered-containers

module Hasura.GraphQL.OrderedMap
  ( empty
  , elems
  , insert
  , lookup
  , toList
  , OMap
  , groupTuples
  , groupListWith
  ) where

import qualified Data.HashMap.Strict        as Map
import           Hasura.Prelude             hiding (toList)

import qualified Hasura.GraphQL.NonEmptySeq as NE

newtype OVal v =
  OVal { _unOVal :: (Int, v) }
  deriving (Show)

getI :: OVal v -> Int
getI = fst . _unOVal

getV :: OVal v -> v
getV = snd . _unOVal

setV :: (v -> v) -> OVal v -> OVal v
setV f (OVal (i, v)) = OVal (i, f v)

newtype OMap k v =
  OMap { _unOMap :: (Int, Map.HashMap k (OVal v)) }
  deriving (Show)

empty :: OMap k v
empty = OMap (0, Map.empty)

insert :: (Eq k, Hashable k) => k -> v -> OMap k v -> OMap k v
insert k v (OMap (index, m)) =
  OMap (index + 1, Map.insert k (OVal (index, v)) m)

insertWith
  :: (Eq k, Hashable k)
  => (Maybe v -> v) -> k -> OMap k v -> OMap k v
insertWith f k oMap@(OMap (index, m)) =
  case Map.lookup k m of
  Just ov ->
    let newVal = setV (f . Just) ov
    in OMap (index, Map.insert k newVal m)
  Nothing ->
    insert k (f Nothing) oMap

toList :: OMap k v -> [(k, v)]
toList (OMap (_, m)) =
  [ (k, getV ov) | (k, ov) <- orderedList]
  where
    orderedList =
      sortBy (comparing (getI . snd)) $ Map.toList m

elems :: OMap k v -> [v]
elems = map snd . toList

lookup :: (Hashable k, Eq k) => k -> OMap k a -> Maybe a
lookup k (OMap (_, m)) =
  getV <$> Map.lookup k m

groupTuples
  :: (Eq k, Hashable k, Foldable t)
  => t (k, v) -> OMap k (NE.NESeq v)
groupTuples =
  foldl' groupFlds empty
  where
    groupFlds m (k, v) =
      insertWith ( maybe (NE.init v) (NE.|> v) ) k m

groupListWith
  :: (Eq k, Hashable k, Foldable t, Functor t)
  => (v -> k) -> t v -> OMap k (NE.NESeq v)
groupListWith f l =
  groupTuples $ fmap (\v -> (f v, v)) l
