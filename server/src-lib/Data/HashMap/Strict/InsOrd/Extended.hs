module Data.HashMap.Strict.InsOrd.Extended
  ( OMap.elems
  , groupTuples
  , groupListWith
  ) where

import qualified Data.HashMap.Strict.InsOrd as OMap
import qualified Data.Sequence.NonEmpty     as NE

import           Data.Hashable              (Hashable)
import           Data.List                  (foldl')

import           Prelude                    (Eq, Foldable, Functor, fmap, ($))

groupTuples
  :: (Eq k, Hashable k, Foldable t)
  => t (k, v) -> OMap.InsOrdHashMap k (NE.NESeq v)
groupTuples =
  foldl' groupFlds OMap.empty
  where
    groupFlds m (k, v) =
      OMap.insertWith (\_ c -> c NE.|> v) k (NE.singleton v) m

groupListWith
  :: (Eq k, Hashable k, Foldable t, Functor t)
  => (v -> k) -> t v -> OMap.InsOrdHashMap k (NE.NESeq v)
groupListWith f l =
  groupTuples $ fmap (\v -> (f v, v)) l
