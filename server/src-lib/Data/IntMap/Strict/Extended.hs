module Data.IntMap.Strict.Extended
  ( module M,
    catMaybes,
  )
where

import Data.IntMap.Strict as M
import Prelude

catMaybes :: IntMap (Maybe v) -> IntMap v
catMaybes = M.mapMaybe id
