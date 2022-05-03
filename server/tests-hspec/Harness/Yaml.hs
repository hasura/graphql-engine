-- | Utility functions related to yaml
module Harness.Yaml
  ( combinationsObject,
    fromObject,
  )
where

import Data.Aeson
  ( Object,
    Value (..),
  )
import Data.Vector qualified as V
import Data.Word (Word8)
import Hasura.Prelude

fromObject :: Value -> Object
fromObject (Object x) = x
fromObject v = error $ "fromObject: Expected object, received" <> show v

-- | Compute all variations of an object and construct a list of
-- 'Value' based on the higher order function that is passed to it.  A
-- single variation of 'Object' is constructed as an 'Array' before
-- it's transformed by the passed function.
--
-- Typical usecase of this function is to use it with
-- 'shouldReturnOneOfYaml' function.
combinationsObject :: (Value -> Value) -> [Object] -> [Value]
combinationsObject fn variants =
  let nSubsets :: [[Object]]
      nSubsets = subsets (fromIntegral $ length variants) variants

      toArray :: [Value]
      toArray = map ((Array . V.fromList) . (map Object)) nSubsets
   in map fn toArray

-- | 'subsets' n xs computes subsets with that the constraint that the
-- length of the subset should be equal to n provided that the list is
-- non empty.
subsets :: Word8 -> [a] -> [[a]]
subsets 0 _ = [[]]
subsets _ [] = []
subsets n (x : xs) = map (x :) (subsets (n - 1) xs) <> subsets n xs
