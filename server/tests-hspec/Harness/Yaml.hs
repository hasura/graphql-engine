-- | Utility functions related to yaml
module Harness.Yaml
  ( combinationsObject,
    fromObject,
    combinationsObjectUsingValue,
  )
where

import Data.Aeson
  ( Object,
    Value (..),
  )
import Data.List (permutations)
import Data.Vector qualified as V
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
  let toArray :: [Value]
      toArray = map ((Array . V.fromList) . (map Object)) (permutations variants)
   in map fn toArray

-- | Same as 'combinationsObject' but the second parameter is a list
-- of 'Value`. We assume that 'Value' internally has only 'Object', if
-- not it will throw exception.
combinationsObjectUsingValue :: (Value -> Value) -> [Value] -> [Value]
combinationsObjectUsingValue fn variants = combinationsObject fn (map fromObject variants)
