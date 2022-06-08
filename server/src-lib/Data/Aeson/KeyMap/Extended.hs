module Data.Aeson.KeyMap.Extended
  ( mapWithKey,
  )
where

import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap (KeyMap, traverseWithKey)
import Data.Functor.Identity
import Hasura.Prelude

mapWithKey :: (Key -> v1 -> v2) -> KeyMap v1 -> KeyMap v2
mapWithKey f km = runIdentity $ traverseWithKey (\k v -> Identity (f k v)) km
