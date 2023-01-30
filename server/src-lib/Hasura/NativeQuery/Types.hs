-- | This module houses the types that are necessary to even talk about native
-- queries abstractly of a concrete implementation.
module Hasura.NativeQuery.Types
  ( NativeQueryName (..),
  )
where

import Hasura.Prelude

newtype NativeQueryName = NativeQueryName {getNativeQueryName :: Text}
  deriving (Eq, Ord, Show, Hashable)
