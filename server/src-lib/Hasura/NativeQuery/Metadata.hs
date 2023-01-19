-- | This module houses the types and functions associated with the default
-- implementation of the metadata of native queries.
module Hasura.NativeQuery.Metadata
  ( NativeQueryArgumentName (..),
    NativeQueryInfoImpl (..),
  )
where

import Hasura.NativeQuery.Types
import Hasura.Prelude
import Hasura.RQL.Types.Backend

newtype NativeQueryArgumentName = NativeQueryArgumentName {getNativeQueryArgumentName :: Text}
  deriving (Eq, Ord, Hashable, Show)

-- | Provisional data type for modelling metadata
data NativeQueryInfoImpl b = NativeQueryInfoImpl
  { nqiiName :: NativeQueryName,
    nqiiCode :: Text,
    nqiiReturns :: TableName b,
    nqiiArgs :: HashMap NativeQueryArgumentName (ScalarType b),
    nqiiComment :: Text
  }
