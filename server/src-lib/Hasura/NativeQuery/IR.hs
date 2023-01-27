-- | This module contains the default types and functions that model Native
-- Queries.
module Hasura.NativeQuery.IR
  ( NativeQueryImpl (..),
  )
where

import Hasura.NativeQuery.Metadata
import Hasura.NativeQuery.Types
import Hasura.Prelude
import Hasura.RQL.Types.Column (ColumnValue)

-- | The default implementation of an invocation of a native query.
data NativeQueryImpl b field = NativeQueryImpl
  { -- | The defined name of the native query.
    -- When translating this is used as a key to look up the actual
    -- native query definition.
    nqName :: NativeQueryName,
    -- | The arguments passed to the native query, if any.
    nqArgs :: HashMap NativeQueryArgumentName (ColumnValue b)
  }
  deriving (Eq, Functor, Foldable, Traversable, Show)
