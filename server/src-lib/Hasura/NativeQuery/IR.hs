-- | This module contains the default types and functions that model Native
-- Queries.
module Hasura.NativeQuery.IR
  ( NativeQueryImpl (..),
  )
where

import Hasura.NativeQuery.Metadata
import Hasura.Prelude
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Column (ColumnValue)

-- | The default implementation of an invocation of a native query.
data NativeQueryImpl b field = NativeQueryImpl
  { -- | The defined name of the native query.
    nqRootFieldName :: NativeQueryNameImpl,
    -- | The raw sql to use in the query
    nqInterpolatedQuery :: InterpolatedQuery field,
    -- | The arguments passed to the native query, if any.
    nqArgs :: HashMap NativeQueryArgumentName (ColumnValue b)
  }
  deriving (Functor, Foldable, Traversable)

deriving instance (Backend b, Eq field) => Eq (NativeQueryImpl b field)

deriving instance (Backend b, Show field) => Show (NativeQueryImpl b field)
