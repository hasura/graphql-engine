-- | This module contains the default types and functions that model Native
-- Queries.
module Hasura.NativeQuery.IR
  ( NativeQuery (..),
  )
where

import Hasura.NativeQuery.Metadata
import Hasura.Prelude
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Column (ColumnValue)

-- | The default implementation of an invocation of a native query.
data NativeQuery b field = NativeQuery
  { -- | The defined name of the native query.
    nqRootFieldName :: NativeQueryName,
    -- | The raw sql to use in the query
    nqInterpolatedQuery :: InterpolatedQuery field,
    -- | The arguments passed to the native query, if any.
    nqArgs :: HashMap NativeQueryArgumentName (ColumnValue b)
  }
  deriving (Functor, Foldable, Traversable)

deriving instance (Backend b, Eq field) => Eq (NativeQuery b field)

deriving instance (Backend b, Show field) => Show (NativeQuery b field)
