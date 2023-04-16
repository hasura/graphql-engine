-- | The RQL IR representation of an invocation of a native query.
module Hasura.NativeQuery.IR
  ( NativeQuery (..),
  )
where

import Hasura.CustomReturnType.IR
import Hasura.NativeQuery.Metadata
import Hasura.Prelude
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Column (ColumnValue)

-- | The RQL IR representation of an invocation of a native query.
data NativeQuery b field = NativeQuery
  { -- | The graphql name of the native query.
    nqRootFieldName :: NativeQueryName,
    -- | The raw sql to use in the query
    nqInterpolatedQuery :: InterpolatedQuery field,
    -- | The arguments passed to the query, if any.
    nqArgs :: HashMap NativeQueryArgumentName (ColumnValue b),
    -- | The return type of the native query
    nqReturnType :: CustomReturnType b
  }
  deriving (Functor, Foldable, Traversable)

deriving instance (Backend b, Eq field) => Eq (NativeQuery b field)

deriving instance (Backend b, Show field) => Show (NativeQuery b field)
