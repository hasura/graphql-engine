-- | The RQL IR representation of an invocation of a stored procedure.
module Hasura.StoredProcedure.IR
  ( StoredProcedure (..),
  )
where

import Hasura.LogicalModel.IR
import Hasura.Prelude
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Column (ColumnValue)
import Hasura.StoredProcedure.Metadata

-- | The RQL IR representation of an invocation of a stored procedure.
data StoredProcedure b field = StoredProcedure
  { -- | The graphql name of the stored procedure.
    spRootFieldName :: StoredProcedureName,
    -- | The raw sql to use in the query
    spInterpolatedQuery :: InterpolatedQuery field,
    -- | The arguments passed to the query, if any.
    spArgs :: HashMap NativeQueryArgumentName (ColumnValue b),
    -- | The return type of the stored procedure
    spLogicalModel :: LogicalModel b
  }
  deriving (Functor, Foldable, Traversable)

deriving instance (Backend b, Eq field) => Eq (StoredProcedure b field)

deriving instance (Backend b, Show field) => Show (StoredProcedure b field)
