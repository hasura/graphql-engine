-- | The RQL IR representation of an invocation of a logical model.
module Hasura.LogicalModel.IR
  ( LogicalModel (..),
  )
where

import Hasura.LogicalModel.Metadata
import Hasura.Prelude
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Column (ColumnValue)

-- | The RQL IR representation of an invocation of a logical model.
data LogicalModel b field = LogicalModel
  { -- | The graphql name of the logical model.
    lmRootFieldName :: LogicalModelName,
    -- | The raw sql to use in the query
    lmInterpolatedQuery :: InterpolatedQuery field,
    -- | The arguments passed to the query, if any.
    lmArgs :: HashMap LogicalModelArgumentName (ColumnValue b)
  }
  deriving (Functor, Foldable, Traversable)

deriving instance (Backend b, Eq field) => Eq (LogicalModel b field)

deriving instance (Backend b, Show field) => Show (LogicalModel b field)
