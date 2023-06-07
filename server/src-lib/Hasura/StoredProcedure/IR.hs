-- | The RQL IR representation of an invocation of a stored procedure.
module Hasura.StoredProcedure.IR
  ( StoredProcedure (..),
  )
where

import Hasura.LogicalModel.IR
import Hasura.Prelude
import Hasura.RQL.Types.Backend
import Hasura.StoredProcedure.Metadata
import Language.GraphQL.Draft.Syntax qualified as G

-- | The RQL IR representation of an invocation of a stored procedure.
data StoredProcedure b field = StoredProcedure
  { -- | The SQL name of the stored procedure.
    spStoredProcedure :: FunctionName b,
    -- | The graphql name of the stored procedure.
    spGraphqlName :: G.Name,
    -- | The arguments passed to the query, if any.
    spArgs :: HashMap ArgumentName (ScalarType b, field),
    -- | The return type of the stored procedure
    spLogicalModel :: LogicalModel b
  }
  deriving (Functor, Foldable, Traversable)

deriving instance (Backend b, Eq field) => Eq (StoredProcedure b field)

deriving instance (Backend b, Show field) => Show (StoredProcedure b field)
