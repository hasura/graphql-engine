-- | This module defines the Update-related IR types specific to MSSQL.
module Hasura.Backends.MSSQL.Types.Update
  ( BackendUpdate (..),
    UpdateOperator (..),
  )
where

import Hasura.Backends.MSSQL.Types.Instances ()
import Hasura.Backends.MSSQL.Types.Internal qualified as MSSQL
import Hasura.Prelude

-- | The MSSQL-specific data of an Update expression.
--
-- This is parameterised over @v@ which enables different phases of IR
-- transformation to maintain the overall structure while enriching/transforming
-- the data at the leaves.
data BackendUpdate v = BackendUpdate
  { -- | The update operations to perform on each column.
    updateOperations :: HashMap MSSQL.ColumnName (UpdateOperator v)
  }
  deriving (Functor, Foldable, Traversable, Generic, Data)

-- | The various @update operators@ supported by MSSQL,
-- i.e. the @_set@, @_inc@ operators that appear in the schema.
--
-- TODO: Docs for MSSQL!
-- See <https://hasura.io/docs/latest/graphql/core/databases/postgres/mutations/update.html#postgres-update-mutation Update Mutations User docs>
data UpdateOperator v
  = UpdateSet v
  | UpdateInc v
  deriving (Functor, Foldable, Traversable, Generic, Data)
