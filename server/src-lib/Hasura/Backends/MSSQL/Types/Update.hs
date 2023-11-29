-- | MSSQL Types Update
--
-- This module defines the Update-related IR types specific to MSSQL.
module Hasura.Backends.MSSQL.Types.Update
  ( UpdateOperator (..),
    Update (..),
    UpdateSet,
    UpdateOutput,
  )
where

import Hasura.Backends.MSSQL.Types.Instances ()
import Hasura.Backends.MSSQL.Types.Internal
import Hasura.Prelude

-- | The various @update operators@ supported by MSSQL,
-- i.e. the @_set@, @_inc@ operators that appear in the schema.
--
-- TODO: Docs for MSSQL!
-- See <https://hasura.io/docs/latest/graphql/core/databases/postgres/mutations/update.html#postgres-update-mutation Update Mutations User docs>
data UpdateOperator v
  = UpdateSet v
  | UpdateInc v
  deriving (Functor, Foldable, Traversable, Generic, Data)

type UpdateSet = HashMap ColumnName (UpdateOperator Expression)

type UpdateOutput = Output Inserted

-- | UPDATE [table_alias] SET [table_alias].column = 'value' OUTPUT INSERTED.column INTO #updated
-- FROM [table_name] AS [table_alias] WHERE <filter-expression>
data Update = Update
  { updateTable :: Aliased TableName,
    updateSet :: UpdateSet,
    updateOutput :: UpdateOutput,
    updateTempTable :: TempTable,
    updateWhere :: Where
  }
