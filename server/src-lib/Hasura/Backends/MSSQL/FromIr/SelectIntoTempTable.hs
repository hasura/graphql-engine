-- | This module contains supporting definitions for building temporary tables
-- based off of the schema of other tables. This is used in mutations to capture
-- the data of rows that are affected.
module Hasura.Backends.MSSQL.FromIr.SelectIntoTempTable
  ( toSelectIntoTempTable,
  )
where

import Hasura.Backends.MSSQL.Instances.Types ()
import Hasura.Backends.MSSQL.Types.Internal as TSQL
import Hasura.Prelude
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Column qualified as IR

-- | Create a temporary table with the same schema as the given table.
toSelectIntoTempTable :: TempTableName -> TableName -> [IR.ColumnInfo 'MSSQL] -> SITTConstraints -> SelectIntoTempTable
toSelectIntoTempTable tempTableName fromTable allColumns withConstraints = do
  SelectIntoTempTable
    { sittTempTableName = tempTableName,
      sittColumns = map columnInfoToUnifiedColumn allColumns,
      sittFromTableName = fromTable,
      sittConstraints = withConstraints
    }

-- | Extracts the type and column name of a ColumnInfo
columnInfoToUnifiedColumn :: IR.ColumnInfo 'MSSQL -> UnifiedColumn
columnInfoToUnifiedColumn colInfo =
  case IR.ciType colInfo of
    IR.ColumnScalar t ->
      UnifiedColumn
        { name = IR.ciColumn colInfo,
          type' = t
        }
    -- Enum values are represented as text value so they will always be of type text
    IR.ColumnEnumReference {} ->
      UnifiedColumn
        { name = IR.ciColumn colInfo,
          type' = TextType
        }
