-- | This module defines the translation function for delete mutations.
module Hasura.Backends.MSSQL.FromIr.Delete (fromDelete) where

import Hasura.Backends.MSSQL.FromIr (FromIr, NameTemplate (..), generateAlias)
import Hasura.Backends.MSSQL.FromIr.Constants (tempTableNameDeleted)
import Hasura.Backends.MSSQL.FromIr.Expression (fromGBoolExp)
import Hasura.Backends.MSSQL.Instances.Types ()
import Hasura.Backends.MSSQL.Types.Internal as TSQL
import Hasura.Prelude
import Hasura.RQL.IR qualified as IR
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Column qualified as IR

fromDelete :: IR.AnnDel 'MSSQL -> FromIr Delete
fromDelete (IR.AnnDel table (permFilter, whereClause) _ allColumns _ _validateInput _isDeleteByPrimaryKey) = do
  tableAlias <- generateAlias (TableTemplate (tableName table))
  runReaderT
    ( do
        permissionsFilter <- fromGBoolExp permFilter
        whereExpression <- fromGBoolExp whereClause
        let columnNames = map IR.ciColumn allColumns
        pure
          Delete
            { deleteTable =
                Aliased
                  { aliasedAlias = tableAlias,
                    aliasedThing = table
                  },
              deleteOutput = Output Deleted (map OutputColumn columnNames),
              deleteTempTable = TempTable tempTableNameDeleted columnNames,
              deleteWhere = Where [permissionsFilter, whereExpression]
            }
    )
    (EntityAlias tableAlias)
