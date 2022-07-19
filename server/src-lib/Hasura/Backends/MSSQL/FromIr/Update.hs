-- | This module defines the translation functions for update mutations.
module Hasura.Backends.MSSQL.FromIr.Update
  ( fromUpdate,
  )
where

import Hasura.Backends.MSSQL.FromIr
  ( FromIr,
    NameTemplate (TableTemplate),
    generateAlias,
  )
import Hasura.Backends.MSSQL.FromIr.Constants (tempTableNameUpdated)
import Hasura.Backends.MSSQL.FromIr.Expression (fromGBoolExp)
import Hasura.Backends.MSSQL.Instances.Types ()
import Hasura.Backends.MSSQL.Types.Internal as TSQL
import Hasura.Backends.MSSQL.Types.Update as TSQL (BackendUpdate (..), Update (..))
import Hasura.Prelude
import Hasura.RQL.IR qualified as IR
import Hasura.RQL.Types.Column qualified as IR
import Hasura.SQL.Backend

fromUpdate :: IR.AnnotatedUpdate 'MSSQL -> FromIr Update
fromUpdate (IR.AnnotatedUpdateG table (permFilter, whereClause) _ backendUpdate _ allColumns _tCase) = do
  tableAlias <- generateAlias (TableTemplate (tableName table))
  runReaderT
    ( do
        permissionsFilter <- fromGBoolExp permFilter
        whereExpression <- fromGBoolExp whereClause
        let columnNames = map IR.ciColumn allColumns
        pure
          Update
            { updateTable =
                Aliased
                  { aliasedAlias = tableAlias,
                    aliasedThing = table
                  },
              updateSet = updateOperations backendUpdate,
              updateOutput = Output Inserted (map OutputColumn columnNames),
              updateTempTable = TempTable tempTableNameUpdated columnNames,
              updateWhere = Where [permissionsFilter, whereExpression]
            }
    )
    (EntityAlias tableAlias)
