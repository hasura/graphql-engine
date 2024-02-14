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
import Hasura.Backends.MSSQL.Types.Internal
import Hasura.Backends.MSSQL.Types.Update
import Hasura.Prelude
import Hasura.RQL.IR qualified as IR
import Hasura.RQL.IR.Update.Batch qualified as IR
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Column qualified as IR

fromUpdate :: IR.AnnotatedUpdate 'MSSQL -> FromIr Update
fromUpdate (IR.AnnotatedUpdateG table updatePermFilter _ (IR.UpdateBatch updateOperations whereClause) _ allColumns _tCase _validateInput) = do
  tableAlias <- generateAlias (TableTemplate (tableName table))
  runReaderT
    ( do
        permissionsFilter <- fromGBoolExp updatePermFilter
        whereExpression <- fromGBoolExp whereClause
        let columnNames = map IR.ciColumn allColumns
        pure
          Update
            { updateTable =
                Aliased
                  { aliasedAlias = tableAlias,
                    aliasedThing = table
                  },
              updateSet = updateOperations,
              updateOutput = Output Inserted (map OutputColumn columnNames),
              updateTempTable = TempTable tempTableNameUpdated columnNames,
              updateWhere = Where [permissionsFilter, whereExpression]
            }
    )
    (EntityAlias tableAlias)
