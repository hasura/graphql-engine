-- | This module provides constants that are either:
--
-- * Simply in common user
-- * Define names that that multiple pieces of code reference.
module Hasura.Backends.MSSQL.FromIr.Constants
  ( trueExpression,
    nullExpression,
    emptyArrayExpression,
    jsonFieldName,
    aggSubselectName,
    existsFieldName,
    aggFieldName,
    tempTableNameInserted,
    tempTableNameValues,
    tempTableNameDeleted,
    tempTableNameUpdated,
  )
where

import Database.ODBC.SQLServer qualified as ODBC
import Hasura.Backends.MSSQL.Instances.Types ()
import Hasura.Backends.MSSQL.Types.Internal as TSQL
import Hasura.Prelude

trueExpression :: Expression
trueExpression = ValueExpression $ ODBC.BoolValue True

nullExpression :: Expression
nullExpression = ValueExpression $ ODBC.TextValue "null"

emptyArrayExpression :: Expression
emptyArrayExpression = ValueExpression $ ODBC.TextValue "[]"

jsonFieldName :: Text
jsonFieldName = "json"

aggSubselectName :: Text
aggSubselectName = "agg_sub"

existsFieldName :: Text
existsFieldName = "exists_placeholder"

aggFieldName :: Text
aggFieldName = "agg"

tempTableNameInserted :: TempTableName
tempTableNameInserted = TempTableName "inserted"

tempTableNameValues :: TempTableName
tempTableNameValues = TempTableName "values"

tempTableNameDeleted :: TempTableName
tempTableNameDeleted = TempTableName "deleted"

tempTableNameUpdated :: TempTableName
tempTableNameUpdated = TempTableName "updated"
