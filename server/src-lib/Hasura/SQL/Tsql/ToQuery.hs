-- | Convert the simple T-SQL AST to an SQL query, ready to be passed
-- to the odbc package's query/exec functions.

module Hasura.SQL.Tsql.ToQuery where

import Database.ODBC.SQLServer
import Hasura.SQL.Tsql.Types

expressionToQuery :: Expression -> Query
expressionToQuery =
  \case
    ValueExpression value -> toSql value
