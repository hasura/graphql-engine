-- | Types for Transact-SQL aka T-SQL; the language of SQL Server.

module Hasura.SQL.Tsql.Types where

import qualified Database.ODBC.SQLServer as Odbc
import Prelude

data Select = Select
  deriving (Eq, Show)

data Expression =
  ValueExpression Odbc.Value
  deriving (Eq, Show)
