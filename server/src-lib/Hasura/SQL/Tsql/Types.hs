-- | Types for Transact-SQL aka T-SQL; the language of SQL Server.

module Hasura.SQL.Tsql.Types where

import Prelude

data Select = Select
  deriving (Eq, Ord, Show)
