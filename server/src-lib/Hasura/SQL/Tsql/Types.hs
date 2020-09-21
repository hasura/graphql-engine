-- | Types for Transact-SQL aka T-SQL; the language of SQL Server.

module Hasura.SQL.Tsql.Types where

import Prelude

data Select = Select
  deriving (Eq, Show)

data Expression = Expression
  deriving (Eq, Show)

-- The set of RQL/DML/SQL types is confusing and seems to be mixed
-- together a little.
--
-- An easy way forward is to define types in here which, when
-- converted to odbc:Query and odbc:Value, will properly pass property
-- tests.

-- TODO: To start off: Make a Bool type, derive validity gen
-- unchecked, produce translator for Bool. Test randomly generated
-- bool expressions against TSQL server.
