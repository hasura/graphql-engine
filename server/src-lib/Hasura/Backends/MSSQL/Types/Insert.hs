-- | Types for MSSQL Insert IR.
module Hasura.Backends.MSSQL.Types.Insert
  ( MSSQLExtraInsertData (..),
  )
where

import Hasura.Backends.MSSQL.Types.Internal

data MSSQLExtraInsertData = MSSQLExtraInsertData
  { _mssqlPrimaryKeyColumns :: ![ColumnName],
    _mssqlIdentityColumns :: ![ColumnName]
  }
