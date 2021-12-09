-- | Types for MSSQL Insert IR.
module Hasura.Backends.MSSQL.Types.Insert
  ( MSSQLExtraInsertData (..),
  )
where

import Hasura.Backends.MSSQL.Types.Internal
import Hasura.Prelude

data MSSQLExtraInsertData v = MSSQLExtraInsertData
  { _mssqlPrimaryKeyColumns :: ![ColumnName],
    _mssqlIdentityColumns :: ![ColumnName]
  }
  deriving (Functor, Foldable, Traversable)
