{-# LANGUAGE UndecidableInstances #-}

-- | Types for MSSQL Insert IR.
module Hasura.Backends.MSSQL.Types.Insert
  ( BackendInsert (..),
    ExtraColumnInfo (..),
    IfMatched (..),
  )
where

import Hasura.Backends.MSSQL.Types.Internal
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp (AnnBoolExp)
import Hasura.RQL.Types.Backend (Backend)
import Hasura.SQL.Backend (BackendType (MSSQL))

data BackendInsert v = BackendInsert
  { _biIfMatched :: Maybe (IfMatched v),
    _biExtraColumnInfo :: ExtraColumnInfo
  }

deriving instance Backend 'MSSQL => Functor BackendInsert

deriving instance Backend 'MSSQL => Foldable BackendInsert

deriving instance Backend 'MSSQL => Traversable BackendInsert

data ExtraColumnInfo = ExtraColumnInfo
  { _eciPrimaryKeyColumns :: ![ColumnName],
    _eciIdentityColumns :: ![ColumnName]
  }

-- | The IR data representing an @if_matched@ clause, which handles upserts.
data IfMatched v = IfMatched
  { _imMatchColumns :: [Column 'MSSQL],
    _imUpdateColumns :: [Column 'MSSQL],
    _imConditions :: AnnBoolExp 'MSSQL v,
    _imColumnPresets :: HashMap ColumnName v
  }

deriving instance Backend 'MSSQL => Functor IfMatched

deriving instance Backend 'MSSQL => Foldable IfMatched

deriving instance Backend 'MSSQL => Traversable IfMatched
