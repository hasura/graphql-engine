{-# LANGUAGE UndecidableInstances #-}

-- | MSSQL Types Insert
--
-- Types for MSSQL Insert IR.
module Hasura.Backends.MSSQL.Types.Insert
  ( BackendInsert (..),
    IfMatched (..),
  )
where

import Hasura.Backends.MSSQL.Types.Instances ()
import Hasura.Backends.MSSQL.Types.Internal
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp (AnnBoolExp)
import Hasura.RQL.Types.Backend (Backend)
import Hasura.RQL.Types.BackendType (BackendType (MSSQL))

-- | Defines the part in insert mutation that is unique for MSSQL the @if_matched@ clause.
data BackendInsert v = BackendInsert
  { -- | @if_matched@ can be omitted (and in that case will be @Nothing@).
    --   If omitted, we only insert new rows (without upserting).
    _biIfMatched :: Maybe (IfMatched v)
  }

deriving instance (Backend 'MSSQL, Show (IfMatched v), Show v) => Show (BackendInsert v)

deriving instance (Backend 'MSSQL) => Functor BackendInsert

deriving instance (Backend 'MSSQL) => Foldable BackendInsert

deriving instance (Backend 'MSSQL) => Traversable BackendInsert

-- | The IR data representing an @if_matched@ clause, which handles upserts.
data IfMatched v = IfMatched
  { -- | Columns to compare when checking if there's a match
    _imMatchColumns :: [Column 'MSSQL],
    -- | Columns to update when there's a match
    _imUpdateColumns :: [Column 'MSSQL],
    -- | A condition for updating columns in case of a match
    _imConditions :: AnnBoolExp 'MSSQL v,
    -- | Default values (presets) for some columns
    _imColumnPresets :: HashMap ColumnName v
  }

deriving instance (Backend 'MSSQL, Show (AnnBoolExp 'MSSQL v), Show v) => Show (IfMatched v)

deriving instance (Backend 'MSSQL) => Functor IfMatched

deriving instance (Backend 'MSSQL) => Foldable IfMatched

deriving instance (Backend 'MSSQL) => Traversable IfMatched
