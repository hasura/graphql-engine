{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module defines the Insert-related IR types specific to Postgres.
module Hasura.Backends.Postgres.Types.Insert
  ( BackendInsert (..),
  )
where

import Data.Kind (Type)
import Hasura.Prelude
import Hasura.RQL.IR.Conflict (ConflictClauseP1)
import Hasura.RQL.Types.Backend (Backend)
import Hasura.SQL.Backend

-- | The PostgreSQL-specific data of an Insert expression.
--
-- This is parameterised over @v@ which enables different phases of IR
-- transformation to maintain the overall structure while enriching/transforming
-- the data at the leaves.
type BackendInsert :: PostgresKind -> Type -> Type
newtype BackendInsert pgKind v = BackendInsert
  { _biConflictClause :: Maybe (ConflictClauseP1 ('Postgres pgKind) v)
  }

deriving instance (Backend ('Postgres pgKind)) => Functor (BackendInsert pgKind)

deriving instance (Backend ('Postgres pgKind)) => Foldable (BackendInsert pgKind)

deriving instance (Backend ('Postgres pgKind)) => Traversable (BackendInsert pgKind)
