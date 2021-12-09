-- | This module defines the Insert-related IR types specific to Postgres.
module Hasura.Backends.Postgres.Types.Insert
  ( BackendInsert (..),
  )
where

import Hasura.Prelude
import Hasura.RQL.IR.Conflict (ConflictClauseP1)

-- | The PostgreSQL-specific data of an Insert expression.
--
-- This is parameterised over @v@ which enables different phases of IR
-- transformation to maintain the overall structure while enriching/transforming
-- the data at the leaves.
data BackendInsert b v = BackendInsert
  { _biConflictClause :: Maybe (ConflictClauseP1 b v)
  }
  deriving (Functor, Foldable, Traversable)
