module Hasura.RQL.IR.Conflict
  ( ConflictClauseP1 (..),
    ConflictTarget (..),
  )
where

import Hasura.Prelude
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.Types.Backend
import Hasura.SQL.Backend

data ConflictTarget (b :: BackendType)
  = CTColumn [Column b]
  | CTConstraint (ConstraintName b)

deriving instance Backend b => Show (ConflictTarget b)

deriving instance Backend b => Eq (ConflictTarget b)

data ConflictClauseP1 (b :: BackendType) v
  = CP1DoNothing (Maybe (ConflictTarget b))
  | CP1Update (ConflictTarget b) [Column b] (PreSetColsG b v) (AnnBoolExp b v)
  deriving (Functor, Foldable, Traversable)
