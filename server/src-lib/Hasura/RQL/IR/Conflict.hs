module Hasura.RQL.IR.Conflict
  ( OnConflictClause (..),
    OnConflictClauseData (..),
    ConflictTarget (..),
  )
where

import Hasura.Prelude
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType

-- TODO: When adding support for other backends, consider whether these types
-- really are generic. If not, please start by moving them to a Postgres-specific
-- module.

data ConflictTarget (b :: BackendType)
  = CTColumn [Column b]
  | CTConstraint (ConstraintName b)

deriving instance (Backend b) => Show (ConflictTarget b)

deriving instance (Backend b) => Eq (ConflictTarget b)

data OnConflictClauseData b v = OnConflictClauseData
  { cp1udConflictTarget :: ConflictTarget b,
    cp1udAffectedColumns :: [Column b],
    cp1udValues :: PreSetColsG b v,
    cp1udFilter :: AnnBoolExp b v
  }
  deriving (Functor, Foldable, Traversable)

data OnConflictClause (b :: BackendType) v
  = OCCDoNothing (Maybe (ConflictTarget b))
  | OCCUpdate (OnConflictClauseData b v)
  deriving (Functor, Foldable, Traversable)
