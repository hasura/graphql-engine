module Hasura.RQL.IR.Insert where


import           Hasura.Prelude

import           Hasura.RQL.IR.BoolExp
import           Hasura.RQL.IR.Returning
import           Hasura.RQL.Types.Column
import           Hasura.RQL.Types.Common
import           Hasura.SQL.Backend


data ConflictTarget (b :: BackendType)
  = CTColumn ![Column b]
  | CTConstraint !(ConstraintName b)
deriving instance Backend b => Show (ConflictTarget b)
deriving instance Backend b => Eq   (ConflictTarget b)

data ConflictClauseP1 (b :: BackendType) v
  = CP1DoNothing !(Maybe (ConflictTarget b))
  | CP1Update !(ConflictTarget b) ![Column b] !(PreSetColsG b v) (AnnBoolExp b v)
  deriving (Functor, Foldable, Traversable)



data InsertQueryP1 (b :: BackendType)
  = InsertQueryP1
  { iqp1Table     :: !(TableName b)
  , iqp1Cols      :: ![Column b]
  , iqp1Tuples    :: ![[SQLExp b]]
  , iqp1Conflict  :: !(Maybe (ConflictClauseP1 b (SQLExp b)))
  , iqp1CheckCond :: !(AnnBoolExpSQL b, Maybe (AnnBoolExpSQL b))
  , iqp1Output    :: !(MutationOutput b)
  , iqp1AllCols   :: ![ColumnInfo b]
  }
