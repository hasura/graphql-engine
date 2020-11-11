module Hasura.RQL.IR.Insert where


import           Hasura.Prelude

<<<<<<< HEAD
import qualified Hasura.Backends.Postgres.SQL.DML   as S

import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.RQL.IR.Returning
import           Hasura.RQL.Types.BoolExp
=======
import           Hasura.RQL.IR.BoolExp
import           Hasura.RQL.IR.Returning
>>>>>>> master
import           Hasura.RQL.Types.Column
import           Hasura.RQL.Types.Common
import           Hasura.SQL.Backend


<<<<<<< HEAD
data ConflictTarget
  = CTColumn ![PGCol]
  | CTConstraint !ConstraintName
  deriving (Show, Eq)

data ConflictClauseP1 (b :: Backend) v
  = CP1DoNothing !(Maybe ConflictTarget)
  | CP1Update !ConflictTarget ![Column b] !(PreSetColsG b v) (AnnBoolExp b v)
=======
data ConflictTarget (b :: BackendType)
  = CTColumn ![Column b]
  | CTConstraint !(ConstraintName b)
deriving instance Backend b => Show (ConflictTarget b)
deriving instance Backend b => Eq   (ConflictTarget b)

data ConflictClauseP1 (b :: BackendType) v
  = CP1DoNothing !(Maybe (ConflictTarget b))
  | CP1Update !(ConflictTarget b) ![Column b] !(PreSetColsG b v) (AnnBoolExp b v)
>>>>>>> master
  deriving (Functor, Foldable, Traversable)



<<<<<<< HEAD
data InsertQueryP1 (b :: Backend)
  = InsertQueryP1
  { iqp1Table     :: !QualifiedTable
  , iqp1Cols      :: ![Column b]
  , iqp1Tuples    :: ![[S.SQLExp]]
  , iqp1Conflict  :: !(Maybe (ConflictClauseP1 b S.SQLExp))
=======
data InsertQueryP1 (b :: BackendType)
  = InsertQueryP1
  { iqp1Table     :: !(TableName b)
  , iqp1Cols      :: ![Column b]
  , iqp1Tuples    :: ![[SQLExp b]]
  , iqp1Conflict  :: !(Maybe (ConflictClauseP1 b (SQLExp b)))
>>>>>>> master
  , iqp1CheckCond :: !(AnnBoolExpSQL b, Maybe (AnnBoolExpSQL b))
  , iqp1Output    :: !(MutationOutput b)
  , iqp1AllCols   :: ![ColumnInfo b]
  }
