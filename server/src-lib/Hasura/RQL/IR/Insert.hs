module Hasura.RQL.IR.Insert where


import           Hasura.Prelude

import qualified Hasura.Backends.Postgres.SQL.DML   as S

import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.RQL.IR.BoolExp
import           Hasura.RQL.IR.Returning
import           Hasura.RQL.Types.Column
import           Hasura.RQL.Types.Common
import           Hasura.SQL.Backend


data ConflictTarget
  = CTColumn ![PGCol]
  | CTConstraint !ConstraintName
  deriving (Show, Eq)

data ConflictClauseP1 (b :: Backend) v
  = CP1DoNothing !(Maybe ConflictTarget)
  | CP1Update !ConflictTarget ![Column b] !(PreSetColsG b v) (AnnBoolExp b v)
  deriving (Functor, Foldable, Traversable)



data InsertQueryP1 (b :: Backend)
  = InsertQueryP1
  { iqp1Table     :: !QualifiedTable
  , iqp1Cols      :: ![Column b]
  , iqp1Tuples    :: ![[S.SQLExp]]
  , iqp1Conflict  :: !(Maybe (ConflictClauseP1 b S.SQLExp))
  , iqp1CheckCond :: !(AnnBoolExpSQL b, Maybe (AnnBoolExpSQL b))
  , iqp1Output    :: !(MutationOutput b)
  , iqp1AllCols   :: ![ColumnInfo b]
  }
