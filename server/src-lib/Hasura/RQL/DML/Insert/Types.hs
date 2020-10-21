module Hasura.RQL.DML.Insert.Types where


import           Hasura.Prelude

import           Hasura.RQL.DML.Returning.Types
import           Hasura.RQL.Types.BoolExp
import           Hasura.RQL.Types.Column
import           Hasura.RQL.Types.Common
import           Hasura.SQL.Backend
import           Hasura.SQL.Types

import qualified Hasura.SQL.DML                 as S


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
