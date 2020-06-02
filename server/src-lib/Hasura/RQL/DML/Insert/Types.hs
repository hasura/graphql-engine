module Hasura.RQL.DML.Insert.Types where


import           Hasura.Prelude

import           Hasura.RQL.DML.Returning.Types
import           Hasura.RQL.Types.BoolExp
import           Hasura.RQL.Types.Column
import           Hasura.SQL.Types

import qualified Hasura.SQL.DML                 as S


data ConflictTarget
  = CTColumn ![PGCol]
  | CTConstraint !ConstraintName
  deriving (Show, Eq)

data ConflictClauseP1 v
  = CP1DoNothing !(Maybe ConflictTarget)
  | CP1Update !ConflictTarget ![PGCol] !(PreSetColsG v) (AnnBoolExp v)
  deriving (Show, Eq)


data InsertQueryP1
  = InsertQueryP1
  { iqp1Table     :: !QualifiedTable
  , iqp1Cols      :: ![PGCol]
  , iqp1Tuples    :: ![[S.SQLExp]]
  , iqp1Conflict  :: !(Maybe (ConflictClauseP1 S.SQLExp))
  , iqp1CheckCond :: !(AnnBoolExpSQL, Maybe AnnBoolExpSQL)
  , iqp1Output    :: !MutationOutput
  , iqp1AllCols   :: ![PGColumnInfo]
  } deriving (Show, Eq)
