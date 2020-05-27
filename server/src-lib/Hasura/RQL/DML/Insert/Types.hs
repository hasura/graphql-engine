module Hasura.RQL.DML.Insert.Types where


import           Hasura.Prelude

-- import           Instances.TH.Lift             ()

-- import           Hasura.RQL.DML.Internal.Types
-- import           Hasura.RQL.DML.Mutation.Types
-- import           Hasura.RQL.Instances          ()
import           Hasura.RQL.Types.BoolExp
import           Hasura.SQL.Types


data ConflictTarget
  = CTColumn ![PGCol]
  | CTConstraint !ConstraintName
  deriving (Show, Eq)

data ConflictClauseP1 v
  = CP1DoNothing !(Maybe ConflictTarget)
  | CP1Update !ConflictTarget ![PGCol] !PreSetColsPartial (Maybe (AnnBoolExp v))
  deriving (Show, Eq)
