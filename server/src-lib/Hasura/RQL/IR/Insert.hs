module Hasura.RQL.IR.Insert where

import           Hasura.Prelude

import           Hasura.RQL.IR.BoolExp
import           Hasura.RQL.IR.Returning
import           Hasura.RQL.Types.Column
import           Hasura.RQL.Types.Common
import           Hasura.SQL.Backend


data AnnInsert (b :: BackendType) v
  = AnnInsert
  { _aiFieldName :: !Text
  , _aiIsSingle  :: Bool
  , _aiData      :: AnnMultiInsert b v
  }

data AnnIns (b :: BackendType) a v
  = AnnIns
  { _aiInsObj         :: !a
  , _aiTableName      :: !(TableName b)
  , _aiConflictClause :: !(Maybe (ConflictClauseP1 b v))
  , _aiCheckCond      :: !(AnnBoolExp b v, Maybe (AnnBoolExp b v))
  , _aiTableCols      :: ![ColumnInfo b]
  , _aiDefVals        :: !(PreSetColsG b v)
  }

type SingleObjIns b v = AnnIns b (AnnInsObj b v) v
type MultiObjIns  b v = AnnIns b [AnnInsObj b v] v

data RelIns (b :: BackendType) a
  = RelIns
  { _riAnnIns  :: !a
  , _riRelInfo :: !(RelInfo b)
  } deriving (Show, Eq)

type ObjRelIns b v = RelIns b (SingleObjIns b v)
type ArrRelIns b v = RelIns b (MultiObjIns  b v)

data AnnInsObj (b :: BackendType) v
  = AnnInsObj
  { _aioColumns :: ![(Column b, v)]
  , _aioObjRels :: ![ObjRelIns b v]
  , _aioArrRels :: ![ArrRelIns b v]
  }

type AnnSingleInsert b v = (SingleObjIns b v, MutationOutputG b v)
type AnnMultiInsert  b v = (MultiObjIns  b v, MutationOutputG b v)

instance Semigroup (AnnInsObj backend v) where
  (AnnInsObj col1 obj1 rel1) <> (AnnInsObj col2 obj2 rel2) =
    AnnInsObj (col1 <> col2) (obj1 <> obj2) (rel1 <> rel2)

instance Monoid (AnnInsObj backend v) where
  mempty = AnnInsObj [] [] []



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
  , iqp1Tuples    :: ![[SQLExpression b]]
  , iqp1Conflict  :: !(Maybe (ConflictClauseP1 b (SQLExpression b)))
  , iqp1CheckCond :: !(AnnBoolExpSQL b, Maybe (AnnBoolExpSQL b))
  , iqp1Output    :: !(MutationOutput b)
  , iqp1AllCols   :: ![ColumnInfo b]
  }
