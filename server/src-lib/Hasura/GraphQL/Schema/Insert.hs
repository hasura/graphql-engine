module Hasura.GraphQL.Schema.Insert where

import           Hasura.Prelude

import qualified Hasura.RQL.DML.Insert.Types    as RQL
import           Hasura.RQL.DML.Returning.Types as RQL

import           Hasura.RQL.Types.BoolExp
import           Hasura.RQL.Types.Column
import           Hasura.RQL.Types.Common
import           Hasura.SQL.Types


-- WIP NOTE
--
-- An abstract representation of insert was tricky to pin down. The
-- following structures are taken from Resolve, and slightly
-- modified. What needs to be decided is where those should go and
-- what we should do with them, really.
--
-- They are currently in this file in order to be accessible from the
-- Context file without creating a dependency loop, but this is only a
-- temporary emplacement: they are not part of the Schema and don't
-- belong in this folder.


data AnnIns a v
  = AnnIns
  { _aiInsObj         :: !a
  , _aiConflictClause :: !(Maybe (RQL.ConflictClauseP1 v))
  , _aiCheckCond      :: !(AnnBoolExp v, AnnBoolExpPartialSQL)
  , _aiTableCols      :: ![PGColumnInfo]
  , _aiDefVals        :: !(PreSetColsPartial)
  } deriving (Show, Eq)

type SingleObjIns v = AnnIns (AnnInsObj v) v
type MultiObjIns  v = AnnIns [AnnInsObj v] v

data RelIns a
  = RelIns
  { _riAnnIns  :: !a
  , _riRelInfo :: !RelInfo
  } deriving (Show, Eq)

type ObjRelIns v = RelIns (SingleObjIns v)
type ArrRelIns v = RelIns (MultiObjIns  v)

data AnnInsObj v
  = AnnInsObj
  { _aioColumns :: ![(PGCol, v)]
  , _aioObjRels :: ![ObjRelIns v]
  , _aioArrRels :: ![ArrRelIns v]
  } deriving (Show, Eq)

type AnnSingleInsert v = (SingleObjIns v, RQL.MutationOutputG v)
type AnnMultiInsert  v = (MultiObjIns  v, RQL.MutationOutputG v)

instance Semigroup (AnnInsObj v) where
  (AnnInsObj col1 obj1 rel1) <> (AnnInsObj col2 obj2 rel2) =
    AnnInsObj (col1 <> col2) (obj1 <> obj2) (rel1 <> rel2)

instance Monoid (AnnInsObj v) where
  mempty = AnnInsObj [] [] []
