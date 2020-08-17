module Hasura.GraphQL.Schema.Insert where

import           Hasura.Prelude

import qualified Hasura.RQL.DML.Insert.Types    as RQL
import qualified Hasura.RQL.DML.Returning.Types as RQL

import           Hasura.RQL.Types.BoolExp
import           Hasura.RQL.Types.Column
import           Hasura.RQL.Types.Common
import           Hasura.SQL.Types


-- At time of writing (August 2020), GraphQL queries and mutations get
-- translated into corresponding RQL queries: RQL is used as the internal
-- intermediary representation, before a query gets translated into
-- SQL. However, RQL inserts represenation does not support nested insertions,
-- which means that GraphQL inserts need a separate representation, found here.

-- FIXME: this code doesn't belong in this folder: arguably, since this is an
-- internal representation of a mutation, it should belong alongside RQL rather
-- than alongside the schema code, especially if we transition RQL to only be an
-- intermediary representation library rather than an actual API (see [1] for
-- more information).
-- [1] https://gist.github.com/abooij/07165b5ac36097178a334bc03805c33b

-- FIXME: this representation was lifted almost verbatim from pre-PDV code, and
-- hasn't been adapted to reflect the changes that PDV brought. It is therefore
-- quite likely that some of the information stored in those structures is
-- redundant, and that they can be simplified.

data AnnInsert v
  = AnnInsert
  { _aiFieldName :: !Text
  , _aiIsSingle  :: Bool
  , _aiData      :: AnnMultiInsert v
  }

data AnnIns a v
  = AnnIns
  { _aiInsObj         :: !a
  , _aiTableName      :: !QualifiedTable
  , _aiConflictClause :: !(Maybe (RQL.ConflictClauseP1 v))
  , _aiCheckCond      :: !(AnnBoolExp v, Maybe (AnnBoolExp v))
  , _aiTableCols      :: ![PGColumnInfo]
  , _aiDefVals        :: !(PreSetColsG v)
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
