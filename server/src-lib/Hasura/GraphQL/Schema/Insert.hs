module Hasura.GraphQL.Schema.Insert where

import           Hasura.Prelude

import qualified Hasura.RQL.IR.Insert               as RQL
import qualified Hasura.RQL.IR.Returning            as RQL

import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.RQL.IR.BoolExp
import           Hasura.RQL.Types.Column
import           Hasura.RQL.Types.Common
import           Hasura.SQL.Backend


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

data AnnInsert (b :: Backend) v
  = AnnInsert
  { _aiFieldName :: !Text
  , _aiIsSingle  :: Bool
  , _aiData      :: AnnMultiInsert b v
  }

data AnnIns (b :: Backend) a v
  = AnnIns
  { _aiInsObj         :: !a
  , _aiTableName      :: !QualifiedTable
  , _aiConflictClause :: !(Maybe (RQL.ConflictClauseP1 b v))
  , _aiCheckCond      :: !(AnnBoolExp b v, Maybe (AnnBoolExp b v))
  , _aiTableCols      :: ![ColumnInfo b]
  , _aiDefVals        :: !(PreSetColsG b v)
  }

type SingleObjIns b v = AnnIns b (AnnInsObj b v) v
type MultiObjIns  b v = AnnIns b [AnnInsObj b v] v

data RelIns a
  = RelIns
  { _riAnnIns  :: !a
  , _riRelInfo :: !RelInfo
  } deriving (Show, Eq)

type ObjRelIns b v = RelIns (SingleObjIns b v)
type ArrRelIns b v = RelIns (MultiObjIns  b v)

data AnnInsObj (b :: Backend) v
  = AnnInsObj
  { _aioColumns :: ![(Column b, v)]
  , _aioObjRels :: ![ObjRelIns b v]
  , _aioArrRels :: ![ArrRelIns b v]
  }

type AnnSingleInsert b v = (SingleObjIns b v, RQL.MutationOutputG b v)
type AnnMultiInsert  b v = (MultiObjIns  b v, RQL.MutationOutputG b v)

instance Semigroup (AnnInsObj backend v) where
  (AnnInsObj col1 obj1 rel1) <> (AnnInsObj col2 obj2 rel2) =
    AnnInsObj (col1 <> col2) (obj1 <> obj2) (rel1 <> rel2)

instance Monoid (AnnInsObj backend v) where
  mempty = AnnInsObj [] [] []
