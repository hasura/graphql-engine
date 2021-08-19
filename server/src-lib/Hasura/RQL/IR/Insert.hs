{- | Internal representation of an insertion in a database table.

What makes this specific mutation tricky is that we support recursive
insertions, across local relationships. Because local joins come in two
different kinds (object relationships and array relations), and because for each
table we expose two root different root fields (insert_one and insert), we
distinguish between *single row inserts* and *multi rows inserts*.

TODO: the distinction between single-row inserts and multi-rows inserts does not
need to be enforced the way it currently is, with booleans and different
types. The distinction could be made in the translation layer, if need be.

-}

module Hasura.RQL.IR.Insert where

import           Hasura.Prelude

import           Data.Kind                     (Type)

import           Hasura.RQL.IR.BoolExp
import           Hasura.RQL.IR.Returning
import           Hasura.RQL.Types.Backend
import           Hasura.RQL.Types.Column
import           Hasura.RQL.Types.Relationship
import           Hasura.SQL.Backend


-- | Overall representation of an insert mutation, corresponding to one root
-- field in our mutation, including the parsed selection set of the mutation's
-- output. For historical reasons, it will always contain a `MultiObjIns`,
-- whether the root mutation is a single row or not, and will distinguish
-- between them using a boolean field.

data AnnInsert (b :: BackendType) (r :: BackendType -> Type) v
  = AnnInsert
  { _aiFieldName :: !Text
  , _aiIsSingle  :: !Bool
  , _aiData      :: !(MultiObjIns b v)
  , _aiOutput    :: !(MutationOutputG b r v)
  } deriving (Functor, Foldable, Traversable)


-- | One individual insert, one node from the tree.
-- The @f@ parameter is used to construct the container for the values to be
-- inserted: 'Single' for a single-row insert, '[]' for a multi-row insert.

data AnnIns (b :: BackendType) (f :: Type -> Type) (v :: Type)
  = AnnIns
  { _aiInsObj         :: !(f (AnnInsObj b v))
  , _aiTableName      :: !(TableName b)
  , _aiConflictClause :: !(Maybe (ConflictClauseP1 b v))
  , _aiCheckCond      :: !(AnnBoolExp b v, Maybe (AnnBoolExp b v))
  , _aiTableCols      :: ![ColumnInfo b]
  , _aiDefVals        :: !(PreSetColsG b v)
  } deriving (Functor, Foldable, Traversable)

-- | Ad-hoc helper.
-- We differientate between single row inserts ('SingleObjIns') and multiple row
-- inserts ('MultiObjIns'), but both use the same underlying representation:
-- 'AnnIns'. The only difference is which functor is used as a parameter. We use
-- '[]' for 'MultiObjIns', and we use this trivial 'Single' for 'SingleObjIns'.

newtype Single a = Single { unSingle :: a }
  deriving (Functor, Foldable, Traversable)

type SingleObjIns b v = AnnIns b Single v
type MultiObjIns  b v = AnnIns b []     v


-- | One individual row to be inserted.
-- Contains the columns' values and all the matching recursive relationship inserts.

data AnnInsObj (b :: BackendType) v
  = AnnInsObj
  { _aioColumns :: ![(Column b, v)]
  , _aioObjRels :: ![ObjRelIns b v]
  , _aioArrRels :: ![ArrRelIns b v]
  } deriving (Functor, Foldable, Traversable)

instance Semigroup (AnnInsObj backend v) where
  (AnnInsObj col1 obj1 rel1) <> (AnnInsObj col2 obj2 rel2) =
    AnnInsObj (col1 <> col2) (obj1 <> obj2) (rel1 <> rel2)

instance Monoid (AnnInsObj backend v) where
  mempty = AnnInsObj [] [] []


-- | One individual relationship.
-- Unlike other types, this one is not parameterized by the type of the leaves
-- @v@, but by the kind of insert has to be performed: multi-row or single row.
-- See 'ObjRelIns' and 'ArrRelIns'.

data RelIns (b :: BackendType) a
  = RelIns
  { _riAnnIns  :: !a
  , _riRelInfo :: !(RelInfo b)
  } deriving (Show, Eq, Functor, Foldable, Traversable)

-- | Insert across an object relationship.
-- Object relationships are 1:1 relationships across tables; an insert across
-- such a relationship can only insert one single row at a time; 'RelIns' is
-- therefore parameterized by a 'SingleObjIns'.

type ObjRelIns b v = RelIns b (SingleObjIns b v)

-- | Insert across an array relationship.
-- Array relationships are 1:* relationships across tables; an insert across
-- such a relationship may therefore contain multiple rows; 'RelIns' is
-- therefore parameterized by a 'MultiObjIns'.

type ArrRelIns b v = RelIns b (MultiObjIns  b v)


-- Conflict resolution options

data ConflictTarget (b :: BackendType)
  = CTColumn ![Column b]
  | CTConstraint !(ConstraintName b)
deriving instance Backend b => Show (ConflictTarget b)
deriving instance Backend b => Eq   (ConflictTarget b)

data ConflictClauseP1 (b :: BackendType) v
  = CP1DoNothing !(Maybe (ConflictTarget b))
  | CP1Update !(ConflictTarget b) ![Column b] !(PreSetColsG b v) (AnnBoolExp b v)
  deriving (Functor, Foldable, Traversable)


-- | Old-style representation used for non-recursive insertions.
-- This is the representation used by RQL.DML, instead of the new fancy
-- recursive one present in this file. Postgres supports both representations,
-- and actually translates recursive queries that do not have any relationships
-- into this representation first.

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
