{-# LANGUAGE TemplateHaskell #-}

-- | Internal representation of an insertion in a database table.
--
-- What makes this specific mutation tricky is that we support recursive
-- insertions, across local relationships. Because local joins come in two
-- different kinds (object relationships and array relations), and because for each
-- table we expose two root different root fields (insert_one and insert), we
-- distinguish between *single row inserts* and *multi rows inserts*.
--
-- TODO: the distinction between single-row inserts and multi-rows inserts does not
-- need to be enforced the way it currently is, with booleans and different
-- types. The distinction could be made in the translation layer, if need be.
module Hasura.RQL.IR.Insert
  ( AnnotatedInsertData (..),
    AnnotatedInsert (..),
    aiFieldName,
    aiIsSingle,
    aiData,
    aiOutput,
    aiNamingConvention,
    AnnotatedInsertField (..),
    AnnotatedInsertRow,
    ArrayRelationInsert,
    OnConflictClause (..),
    OnConflictClauseData (..),
    ConflictTarget (..),
    InsertQueryP1 (..),
    MultiObjectInsert,
    ObjectRelationInsert,
    RelationInsert (..),
    Single (..),
    SingleObjectInsert,
    getInsertArrayRelationships,
    getInsertColumns,
    getInsertObjectRelationships,
    _AIArrayRelationship,
    _AIColumn,
    _AIObjectRelationship,
  )
where

import Control.Lens ((^?))
import Control.Lens.TH (makeLenses, makePrisms)
import Data.Kind (Type)
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.IR.Conflict
import Hasura.RQL.IR.Returning
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.NamingCase (NamingCase)
import Hasura.RQL.Types.Permission
import Hasura.RQL.Types.Relationships.Local

-- | Overall representation of an insert mutation, corresponding to one root
-- field in our mutation, including the parsed selection set of the mutation's
-- output. For historical reasons, it will always contain a `MultiObjectInsert`,
-- whether the root mutation is a single row or not, and will distinguish
-- between them using a boolean field.
data AnnotatedInsert (b :: BackendType) (r :: Type) v = AnnotatedInsert
  { _aiFieldName :: Text,
    _aiIsSingle :: Bool,
    _aiData :: MultiObjectInsert b v,
    _aiOutput :: MutationOutputG b r v,
    _aiNamingConvention :: Maybe NamingCase
  }
  deriving (Functor, Foldable, Traversable)

-- | One individual insert, one node from the tree.
-- The @f@ parameter is used to construct the container for the values to be
-- inserted: 'Single' for a single-row insert, '[]' for a multi-row insert.
data AnnotatedInsertData (b :: BackendType) (f :: Type -> Type) (v :: Type) = AnnotatedInsertData
  { _aiInsertObject :: f (AnnotatedInsertRow b v),
    _aiTableName :: TableName b,
    _aiCheckCondition :: (AnnBoolExp b v, Maybe (AnnBoolExp b v)),
    _aiTableColumns :: [ColumnInfo b],
    _aiPrimaryKey :: Maybe (NESeq (Column b)),
    _aiExtraTableMetadata :: ExtraTableMetadata b,
    _aiPresetValues :: PreSetColsG b v,
    _aiBackendInsert :: BackendInsert b v,
    _aiValidateInput :: Maybe (ValidateInput ResolvedWebhook)
  }
  deriving (Functor, Foldable, Traversable)

-- | Ad-hoc helper.
-- We differientate between single row inserts ('SingleObjIns') and multiple row
-- inserts ('MultiObjIns'), but both use the same underlying representation:
-- 'AnnIns'. The only difference is which functor is used as a parameter. We use
-- '[]' for 'MultiObjIns', and we use this trivial 'Single' for 'SingleObjIns'.
newtype Single a = Single {unSingle :: a}
  deriving (Functor, Foldable, Traversable)

type SingleObjectInsert b v = AnnotatedInsertData b Single v

type MultiObjectInsert b v = AnnotatedInsertData b [] v

-- | An insert item.
-- The object and array relationships are not unavailable when 'XNestedInserts b = XDisable'
data AnnotatedInsertField (b :: BackendType) v
  = AIColumn (Column b, v)
  | AIObjectRelationship (XNestedInserts b) (ObjectRelationInsert b v)
  | AIArrayRelationship (XNestedInserts b) (ArrayRelationInsert b v)
  deriving (Functor, Foldable, Traversable)

-- | One individual row to be inserted.
-- Contains the columns' values and all the matching recursive relationship inserts.
type AnnotatedInsertRow b v = [AnnotatedInsertField b v]

-- | One individual relationship.
-- Unlike other types, this one is not parameterized by the type of the leaves
-- @v@, but by the kind of insert has to be performed: multi-row or single row.
-- See 'ObjectRelationInsert' and 'ArrayRelationInsert'.
data RelationInsert (b :: BackendType) a = RelationInsert
  { _riInsertData :: a,
    _riRelationInfo :: RelInfo b
  }
  deriving (Show, Eq, Functor, Foldable, Traversable)

-- | Insert across an object relationship.
-- Object relationships are 1:1 relationships across tables; an insert across
-- such a relationship can only insert one single row at a time; 'RelIns' is
-- therefore parameterized by a 'SingleObjectInsert'.
type ObjectRelationInsert b v = RelationInsert b (SingleObjectInsert b v)

-- | Insert across an array relationship.
-- Array relationships are 1:* relationships across tables; an insert across
-- such a relationship may therefore contain multiple rows; 'RelIns' is
-- therefore parameterized by a 'MultiObjectInsert'.
type ArrayRelationInsert b v = RelationInsert b (MultiObjectInsert b v)

-- | Old-style representation used for non-recursive insertions.
-- This is the representation used by RQL.DML, instead of the new fancy
-- recursive one present in this file. Postgres supports both representations,
-- and actually translates recursive queries that do not have any relationships
-- into this representation first.
data InsertQueryP1 (b :: BackendType) = InsertQueryP1
  { iqp1Table :: TableName b,
    iqp1Cols :: [Column b],
    iqp1Tuples :: [[SQLExpression b]],
    iqp1Conflict :: Maybe (OnConflictClause b (SQLExpression b)),
    iqp1CheckCond :: (AnnBoolExpSQL b, Maybe (AnnBoolExpSQL b)),
    iqp1Output :: MutationOutput b,
    iqp1AllCols :: [ColumnInfo b]
  }

$(makeLenses ''AnnotatedInsert)
$(makePrisms ''AnnotatedInsertField)

getInsertColumns :: AnnotatedInsertRow b v -> [(Column b, v)]
getInsertColumns = mapMaybe (^? _AIColumn)

getInsertObjectRelationships :: AnnotatedInsertRow b v -> [ObjectRelationInsert b v]
getInsertObjectRelationships = mapMaybe (fmap snd . (^? _AIObjectRelationship))

getInsertArrayRelationships :: AnnotatedInsertRow b v -> [ArrayRelationInsert b v]
getInsertArrayRelationships = mapMaybe (fmap snd . (^? _AIArrayRelationship))
