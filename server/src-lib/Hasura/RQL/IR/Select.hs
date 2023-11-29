{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This modules defines the tree of Select types: how we represent a query internally, from its top
--   level 'QueryDB' down to each individual field. Most of those types have three type arguments:
--
--   b: BackendType
--     The backend that is targeted by that specific select (Postgres Vanilla, MSSQL...); we use the
--     type families in the Backend class to decide how different parts of the IR are represented in
--     different backends.
--
--   v: Type
--     The type of the leaf values in our AST; used almost exclusively for column values, over which
--     queries can be parameterized. The output of the parser phase will use @UnpreparedValue b@ for
--     the leaves, and most backends will then transform the AST to interpret those values and
--     consequently change @v@ to be @SQLExpression b@
--
--   r: BackendType -> Type
--     Joins across backends mean that the aforementioned @b@ parameter won't be the same throughout
--     the entire tree; at some point we will have an 'AnyBackend' used to encapsulate a branch that
--     uses a different @b@. We still want, however, to be able to parameterize the values of the
--     leaves in that separate branch, and that's what the @r@ parameter is for. We also use
--     'UnpreparedValue' here during the parsing phase, meaning all leaf values will be
--     @UnpreparedValue b@ for their respective backend @b@, and most backends will then transform
--     their AST, cutting all such remote branches, and therefore using @Const Void@ for @r@.
module Hasura.RQL.IR.Select
  ( AggregateField (..),
    AggregateFields,
    AggregateOp (..),
    AnnAggregateSelect,
    AnnAggregateSelectG,
    AnnColumnField (..),
    AnnField,
    AnnFieldG (..),
    AnnFields,
    AnnFieldsG,
    AnnNestedObjectSelectG (..),
    AnnNestedObjectSelect,
    AnnNestedArraySelectG (..),
    AnnNestedArraySelect,
    AnnObjectSelect,
    AnnObjectSelectG (..),
    AnnSimpleSelect,
    AnnSimpleSelectG,
    AnnSimpleStreamSelect,
    AnnSimpleStreamSelectG,
    ArrayAggregateSelect,
    ArrayAggregateSelectG,
    ArrayConnectionSelect,
    ArrayRelationSelectG,
    ArraySelect,
    ArraySelectFieldsG,
    ArraySelectG (..),
    SelectionField (..),
    SelectionFields,
    ComputedFieldScalarSelect (..),
    ComputedFieldSelect (..),
    ConnectionField (..),
    ConnectionFields,
    ConnectionSelect (..),
    ConnectionSlice (..),
    ConnectionSplit (..),
    ConnectionSplitKind (..),
    EdgeField (..),
    EdgeFields,
    ObjectRelationSelect,
    ObjectRelationSelectG,
    PageInfoField (..),
    PageInfoFields,
    QueryDB (..),
    RemoteSourceSelect (..),
    RemoteRelationshipSelect (..),
    SourceRelationshipSelection (..),
    TableAggregateField,
    TableAggregateFieldG (..),
    TableAggregateFields,
    TableAggregateFieldsG,
    GroupByG (..),
    GroupByField (..),
    GroupKeyField (..),
    CountDistinct (..),
    insertFunctionArg,
    mkAnnColumnField,
    mkAnnColumnFieldAsText,
    traverseSourceRelationshipSelection,
    module Hasura.RQL.IR.Select.AnnSelectG,
    module Hasura.RQL.IR.Select.Args,
    module Hasura.RQL.IR.Select.From,
    module Hasura.RQL.IR.Select.OrderBy,
    module Hasura.RQL.IR.Select.TablePerm,
    module Hasura.RQL.IR.Select.RelationSelect,
  )
where

import Data.Bifoldable
import Data.HashMap.Strict qualified as HashMap
import Data.Kind (Type)
import Data.List.NonEmpty qualified as NE
import Data.Sequence qualified as Seq
import Hasura.Function.Cache
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.IR.OrderBy
import Hasura.RQL.IR.Select.AnnSelectG
import Hasura.RQL.IR.Select.Args
import Hasura.RQL.IR.Select.From
import Hasura.RQL.IR.Select.OrderBy
import Hasura.RQL.IR.Select.RelationSelect
import Hasura.RQL.IR.Select.TablePerm
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.ComputedField
import Hasura.RQL.Types.Instances ()
import Hasura.RQL.Types.Relationships.Remote
import Hasura.RQL.Types.Schema.Options (StringifyNumbers)

-- Root selection

data QueryDB (b :: BackendType) (r :: Type) v
  = QDBMultipleRows (AnnSimpleSelectG b r v)
  | QDBSingleRow (AnnSimpleSelectG b r v)
  | QDBAggregation (AnnAggregateSelectG b r v)
  | QDBConnection (ConnectionSelect b r v)
  | QDBStreamMultipleRows (AnnSimpleStreamSelectG b r v)
  deriving stock (Generic, Functor, Foldable, Traversable)

instance (Backend b) => Bifoldable (QueryDB b) where
  bifoldMap f g = \case
    QDBMultipleRows annSel -> bifoldMapAnnSelectG f g annSel
    QDBSingleRow annSel -> bifoldMapAnnSelectG f g annSel
    QDBAggregation annSel -> bifoldMapAnnSelectG f g annSel
    QDBConnection connSel -> bifoldMap f g connSel
    QDBStreamMultipleRows annSel -> bifoldMapAnnSelectStreamG f g annSel

-- Select

type AnnSimpleSelectG b r v = AnnSelectG b (AnnFieldG b r) v

type AnnAggregateSelectG b r v = AnnSelectG b (TableAggregateFieldG b r) v

type AnnSimpleStreamSelectG b r v = AnnSelectStreamG b (AnnFieldG b r) v

type AnnSimpleSelect b = AnnSimpleSelectG b Void (SQLExpression b)

type AnnAggregateSelect b = AnnAggregateSelectG b Void (SQLExpression b)

type AnnSimpleStreamSelect b = AnnSimpleStreamSelectG b Void (SQLExpression b)

-- Relay select

data ConnectionSelect (b :: BackendType) (r :: Type) v = ConnectionSelect
  { _csXRelay :: XRelay b,
    _csPrimaryKeyColumns :: PrimaryKeyColumns b,
    _csSplit :: Maybe (NE.NonEmpty (ConnectionSplit b v)),
    _csSlice :: Maybe ConnectionSlice,
    _csSelect :: (AnnSelectG b (ConnectionField b r) v)
  }
  deriving stock (Functor, Foldable, Traversable)

deriving stock instance
  ( Backend b,
    Eq (AnnSelectG b (ConnectionField b r) v),
    Eq (ConnectionSlice),
    Eq (ConnectionSplit b v),
    Eq (PrimaryKeyColumns b)
  ) =>
  Eq (ConnectionSelect b r v)

deriving stock instance
  ( Backend b,
    Show (AnnSelectG b (ConnectionField b r) v),
    Show (ConnectionSlice),
    Show (ConnectionSplit b v),
    Show (PrimaryKeyColumns b)
  ) =>
  Show (ConnectionSelect b r v)

instance (Backend b) => Bifoldable (ConnectionSelect b) where
  bifoldMap f g ConnectionSelect {..} =
    foldMap (foldMap $ foldMap g) _csSplit
      <> bifoldMapAnnSelectG f g _csSelect

data ConnectionSplit (b :: BackendType) v = ConnectionSplit
  { _csKind :: ConnectionSplitKind,
    _csValue :: v,
    _csOrderBy :: (OrderByItemG b (AnnotatedOrderByElement b v))
  }
  deriving stock (Functor, Generic, Foldable, Traversable)

deriving stock instance
  ( Backend b,
    Eq v,
    Eq (OrderByItemG b (AnnotatedOrderByElement b v))
  ) =>
  Eq (ConnectionSplit b v)

deriving stock instance
  ( Backend b,
    Show v,
    Show (OrderByItemG b (AnnotatedOrderByElement b v))
  ) =>
  Show (ConnectionSplit b v)

instance
  ( Backend b,
    Hashable v,
    Hashable (OrderByItemG b (AnnotatedOrderByElement b v))
  ) =>
  Hashable (ConnectionSplit b v)

data ConnectionSlice
  = SliceFirst Int
  | SliceLast Int
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable)

data ConnectionSplitKind
  = CSKBefore
  | CSKAfter
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable)

-- Fields

-- | captures a remote relationship's selection and the necessary context
data RemoteRelationshipSelect b r = RemoteRelationshipSelect
  { -- | The fields on the table that are required for the join condition
    -- of the remote relationship
    _rrsLHSJoinFields :: HashMap FieldName (DBJoinField b),
    -- | The field that captures the relationship
    -- r ~ (RemoteRelationshipField UnpreparedValue) when the AST is emitted by the parser.
    -- r ~ Void when an execution tree is constructed so that a backend is
    -- absolved of dealing with remote relationships.
    _rrsRelationship :: r
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

data AnnFieldG (b :: BackendType) (r :: Type) v
  = AFColumn (AnnColumnField b v)
  | AFObjectRelation (ObjectRelationSelectG b r v)
  | AFArrayRelation (ArraySelectG b r v)
  | AFComputedField (XComputedField b) ComputedFieldName (ComputedFieldSelect b r v)
  | -- | A remote relationship field
    AFRemote (RemoteRelationshipSelect b r)
  | AFNodeId (XRelay b) SourceName (TableName b) (PrimaryKeyColumns b)
  | AFExpression Text
  | -- | Nested object.
    AFNestedObject (AnnNestedObjectSelectG b r v) -- TODO(dmoverton): move XNestedObject to a field in AFNestedObject constructor for consistency with AFNestedArray
  | -- | Nested array
    AFNestedArray (XNestedObjects b) (AnnNestedArraySelectG b r v)
  deriving stock (Functor, Foldable, Traversable)

deriving stock instance
  ( Backend b,
    Eq (AnnColumnField b v),
    Eq (ArraySelectG b r v),
    Eq (ComputedFieldSelect b r v),
    Eq (ObjectRelationSelectG b r v),
    Eq (RemoteRelationshipSelect b r),
    Eq (AnnNestedObjectSelectG b r v),
    Eq (AnnNestedArraySelectG b r v)
  ) =>
  Eq (AnnFieldG b r v)

deriving stock instance
  ( Backend b,
    Show (AnnColumnField b v),
    Show (ArraySelectG b r v),
    Show (ComputedFieldSelect b r v),
    Show (ObjectRelationSelectG b r v),
    Show (RemoteRelationshipSelect b r),
    Show (AnnNestedObjectSelectG b r v),
    Show (AnnNestedArraySelectG b r v)
  ) =>
  Show (AnnFieldG b r v)

instance (Backend b) => Bifoldable (AnnFieldG b) where
  bifoldMap f g = \case
    AFColumn col -> foldMap g col
    AFObjectRelation objRel -> foldMap (bifoldMap f g) objRel
    AFArrayRelation arrRel -> bifoldMap f g arrRel
    AFComputedField _ _ cf -> bifoldMap f g cf
    AFRemote r -> foldMap f r
    AFNodeId {} -> mempty
    AFExpression {} -> mempty
    AFNestedObject no -> bifoldMap f g no
    AFNestedArray _ na -> bifoldMap f g na

type AnnField b = AnnFieldG b Void (SQLExpression b)

type AnnFields b = AnnFieldsG b Void (SQLExpression b)

mkAnnColumnField ::
  Column backend ->
  ColumnType backend ->
  AnnRedactionExp backend v ->
  Maybe (ScalarSelectionArguments backend) ->
  AnnFieldG backend r v
mkAnnColumnField col typ redactionExp colOpM =
  AFColumn (AnnColumnField col typ False colOpM redactionExp)

mkAnnColumnFieldAsText ::
  ColumnInfo backend ->
  AnnFieldG backend r v
mkAnnColumnFieldAsText ci =
  AFColumn (AnnColumnField (ciColumn ci) (ciType ci) True Nothing NoRedaction)

traverseSourceRelationshipSelection ::
  (Applicative f, Backend backend) =>
  (vf backend -> f (vg backend)) ->
  SourceRelationshipSelection backend r vf ->
  f (SourceRelationshipSelection backend r vg)
traverseSourceRelationshipSelection f = \case
  SourceRelationshipObject s ->
    SourceRelationshipObject <$> traverse f s
  SourceRelationshipArray s ->
    SourceRelationshipArray <$> traverse f s
  SourceRelationshipArrayAggregate s ->
    SourceRelationshipArrayAggregate <$> traverse f s

-- Aggregation fields

data TableAggregateFieldG (b :: BackendType) (r :: Type) v
  = TAFAgg (AggregateFields b v)
  | TAFNodes (XNodesAgg b) (AnnFieldsG b r v)
  | TAFGroupBy (XGroupBy b) (GroupByG b r v)
  | TAFExp Text
  deriving stock (Functor, Foldable, Traversable)

deriving stock instance
  ( Backend b,
    Eq (AggregateFields b v),
    Eq (AnnFieldsG b r v),
    Eq (GroupByG b r v)
  ) =>
  Eq (TableAggregateFieldG b r v)

deriving stock instance
  ( Backend b,
    Show (AggregateFields b v),
    Show (AnnFieldsG b r v),
    Show (GroupByG b r v)
  ) =>
  Show (TableAggregateFieldG b r v)

instance (Backend b) => Bifoldable (TableAggregateFieldG b) where
  bifoldMap :: (Monoid m) => (r -> m) -> (v -> m) -> TableAggregateFieldG b r v -> m
  bifoldMap mapR mapV = \case
    TAFAgg aggFields -> foldMap (foldMap $ foldMap mapV) aggFields
    TAFNodes _ fields -> foldMap (foldMap $ bifoldMap mapR mapV) fields
    TAFGroupBy _ groupByFields -> bifoldMap mapR mapV groupByFields
    TAFExp {} -> mempty

data AggregateField (b :: BackendType) v
  = AFCount (CountType b v)
  | AFOp (AggregateOp b v)
  | AFExp Text

deriving stock instance (Backend b) => Functor (AggregateField b)

deriving stock instance (Backend b) => Foldable (AggregateField b)

deriving stock instance (Backend b) => Traversable (AggregateField b)

deriving stock instance
  (Backend b, Eq (CountType b v), Eq (AggregateOp b v), Eq v) =>
  Eq (AggregateField b v)

deriving stock instance
  (Backend b, Show (CountType b v), Show (AggregateOp b v), Show v) =>
  Show (AggregateField b v)

data AggregateOp (b :: BackendType) v = AggregateOp
  { _aoOp :: Text,
    _aoFields :: SelectionFields b v
  }
  deriving (Functor, Foldable, Traversable)

deriving stock instance
  (Backend b, Eq (SelectionFields b v), Eq v) =>
  Eq (AggregateOp b v)

deriving stock instance
  (Backend b, Show (SelectionFields b v), Show v) =>
  Show (AggregateOp b v)

data GroupByG (b :: BackendType) r v = GroupByG
  { _gbgKeys :: [GroupKeyField b],
    _gbgFields :: Fields (GroupByField b r v)
  }
  deriving (Functor, Foldable, Traversable)

deriving stock instance (Backend b, Eq (GroupByField b r v), Eq (GroupKeyField b)) => Eq (GroupByG b r v)

deriving stock instance (Backend b, Show (GroupByField b r v), Show (GroupKeyField b)) => Show (GroupByG b r v)

instance (Backend b) => Bifoldable (GroupByG b) where
  bifoldMap :: (Monoid m) => (r -> m) -> (v -> m) -> GroupByG b r v -> m
  bifoldMap mapR mapV GroupByG {..} =
    foldMap (foldMap $ bifoldMap mapR mapV) _gbgFields

data GroupByField (b :: BackendType) r v
  = GBFGroupKey (Fields (GroupKeyField b))
  | GBFAggregate (AggregateFields b v)
  | GBFNodes (AnnFieldsG b r v)
  | GBFExp Text
  deriving (Functor, Foldable, Traversable)

deriving stock instance (Backend b, Eq (GroupKeyField b), Eq (AggregateField b v), Eq (AnnFieldG b r v)) => Eq (GroupByField b r v)

deriving stock instance (Backend b, Show (GroupKeyField b), Show (AggregateField b v), Show (AnnFieldG b r v)) => Show (GroupByField b r v)

instance (Backend b) => Bifoldable (GroupByField b) where
  bifoldMap :: (Monoid m) => (r -> m) -> (v -> m) -> GroupByField b r v -> m
  bifoldMap mapR mapV = \case
    GBFGroupKey _groupKeyFields -> mempty
    GBFAggregate aggFields -> foldMap (foldMap $ foldMap mapV) aggFields
    GBFNodes fields -> foldMap (foldMap $ bifoldMap mapR mapV) fields
    GBFExp _text -> mempty

data GroupKeyField (b :: BackendType)
  = GKFColumn (Column b)
  | GKFExp Text

deriving stock instance (Backend b) => Eq (GroupKeyField b)

deriving stock instance (Backend b) => Show (GroupKeyField b)

-- | Types of fields that can be selected in a user query.
data SelectionField (b :: BackendType) v
  = SFCol
      (Column b)
      (ColumnType b)
      -- | This type is used to determine whether the column should be redacted
      -- before being aggregated
      (AnnRedactionExp b v)
  | SFComputedField ComputedFieldName (ComputedFieldScalarSelect b v)
  | SFExp Text
  deriving (Functor, Foldable, Traversable)

deriving stock instance
  (Backend b, Eq (FunctionArgumentExp b v), Eq (AnnRedactionExp b v), Eq v) =>
  Eq (SelectionField b v)

deriving stock instance
  (Backend b, Show (FunctionArgumentExp b v), Show (AnnRedactionExp b v), Show v) =>
  Show (SelectionField b v)

type TableAggregateField b = TableAggregateFieldG b Void (SQLExpression b)

type TableAggregateFields b = TableAggregateFieldsG b Void (SQLExpression b)

type TableAggregateFieldsG b r v = Fields (TableAggregateFieldG b r v)

type SelectionFields b v = Fields (SelectionField b v)

type AggregateFields b v = Fields (AggregateField b v)

type AnnFieldsG b r v = Fields (AnnFieldG b r v)

-- Relay fields

data ConnectionField (b :: BackendType) (r :: Type) v
  = ConnectionTypename Text
  | ConnectionPageInfo PageInfoFields
  | ConnectionEdges (EdgeFields b r v)
  deriving stock (Functor, Foldable, Traversable)

deriving stock instance
  ( Eq (EdgeFields b r v)
  ) =>
  Eq (ConnectionField b r v)

deriving stock instance
  ( Show (EdgeFields b r v)
  ) =>
  Show (ConnectionField b r v)

instance (Backend b) => Bifoldable (ConnectionField b) where
  bifoldMap f g = \case
    ConnectionTypename {} -> mempty
    ConnectionPageInfo {} -> mempty
    ConnectionEdges edgeFields -> foldMap (foldMap $ bifoldMap f g) edgeFields

data PageInfoField
  = PageInfoTypename Text
  | PageInfoHasNextPage
  | PageInfoHasPreviousPage
  | PageInfoStartCursor
  | PageInfoEndCursor
  deriving stock (Show, Eq)

data EdgeField (b :: BackendType) (r :: Type) v
  = EdgeTypename Text
  | EdgeCursor
  | EdgeNode (AnnFieldsG b r v)
  deriving stock (Functor, Foldable, Traversable)

deriving stock instance
  ( Eq (AnnFieldsG b r v)
  ) =>
  Eq (EdgeField b r v)

deriving stock instance
  ( Show (AnnFieldsG b r v)
  ) =>
  Show (EdgeField b r v)

instance (Backend b) => Bifoldable (EdgeField b) where
  bifoldMap f g = \case
    EdgeTypename {} -> mempty
    EdgeCursor -> mempty
    EdgeNode annFields -> foldMap (foldMap $ bifoldMap f g) annFields

type ConnectionFields b r v = Fields (ConnectionField b r v)

type PageInfoFields = Fields PageInfoField

type EdgeFields b r v = Fields (EdgeField b r v)

data AnnColumnField (b :: BackendType) v = AnnColumnField
  { _acfColumn :: Column b,
    _acfType :: ColumnType b,
    -- | If this field is 'True', columns are explicitly casted to @text@ when
    -- fetched, which avoids an issue that occurs because we don’t currently
    -- have proper support for array types. See
    -- https://github.com/hasura/graphql-engine/pull/3198 for more details.
    _acfAsText :: Bool,
    -- | Arguments of this column's selection. See 'ScalarSelectionArguments'
    _acfArguments :: Maybe (ScalarSelectionArguments b),
    -- | This type is used to determine whether the column should be redacted
    _acfRedactionExpression :: AnnRedactionExp b v
  }
  deriving stock (Functor, Foldable, Traversable)

deriving stock instance
  ( Backend b,
    Eq (AnnRedactionExp b v)
  ) =>
  Eq (AnnColumnField b v)

deriving stock instance
  ( Backend b,
    Show (AnnRedactionExp b v)
  ) =>
  Show (AnnColumnField b v)

-- Computed field

data ComputedFieldScalarSelect (b :: BackendType) v = ComputedFieldScalarSelect
  { _cfssFunction :: FunctionName b,
    _cfssArguments :: FunctionArgsExp b v,
    _cfssType :: ScalarType b,
    _cfssScalarArguments :: (Maybe (ScalarSelectionArguments b)),
    -- | This type is used to determine whether the computed field should be redacted
    _cfssRedactionExpression :: AnnRedactionExp b v
  }
  deriving stock (Functor, Foldable, Traversable)

deriving stock instance
  ( Backend b,
    Show v,
    Show (FunctionArgumentExp b v),
    Show (AnnRedactionExp b v)
  ) =>
  Show (ComputedFieldScalarSelect b v)

deriving stock instance
  ( Backend b,
    Eq v,
    Eq (FunctionArgumentExp b v),
    Eq (AnnRedactionExp b v)
  ) =>
  Eq (ComputedFieldScalarSelect b v)

data ComputedFieldSelect (b :: BackendType) (r :: Type) v
  = CFSScalar
      -- | Type containing info about the computed field
      (ComputedFieldScalarSelect b v)
  | CFSTable JsonAggSelect (AnnSimpleSelectG b r v)
  deriving stock (Functor, Foldable, Traversable)

deriving stock instance
  ( Backend b,
    Eq (AnnSimpleSelectG b r v),
    Eq (ComputedFieldScalarSelect b v)
  ) =>
  Eq (ComputedFieldSelect b r v)

deriving stock instance
  ( Backend b,
    Show (AnnSimpleSelectG b r v),
    Show (ComputedFieldScalarSelect b v)
  ) =>
  Show (ComputedFieldSelect b r v)

instance (Backend b) => Bifoldable (ComputedFieldSelect b) where
  bifoldMap f g = \case
    CFSScalar cfsSelect -> foldMap g cfsSelect
    CFSTable _ simpleSelect -> bifoldMapAnnSelectG f g simpleSelect

-- Local relationship

type ArrayRelationSelectG b r v = AnnRelationSelectG b (AnnSimpleSelectG b r v)

type ArrayAggregateSelectG b r v = AnnRelationSelectG b (AnnAggregateSelectG b r v)

type ArrayConnectionSelect b r v = AnnRelationSelectG b (ConnectionSelect b r v)

type ArrayAggregateSelect b = ArrayAggregateSelectG b Void (SQLExpression b)

data AnnObjectSelectG (b :: BackendType) (r :: Type) v = AnnObjectSelectG
  { _aosFields :: AnnFieldsG b r v,
    _aosTarget :: SelectFromG b v,
    _aosTargetFilter :: (AnnBoolExp b v)
  }
  deriving stock (Functor, Foldable, Traversable)

deriving stock instance
  ( Backend b,
    Eq (SelectFromG b v),
    Eq (AnnBoolExp b v),
    Eq (AnnFieldsG b r v)
  ) =>
  Eq (AnnObjectSelectG b r v)

deriving stock instance
  ( Backend b,
    Show (SelectFromG b v),
    Show (AnnBoolExp b v),
    Show (AnnFieldsG b r v)
  ) =>
  Show (AnnObjectSelectG b r v)

instance (Backend b) => Bifoldable (AnnObjectSelectG b) where
  bifoldMap f g AnnObjectSelectG {..} =
    foldMap (foldMap $ bifoldMap f g) _aosFields <> foldMap (foldMap g) _aosTargetFilter

type AnnObjectSelect b r = AnnObjectSelectG b r (SQLExpression b)

type ObjectRelationSelectG b r v = AnnRelationSelectG b (AnnObjectSelectG b r v)

type ObjectRelationSelect b = ObjectRelationSelectG b Void (SQLExpression b)

data ArraySelectG (b :: BackendType) (r :: Type) v
  = ASSimple (ArrayRelationSelectG b r v)
  | ASAggregate (ArrayAggregateSelectG b r v)
  | ASConnection (ArrayConnectionSelect b r v)
  deriving stock (Functor, Foldable, Traversable)

deriving stock instance
  ( Eq (ArrayRelationSelectG b r v),
    Eq (ArrayAggregateSelectG b r v),
    Eq (ArrayConnectionSelect b r v)
  ) =>
  Eq (ArraySelectG b r v)

deriving stock instance
  ( Show (ArrayRelationSelectG b r v),
    Show (ArrayAggregateSelectG b r v),
    Show (ArrayConnectionSelect b r v)
  ) =>
  Show (ArraySelectG b r v)

instance (Backend b) => Bifoldable (ArraySelectG b) where
  bifoldMap f g = \case
    ASSimple arrayRelationSelect -> foldMap (bifoldMapAnnSelectG f g) arrayRelationSelect
    ASAggregate arrayAggregateSelect -> foldMap (bifoldMapAnnSelectG f g) arrayAggregateSelect
    ASConnection arrayConnectionSelect -> foldMap (bifoldMap f g) arrayConnectionSelect

type ArraySelect b = ArraySelectG b Void (SQLExpression b)

type ArraySelectFieldsG b r v = Fields (ArraySelectG b r v)

-- | Captures the selection set of a remote source relationship.
data
  SourceRelationshipSelection
    (b :: BackendType)
    (r :: Type)
    (vf :: BackendType -> Type)
  = SourceRelationshipObject (AnnObjectSelectG b r (vf b))
  | SourceRelationshipArray (AnnSimpleSelectG b r (vf b))
  | SourceRelationshipArrayAggregate (AnnAggregateSelectG b r (vf b))

deriving stock instance
  ( Backend b,
    Eq (AnnAggregateSelectG b r (vf b)),
    Eq (AnnObjectSelectG b r (vf b)),
    Eq (AnnSimpleSelectG b r (vf b))
  ) =>
  Eq (SourceRelationshipSelection b r vf)

deriving stock instance
  ( Backend b,
    Show (AnnAggregateSelectG b r (vf b)),
    Show (AnnObjectSelectG b r (vf b)),
    Show (AnnSimpleSelectG b r (vf b))
  ) =>
  Show (SourceRelationshipSelection b r vf)

-- | A relationship to a remote source. 'vf' (could use a better name) is
-- analogous to 'v' in other IR types such as 'AnnFieldG'. vf's kind is
-- (BackendType -> Type) instead of v's 'Type' so that 'v' of 'AnnFieldG' can
-- be specific to the backend that it captures ('b' of an AnnFieldG changes as
-- we walk down the IR branches which capture relationships to other databases)
data
  RemoteSourceSelect
    (r :: Type)
    (vf :: BackendType -> Type)
    (tgt :: BackendType) = RemoteSourceSelect
  { _rssName :: SourceName,
    _rssConfig :: SourceConfig tgt,
    _rssSelection :: SourceRelationshipSelection tgt r vf,
    -- | Additional information about the source's join columns:
    -- (ScalarType tgt) so that the remote can interpret the join values coming
    -- from src
    -- (Column tgt) so that an appropriate join condition / IN clause can be built
    -- by the remote
    _rssJoinMapping :: (HashMap.HashMap FieldName (ScalarType tgt, Column tgt)),
    _rssStringifyNums :: StringifyNumbers
  }

deriving stock instance
  ( Backend tgt,
    Eq (SourceRelationshipSelection tgt r vf)
  ) =>
  Eq (RemoteSourceSelect r vf tgt)

deriving stock instance
  ( Backend tgt,
    Show (SourceRelationshipSelection tgt r vf),
    Show (SourceConfig tgt)
  ) =>
  Show (RemoteSourceSelect r vf tgt)

-- Nested objects

data AnnNestedObjectSelectG (b :: BackendType) (r :: Type) v = AnnNestedObjectSelectG
  { _anosSupportsNestedObjects :: XNestedObjects b,
    _anosColumn :: Column b,
    _anosFields :: AnnFieldsG b r v
  }
  deriving stock (Functor, Foldable, Traversable)

deriving stock instance
  ( Backend b,
    Eq (AnnFieldsG b r v)
  ) =>
  Eq (AnnNestedObjectSelectG b r v)

deriving stock instance
  ( Backend b,
    Show (AnnFieldsG b r v)
  ) =>
  Show (AnnNestedObjectSelectG b r v)

instance (Backend b) => Bifoldable (AnnNestedObjectSelectG b) where
  bifoldMap f g AnnNestedObjectSelectG {..} =
    foldMap (foldMap $ bifoldMap f g) _anosFields

type AnnNestedObjectSelect b r = AnnNestedObjectSelectG b r (SQLExpression b)

-- Nested arrays

data AnnNestedArraySelectG (b :: BackendType) (r :: Type) v
  = ANASSimple (AnnFieldG b r v)
  | ANASAggregate (AnnAggregateSelectG b r v)
  deriving stock (Functor, Foldable, Traversable)

deriving stock instance
  (Backend b, Eq (AnnFieldG b r v), Eq (AnnAggregateSelectG b r v)) => Eq (AnnNestedArraySelectG b r v)

deriving stock instance
  (Backend b, Show (AnnFieldG b r v), Show (AnnAggregateSelectG b r v)) => Show (AnnNestedArraySelectG b r v)

instance (Backend b) => Bifoldable (AnnNestedArraySelectG b) where
  bifoldMap f g = \case
    ANASSimple field -> bifoldMap f g field
    ANASAggregate agg -> bifoldMapAnnSelectG f g agg

type AnnNestedArraySelect b r = AnnNestedArraySelectG b r (SQLExpression b)

-- | If argument positional index is less than or equal to length of
-- 'positional' arguments then insert the value in 'positional' arguments else
-- insert the value with argument name in 'named' arguments
insertFunctionArg ::
  FunctionArgName ->
  Int ->
  a ->
  FunctionArgsExpG a ->
  FunctionArgsExpG a
insertFunctionArg argName idx value (FunctionArgsExp positional named) =
  if (idx + 1) <= length positional
    then FunctionArgsExp (insertAt idx value positional) named
    else
      FunctionArgsExp positional
        $ HashMap.insert (getFuncArgNameTxt argName) value named
  where
    insertAt i a = toList . Seq.insertAt i a . Seq.fromList

-- | The "distinct" input field inside "count" aggregate field
--
-- count (
--   distinct: Boolean
-- ): Int
data CountDistinct
  = SelectCountDistinct
  | SelectCountNonDistinct
