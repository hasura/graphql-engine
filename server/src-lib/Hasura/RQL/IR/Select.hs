{-# LANGUAGE UndecidableInstances #-}

{- | This modules defines the tree of Select types: how we represent a query internally, from its top
   level 'QueryDB' down to each individual field. Most of those types have three type arguments:

   b: BackendType
     The backend that is targeted by that specific select (Postgres Vanilla, MSSQL...); we use the
     type families in the Backend class to decide how different parts of the IR are represented in
     different backends.

   v: Type
     The type of the leaf values in our AST; used almost exclusively for column values, over which
     queries can be parameterized. The output of the parser phase will use @UnpreparedValue b@ for
     the leaves, and most backends will then transform the AST to interpret those values and
     consequently change @v@ to be @SQLExpression b@

   r: BackendType -> Type
     Joins across backends mean that the aforementioned @b@ parameter won't be the same throughout
     the entire tree; at some point we will have an 'AnyBackend' used to encapsulate a branch that
     uses a different @b@. We still want, however, to be able to parameterize the values of the
     leaves in that separate branch, and that's what the @r@ parameter is for. We also use
     'UnpreparedValue' here during the parsing phase, meaning all leaf values will be
     @UnpreparedValue b@ for their respective backend @b@, and most backends will then transform
     their AST, cutting all such remote branches, and therefore using @Const Void@ for @r@.
-}

module Hasura.RQL.IR.Select where

import           Hasura.Prelude

import qualified Data.HashMap.Strict                 as HM
import qualified Data.List.NonEmpty                  as NE
import qualified Data.Sequence                       as Seq
import qualified Language.GraphQL.Draft.Syntax       as G

import           Control.Lens.TH                     (makeLenses, makePrisms)
import           Data.Int                            (Int64)
import           Data.Kind                           (Type)

import qualified Hasura.SQL.AnyBackend               as AB

import           Hasura.GraphQL.Parser.Schema        (InputValue)
import           Hasura.RQL.IR.BoolExp
import           Hasura.RQL.IR.OrderBy
import           Hasura.RQL.Types.Backend
import           Hasura.RQL.Types.Column
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.Function
import           Hasura.RQL.Types.Instances          ()
import           Hasura.RQL.Types.Relationship
import           Hasura.RQL.Types.RemoteRelationship
import           Hasura.RQL.Types.RemoteSchema
import           Hasura.SQL.Backend


data QueryDB (b :: BackendType) (r :: BackendType -> Type) v
  = QDBMultipleRows (AnnSimpleSelG       b r v)
  | QDBSingleRow    (AnnSimpleSelG       b r v)
  | QDBAggregation  (AnnAggregateSelectG b r v)
  | QDBConnection   (ConnectionSelect    b r v)
  deriving stock (Generic)

traverseQueryDB
  :: forall backend r f a b
   . (Applicative f, Backend backend)
  => (a -> f b)
  -> QueryDB backend r a
  -> f (QueryDB backend r b)
traverseQueryDB f = \case
  QDBMultipleRows s -> QDBMultipleRows <$> traverseAnnSimpleSelect    f s
  QDBSingleRow    s -> QDBSingleRow    <$> traverseAnnSimpleSelect    f s
  QDBAggregation  s -> QDBAggregation  <$> traverseAnnAggregateSelect f s
  QDBConnection   s -> QDBConnection   <$> traverseConnectionSelect   f s



data AnnSelectG (b :: BackendType) a v
  = AnnSelectG
  { _asnFields   :: !a
  , _asnFrom     :: !(SelectFromG b v)
  , _asnPerm     :: !(TablePermG b v)
  , _asnArgs     :: !(SelectArgsG b v)
  , _asnStrfyNum :: !Bool
  } deriving (Generic)

deriving instance
  ( Show (BooleanOperators b v)
  , Backend b
  , Show v
  , Show a
  ) => Show (AnnSelectG b a v)

deriving instance
  ( Eq (BooleanOperators b v)
  , Backend b
  , Eq v
  , Eq a
  ) => Eq (AnnSelectG b a v)

instance
  ( Hashable (BooleanOperators b v)
  , Backend b
  , Hashable v
  , Hashable a
  ) => Hashable (AnnSelectG b a v)

type AnnSimpleSelG       b r v = AnnSelectG b (AnnFieldsG            b r v) v
type AnnAggregateSelectG b r v = AnnSelectG b (TableAggregateFieldsG b r v) v
type AnnSimpleSel        b     = AnnSimpleSelG       b (Const Void) (SQLExpression b)
type AnnAggregateSelect  b     = AnnAggregateSelectG b (Const Void) (SQLExpression b)

traverseAnnSelect
  :: (Applicative f, Backend backend)
  => (a -> f b) -> (v -> f w)
  -> AnnSelectG backend a v -> f (AnnSelectG backend b w)
traverseAnnSelect f1 f2 (AnnSelectG flds tabFrom perm args strfyNum) =
  AnnSelectG
  <$> f1 flds
  <*> traverse f2 tabFrom
  <*> traverseTablePerm f2 perm
  <*> traverseSelectArgs f2 args
  <*> pure strfyNum

traverseAnnSimpleSelect
  :: (Applicative f, Backend backend)
  => (a -> f b)
  -> AnnSimpleSelG backend r a -> f (AnnSimpleSelG backend r b)
traverseAnnSimpleSelect f = traverseAnnSelect (traverseAnnFields f) f


traverseAnnAggregateSelect
  :: (Applicative f, Backend backend)
  => (a -> f b)
  -> AnnAggregateSelectG backend r a -> f (AnnAggregateSelectG backend r b)
traverseAnnAggregateSelect f =
  traverseAnnSelect (traverse (traverse (traverseTableAggregateField f))) f


-- Relay select

data ConnectionSelect (b :: BackendType) (r :: BackendType -> Type) v
  = ConnectionSelect
  { _csXRelay            :: !(XRelay b)
  , _csPrimaryKeyColumns :: !(PrimaryKeyColumns b)
  , _csSplit             :: !(Maybe (NE.NonEmpty (ConnectionSplit b v)))
  , _csSlice             :: !(Maybe ConnectionSlice)
  , _csSelect            :: !(AnnSelectG b (ConnectionFields b r v) v)
  } deriving (Generic)

deriving instance
  ( Show (BooleanOperators b v)
  , Backend b
  , Show v
  , Show (r b)
  ) => Show (ConnectionSelect b r v)

deriving instance
  ( Eq (BooleanOperators b v)
  , Backend b
  , Eq v
  , Eq (r b)
  ) => Eq (ConnectionSelect b r v)

instance
  ( Hashable (BooleanOperators b v)
  , Backend b
  , Hashable v
  , Hashable (r b)
  ) => Hashable (ConnectionSelect b r v)

data ConnectionSplit (b :: BackendType) v
  = ConnectionSplit
  { _csKind    :: !ConnectionSplitKind
  , _csValue   :: !v
  , _csOrderBy :: !(OrderByItemG b (AnnOrderByElementG b ()))
  } deriving (Functor, Generic, Foldable, Traversable)

deriving instance
  ( Backend b
  , Show v
  ) => Show (ConnectionSplit b v)

deriving instance
  ( Backend b
  , Eq v
  ) => Eq (ConnectionSplit b v)

instance (Backend b, Hashable (ColumnInfo b), Hashable v) => Hashable (ConnectionSplit b v)

data ConnectionSlice
  = SliceFirst !Int
  | SliceLast !Int
  deriving (Show, Eq, Generic)
instance Hashable ConnectionSlice

data ConnectionSplitKind
  = CSKBefore
  | CSKAfter
  deriving (Show, Eq, Generic)
instance Hashable ConnectionSplitKind

traverseConnectionSelect
  :: (Applicative f, Backend backend)
  => (a -> f b)
  -> ConnectionSelect backend r a -> f (ConnectionSelect backend r b)
traverseConnectionSelect f (ConnectionSelect x pkCols cSplit cSlice sel) =
  ConnectionSelect x pkCols
  <$> traverse (traverse (traverseConnectionSplit f)) cSplit
  <*> pure cSlice
  <*> traverseAnnSelect (traverse (traverse (traverseConnectionField f))) f sel

traverseConnectionSplit
  :: (Applicative f)
  => (a -> f b) -> ConnectionSplit backend a -> f (ConnectionSplit backend b)
traverseConnectionSplit f (ConnectionSplit k v ob) =
  ConnectionSplit k <$> f v <*> pure ob

-- From

data SelectFromG (b :: BackendType) v
  = FromTable !(TableName b)
  | FromIdentifier !(Identifier b)
  | FromFunction !(FunctionName b)
                 !(FunctionArgsExpTableRow b v)
                 -- a definition list
                 !(Maybe [(Column b, ScalarType b)])
  deriving (Functor, Foldable, Traversable, Generic)
instance (Backend b, Hashable v) => Hashable (SelectFromG b v)

deriving instance
  ( Backend b
  , Show v
  ) => Show (SelectFromG b v)

deriving instance
  ( Backend b
  , Eq v
  ) => Eq (SelectFromG b v)

type SelectFrom b = SelectFromG b (SQLExpression b)


-- Select arguments

data SelectArgsG (b :: BackendType) v
  = SelectArgs
  { _saWhere    :: !(Maybe (AnnBoolExp b v))
  , _saOrderBy  :: !(Maybe (NE.NonEmpty (AnnOrderByItemG b v)))
  , _saLimit    :: !(Maybe Int)
  , _saOffset   :: !(Maybe Int64)
  , _saDistinct :: !(Maybe (NE.NonEmpty (Column b)))
  } deriving (Generic)

deriving instance
  ( Backend b
  , Eq (BooleanOperators b v)
  , Eq v
  ) => Eq (SelectArgsG b v)

deriving instance
  ( Show (BooleanOperators b v)
  , Backend b
  , Show v
  ) => Show (SelectArgsG b v)

instance
  ( Backend b
  , Hashable (BooleanOperators b v)
  , Hashable v
  ) => Hashable (SelectArgsG b v)

type SelectArgs b = SelectArgsG b (SQLExpression b)

traverseSelectArgs
  :: (Applicative f, Backend backend)
  => (a -> f b) -> SelectArgsG backend a -> f (SelectArgsG backend b)
traverseSelectArgs f (SelectArgs wh ordBy lmt ofst distCols) =
  SelectArgs
  <$> traverse (traverseAnnBoolExp f) wh
  -- traversing through maybe -> nonempty -> annorderbyitem
  <*> traverse (traverse (traverseAnnOrderByItem f)) ordBy
  <*> pure lmt
  <*> pure ofst
  <*> pure distCols

noSelectArgs :: SelectArgsG backend v
noSelectArgs = SelectArgs Nothing Nothing Nothing Nothing Nothing


-- Order by argument

data AnnOrderByElementG (b :: BackendType) v
  = AOCColumn !(ColumnInfo b)
  | AOCObjectRelation !(RelInfo b) !v !(AnnOrderByElementG b v)
  | AOCArrayAggregation !(RelInfo b) !v !(AnnAggregateOrderBy b)
  deriving (Generic, Functor)
deriving instance (Backend b, Eq v) => Eq (AnnOrderByElementG b v)
deriving instance (Backend b, Show v) => Show (AnnOrderByElementG b v)
instance (Backend b, Hashable v) => Hashable (AnnOrderByElementG b v)

data AnnAggregateOrderBy (b :: BackendType)
  = AAOCount
  | AAOOp !Text !(ColumnInfo b)
  deriving (Generic)
deriving instance (Backend b) => Eq (AnnAggregateOrderBy b)
deriving instance (Backend b) => Show (AnnAggregateOrderBy b)
instance (Backend b) => Hashable (AnnAggregateOrderBy b)

type AnnOrderByElement b v = AnnOrderByElementG b (AnnBoolExp b v)
type AnnOrderByItemG b v = OrderByItemG b (AnnOrderByElement b v)
type AnnOrderByItem b = AnnOrderByItemG b (SQLExpression b)
type OrderByItemExp b = OrderByItemG b (AnnOrderByElement b (SQLExpression b), (Alias b, SQLExpression b))

traverseAnnOrderByElement
  :: (Applicative f, Backend backend)
  => (a -> f b) -> AnnOrderByElement backend a -> f (AnnOrderByElement backend b)
traverseAnnOrderByElement f = \case
  AOCColumn pgColInfo -> pure $ AOCColumn pgColInfo
  AOCObjectRelation relInfo annBoolExp annObCol ->
    AOCObjectRelation relInfo
    <$> traverseAnnBoolExp f annBoolExp
    <*> traverseAnnOrderByElement f annObCol
  AOCArrayAggregation relInfo annBoolExp annAggOb ->
    AOCArrayAggregation relInfo
    <$> traverseAnnBoolExp f annBoolExp
    <*> pure annAggOb

traverseAnnOrderByItem
  :: (Applicative f, Backend backend)
  => (a -> f b) -> AnnOrderByItemG backend a -> f (AnnOrderByItemG backend b)
traverseAnnOrderByItem f =
  traverse (traverseAnnOrderByElement f)


-- Fields

type Fields a = [(FieldName, a)]

data AnnFieldG (b :: BackendType) (r :: BackendType -> Type) v
  = AFColumn !(AnnColumnField b v)
  | AFObjectRelation !(ObjectRelationSelectG b r v)
  | AFArrayRelation !(ArraySelectG b r v)
  | AFComputedField !(XComputedField b) !(ComputedFieldSelect b r v)
  | AFRemote !(r b)
  | AFNodeId !(XRelay b) !(TableName b) !(PrimaryKeyColumns b)
  | AFExpression !Text
  deriving (Generic)

deriving instance
  ( Show (BooleanOperators b v)
  , Backend b
  , Show v
  , Show (r b)
  ) => Show (AnnFieldG b r v)

deriving instance
  ( Eq (BooleanOperators b v)
  , Backend b
  , Eq v
  , Eq (r b)
  ) => Eq (AnnFieldG b r v)

instance
  ( Hashable (BooleanOperators b v)
  , Backend b
  , Hashable v
  , Hashable (r b)
  ) => Hashable (AnnFieldG b r v)

type AnnField  b = AnnFieldG  b (Const Void) (SQLExpression b)
type AnnFields b = AnnFieldsG b (Const Void) (SQLExpression b)

mkAnnColumnField
  :: ColumnInfo backend
  -> Maybe (AnnColumnCaseBoolExp backend v)
  -> Maybe (ColumnOp backend)
  -> AnnFieldG backend r v
mkAnnColumnField ci caseBoolExp colOpM =
  AFColumn (AnnColumnField ci False colOpM caseBoolExp)

mkAnnColumnFieldAsText
  :: ColumnInfo backend
  -> AnnFieldG backend r v
mkAnnColumnFieldAsText ci =
  AFColumn (AnnColumnField ci True Nothing Nothing)

traverseAnnFields
  :: (Applicative f, Backend backend)
  => (a -> f b) -> AnnFieldsG backend r a -> f (AnnFieldsG backend r b)
traverseAnnFields f = traverse (traverse (traverseAnnField f))

traverseAnnField
  :: (Applicative f, Backend backend)
  => (a -> f b)
  -> AnnFieldG backend r a
  -> f (AnnFieldG backend r b)
traverseAnnField f = \case
  AFColumn colFld       -> AFColumn          <$> traverseAnnColumnField f colFld
  AFObjectRelation sel  -> AFObjectRelation  <$> traverse (traverseAnnObjectSelect f) sel
  AFArrayRelation sel   -> AFArrayRelation   <$> traverseArraySelect f sel
  AFComputedField x sel -> AFComputedField x <$> traverseComputedFieldSelect f sel
  AFRemote s            -> pure $ AFRemote s
  AFNodeId x qt pKeys   -> pure $ AFNodeId x qt pKeys
  AFExpression t        -> pure $ AFExpression t


-- Aggregation fields

data TableAggregateFieldG (b :: BackendType) (r :: BackendType -> Type) v
  = TAFAgg !(AggregateFields b)
  | TAFNodes (XNodesAgg b) !(AnnFieldsG b r v)
  | TAFExp !Text
  deriving (Generic)

deriving instance
  ( Show (BooleanOperators b v)
  , Backend b
  , Show v
  , Show (r b)
  ) => Show (TableAggregateFieldG b r v)

deriving instance
  ( Eq (BooleanOperators b v)
  , Backend b
  , Eq v
  , Eq (r b)
  ) => Eq (TableAggregateFieldG b r v)

instance
  ( Hashable (BooleanOperators b v)
  , Backend b
  , Hashable v
  , Hashable (r b)
  ) => Hashable (TableAggregateFieldG b r v)

data AggregateField (b :: BackendType)
  = AFCount !(CountType b)
  | AFOp !(AggregateOp b)
  | AFExp !Text
  deriving (Generic)
deriving instance (Backend b) => Show (AggregateField b)
deriving instance (Backend b) => Eq (AggregateField b)
instance (Backend b) => Hashable (AggregateField b)

data AggregateOp (b :: BackendType)
  = AggregateOp
  { _aoOp     :: !Text
  , _aoFields :: !(ColumnFields b)
  } deriving (Generic, Show, Eq)
instance (Backend b) => Hashable (AggregateOp b)

data ColFld (b :: BackendType)
  = CFCol !(Column b) !(ColumnType b)
  | CFExp !Text
  deriving (Generic)
deriving instance (Backend b) => Show (ColFld b)
deriving instance (Backend b) => Eq (ColFld b)
instance (Backend b) => Hashable (ColFld b)

type TableAggregateField   b     = TableAggregateFieldG  b (Const Void) (SQLExpression b)
type TableAggregateFields  b     = TableAggregateFieldsG b (Const Void) (SQLExpression b)
type TableAggregateFieldsG b r v = Fields (TableAggregateFieldG b r v)

type ColumnFields b    = Fields (ColFld b)
type AggregateFields b = Fields (AggregateField b)
type AnnFieldsG b r v  = Fields (AnnFieldG b r v)

traverseTableAggregateField
  :: (Applicative f, Backend backend)
  => (a -> f b)
  -> TableAggregateFieldG backend r a
  -> f (TableAggregateFieldG backend r b)
traverseTableAggregateField f = \case
  TAFAgg aggFlds     -> pure $ TAFAgg aggFlds
  TAFNodes x annFlds -> TAFNodes x <$> traverseAnnFields f annFlds
  TAFExp t           -> pure $ TAFExp t


-- Relay fields

data ConnectionField (b :: BackendType) (r :: BackendType -> Type) v
  = ConnectionTypename !Text
  | ConnectionPageInfo !PageInfoFields
  | ConnectionEdges !(EdgeFields b r v)
  deriving (Generic)

deriving instance
  ( Show (BooleanOperators b v)
  , Backend b
  , Show v
  , Show (r b)
  ) => Show (ConnectionField b r v)

deriving instance
  ( Eq (BooleanOperators b v)
  , Backend b
  , Eq v
  , Eq (r b)
  ) => Eq (ConnectionField b r v)

instance
  ( Hashable (BooleanOperators b v)
  , Backend b
  , Hashable v
  , Hashable (r b)
  ) => Hashable (ConnectionField b r v)

data PageInfoField
  = PageInfoTypename !Text
  | PageInfoHasNextPage
  | PageInfoHasPreviousPage
  | PageInfoStartCursor
  | PageInfoEndCursor
  deriving (Show, Eq, Generic)

instance Hashable PageInfoField

data EdgeField (b :: BackendType) (r :: BackendType -> Type) v
  = EdgeTypename !Text
  | EdgeCursor
  | EdgeNode !(AnnFieldsG b r v)
  deriving (Generic)

deriving instance
  ( Show (BooleanOperators b v)
  , Backend b
  , Show v
  , Show (r b)
  ) => Show (EdgeField b r v)

deriving instance
  ( Eq (BooleanOperators b v)
  , Backend b
  , Eq v
  , Eq (r b)
  ) => Eq (EdgeField b r v)

instance
  ( Hashable (BooleanOperators b v)
  , Backend b
  , Hashable v
  , Hashable (r b)
  ) => Hashable (EdgeField b r v)

type ConnectionFields b r v = Fields (ConnectionField b r v)

type PageInfoFields   = Fields PageInfoField
type EdgeFields b r v = Fields (EdgeField b r v)

traverseConnectionField
  :: (Applicative f, Backend backend)
  => (a -> f b)
  -> ConnectionField backend r a
  -> f (ConnectionField backend r b)
traverseConnectionField f = \case
  ConnectionTypename t -> pure $ ConnectionTypename t
  ConnectionPageInfo fields -> pure $ ConnectionPageInfo fields
  ConnectionEdges fields ->
    ConnectionEdges <$> traverse (traverse (traverseEdgeField f)) fields

data ArgumentExp (b :: BackendType) a
  = AETableRow !(Maybe (Identifier b)) -- ^ table row accessor
  | AESession !a -- ^ JSON/JSONB hasura session variable object
  | AEInput !a
  deriving (Functor, Foldable, Traversable, Generic)
deriving instance (Backend b, Show a) => Show (ArgumentExp b a)
deriving instance (Backend b, Eq   a) => Eq   (ArgumentExp b a)
instance (Backend b, Hashable v) => Hashable (ArgumentExp b v)

traverseEdgeField
  :: (Applicative f, Backend backend)
  => (a -> f b)
  -> EdgeField backend r a
  -> f (EdgeField backend r b)
traverseEdgeField f = \case
  EdgeTypename t  -> pure $ EdgeTypename t
  EdgeCursor      -> pure EdgeCursor
  EdgeNode fields -> EdgeNode <$> traverseAnnFields f fields


-- Column

data AnnColumnField (b :: BackendType) v
  = AnnColumnField
  -- TODO: We don't ever need 'ColumnInfo b', so this should be changed to
  -- (Column b, ScalarType b). Constructing and adding an AnnColumnField
  -- manually currently is not very easy and uses a bunch of unsafe functions
  { _acfInfo               :: !(ColumnInfo b)
  , _acfAsText             :: !Bool
  -- ^ If this field is 'True', columns are explicitly casted to @text@ when fetched, which avoids
  -- an issue that occurs because we don’t currently have proper support for array types. See
  -- https://github.com/hasura/graphql-engine/pull/3198 for more details.
  , _acfOp                 :: !(Maybe (ColumnOp b))
  , _acfCaseBoolExpression :: !(Maybe (AnnColumnCaseBoolExp b v))
  -- ^ This type is used to determine if whether the column
  -- should be nullified. When the value is `Nothing`, the column value
  -- will be outputted as computed and when the value is `Just c`, the
  -- column will be outputted when `c` evaluates to `true` and `null`
  -- when `c` evaluates to `false`.
  } deriving (Generic)

deriving instance
  ( Show (BooleanOperators b v)
  , Backend b
  , Show v
  ) => Show (AnnColumnField b v)

deriving instance
  ( Eq (BooleanOperators b v)
  , Backend b
  , Eq v
  ) => Eq (AnnColumnField b v)

instance
  ( Hashable (BooleanOperators b v)
  , Backend b
  , Hashable v
  ) => Hashable (AnnColumnField b v)

data ColumnOp (b :: BackendType)
  = ColumnOp
  { _colOp  :: SQLOperator b
  , _colExp :: SQLExpression b
  } deriving (Generic)

deriving instance Backend b => Show (ColumnOp b)
deriving instance Backend b => Eq   (ColumnOp b)
instance Backend b => Hashable (ColumnOp b)

traverseAnnColumnField
  :: (Applicative f, Backend backend)
  => (a -> f b)
  -> AnnColumnField backend a
  -> f (AnnColumnField backend b)
traverseAnnColumnField f (AnnColumnField info asText op caseBoolExpMaybe) =
  AnnColumnField info asText op
  <$> traverse (traverseAnnColumnCaseBoolExp f) caseBoolExpMaybe


-- Computed field

data ComputedFieldScalarSelect (b :: BackendType) v
  = ComputedFieldScalarSelect
  { _cfssFunction  :: !(FunctionName b)
  , _cfssArguments :: !(FunctionArgsExpTableRow b v)
  , _cfssType      :: !(ScalarType b)
  , _cfssColumnOp  :: !(Maybe (ColumnOp b))
  } deriving (Functor, Foldable, Traversable, Generic)
deriving instance (Backend b, Show v) => Show (ComputedFieldScalarSelect b v)
deriving instance (Backend b, Eq   v) => Eq   (ComputedFieldScalarSelect b v)
instance (Backend b, Hashable   v) => Hashable   (ComputedFieldScalarSelect b v)

data ComputedFieldSelect (b :: BackendType) (r :: BackendType -> Type) v
  = CFSScalar
      !(ComputedFieldScalarSelect b v)
      -- ^ Type containing info about the computed field
      !(Maybe (AnnColumnCaseBoolExp b v))
      -- ^ This type is used to determine if whether the scalar
      -- computed field should be nullified. When the value is `Nothing`,
      -- the scalar computed value will be outputted as computed and when the
      -- value is `Just c`, the scalar computed field will be outputted when
      -- `c` evaluates to `true` and `null` when `c` evaluates to `false`
  | CFSTable !JsonAggSelect !(AnnSimpleSelG b r v)
  deriving (Generic)

deriving instance
  ( Show (BooleanOperators b v)
  , Backend b
  , Show v
  , Show (r b)
  ) => Show (ComputedFieldSelect b r v)

deriving instance
  ( Eq (BooleanOperators b v)
  , Backend b
  , Eq v
  , Eq (r b)
  ) => Eq (ComputedFieldSelect b r v)

instance
  ( Hashable (BooleanOperators b v)
  , Backend b
  , Hashable v
  , Hashable (r b)
  ) => Hashable (ComputedFieldSelect b r v)

traverseComputedFieldSelect
  :: (Applicative f, Backend backend)
  => (v -> f w)
  -> ComputedFieldSelect backend r v
  -> f (ComputedFieldSelect backend r w)
traverseComputedFieldSelect fv = \case
  CFSScalar scalarSel caseBoolExpMaybe ->
    CFSScalar <$> traverse fv scalarSel <*> traverse (traverseAnnColumnCaseBoolExp fv) caseBoolExpMaybe
  CFSTable b tableSel -> CFSTable b <$> traverseAnnSimpleSelect fv tableSel


-- Local relationship

data AnnRelationSelectG (b :: BackendType) a
  = AnnRelationSelectG
  { aarRelationshipName :: !RelName -- Relationship name
  , aarColumnMapping    :: !(HashMap (Column b) (Column b)) -- Column of left table to join with
  , aarAnnSelect        :: !a -- Current table. Almost ~ to SQL Select
  } deriving  (Functor, Foldable, Traversable, Generic)

deriving instance
  ( Backend b
  , Show v
  ) => Show (AnnRelationSelectG b v)

deriving instance
  ( Backend b
  , Eq v
  ) => Eq (AnnRelationSelectG b v)

instance
  ( Backend b
  , Hashable v
  ) => Hashable (AnnRelationSelectG b v)

type ArrayRelationSelectG  b r v = AnnRelationSelectG b (AnnSimpleSelG       b r v)
type ArrayAggregateSelectG b r v = AnnRelationSelectG b (AnnAggregateSelectG b r v)
type ArrayConnectionSelect b r v = AnnRelationSelectG b (ConnectionSelect    b r v)
type ArrayAggregateSelect  b     = ArrayAggregateSelectG b (Const Void) (SQLExpression b)

data AnnObjectSelectG (b :: BackendType) (r :: BackendType -> Type) v
  = AnnObjectSelectG
  { _aosFields      :: !(AnnFieldsG b r v)
  , _aosTableFrom   :: !(TableName b)
  , _aosTableFilter :: !(AnnBoolExp b v)
  } deriving (Generic)

deriving instance
  ( Show (BooleanOperators b v)
  , Backend b
  , Show v
  , Show (r b)
  ) => Show (AnnObjectSelectG b r v)

deriving instance
  ( Eq (BooleanOperators b v)
  , Backend b
  , Eq v
  , Eq (r b)
  ) => Eq (AnnObjectSelectG b r v)

instance
  ( Hashable (BooleanOperators b v)
  , Backend b
  , Hashable v
  , Hashable (r b)
  ) => Hashable (AnnObjectSelectG b r v)

type AnnObjectSelect b r = AnnObjectSelectG b r (SQLExpression b)

traverseAnnObjectSelect
  :: (Applicative f, Backend backend)
  => (a -> f b)
  -> AnnObjectSelectG backend r a
  -> f (AnnObjectSelectG backend r b)
traverseAnnObjectSelect f (AnnObjectSelectG fields fromTable permissionFilter) =
  AnnObjectSelectG
  <$> traverseAnnFields f fields
  <*> pure fromTable
  <*> traverseAnnBoolExp f permissionFilter

type ObjectRelationSelectG b r v = AnnRelationSelectG b (AnnObjectSelectG b r v)
type ObjectRelationSelect  b     = ObjectRelationSelectG b (Const Void) (SQLExpression b)


data ArraySelectG (b :: BackendType) (r :: BackendType -> Type) v
  = ASSimple     !(ArrayRelationSelectG  b r v)
  | ASAggregate  !(ArrayAggregateSelectG b r v)
  | ASConnection !(ArrayConnectionSelect b r v)
  deriving (Generic)

deriving instance
  ( Show (BooleanOperators b v)
  , Backend b
  , Show v
  , Show (r b)
  ) => Show (ArraySelectG b r v)

deriving instance
  ( Eq (BooleanOperators b v)
  , Backend b
  , Eq v
  , Eq (r b)
  ) => Eq (ArraySelectG b r v)

instance
  ( Hashable (BooleanOperators b v)
  , Backend b
  , Hashable v
  , Hashable (r b)
  ) => Hashable (ArraySelectG b r v)

type ArraySelect b = ArraySelectG b (Const Void) (SQLExpression b)
type ArraySelectFieldsG b r v = Fields (ArraySelectG b r v)

traverseArraySelect
  :: (Applicative f, Backend backend)
  => (a -> f b)
  -> ArraySelectG backend r a
  -> f (ArraySelectG backend r b)
traverseArraySelect f = \case
  ASSimple arrRel ->
    ASSimple <$> traverse (traverseAnnSimpleSelect f) arrRel
  ASAggregate arrRelAgg ->
    ASAggregate <$> traverse (traverseAnnAggregateSelect f) arrRelAgg
  ASConnection relConnection ->
    ASConnection <$> traverse (traverseConnectionSelect f) relConnection


-- Remote schema relationships

data RemoteFieldArgument
  = RemoteFieldArgument
  { _rfaArgument :: !G.Name
  , _rfaValue    :: !(InputValue RemoteSchemaVariable)
  } deriving (Eq,Show)

data RemoteSchemaSelect (b :: BackendType)
  = RemoteSchemaSelect
  { _rselArgs          :: ![RemoteFieldArgument]
  , _rselSelection     :: !(G.SelectionSet G.NoFragments RemoteSchemaVariable)
  , _rselHasuraColumns :: !(HashSet (ColumnInfo b))
  , _rselFieldCall     :: !(NonEmpty FieldCall)
  , _rselRemoteSchema  :: !RemoteSchemaInfo
  }

-- Remote source relationships

data SourceRelationshipSelection
    (b :: BackendType)
    (r :: BackendType -> Type)
    (vf :: BackendType -> Type)
  = SourceRelationshipObject !(AnnObjectSelectG b r (vf b))
  | SourceRelationshipArray !(AnnSimpleSelG b r (vf b))
  | SourceRelationshipArrayAggregate !(AnnAggregateSelectG b r (vf b))
  deriving (Generic)

deriving instance
  ( Show (BooleanOperators b (v b))
  , Backend b
  , Show (v b)
  , Show (r b)
  ) => Show (SourceRelationshipSelection b r v)

deriving instance
  ( Eq (BooleanOperators b (v b))
  , Backend b
  , Eq (v b)
  , Eq (r b)
  ) => Eq (SourceRelationshipSelection b r v)

instance
  ( Hashable (BooleanOperators b (v b))
  , Backend b
  , Hashable (v b)
  , Hashable (r b)
  ) => Hashable (SourceRelationshipSelection b r v)

data RemoteSourceSelect
    (src :: BackendType)
    (vf :: BackendType -> Type)
    (tgt :: BackendType)
  = RemoteSourceSelect
    { _rssSourceName   :: !SourceName
    , _rssSourceConfig :: !(SourceConfig tgt)
    , _rssSelection    :: !(SourceRelationshipSelection tgt (RemoteSelect vf) vf)
    , _rssJoinMapping  :: !(HM.HashMap FieldName (ColumnInfo src, ScalarType tgt, Column tgt))
    -- ^ Additional information about the source's join columns:
    -- (ColumnInfo src) so that we can add the join column to the AST
    -- (ScalarType tgt) so that the remote can interpret the join values coming
    -- from src
    -- (Column tgt) so that an appropriate join condition / in clause can be built
    -- by the remote
    }

data RemoteSelect
    (vf :: BackendType -> Type)
    (src :: BackendType)
  = RemoteSelectRemoteSchema !(RemoteSchemaSelect src)
  | RemoteSelectSource !(AB.AnyBackend (RemoteSourceSelect src vf))

-- Permissions

data TablePermG (b :: BackendType) v
  = TablePerm
  { _tpFilter :: !(AnnBoolExp b v)
  , _tpLimit  :: !(Maybe Int)
  } deriving (Generic)

deriving instance
  ( Show (BooleanOperators b v)
  , Backend b
  , Show v
  ) => Show (TablePermG b v)

deriving instance
  ( Eq (BooleanOperators b v)
  , Backend b
  , Eq v
  ) => Eq (TablePermG b v)

instance
  ( Backend b
  , Hashable (BooleanOperators b v)
  , Hashable (ColumnInfo b)
  , Hashable v
  ) => Hashable (TablePermG b v)

type TablePerm b = TablePermG b (SQLExpression b)

noTablePermissions :: TablePermG backend v
noTablePermissions =
  TablePerm annBoolExpTrue Nothing

traverseTablePerm
  :: (Applicative f, Backend backend)
  => (a -> f b)
  -> TablePermG backend a
  -> f (TablePermG backend b)
traverseTablePerm f (TablePerm boolExp limit) =
  TablePerm
  <$> traverseAnnBoolExp f boolExp
  <*> pure limit


-- Function arguments

data FunctionArgsExpG a
  = FunctionArgsExp
  { _faePositional :: ![a]
  , _faeNamed      :: !(HM.HashMap Text a)
  } deriving (Show, Eq, Functor, Foldable, Traversable, Generic)
instance (Hashable a) => Hashable (FunctionArgsExpG a)

type FunctionArgsExpTableRow b v = FunctionArgsExpG (ArgumentExp b v)
type FunctionArgExp          b   = FunctionArgsExpG (SQLExpression b)

emptyFunctionArgsExp :: FunctionArgsExpG a
emptyFunctionArgsExp = FunctionArgsExp [] HM.empty

-- | If argument positional index is less than or equal to length of
-- 'positional' arguments then insert the value in 'positional' arguments else
-- insert the value with argument name in 'named' arguments
insertFunctionArg
  :: FunctionArgName
  -> Int
  -> a
  -> FunctionArgsExpG a
  -> FunctionArgsExpG a
insertFunctionArg argName idx value (FunctionArgsExp positional named) =
  if (idx + 1) <= length positional then
    FunctionArgsExp (insertAt idx value positional) named
  else FunctionArgsExp positional $
    HM.insert (getFuncArgNameTxt argName) value named
  where
    insertAt i a = toList . Seq.insertAt i a . Seq.fromList


-- Lenses

$(makeLenses ''AnnSelectG)
$(makePrisms ''AnnFieldG)
$(makePrisms ''AnnOrderByElementG)
