{-# LANGUAGE UndecidableInstances #-}

module Hasura.RQL.IR.Select where

import           Hasura.Prelude

import qualified Data.HashMap.Strict                 as HM
import qualified Data.List.NonEmpty                  as NE
import qualified Data.Sequence                       as Seq
import qualified Language.GraphQL.Draft.Syntax       as G

import           Control.Lens.TH                     (makeLenses, makePrisms)

import           Hasura.GraphQL.Parser.Schema
import           Hasura.RQL.IR.BoolExp
import           Hasura.RQL.IR.OrderBy
import           Hasura.RQL.Types.Column
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.Function
import           Hasura.RQL.Types.RemoteRelationship
import           Hasura.RQL.Types.RemoteSchema
import           Hasura.SQL.Backend


data JsonAggSelect
  = JASMultipleRows
  | JASSingleObject
  deriving (Show, Eq, Generic)
instance Hashable JsonAggSelect

data AnnAggregateOrderBy (b :: BackendType)
  = AAOCount
  | AAOOp !Text !(ColumnInfo b)
  deriving (Generic)
deriving instance (Backend b, Eq (ColumnInfo b)) => Eq (AnnAggregateOrderBy b)
instance (Backend b, Hashable (ColumnInfo b)) => Hashable (AnnAggregateOrderBy b)

data AnnOrderByElementG (b :: BackendType) v
  = AOCColumn !(ColumnInfo b)
  | AOCObjectRelation !(RelInfo b) !v !(AnnOrderByElementG b v)
  | AOCArrayAggregation !(RelInfo b) !v !(AnnAggregateOrderBy b)
  deriving (Generic, Functor)
deriving instance (Backend b, Eq (ColumnInfo b), Eq v) => Eq (AnnOrderByElementG b v)
instance (Backend b, Hashable (ColumnInfo b), Hashable v) => Hashable (AnnOrderByElementG b v)

type AnnOrderByElement b v = AnnOrderByElementG b (AnnBoolExp b v)

traverseAnnOrderByElement
  :: (Applicative f)
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

type AnnOrderByItemG b v = OrderByItemG b (AnnOrderByElement b v)

traverseAnnOrderByItem
  :: (Applicative f)
  => (a -> f b) -> AnnOrderByItemG backend a -> f (AnnOrderByItemG backend b)
traverseAnnOrderByItem f =
  traverse (traverseAnnOrderByElement f)

type AnnOrderByItem b = AnnOrderByItemG b (SQLExpression b)

type OrderByItemExp b =
  OrderByItemG b (AnnOrderByElement b (SQLExpression b), (Alias b, (SQLExpression b)))

data AnnRelationSelectG (b :: BackendType) a
  = AnnRelationSelectG
  { aarRelationshipName :: !RelName -- Relationship name
  , aarColumnMapping    :: !(HashMap (Column b) (Column b)) -- Column of left table to join with
  , aarAnnSelect        :: !a -- Current table. Almost ~ to SQL Select
  } deriving (Functor, Foldable, Traversable)

type ArrayRelationSelectG b v = AnnRelationSelectG b (AnnSimpleSelG b v)
type ArrayAggregateSelectG b v = AnnRelationSelectG b (AnnAggregateSelectG b v)
type ArrayConnectionSelect b v = AnnRelationSelectG b (ConnectionSelect b v)
type ArrayAggregateSelect b = ArrayAggregateSelectG b (SQLExpression b)

data AnnObjectSelectG (b :: BackendType) v
  = AnnObjectSelectG
  { _aosFields      :: !(AnnFieldsG b v)
  , _aosTableFrom   :: !(TableName b)
  , _aosTableFilter :: !(AnnBoolExp b v)
  }

type AnnObjectSelect b = AnnObjectSelectG b (SQLExpression b)

traverseAnnObjectSelect
  :: (Applicative f)
  => (a -> f b)
  -> AnnObjectSelectG backend a -> f (AnnObjectSelectG backend b)
traverseAnnObjectSelect f (AnnObjectSelectG fields fromTable permissionFilter) =
  AnnObjectSelectG
  <$> traverseAnnFields f fields
  <*> pure fromTable
  <*> traverseAnnBoolExp f permissionFilter

type ObjectRelationSelectG b v = AnnRelationSelectG b (AnnObjectSelectG b v)
type ObjectRelationSelect b = ObjectRelationSelectG b (SQLExpression b)

data ComputedFieldScalarSelect (b :: BackendType) v
  = ComputedFieldScalarSelect
  { _cfssFunction  :: !(FunctionName b)
  , _cfssArguments :: !(FunctionArgsExpTableRow b v)
  , _cfssType      :: !(ScalarType b)
  , _cfssColumnOp  :: !(Maybe (ColumnOp b))
  } deriving (Functor, Foldable, Traversable)
deriving instance (Backend b, Show v) => Show (ComputedFieldScalarSelect b v)
deriving instance (Backend b, Eq   v) => Eq   (ComputedFieldScalarSelect b v)

data ComputedFieldSelect (b :: BackendType) v
  = CFSScalar !(ComputedFieldScalarSelect b v)
  | CFSTable !JsonAggSelect !(AnnSimpleSelG b v)

traverseComputedFieldSelect
  :: (Applicative f)
  => (v -> f w)
  -> ComputedFieldSelect backend v -> f (ComputedFieldSelect backend w)
traverseComputedFieldSelect fv = \case
  CFSScalar scalarSel -> CFSScalar <$> traverse fv scalarSel
  CFSTable b tableSel -> CFSTable b <$> traverseAnnSimpleSelect fv tableSel

type Fields a = [(FieldName, a)]

data ArraySelectG (b :: BackendType) v
  = ASSimple !(ArrayRelationSelectG b v)
  | ASAggregate !(ArrayAggregateSelectG b v)
  | ASConnection !(ArrayConnectionSelect b v)

traverseArraySelect
  :: (Applicative f)
  => (a -> f b)
  -> ArraySelectG backend a
  -> f (ArraySelectG backend b)
traverseArraySelect f = \case
  ASSimple arrRel ->
    ASSimple <$> traverse (traverseAnnSimpleSelect f) arrRel
  ASAggregate arrRelAgg ->
    ASAggregate <$> traverse (traverseAnnAggregateSelect f) arrRelAgg
  ASConnection relConnection ->
    ASConnection <$> traverse (traverseConnectionSelect f) relConnection

type ArraySelect b = ArraySelectG b (SQLExpression b)

type ArraySelectFieldsG b v = Fields (ArraySelectG b v)

data ColumnOp (b :: BackendType)
  = ColumnOp
  { _colOp  :: (SQLOperator b)
  , _colExp :: (SQLExpression b)
  }
deriving instance Backend b => Show (ColumnOp b)
deriving instance Backend b => Eq   (ColumnOp b)

data AnnColumnField (b :: BackendType)
  = AnnColumnField
  { _acfInfo   :: !(ColumnInfo b)
  , _acfAsText :: !Bool
  -- ^ If this field is 'True', columns are explicitly casted to @text@ when fetched, which avoids
  -- an issue that occurs because we don’t currently have proper support for array types. See
  -- https://github.com/hasura/graphql-engine/pull/3198 for more details.
  , _acfOp     :: !(Maybe (ColumnOp b))
  }

data RemoteFieldArgument
  = RemoteFieldArgument
  { _rfaArgument :: !G.Name
  , _rfaValue    :: !(InputValue Variable)
  } deriving (Eq,Show)

data RemoteSelect (b :: BackendType)
  = RemoteSelect
  { _rselArgs          :: ![RemoteFieldArgument]
  , _rselSelection     :: !(G.SelectionSet G.NoFragments Variable)
  , _rselHasuraColumns :: !(HashSet (ColumnInfo b))
  , _rselFieldCall     :: !(NonEmpty FieldCall)
  , _rselRemoteSchema  :: !RemoteSchemaInfo
  }

data AnnFieldG (b :: BackendType) v
  = AFColumn !(AnnColumnField b)
  | AFObjectRelation !(ObjectRelationSelectG b v)
  | AFArrayRelation !(ArraySelectG b v)
  | AFComputedField !(ComputedFieldSelect b v)
  | AFRemote !(RemoteSelect b)
  | AFNodeId !(TableName b) !(PrimaryKeyColumns b)
  | AFExpression !Text

mkAnnColumnField :: ColumnInfo backend -> Maybe (ColumnOp backend) -> AnnFieldG backend v
mkAnnColumnField ci colOpM =
  AFColumn $ AnnColumnField ci False colOpM

mkAnnColumnFieldAsText :: ColumnInfo backend -> AnnFieldG backend v
mkAnnColumnFieldAsText ci =
  AFColumn $ AnnColumnField ci True Nothing

traverseAnnField
  :: (Applicative f)
  => (a -> f b) -> AnnFieldG backend a -> f (AnnFieldG backend b)
traverseAnnField f = \case
  AFColumn colFld      -> pure $ AFColumn colFld
  AFObjectRelation sel -> AFObjectRelation <$> traverse (traverseAnnObjectSelect f) sel
  AFArrayRelation sel  -> AFArrayRelation <$> traverseArraySelect f sel
  AFComputedField sel  -> AFComputedField <$> traverseComputedFieldSelect f sel
  AFRemote s           -> pure $ AFRemote s
  AFNodeId qt pKeys    -> pure $ AFNodeId qt pKeys
  AFExpression t       -> AFExpression <$> pure t

type AnnField b = AnnFieldG b (SQLExpression b)

data SelectArgsG (b :: BackendType) v
  = SelectArgs
  { _saWhere    :: !(Maybe (AnnBoolExp b v))
  , _saOrderBy  :: !(Maybe (NE.NonEmpty (AnnOrderByItemG b v)))
  , _saLimit    :: !(Maybe Int)
  , _saOffset   :: !(Maybe (SQLExpression b))
  , _saDistinct :: !(Maybe (NE.NonEmpty (Column b)))
  } deriving (Generic)
deriving instance (Backend b, Eq (ColumnInfo b), Eq v) => Eq (SelectArgsG b v)
instance (Backend b, Hashable (ColumnInfo b), Hashable v) => Hashable (SelectArgsG b v)

traverseSelectArgs
  :: (Applicative f)
  => (a -> f b) -> SelectArgsG backend a -> f (SelectArgsG backend b)
traverseSelectArgs f (SelectArgs wh ordBy lmt ofst distCols) =
  SelectArgs
  <$> traverse (traverseAnnBoolExp f) wh
  -- traversing through maybe -> nonempty -> annorderbyitem
  <*> traverse (traverse (traverseAnnOrderByItem f)) ordBy
  <*> pure lmt
  <*> pure ofst
  <*> pure distCols

type SelectArgs b = SelectArgsG b (SQLExpression b)

noSelectArgs :: SelectArgsG backend v
noSelectArgs = SelectArgs Nothing Nothing Nothing Nothing Nothing

data ColFld (b :: BackendType)
  = CFCol !(Column b)
  | CFExp !Text
{-
deriving instance Eq (Column b) => Eq (ColFld b)
deriving instance Show (Column b) => Show (ColFld b)
-}

type ColumnFields b = Fields (ColFld b)

data AggregateOp (b :: BackendType)
  = AggregateOp
  { _aoOp     :: !Text
  , _aoFields :: !(ColumnFields b)
  }

data AggregateField (b :: BackendType)
  = AFCount !(CountType b)
  | AFOp !(AggregateOp b)
  | AFExp !Text

type AggregateFields b = Fields (AggregateField b)
type AnnFieldsG b v = Fields (AnnFieldG b v)

traverseAnnFields
  :: (Applicative f)
  => (a -> f b) -> AnnFieldsG backend a -> f (AnnFieldsG backend b)
traverseAnnFields f = traverse (traverse (traverseAnnField f))

type AnnFields b = AnnFieldsG b (SQLExpression b)

data TableAggregateFieldG (b :: BackendType) v
  = TAFAgg !(AggregateFields b)
  | TAFNodes !(AnnFieldsG b v)
  | TAFExp !Text

data PageInfoField
  = PageInfoTypename !Text
  | PageInfoHasNextPage
  | PageInfoHasPreviousPage
  | PageInfoStartCursor
  | PageInfoEndCursor
  deriving (Show, Eq)
type PageInfoFields = Fields PageInfoField

data EdgeField (b :: BackendType) v
  = EdgeTypename !Text
  | EdgeCursor
  | EdgeNode !(AnnFieldsG b v)
type EdgeFields b v = Fields (EdgeField b v)

traverseEdgeField
  :: (Applicative f)
  => (a -> f b) -> EdgeField backend a -> f (EdgeField backend b)
traverseEdgeField f = \case
  EdgeTypename t  -> pure $ EdgeTypename t
  EdgeCursor      -> pure EdgeCursor
  EdgeNode fields -> EdgeNode <$> traverseAnnFields f fields

data ConnectionField (b :: BackendType) v
  = ConnectionTypename !Text
  | ConnectionPageInfo !PageInfoFields
  | ConnectionEdges !(EdgeFields b v)
type ConnectionFields b v = Fields (ConnectionField b v)

traverseConnectionField
  :: (Applicative f)
  => (a -> f b) -> ConnectionField backend a -> f (ConnectionField backend b)
traverseConnectionField f = \case
  ConnectionTypename t -> pure $ ConnectionTypename t
  ConnectionPageInfo fields -> pure $ ConnectionPageInfo fields
  ConnectionEdges fields ->
    ConnectionEdges <$> traverse (traverse (traverseEdgeField f)) fields

traverseTableAggregateField
  :: (Applicative f)
  => (a -> f b) -> TableAggregateFieldG backend a -> f (TableAggregateFieldG backend b)
traverseTableAggregateField f = \case
  TAFAgg aggFlds   -> pure $ TAFAgg aggFlds
  TAFNodes annFlds -> TAFNodes <$> traverseAnnFields f annFlds
  TAFExp t         -> pure $ TAFExp t

type TableAggregateField b = TableAggregateFieldG b (SQLExpression b)
type TableAggregateFieldsG b v = Fields (TableAggregateFieldG b v)
type TableAggregateFields b = TableAggregateFieldsG b (SQLExpression b)

data ArgumentExp (b :: BackendType) a
  = AETableRow !(Maybe (Identifier b)) -- ^ table row accessor
  | AESession !a -- ^ JSON/JSONB hasura session variable object
  | AEInput !a
  deriving (Functor, Foldable, Traversable, Generic)
deriving instance (Backend b, Show a) => Show (ArgumentExp b a)
deriving instance (Backend b, Eq   a) => Eq   (ArgumentExp b a)
instance (Backend b, Hashable v) => Hashable (ArgumentExp b v)

type FunctionArgsExpTableRow b v = FunctionArgsExpG (ArgumentExp b v)

data SelectFromG (b :: BackendType) v
  = FromTable !(TableName b)
  | FromIdentifier !(Identifier b)
  | FromFunction !(FunctionName b)
                 !(FunctionArgsExpTableRow b v)
                 -- a definition list
                 !(Maybe [(Column b, ScalarType b)])
  deriving (Functor, Foldable, Traversable, Generic)
instance (Backend b, Hashable v) => Hashable (SelectFromG b v)

type SelectFrom b = SelectFromG b (SQLExpression b)

data TablePermG (b :: BackendType) v
  = TablePerm
  { _tpFilter :: !(AnnBoolExp b v)
  , _tpLimit  :: !(Maybe Int)
  } deriving (Generic)
instance (Backend b, Hashable (ColumnInfo b), Hashable v) => Hashable (TablePermG b v)

traverseTablePerm
  :: (Applicative f)
  => (a -> f b)
  -> TablePermG backend a
  -> f (TablePermG backend b)
traverseTablePerm f (TablePerm boolExp limit) =
  TablePerm
  <$> traverseAnnBoolExp f boolExp
  <*> pure limit

noTablePermissions :: TablePermG backend v
noTablePermissions =
  TablePerm annBoolExpTrue Nothing

type TablePerm b = TablePermG b (SQLExpression b)

data AnnSelectG (b :: BackendType) a v
  = AnnSelectG
  { _asnFields   :: !a
  , _asnFrom     :: !(SelectFromG b v)
  , _asnPerm     :: !(TablePermG b v)
  , _asnArgs     :: !(SelectArgsG b v)
  , _asnStrfyNum :: !Bool
  }

traverseAnnSimpleSelect
  :: (Applicative f)
  => (a -> f b)
  -> AnnSimpleSelG backend a -> f (AnnSimpleSelG backend b)
traverseAnnSimpleSelect f = traverseAnnSelect (traverseAnnFields f) f

traverseAnnAggregateSelect
  :: (Applicative f)
  => (a -> f b)
  -> AnnAggregateSelectG backend a -> f (AnnAggregateSelectG backend b)
traverseAnnAggregateSelect f =
  traverseAnnSelect (traverse (traverse (traverseTableAggregateField f))) f

traverseAnnSelect
  :: (Applicative f)
  => (a -> f b) -> (v -> f w)
  -> AnnSelectG backend a v -> f (AnnSelectG backend b w)
traverseAnnSelect f1 f2 (AnnSelectG flds tabFrom perm args strfyNum) =
  AnnSelectG
  <$> f1 flds
  <*> traverse f2 tabFrom
  <*> traverseTablePerm f2 perm
  <*> traverseSelectArgs f2 args
  <*> pure strfyNum

type AnnSimpleSelG b v = AnnSelectG    b (AnnFieldsG b v) v
type AnnSimpleSel  b   = AnnSimpleSelG b (SQLExpression b)

type AnnAggregateSelectG b v = AnnSelectG b (TableAggregateFieldsG b v) v
type AnnAggregateSelect b = AnnAggregateSelectG b (SQLExpression b)

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

data ConnectionSplit (b :: BackendType) v
  = ConnectionSplit
  { _csKind    :: !ConnectionSplitKind
  , _csValue   :: !v
  , _csOrderBy :: !(OrderByItemG b (AnnOrderByElementG b ()))
  } deriving (Functor, Generic, Foldable, Traversable)
instance (Backend b, Hashable (ColumnInfo b), Hashable v) => Hashable (ConnectionSplit b v)

traverseConnectionSplit
  :: (Applicative f)
  => (a -> f b) -> ConnectionSplit backend a -> f (ConnectionSplit backend b)
traverseConnectionSplit f (ConnectionSplit k v ob) =
  ConnectionSplit k <$> f v <*> pure ob

data ConnectionSelect (b :: BackendType) v
  = ConnectionSelect
  { _csPrimaryKeyColumns :: !(PrimaryKeyColumns b)
  , _csSplit             :: !(Maybe (NE.NonEmpty (ConnectionSplit b v)))
  , _csSlice             :: !(Maybe ConnectionSlice)
  , _csSelect            :: !(AnnSelectG b (ConnectionFields b v) v)
  }

traverseConnectionSelect
  :: (Applicative f)
  => (a -> f b)
  -> ConnectionSelect backend a -> f (ConnectionSelect backend b)
traverseConnectionSelect f (ConnectionSelect pkCols cSplit cSlice sel) =
  ConnectionSelect pkCols
  <$> traverse (traverse (traverseConnectionSplit f)) cSplit
  <*> pure cSlice
  <*> traverseAnnSelect (traverse (traverse (traverseConnectionField f))) f sel

data FunctionArgsExpG a
  = FunctionArgsExp
  { _faePositional :: ![a]
  , _faeNamed      :: !(HM.HashMap Text a)
  } deriving (Show, Eq, Functor, Foldable, Traversable, Generic)
instance (Hashable a) => Hashable (FunctionArgsExpG a)

emptyFunctionArgsExp :: FunctionArgsExpG a
emptyFunctionArgsExp = FunctionArgsExp [] HM.empty

type FunctionArgExp b = FunctionArgsExpG (SQLExpression b)

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


$(makeLenses ''AnnSelectG)
$(makePrisms ''AnnFieldG)
$(makePrisms ''AnnOrderByElementG)
