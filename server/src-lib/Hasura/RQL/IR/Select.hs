{-# LANGUAGE DeriveLift           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}

module Hasura.RQL.IR.Select where

import           Hasura.Prelude

import qualified Data.HashMap.Strict                 as HM
import qualified Data.List.NonEmpty                  as NE
import qualified Data.Sequence                       as Seq
import qualified Language.GraphQL.Draft.Syntax       as G

import           Control.Lens.TH                     (makeLenses, makePrisms)

import qualified Hasura.Backends.Postgres.SQL.DML    as S

import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.GraphQL.Parser.Schema
import           Hasura.RQL.IR.BoolExp
import           Hasura.RQL.Types.Column
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.DML
import           Hasura.RQL.Types.Function
import           Hasura.RQL.Types.RemoteRelationship
import           Hasura.RQL.Types.RemoteSchema
import           Hasura.SQL.Backend


data JsonAggSelect
  = JASMultipleRows
  | JASSingleObject
  deriving (Show, Eq, Generic)
instance Hashable JsonAggSelect

data AnnAggregateOrderBy (b :: Backend)
  = AAOCount
  | AAOOp !Text !(ColumnInfo b)
  deriving (Generic)
deriving instance Eq (AnnAggregateOrderBy 'Postgres)
instance Hashable (AnnAggregateOrderBy 'Postgres)

data AnnOrderByElementG (b :: Backend) v
  = AOCColumn !(ColumnInfo b)
  | AOCObjectRelation !RelInfo !v !(AnnOrderByElementG b v)
  | AOCArrayAggregation !RelInfo !v !(AnnAggregateOrderBy b)
  deriving (Generic, Functor)
deriving instance Eq v => Eq (AnnOrderByElementG 'Postgres v)
instance (Hashable v) => Hashable (AnnOrderByElementG 'Postgres v)

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

type AnnOrderByItemG b v = OrderByItemG (AnnOrderByElement b v)

traverseAnnOrderByItem
  :: (Applicative f)
  => (a -> f b) -> AnnOrderByItemG backend a -> f (AnnOrderByItemG backend b)
traverseAnnOrderByItem f =
  traverse (traverseAnnOrderByElement f)

type AnnOrderByItem b = AnnOrderByItemG b S.SQLExp

type OrderByItemExp b =
  OrderByItemG (AnnOrderByElement b S.SQLExp, (S.Alias, S.SQLExp))

data AnnRelationSelectG (b :: Backend) a
  = AnnRelationSelectG
  { aarRelationshipName :: !RelName -- Relationship name
  , aarColumnMapping    :: !(HashMap (Column b) (Column b)) -- Column of left table to join with
  , aarAnnSelect        :: !a -- Current table. Almost ~ to SQL Select
  } deriving (Functor, Foldable, Traversable)

type ArrayRelationSelectG b v = AnnRelationSelectG b (AnnSimpleSelG b v)
type ArrayAggregateSelectG b v = AnnRelationSelectG b (AnnAggregateSelectG b v)
type ArrayConnectionSelect b v = AnnRelationSelectG b (ConnectionSelect b v)
type ArrayAggregateSelect b = ArrayAggregateSelectG b S.SQLExp

data AnnObjectSelectG (b :: Backend) v
  = AnnObjectSelectG
  { _aosFields      :: !(AnnFieldsG b v)
  , _aosTableFrom   :: !QualifiedTable
  , _aosTableFilter :: !(AnnBoolExp b v)
  }

type AnnObjectSelect b = AnnObjectSelectG b S.SQLExp

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
type ObjectRelationSelect b = ObjectRelationSelectG b S.SQLExp

data ComputedFieldScalarSelect v
  = ComputedFieldScalarSelect
  { _cfssFunction  :: !QualifiedFunction
  , _cfssArguments :: !(FunctionArgsExpTableRow v)
  , _cfssType      :: !PGScalarType
  , _cfssColumnOp  :: !(Maybe ColumnOp)
  } deriving (Show, Eq, Functor, Foldable, Traversable)

data ComputedFieldSelect (b :: Backend) v
  = CFSScalar !(ComputedFieldScalarSelect v)
  | CFSTable !JsonAggSelect !(AnnSimpleSelG b v)

traverseComputedFieldSelect
  :: (Applicative f)
  => (v -> f w)
  -> ComputedFieldSelect backend v -> f (ComputedFieldSelect backend w)
traverseComputedFieldSelect fv = \case
  CFSScalar scalarSel -> CFSScalar <$> traverse fv scalarSel
  CFSTable b tableSel -> CFSTable b <$> traverseAnnSimpleSelect fv tableSel

type Fields a = [(FieldName, a)]

data ArraySelectG (b :: Backend) v
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

type ArraySelect b = ArraySelectG b S.SQLExp

type ArraySelectFieldsG b v = Fields (ArraySelectG b v)

data ColumnOp
  = ColumnOp
  { _colOp  :: S.SQLOp
  , _colExp :: S.SQLExp
  } deriving (Show, Eq)

data AnnColumnField (b :: Backend)
  = AnnColumnField
  { _acfInfo   :: !(ColumnInfo b)
  , _acfAsText :: !Bool
  -- ^ If this field is 'True', columns are explicitly casted to @text@ when fetched, which avoids
  -- an issue that occurs because we don’t currently have proper support for array types. See
  -- https://github.com/hasura/graphql-engine/pull/3198 for more details.
  , _acfOp     :: !(Maybe ColumnOp)
  }

data RemoteFieldArgument
  = RemoteFieldArgument
  { _rfaArgument :: !G.Name
  , _rfaValue    :: !(InputValue Variable)
  } deriving (Eq,Show)

data RemoteSelect (b :: Backend)
  = RemoteSelect
  { _rselArgs          :: ![RemoteFieldArgument]
  , _rselSelection     :: !(G.SelectionSet G.NoFragments Variable)
  , _rselHasuraColumns :: !(HashSet (ColumnInfo b))
  , _rselFieldCall     :: !(NonEmpty FieldCall)
  , _rselRemoteSchema  :: !RemoteSchemaInfo
  }

data AnnFieldG (b :: Backend) v
  = AFColumn !(AnnColumnField b)
  | AFObjectRelation !(ObjectRelationSelectG b v)
  | AFArrayRelation !(ArraySelectG b v)
  | AFComputedField !(ComputedFieldSelect b v)
  | AFRemote !(RemoteSelect b)
  | AFNodeId !QualifiedTable !(PrimaryKeyColumns b)
  | AFExpression !Text

mkAnnColumnField :: ColumnInfo backend -> Maybe ColumnOp -> AnnFieldG backend v
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

type AnnField b = AnnFieldG b S.SQLExp

data SelectArgsG (b :: Backend) v
  = SelectArgs
  { _saWhere    :: !(Maybe (AnnBoolExp b v))
  , _saOrderBy  :: !(Maybe (NE.NonEmpty (AnnOrderByItemG b v)))
  , _saLimit    :: !(Maybe Int)
  , _saOffset   :: !(Maybe S.SQLExp)
  , _saDistinct :: !(Maybe (NE.NonEmpty (Column b)))
  } deriving (Generic)
deriving instance Eq v => Eq (SelectArgsG 'Postgres v)
instance (Hashable v) => Hashable (SelectArgsG 'Postgres v)

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

type SelectArgs b = SelectArgsG b S.SQLExp

noSelectArgs :: SelectArgsG backend v
noSelectArgs = SelectArgs Nothing Nothing Nothing Nothing Nothing

data ColFld (b :: Backend)
  = CFCol !(Column b)
  | CFExp !Text
{-
deriving instance Eq (Column b) => Eq (ColFld b)
deriving instance Show (Column b) => Show (ColFld b)
-}

type ColumnFields b = Fields (ColFld b)

data AggregateOp (b :: Backend)
  = AggregateOp
  { _aoOp     :: !Text
  , _aoFields :: !(ColumnFields b)
  }

data AggregateField (b :: Backend)
  = AFCount !S.CountType
  | AFOp !(AggregateOp b)
  | AFExp !Text

type AggregateFields b = Fields (AggregateField b)
type AnnFieldsG b v = Fields (AnnFieldG b v)

traverseAnnFields
  :: (Applicative f)
  => (a -> f b) -> AnnFieldsG backend a -> f (AnnFieldsG backend b)
traverseAnnFields f = traverse (traverse (traverseAnnField f))

type AnnFields b = AnnFieldsG b S.SQLExp

data TableAggregateFieldG (b :: Backend) v
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

data EdgeField (b :: Backend) v
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

data ConnectionField (b :: Backend) v
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

type TableAggregateField b = TableAggregateFieldG b S.SQLExp
type TableAggregateFieldsG b v = Fields (TableAggregateFieldG b v)
type TableAggregateFields b = TableAggregateFieldsG b S.SQLExp

data ArgumentExp a
  = AETableRow !(Maybe Identifier) -- ^ table row accessor
  | AESession !a -- ^ JSON/JSONB hasura session variable object
  | AEInput !a
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic)
instance (Hashable v) => Hashable (ArgumentExp v)

type FunctionArgsExpTableRow v = FunctionArgsExpG (ArgumentExp v)

data SelectFromG (b :: Backend) v
  = FromTable !QualifiedTable
  | FromIdentifier !Identifier
  | FromFunction !QualifiedFunction
                 !(FunctionArgsExpTableRow v)
                 -- a definition list
                 !(Maybe [(Column b, ScalarType b)])
  deriving (Functor, Foldable, Traversable, Generic)
instance (Hashable v) => Hashable (SelectFromG 'Postgres v)

type SelectFrom b = SelectFromG b S.SQLExp

data TablePermG (b :: Backend) v
  = TablePerm
  { _tpFilter :: !(AnnBoolExp b v)
  , _tpLimit  :: !(Maybe Int)
  } deriving (Generic)
instance (Hashable v) => Hashable (TablePermG 'Postgres v)

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

type TablePerm b = TablePermG b S.SQLExp

data AnnSelectG (b :: Backend) a v
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

type AnnSimpleSelG b v = AnnSelectG b (AnnFieldsG b v) v
type AnnSimpleSel b = AnnSimpleSelG b S.SQLExp

type AnnAggregateSelectG b v = AnnSelectG b (TableAggregateFieldsG b v) v
type AnnAggregateSelect b = AnnAggregateSelectG b S.SQLExp

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

data ConnectionSplit (b :: Backend) v
  = ConnectionSplit
  { _csKind    :: !ConnectionSplitKind
  , _csValue   :: !v
  , _csOrderBy :: !(OrderByItemG (AnnOrderByElementG b ()))
  } deriving (Functor, Generic, Foldable, Traversable)
instance (Hashable v) => Hashable (ConnectionSplit 'Postgres v)

traverseConnectionSplit
  :: (Applicative f)
  => (a -> f b) -> ConnectionSplit backend a -> f (ConnectionSplit backend b)
traverseConnectionSplit f (ConnectionSplit k v ob) =
  ConnectionSplit k <$> f v <*> pure ob

data ConnectionSelect (b :: Backend) v
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

type FunctionArgExp = FunctionArgsExpG S.SQLExp

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

data SourcePrefixes
  = SourcePrefixes
  { _pfThis :: !Identifier -- ^ Current source prefix
  , _pfBase :: !Identifier
  -- ^ Base table source row identifier to generate
  -- the table's column identifiers for computed field
  -- function input parameters
  } deriving (Show, Eq, Generic)
instance Hashable SourcePrefixes

data SelectSource
  = SelectSource
  { _ssPrefix   :: !Identifier
  , _ssFrom     :: !S.FromItem
  , _ssDistinct :: !(Maybe S.DistinctExpr)
  , _ssWhere    :: !S.BoolExp
  , _ssOrderBy  :: !(Maybe S.OrderByExp)
  , _ssLimit    :: !(Maybe Int)
  , _ssOffset   :: !(Maybe S.SQLExp)
  } deriving (Show, Eq, Generic)
instance Hashable SelectSource

data SelectNode (b :: Backend)
  = SelectNode
  { _snExtractors :: !(HM.HashMap S.Alias S.SQLExp)
  , _snJoinTree   :: !(JoinTree b)
  }

instance Semigroup (SelectNode 'Postgres) where
  SelectNode lExtrs lJoinTree <> SelectNode rExtrs rJoinTree =
    SelectNode (lExtrs <> rExtrs) (lJoinTree <> rJoinTree)

data ObjectSelectSource
  = ObjectSelectSource
  { _ossPrefix :: !Identifier
  , _ossFrom   :: !S.FromItem
  , _ossWhere  :: !S.BoolExp
  } deriving (Show, Eq, Generic)
instance Hashable ObjectSelectSource

objectSelectSourceToSelectSource :: ObjectSelectSource -> SelectSource
objectSelectSourceToSelectSource ObjectSelectSource{..} =
  SelectSource _ossPrefix _ossFrom Nothing _ossWhere Nothing Nothing Nothing

data ObjectRelationSource (b :: Backend)
  = ObjectRelationSource
  { _orsRelationshipName :: !RelName
  , _orsRelationMapping  :: !(HM.HashMap (Column b) (Column b))
  , _orsSelectSource     :: !ObjectSelectSource
  } deriving (Generic)
instance Hashable (ObjectRelationSource 'Postgres)
deriving instance Eq (Column b) => Eq (ObjectRelationSource b)

data ArrayRelationSource (b :: Backend)
  = ArrayRelationSource
  { _arsAlias           :: !S.Alias
  , _arsRelationMapping :: !(HM.HashMap (Column b) (Column b))
  , _arsSelectSource    :: !SelectSource
  } deriving (Generic)
instance Hashable (ArrayRelationSource 'Postgres)
deriving instance Eq (Column b) => Eq (ArrayRelationSource b)

data ArraySelectNode (b :: Backend)
  = ArraySelectNode
  { _asnTopExtractors :: ![S.Extractor]
  , _asnSelectNode    :: !(SelectNode b)
  }

instance Semigroup (ArraySelectNode 'Postgres) where
  ArraySelectNode lTopExtrs lSelNode <> ArraySelectNode rTopExtrs rSelNode =
    ArraySelectNode (lTopExtrs <> rTopExtrs) (lSelNode <> rSelNode)

data ComputedFieldTableSetSource
  = ComputedFieldTableSetSource
  { _cftssFieldName    :: !FieldName
  , _cftssSelectType   :: !JsonAggSelect
  , _cftssSelectSource :: !SelectSource
  } deriving (Show, Eq, Generic)
instance Hashable ComputedFieldTableSetSource

data ArrayConnectionSource (b :: Backend)
  = ArrayConnectionSource
  { _acsAlias           :: !S.Alias
  , _acsRelationMapping :: !(HM.HashMap (Column b) (Column b))
  , _acsSplitFilter     :: !(Maybe S.BoolExp)
  , _acsSlice           :: !(Maybe ConnectionSlice)
  , _acsSource          :: !SelectSource
  } deriving (Generic)
deriving instance Eq (Column b) => Eq (ArrayConnectionSource b)

instance Hashable (ArrayConnectionSource 'Postgres)

data JoinTree (b :: Backend)
  = JoinTree
  { _jtObjectRelations        :: !(HM.HashMap (ObjectRelationSource b) (SelectNode b))
  , _jtArrayRelations         :: !(HM.HashMap (ArrayRelationSource b) (ArraySelectNode b))
  , _jtArrayConnections       :: !(HM.HashMap (ArrayConnectionSource b) (ArraySelectNode b))
  , _jtComputedFieldTableSets :: !(HM.HashMap ComputedFieldTableSetSource (SelectNode b))
  }

instance Semigroup (JoinTree 'Postgres) where
  JoinTree lObjs lArrs lArrConns lCfts <> JoinTree rObjs rArrs rArrConns rCfts =
    JoinTree (HM.unionWith (<>) lObjs rObjs)
             (HM.unionWith (<>) lArrs rArrs)
             (HM.unionWith (<>) lArrConns rArrConns)
             (HM.unionWith (<>) lCfts rCfts)

instance Monoid (JoinTree 'Postgres) where
  mempty = JoinTree mempty mempty mempty mempty

data PermissionLimitSubQuery
  = PLSQRequired !Int -- ^ Permission limit
  | PLSQNotRequired
  deriving (Show, Eq)

$(makeLenses ''AnnSelectG)
$(makePrisms ''AnnFieldG)
$(makePrisms ''AnnOrderByElementG)
