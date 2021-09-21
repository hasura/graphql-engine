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

import           Hasura.GraphQL.Parser.Schema        (InputValue)
import           Hasura.RQL.IR.BoolExp
import           Hasura.RQL.IR.OrderBy
import           Hasura.RQL.Types.Backend
import           Hasura.RQL.Types.Column
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.ComputedField
import           Hasura.RQL.Types.Function
import           Hasura.RQL.Types.Instances          ()
import           Hasura.RQL.Types.Relationship
import           Hasura.RQL.Types.RemoteRelationship
import           Hasura.RQL.Types.RemoteSchema
import           Hasura.SQL.Backend


-- Root selection

data QueryDB (b :: BackendType) (r :: BackendType -> Type) v
  = QDBMultipleRows (AnnSimpleSelectG    b r v)
  | QDBSingleRow    (AnnSimpleSelectG    b r v)
  | QDBAggregation  (AnnAggregateSelectG b r v)
  | QDBConnection   (ConnectionSelect    b r v)
  deriving stock (Generic, Functor, Foldable, Traversable)


-- Select

data AnnSelectG (b :: BackendType) (r :: BackendType -> Type) (f :: Type -> Type) (v :: Type)
  = AnnSelectG
  { _asnFields   :: !(Fields (f v))
  , _asnFrom     :: !(SelectFromG b v)
  , _asnPerm     :: !(TablePermG b v)
  , _asnArgs     :: !(SelectArgsG b v)
  , _asnStrfyNum :: !Bool
  } deriving (Functor, Foldable, Traversable)

type AnnSimpleSelectG    b r v = AnnSelectG b r (AnnFieldG            b r) v
type AnnAggregateSelectG b r v = AnnSelectG b r (TableAggregateFieldG b r) v
type AnnSimpleSelect     b     = AnnSimpleSelectG    b (Const Void) (SQLExpression b)
type AnnAggregateSelect  b     = AnnAggregateSelectG b (Const Void) (SQLExpression b)


-- Relay select

data ConnectionSelect (b :: BackendType) (r :: BackendType -> Type) v
  = ConnectionSelect
  { _csXRelay            :: !(XRelay b)
  , _csPrimaryKeyColumns :: !(PrimaryKeyColumns b)
  , _csSplit             :: !(Maybe (NE.NonEmpty (ConnectionSplit b v)))
  , _csSlice             :: !(Maybe ConnectionSlice)
  , _csSelect            :: !(AnnSelectG b r (ConnectionField b r) v)
  } deriving (Functor, Foldable, Traversable)

data ConnectionSplit (b :: BackendType) v
  = ConnectionSplit
  { _csKind    :: !ConnectionSplitKind
  , _csValue   :: !v
  , _csOrderBy :: !(OrderByItemG b (AnnotatedOrderByElement b v))
  } deriving (Functor, Generic, Foldable, Traversable)
instance (Backend b, Hashable (ColumnInfo b), Hashable v, Hashable (BooleanOperators b v)) => Hashable (ConnectionSplit b v)

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

type SelectFrom b = SelectFromG b (SQLExpression b)


-- Select arguments

data SelectArgsG (b :: BackendType) v
  = SelectArgs
  { _saWhere    :: !(Maybe (AnnBoolExp b v))
  , _saOrderBy  :: !(Maybe (NE.NonEmpty (AnnotatedOrderByItemG b v)))
  , _saLimit    :: !(Maybe Int)
  , _saOffset   :: !(Maybe Int64)
  , _saDistinct :: !(Maybe (NE.NonEmpty (Column b)))
  } deriving (Generic, Functor, Foldable, Traversable)

deriving instance
  ( Backend b
  , Eq (BooleanOperators b v)
  , Eq v
  ) => Eq (SelectArgsG b v)

instance
  ( Backend b
  , Hashable (BooleanOperators b v)
  , Hashable v
  ) => Hashable (SelectArgsG b v)

type SelectArgs b = SelectArgsG b (SQLExpression b)

noSelectArgs :: SelectArgsG backend v
noSelectArgs = SelectArgs Nothing Nothing Nothing Nothing Nothing


-- Order by argument

-- | The order by element for a computed field based on its return type
data ComputedFieldOrderByElement (b :: BackendType) v
  = CFOBEScalar !(ScalarType b)
  -- ^ Sort by the scalar computed field
  | CFOBETableAggregation !(TableName b)
    !(AnnBoolExp b v) -- ^ Permission filter of the retuning table
    !(AnnotatedAggregateOrderBy b)
  -- ^ Sort by aggregation fields of table rows returned by computed field
  deriving (Generic, Functor, Foldable, Traversable)
deriving instance (Backend b, Eq v, Eq (BooleanOperators b v)) => Eq (ComputedFieldOrderByElement b v)
instance (Backend b, Hashable v, Hashable (BooleanOperators b v)) => Hashable (ComputedFieldOrderByElement b v)

data ComputedFieldOrderBy (b :: BackendType) v
  = ComputedFieldOrderBy
  { _cfobXField          :: !(XComputedField b)
  , _cfobName            :: !ComputedFieldName
  , _cfobFunction        :: !(FunctionName b)
  , _cfobFunctionArgsExp :: !(FunctionArgsExpTableRow b v)
  , _cfobOrderByElement  :: !(ComputedFieldOrderByElement b v)
  } deriving (Generic, Functor, Foldable, Traversable)
deriving instance (Backend b, Eq v, Eq (BooleanOperators b v)) => Eq (ComputedFieldOrderBy b v)
instance (Backend b, Hashable v, Hashable (BooleanOperators b v)) => Hashable (ComputedFieldOrderBy b v)

data AnnotatedOrderByElement (b :: BackendType) v
  = AOCColumn !(ColumnInfo b)
  | AOCObjectRelation !(RelInfo b)
    !(AnnBoolExp b v) -- ^ Permission filter of the remote table to which the relationship is defined
    !(AnnotatedOrderByElement b v)
  | AOCArrayAggregation !(RelInfo b)
    !(AnnBoolExp b v) -- ^ Permission filter of the remote table to which the relationship is defined
    !(AnnotatedAggregateOrderBy b)
  | AOCComputedField !(ComputedFieldOrderBy b v)
  deriving (Generic, Functor, Foldable, Traversable)
deriving instance (Backend b, Eq v, Eq (BooleanOperators b v)) => Eq (AnnotatedOrderByElement b v)
instance (Backend b, Hashable v, Hashable (BooleanOperators b v)) => Hashable (AnnotatedOrderByElement b v)

data AnnotatedAggregateOrderBy (b :: BackendType)
  = AAOCount
  | AAOOp !Text !(ColumnInfo b)
  deriving (Generic)
deriving instance (Backend b) => Eq (AnnotatedAggregateOrderBy b)
instance (Backend b) => Hashable (AnnotatedAggregateOrderBy b)

type AnnotatedOrderByItemG b v = OrderByItemG b (AnnotatedOrderByElement b v)
type AnnotatedOrderByItem b = AnnotatedOrderByItemG b (SQLExpression b)


-- Fields

-- The field name here is the GraphQL alias, i.e, the name with which the field
-- should appear in the response
type Fields a = [(FieldName, a)]

data AnnFieldG (b :: BackendType) (r :: BackendType -> Type) v
  = AFColumn !(AnnColumnField b v)
  | AFObjectRelation !(ObjectRelationSelectG b r v)
  | AFArrayRelation !(ArraySelectG b r v)
  | AFComputedField !(XComputedField b) !ComputedFieldName !(ComputedFieldSelect b r v)
  -- | A relationship to a remote source/remote schema. Its kind is
  -- (r :: BackendType -> Type) so that AFRemote can capture something
  -- that is specific to the backend AnnFieldG. See RemoteSelect. When
  -- remote joins are extracted from the structure, 'r' becomes 'Const Void'
  | AFRemote !(r b)
  | AFNodeId !(XRelay b) !(TableName b) !(PrimaryKeyColumns b)
  | AFExpression !Text
  deriving (Functor, Foldable, Traversable)

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

-- Aggregation fields

data TableAggregateFieldG (b :: BackendType) (r :: BackendType -> Type) v
  = TAFAgg !(AggregateFields b)
  | TAFNodes (XNodesAgg b) !(AnnFieldsG b r v)
  | TAFExp !Text
  deriving (Functor, Foldable, Traversable)

data AggregateField (b :: BackendType)
  = AFCount !(CountType b)
  | AFOp !(AggregateOp b)
  | AFExp !Text

data AggregateOp (b :: BackendType)
  = AggregateOp
  { _aoOp     :: !Text
  , _aoFields :: !(ColumnFields b)
  }

data ColFld (b :: BackendType)
  = CFCol !(Column b) !(ColumnType b)
  | CFExp !Text

type TableAggregateField   b     = TableAggregateFieldG  b (Const Void) (SQLExpression b)
type TableAggregateFields  b     = TableAggregateFieldsG b (Const Void) (SQLExpression b)
type TableAggregateFieldsG b r v = Fields (TableAggregateFieldG b r v)

type ColumnFields b    = Fields (ColFld b)
type AggregateFields b = Fields (AggregateField b)
type AnnFieldsG b r v  = Fields (AnnFieldG b r v)


-- Relay fields

data ConnectionField (b :: BackendType) (r :: BackendType -> Type) v
  = ConnectionTypename !Text
  | ConnectionPageInfo !PageInfoFields
  | ConnectionEdges !(EdgeFields b r v)
  deriving (Functor, Foldable, Traversable)

data PageInfoField
  = PageInfoTypename !Text
  | PageInfoHasNextPage
  | PageInfoHasPreviousPage
  | PageInfoStartCursor
  | PageInfoEndCursor
  deriving (Show, Eq)

data EdgeField (b :: BackendType) (r :: BackendType -> Type) v
  = EdgeTypename !Text
  | EdgeCursor
  | EdgeNode !(AnnFieldsG b r v)
  deriving (Functor, Foldable, Traversable)

type ConnectionFields b r v = Fields (ConnectionField b r v)

type PageInfoFields   = Fields PageInfoField
type EdgeFields b r v = Fields (EdgeField b r v)

-- Column

data AnnColumnField (b :: BackendType) v
  = AnnColumnField
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
  } deriving (Functor, Foldable, Traversable)

data ColumnOp (b :: BackendType)
  = ColumnOp
  { _colOp  :: SQLOperator b
  , _colExp :: SQLExpression b
  }

deriving instance Backend b => Show (ColumnOp b)
deriving instance Backend b => Eq   (ColumnOp b)


-- Computed field

data ComputedFieldScalarSelect (b :: BackendType) v
  = ComputedFieldScalarSelect
  { _cfssFunction  :: !(FunctionName b)
  , _cfssArguments :: !(FunctionArgsExpTableRow b v)
  , _cfssType      :: !(ScalarType b)
  , _cfssColumnOp  :: !(Maybe (ColumnOp b))
  } deriving (Functor, Foldable, Traversable)
deriving instance (Backend b, Show v) => Show (ComputedFieldScalarSelect b v)
deriving instance (Backend b, Eq   v) => Eq   (ComputedFieldScalarSelect b v)

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
  | CFSTable !JsonAggSelect !(AnnSimpleSelectG b r v)
  deriving (Functor, Foldable, Traversable)


-- Local relationship

data AnnRelationSelectG (b :: BackendType) a
  = AnnRelationSelectG
  { aarRelationshipName :: !RelName -- Relationship name
  , aarColumnMapping    :: !(HashMap (Column b) (Column b)) -- Column of left table to join with
  , aarAnnSelect        :: !a -- Current table. Almost ~ to SQL Select
  } deriving  (Functor, Foldable, Traversable)

type ArrayRelationSelectG  b r v = AnnRelationSelectG b (AnnSimpleSelectG    b r v)
type ArrayAggregateSelectG b r v = AnnRelationSelectG b (AnnAggregateSelectG b r v)
type ArrayConnectionSelect b r v = AnnRelationSelectG b (ConnectionSelect    b r v)
type ArrayAggregateSelect  b     = ArrayAggregateSelectG b (Const Void) (SQLExpression b)

data AnnObjectSelectG (b :: BackendType) (r :: BackendType -> Type) v
  = AnnObjectSelectG
  { _aosFields      :: !(AnnFieldsG b r v)
  , _aosTableFrom   :: !(TableName b)
  , _aosTableFilter :: !(AnnBoolExp b v)
  } deriving (Functor, Foldable, Traversable)

type AnnObjectSelect b r = AnnObjectSelectG b r (SQLExpression b)

type ObjectRelationSelectG b r v = AnnRelationSelectG b (AnnObjectSelectG b r v)
type ObjectRelationSelect  b     = ObjectRelationSelectG b (Const Void) (SQLExpression b)


data ArraySelectG (b :: BackendType) (r :: BackendType -> Type) v
  = ASSimple     !(ArrayRelationSelectG  b r v)
  | ASAggregate  !(ArrayAggregateSelectG b r v)
  | ASConnection !(ArrayConnectionSelect b r v)
  deriving (Functor, Foldable, Traversable)

type ArraySelect b = ArraySelectG b (Const Void) (SQLExpression b)
type ArraySelectFieldsG b r v = Fields (ArraySelectG b r v)


-- Remote schema relationships

data RemoteFieldArgument
  = RemoteFieldArgument
  { _rfaArgument :: !G.Name
  , _rfaValue    :: !(InputValue RemoteSchemaVariable)
  } deriving (Eq,Show)

data RemoteSchemaSelect (b :: BackendType)
  = RemoteSchemaSelect
  { _rselArgs             :: ![RemoteFieldArgument]
  , _rselResultCustomizer :: !RemoteResultCustomizer
  , _rselSelection        :: !(G.SelectionSet G.NoFragments RemoteSchemaVariable)
  , _rselHasuraFields     :: !(HashSet (DBJoinField b))
  , _rselFieldCall        :: !(NonEmpty FieldCall)
  , _rselRemoteSchema     :: !RemoteSchemaInfo
  }

-- | Captures the selection set of a remote source relationship.
data SourceRelationshipSelection
    (b :: BackendType)
    (r :: BackendType -> Type)
    (vf :: BackendType -> Type)
  = SourceRelationshipObject !(AnnObjectSelectG b r (vf b))
  | SourceRelationshipArray !(AnnSimpleSelectG b r (vf b))
  | SourceRelationshipArrayAggregate !(AnnAggregateSelectG b r (vf b))

-- | A relationship to a remote source. 'vf' (could use a better name) is
-- analogous to 'v' in other IR types such as 'AnnFieldG'. vf's kind is
-- (BackendType -> Type) instead of v's 'Type' so that 'v' of 'AnnFieldG' can
-- be specific to the backend that it captures ('b' of an AnnFieldG changes as
-- we walk down the IR branches which capture relationships to other databases)
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
    -- (Column tgt) so that an appropriate join condition / IN clause can be built
    -- by the remote
    }

-- | A remote relationship to either a remote schema or a remote source.
-- See RemoteSourceSelect for explanation on 'vf'.
data RemoteSelect
    (vf :: BackendType -> Type)
    (src :: BackendType)
  = RemoteSelectRemoteSchema !(RemoteSchemaSelect src)
  -- | RemoteSelectSource !(AB.AnyBackend (RemoteSourceSelect src vf))
  -- ^ AnyBackend is used here to capture a relationship to an arbitrary target

-- Permissions

data TablePermG (b :: BackendType) v
  = TablePerm
  { _tpFilter :: !(AnnBoolExp b v)
  , _tpLimit  :: !(Maybe Int)
  } deriving (Generic, Functor, Foldable, Traversable)

instance
  ( Backend b
  , Hashable (BooleanOperators b v)
  , Hashable (ColumnInfo b)
  , Hashable v
  ) => Hashable (TablePermG b v)

type TablePerm b = TablePermG b (SQLExpression b)

noTablePermissions :: TablePermG backend v
noTablePermissions = TablePerm annBoolExpTrue Nothing


-- Function arguments

data ArgumentExp (b :: BackendType) a
  = AETableRow !(Maybe (Identifier b)) -- ^ table row accessor
  | AESession !a -- ^ JSON/JSONB hasura session variable object
  | AEInput !a
  deriving (Functor, Foldable, Traversable, Generic)
deriving instance (Backend b, Show a) => Show (ArgumentExp b a)
deriving instance (Backend b, Eq   a) => Eq   (ArgumentExp b a)
instance (Backend b, Hashable v) => Hashable (ArgumentExp b v)

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

functionArgsWithTableRowAndSession
  :: v
  -> FunctionTableArgument
  -> Maybe FunctionSessionArgument
  -> [ArgumentExp b v]
functionArgsWithTableRowAndSession  _    _              Nothing = [AETableRow Nothing] -- No session argument
functionArgsWithTableRowAndSession  sess (FTAFirst)     _       = [AETableRow Nothing, AESession sess]
functionArgsWithTableRowAndSession  sess (FTANamed _ 0) _       = [AETableRow Nothing, AESession sess] -- Index is 0 implies table argument is first
functionArgsWithTableRowAndSession  sess _              _       = [AESession sess, AETableRow Nothing]


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
$(makeLenses ''SelectArgsG)
$(makePrisms ''AnnFieldG)
$(makePrisms ''AnnotatedOrderByElement)
