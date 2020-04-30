{-# LANGUAGE DeriveLift        #-}
{-# LANGUAGE OverloadedStrings #-}

module Hasura.RQL.DML.Select.Types where

import           Control.Lens                  hiding ((.=))
import           Data.Aeson.Types
import           Language.Haskell.TH.Syntax    (Lift)

import qualified Data.HashMap.Strict           as HM
import qualified Data.List.NonEmpty            as NE
import qualified Data.Sequence                 as Seq
import qualified Data.Text                     as T
import qualified Language.GraphQL.Draft.Syntax as G

import           Hasura.Prelude
import           Hasura.RQL.Types
import qualified Hasura.SQL.DML                as S
import           Hasura.SQL.Types

type SelectQExt = SelectG ExtCol BoolExp Int

data JsonAggSelect
  = JASMultipleRows
  | JASSingleObject
  deriving (Show, Eq)

-- Columns in RQL
data ExtCol
  = ECSimple !PGCol
  | ECRel !RelName !(Maybe RelName) !SelectQExt
  deriving (Show, Eq, Lift)

instance ToJSON ExtCol where
  toJSON (ECSimple s) = toJSON s
  toJSON (ECRel rn mrn selq) =
    object $ [ "name" .= rn
             , "alias" .= mrn
             ] ++ selectGToPairs selq

instance FromJSON ExtCol where
  parseJSON v@(Object o) =
    ECRel
    <$> o .:  "name"
    <*> o .:? "alias"
    <*> parseJSON v
  parseJSON v@(String _) =
    ECSimple <$> parseJSON v
  parseJSON _ =
    fail $ mconcat
    [ "A column should either be a string or an "
    , "object (relationship)"
    ]

data AnnAggOrdBy
  = AAOCount
  | AAOOp !T.Text !PGCol
  deriving (Show, Eq)

data AnnObColG v
  = AOCPG !PGCol
  | AOCObj !RelInfo !(AnnBoolExp v) !(AnnObColG v)
  | AOCAgg !RelInfo !(AnnBoolExp v) !AnnAggOrdBy
  deriving (Show, Eq)

traverseAnnObCol
  :: (Applicative f)
  => (a -> f b) -> AnnObColG a -> f (AnnObColG b)
traverseAnnObCol f = \case
  AOCPG pgColInfo -> pure $ AOCPG pgColInfo
  AOCObj relInfo annBoolExp annObCol ->
    AOCObj relInfo
    <$> traverseAnnBoolExp f annBoolExp
    <*> traverseAnnObCol f annObCol
  AOCAgg relInfo annBoolExp annAggOb ->
    AOCAgg relInfo
    <$> traverseAnnBoolExp f annBoolExp
    <*> pure annAggOb

type AnnObCol = AnnObColG S.SQLExp

type AnnOrderByItemG v = OrderByItemG (AnnObColG v)

traverseAnnOrderByItem
  :: (Applicative f)
  => (a -> f b) -> AnnOrderByItemG a -> f (AnnOrderByItemG b)
traverseAnnOrderByItem f =
  traverse (traverseAnnObCol f)

type AnnOrderByItem = AnnOrderByItemG S.SQLExp

data AnnRelG a
  = AnnRelG
  { aarName    :: !RelName -- Relationship name
  , aarMapping :: !(HashMap PGCol PGCol) -- Column of left table to join with
  , aarAnnSel  :: !a -- Current table. Almost ~ to SQL Select
  } deriving (Show, Eq, Functor, Foldable, Traversable)

type ObjSelG v = AnnRelG (AnnSimpleSelG v)
type ObjSel = ObjSelG S.SQLExp

type ArrRelG v = AnnRelG (AnnSimpleSelG v)
type ArrRelAggG v = AnnRelG (AnnAggSelG v)
type ArrRelAgg = ArrRelAggG S.SQLExp

data ComputedFieldScalarSel v
  = ComputedFieldScalarSel
  { _cfssFunction  :: !QualifiedFunction
  , _cfssArguments :: !(FunctionArgsExpTableRow v)
  , _cfssType      :: !PGScalarType
  , _cfssColumnOp  :: !(Maybe ColOp)
  } deriving (Show, Eq, Functor, Foldable, Traversable)

data ComputedFieldSel v
  = CFSScalar !(ComputedFieldScalarSel v)
  | CFSTable !JsonAggSelect !(AnnSimpleSelG v)
  deriving (Show, Eq)

traverseComputedFieldSel
  :: (Applicative f)
  => (v -> f w)
  -> ComputedFieldSel v -> f (ComputedFieldSel w)
traverseComputedFieldSel fv = \case
  CFSScalar scalarSel -> CFSScalar <$> traverse fv scalarSel
  CFSTable b tableSel -> CFSTable b <$> traverseAnnSimpleSel fv tableSel

type Fields a = [(FieldName, a)]

data ArrSelG v
  = ASSimple !(ArrRelG v)
  | ASAgg !(ArrRelAggG v)
  deriving (Show, Eq)

traverseArrSel
  :: (Applicative f)
  => (a -> f b)
  -> ArrSelG a
  -> f (ArrSelG b)
traverseArrSel f = \case
  ASSimple arrRel -> ASSimple <$> traverse (traverseAnnSimpleSel f) arrRel
  ASAgg arrRelAgg -> ASAgg <$> traverse (traverseAnnAggSel f) arrRelAgg

type ArrSel = ArrSelG S.SQLExp

type ArrSelFldsG v = Fields (ArrSelG v)

data ColOp
  = ColOp
  { _colOp  :: S.SQLOp
  , _colExp :: S.SQLExp
  } deriving (Show, Eq)

data AnnColField
  = AnnColField
  { _acfInfo   :: !PGColumnInfo
  , _acfAsText :: !Bool
  -- ^ If this field is 'True', columns are explicitly casted to @text@ when fetched, which avoids
  -- an issue that occurs because we don’t currently have proper support for array types. See
  -- https://github.com/hasura/graphql-engine/pull/3198 for more details.
  , _acfOp     :: !(Maybe ColOp)
  } deriving (Show, Eq)

data RemoteSelect
  = RemoteSelect
  { _rselArgs          :: ![G.Argument]
  , _rselSelection     :: ![G.Field]
  , _rselHasuraColumns :: !(HashSet PGColumnInfo)
  , _rselFieldCall     :: !(NonEmpty FieldCall)
  , _rselRemoteSchema  :: !RemoteSchemaInfo
  } deriving (Show, Eq)

data AnnFldG v
  = FCol !AnnColField
  | FObj !(ObjSelG v)
  | FArr !(ArrSelG v)
  | FComputedField !(ComputedFieldSel v)
  | FRemote !RemoteSelect
  | FExp !T.Text
  deriving (Show, Eq)

mkAnnColField :: PGColumnInfo -> Maybe ColOp -> AnnFldG v
mkAnnColField ci colOpM =
  FCol $ AnnColField ci False colOpM

mkAnnColFieldAsText :: PGColumnInfo -> AnnFldG v
mkAnnColFieldAsText ci =
  FCol $ AnnColField ci True Nothing

traverseAnnFld
  :: (Applicative f)
  => (a -> f b) -> AnnFldG a -> f (AnnFldG b)
traverseAnnFld f = \case
  FCol colFld -> pure $ FCol colFld
  FObj sel -> FObj <$> traverse (traverseAnnSimpleSel f) sel
  FArr sel -> FArr <$> traverseArrSel f sel
  FComputedField sel -> FComputedField <$> traverseComputedFieldSel f sel
  FExp t -> FExp <$> pure t
  FRemote s -> pure $ FRemote s

type AnnFld = AnnFldG S.SQLExp

data TableArgsG v
  = TableArgs
  { _taWhere    :: !(Maybe (AnnBoolExp v))
  , _taOrderBy  :: !(Maybe (NE.NonEmpty (AnnOrderByItemG v)))
  , _taLimit    :: !(Maybe Int)
  , _taOffset   :: !(Maybe S.SQLExp)
  , _taDistCols :: !(Maybe (NE.NonEmpty PGCol))
  } deriving (Show, Eq)

traverseTableArgs
  :: (Applicative f)
  => (a -> f b) -> TableArgsG a -> f (TableArgsG b)
traverseTableArgs f (TableArgs wh ordBy lmt ofst distCols) =
  TableArgs
  <$> traverse (traverseAnnBoolExp f) wh
  -- traversing through maybe -> nonempty -> annorderbyitem
  <*> traverse (traverse (traverseAnnOrderByItem f)) ordBy
  <*> pure lmt
  <*> pure ofst
  <*> pure distCols

type TableArgs = TableArgsG S.SQLExp

noTableArgs :: TableArgsG v
noTableArgs = TableArgs Nothing Nothing Nothing Nothing Nothing

data PGColFld
  = PCFCol !PGCol
  | PCFExp !T.Text
  deriving (Show, Eq)

type ColFlds = Fields PGColFld

data AggOp
  = AggOp
  { _aoOp   :: !T.Text
  , _aoFlds :: !ColFlds
  } deriving (Show, Eq)

data AggFld
  = AFCount !S.CountType
  | AFOp !AggOp
  | AFExp !T.Text
  deriving (Show, Eq)

type AggFlds = Fields AggFld
type AnnFldsG v = Fields (AnnFldG v)

traverseAnnFlds
  :: (Applicative f)
  => (a -> f b) -> AnnFldsG a -> f (AnnFldsG b)
traverseAnnFlds f = traverse (traverse (traverseAnnFld f))

type AnnFlds = AnnFldsG S.SQLExp

data TableAggFldG v
  = TAFAgg !AggFlds
  | TAFNodes !(AnnFldsG v)
  | TAFExp !T.Text
  deriving (Show, Eq)

traverseTableAggFld
  :: (Applicative f)
  => (a -> f b) -> TableAggFldG a -> f (TableAggFldG b)
traverseTableAggFld f = \case
  TAFAgg aggFlds -> pure $ TAFAgg aggFlds
  TAFNodes annFlds -> TAFNodes <$> traverseAnnFlds f annFlds
  TAFExp t -> pure $ TAFExp t

type TableAggFld = TableAggFldG S.SQLExp
type TableAggFldsG v = Fields (TableAggFldG v)
type TableAggFlds = TableAggFldsG S.SQLExp

data ArgumentExp a
  = AETableRow !(Maybe Iden) -- ^ table row accessor
  | AESession !a -- ^ JSON/JSONB hasura session variable object
  | AEInput !a
  deriving (Show, Eq, Functor, Foldable, Traversable)

type FunctionArgsExpTableRow v = FunctionArgsExpG (ArgumentExp v)

data SelectFromG v
  = FromTable !QualifiedTable
  | FromIden !Iden
  | FromFunction !QualifiedFunction
                 !(FunctionArgsExpTableRow v)
                 !(Maybe [(PGCol, PGScalarType)])
  deriving (Show, Eq, Functor, Foldable, Traversable)

type SelectFrom = SelectFromG S.SQLExp

data TablePermG v
  = TablePerm
  { _tpFilter :: !(AnnBoolExp v)
  , _tpLimit  :: !(Maybe Int)
  } deriving (Eq, Show)

traverseTablePerm
  :: (Applicative f)
  => (a -> f b)
  -> TablePermG a
  -> f (TablePermG b)
traverseTablePerm f (TablePerm boolExp limit) =
  TablePerm
  <$> traverseAnnBoolExp f boolExp
  <*> pure limit

noTablePermissions :: TablePermG v
noTablePermissions =
  TablePerm annBoolExpTrue Nothing

type TablePerm = TablePermG S.SQLExp

data AnnSelG a v
  = AnnSelG
  { _asnFields   :: !a
  , _asnFrom     :: !(SelectFromG v)
  , _asnPerm     :: !(TablePermG v)
  , _asnArgs     :: !(TableArgsG v)
  , _asnStrfyNum :: !Bool
  } deriving (Show, Eq)

getPermLimit :: AnnSelG a v -> Maybe Int
getPermLimit = _tpLimit . _asnPerm

traverseAnnSimpleSel
  :: (Applicative f)
  => (a -> f b)
  -> AnnSimpleSelG a -> f (AnnSimpleSelG b)
traverseAnnSimpleSel f = traverseAnnSel (traverseAnnFlds f) f

traverseAnnAggSel
  :: (Applicative f)
  => (a -> f b)
  -> AnnAggSelG a -> f (AnnAggSelG b)
traverseAnnAggSel f =
  traverseAnnSel (traverse (traverse (traverseTableAggFld f))) f

traverseAnnSel
  :: (Applicative f)
  => (a -> f b) -> (v -> f w)
  -> AnnSelG a v -> f (AnnSelG b w)
traverseAnnSel f1 f2 (AnnSelG flds tabFrom perm args strfyNum) =
  AnnSelG
  <$> f1 flds
  <*> traverse f2 tabFrom
  <*> traverseTablePerm f2 perm
  <*> traverseTableArgs f2 args
  <*> pure strfyNum

type AnnSimpleSelG v = AnnSelG (AnnFldsG v) v
type AnnSimpleSel = AnnSimpleSelG S.SQLExp

type AnnAggSelG v = AnnSelG (TableAggFldsG v) v
type AnnAggSel = AnnAggSelG S.SQLExp

data FunctionArgsExpG a
  = FunctionArgsExp
  { _faePositional :: ![a]
  , _faeNamed      :: !(HM.HashMap Text a)
  } deriving (Show, Eq, Functor, Foldable, Traversable)

emptyFunctionArgsExp :: FunctionArgsExpG a
emptyFunctionArgsExp = FunctionArgsExp [] HM.empty

type FunctionArgExp = FunctionArgsExpG S.SQLExp

-- | If argument positional index is less than or equal to length of 'positional' arguments then
-- insert the value in 'positional' arguments else insert the value with argument name in 'named' arguments
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

data BaseNode
  = BaseNode
  { _bnPrefix              :: !Iden
  , _bnDistinct            :: !(Maybe S.DistinctExpr)
  , _bnFrom                :: !S.FromItem
  , _bnWhere               :: !S.BoolExp
  , _bnOrderBy             :: !(Maybe S.OrderByExp)
  , _bnLimit               :: !(Maybe Int)
  , _bnOffset              :: !(Maybe S.SQLExp)

  , _bnExtrs               :: !(HM.HashMap S.Alias S.SQLExp)
  , _bnObjs                :: !(HM.HashMap RelName ObjNode)
  , _bnArrs                :: !(HM.HashMap S.Alias ArrNode)
  , _bnComputedFieldTables :: !(HM.HashMap FieldName CFTableNode)
  } deriving (Show, Eq)

mergeBaseNodes :: BaseNode -> BaseNode -> BaseNode
mergeBaseNodes lNodeDet rNodeDet =
  BaseNode pfx dExp f whr ordBy limit offset
  (HM.union lExtrs rExtrs)
  (HM.unionWith mergeObjNodes lObjs rObjs)
  (HM.unionWith mergeArrNodes lArrs rArrs)
  (HM.unionWith mergeCFTableNodes lCFTables rCFTables)
  where
    BaseNode pfx dExp f whr ordBy limit offset lExtrs lObjs lArrs lCFTables
      = lNodeDet
    BaseNode _   _    _ _   _     _     _      rExtrs rObjs rArrs rCFTables
      = rNodeDet

data OrderByNode
  = OBNNothing
  | OBNObjNode !RelName !ObjNode
  | OBNArrNode !S.Alias !ArrNode
  deriving (Show, Eq)

data ArrRelCtxG v
  = ArrRelCtx
  { aacFields    :: !(ArrSelFldsG v)
  , aacAggOrdBys :: ![RelName]
  } deriving (Show, Eq)

type ArrRelCtx = ArrRelCtxG S.SQLExp

emptyArrRelCtx :: ArrRelCtxG a
emptyArrRelCtx = ArrRelCtx [] []

data ArrNodeItemG v
  = ANIField !(FieldName, ArrSelG v)
  | ANIAggOrdBy !RelName
  deriving (Show, Eq)

type ArrNodeItem = ArrNodeItemG S.SQLExp

data ObjNode
  = ObjNode
  { _rnRelMapping :: !(HashMap PGCol PGCol)
  , _rnNodeDet    :: !BaseNode
  } deriving (Show, Eq)

mergeObjNodes :: ObjNode -> ObjNode -> ObjNode
mergeObjNodes lNode rNode =
  ObjNode colMapping $ mergeBaseNodes lBN rBN
  where
    ObjNode colMapping lBN = lNode
    ObjNode _          rBN = rNode

-- simple array select, aggregate select and order by
-- nodes differ in extractors
data ArrNode
  = ArrNode
  { _anExtr       :: ![S.Extractor]
  , _anRelMapping :: !(HashMap PGCol PGCol)
  , _anNodeDet    :: !BaseNode
  } deriving (Show, Eq)

mergeArrNodes :: ArrNode -> ArrNode -> ArrNode
mergeArrNodes lNode rNode =
  ArrNode (lExtrs `union` rExtrs) colMapping $
  mergeBaseNodes lBN rBN
  where
    ArrNode lExtrs colMapping lBN = lNode
    ArrNode rExtrs _          rBN = rNode

data ArrNodeInfo
  = ArrNodeInfo
  { _aniAlias            :: !S.Alias
  , _aniPrefix           :: !Iden
  , _aniSubQueryRequired :: !Bool
  } deriving (Show, Eq)

-- | Node for computed field returning setof <table>
data CFTableNode
  = CFTableNode
  { _ctnSelectType :: !JsonAggSelect
  , _ctnNode       :: !BaseNode
  } deriving (Show, Eq)

mergeCFTableNodes :: CFTableNode -> CFTableNode -> CFTableNode
mergeCFTableNodes lNode rNode =
  CFTableNode
  (_ctnSelectType rNode)
  (mergeBaseNodes (_ctnNode lNode) (_ctnNode rNode))

data Prefixes
  = Prefixes
  { _pfThis :: !Iden -- Current node prefix
  , _pfBase :: !Iden -- Base table row identifier for computed field function
  } deriving (Show, Eq)

$(makeLenses ''AnnSelG)
$(makePrisms ''AnnFldG)
