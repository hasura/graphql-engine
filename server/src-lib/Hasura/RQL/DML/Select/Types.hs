{-# LANGUAGE DeriveLift        #-}
{-# LANGUAGE OverloadedStrings #-}

module Hasura.RQL.DML.Select.Types where

import           Data.Aeson.Types
import           Language.Haskell.TH.Syntax (Lift)

import qualified Data.HashMap.Strict        as HM
import qualified Data.List.NonEmpty         as NE
import qualified Data.Text                  as T

import           Hasura.Prelude
import           Hasura.RQL.Types
import qualified Hasura.SQL.DML             as S
import           Hasura.SQL.Types

type SelectQExt = SelectG ExtCol BoolExp Int
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
  parseJSON (String s) =
    return $ ECSimple $ PGCol s
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
  = AOCPG !PGColInfo
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
  , aarMapping :: ![(PGCol, PGCol)] -- Column of left table to join with
  , aarAnnSel  :: !a -- Current table. Almost ~ to SQL Select
  } deriving (Show, Eq, Functor, Foldable, Traversable)

type ObjSelG v = AnnRelG (AnnSimpleSelG v)
type ObjSel = ObjSelG S.SQLExp
type ArrRelG v = AnnRelG (AnnSimpleSelG v)
type ArrRelAggG v = AnnRelG (AnnAggSelG v)

type ArrRelAgg = ArrRelAggG S.SQLExp

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

data AnnFldG v
  = FCol !PGColInfo !(Maybe ColOp)
  | FObj !(ObjSelG v)
  | FArr !(ArrSelG v)
  | FExp !T.Text
  deriving (Show, Eq)

traverseAnnFld
  :: (Applicative f)
  => (a -> f b) -> AnnFldG a -> f (AnnFldG b)
traverseAnnFld f = \case
  FCol pgColInfo colOpM -> pure $ FCol pgColInfo colOpM
  FObj sel -> FObj <$> traverse (traverseAnnSimpleSel f) sel
  FArr sel -> FArr <$> traverseArrSel f sel
  FExp t -> FExp <$> pure t

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
  TAFNodes annFlds ->
    TAFNodes <$> traverse (traverse (traverseAnnFld f)) annFlds
  TAFExp t -> pure $ TAFExp t

type TableAggFld = TableAggFldG S.SQLExp
type TableAggFldsG v = Fields (TableAggFldG v)
type TableAggFlds = TableAggFldsG S.SQLExp

data TableFrom
  = TableFrom
  { _tfTable :: !QualifiedTable
  , _tfIden  :: !(Maybe Iden)
  } deriving (Show, Eq)

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

type TablePerm = TablePermG S.SQLExp

data AnnSelG a v
  = AnnSelG
  { _asnFields   :: !a
  , _asnFrom     :: !TableFrom
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
traverseAnnSimpleSel f =
  traverseAnnSel (traverse (traverse (traverseAnnFld f))) f

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
  <*> pure tabFrom
  <*> traverseTablePerm f2 perm
  <*> traverseTableArgs f2 args
  <*> pure strfyNum

type AnnSimpleSelG v = AnnSelG (AnnFldsG v) v
type AnnSimpleSel = AnnSimpleSelG S.SQLExp

type AnnAggSelG v = AnnSelG (TableAggFldsG v) v
type AnnAggSel = AnnAggSelG S.SQLExp

data AnnFnSelG s v
  = AnnFnSel
  { _afFn     :: !QualifiedFunction
  , _afFnArgs :: ![v]
  , _afSelect :: !s
  } deriving (Show, Eq)

traverseAnnFnSel
  :: (Applicative f)
  => (a -> f b) -> (v -> f w)
  -> AnnFnSelG a v -> f (AnnFnSelG b w)
traverseAnnFnSel fs fv (AnnFnSel fn fnArgs s) =
  AnnFnSel fn <$> traverse fv fnArgs <*> fs s

type AnnFnSelSimpleG v = AnnFnSelG (AnnSimpleSelG v) v
type AnnFnSelSimple = AnnFnSelSimpleG S.SQLExp

traverseAnnFnSimple
  :: (Applicative f)
  => (a -> f b)
  -> AnnFnSelSimpleG a -> f (AnnFnSelSimpleG b)
traverseAnnFnSimple f =
  traverseAnnFnSel (traverseAnnSimpleSel f) f

type AnnFnSelAggG v = AnnFnSelG (AnnAggSelG v) v
type AnnFnSelAgg = AnnFnSelAggG S.SQLExp

traverseAnnFnAgg
  :: (Applicative f)
  => (a -> f b)
  -> AnnFnSelAggG a -> f (AnnFnSelAggG b)
traverseAnnFnAgg f =
  traverseAnnFnSel (traverseAnnAggSel f) f

data BaseNode
  = BaseNode
  { _bnPrefix   :: !Iden
  , _bnDistinct :: !(Maybe S.DistinctExpr)
  , _bnFrom     :: !S.FromItem
  , _bnWhere    :: !S.BoolExp
  , _bnOrderBy  :: !(Maybe S.OrderByExp)
  , _bnLimit    :: !(Maybe Int)
  , _bnOffset   :: !(Maybe S.SQLExp)

  , _bnExtrs    :: !(HM.HashMap S.Alias S.SQLExp)
  , _bnObjs     :: !(HM.HashMap RelName ObjNode)
  , _bnArrs     :: !(HM.HashMap S.Alias ArrNode)
  } deriving (Show, Eq)

mergeBaseNodes :: BaseNode -> BaseNode -> BaseNode
mergeBaseNodes lNodeDet rNodeDet =
  BaseNode pfx dExp f whr ordBy limit offset
  (HM.union lExtrs rExtrs)
  (HM.unionWith mergeObjNodes lObjs rObjs)
  (HM.unionWith mergeArrNodes lArrs rArrs)
  where
    BaseNode pfx dExp f whr ordBy limit offset lExtrs lObjs lArrs
      = lNodeDet
    BaseNode _   _    _ _   _     _     _      rExtrs rObjs rArrs
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
  { _rnRelMapping :: ![(PGCol, PGCol)]
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
  , _anRelMapping :: ![(PGCol, PGCol)]
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
