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
import           Hasura.SQL.Types

import qualified Hasura.SQL.DML             as S

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

data AnnObCol
  = AOCPG !PGColInfo
  | AOCObj !RelInfo !AnnBoolExpSQL !AnnObCol
  | AOCAgg !RelInfo !AnnBoolExpSQL !AnnAggOrdBy
  deriving (Show, Eq)

type AnnOrderByItem = OrderByItemG AnnObCol

data AnnRelG a
  = AnnRelG
  { aarName    :: !RelName -- Relationship name
  , aarMapping :: ![(PGCol, PGCol)] -- Column of left table to join with
  , aarAnnSel  :: !a -- Current table. Almost ~ to SQL Select
  } deriving (Show, Eq)

type ObjSel = AnnRelG AnnSel
type ArrRel = AnnRelG AnnSel
type ArrRelAgg = AnnRelG AnnAggSel

data ArrSel
  = ASSimple !ArrRel
  | ASAgg !ArrRelAgg
  deriving (Show, Eq)

data AnnFld
  = FCol !PGColInfo
  | FObj !ObjSel
  | FArr !ArrSel
  | FExp !T.Text
  deriving (Show, Eq)

data TableArgs
  = TableArgs
  { _taWhere    :: !(Maybe AnnBoolExpSQL)
  , _taOrderBy  :: !(Maybe (NE.NonEmpty AnnOrderByItem))
  , _taLimit    :: !(Maybe Int)
  , _taOffset   :: !(Maybe S.SQLExp)
  , _taDistCols :: !(Maybe (NE.NonEmpty PGCol))
  } deriving (Show, Eq)

noTableArgs :: TableArgs
noTableArgs = TableArgs Nothing Nothing Nothing Nothing Nothing

data PGColFld
  = PCFCol !PGCol
  | PCFExp !T.Text
  deriving (Show, Eq)

type ColFlds = [(FieldName, PGColFld)]

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

type AggFlds = [(FieldName, AggFld)]

data TableAggFld
  = TAFAgg !AggFlds
  | TAFNodes ![(FieldName, AnnFld)]
  | TAFExp !T.Text
  deriving (Show, Eq)

data TableFrom
  = TableFrom
  { _tfTable :: !QualifiedTable
  , _tfIden  :: !(Maybe Iden)
  } deriving (Show, Eq)

data TablePerm
  = TablePerm
  { _tpFilter :: !AnnBoolExpSQL
  , _tpLimit  :: !(Maybe Int)
  } deriving (Eq, Show)

data AnnSelG a
  = AnnSelG
  { _asnFields :: !a
  , _asnFrom   :: !TableFrom
  , _asnPerm   :: !TablePerm
  , _asnArgs   :: !TableArgs
  } deriving (Show, Eq)

type AnnSel = AnnSelG [(FieldName, AnnFld)]
type AnnAggSel = AnnSelG [(FieldName, TableAggFld)]

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

data ArrRelCtx
  = ArrRelCtx
  { aacFields    :: ![(FieldName, ArrSel)]
  , aacAggOrdBys :: ![RelName]
  } deriving (Show, Eq)

emptyArrRelCtx :: ArrRelCtx
emptyArrRelCtx = ArrRelCtx [] []

data ArrNodeItem
  = ANIField !(FieldName, ArrSel)
  | ANIAggOrdBy !RelName
  deriving (Show, Eq)

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
