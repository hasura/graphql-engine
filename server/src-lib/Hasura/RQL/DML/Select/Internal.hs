{-# LANGUAGE DeriveLift        #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Hasura.RQL.DML.Select.Internal where

import           Data.Aeson.Types
import           Instances.TH.Lift          ()
import           Language.Haskell.TH.Syntax (Lift)

import qualified Data.HashMap.Strict        as HM
import qualified Data.List.NonEmpty         as NE
import qualified Data.Text                  as T

import           Hasura.Prelude
import           Hasura.RQL.DML.Internal
import           Hasura.RQL.GBoolExp
import           Hasura.RQL.Types
import           Hasura.SQL.Types

import qualified Hasura.SQL.DML             as S

-- Conversion of SelectQ happens in 2 Stages.
-- Stage 1 : Convert input query into an annotated AST
-- Stage 2 : Convert annotated AST to SQL Select

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

data AnnObCol
  = AOCPG !PGColInfo
  | AOCRel !RelInfo !S.BoolExp !AnnObCol
  deriving (Show, Eq)

type AnnOrderByItem = OrderByItemG AnnObCol

data AnnRel
  = AnnRel
  { arName    :: !RelName    -- Relationship name
  , arType    :: !RelType    -- Relationship type (ObjRel, ArrRel)
  , arMapping :: ![(PGCol, PGCol)]      -- Column of the left table to join with
  , arAnnSel  :: !AnnSel -- Current table. Almost ~ to SQL Select
  } deriving (Show, Eq)

type AnnAggSel = AnnSelG [(T.Text, TableAggFld)]

data AggSel
  = AggSel
  { agColMapping :: ![(PGCol, PGCol)]
  , agAnnSel     :: !AnnAggSel
  } deriving (Show, Eq)

data AnnFld
  = FCol !PGColInfo
  | FExp !T.Text
  | FRel !AnnRel
  | FAgg !AggSel
  deriving (Show, Eq)

data TableArgs
  = TableArgs
  { _taWhere   :: !(Maybe (GBoolExp AnnSQLBoolExp))
  , _taOrderBy :: !(Maybe (NE.NonEmpty AnnOrderByItem))
  , _taLimit   :: !(Maybe Int)
  , _taOffset  :: !(Maybe S.SQLExp)
  } deriving (Show, Eq)

noTableArgs :: TableArgs
noTableArgs = TableArgs Nothing Nothing Nothing Nothing

data PGColFld
  = PCFCol !PGCol
  | PCFExp !T.Text
  deriving (Show, Eq)

type ColFlds = [(T.Text, PGColFld)]

data AggFld
  = AFCount
  | AFSum !ColFlds
  | AFAvg !ColFlds
  | AFMax !ColFlds
  | AFMin !ColFlds
  | AFExp !T.Text
  deriving (Show, Eq)

type AggFlds = [(T.Text, AggFld)]

data TableAggFld
  = TAFAgg !AggFlds
  | TAFNodes ![(FieldName, AnnFld)]
  | TAFExp !T.Text
  deriving (Show, Eq)

data TableFrom
  = TableFrom
  { _tfTable :: !QualifiedTable
  , _tfFrom  :: !(Maybe S.FromItem)
  } deriving (Show, Eq)

data TablePerm
  = TablePerm
  { _tpFilter :: !S.BoolExp
  , _tpLimit  :: !(Maybe Int)
  } deriving (Show, Eq)

data AnnSelG a
  = AnnSelG
  { _asnFields :: !a
  , _asnFrom   :: !TableFrom
  , _asnPerm   :: !TablePerm
  , _asnArgs   :: !TableArgs
  } deriving (Show, Eq)

type AnnSel = AnnSelG [(FieldName, AnnFld)]

data BaseNode
  = BaseNode
  { _bnPrefix  :: !Iden
  , _bnFrom    :: !S.FromItem
  , _bnWhere   :: !S.BoolExp
  , _bnOrderBy :: !(Maybe S.OrderByExp)
  , _bnLimit   :: !(Maybe Int)
  , _bnOffset  :: !(Maybe S.SQLExp)

  , _bnExtrs   :: !(HM.HashMap S.Alias S.SQLExp)
  , _bnObjRels :: !(HM.HashMap RelName RelNode)
  , _bnArrRels :: !(HM.HashMap S.Alias RelNode)
  , _bnAggs    :: !(HM.HashMap S.Alias AggNode)

  } deriving (Show, Eq)

txtToAlias :: Text -> S.Alias
txtToAlias = S.Alias . Iden

aggFldToExp :: Iden -> AggFlds -> S.SQLExp
aggFldToExp pfx aggFlds = jsonRow
  where
    jsonRow = S.applyJsonBuildObj (concatMap aggToFlds aggFlds)
    withAls fldName sqlExp = [S.SELit fldName, sqlExp]
    aggToFlds (t, fld) = withAls t $ case fld of
      AFCount       -> S.SEUnsafe "count(*)"
      AFSum sumFlds -> colFldsToObj "sum" sumFlds
      AFAvg avgFlds -> colFldsToObj "avg" avgFlds
      AFMax maxFlds -> colFldsToObj "max" maxFlds
      AFMin minFlds -> colFldsToObj "min" minFlds
      AFExp e       -> S.SELit e

    colFldsToObj op flds =
      S.applyJsonBuildObj $ concatMap (colFldsToExtr op) flds

    colFldsToExtr op (t, PCFCol col) =
      [ S.SELit t
      , S.SEFnApp op [S.SEIden $ mkBaseTableColAls pfx col] Nothing
      ]
    colFldsToExtr _ (t, PCFExp e) =
      [ S.SELit t , S.SELit e]

asSingleRow :: S.Alias -> S.FromItem -> S.Select
asSingleRow col fromItem =
  S.mkSelect
  { S.selExtr  = [S.Extractor extr $ Just col]
  , S.selFrom  = Just $ S.FromExp [fromItem]
  }
  where
    extr    = S.SEFnApp "coalesce" [jsonAgg, S.SELit "null"] Nothing
    jsonAgg = S.SEOpApp (S.SQLOp "->")
              [ S.SEFnApp "json_agg" [S.SEIden $ toIden col] Nothing
              , S.SEUnsafe "0"
              ]

aggNodeToSelect :: BaseNode -> S.Extractor -> S.BoolExp -> S.Select
aggNodeToSelect bn extr joinCond =
  S.mkSelect
    { S.selExtr = [extr]
    , S.selFrom = Just $ S.FromExp [selFrom]
    }
  where
    selFrom = S.mkSelFromItem (baseNodeToSel joinCond bn) $ S.Alias $
              _bnPrefix bn

withJsonAgg :: Maybe S.OrderByExp -> S.Alias -> S.FromItem -> S.Select
withJsonAgg orderByM col fromItem =
  S.mkSelect
  { S.selExtr = [S.Extractor extr $ Just col]
  , S.selFrom = Just $ S.FromExp [fromItem]
  }
  where
    extr    = S.SEFnApp "coalesce" [jsonAgg, S.SELit "[]"] Nothing
    jsonAgg = S.SEFnApp "json_agg" [S.SEIden $ toIden col] orderByM

asJsonAggSel :: Bool -> S.Alias -> S.BoolExp -> BaseNode -> S.Select
asJsonAggSel singleObj als joinCond n =
  let ordByM = _bnOrderBy n
      fromItem = S.mkSelFromItem (baseNodeToSel joinCond n) $
                 S.Alias $ _bnPrefix n
  in bool
     (withJsonAgg ordByM als fromItem)
     (asSingleRow als fromItem)
     singleObj

-- array relationships are not grouped, so have to be prefixed by
-- parent's alias
mkUniqArrRelAls :: FieldName -> FieldName -> Iden
mkUniqArrRelAls parAls relAls =
  Iden $
  getFieldNameTxt parAls <> "." <> getFieldNameTxt relAls

mkArrRelTableAls :: Iden -> FieldName -> FieldName -> Iden
mkArrRelTableAls pfx parAls relAls =
  pfx <> Iden ".ar." <> uniqArrRelAls
  where
    uniqArrRelAls = mkUniqArrRelAls parAls relAls

mkObjRelTableAls :: Iden -> RelName -> Iden
mkObjRelTableAls pfx relName =
  pfx <> Iden ".or." <> toIden relName

mkAggAls :: Iden -> FieldName -> Iden
mkAggAls pfx fldAls =
  pfx <> Iden ".agg." <> toIden fldAls

mkBaseTableAls :: Iden -> Iden
mkBaseTableAls pfx =
  pfx <> Iden ".base"

mkBaseTableColAls :: Iden -> PGCol -> Iden
mkBaseTableColAls pfx pgCol =
  pfx <> Iden ".pg." <> toIden pgCol

-- posttgres ignores anything beyond 63 chars for an iden
-- in this case, we'll need to use json_build_object function
-- json_build_object is slower than row_to_json hence it is only
-- used when needed
buildJsonObject
  :: Iden -> FieldName
  -> [(FieldName, AnnFld)] -> (S.Alias, S.SQLExp)
buildJsonObject pfx parAls flds =
  if any ( (> 63) . T.length . getFieldNameTxt . fst ) flds
  then withJsonBuildObj parAls jsonBuildObjExps
  else withRowToJSON parAls rowToJsonExtrs
  where
    jsonBuildObjExps = concatMap (toSQLFld withAlsExp) flds
    rowToJsonExtrs = map (toSQLFld withAlsExtr) flds

    withAlsExp fldName sqlExp =
      [S.SELit $ getFieldNameTxt fldName, sqlExp]
    withAlsExtr fldName sqlExp =
      S.Extractor sqlExp $ Just $ S.toAlias fldName

    toSQLFld f (fldAls, fld) = f fldAls $ case fld of
      FCol col    -> toJSONableExp (pgiType col) $
                     S.mkQIdenExp (mkBaseTableAls pfx) $ pgiName col
      FExp e      -> S.SELit e
      FRel annRel ->
        let qual = case arType annRel of
              ObjRel -> mkObjRelTableAls pfx $ arName annRel
              ArrRel -> mkArrRelTableAls pfx parAls fldAls
        in S.mkQIdenExp qual fldAls
      FAgg _      -> S.mkQIdenExp (mkAggAls pfx fldAls) fldAls

-- uses row_to_json to build a json object
withRowToJSON
  :: FieldName -> [S.Extractor] -> (S.Alias, S.SQLExp)
withRowToJSON parAls extrs =
  (S.toAlias parAls, jsonRow)
  where
    jsonRow = S.applyRowToJson extrs

-- uses json_build_object to build a json object
withJsonBuildObj
  :: FieldName -> [S.SQLExp] -> (S.Alias, S.SQLExp)
withJsonBuildObj parAls exps =
  (S.toAlias parAls, jsonRow)
  where
    jsonRow = S.applyJsonBuildObj exps

processAnnOrderByItem
  :: Iden
  -> AnnOrderByItem
       -- the extractors which will select the needed columns
  -> ( (S.Alias, S.SQLExp)
       -- the sql order by item that is attached to the final select
     , S.OrderByItem
       -- optionally we may have to add an obj rel node
     , Maybe (RelName, RelNode)
     )
processAnnOrderByItem pfx (OrderByItemG obTyM annObCol obNullsM) =
  ( (obColAls, obColExp)
  , sqlOrdByItem
  , relNodeM
  )
  where
    ((obColAls, obColExp), relNodeM) = processAnnOrderByCol pfx annObCol

    sqlOrdByItem =
      S.OrderByItem (S.SEIden $ toIden obColAls) obTyM obNullsM

processAnnOrderByCol
  :: Iden
  -> AnnObCol
       -- the extractors which will select the needed columns
  -> ( (S.Alias, S.SQLExp)
       -- optionally we may have to add an obj rel node
     , Maybe (RelName, RelNode)
     )
processAnnOrderByCol pfx = \case
  AOCPG colInfo ->
    let
      qualCol  = S.mkQIdenExp (mkBaseTableAls pfx) (toIden $ pgiName colInfo)
      obColAls = mkBaseTableColAls pfx $ pgiName colInfo
    in ( (S.Alias obColAls, qualCol)
       , Nothing
       )
  -- "pfx.or.relname"."pfx.ob.or.relname.rest" AS "pfx.ob.or.relname.rest"
  AOCRel (RelInfo rn _ colMapping relTab _ _) relFltr rest ->
    let relPfx  = mkObjRelTableAls pfx rn
        ((nesAls, nesCol), nesNodeM) = processAnnOrderByCol relPfx rest
        qualCol = S.mkQIdenExp relPfx nesAls
        relBaseNode =
          BaseNode relPfx (S.FISimple relTab Nothing) relFltr
          Nothing Nothing Nothing
          (HM.singleton nesAls nesCol)
          (maybe HM.empty (uncurry HM.singleton) nesNodeM)
          HM.empty HM.empty
        relNode = RelNode rn (fromRel rn) colMapping relBaseNode
    in ( (nesAls, qualCol)
       , Just (rn, relNode)
       )

mkEmptyBaseNode :: Iden -> TableFrom -> BaseNode
mkEmptyBaseNode pfx tableFrom =
  BaseNode pfx fromItem (S.BELit True) Nothing Nothing Nothing
  selOne HM.empty HM.empty HM.empty
  where
    selOne = HM.singleton (S.Alias $ pfx <> Iden "__one") (S.SEUnsafe "1")
    TableFrom tn fromItemM = tableFrom
    fromItem = fromMaybe (S.FISimple tn Nothing) fromItemM

-- If query limit > permission limit then consider permission limit Else consider query limit
applyPermLimit
  :: Maybe Int -- Permission limit
  -> Maybe Int -- Query limit
  -> Maybe Int -- Return SQL exp
applyPermLimit mPermLimit mQueryLimit =
  maybe mQueryLimit compareWithPermLimit mPermLimit
  where
    compareWithPermLimit pLimit =
      maybe (Just pLimit) (compareLimits pLimit) mQueryLimit
    compareLimits pLimit qLimit = Just $
      if qLimit > pLimit then pLimit else qLimit

aggSelToAggNode :: Iden -> FieldName -> AggSel -> AggNode
aggSelToAggNode pfx als aggSel =
  AggNode colMapping extr mergedBN
  where
    AggSel colMapping annSel = aggSel
    AnnSelG aggFlds tabFrm tabPerm tabArgs = annSel
    fldAls = S.Alias $ toIden als

    extr = flip S.Extractor (Just fldAls) $ S.applyJsonBuildObj $
           concatMap selFldToExtr aggFlds

    ordBy = _bnOrderBy mergedBN

    allBNs = map mkAggBaseNode aggFlds
    emptyBN = mkEmptyBaseNode pfx tabFrm
    mergedBN = foldr mergeBaseNodes emptyBN allBNs

    mkAggBaseNode (t, selFld) =
      mkBaseNode pfx (FieldName t) selFld tabFrm tabPerm tabArgs

    selFldToExtr (t, fld) = (:) (S.SELit t) $ pure $ case fld of
      TAFAgg flds ->
        aggFldToExp pfx flds
      TAFNodes _ ->
        let jsonAgg = S.SEFnApp "json_agg" [S.SEIden $ Iden t] ordBy
        in S.SEFnApp "coalesce" [jsonAgg, S.SELit "[]"] Nothing
      TAFExp e ->
        -- bool_or to force aggregation
        S.SEFnApp "coalesce"
        [ S.SELit e , S.SEUnsafe "bool_or('true')::text"] Nothing

mkBaseNode
  :: Iden -> FieldName -> TableAggFld -> TableFrom
  -> TablePerm -> TableArgs -> BaseNode
mkBaseNode pfx fldAls annSelFlds tableFrom tablePerm tableArgs =
  BaseNode pfx fromItem finalWhere ordByExpM finalLimit offsetM
  allExtrs allObjsWithOb allArrs aggs
  where
    TableFrom tn fromItemM = tableFrom
    TablePerm fltr permLimitM = tablePerm
    TableArgs whereM orderByM limitM offsetM = tableArgs
    (allExtrs, allObjsWithOb, allArrs, aggs) = case annSelFlds of
      TAFNodes flds ->
        let selExtr = buildJsonObject pfx fldAls flds
            -- all the relationships
            (allObjs, allArrRels) =
              foldl' addRel (HM.empty, HM.empty) $
              mapMaybe (\(als, f) -> (als,) <$> getAnnRel f) flds
            allObjRelsWithOb =
              foldl' (\objs (rn, relNode) -> HM.insertWith mergeRelNodes rn relNode objs)
              allObjs $ catMaybes $ maybe [] _3 procOrdByM
            aggItems = HM.fromList $ map mkAggItem $
              mapMaybe (\(als, f) -> (als,) <$> getAggFld f) flds
        in ( HM.fromList $ selExtr:obExtrs
           , allObjRelsWithOb
           , allArrRels
           , aggItems
           )
      TAFAgg aggFlds ->
        let extrs = concatMap (fetchExtrFromAggFld . snd) aggFlds
        in ( HM.fromList $ extrs <> obExtrs
           , HM.empty
           , HM.empty
           , HM.empty
           )
      TAFExp _ -> (HM.fromList obExtrs, HM.empty, HM.empty, HM.empty)

    fetchExtrFromAggFld AFCount         = []
    fetchExtrFromAggFld (AFSum sumFlds) = colFldsToExps sumFlds
    fetchExtrFromAggFld (AFAvg avgFlds) = colFldsToExps avgFlds
    fetchExtrFromAggFld (AFMax maxFlds) = colFldsToExps maxFlds
    fetchExtrFromAggFld (AFMin minFlds) = colFldsToExps minFlds
    fetchExtrFromAggFld (AFExp _)       = []

    colFldsToExps = mapMaybe (mkColExp . snd)

    mkColExp (PCFCol c) =
      let qualCol = S.mkQIdenExp (mkBaseTableAls pfx) (toIden c)
          colAls = mkBaseTableColAls pfx c
      in Just (S.Alias colAls, qualCol)
    mkColExp _ = Nothing

    finalWhere = maybe fltr (S.BEBin S.AndOp fltr . cBoolExp) whereM
    finalLimit = applyPermLimit permLimitM limitM

    fromItem = fromMaybe (S.FISimple tn Nothing) fromItemM

    _1 (a, _, _) = a
    _2 (_, b, _) = b
    _3 (_, _, c) = c

    procOrdByM = unzip3 . map (processAnnOrderByItem pfx) . toList <$> orderByM
    ordByExpM  = S.OrderByExp . _2 <$> procOrdByM

    -- the columns needed for orderby
    obExtrs  = maybe [] _1 procOrdByM

    mkRelPfx rTy rn relAls = case rTy of
      ObjRel -> mkObjRelTableAls pfx rn
      ArrRel -> mkArrRelTableAls pfx fldAls relAls

    -- process a relationship
    addRel (objs, arrs) (relAls, annRel) =
      let relName    = arName annRel
          relNodePfx = mkRelPfx (arType annRel) relName relAls
          relNode    = mkRelNode relNodePfx (relAls, annRel)
      in case arType annRel of
        -- in case of object relationships, we merge
        ObjRel ->
          (HM.insertWith mergeRelNodes relName relNode objs, arrs)
        ArrRel ->
          let arrRelTableAls = S.Alias $ mkUniqArrRelAls fldAls relAls
          in (objs, HM.insert arrRelTableAls relNode arrs)

    -- process agg field
    mkAggItem (f, aggSel) =
      let aggPfx = mkAggAls pfx f
          aggAls = S.Alias aggPfx
          aggNode = aggSelToAggNode aggPfx f aggSel
      in (aggAls, aggNode)

    getAnnRel = \case
      FRel ar -> Just ar
      _ -> Nothing

    getAggFld = \case
      FAgg af -> Just af
      _ -> Nothing

annSelToBaseNode :: Iden -> FieldName -> AnnSel -> BaseNode
annSelToBaseNode pfx fldAls annSel =
  mkBaseNode pfx fldAls (TAFNodes selFlds) tabFrm tabPerm tabArgs
  where
    AnnSelG selFlds tabFrm tabPerm tabArgs = annSel

mergeBaseNodes :: BaseNode -> BaseNode -> BaseNode
mergeBaseNodes lNodeDet rNodeDet =
  BaseNode pfx f whr ordBy limit offset
  (HM.union lExtrs rExtrs)
  (HM.unionWith mergeRelNodes lObjs rObjs)
  (HM.union lArrs rArrs)
  (HM.union lAggs rAggs)
  where
    (BaseNode pfx f whr ordBy limit offset lExtrs lObjs lArrs lAggs) = lNodeDet
    (BaseNode _   _ _   _     _     _      rExtrs rObjs rArrs rAggs) = rNodeDet

-- should only be used to merge obj rel nodes
mergeRelNodes :: RelNode -> RelNode -> RelNode
mergeRelNodes lNode rNode =
  RelNode rn rAls rMapn $ mergeBaseNodes lNodeDet rNodeDet
  where
    (RelNode rn rAls rMapn lNodeDet) = lNode
    (RelNode _  _    _     rNodeDet) = rNode

data RelNode
  = RelNode
  { _rnRelName    :: !RelName
  , _rnRelAlias   :: !FieldName
  , _rnRelMapping :: ![(PGCol, PGCol)]
  , _rnNodeDet    :: !BaseNode
  } deriving (Show, Eq)

mkRelNode :: Iden -> (FieldName, AnnRel) -> RelNode
mkRelNode pfx (relAls, AnnRel rn _ rMapn rAnnSel) =
  RelNode rn relAls rMapn $ annSelToBaseNode pfx relAls rAnnSel

data AggNode
  = AggNode
  { _anColMapping :: ![(PGCol, PGCol)]
  , _anExtr       :: !S.Extractor
  , _anNodeDet    :: !BaseNode
  } deriving (Show, Eq)

injectJoinCond :: S.BoolExp       -- ^ Join condition
               -> S.BoolExp -- ^ Where condition
               -> S.WhereFrag     -- ^ New where frag
injectJoinCond joinCond whereCond =
  S.WhereFrag $ S.simplifyBoolExp $ S.BEBin S.AndOp joinCond whereCond

mkJoinCond :: S.Alias -> [(PGCol, PGCol)] -> S.BoolExp
mkJoinCond baseTableAls colMapn =
  foldl' (S.BEBin S.AndOp) (S.BELit True) $ flip map colMapn $
  \(lCol, rCol) ->
    S.BECompare S.SEQ (S.mkQIdenExp baseTableAls lCol) (S.mkSIdenExp rCol)

baseNodeToSel :: S.BoolExp -> BaseNode -> S.Select
baseNodeToSel joinCond (BaseNode pfx fromItem whr ordByM limitM offsetM extrs objRels arrRels aggs) =
  S.mkSelect
  { S.selExtr    = [S.Extractor e $ Just a | (a, e) <- HM.toList extrs]
  , S.selFrom    = Just $ S.FromExp [joinedFrom]
  , S.selOrderBy = ordByM
  , S.selLimit   = S.LimitExp . S.intToSQLExp <$> limitM
  , S.selOffset  = S.OffsetExp <$> offsetM
  }
  where
    -- this is the table which is aliased as "pfx.base"
    baseSel = S.mkSelect
      { S.selExtr  = [S.Extractor S.SEStar Nothing]
      , S.selFrom  = Just $ S.FromExp [fromItem]
      , S.selWhere = Just $ injectJoinCond joinCond whr
      }
    baseSelAls = S.Alias $ mkBaseTableAls pfx
    baseFromItem = S.FISelect (S.Lateral False) baseSel baseSelAls

    -- function to create a joined from item from two from items
    leftOuterJoin current new =
      S.FIJoin $ S.JoinExpr current S.LeftOuter new $
      S.JoinOn $ S.BELit True

    -- this is the from eexp for the final select
    joinedFrom :: S.FromItem
    joinedFrom = foldl' leftOuterJoin baseFromItem $
                 map objRelToFromItem (HM.elems objRels) <>
                 map arrRelToFromItem (HM.elems arrRels) <>
                 map aggToFromItem (HM.toList aggs)

    objRelToFromItem :: RelNode -> S.FromItem
    objRelToFromItem (RelNode _ _ relMapn relBaseNode) =
      let als = S.Alias $ _bnPrefix relBaseNode
          sel = baseNodeToSel (mkJoinCond baseSelAls relMapn) relBaseNode
      in S.mkLateralFromItem sel als

    arrRelToFromItem :: RelNode -> S.FromItem
    arrRelToFromItem (RelNode _ relFld relMapn relBaseNode) =
      let als = S.Alias $ _bnPrefix relBaseNode
          fldAls = S.Alias $ toIden relFld
          sel = asJsonAggSel False fldAls (mkJoinCond baseSelAls relMapn) relBaseNode
      in S.mkLateralFromItem sel als

    aggToFromItem :: (S.Alias, AggNode) -> S.FromItem
    aggToFromItem (als, AggNode colMapn extr bn) =
      let sel = aggNodeToSelect bn extr (mkJoinCond baseSelAls colMapn)
      in S.mkLateralFromItem sel als
