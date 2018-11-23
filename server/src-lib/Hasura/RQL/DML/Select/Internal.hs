{-# LANGUAGE DeriveLift        #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Hasura.RQL.DML.Select.Internal where

import           Control.Arrow              ((&&&))
import           Data.Aeson.Types
import           Data.List                  (delete, sort)
import           Instances.TH.Lift          ()
import           Language.Haskell.TH.Syntax (Lift)

import qualified Data.HashMap.Strict        as HM
import qualified Data.List.NonEmpty         as NE
import qualified Data.Text                  as T

import           Hasura.Prelude
import           Hasura.RQL.DML.Internal
import           Hasura.RQL.GBoolExp
import           Hasura.RQL.Types
import           Hasura.Server.Utils
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

data AnnAggOrdBy
  = AAOCount
  | AAOOp !T.Text !PGCol
  deriving (Show, Eq)

data AnnObCol
  = AOCPG !PGColInfo
  | AOCRel !RelInfo !AnnBoolExpSQL !AnnObCol
  | AOCAgg !RelInfo !AnnBoolExpSQL !AnnAggOrdBy
  deriving (Show, Eq)

type AnnOrderByItem = OrderByItemG AnnObCol

data AnnRel
  = AnnRel
  { arName    :: !RelName    -- Relationship name
  , arType    :: !RelType    -- Relationship type (ObjRel, ArrRel)
  , arMapping :: ![(PGCol, PGCol)]      -- Column of the left table to join with
  , arAnnSel  :: !AnnSel -- Current table. Almost ~ to SQL Select
  } deriving (Show, Eq)

data ArrRelNode
  = ArrRelNode
  { arnExtrAls :: ![S.Alias]
  , arnNode    :: !RelNode
  } deriving (Show, Eq)

mergeArrRelNodes :: ArrRelNode -> ArrRelNode -> ArrRelNode
mergeArrRelNodes lJAS rJAS =
  ArrRelNode (lExtrs `union` rExtrs) $ mergeRelNodes lBN rBN
  where
    ArrRelNode lExtrs lBN = lJAS
    ArrRelNode rExtrs rBN = rJAS


type AnnAggSel = AnnSelG [(T.Text, TableAggFld)]

data AggSel
  = AggSel
  { agRelName    :: !RelName
  , agColMapping :: ![(PGCol, PGCol)]
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

type ColFlds = [(T.Text, PGColFld)]

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

type AggFlds = [(T.Text, AggFld)]

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

tableFromToFromItem :: TableFrom -> S.FromItem
tableFromToFromItem = \case
  TableFrom tn Nothing  -> S.FISimple tn Nothing
  TableFrom _  (Just i) -> S.FIIden i

tableFromToQual :: TableFrom -> S.Qual
tableFromToQual = \case
  TableFrom tn Nothing  -> S.QualTable tn
  TableFrom _  (Just i) -> S.QualIden i

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
  , _bnObjRels  :: !(HM.HashMap RelName RelNode)
  , _bnArrRels  :: !(HM.HashMap S.Alias ArrRelNode)
  , _bnAggs     :: !(HM.HashMap S.Alias AggNode)
  } deriving (Show, Eq)

txtToAlias :: Text -> S.Alias
txtToAlias = S.Alias . Iden

aggFldToExp :: AggFlds -> S.SQLExp
aggFldToExp aggFlds = jsonRow
  where
    jsonRow = S.applyJsonBuildObj (concatMap aggToFlds aggFlds)
    withAls fldName sqlExp = [S.SELit fldName, sqlExp]
    aggToFlds (t, fld) = withAls t $ case fld of
      AFCount cty -> S.SECount cty
      AFOp aggOp  -> aggOpToObj aggOp
      AFExp e     -> S.SELit e

    aggOpToObj (AggOp op flds) =
      S.applyJsonBuildObj $ concatMap (colFldsToExtr op) flds

    colFldsToExtr op (t, PCFCol col) =
      [ S.SELit t
      , S.SEFnApp op [S.SEIden $ toIden col] Nothing
      ]
    colFldsToExtr _ (t, PCFExp e) =
      [ S.SELit t , S.SELit e]

asSingleRow :: [S.Alias] -> S.FromItem -> S.Select
asSingleRow cols fromItem =
  S.mkSelect
  { S.selExtr  = flip map cols $
                 \c -> S.Extractor (mkExtr c) $ Just c
  , S.selFrom  = Just $ S.FromExp [fromItem]
  }
  where
    mkExtr c    = S.SEFnApp "coalesce" [jsonAgg c, S.SELit "null"] Nothing
    jsonAgg c = S.SEOpApp (S.SQLOp "->")
              [ S.SEFnApp "json_agg" [S.SEIden $ toIden c] Nothing
              , S.SEUnsafe "0"
              ]

aggNodeToSelect :: BaseNode -> [S.Extractor] -> S.BoolExp -> S.Select
aggNodeToSelect bn extrs joinCond =
  S.mkSelect
    { S.selExtr = extrs
    , S.selFrom = Just $ S.FromExp [selFrom]
    }
  where
    selFrom = S.mkSelFromItem (baseNodeToSel joinCond bn) $ S.Alias $
              _bnPrefix bn

withJsonAgg :: Maybe S.OrderByExp -> [S.Alias] -> S.FromItem -> S.Select
withJsonAgg orderByM cols fromItem =
  S.mkSelect
  { S.selExtr = flip map cols $
                \c -> S.Extractor (mkExtr c) $ Just c
  , S.selFrom = Just $ S.FromExp [fromItem]
  }
  where
    mkExtr c = S.SEFnApp "coalesce" [jsonAgg c, S.SELit "[]"] Nothing
    jsonAgg c = S.SEFnApp "json_agg" [S.SEIden $ toIden c] orderByM

asJsonAggSel :: Bool -> [S.Alias] -> S.BoolExp -> BaseNode -> S.Select
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
mkUniqArrRelAls :: FieldName -> [FieldName] -> Iden
mkUniqArrRelAls parAls flds =
  Iden $
  getFieldNameTxt parAls <> "."
  <> T.intercalate "." (map getFieldNameTxt flds)

mkArrRelTableAls :: Iden -> FieldName -> [FieldName] -> Iden
mkArrRelTableAls pfx parAls flds =
  pfx <> Iden ".ar." <> uniqArrRelAls
  where
    uniqArrRelAls = mkUniqArrRelAls parAls flds

mkObjRelTableAls :: Iden -> RelName -> Iden
mkObjRelTableAls pfx relName =
  pfx <> Iden ".or." <> toIden relName

mkAggAls :: Iden -> FieldName -> Iden
mkAggAls pfx fldAls =
  pfx <> Iden ".agg." <> toIden fldAls

mkMergedAggNodeAls :: Iden -> RelName -> [FieldName] -> Iden
mkMergedAggNodeAls pfx relName flds =
  pfx <> Iden ".agg." <> toIden relName <> Iden "."
  <> Iden (T.intercalate "." $ map getFieldNameTxt flds)

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
  :: Iden -> FieldName -> AllAggCtx -> [(FieldName, AnnRel)]
  -> [(FieldName, AnnFld)] -> (S.Alias, S.SQLExp)
buildJsonObject pfx parAls allAggCtx allArrRels flds =
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
              ArrRel -> snd $ mkArrRelPfx pfx parAls
                        allArrRels (fldAls, annRel)
        in S.mkQIdenExp qual fldAls
      FAgg aggSel      ->
        let aggPfx = mkAggNodePfx pfx allAggCtx $
                     ANIField (fldAls, aggSel)
        in S.mkQIdenExp aggPfx fldAls

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

data OrderByNode
  = OBNNothing
  | OBNRelNode !RelName !RelNode
  | OBNAggNode !S.Alias !AggNode
  deriving (Show, Eq)

mkAggObFld :: AnnAggOrdBy -> FieldName
mkAggObFld = \case
  AAOCount     -> FieldName "count"
  AAOOp op col -> FieldName $ op <> "." <> getPGColTxt col

mkAggObExtrAndFlds :: AnnAggOrdBy -> (S.Extractor, AggFlds)
mkAggObExtrAndFlds annAggOb = case annAggOb of
  AAOCount       ->
    ( S.Extractor S.countStar als
    , [("count", AFCount S.CTStar)]
    )
  AAOOp op pgCol ->
    ( S.Extractor (S.SEFnApp op [S.SEIden $ toIden pgCol] Nothing) als
    , [(op, AFOp $ AggOp op [(getPGColTxt pgCol, PCFCol pgCol)])]
    )
  where
    als = Just $ S.toAlias $ mkAggObFld annAggOb

processAnnOrderByItem
  :: Iden
  -> AllAggCtx
  -> AnnOrderByItem
       -- the extractors which will select the needed columns
  -> ( (S.Alias, S.SQLExp)
       -- the sql order by item that is attached to the final select
     , S.OrderByItem
       -- extra nodes for order by
     , OrderByNode
     )
processAnnOrderByItem pfx aggCtx (OrderByItemG obTyM annObCol obNullsM) =
  ( (obColAls, obColExp)
  , sqlOrdByItem
  , relNodeM
  )
  where
    ((obColAls, obColExp), relNodeM) = processAnnOrderByCol pfx aggCtx annObCol

    sqlOrdByItem =
      S.OrderByItem (S.SEIden $ toIden obColAls) obTyM obNullsM

processAnnOrderByCol
  :: Iden
  -> AllAggCtx
  -> AnnObCol
       -- the extractors which will select the needed columns
  -> ( (S.Alias, S.SQLExp)
       -- extra nodes for order by
     , OrderByNode
     )
processAnnOrderByCol pfx aggCtx = \case
  AOCPG colInfo ->
    let
      qualCol  = S.mkQIdenExp (mkBaseTableAls pfx) (toIden $ pgiName colInfo)
      obColAls = mkBaseTableColAls pfx $ pgiName colInfo
    in ( (S.Alias obColAls, qualCol)
       , OBNNothing
       )
  -- "pfx.or.relname"."pfx.ob.or.relname.rest" AS "pfx.ob.or.relname.rest"
  AOCRel (RelInfo rn _ colMapping relTab _) relFltr rest ->
    let relPfx  = mkObjRelTableAls pfx rn
        emptyAggCtx = AllAggCtx [] []
        ((nesAls, nesCol), ordByNode) = processAnnOrderByCol relPfx emptyAggCtx rest
        (objRelNodeM, aggNodeM) = case ordByNode of
          OBNNothing           -> (Nothing, Nothing)
          OBNRelNode name node -> (Just (name, node), Nothing)
          OBNAggNode als node  -> (Nothing, Just (als, node))
        qualCol = S.mkQIdenExp relPfx nesAls
        relBaseNode =
          BaseNode relPfx Nothing (S.FISimple relTab Nothing)
          (toSQLBoolExp (S.QualTable relTab) relFltr)
          Nothing Nothing Nothing
          (HM.singleton nesAls nesCol)
          (maybe HM.empty (uncurry HM.singleton) objRelNodeM)
          HM.empty
          (maybe HM.empty (uncurry HM.singleton) aggNodeM)
        relNode = RelNode rn (fromRel rn) colMapping relBaseNode
    in ( (nesAls, qualCol)
       , OBNRelNode rn relNode
       )
  AOCAgg (RelInfo rn _ colMapping relTab _ ) relFltr annAggOb ->
    let aggPfx = mkAggNodePfx pfx aggCtx $ ANIOrdBy rn
        fldName = mkAggObFld annAggOb
        qOrdBy = S.mkQIdenExp aggPfx $ toIden fldName
        tabFrom = TableFrom relTab Nothing
        tabPerm = TablePerm relFltr Nothing
        (extr, aggFlds) = mkAggObExtrAndFlds annAggOb
        selFld = TAFAgg aggFlds
        bn = mkBaseNode aggPfx fldName selFld tabFrom tabPerm noTableArgs
        aggNode = AggNode colMapping [extr] $ mergeBaseNodes bn $
                  mkEmptyBaseNode aggPfx tabFrom
        obAls = aggPfx <> Iden "." <> toIden fldName
    in ( (S.Alias obAls, qOrdBy)
       , OBNAggNode (S.Alias aggPfx) aggNode
       )

processDistinctOnCol
  :: Iden
  -> NE.NonEmpty PGCol
  -> ( S.DistinctExpr
     -- additional column extractors
     , [(S.Alias, S.SQLExp)]
     )
processDistinctOnCol pfx neCols = (distOnExp, colExtrs)
  where
    cols = toList neCols
    distOnExp = S.DistinctOn $ map (S.SEIden . toIden . mkQColAls) cols
    mkQCol c = S.mkQIdenExp (mkBaseTableAls pfx) $ toIden c
    mkQColAls = S.Alias . mkBaseTableColAls pfx
    colExtrs = flip map cols $ mkQColAls &&& mkQCol


mkEmptyBaseNode :: Iden -> TableFrom -> BaseNode
mkEmptyBaseNode pfx tableFrom =
  BaseNode pfx Nothing fromItem (S.BELit True) Nothing Nothing Nothing
  selOne HM.empty HM.empty HM.empty
  where
    selOne = HM.singleton (S.Alias $ pfx <> Iden "__one") (S.SEUnsafe "1")
    fromItem = tableFromToFromItem tableFrom

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
  AggNode colMapping [extr] mergedBN
  where
    AggSel _ colMapping annSel = aggSel
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
      TAFAgg flds -> aggFldToExp flds
      TAFNodes _ ->
        let jsonAgg = S.SEFnApp "json_agg" [S.SEIden $ Iden t] ordBy
        in S.SEFnApp "coalesce" [jsonAgg, S.SELit "[]"] Nothing
      TAFExp e ->
        -- bool_or to force aggregation
        S.SEFnApp "coalesce"
        [ S.SELit e , S.SEUnsafe "bool_or('true')::text"] Nothing

data AllAggCtx
  = AllAggCtx
  { aacFields :: ![(FieldName, AggSel)]
  , aacOrdBys :: ![RelName]
  } deriving (Show, Eq)

data AggNodeItem
  = ANIField !(FieldName, AggSel)
  | ANIOrdBy !RelName
  deriving (Show, Eq)

mkAggNodePfx
  :: Iden
  -> AllAggCtx
  -> AggNodeItem
  -> Iden
mkAggNodePfx pfx (AllAggCtx aggFlds obRels) = \case
  ANIField aggFld@(fld, AggSel rn _ annAggSel) ->
    let tabArgs = _asnArgs annAggSel
        similarFlds = getSimilarAggFlds rn tabArgs $ delete aggFld
        similarOrdByFound = rn `elem` obRels && tabArgs == noTableArgs
        similarFound = not (null similarFlds) || similarOrdByFound
        extraOrdByFlds = bool [] [ordByFld] similarOrdByFound
        sortedFlds = sort $ fld : (similarFlds <> extraOrdByFlds)
    in bool (mkAggAls pfx fld) (mkMergedAggNodeAls pfx rn sortedFlds)
             similarFound
  ANIOrdBy rn ->
    let similarFlds = getSimilarAggFlds rn noTableArgs id
        sortedFlds = sort $ ordByFld:similarFlds
    in mkMergedAggNodeAls pfx rn sortedFlds
  where
    ordByFld = FieldName "order_by"
    getSimilarAggFlds rn tabArgs f = map fst $
      flip filter (f aggFlds) $ \(_, AggSel arn _ annAggSel) ->
        (rn == arn) && (tabArgs == _asnArgs annAggSel)

mkAllAggCtx
  :: TableAggFld
  -> Maybe (NE.NonEmpty AnnOrderByItem)
  -> AllAggCtx
mkAllAggCtx tabAggFld orderByM =
  AllAggCtx aggFlds ordByAggRels
  where
    aggFlds = case tabAggFld of
      TAFNodes flds -> mapMaybe getAggFld flds
      _             -> []

    ordByAggRels = maybe []
      (mapMaybe (fetchAggOrdByRels . obiColumn) . toList) orderByM

    getAggFld (f, annFld) = case annFld of
        FAgg af -> Just (f, af)
        _       -> Nothing

    fetchAggOrdByRels (AOCAgg ri _ _) = Just $ riName ri
    fetchAggOrdByRels _               = Nothing

mkArrRelPfx
  :: Iden -> FieldName -> [(FieldName, AnnRel)]
  -> (FieldName, AnnRel) -> (S.Alias, Iden)
mkArrRelPfx pfx parAls allArrRels arrRel@(fld, annRel) =
  let getTabArgs = _asnArgs . arAnnSel
      similarFlds = map fst $
        flip filter (delete arrRel allArrRels) $
          \(_, ar) -> (arName ar == arName annRel)
                      && (getTabArgs ar == getTabArgs annRel)
      sortedFlds = sort $ fld:similarFlds
  in ( S.Alias $ mkUniqArrRelAls parAls sortedFlds
     , mkArrRelTableAls pfx parAls sortedFlds
     )

mkBaseNode
  :: Iden -> FieldName -> TableAggFld -> TableFrom
  -> TablePerm -> TableArgs -> BaseNode
mkBaseNode pfx fldAls annSelFlds tableFrom tablePerm tableArgs =
  BaseNode pfx distExprM fromItem finalWhere ordByExpM finalLimit offsetM
  allExtrs allObjsWithOb allArrs aggs
  where
    TablePerm fltr permLimitM = tablePerm
    TableArgs whereM orderByM limitM offsetM distM = tableArgs
    allAggCtx@(AllAggCtx aggFlds _) = mkAllAggCtx annSelFlds orderByM

    (allExtrs, allObjsWithOb, allArrs, aggs) = case annSelFlds of
      TAFNodes flds ->
        let selExtr = buildJsonObject pfx fldAls allAggCtx arrRels flds
            -- all the relationships
            relFlds = mapMaybe getAnnRel flds
            arrRels = flip filter relFlds $ \(_, annRel) ->
                      arType annRel == ArrRel
            (allObjs, allArrRels) =
              foldl' (addRel arrRels) (HM.empty, HM.empty) relFlds
            aggItems = HM.fromListWith mergeAggNodes $ map mkAggItem aggFlds
        in ( HM.fromList $ selExtr:obExtrs <> distExtrs
           , mkTotalObjRels allObjs
           , allArrRels
           , HM.unionWith mergeAggNodes aggItems aggItemsWithOb
           )
      TAFAgg tabAggs ->
        let extrs = concatMap (fetchExtrFromAggFld . snd) tabAggs
        in ( HM.fromList $ extrs <> obExtrs <> distExtrs
           , mkTotalObjRels HM.empty
           , HM.empty
           , aggItemsWithOb
           )
      TAFExp _ -> (HM.fromList obExtrs, HM.empty, HM.empty, HM.empty)

    fetchExtrFromAggFld (AFCount cty) = countTyToExps cty
    fetchExtrFromAggFld (AFOp aggOp)  = aggOpToExps aggOp
    fetchExtrFromAggFld (AFExp _)     = []

    countTyToExps S.CTStar            = []
    countTyToExps (S.CTSimple cols)   = colsToExps cols
    countTyToExps (S.CTDistinct cols) = colsToExps cols

    colsToExps = mapMaybe (mkColExp . PCFCol)

    aggOpToExps = mapMaybe (mkColExp . snd) . _aoFlds

    mkColExp (PCFCol c) =
      let qualCol = S.mkQIdenExp (mkBaseTableAls pfx) (toIden c)
          colAls = toIden c
      in Just (S.Alias colAls, qualCol)
    mkColExp _ = Nothing

    finalWhere =
      toSQLBoolExp tableQual $ maybe fltr (andAnnBoolExps fltr) whereM
    fromItem = tableFromToFromItem tableFrom
    tableQual = tableFromToQual tableFrom
    finalLimit = applyPermLimit permLimitM limitM

    procOrdByM = unzip3 . map (processAnnOrderByItem pfx allAggCtx) . toList <$> orderByM

    distItemsM = processDistinctOnCol pfx <$> distM
    distExprM = fst <$> distItemsM
    distExtrs = fromMaybe [] (snd <$> distItemsM)

    ordByExpM  = S.OrderByExp . _2 <$> procOrdByM
    mkTotalObjRels objRels =
      foldl' (\objs (rn, relNode) -> HM.insertWith mergeRelNodes rn relNode objs)
      objRels $ mapMaybe getOrdByRelNode $ maybe [] _3 procOrdByM
    aggItemsWithOb = HM.fromListWith mergeAggNodes
      $ mapMaybe getOrdByAggNode $ maybe [] _3 procOrdByM

    -- the columns needed for orderby
    obExtrs  = maybe [] _1 procOrdByM

    -- process a relationship
    addRel allArrRels (objs, arrs) (relAls, annRel) =
      let relName    = arName annRel
      in case arType annRel of
        -- in case of object relationships, we merge
        ObjRel ->
          let relNodePfx = mkObjRelTableAls pfx relName
              relNode = mkRelNode relNodePfx (relAls, annRel)
          in (HM.insertWith mergeRelNodes relName relNode objs, arrs)
        ArrRel ->
          let --arrRelTableAls = S.Alias $ mkUniqArrRelAls fldAls relAls
              (arrRelTableAls, relNodePfx) =
                mkArrRelPfx pfx fldAls allArrRels (relAls, annRel)
              relNode = mkRelNode relNodePfx (relAls, annRel)
              jsonAggNode = ArrRelNode [S.Alias $ toIden relAls] relNode
          in (objs, HM.insertWith mergeArrRelNodes arrRelTableAls jsonAggNode arrs)


    -- process agg field
    mkAggItem (f, aggSel) =
      let aggPfx = mkAggNodePfx pfx allAggCtx $ ANIField (f, aggSel)
          aggAls = S.Alias aggPfx
          aggNode = aggSelToAggNode aggPfx f aggSel
      in (aggAls, aggNode)

    getAnnRel (f, annFld) = case annFld of
      FRel ar -> Just (f, ar)
      _       -> Nothing

    getOrdByRelNode (OBNRelNode name node) = Just (name, node)
    getOrdByRelNode _                      = Nothing

    getOrdByAggNode (OBNAggNode als node) = Just (als, node)
    getOrdByAggNode _                     = Nothing

annSelToBaseNode :: Iden -> FieldName -> AnnSel -> BaseNode
annSelToBaseNode pfx fldAls annSel =
  mkBaseNode pfx fldAls (TAFNodes selFlds) tabFrm tabPerm tabArgs
  where
    AnnSelG selFlds tabFrm tabPerm tabArgs = annSel

mergeBaseNodes :: BaseNode -> BaseNode -> BaseNode
mergeBaseNodes lNodeDet rNodeDet =
  BaseNode pfx dExp f whr ordBy limit offset
  (HM.union lExtrs rExtrs)
  (HM.unionWith mergeRelNodes lObjs rObjs)
  (HM.union lArrs rArrs)
  (HM.union lAggs rAggs)
  where
    (BaseNode pfx dExp f whr ordBy limit offset lExtrs lObjs lArrs lAggs) = lNodeDet
    (BaseNode _ _  _ _   _     _     _      rExtrs rObjs rArrs rAggs) = rNodeDet

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
  , _anExtr       :: ![S.Extractor]
  , _anNodeDet    :: !BaseNode
  } deriving (Show, Eq)

mergeAggNodes :: AggNode -> AggNode -> AggNode
mergeAggNodes lAggNode rAggNode =
  AggNode colMapn (lExtrs `union` rExtrs) $ mergeBaseNodes lBN rBN
  where
    AggNode colMapn lExtrs lBN = lAggNode
    AggNode _ rExtrs rBN = rAggNode

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
baseNodeToSel joinCond baseNode =
  S.mkSelect
  { S.selExtr    = [S.Extractor e $ Just a | (a, e) <- HM.toList extrs]
  , S.selFrom    = Just $ S.FromExp [joinedFrom]
  , S.selOrderBy = ordByM
  , S.selLimit   = S.LimitExp . S.intToSQLExp <$> limitM
  , S.selOffset  = S.OffsetExp <$> offsetM
  , S.selDistinct = dExp
  }
  where
    BaseNode pfx dExp fromItem whr ordByM limitM
             offsetM extrs objRels arrRels aggs
             = baseNode
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

    arrRelToFromItem :: ArrRelNode -> S.FromItem
    arrRelToFromItem (ArrRelNode alses relNode) =
      let RelNode _ _ relMapn relBaseNode = relNode
          als = S.Alias $ _bnPrefix relBaseNode
          sel = asJsonAggSel False alses (mkJoinCond baseSelAls relMapn) relBaseNode
      in S.mkLateralFromItem sel als

    aggToFromItem :: (S.Alias, AggNode) -> S.FromItem
    aggToFromItem (als, AggNode colMapn extr bn) =
      let sel = aggNodeToSelect bn extr (mkJoinCond baseSelAls colMapn)
      in S.mkLateralFromItem sel als
