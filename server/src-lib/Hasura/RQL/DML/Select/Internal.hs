module Hasura.RQL.DML.Select.Internal
  ( mkSQLSelect
  , mkAggSelect
  , module Hasura.RQL.DML.Select.Types
  )
where

import           Control.Arrow               ((&&&))
import           Data.List                   (delete, sort)
import           Instances.TH.Lift           ()

import qualified Data.HashMap.Strict         as HM
import qualified Data.List.NonEmpty          as NE
import qualified Data.Text                   as T

import           Hasura.Prelude
import           Hasura.RQL.DML.Internal
import           Hasura.RQL.DML.Select.Types
import           Hasura.RQL.GBoolExp
import           Hasura.RQL.Types
import           Hasura.Server.Utils
import           Hasura.SQL.Rewrite          (prefixNumToAliases)
import           Hasura.SQL.Types

import qualified Hasura.SQL.DML              as S

-- Conversion of SelectQ happens in 2 Stages.
-- Stage 1 : Convert input query into an annotated AST
-- Stage 2 : Convert annotated AST to SQL Select

tableFromToFromItem :: TableFrom -> S.FromItem
tableFromToFromItem = \case
  TableFrom tn Nothing  -> S.FISimple tn Nothing
  TableFrom _  (Just i) -> S.FIIden i

tableFromToQual :: TableFrom -> S.Qual
tableFromToQual = \case
  TableFrom tn Nothing  -> S.QualTable tn
  TableFrom _  (Just i) -> S.QualIden i

aggFldToExp :: AggFlds -> S.SQLExp
aggFldToExp aggFlds = jsonRow
  where
    jsonRow = S.applyJsonBuildObj (concatMap aggToFlds aggFlds)
    withAls fldName sqlExp = [S.SELit fldName, sqlExp]
    aggToFlds (FieldName t, fld) = withAls t $ case fld of
      AFCount cty -> S.SECount cty
      AFOp aggOp  -> aggOpToObj aggOp
      AFExp e     -> S.SELit e

    aggOpToObj (AggOp op flds) =
      S.applyJsonBuildObj $ concatMap (colFldsToExtr op) flds

    colFldsToExtr op (FieldName t, PCFCol col) =
      [ S.SELit t
      , S.SEFnApp op [S.SEIden $ toIden col] Nothing
      ]
    colFldsToExtr _ (FieldName t, PCFExp e) =
      [ S.SELit t , S.SELit e]

arrNodeToSelect :: BaseNode -> [S.Extractor] -> S.BoolExp -> S.Select
arrNodeToSelect bn extrs joinCond =
  S.mkSelect
    { S.selExtr = extrs
    , S.selFrom = Just $ S.FromExp [selFrom]
    }
  where
    selFrom = S.mkSelFromItem (baseNodeToSel joinCond bn) $ S.Alias $
              _bnPrefix bn

asSingleRowExtr :: S.Alias -> S.Extractor
asSingleRowExtr col = S.Extractor extr $ Just col
  where
    extr    = S.SEFnApp "coalesce" [jsonAgg, S.SELit "null"] Nothing
    jsonAgg = S.SEOpApp (S.SQLOp "->")
              [ S.SEFnApp "json_agg" [S.SEIden $ toIden col] Nothing
              , S.SEUnsafe "0"
              ]

withJsonAggExtr :: Maybe S.OrderByExp -> S.Alias -> S.Extractor
withJsonAggExtr orderByM col =
  S.Extractor extr $ Just col
  where
    extr = S.SEFnApp "coalesce" [jsonAgg, S.SELit "[]"] Nothing
    jsonAgg = S.SEFnApp "json_agg" [S.SEIden $ toIden col] orderByM

asJsonAggExtr :: Bool -> S.Alias -> Maybe S.OrderByExp -> S.Extractor
asJsonAggExtr singleObj als ordByExpM =
  bool (withJsonAggExtr ordByExpM als) (asSingleRowExtr als) singleObj

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

mkBaseTableAls :: Iden -> Iden
mkBaseTableAls pfx =
  pfx <> Iden ".base"

mkBaseTableColAls :: Iden -> PGCol -> Iden
mkBaseTableColAls pfx pgCol =
  pfx <> Iden ".pg." <> toIden pgCol

ordByFldName :: FieldName
ordByFldName = FieldName "order_by"

-- posttgres ignores anything beyond 63 chars for an iden
-- in this case, we'll need to use json_build_object function
-- json_build_object is slower than row_to_json hence it is only
-- used when needed
buildJsonObject
  :: Iden -> FieldName -> ArrRelCtx
  -> [(FieldName, AnnFld)] -> (S.Alias, S.SQLExp)
buildJsonObject pfx parAls arrRelCtx flds =
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

    toSQLFld :: (FieldName -> S.SQLExp -> f)
             -> (FieldName, AnnFld) -> f
    toSQLFld f (fldAls, fld) = f fldAls $ case fld of
      FCol col    -> toJSONableExp (pgiType col) $
                     S.mkQIdenExp (mkBaseTableAls pfx) $ pgiName col
      FExp e      -> S.SELit e
      FObj objSel ->
        let qual = mkObjRelTableAls pfx $ aarName objSel
        in S.mkQIdenExp qual fldAls
      FArr arrSel      ->
        let arrPfx = snd $ mkArrNodePfx pfx parAls arrRelCtx $
                     ANIField (fldAls, arrSel)
        in S.mkQIdenExp arrPfx fldAls

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

mkAggObFld :: AnnAggOrdBy -> FieldName
mkAggObFld = \case
  AAOCount     -> FieldName "count"
  AAOOp op col -> FieldName $ op <> "." <> getPGColTxt col

mkAggObExtrAndFlds :: AnnAggOrdBy -> (S.Extractor, AggFlds)
mkAggObExtrAndFlds annAggOb = case annAggOb of
  AAOCount       ->
    ( S.Extractor S.countStar als
    , [(FieldName "count", AFCount S.CTStar)]
    )
  AAOOp op pgCol ->
    ( S.Extractor (S.SEFnApp op [S.SEIden $ toIden pgCol] Nothing) als
    , [(FieldName op, AFOp $ AggOp op [(fromPGCol pgCol, PCFCol pgCol)])]
    )
  where
    als = Just $ S.toAlias $ mkAggObFld annAggOb

processAnnOrderByItem
  :: Iden
  -> FieldName
  -> ArrRelCtx
  -> AnnOrderByItem
       -- the extractors which will select the needed columns
  -> ( (S.Alias, S.SQLExp)
       -- the sql order by item that is attached to the final select
     , S.OrderByItem
       -- extra nodes for order by
     , OrderByNode
     )
processAnnOrderByItem pfx parAls arrRelCtx (OrderByItemG obTyM annObCol obNullsM) =
  ( (obColAls, obColExp)
  , sqlOrdByItem
  , relNodeM
  )
  where
    ((obColAls, obColExp), relNodeM) = processAnnOrderByCol pfx parAls arrRelCtx annObCol

    sqlOrdByItem =
      S.OrderByItem (S.SEIden $ toIden obColAls)
      (unOrderType <$> obTyM) (unNullsOrder <$> obNullsM)

processAnnOrderByCol
  :: Iden
  -> FieldName
  -> ArrRelCtx
  -> AnnObCol
       -- the extractors which will select the needed columns
  -> ( (S.Alias, S.SQLExp)
       -- extra nodes for order by
     , OrderByNode
     )
processAnnOrderByCol pfx parAls arrRelCtx = \case
  AOCPG colInfo ->
    let
      qualCol  = S.mkQIdenExp (mkBaseTableAls pfx) (toIden $ pgiName colInfo)
      obColAls = mkBaseTableColAls pfx $ pgiName colInfo
    in ( (S.Alias obColAls, qualCol)
       , OBNNothing
       )
  -- "pfx.or.relname"."pfx.ob.or.relname.rest" AS "pfx.ob.or.relname.rest"
  AOCObj (RelInfo rn _ colMapping relTab _) relFltr rest ->
    let relPfx  = mkObjRelTableAls pfx rn
        ((nesAls, nesCol), ordByNode) =
          processAnnOrderByCol relPfx ordByFldName emptyArrRelCtx rest
        (objNodeM, arrNodeM) = case ordByNode of
          OBNNothing           -> (Nothing, Nothing)
          OBNObjNode name node -> (Just (name, node), Nothing)
          OBNArrNode als node  -> (Nothing, Just (als, node))
        qualCol = S.mkQIdenExp relPfx nesAls
        relBaseNode =
          BaseNode relPfx Nothing (S.FISimple relTab Nothing)
          (toSQLBoolExp (S.QualTable relTab) relFltr)
          Nothing Nothing Nothing
          (HM.singleton nesAls nesCol)
          (maybe HM.empty (uncurry HM.singleton) objNodeM)
          (maybe HM.empty (uncurry HM.singleton) arrNodeM)
        relNode = ObjNode colMapping relBaseNode
    in ( (nesAls, qualCol)
       , OBNObjNode rn relNode
       )
  AOCAgg (RelInfo rn _ colMapping relTab _ ) relFltr annAggOb ->
    let (arrAls, arrPfx) =
          mkArrNodePfx pfx parAls arrRelCtx $ ANIAggOrdBy rn
        fldName = mkAggObFld annAggOb
        qOrdBy = S.mkQIdenExp arrPfx $ toIden fldName
        tabFrom = TableFrom relTab Nothing
        tabPerm = TablePerm relFltr Nothing
        (extr, arrFlds) = mkAggObExtrAndFlds annAggOb
        selFld = TAFAgg arrFlds
        bn = mkBaseNode arrPfx fldName selFld tabFrom tabPerm noTableArgs
        aggNode = ArrNode [extr] colMapping $ mergeBaseNodes bn $
                  mkEmptyBaseNode arrPfx tabFrom
        obAls = arrPfx <> Iden "." <> toIden fldName
    in ( (S.Alias obAls, qOrdBy)
       , OBNArrNode arrAls aggNode
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
  BaseNode pfx Nothing fromItem (S.BELit True) Nothing Nothing
  Nothing selOne HM.empty HM.empty
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

aggSelToArrNode :: Iden -> FieldName -> ArrRelAgg -> ArrNode
aggSelToArrNode pfx als aggSel =
  ArrNode [extr] colMapping mergedBN
  where
    AnnRelG _ colMapping annSel = aggSel
    AnnSelG aggFlds tabFrm tabPerm tabArgs = annSel
    fldAls = S.Alias $ toIden als

    extr = flip S.Extractor (Just fldAls) $ S.applyJsonBuildObj $
           concatMap selFldToExtr aggFlds

    ordBy = _bnOrderBy mergedBN

    allBNs = map mkAggBaseNode aggFlds
    emptyBN = mkEmptyBaseNode pfx tabFrm
    mergedBN = foldr mergeBaseNodes emptyBN allBNs

    mkAggBaseNode (fn, selFld) =
      mkBaseNode pfx fn selFld tabFrm tabPerm tabArgs

    selFldToExtr (FieldName t, fld) = (:) (S.SELit t) $ pure $ case fld of
      TAFAgg flds -> aggFldToExp flds
      TAFNodes _ ->
        let jsonAgg = S.SEFnApp "json_agg" [S.SEIden $ Iden t] ordBy
        in S.SEFnApp "coalesce" [jsonAgg, S.SELit "[]"] Nothing
      TAFExp e ->
        -- bool_or to force aggregation
        S.SEFnApp "coalesce"
        [ S.SELit e , S.SEUnsafe "bool_or('true')::text"] Nothing

mkArrNodePfx
  :: Iden
  -> FieldName
  -> ArrRelCtx
  -> ArrNodeItem
  -> (S.Alias, Iden)
mkArrNodePfx pfx parAls (ArrRelCtx arrFlds obRels) = \case
  ANIField aggFld@(fld, annArrSel) ->
    let (rn, tabArgs) = fetchRNAndTArgs annArrSel
        similarFlds = getSimilarAggFlds rn tabArgs $ delete aggFld
        similarOrdByFound = rn `elem` obRels && tabArgs == noTableArgs
        extraOrdByFlds = bool [] [ordByFldName] similarOrdByFound
        sortedFlds = sort $ fld : (similarFlds <> extraOrdByFlds)
    in ( S.Alias $ mkUniqArrRelAls parAls sortedFlds
       , mkArrRelTableAls pfx parAls sortedFlds
       )
  ANIAggOrdBy rn ->
    let similarFlds = getSimilarAggFlds rn noTableArgs id
        sortedFlds = sort $ ordByFldName:similarFlds
    in ( S.Alias $ mkUniqArrRelAls parAls sortedFlds
       , mkArrRelTableAls pfx parAls sortedFlds
       )
  where
    getSimilarAggFlds rn tabArgs f = map fst $
      flip filter (f arrFlds) $ \(_, annArrSel) ->
        let (lrn, lTabArgs) = fetchRNAndTArgs annArrSel
        in (lrn == rn) && (lTabArgs == tabArgs)

    fetchRNAndTArgs (ASSimple (AnnRelG rn _ annSel)) =
      (rn, _asnArgs annSel)
    fetchRNAndTArgs (ASAgg (AnnRelG rn _ annSel)) =
      (rn, _asnArgs annSel)

fetchOrdByAggRels
  :: Maybe (NE.NonEmpty AnnOrderByItem)
  -> [RelName]
fetchOrdByAggRels orderByM = fromMaybe [] relNamesM
  where
    relNamesM =
      mapMaybe (fetchAggOrdByRels . obiColumn) . toList <$> orderByM

    fetchAggOrdByRels (AOCAgg ri _ _) = Just $ riName ri
    fetchAggOrdByRels _               = Nothing

mkOrdByItems
  :: Iden -> FieldName
  -> Maybe (NE.NonEmpty AnnOrderByItem)
  -> ArrRelCtx
     -- extractors
  -> ( [(S.Alias, S.SQLExp)]
     -- object relation nodes
     , HM.HashMap RelName ObjNode
     -- array relation aggregate nodes
     , HM.HashMap S.Alias ArrNode
     -- final order by expression
     , Maybe S.OrderByExp
     )
mkOrdByItems pfx fldAls orderByM arrRelCtx =
  (obExtrs, ordByObjsMap, ordByArrsMap, ordByExpM)
  where
    procAnnOrdBy' = processAnnOrderByItem pfx fldAls arrRelCtx
    procOrdByM =
      unzip3 . map procAnnOrdBy' . toList <$> orderByM

    obExtrs  = maybe [] _1 procOrdByM
    ordByExpM  = S.OrderByExp . _2 <$> procOrdByM

    ordByObjs = mapMaybe getOrdByRelNode $ maybe [] _3 procOrdByM
    ordByObjsMap = HM.fromListWith mergeObjNodes ordByObjs

    ordByAggArrs = mapMaybe getOrdByAggNode $ maybe [] _3 procOrdByM
    ordByArrsMap = HM.fromListWith mergeArrNodes ordByAggArrs

    getOrdByRelNode (OBNObjNode name node) = Just (name, node)
    getOrdByRelNode _                      = Nothing

    getOrdByAggNode (OBNArrNode als node) = Just (als, node)
    getOrdByAggNode _                     = Nothing

mkBaseNode
  :: Iden -> FieldName -> TableAggFld -> TableFrom
  -> TablePerm -> TableArgs -> BaseNode
mkBaseNode pfx fldAls annSelFlds tableFrom tablePerm tableArgs =
  BaseNode pfx distExprM fromItem finalWhere ordByExpM finalLimit offsetM
  allExtrs allObjsWithOb allArrsWithOb
  where
    TablePerm fltr permLimitM = tablePerm
    TableArgs whereM orderByM limitM offsetM distM = tableArgs
    aggOrdByRelNames = fetchOrdByAggRels orderByM

    (allExtrs, allObjsWithOb, allArrsWithOb, ordByExpM) =
      case annSelFlds of
        TAFNodes flds ->
          let arrFlds = mapMaybe getAnnArr flds
              arrRelCtx = mkArrRelCtx arrFlds
              selExtr = buildJsonObject pfx fldAls arrRelCtx flds
              -- all object relationships
              objNodes = HM.fromListWith mergeObjNodes $
                        map mkObjItem (mapMaybe getAnnObj flds)
              -- all array items (array relationships + aggregates)
              arrNodes = HM.fromListWith mergeArrNodes $
                         map (mkArrItem arrRelCtx) arrFlds

              (obExtrs, ordByObjs, ordByArrs, obeM)
                      = mkOrdByItems' arrRelCtx
              allObjs = HM.unionWith mergeObjNodes objNodes ordByObjs
              allArrs = HM.unionWith mergeArrNodes arrNodes ordByArrs

          in ( HM.fromList $ selExtr:obExtrs <> distExtrs
             , allObjs
             , allArrs
             , obeM
             )
        TAFAgg tabAggs ->
          let extrs = concatMap (fetchExtrFromAggFld . snd) tabAggs
              (obExtrs, ordByObjs, ordByArrs, obeM)
                      = mkOrdByItems' emptyArrRelCtx
          in ( HM.fromList $ extrs <> obExtrs <> distExtrs
             , ordByObjs
             , ordByArrs
             , obeM
             )
        TAFExp _ ->
          let (obExtrs, ordByObjs, ordByArrs, obeM)
                      = mkOrdByItems' emptyArrRelCtx
          in (HM.fromList obExtrs, ordByObjs, ordByArrs, obeM)

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

    mkArrRelCtx arrSels = ArrRelCtx arrSels aggOrdByRelNames

    mkOrdByItems' = mkOrdByItems pfx fldAls orderByM

    distItemsM = processDistinctOnCol pfx <$> distM
    distExprM = fst <$> distItemsM
    distExtrs = fromMaybe [] (snd <$> distItemsM)

    -- process an object relationship
    mkObjItem (fld, objSel) =
      let relName = aarName objSel
          objNodePfx = mkObjRelTableAls pfx $ aarName objSel
          objNode = mkObjNode objNodePfx (fld, objSel)
      in (relName, objNode)

    -- process an array/array-aggregate item
    mkArrItem arrRelCtx (fld, arrSel) =
      let (arrAls, arrPfx) = mkArrNodePfx pfx fldAls arrRelCtx $
                             ANIField (fld, arrSel)
          arrNode = mkArrNode arrPfx (fld, arrSel)
      in (arrAls, arrNode)

    getAnnObj (f, annFld) = case annFld of
      FObj ob -> Just (f, ob)
      _       -> Nothing

    getAnnArr (f, annFld) = case annFld of
      FArr ar -> Just (f, ar)
      _       -> Nothing

annSelToBaseNode :: Iden -> FieldName -> AnnSel -> BaseNode
annSelToBaseNode pfx fldAls annSel =
  mkBaseNode pfx fldAls (TAFNodes selFlds) tabFrm tabPerm tabArgs
  where
    AnnSelG selFlds tabFrm tabPerm tabArgs = annSel

mkObjNode :: Iden -> (FieldName, ObjSel) -> ObjNode
mkObjNode pfx (fldName, AnnRelG _ rMapn rAnnSel) =
  ObjNode rMapn $ annSelToBaseNode pfx fldName rAnnSel

mkArrNode :: Iden -> (FieldName, ArrSel) -> ArrNode
mkArrNode pfx (fldName, annArrSel) = case annArrSel of
  ASSimple annArrRel ->
    let bn = annSelToBaseNode pfx fldName $ aarAnnSel annArrRel
        extr = asJsonAggExtr False (S.toAlias fldName) $
               _bnOrderBy bn
    in ArrNode [extr] (aarMapping annArrRel) bn

  ASAgg annAggSel -> aggSelToArrNode pfx fldName annAggSel

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
             offsetM extrs objRels arrRels
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
                 map objNodeToFromItem (HM.elems objRels) <>
                 map arrNodeToFromItem (HM.elems arrRels)

    objNodeToFromItem :: ObjNode -> S.FromItem
    objNodeToFromItem (ObjNode relMapn relBaseNode) =
      let als = S.Alias $ _bnPrefix relBaseNode
          sel = baseNodeToSel (mkJoinCond baseSelAls relMapn) relBaseNode
      in S.mkLateralFromItem sel als

    arrNodeToFromItem :: ArrNode -> S.FromItem
    arrNodeToFromItem (ArrNode es colMapn bn) =
      let sel = arrNodeToSelect bn es (mkJoinCond baseSelAls colMapn)
          als = S.Alias $ _bnPrefix bn
      in S.mkLateralFromItem sel als

mkAggSelect :: AnnAggSel -> S.Select
mkAggSelect annAggSel =
  prefixNumToAliases $ arrNodeToSelect bn extr $ S.BELit True
  where
    aggSel = AnnRelG (RelName "root") [] annAggSel
    ArrNode extr _ bn =
      aggSelToArrNode (Iden "root") (FieldName "root") aggSel

mkSQLSelect :: Bool -> AnnSel -> S.Select
mkSQLSelect isSingleObject annSel =
  prefixNumToAliases $ arrNodeToSelect baseNode extrs $ S.BELit True
  where
    extrs = pure $ asJsonAggExtr isSingleObject rootFldAls $ _bnOrderBy baseNode
    baseNode = annSelToBaseNode (toIden rootFldName) rootFldName annSel
    rootFldName = FieldName "root"
    rootFldAls  = S.Alias $ toIden rootFldName
