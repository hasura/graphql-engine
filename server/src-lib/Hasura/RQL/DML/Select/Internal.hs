module Hasura.RQL.DML.Select.Internal
  ( mkSQLSelect
  , mkAggSelect
  , module Hasura.RQL.DML.Select.Types
  )
where

import           Control.Lens                hiding (op)
import qualified Data.List                   as L
import           Instances.TH.Lift           ()

import qualified Data.HashMap.Strict         as HM
import qualified Data.List.NonEmpty          as NE
import qualified Data.Text                   as T

import           Hasura.Prelude
import           Hasura.RQL.DML.Internal
import           Hasura.RQL.DML.Select.Types
import           Hasura.RQL.GBoolExp
import           Hasura.RQL.Types
import           Hasura.SQL.Rewrite          (prefixNumToAliases)
import           Hasura.SQL.Types

import qualified Hasura.SQL.DML              as S

-- Conversion of SelectQ happens in 2 Stages.
-- Stage 1 : Convert input query into an annotated AST
-- Stage 2 : Convert annotated AST to SQL Select

functionToIden :: QualifiedFunction -> Iden
functionToIden = Iden . qualObjectToText

selFromToFromItem :: Iden -> SelectFrom -> S.FromItem
selFromToFromItem pfx = \case
  FromTable tn -> S.FISimple tn Nothing
  FromIden i   -> S.FIIden i
  FromFunction qf args defListM ->
    S.FIFunc $ S.FunctionExp qf (fromTableRowArgs pfx args) $
    Just $ S.mkFunctionAlias (functionToIden qf) defListM

-- This function shouldn't be present ideally
-- You should be able to retrieve this information
-- from the FromItem generated with selFromToFromItem
-- however given from S.FromItem is modelled, it is not
-- possible currently
selFromToQual :: SelectFrom -> S.Qual
selFromToQual = \case
  FromTable tn         -> S.QualTable tn
  FromIden i           -> S.QualIden i Nothing
  FromFunction qf _ _  -> S.QualIden (functionToIden qf) Nothing

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

asSingleRowExtr :: S.Alias -> S.SQLExp
asSingleRowExtr col =
  S.SEFnApp "coalesce" [jsonAgg, S.SELit "null"] Nothing
  where
    jsonAgg = S.SEOpApp (S.SQLOp "->")
              [ S.SEFnApp "json_agg" [S.SEIden $ toIden col] Nothing
              , S.SEUnsafe "0"
              ]

withJsonAggExtr
  :: Bool -> Maybe Int -> Maybe S.OrderByExp -> S.Alias -> S.SQLExp
withJsonAggExtr subQueryReq permLimitM ordBy alias =
  -- if select has aggregations then use subquery to apply permission limit
  if subQueryReq then maybe simpleJsonAgg withPermLimit permLimitM
  else simpleJsonAgg
  where
    simpleJsonAgg = mkSimpleJsonAgg rowIdenExp ordBy
    rowIdenExp = S.SEIden $ S.getAlias alias
    subSelAls = Iden "sub_query"
    unnestTable = Iden "unnest_table"

    mkSimpleJsonAgg rowExp ob =
      let jsonAggExp = S.SEFnApp "json_agg" [rowExp] ob
      in S.SEFnApp "coalesce" [jsonAggExp, S.SELit "[]"] Nothing

    withPermLimit limit =
      let subSelect = mkSubSelect limit
          rowIden = S.mkQIdenExp subSelAls alias
          extr = S.Extractor (mkSimpleJsonAgg rowIden newOrdBy) Nothing
          fromExp = S.FromExp $ pure $
                    S.mkSelFromItem subSelect $ S.Alias subSelAls
      in S.SESelect $ S.mkSelect { S.selExtr = pure extr
                                 , S.selFrom = Just fromExp
                                 }

    mkSubSelect limit =
      let jsonRowExtr = flip S.Extractor (Just alias) $
                        S.mkQIdenExp unnestTable alias
          obExtrs = flip map newOBAliases $ \a ->
                    S.Extractor (S.mkQIdenExp unnestTable a) $ Just $ S.Alias a
      in S.mkSelect { S.selExtr    = jsonRowExtr : obExtrs
                    , S.selFrom    = Just $ S.FromExp $ pure unnestFromItem
                    , S.selLimit   = Just $ S.LimitExp $ S.intToSQLExp limit
                    , S.selOrderBy = newOrdBy
                    }

    unnestFromItem =
      let arrayAggItems = flip map (rowIdenExp : obCols) $
                          \s -> S.SEFnApp "array_agg" [s] Nothing
      in S.FIUnnest arrayAggItems (S.Alias unnestTable) $
         rowIdenExp : map S.SEIden newOBAliases

    newOrdBy = bool (Just $ S.OrderByExp newOBItems) Nothing $ null newOBItems

    (newOBItems, obCols, newOBAliases) = maybe ([], [], []) transformOrdBy ordBy
    transformOrdBy (S.OrderByExp l) = unzip3 $
      flip map (zip l [1..]) $ \(obItem, i::Int) ->
                 let iden = Iden $ "ob_col_" <> T.pack (show i)
                 in ( obItem{S.oColumn = S.SEIden iden}
                    , S.oColumn obItem
                    , iden
                    )

asJsonAggExtr
  :: JsonAggSelect -> S.Alias -> Bool -> Maybe Int -> Maybe S.OrderByExp -> S.Extractor
asJsonAggExtr jsonAggSelect als subQueryReq permLimit ordByExpM =
  flip S.Extractor (Just als) $ case jsonAggSelect of
    JASMultipleRows -> withJsonAggExtr subQueryReq permLimit ordByExpM als
    JASSingleObject -> asSingleRowExtr als

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

mkComputedFieldTableAls :: Iden -> FieldName -> Iden
mkComputedFieldTableAls pfx fldAls =
  pfx <> Iden ".cf." <> toIden fldAls

mkBaseTableAls :: Iden -> Iden
mkBaseTableAls pfx =
  pfx <> Iden ".base"

mkBaseTableColAls :: Iden -> PGCol -> Iden
mkBaseTableColAls pfx pgColumn =
  pfx <> Iden ".pg." <> toIden pgColumn

mkOrderByFieldName :: RelName -> FieldName
mkOrderByFieldName relName =
  FieldName $ relNameToTxt relName <> "." <> "order_by"

fromTableRowArgs
  :: Iden -> FunctionArgsExpTableRow S.SQLExp -> S.FunctionArgs
fromTableRowArgs pfx = toFunctionArgs . fmap toSQLExp
  where
    toFunctionArgs (FunctionArgsExp positional named) =
      S.FunctionArgs positional named
    toSQLExp (AETableRow Nothing)    = S.SERowIden $ mkBaseTableAls pfx
    toSQLExp (AETableRow (Just acc)) = S.mkQIdenExp (mkBaseTableAls pfx) acc
    toSQLExp (AEInput s)             = s

-- posttgres ignores anything beyond 63 chars for an iden
-- in this case, we'll need to use json_build_object function
-- json_build_object is slower than row_to_json hence it is only
-- used when needed
buildJsonObject
  :: Iden -> FieldName -> ArrRelCtx -> Bool
  -> [(FieldName, AnnFld)] -> (S.Alias, S.SQLExp)
buildJsonObject pfx parAls arrRelCtx strfyNum flds =
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
      FCol c      -> toSQLCol c
      FExp e      -> S.SELit e
      FObj objSel ->
        let qual = mkObjRelTableAls pfx $ aarName objSel
        in S.mkQIdenExp qual fldAls
      FArr arrSel      ->
        let arrPfx = _aniPrefix $ mkArrNodeInfo pfx parAls arrRelCtx $
                                  ANIField (fldAls, arrSel)
        in S.mkQIdenExp arrPfx fldAls
      FComputedField (CFSScalar computedFieldScalar) ->
        fromScalarComputedField computedFieldScalar
      FComputedField (CFSTable _ _) ->
        let ccPfx = mkComputedFieldTableAls pfx fldAls
        in S.mkQIdenExp ccPfx fldAls
      FRemote _ -> S.SELit "null: remote field selected"

    toSQLCol :: AnnColField -> S.SQLExp
    toSQLCol (AnnColField col asText colOpM) =
      toJSONableExp strfyNum (pgiType col) asText $ withColOp colOpM $
      S.mkQIdenExp (mkBaseTableAls pfx) $ pgiColumn col

    fromScalarComputedField :: ComputedFieldScalarSel S.SQLExp -> S.SQLExp
    fromScalarComputedField computedFieldScalar =
      toJSONableExp strfyNum (PGColumnScalar ty) False $ withColOp colOpM $
      S.SEFunction $ S.FunctionExp fn (fromTableRowArgs pfx args) Nothing
      where
        ComputedFieldScalarSel fn args ty colOpM = computedFieldScalar

    withColOp :: Maybe ColOp -> S.SQLExp -> S.SQLExp
    withColOp colOpM sqlExp = case colOpM of
      Nothing              -> sqlExp
      Just (ColOp op cExp) -> S.mkSQLOpExp op sqlExp cExp

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
  AAOOp op pgColumn ->
    ( S.Extractor (S.SEFnApp op [S.SEIden $ toIden pgColumn] Nothing) als
    , [(FieldName op, AFOp $ AggOp op [(fromPGCol pgColumn, PCFCol pgColumn)])]
    )
  where
    als = Just $ S.toAlias $ mkAggObFld annAggOb

processAnnOrderByItem
  :: Iden
  -> FieldName
  -> ArrRelCtx
  -> Bool
  -> AnnOrderByItem
       -- the extractors which will select the needed columns
  -> ( (S.Alias, S.SQLExp)
       -- the sql order by item that is attached to the final select
     , S.OrderByItem
       -- extra nodes for order by
     , OrderByNode
     )
processAnnOrderByItem pfx parAls arrRelCtx strfyNum obItemG =
  ( (obColAls, obColExp)
  , sqlOrdByItem
  , relNodeM
  )
  where
    OrderByItemG obTyM annObCol obNullsM = obItemG
    ((obColAls, obColExp), relNodeM) =
      processAnnOrderByCol pfx parAls arrRelCtx strfyNum annObCol

    sqlOrdByItem =
      S.OrderByItem (S.SEIden $ toIden obColAls)
      (unOrderType <$> obTyM) (unNullsOrder <$> obNullsM)

processAnnOrderByCol
  :: Iden
  -> FieldName
  -> ArrRelCtx
  -> Bool
  -> AnnObCol
       -- the extractors which will select the needed columns
  -> ( (S.Alias, S.SQLExp)
       -- extra nodes for order by
     , OrderByNode
     )
processAnnOrderByCol pfx parAls arrRelCtx strfyNum = \case
  AOCPG pgColumn ->
    let
      qualCol  = S.mkQIdenExp (mkBaseTableAls pfx) (toIden pgColumn)
      obColAls = mkBaseTableColAls pfx pgColumn
    in ( (S.Alias obColAls, qualCol)
       , OBNNothing
       )
  -- "pfx.or.relname"."pfx.ob.or.relname.rest" AS "pfx.ob.or.relname.rest"
  AOCObj (RelInfo rn _ colMapping relTab _ _) relFltr rest ->
    let relPfx  = mkObjRelTableAls pfx rn
        ordByFldName = mkOrderByFieldName rn
        ((nesAls, nesCol), ordByNode) =
          processAnnOrderByCol relPfx ordByFldName emptyArrRelCtx strfyNum rest
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
          HM.empty
        relNode = ObjNode colMapping relBaseNode
    in ( (nesAls, qualCol)
       , OBNObjNode rn relNode
       )
  AOCAgg (RelInfo rn _ colMapping relTab _ _) relFltr annAggOb ->
    let ArrNodeInfo arrAls arrPfx _ =
          mkArrNodeInfo pfx parAls arrRelCtx $ ANIAggOrdBy rn
        fldName = mkAggObFld annAggOb
        qOrdBy = S.mkQIdenExp arrPfx $ toIden fldName
        tabFrom = FromTable relTab
        tabPerm = TablePerm relFltr Nothing
        (extr, arrFlds) = mkAggObExtrAndFlds annAggOb
        selFld = TAFAgg arrFlds
        bn = mkBaseNode False (Prefixes arrPfx pfx) fldName selFld tabFrom
             tabPerm noTableArgs strfyNum
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


mkEmptyBaseNode :: Iden -> SelectFrom -> BaseNode
mkEmptyBaseNode pfx selectFrom =
  BaseNode pfx Nothing fromItem (S.BELit True) Nothing Nothing
  Nothing selOne HM.empty HM.empty HM.empty
  where
    selOne = HM.singleton (S.Alias $ pfx <> Iden "__one") (S.SEUnsafe "1")
    fromItem = selFromToFromItem pfx selectFrom

aggSelToArrNode :: Prefixes -> FieldName -> ArrRelAgg -> ArrNode
aggSelToArrNode pfxs als aggSel =
  ArrNode [extr] colMapping mergedBN
  where
    AnnRelG _ colMapping annSel = aggSel
    AnnSelG aggFlds tabFrm tabPerm tabArgs strfyNum = annSel
    fldAls = S.Alias $ toIden als

    extr = flip S.Extractor (Just fldAls) $ S.applyJsonBuildObj $
           concatMap selFldToExtr aggFlds

    permLimit = _tpLimit tabPerm
    ordBy = _bnOrderBy mergedBN

    allBNs = map mkAggBaseNode aggFlds
    emptyBN = mkEmptyBaseNode (_pfThis pfxs) tabFrm
    mergedBN = foldr mergeBaseNodes emptyBN allBNs

    mkAggBaseNode (fn, selFld) =
      mkBaseNode subQueryReq pfxs fn selFld tabFrm tabPerm tabArgs strfyNum

    selFldToExtr (FieldName t, fld) = (:) (S.SELit t) $ pure $ case fld of
      TAFAgg flds -> aggFldToExp flds
      TAFNodes _  ->
        withJsonAggExtr subQueryReq permLimit ordBy $ S.Alias $ Iden t
      TAFExp e    ->
        -- bool_or to force aggregation
        S.SEFnApp "coalesce"
        [ S.SELit e , S.SEUnsafe "bool_or('true')::text"] Nothing

    subQueryReq = hasAggFld aggFlds

hasAggFld :: Foldable t => t (a, TableAggFldG v) -> Bool
hasAggFld = any (isTabAggFld . snd)
  where
    isTabAggFld (TAFAgg _) = True
    isTabAggFld _          = False

mkArrNodeInfo
  :: Iden
  -> FieldName
  -> ArrRelCtx
  -> ArrNodeItem
  -> ArrNodeInfo
mkArrNodeInfo pfx parAls (ArrRelCtx arrFlds obRels) = \case
  ANIField aggFld@(fld, annArrSel) ->
    let (rn, tabArgs) = fetchRNAndTArgs annArrSel
        similarFlds = getSimilarAggFlds rn tabArgs $ L.delete aggFld
        similarFldNames = map fst similarFlds
        similarOrdByFound = rn `elem` obRels && tabArgs == noTableArgs
        ordByFldName = mkOrderByFieldName rn
        extraOrdByFlds = bool [] [ordByFldName] similarOrdByFound
        sortedFlds = L.sort $ fld : (similarFldNames <> extraOrdByFlds)
        alias = S.Alias $ mkUniqArrRelAls parAls sortedFlds
        prefix = mkArrRelTableAls pfx parAls sortedFlds
    in ArrNodeInfo alias prefix $
       subQueryRequired similarFlds similarOrdByFound
  ANIAggOrdBy rn ->
    let similarFlds = map fst $ getSimilarAggFlds rn noTableArgs id
        ordByFldName = mkOrderByFieldName rn
        sortedFlds = L.sort $ ordByFldName:similarFlds
        alias = S.Alias $ mkUniqArrRelAls parAls sortedFlds
        prefix = mkArrRelTableAls pfx parAls sortedFlds
    in ArrNodeInfo alias prefix False
  where
    getSimilarAggFlds rn tabArgs f =
      flip filter (f arrFlds) $ \(_, annArrSel) ->
        let (lrn, lTabArgs) = fetchRNAndTArgs annArrSel
        in (lrn == rn) && (lTabArgs == tabArgs)

    subQueryRequired similarFlds hasSimOrdBy =
      hasSimOrdBy || any hasAgg similarFlds

    hasAgg (_, ASSimple _)                 = False
    hasAgg (_, ASAgg (AnnRelG _ _ annSel)) = hasAggFld $ _asnFields annSel

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
  -> Bool
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
mkOrdByItems pfx fldAls orderByM strfyNum arrRelCtx =
  (obExtrs, ordByObjsMap, ordByArrsMap, ordByExpM)
  where
    procAnnOrdBy' = processAnnOrderByItem pfx fldAls arrRelCtx strfyNum
    procOrdByM =
      unzip3 . map procAnnOrdBy' . toList <$> orderByM

    obExtrs  = maybe [] (^. _1) procOrdByM
    ordByExpM  = S.OrderByExp . (^. _2) <$> procOrdByM

    ordByObjs = mapMaybe getOrdByRelNode $ maybe [] (^. _3) procOrdByM
    ordByObjsMap = HM.fromListWith mergeObjNodes ordByObjs

    ordByAggArrs = mapMaybe getOrdByAggNode $ maybe [] (^. _3) procOrdByM
    ordByArrsMap = HM.fromListWith mergeArrNodes ordByAggArrs

    getOrdByRelNode (OBNObjNode name node) = Just (name, node)
    getOrdByRelNode _                      = Nothing

    getOrdByAggNode (OBNArrNode als node) = Just (als, node)
    getOrdByAggNode _                     = Nothing

mkBaseNode
  :: Bool
  -> Prefixes
  -> FieldName
  -> TableAggFld
  -> SelectFrom
  -> TablePerm
  -> TableArgs
  -> Bool
  -> BaseNode
mkBaseNode subQueryReq pfxs fldAls annSelFlds selectFrom
           tablePerm tableArgs strfyNum =
  BaseNode thisPfx distExprM fromItem finalWhere ordByExpM finalLimit offsetM
  allExtrs allObjsWithOb allArrsWithOb computedFields
  where
    Prefixes thisPfx baseTablepfx = pfxs
    TablePerm permFilter permLimit = tablePerm
    TableArgs whereM orderByM inpLimitM offsetM distM = tableArgs

    -- if sub query is used, then only use input limit
    --    because permission limit is being applied in subquery
    -- else compare input and permission limits
    finalLimit =
      if subQueryReq then inpLimitM
      else withPermLimit

    withPermLimit =
      case (inpLimitM, permLimit) of
        (inpLim, Nothing)     -> inpLim
        (Nothing, permLim)    -> permLim
        (Just inp, Just perm) -> Just $ if inp < perm then inp else perm


    aggOrdByRelNames = fetchOrdByAggRels orderByM

    (allExtrs, allObjsWithOb, allArrsWithOb, computedFields, ordByExpM) =
      case annSelFlds of
        TAFNodes flds ->
          let arrFlds = mapMaybe getAnnArr flds
              arrRelCtx = mkArrRelCtx arrFlds
              selExtr = buildJsonObject thisPfx fldAls arrRelCtx strfyNum flds
              -- all object relationships
              objNodes = HM.fromListWith mergeObjNodes $
                        map mkObjItem (mapMaybe getAnnObj flds)
              -- all array items (array relationships + aggregates)
              arrNodes = HM.fromListWith mergeArrNodes $
                         map (mkArrItem arrRelCtx) arrFlds
              -- all computed fields with table returns
              computedFieldNodes = HM.fromList $ map mkComputedFieldTable $
                                   mapMaybe getComputedFieldTable flds

              (obExtrs, ordByObjs, ordByArrs, obeM)
                      = mkOrdByItems' arrRelCtx
              allObjs = HM.unionWith mergeObjNodes objNodes ordByObjs
              allArrs = HM.unionWith mergeArrNodes arrNodes ordByArrs

          in ( HM.fromList $ selExtr:obExtrs <> distExtrs
             , allObjs
             , allArrs
             , computedFieldNodes
             , obeM
             )
        TAFAgg tabAggs ->
          let extrs = concatMap (fetchExtrFromAggFld . snd) tabAggs
              (obExtrs, ordByObjs, ordByArrs, obeM)
                      = mkOrdByItems' emptyArrRelCtx
          in ( HM.fromList $ extrs <> obExtrs <> distExtrs
             , ordByObjs
             , ordByArrs
             , HM.empty
             , obeM
             )
        TAFExp _ ->
          let (obExtrs, ordByObjs, ordByArrs, obeM)
                      = mkOrdByItems' emptyArrRelCtx
          in (HM.fromList obExtrs, ordByObjs, ordByArrs, HM.empty, obeM)

    fetchExtrFromAggFld (AFCount cty) = countTyToExps cty
    fetchExtrFromAggFld (AFOp aggOp)  = aggOpToExps aggOp
    fetchExtrFromAggFld (AFExp _)     = []

    countTyToExps S.CTStar            = []
    countTyToExps (S.CTSimple cols)   = colsToExps cols
    countTyToExps (S.CTDistinct cols) = colsToExps cols

    colsToExps = mapMaybe (mkColExp . PCFCol)

    aggOpToExps = mapMaybe (mkColExp . snd) . _aoFlds

    mkColExp (PCFCol c) =
      let qualCol = S.mkQIdenExp (mkBaseTableAls thisPfx) (toIden c)
          colAls = toIden c
      in Just (S.Alias colAls, qualCol)
    mkColExp _ = Nothing

    finalWhere = toSQLBoolExp tableQual $
                 maybe permFilter (andAnnBoolExps permFilter) whereM
    fromItem = selFromToFromItem baseTablepfx selectFrom
    tableQual = selFromToQual selectFrom

    mkArrRelCtx arrSels = ArrRelCtx arrSels aggOrdByRelNames

    mkOrdByItems' = mkOrdByItems thisPfx fldAls orderByM strfyNum

    distItemsM = processDistinctOnCol thisPfx <$> distM
    distExprM = fst <$> distItemsM
    distExtrs = maybe [] snd distItemsM

    -- process an object relationship
    mkObjItem (fld, objSel) =
      let relName = aarName objSel
          objNodePfx = mkObjRelTableAls thisPfx $ aarName objSel
          objNode = mkObjNode (Prefixes objNodePfx thisPfx) (fld, objSel)
      in (relName, objNode)

    -- process an array/array-aggregate item
    mkArrItem arrRelCtx (fld, arrSel) =
      let ArrNodeInfo arrAls arrPfx subQReq =
            mkArrNodeInfo thisPfx fldAls arrRelCtx $ ANIField (fld, arrSel)
          arrNode = mkArrNode subQReq (Prefixes arrPfx thisPfx) (fld, arrSel)
      in (arrAls, arrNode)

    -- process a computed field, which returns a table
    mkComputedFieldTable (fld, jsonAggSelect, sel) =
      let prefixes = Prefixes (mkComputedFieldTableAls thisPfx fld) thisPfx
          baseNode = annSelToBaseNode False prefixes fld sel
      in (fld, CFTableNode jsonAggSelect baseNode)

    getAnnObj (f, annFld) = case annFld of
      FObj ob -> Just (f, ob)
      _       -> Nothing

    getAnnArr (f, annFld) = case annFld of
      FArr ar -> Just (f, ar)
      _       -> Nothing

    getComputedFieldTable (f, annFld) = case annFld of
      FComputedField (CFSTable jas sel) -> Just (f, jas, sel)
      _                                 -> Nothing

annSelToBaseNode :: Bool -> Prefixes -> FieldName -> AnnSimpleSel -> BaseNode
annSelToBaseNode subQueryReq pfxs fldAls annSel =
  mkBaseNode subQueryReq pfxs fldAls (TAFNodes selFlds) tabFrm tabPerm tabArgs strfyNum
  where
    AnnSelG selFlds tabFrm tabPerm tabArgs strfyNum = annSel

mkObjNode :: Prefixes -> (FieldName, ObjSel) -> ObjNode
mkObjNode pfxs (fldName, AnnRelG _ rMapn rAnnSel) =
  ObjNode rMapn $ annSelToBaseNode False pfxs fldName rAnnSel

mkArrNode :: Bool -> Prefixes -> (FieldName, ArrSel) -> ArrNode
mkArrNode subQueryReq pfxs (fldName, annArrSel) = case annArrSel of
  ASSimple annArrRel ->
    let bn = annSelToBaseNode subQueryReq pfxs fldName $ aarAnnSel annArrRel
        permLimit = getPermLimit $ aarAnnSel annArrRel
        extr = asJsonAggExtr JASMultipleRows (S.toAlias fldName) subQueryReq permLimit $
               _bnOrderBy bn
    in ArrNode [extr] (aarMapping annArrRel) bn

  ASAgg annAggSel -> aggSelToArrNode pfxs fldName annAggSel

injectJoinCond :: S.BoolExp       -- ^ Join condition
               -> S.BoolExp -- ^ Where condition
               -> S.WhereFrag     -- ^ New where frag
injectJoinCond joinCond whereCond =
  S.WhereFrag $ S.simplifyBoolExp $ S.BEBin S.AndOp joinCond whereCond

mkJoinCond :: S.Alias -> HashMap PGCol PGCol -> S.BoolExp
mkJoinCond baseTablepfx colMapn =
  foldl' (S.BEBin S.AndOp) (S.BELit True) $ flip map (HM.toList colMapn) $ \(lCol, rCol) ->
    S.BECompare S.SEQ (S.mkQIdenExp baseTablepfx lCol) (S.mkSIdenExp rCol)

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
             offsetM extrs objRels arrRels computedFields
             = baseNode
    -- this is the table which is aliased as "pfx.base"
    baseSel = S.mkSelect
      { S.selExtr  = [S.Extractor (S.SEStar Nothing) Nothing]
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
                 map arrNodeToFromItem (HM.elems arrRels) <>
                 map computedFieldNodeToFromItem (HM.toList computedFields)

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

    computedFieldNodeToFromItem :: (FieldName, CFTableNode) -> S.FromItem
    computedFieldNodeToFromItem (fld, CFTableNode jsonAggSelect bn) =
      let internalSel = baseNodeToSel (S.BELit True) bn
          als = S.Alias $ _bnPrefix bn
          extr = asJsonAggExtr jsonAggSelect (S.toAlias fld) False Nothing $
                 _bnOrderBy bn
          internalSelFrom = S.mkSelFromItem internalSel als
          sel = S.mkSelect
                { S.selExtr = pure extr
                , S.selFrom = Just $ S.FromExp [internalSelFrom]
                }
      in S.mkLateralFromItem sel als

mkAggSelect :: AnnAggSel -> S.Select
mkAggSelect annAggSel =
  prefixNumToAliases $ arrNodeToSelect bn extr $ S.BELit True
  where
    aggSel = AnnRelG rootRelName HM.empty annAggSel
    rootIden = Iden "root"
    rootPrefix = Prefixes rootIden rootIden
    ArrNode extr _ bn =
      aggSelToArrNode rootPrefix (FieldName "root") aggSel

mkSQLSelect :: JsonAggSelect -> AnnSimpleSel -> S.Select
mkSQLSelect jsonAggSelect annSel =
  prefixNumToAliases $ arrNodeToSelect baseNode extrs $ S.BELit True
  where
    permLimit = getPermLimit annSel
    extrs = pure $ asJsonAggExtr jsonAggSelect rootFldAls False permLimit
            $ _bnOrderBy baseNode
    rootFldIden = toIden rootFldName
    rootPrefix = Prefixes rootFldIden rootFldIden
    baseNode = annSelToBaseNode False rootPrefix rootFldName annSel
    rootFldName = FieldName "root"
    rootFldAls  = S.Alias $ toIden rootFldName
