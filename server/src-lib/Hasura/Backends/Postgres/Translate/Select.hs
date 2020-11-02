module Hasura.Backends.Postgres.Translate.Select
  ( selectQuerySQL
  , selectAggregateQuerySQL
  , connectionSelectQuerySQL
  , asSingleRowJsonResp
  , mkSQLSelect
  , mkAggregateSelect
  , mkConnectionSelect
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict                        as HM
import qualified Data.List.NonEmpty                         as NE
import qualified Data.Text                                  as T
import qualified Database.PG.Query                          as Q

import           Control.Lens                               hiding (op)
import           Control.Monad.Writer.Strict
import           Data.Text.Extended
import           Instances.TH.Lift                          ()

import qualified Hasura.Backends.Postgres.SQL.DML           as S

import           Hasura.Backends.Postgres.SQL.Rewrite
import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.Backends.Postgres.Translate.BoolExp
import           Hasura.EncJSON
import           Hasura.GraphQL.Schema.Common
import           Hasura.RQL.DML.Internal
import           Hasura.RQL.IR.Select
import           Hasura.RQL.Types
import           Hasura.SQL.Types


selectQuerySQL :: JsonAggSelect -> AnnSimpleSel 'Postgres -> Q.Query
selectQuerySQL jsonAggSelect sel =
  Q.fromBuilder $ toSQL $ mkSQLSelect jsonAggSelect sel

selectAggregateQuerySQL :: AnnAggregateSelect 'Postgres -> Q.Query
selectAggregateQuerySQL =
  Q.fromBuilder . toSQL . mkAggregateSelect

connectionSelectQuerySQL :: ConnectionSelect 'Postgres S.SQLExp -> Q.Query
connectionSelectQuerySQL =
  Q.fromBuilder . toSQL . mkConnectionSelect

asSingleRowJsonResp :: Q.Query -> [Q.PrepArg] -> Q.TxE QErr EncJSON
asSingleRowJsonResp query args =
  encJFromBS . runIdentity . Q.getRow
  <$> Q.rawQE dmlTxErrorHandler query args True


-- Conversion of SelectQ happens in 2 Stages.
-- Stage 1 : Convert input query into an annotated AST
-- Stage 2 : Convert annotated AST to SQL Select

functionToIdentifier :: QualifiedFunction -> Identifier
functionToIdentifier = Identifier . qualifiedObjectToText

selectFromToFromItem :: Identifier -> SelectFrom 'Postgres -> S.FromItem
selectFromToFromItem pfx = \case
  FromTable tn -> S.FISimple tn Nothing
  FromIdentifier i   -> S.FIIdentifier i
  FromFunction qf args defListM ->
    S.FIFunc $ S.FunctionExp qf (fromTableRowArgs pfx args) $
    Just $ S.mkFunctionAlias (functionToIdentifier qf) defListM

-- This function shouldn't be present ideally
-- You should be able to retrieve this information
-- from the FromItem generated with selectFromToFromItem
-- however given from S.FromItem is modelled, it is not
-- possible currently
selectFromToQual :: SelectFrom backend -> S.Qual
selectFromToQual = \case
  FromTable tn        -> S.QualTable tn
  FromIdentifier i    -> S.QualifiedIdentifier i Nothing
  FromFunction qf _ _ -> S.QualifiedIdentifier (functionToIdentifier qf) Nothing

aggregateFieldToExp :: AggregateFields 'Postgres -> S.SQLExp
aggregateFieldToExp aggFlds = jsonRow
  where
    jsonRow = S.applyJsonBuildObj (concatMap aggToFlds aggFlds)
    withAls fldName sqlExp = [S.SELit fldName, sqlExp]
    aggToFlds (FieldName t, fld) = withAls t $ case fld of
      AFCount cty -> S.SECount cty
      AFOp aggOp  -> aggOpToObj aggOp
      AFExp e     -> S.SELit e

    aggOpToObj (AggregateOp opText flds) =
      S.applyJsonBuildObj $ concatMap (colFldsToExtr opText) flds

    colFldsToExtr opText (FieldName t, CFCol col) =
      [ S.SELit t
      , S.SEFnApp opText [S.SEIdentifier $ toIdentifier col] Nothing
      ]
    colFldsToExtr _ (FieldName t, CFExp e) =
      [ S.SELit t , S.SELit e]

asSingleRowExtr :: S.Alias -> S.SQLExp
asSingleRowExtr col =
  S.SEFnApp "coalesce" [jsonAgg, S.SELit "null"] Nothing
  where
    jsonAgg = S.SEOpApp (S.SQLOp "->")
              [ S.SEFnApp "json_agg" [S.SEIdentifier $ toIdentifier col] Nothing
              , S.SEUnsafe "0"
              ]

withJsonAggExtr
  :: PermissionLimitSubQuery -> Maybe S.OrderByExp -> S.Alias -> S.SQLExp
withJsonAggExtr permLimitSubQuery ordBy alias =
  -- if select has aggregations then use subquery to apply permission limit
  case permLimitSubQuery of
    PLSQRequired permLimit -> withPermLimit permLimit
    PLSQNotRequired        -> simpleJsonAgg
  where
    simpleJsonAgg = mkSimpleJsonAgg rowIdenExp ordBy
    rowIdenExp = S.SEIdentifier $ S.getAlias alias
    subSelAls = Identifier "sub_query"
    unnestTable = Identifier "unnest_table"

    mkSimpleJsonAgg rowExp ob =
      let jsonAggExp = S.SEFnApp "json_agg" [rowExp] ob
      in S.SEFnApp "coalesce" [jsonAggExp, S.SELit "[]"] Nothing

    withPermLimit limit =
      let subSelect = mkSubSelect limit
          rowIdentifier = S.mkQIdenExp subSelAls alias
          extr = S.Extractor (mkSimpleJsonAgg rowIdentifier newOrderBy) Nothing
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
                    , S.selOrderBy = newOrderBy
                    }

    unnestFromItem =
      let arrayAggItems = flip map (rowIdenExp : obCols) $
                          \s -> S.SEFnApp "array_agg" [s] Nothing
      in S.FIUnnest arrayAggItems (S.Alias unnestTable) $
         rowIdenExp : map S.SEIdentifier newOBAliases

    newOrderBy = S.OrderByExp <$> NE.nonEmpty newOBItems

    (newOBItems, obCols, newOBAliases) = maybe ([], [], []) transformOrderBy ordBy
    transformOrderBy (S.OrderByExp l) = unzip3 $
      flip map (zip (toList l) [1..]) $ \(obItem, i::Int) ->
                 let iden = Identifier $ "ob_col_" <> T.pack (show i)
                 in ( obItem{S.oColumn = S.SEIdentifier iden}
                    , S.oColumn obItem
                    , iden
                    )

asJsonAggExtr
  :: JsonAggSelect -> S.Alias -> PermissionLimitSubQuery -> Maybe S.OrderByExp -> S.Extractor
asJsonAggExtr jsonAggSelect als permLimitSubQuery ordByExpM =
  flip S.Extractor (Just als) $ case jsonAggSelect of
    JASMultipleRows -> withJsonAggExtr permLimitSubQuery ordByExpM als
    JASSingleObject -> asSingleRowExtr als

-- array relationships are not grouped, so have to be prefixed by
-- parent's alias
mkUniqArrayRelationAlias :: FieldName -> [FieldName] -> Identifier
mkUniqArrayRelationAlias parAls flds =
  let sortedFields = sort flds
  in Identifier $
     getFieldNameTxt parAls <> "."
     <> T.intercalate "." (map getFieldNameTxt sortedFields)

mkArrayRelationTableAlias :: Identifier -> FieldName -> [FieldName] -> Identifier
mkArrayRelationTableAlias pfx parAls flds =
  pfx <> Identifier ".ar." <> uniqArrRelAls
  where
    uniqArrRelAls = mkUniqArrayRelationAlias parAls flds

mkObjectRelationTableAlias :: Identifier -> RelName -> Identifier
mkObjectRelationTableAlias pfx relName =
  pfx <> Identifier ".or." <> toIdentifier relName

mkComputedFieldTableAlias :: Identifier -> FieldName -> Identifier
mkComputedFieldTableAlias pfx fldAls =
  pfx <> Identifier ".cf." <> toIdentifier fldAls

mkBaseTableAlias :: Identifier -> Identifier
mkBaseTableAlias pfx =
  pfx <> Identifier ".base"

mkBaseTableColumnAlias :: Identifier -> PGCol -> Identifier
mkBaseTableColumnAlias pfx pgColumn =
  pfx <> Identifier ".pg." <> toIdentifier pgColumn

mkOrderByFieldName :: RelName -> FieldName
mkOrderByFieldName relName =
  FieldName $ relNameToTxt relName <> "." <> "order_by"

mkAggregateOrderByAlias :: AnnAggregateOrderBy 'Postgres -> S.Alias
mkAggregateOrderByAlias = (S.Alias . Identifier) . \case
  AAOCount         -> "count"
  AAOOp opText col -> opText <> "." <> getPGColTxt (pgiColumn col)

mkArrayRelationSourcePrefix
  :: Identifier
  -> FieldName
  -> HM.HashMap FieldName [FieldName]
  -> FieldName
  -> Identifier
mkArrayRelationSourcePrefix parentSourcePrefix parentFieldName similarFieldsMap fieldName =
  mkArrayRelationTableAlias parentSourcePrefix parentFieldName $
  HM.lookupDefault [fieldName] fieldName similarFieldsMap

mkArrayRelationAlias
  :: FieldName
  -> HM.HashMap FieldName [FieldName]
  -> FieldName
  -> S.Alias
mkArrayRelationAlias parentFieldName similarFieldsMap fieldName =
  S.Alias $ mkUniqArrayRelationAlias parentFieldName $
  HM.lookupDefault [fieldName] fieldName similarFieldsMap

fromTableRowArgs
  :: Identifier -> FunctionArgsExpTableRow S.SQLExp -> S.FunctionArgs
fromTableRowArgs pfx = toFunctionArgs . fmap toSQLExp
  where
    toFunctionArgs (FunctionArgsExp positional named) =
      S.FunctionArgs positional named
    toSQLExp (AETableRow Nothing)    = S.SERowIdentifier $ mkBaseTableAlias pfx
    toSQLExp (AETableRow (Just acc)) = S.mkQIdenExp (mkBaseTableAlias pfx) acc
    toSQLExp (AESession s)           = s
    toSQLExp (AEInput s)             = s

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

-- | Forces aggregation
withForceAggregation :: S.TypeAnn -> S.SQLExp -> S.SQLExp
withForceAggregation tyAnn e =
  -- bool_or to force aggregation
  S.SEFnApp "coalesce" [e, S.SETyAnn (S.SEUnsafe "bool_or('true')") tyAnn] Nothing

mkAggregateOrderByExtractorAndFields
  :: AnnAggregateOrderBy 'Postgres -> (S.Extractor, AggregateFields 'Postgres)
mkAggregateOrderByExtractorAndFields annAggOrderBy =
  case annAggOrderBy of
    AAOCount       ->
      ( S.Extractor S.countStar alias
      , [(FieldName "count", AFCount S.CTStar)]
      )
    AAOOp opText pgColumnInfo ->
      let pgColumn = pgiColumn pgColumnInfo
      in ( S.Extractor (S.SEFnApp opText [S.SEIdentifier $ toIdentifier pgColumn] Nothing) alias
         , [(FieldName opText, AFOp $ AggregateOp opText [(fromPGCol pgColumn, CFCol pgColumn)])]
         )
  where
    alias = Just $ mkAggregateOrderByAlias annAggOrderBy

mkAnnOrderByAlias
  :: Identifier -> FieldName -> SimilarArrayFields -> AnnOrderByElementG 'Postgres v -> S.Alias
mkAnnOrderByAlias pfx parAls similarFields = \case
  AOCColumn pgColumnInfo ->
    let pgColumn = pgiColumn pgColumnInfo
        obColAls = mkBaseTableColumnAlias pfx pgColumn
    in S.Alias obColAls
  -- "pfx.or.relname"."pfx.ob.or.relname.rest" AS "pfx.ob.or.relname.rest"
  AOCObjectRelation relInfo _ rest ->
    let rn = riName relInfo
        relPfx  = mkObjectRelationTableAlias pfx rn
        ordByFldName = mkOrderByFieldName rn
        nesAls = mkAnnOrderByAlias relPfx ordByFldName mempty rest
    in nesAls
  AOCArrayAggregation relInfo _ aggOrderBy ->
    let rn = riName relInfo
        arrPfx = mkArrayRelationSourcePrefix pfx parAls similarFields $
                 mkOrderByFieldName rn
        obAls = arrPfx <> Identifier "." <> toIdentifier (mkAggregateOrderByAlias aggOrderBy)
    in S.Alias obAls

processDistinctOnColumns
  :: Identifier
  -> NE.NonEmpty PGCol
  -> ( S.DistinctExpr
     , [(S.Alias, S.SQLExp)] -- additional column extractors
     )
processDistinctOnColumns pfx neCols = (distOnExp, colExtrs)
  where
    cols = toList neCols
    distOnExp = S.DistinctOn $ map (S.SEIdentifier . toIdentifier . mkQColAls) cols
    mkQCol c = S.mkQIdenExp (mkBaseTableAlias pfx) $ toIdentifier c
    mkQColAls = S.Alias . mkBaseTableColumnAlias pfx
    colExtrs = flip map cols $ mkQColAls &&& mkQCol

type SimilarArrayFields = HM.HashMap FieldName [FieldName]

mkSimilarArrayFields
  :: Eq v
  => AnnFieldsG 'Postgres v
  -> Maybe (NE.NonEmpty (AnnOrderByItemG 'Postgres v))
  -> SimilarArrayFields
mkSimilarArrayFields annFields maybeOrderBys =
  HM.fromList $ flip map allTuples $
  \(relNameAndArgs, fieldName) -> (fieldName, getSimilarFields relNameAndArgs)
  where
    getSimilarFields relNameAndArgs = map snd $ filter ((== relNameAndArgs) . fst) allTuples
    allTuples = arrayRelationTuples <> aggOrderByRelationTuples
    arrayRelationTuples =
      let arrayFields = mapMaybe getAnnArr annFields
      in flip map arrayFields $
         \(f, relSel) -> (getArrayRelNameAndSelectArgs relSel, f)

    aggOrderByRelationTuples =
      let mkItem (relName, fieldName) = ( (relName, noSelectArgs)
                                        , fieldName
                                        )
      in map mkItem $ maybe []
         (mapMaybe (fetchAggOrderByRels . obiColumn) . toList) maybeOrderBys

    fetchAggOrderByRels (AOCArrayAggregation ri _ _) =
      Just (riName ri, mkOrderByFieldName $ riName ri)
    fetchAggOrderByRels _               = Nothing

getArrayRelNameAndSelectArgs :: ArraySelectG backend v -> (RelName, SelectArgsG backend v)
getArrayRelNameAndSelectArgs = \case
  ASSimple r     -> (aarRelationshipName r, _asnArgs $ aarAnnSelect r)
  ASAggregate r  -> (aarRelationshipName r, _asnArgs $ aarAnnSelect r)
  ASConnection r -> (aarRelationshipName r, _asnArgs $ _csSelect $ aarAnnSelect r)

getAnnArr :: (a, AnnFieldG backend v) -> Maybe (a, ArraySelectG backend v)
getAnnArr (f, annFld) = case annFld of
  AFArrayRelation (ASConnection _) -> Nothing
  AFArrayRelation ar               -> Just (f, ar)
  _                                -> Nothing


withWriteJoinTree
  :: (MonadWriter (JoinTree backend) m)
  => (JoinTree backend -> b -> JoinTree backend)
  -> m (a, b)
  -> m a
withWriteJoinTree joinTreeUpdater action =
  pass $ do
    (out, result) <- action
    let fromJoinTree joinTree =
          joinTreeUpdater joinTree result
    pure (out, fromJoinTree)

withWriteObjectRelation
  :: (MonadWriter (JoinTree backend) m, Hashable (ObjectRelationSource backend))
  => m ( ObjectRelationSource backend
       , HM.HashMap S.Alias S.SQLExp
       , a
       )
  -> m a
withWriteObjectRelation action =
  withWriteJoinTree updateJoinTree $ do
    (source, nodeExtractors, out) <- action
    pure (out, (source, nodeExtractors))
  where
    updateJoinTree joinTree (source, nodeExtractors) =
      let selectNode = SelectNode nodeExtractors joinTree
      in mempty{_jtObjectRelations = HM.singleton source selectNode}

withWriteArrayRelation
  :: (MonadWriter (JoinTree 'Postgres) m)
  => m ( ArrayRelationSource 'Postgres
       , S.Extractor
       , HM.HashMap S.Alias S.SQLExp
       , a
       )
  -> m a
withWriteArrayRelation action =
  withWriteJoinTree updateJoinTree $ do
  (source, topExtractor, nodeExtractors, out) <- action
  pure (out, (source, topExtractor, nodeExtractors))
  where
    updateJoinTree joinTree (source, topExtractor, nodeExtractors) =
      let arraySelectNode = ArraySelectNode [topExtractor] $
                            SelectNode nodeExtractors joinTree
      in mempty{_jtArrayRelations = HM.singleton source arraySelectNode}

withWriteArrayConnection
  :: (MonadWriter (JoinTree 'Postgres) m)
  => m ( ArrayConnectionSource 'Postgres
       , S.Extractor
       , HM.HashMap S.Alias S.SQLExp
       , a
       )
  -> m a
withWriteArrayConnection action =
  withWriteJoinTree updateJoinTree $ do
  (source, topExtractor, nodeExtractors, out) <- action
  pure (out, (source, topExtractor, nodeExtractors))
  where
    updateJoinTree joinTree (source, topExtractor, nodeExtractors) =
      let arraySelectNode = ArraySelectNode [topExtractor] $
                            SelectNode nodeExtractors joinTree
      in mempty{_jtArrayConnections = HM.singleton source arraySelectNode}

withWriteComputedFieldTableSet
  :: (MonadWriter (JoinTree backend) m)
  => m ( ComputedFieldTableSetSource
       , HM.HashMap S.Alias S.SQLExp
       , a
       )
  -> m a
withWriteComputedFieldTableSet action =
  withWriteJoinTree updateJoinTree $ do
    (source, nodeExtractors, out) <- action
    pure (out, (source, nodeExtractors))
  where
    updateJoinTree joinTree (source, nodeExtractors) =
      let selectNode = SelectNode nodeExtractors joinTree
      in mempty{_jtComputedFieldTableSets = HM.singleton source selectNode}


processAnnSimpleSelect
  :: forall m . ( MonadReader Bool m
               , MonadWriter (JoinTree 'Postgres) m
               )
  => SourcePrefixes
  -> FieldName
  -> PermissionLimitSubQuery
  -> AnnSimpleSel 'Postgres
  -> m ( SelectSource
       , HM.HashMap S.Alias S.SQLExp
       )
processAnnSimpleSelect sourcePrefixes fieldAlias permLimitSubQuery annSimpleSel = do
  (selectSource, orderByAndDistinctExtrs, _) <-
    processSelectParams sourcePrefixes fieldAlias similarArrayFields tableFrom
    permLimitSubQuery tablePermissions tableArgs
  annFieldsExtr <- processAnnFields (_pfThis sourcePrefixes) fieldAlias similarArrayFields annSelFields
  let allExtractors = HM.fromList $ annFieldsExtr : orderByAndDistinctExtrs
  pure (selectSource, allExtractors)
  where
    AnnSelectG annSelFields tableFrom tablePermissions tableArgs _ = annSimpleSel
    similarArrayFields =
      mkSimilarArrayFields annSelFields $ _saOrderBy tableArgs

processAnnAggregateSelect
  :: forall m. ( MonadReader Bool m
               , MonadWriter (JoinTree 'Postgres) m
               )
  => SourcePrefixes
  -> FieldName
  -> AnnAggregateSelect 'Postgres
  -> m ( SelectSource
       , HM.HashMap S.Alias S.SQLExp
       , S.Extractor
       )
processAnnAggregateSelect sourcePrefixes fieldAlias annAggSel = do
  (selectSource, orderByAndDistinctExtrs, _) <-
    processSelectParams sourcePrefixes fieldAlias similarArrayFields tableFrom
    permLimitSubQuery tablePermissions tableArgs
  let thisSourcePrefix = _pfThis sourcePrefixes
  processedFields <- forM aggSelFields $ \(fieldName, field) ->
    (fieldName,) <$>
    case field of
      TAFAgg aggFields ->
        pure ( aggregateFieldsToExtractorExps thisSourcePrefix aggFields
             , aggregateFieldToExp aggFields
             )
      TAFNodes annFields -> do
        annFieldExtr <- processAnnFields thisSourcePrefix fieldName similarArrayFields annFields
        pure ( [annFieldExtr]
             , withJsonAggExtr permLimitSubQuery (_ssOrderBy selectSource) $
               S.Alias $ toIdentifier fieldName
             )
      TAFExp e ->
        pure ( []
             , withForceAggregation S.textTypeAnn $ S.SELit e
             )

  let topLevelExtractor =
        flip S.Extractor (Just $ S.Alias $ toIdentifier fieldAlias) $
        S.applyJsonBuildObj $ flip concatMap (map (second snd) processedFields) $
        \(FieldName fieldText, fieldExp) -> [S.SELit fieldText, fieldExp]
      nodeExtractors = HM.fromList $
        concatMap (fst . snd) processedFields <> orderByAndDistinctExtrs

  pure (selectSource, nodeExtractors, topLevelExtractor)
  where
    AnnSelectG aggSelFields tableFrom tablePermissions tableArgs _ = annAggSel
    permLimit = _tpLimit tablePermissions
    orderBy = _saOrderBy tableArgs
    permLimitSubQuery = mkPermissionLimitSubQuery permLimit aggSelFields orderBy
    similarArrayFields = HM.unions $
      flip map (map snd aggSelFields) $ \case
        TAFAgg _ -> mempty
        TAFNodes annFlds ->
          mkSimilarArrayFields annFlds orderBy
        TAFExp _ -> mempty

mkPermissionLimitSubQuery
  :: Maybe Int
  -> TableAggregateFields backend
  -> Maybe (NE.NonEmpty (AnnOrderByItem backend))
  -> PermissionLimitSubQuery
mkPermissionLimitSubQuery permLimit aggFields orderBys =
  case permLimit of
    Nothing -> PLSQNotRequired
    Just limit ->
      if hasAggregateField || hasAggOrderBy then PLSQRequired limit
      else PLSQNotRequired
  where
    hasAggregateField = flip any (map snd aggFields) $
      \case
        TAFAgg _ -> True
        _        -> False

    hasAggOrderBy = case orderBys of
      Nothing -> False
      Just l -> flip any (concatMap toList $ toList l) $
                \case
                  AOCArrayAggregation{} -> True
                  _                     -> False

processArrayRelation
  :: forall m. ( MonadReader Bool m
               , MonadWriter (JoinTree 'Postgres) m
               )
  => SourcePrefixes
  -> FieldName
  -> S.Alias
  -> ArraySelect 'Postgres
  -> m ()
processArrayRelation sourcePrefixes fieldAlias relAlias arrSel =
  case arrSel of
    ASSimple annArrRel -> withWriteArrayRelation $ do
      let AnnRelationSelectG _ colMapping sel = annArrRel
          permLimitSubQuery =
            maybe PLSQNotRequired PLSQRequired $ _tpLimit $ _asnPerm sel
      (source, nodeExtractors) <-
        processAnnSimpleSelect sourcePrefixes fieldAlias permLimitSubQuery sel
      let topExtr = asJsonAggExtr JASMultipleRows (S.toAlias fieldAlias)
                    permLimitSubQuery $ _ssOrderBy source
      pure ( ArrayRelationSource relAlias colMapping source
           , topExtr
           , nodeExtractors
           , ()
           )
    ASAggregate aggSel -> withWriteArrayRelation $ do
      let AnnRelationSelectG _ colMapping sel = aggSel
      (source, nodeExtractors, topExtr) <-
        processAnnAggregateSelect sourcePrefixes fieldAlias sel
      pure ( ArrayRelationSource relAlias colMapping source
           , topExtr
           , nodeExtractors
           , ()
           )
    ASConnection connSel -> withWriteArrayConnection $ do
      let AnnRelationSelectG _ colMapping sel = connSel
      (source, topExtractor, nodeExtractors) <-
        processConnectionSelect sourcePrefixes fieldAlias relAlias colMapping sel
      pure ( source
           , topExtractor
           , nodeExtractors
           , ()
           )

processSelectParams
  :: forall m. ( MonadReader Bool m
               , MonadWriter (JoinTree 'Postgres) m
               )
  => SourcePrefixes
  -> FieldName
  -> SimilarArrayFields
  -> SelectFrom 'Postgres
  -> PermissionLimitSubQuery
  -> TablePerm 'Postgres
  -> SelectArgs 'Postgres
  -> m ( SelectSource
       , [(S.Alias, S.SQLExp)]
       , Maybe S.SQLExp -- Order by cursor
       )
processSelectParams sourcePrefixes fieldAlias similarArrFields selectFrom
                    permLimitSubQ tablePermissions tableArgs = do
  maybeOrderBy <- mapM
                  (processOrderByItems thisSourcePrefix fieldAlias similarArrFields)
                  orderByM
  let fromItem = selectFromToFromItem (_pfBase sourcePrefixes) selectFrom
      (maybeDistinct, distinctExtrs) =
        maybe (Nothing, []) (first Just) $ processDistinctOnColumns thisSourcePrefix <$> distM
      finalWhere = toSQLBoolExp (selectFromToQual selectFrom) $
                   maybe permFilter (andAnnBoolExps permFilter) whereM
      selectSource = SelectSource thisSourcePrefix fromItem maybeDistinct finalWhere
                     ((^. _2) <$> maybeOrderBy) finalLimit offsetM
      orderByExtrs = maybe [] (^. _1) maybeOrderBy
  pure ( selectSource
       , orderByExtrs <> distinctExtrs
       , (^. _3) <$> maybeOrderBy
       )
  where
    thisSourcePrefix = _pfThis sourcePrefixes
    SelectArgs whereM orderByM inpLimitM offsetM distM = tableArgs
    TablePerm permFilter permLimit = tablePermissions
    finalLimit =
    -- if sub query is required, then only use input limit
    --    because permission limit is being applied in subquery
    -- else compare input and permission limits
      case permLimitSubQ of
        PLSQRequired _  -> inpLimitM
        PLSQNotRequired -> compareLimits

    compareLimits =
      case (inpLimitM, permLimit) of
        (inpLim, Nothing)     -> inpLim
        (Nothing, permLim)    -> permLim
        (Just inp, Just perm) -> Just $ if inp < perm then inp else perm

processOrderByItems
  :: forall m. ( MonadReader Bool m
               , MonadWriter (JoinTree 'Postgres) m
               )
  => Identifier
  -> FieldName
  -> SimilarArrayFields
  -> NE.NonEmpty (AnnOrderByItem 'Postgres)
  -> m ( [(S.Alias, S.SQLExp)] -- Order by Extractors
       , S.OrderByExp
       , S.SQLExp -- The cursor expression
       )
processOrderByItems sourcePrefix' fieldAlias' similarArrayFields orderByItems = do
  orderByItemExps <- forM orderByItems processAnnOrderByItem
  let orderByExp = S.OrderByExp $ toOrderByExp <$> orderByItemExps
      orderByExtractors = concat $ toList $ map snd . toList <$> orderByItemExps
      cursor = mkCursorExp $ toList orderByItemExps
  pure (orderByExtractors, orderByExp, cursor)
  where
    processAnnOrderByItem :: AnnOrderByItem 'Postgres -> m (OrderByItemExp 'Postgres)
    processAnnOrderByItem orderByItem =
      forM orderByItem $ \ordByCol -> (ordByCol,) <$>
      processAnnOrderByElement sourcePrefix' fieldAlias' ordByCol

    processAnnOrderByElement
      :: Identifier -> FieldName -> AnnOrderByElement 'Postgres S.SQLExp -> m (S.Alias, S.SQLExp)
    processAnnOrderByElement sourcePrefix fieldAlias annObCol = do
      let ordByAlias = mkAnnOrderByAlias sourcePrefix fieldAlias similarArrayFields annObCol
      (ordByAlias, ) <$> case annObCol of
        AOCColumn pgColInfo -> pure $
          S.mkQIdenExp (mkBaseTableAlias sourcePrefix) $ toIdentifier $ pgiColumn pgColInfo

        AOCObjectRelation relInfo relFilter rest -> withWriteObjectRelation $ do
          let RelInfo relName _ colMapping relTable _ _ = relInfo
              relSourcePrefix = mkObjectRelationTableAlias sourcePrefix relName
              fieldName = mkOrderByFieldName relName
          (relOrderByAlias, relOrdByExp) <-
            processAnnOrderByElement relSourcePrefix fieldName rest
          let selectSource = ObjectSelectSource relSourcePrefix
                             (S.FISimple relTable Nothing)
                             (toSQLBoolExp (S.QualTable relTable) relFilter)
              relSource = ObjectRelationSource relName colMapping selectSource
          pure ( relSource
               , HM.singleton relOrderByAlias relOrdByExp
               , S.mkQIdenExp relSourcePrefix relOrderByAlias
               )

        AOCArrayAggregation relInfo relFilter aggOrderBy -> withWriteArrayRelation $ do
          let RelInfo relName _ colMapping relTable _ _ = relInfo
              fieldName = mkOrderByFieldName relName
              relSourcePrefix = mkArrayRelationSourcePrefix sourcePrefix fieldAlias
                                similarArrayFields fieldName
              relAlias = mkArrayRelationAlias fieldAlias similarArrayFields fieldName
              (topExtractor, fields) = mkAggregateOrderByExtractorAndFields aggOrderBy
              selectSource = SelectSource relSourcePrefix
                             (S.FISimple relTable Nothing) Nothing
                             (toSQLBoolExp (S.QualTable relTable) relFilter)
                             Nothing Nothing Nothing
              relSource = ArrayRelationSource relAlias colMapping selectSource
          pure ( relSource
               , topExtractor
               , HM.fromList $ aggregateFieldsToExtractorExps relSourcePrefix fields
               , S.mkQIdenExp relSourcePrefix (mkAggregateOrderByAlias aggOrderBy)
               )

    toOrderByExp :: OrderByItemExp 'Postgres -> S.OrderByItem
    toOrderByExp orderByItemExp =
      let OrderByItemG obTyM expAlias obNullsM = fst . snd <$> orderByItemExp
      in S.OrderByItem (S.SEIdentifier $ toIdentifier expAlias)
         (unOrderType <$> obTyM) (unNullsOrder <$> obNullsM)

    mkCursorExp :: [OrderByItemExp 'Postgres] -> S.SQLExp
    mkCursorExp orderByItemExps =
      S.applyJsonBuildObj $ flip concatMap orderByItemExps $
      \orderByItemExp ->
        let OrderByItemG _ (annObCol, (_, valExp)) _ = orderByItemExp
        in annObColToJSONField valExp annObCol
      where
        annObColToJSONField valExp = \case
          AOCColumn pgCol -> [S.SELit $ getPGColTxt $ pgiColumn pgCol, valExp]
          AOCObjectRelation relInfo _ obCol ->
            [ S.SELit $ relNameToTxt $ riName relInfo
            , S.applyJsonBuildObj $ annObColToJSONField valExp obCol
            ]
          AOCArrayAggregation relInfo _ aggOrderBy ->
            [ S.SELit $ relNameToTxt (riName relInfo) <> "_aggregate"
            , S.applyJsonBuildObj $
              case aggOrderBy of
                AAOCount -> [S.SELit "count", valExp]
                AAOOp opText colInfo ->
                  [ S.SELit opText
                  , S.applyJsonBuildObj [S.SELit $ getPGColTxt $ pgiColumn colInfo, valExp]
                  ]
            ]

aggregateFieldsToExtractorExps
  :: Identifier -> AggregateFields 'Postgres -> [(S.Alias, S.SQLExp)]
aggregateFieldsToExtractorExps sourcePrefix aggregateFields =
  flip concatMap aggregateFields $ \(_, field) ->
    case field of
      AFCount cty -> case cty of
        S.CTStar          -> []
        S.CTSimple cols   -> colsToExps cols
        S.CTDistinct cols -> colsToExps cols
      AFOp aggOp  -> aggOpToExps aggOp
      AFExp _     -> []
  where
    colsToExps = mapMaybe (mkColExp . CFCol)
    aggOpToExps = mapMaybe (mkColExp . snd) . _aoFields

    mkColExp (CFCol c) =
      let qualCol = S.mkQIdenExp (mkBaseTableAlias sourcePrefix) (toIdentifier c)
          colAls = toIdentifier c
      in Just (S.Alias colAls, qualCol)
    mkColExp _ = Nothing

processAnnFields
  :: forall m . ( MonadReader Bool m
               , MonadWriter (JoinTree 'Postgres) m
               )
  => Identifier
  -> FieldName
  -> SimilarArrayFields
  -> AnnFields 'Postgres
  -> m (S.Alias, S.SQLExp)
processAnnFields sourcePrefix fieldAlias similarArrFields annFields = do
  fieldExps <- forM annFields $ \(fieldName, field) ->
    (fieldName,) <$>
    case field of
      AFExpression t -> pure $ S.SELit t

      AFNodeId tn pKeys -> pure $ mkNodeId tn pKeys

      AFColumn c -> toSQLCol c

      AFRemote _ -> pure $ S.SELit "null: remote field selected"

      AFObjectRelation objSel -> withWriteObjectRelation $ do
        let AnnRelationSelectG relName relMapping annObjSel = objSel
            AnnObjectSelectG objAnnFields tableFrom tableFilter = annObjSel
            objRelSourcePrefix = mkObjectRelationTableAlias sourcePrefix relName
            sourcePrefixes = mkSourcePrefixes objRelSourcePrefix
        annFieldsExtr <- processAnnFields (_pfThis sourcePrefixes) fieldName HM.empty objAnnFields
        let selectSource = ObjectSelectSource (_pfThis sourcePrefixes)
                           (S.FISimple tableFrom Nothing)
                           (toSQLBoolExp (S.QualTable tableFrom) tableFilter)
            objRelSource = ObjectRelationSource relName relMapping selectSource
        pure ( objRelSource
             , HM.fromList [annFieldsExtr]
             , S.mkQIdenExp objRelSourcePrefix fieldName
             )

      AFArrayRelation arrSel -> do
        let arrRelSourcePrefix = mkArrayRelationSourcePrefix sourcePrefix fieldAlias similarArrFields fieldName
            arrRelAlias = mkArrayRelationAlias fieldAlias similarArrFields fieldName
        processArrayRelation (mkSourcePrefixes arrRelSourcePrefix) fieldName arrRelAlias arrSel
        pure $ S.mkQIdenExp arrRelSourcePrefix fieldName

      AFComputedField (CFSScalar scalar) -> fromScalarComputedField scalar

      AFComputedField (CFSTable selectTy sel) -> withWriteComputedFieldTableSet $ do
        let computedFieldSourcePrefix =
              mkComputedFieldTableAlias sourcePrefix fieldName
        (selectSource, nodeExtractors) <-
          processAnnSimpleSelect (mkSourcePrefixes computedFieldSourcePrefix)
          fieldName PLSQNotRequired sel
        let computedFieldTableSetSource =
              ComputedFieldTableSetSource fieldName selectTy selectSource
        pure ( computedFieldTableSetSource
             , nodeExtractors
             , S.mkQIdenExp computedFieldSourcePrefix fieldName
             )

  pure $
    -- posttgres ignores anything beyond 63 chars for an iden
    -- in this case, we'll need to use json_build_object function
    -- json_build_object is slower than row_to_json hence it is only
    -- used when needed
    if any ( (> 63) . T.length . getFieldNameTxt . fst ) fieldExps then
      withJsonBuildObj fieldAlias $ concatMap toJsonBuildObjectExps fieldExps
    else withRowToJSON fieldAlias $ map toRowToJsonExtr fieldExps
  where
    mkSourcePrefixes newPrefix = SourcePrefixes newPrefix sourcePrefix
    toJsonBuildObjectExps (fieldName, fieldExp) =
      [S.SELit $ getFieldNameTxt fieldName, fieldExp]

    toRowToJsonExtr (fieldName, fieldExp) =
      S.Extractor fieldExp $ Just $ S.toAlias fieldName

    toSQLCol :: AnnColumnField 'Postgres -> m S.SQLExp
    toSQLCol (AnnColumnField col asText colOpM) = do
      strfyNum <- ask
      pure $ toJSONableExp strfyNum (pgiType col) asText $ withColumnOp colOpM $
             S.mkQIdenExp (mkBaseTableAlias sourcePrefix) $ pgiColumn col

    fromScalarComputedField :: ComputedFieldScalarSelect S.SQLExp -> m S.SQLExp
    fromScalarComputedField computedFieldScalar = do
      strfyNum <- ask
      pure $ toJSONableExp strfyNum (PGColumnScalar ty) False $ withColumnOp colOpM $
             S.SEFunction $ S.FunctionExp fn (fromTableRowArgs sourcePrefix args) Nothing
      where
        ComputedFieldScalarSelect fn args ty colOpM = computedFieldScalar

    withColumnOp :: Maybe ColumnOp -> S.SQLExp -> S.SQLExp
    withColumnOp colOpM sqlExp = case colOpM of
      Nothing                     -> sqlExp
      Just (ColumnOp opText cExp) -> S.mkSQLOpExp opText sqlExp cExp

    mkNodeId :: QualifiedTable -> PrimaryKeyColumns 'Postgres -> S.SQLExp
    mkNodeId (QualifiedObject tableSchema tableName) pkeyColumns =
      let columnInfoToSQLExp pgColumnInfo =
            toJSONableExp False (pgiType pgColumnInfo) False $
            S.mkQIdenExp (mkBaseTableAlias sourcePrefix) $ pgiColumn pgColumnInfo

      -- See Note [Relay Node id].
      in encodeBase64 $ flip S.SETyAnn S.textTypeAnn $ S.applyJsonBuildArray $
         [ S.intToSQLExp $ nodeIdVersionInt currentNodeIdVersion
         , S.SELit (getSchemaTxt tableSchema)
         , S.SELit (toTxt tableName)
         ] <> map columnInfoToSQLExp (toList pkeyColumns)

injectJoinCond :: S.BoolExp       -- ^ Join condition
               -> S.BoolExp -- ^ Where condition
               -> S.WhereFrag     -- ^ New where frag
injectJoinCond joinCond whereCond =
  S.WhereFrag $ S.simplifyBoolExp $ S.BEBin S.AndOp joinCond whereCond

mkJoinCond :: S.Alias -> HashMap PGCol PGCol -> S.BoolExp
mkJoinCond baseTablepfx colMapn =
  foldl' (S.BEBin S.AndOp) (S.BELit True) $ flip map (HM.toList colMapn) $ \(lCol, rCol) ->
    S.BECompare S.SEQ (S.mkQIdenExp baseTablepfx lCol) (S.mkSIdenExp rCol)

generateSQLSelect
  :: S.BoolExp -- ^ Pre join condition
  -> SelectSource
  -> SelectNode 'Postgres
  -> S.Select
generateSQLSelect joinCondition selectSource selectNode =
  S.mkSelect
  { S.selExtr = [S.Extractor e $ Just a | (a, e) <- HM.toList extractors]
  , S.selFrom = Just $ S.FromExp [joinedFrom]
  , S.selOrderBy = maybeOrderby
  , S.selLimit = S.LimitExp . S.intToSQLExp <$> maybeLimit
  , S.selOffset = S.OffsetExp <$> maybeOffset
  , S.selDistinct = maybeDistinct
  }
  where
    SelectSource sourcePrefix fromItem maybeDistinct whereExp
      maybeOrderby maybeLimit maybeOffset = selectSource
    SelectNode extractors joinTree = selectNode
    JoinTree objectRelations arrayRelations arrayConnections computedFields = joinTree
    -- this is the table which is aliased as "sourcePrefix.base"
    baseSelect = S.mkSelect
      { S.selExtr  = [S.Extractor (S.SEStar Nothing) Nothing]
      , S.selFrom  = Just $ S.FromExp [fromItem]
      , S.selWhere = Just $ injectJoinCond joinCondition whereExp
      }
    baseSelectAlias = S.Alias $ mkBaseTableAlias sourcePrefix
    baseFromItem = S.mkSelFromItem baseSelect baseSelectAlias

    -- function to create a joined from item from two from items
    leftOuterJoin current new =
      S.FIJoin $ S.JoinExpr current S.LeftOuter new $
      S.JoinOn $ S.BELit True

    -- this is the from eexp for the final select
    joinedFrom :: S.FromItem
    joinedFrom = foldl' leftOuterJoin baseFromItem $
                 map objectRelationToFromItem (HM.toList objectRelations) <>
                 map arrayRelationToFromItem (HM.toList arrayRelations) <>
                 map arrayConnectionToFromItem (HM.toList arrayConnections) <>
                 map computedFieldToFromItem (HM.toList computedFields)


    objectRelationToFromItem
      :: (ObjectRelationSource 'Postgres, SelectNode 'Postgres) -> S.FromItem
    objectRelationToFromItem (objectRelationSource, node) =
      let ObjectRelationSource _ colMapping objectSelectSource = objectRelationSource
          alias = S.Alias $ _ossPrefix objectSelectSource
          source = objectSelectSourceToSelectSource objectSelectSource
          select = generateSQLSelect (mkJoinCond baseSelectAlias colMapping) source node
      in S.mkLateralFromItem select alias

    arrayRelationToFromItem
      :: (ArrayRelationSource 'Postgres, ArraySelectNode 'Postgres) -> S.FromItem
    arrayRelationToFromItem (arrayRelationSource, arraySelectNode) =
      let ArrayRelationSource _ colMapping source = arrayRelationSource
          alias = S.Alias $ _ssPrefix source
          select = generateSQLSelectFromArrayNode source arraySelectNode $
                   mkJoinCond baseSelectAlias colMapping
      in S.mkLateralFromItem select alias

    arrayConnectionToFromItem
      :: (ArrayConnectionSource 'Postgres, ArraySelectNode 'Postgres) -> S.FromItem
    arrayConnectionToFromItem (arrayConnectionSource, arraySelectNode) =
      let selectWith = connectionToSelectWith baseSelectAlias arrayConnectionSource arraySelectNode
          alias = S.Alias $ _ssPrefix $ _acsSource arrayConnectionSource
      in S.FISelectWith (S.Lateral True) selectWith alias

    computedFieldToFromItem
      :: (ComputedFieldTableSetSource, SelectNode 'Postgres) -> S.FromItem
    computedFieldToFromItem (computedFieldTableSource, node) =
      let ComputedFieldTableSetSource fieldName selectTy source = computedFieldTableSource
          internalSelect = generateSQLSelect (S.BELit True) source node
          extractor = asJsonAggExtr selectTy (S.toAlias fieldName) PLSQNotRequired $
                      _ssOrderBy source
          alias = S.Alias $ _ssPrefix source
          select = S.mkSelect
                { S.selExtr = [extractor]
                , S.selFrom = Just $ S.FromExp [S.mkSelFromItem internalSelect alias]
                }
      in S.mkLateralFromItem select alias

generateSQLSelectFromArrayNode
  :: SelectSource
  -> ArraySelectNode 'Postgres
  -> S.BoolExp
  -> S.Select
generateSQLSelectFromArrayNode selectSource arraySelectNode joinCondition =
  S.mkSelect
  { S.selExtr = topExtractors
  , S.selFrom = Just $ S.FromExp [selectFrom]
  }
  where
    ArraySelectNode topExtractors selectNode = arraySelectNode
    selectFrom = S.mkSelFromItem
                 (generateSQLSelect joinCondition selectSource selectNode) $
                 S.Alias $ _ssPrefix selectSource

mkAggregateSelect :: AnnAggregateSelect 'Postgres -> S.Select
mkAggregateSelect annAggSel =
  let ((selectSource, nodeExtractors, topExtractor), joinTree) =
        runWriter $ flip runReaderT strfyNum $
        processAnnAggregateSelect sourcePrefixes rootFieldName annAggSel
      selectNode = SelectNode nodeExtractors joinTree
      arrayNode = ArraySelectNode [topExtractor] selectNode
  in prefixNumToAliases $
     generateSQLSelectFromArrayNode selectSource arrayNode $ S.BELit True
  where
    strfyNum = _asnStrfyNum annAggSel
    rootFieldName = FieldName "root"
    rootIdentifier = toIdentifier rootFieldName
    sourcePrefixes = SourcePrefixes rootIdentifier rootIdentifier

mkSQLSelect :: JsonAggSelect -> AnnSimpleSel 'Postgres -> S.Select
mkSQLSelect jsonAggSelect annSel =
  let permLimitSubQuery = PLSQNotRequired
      ((selectSource, nodeExtractors), joinTree) =
        runWriter $ flip runReaderT strfyNum $
        processAnnSimpleSelect sourcePrefixes rootFldName permLimitSubQuery annSel
      selectNode = SelectNode nodeExtractors joinTree
      topExtractor = asJsonAggExtr jsonAggSelect rootFldAls permLimitSubQuery
                     $ _ssOrderBy selectSource
      arrayNode = ArraySelectNode [topExtractor] selectNode
  in prefixNumToAliases $
     generateSQLSelectFromArrayNode selectSource arrayNode $ S.BELit True
  where
    strfyNum = _asnStrfyNum annSel
    rootFldIdentifier = toIdentifier rootFldName
    sourcePrefixes = SourcePrefixes rootFldIdentifier rootFldIdentifier
    rootFldName = FieldName "root"
    rootFldAls  = S.Alias $ toIdentifier rootFldName

mkConnectionSelect :: ConnectionSelect 'Postgres S.SQLExp -> S.SelectWithG S.Select
mkConnectionSelect connectionSelect =
  let ((connectionSource, topExtractor, nodeExtractors), joinTree) =
        runWriter $ flip runReaderT strfyNum $
        processConnectionSelect sourcePrefixes rootFieldName
        (S.Alias rootIdentifier) mempty connectionSelect
      selectNode = ArraySelectNode [topExtractor] $
                       SelectNode nodeExtractors joinTree
  in prefixNumToAliasesSelectWith $
     connectionToSelectWith (S.Alias rootIdentifier) connectionSource selectNode
  where
    strfyNum = _asnStrfyNum $ _csSelect connectionSelect
    rootFieldName = FieldName "root"
    rootIdentifier = toIdentifier rootFieldName
    sourcePrefixes = SourcePrefixes rootIdentifier rootIdentifier

-- | First element extractor expression from given record set
-- For example:- To get first "id" column from given row set,
-- the function generates the SQL expression AS `(array_agg("id"))[1]`
mkFirstElementExp :: S.SQLExp -> S.SQLExp
mkFirstElementExp expIdentifier =
  -- For Example
  S.SEArrayIndex (S.SEFnApp "array_agg" [expIdentifier] Nothing) (S.intToSQLExp 1)

-- | Last element extractor expression from given record set.
-- For example:- To get first "id" column from given row set,
-- the function generates the SQL expression AS `(array_agg("id"))[array_length(array_agg("id"), 1)]`
mkLastElementExp :: S.SQLExp -> S.SQLExp
mkLastElementExp expIdentifier =
  let arrayExp = S.SEFnApp "array_agg" [expIdentifier] Nothing
  in S.SEArrayIndex arrayExp $
     S.SEFnApp "array_length" [arrayExp, S.intToSQLExp 1] Nothing

cursorIdentifier :: Identifier
cursorIdentifier = Identifier "__cursor"

startCursorIdentifier :: Identifier
startCursorIdentifier = Identifier "__start_cursor"

endCursorIdentifier :: Identifier
endCursorIdentifier = Identifier "__end_cursor"

hasPreviousPageIdentifier :: Identifier
hasPreviousPageIdentifier = Identifier "__has_previous_page"

hasNextPageIdentifier :: Identifier
hasNextPageIdentifier = Identifier "__has_next_page"

pageInfoSelectAliasIdentifier :: Identifier
pageInfoSelectAliasIdentifier = Identifier "__page_info"

cursorsSelectAliasIdentifier :: Identifier
cursorsSelectAliasIdentifier = Identifier "__cursors_select"

encodeBase64 :: S.SQLExp -> S.SQLExp
encodeBase64 =
  removeNewline . bytesToBase64Text . convertToBytes
  where
    convertToBytes e =
      S.SEFnApp "convert_to" [e, S.SELit "UTF8"] Nothing
    bytesToBase64Text e =
      S.SEFnApp "encode" [e, S.SELit "base64"] Nothing
    removeNewline e =
      S.SEFnApp "regexp_replace" [e, S.SELit "\\n", S.SELit "", S.SELit "g"] Nothing


processConnectionSelect
  :: ( MonadReader Bool m
     , MonadWriter (JoinTree 'Postgres) m
     )
  => SourcePrefixes
  -> FieldName
  -> S.Alias
  -> HM.HashMap PGCol PGCol
  -> ConnectionSelect 'Postgres S.SQLExp
  -> m ( ArrayConnectionSource 'Postgres
       , S.Extractor
       , HM.HashMap S.Alias S.SQLExp
       )
processConnectionSelect sourcePrefixes fieldAlias relAlias colMapping connectionSelect = do
  (selectSource, orderByAndDistinctExtrs, maybeOrderByCursor) <-
    processSelectParams sourcePrefixes fieldAlias similarArrayFields selectFrom
    permLimitSubQuery tablePermissions tableArgs

  let mkCursorExtractor = (S.Alias cursorIdentifier,) . (`S.SETyAnn` S.textTypeAnn)
      cursorExtractors = case maybeOrderByCursor of
        Just orderByCursor -> [mkCursorExtractor orderByCursor]
        Nothing ->
          -- Extract primary key columns from base select along with cursor expression.
          -- Those columns are required to perform connection split via a WHERE clause.
          mkCursorExtractor primaryKeyColumnsObjectExp : primaryKeyColumnExtractors
      orderByExp = _ssOrderBy selectSource
  (topExtractorExp, exps) <- flip runStateT [] $ processFields orderByExp
  let topExtractor = S.Extractor topExtractorExp $ Just $ S.Alias fieldIdentifier
      allExtractors = HM.fromList $ cursorExtractors <> exps <> orderByAndDistinctExtrs
      arrayConnectionSource = ArrayConnectionSource relAlias colMapping
                              (mkSplitBoolExp <$> maybeSplit) maybeSlice selectSource
  pure ( arrayConnectionSource
       , topExtractor
       , allExtractors
       )
  where
    ConnectionSelect primaryKeyColumns maybeSplit maybeSlice select = connectionSelect
    AnnSelectG fields selectFrom tablePermissions tableArgs _ = select
    fieldIdentifier = toIdentifier fieldAlias
    thisPrefix = _pfThis sourcePrefixes
    permLimitSubQuery = PLSQNotRequired

    primaryKeyColumnsObjectExp =
      S.applyJsonBuildObj $ flip concatMap (toList primaryKeyColumns) $
      \pgColumnInfo ->
        [ S.SELit $ getPGColTxt $ pgiColumn pgColumnInfo
        , toJSONableExp False (pgiType pgColumnInfo) False $
          S.mkQIdenExp (mkBaseTableAlias thisPrefix) $ pgiColumn pgColumnInfo
        ]

    primaryKeyColumnExtractors =
      flip map (toList primaryKeyColumns) $
      \pgColumnInfo ->
        let pgColumn = pgiColumn pgColumnInfo
        in ( S.Alias $ mkBaseTableColumnAlias thisPrefix pgColumn
           , S.mkQIdenExp (mkBaseTableAlias thisPrefix) pgColumn
           )

    mkSplitBoolExp (firstSplit NE.:| rest) =
      S.BEBin S.OrOp (mkSplitCompareExp firstSplit) $ mkBoolExpFromRest firstSplit rest
      where
        mkBoolExpFromRest previousSplit =
          S.BEBin S.AndOp (mkEqualityCompareExp previousSplit) . \case
            []                         -> S.BELit False
            (thisSplit:remainingSplit) -> mkSplitBoolExp (thisSplit NE.:| remainingSplit)

        mkSplitCompareExp (ConnectionSplit kind v (OrderByItemG obTyM obCol _)) =
          let obAlias = mkAnnOrderByAlias thisPrefix fieldAlias similarArrayFields obCol
              obTy = maybe S.OTAsc unOrderType obTyM
              compareOp = case (kind, obTy) of
                (CSKAfter, S.OTAsc)   -> S.SGT
                (CSKAfter, S.OTDesc)  -> S.SLT
                (CSKBefore, S.OTAsc)  -> S.SLT
                (CSKBefore, S.OTDesc) -> S.SGT
          in S.BECompare compareOp (S.SEIdentifier $ toIdentifier obAlias) v

        mkEqualityCompareExp (ConnectionSplit _ v orderByItem) =
          let obAlias = mkAnnOrderByAlias thisPrefix fieldAlias similarArrayFields $
                        obiColumn orderByItem
          in S.BECompare S.SEQ (S.SEIdentifier $ toIdentifier obAlias) v

    similarArrayFields = HM.unions $
      flip map (map snd fields) $ \case
      ConnectionTypename{} -> mempty
      ConnectionPageInfo{} -> mempty
      ConnectionEdges edges -> HM.unions $
        flip map (map snd edges) $ \case
        EdgeTypename{} -> mempty
        EdgeCursor{} -> mempty
        EdgeNode annFields ->
          mkSimilarArrayFields annFields $ _saOrderBy tableArgs

    mkSimpleJsonAgg rowExp ob =
      let jsonAggExp = S.SEFnApp "json_agg" [rowExp] ob
      in S.SEFnApp "coalesce" [jsonAggExp, S.SELit "[]"] Nothing

    processFields
      :: ( MonadReader Bool m
         , MonadWriter (JoinTree 'Postgres) m
         , MonadState [(S.Alias, S.SQLExp)] m
         )
      => Maybe S.OrderByExp -> m S.SQLExp
    processFields orderByExp =
      fmap (S.applyJsonBuildObj . concat) $
      forM fields $
        \(FieldName fieldText, field) -> (S.SELit fieldText:) . pure <$>
        case field of
          ConnectionTypename t              -> pure $ withForceAggregation S.textTypeAnn $ S.SELit t
          ConnectionPageInfo pageInfoFields -> pure $ processPageInfoFields pageInfoFields
          ConnectionEdges edges ->
            fmap (flip mkSimpleJsonAgg orderByExp . S.applyJsonBuildObj . concat) $ forM edges $
            \(FieldName edgeText, edge) -> (S.SELit edgeText:) . pure <$>
            case edge of
              EdgeTypename t -> pure $ S.SELit t
              EdgeCursor     -> pure $ encodeBase64 $ S.SEIdentifier (toIdentifier cursorIdentifier)
              EdgeNode annFields -> do
                let edgeFieldName = FieldName $
                      getFieldNameTxt fieldAlias <> "." <> fieldText <> "." <> edgeText
                    edgeFieldIdentifier = toIdentifier edgeFieldName
                annFieldsExtrExp <- processAnnFields thisPrefix edgeFieldName similarArrayFields annFields
                modify' (<> [annFieldsExtrExp])
                pure $ S.SEIdentifier edgeFieldIdentifier

    processPageInfoFields infoFields =
      S.applyJsonBuildObj $ flip concatMap infoFields $
      \(FieldName fieldText, field) -> (:) (S.SELit fieldText) $ pure case field of
        PageInfoTypename t      -> withForceAggregation S.textTypeAnn $ S.SELit t
        PageInfoHasNextPage     -> withForceAggregation S.boolTypeAnn $
          mkSingleFieldSelect (S.SEIdentifier hasNextPageIdentifier) pageInfoSelectAliasIdentifier
        PageInfoHasPreviousPage -> withForceAggregation S.boolTypeAnn $
          mkSingleFieldSelect (S.SEIdentifier hasPreviousPageIdentifier) pageInfoSelectAliasIdentifier
        PageInfoStartCursor     -> withForceAggregation S.textTypeAnn $
          encodeBase64 $ mkSingleFieldSelect (S.SEIdentifier startCursorIdentifier) cursorsSelectAliasIdentifier
        PageInfoEndCursor       -> withForceAggregation S.textTypeAnn $
          encodeBase64 $ mkSingleFieldSelect (S.SEIdentifier endCursorIdentifier) cursorsSelectAliasIdentifier
      where
        mkSingleFieldSelect field fromIdentifier = S.SESelect
          S.mkSelect { S.selExtr = [S.Extractor field Nothing]
                     , S.selFrom = Just $ S.FromExp [S.FIIdentifier fromIdentifier]
                     }

connectionToSelectWith
  :: S.Alias
  -> ArrayConnectionSource 'Postgres
  -> ArraySelectNode 'Postgres
  -> S.SelectWithG S.Select
connectionToSelectWith baseSelectAlias arrayConnectionSource arraySelectNode =
  let extractionSelect = S.mkSelect
                         { S.selExtr = topExtractors
                         , S.selFrom = Just $ S.FromExp [S.FIIdentifier finalSelectIdentifier]
                         }
  in S.SelectWith fromBaseSelections extractionSelect
  where
    ArrayConnectionSource _ columnMapping maybeSplit maybeSlice selectSource =
      arrayConnectionSource
    ArraySelectNode topExtractors selectNode = arraySelectNode
    baseSelectIdentifier = Identifier "__base_select"
    splitSelectIdentifier = Identifier "__split_select"
    sliceSelectIdentifier = Identifier "__slice_select"
    finalSelectIdentifier = Identifier "__final_select"

    rowNumberIdentifier = Identifier "__row_number"
    rowNumberExp = S.SEUnsafe "(row_number() over (partition by 1))"
    startRowNumberIdentifier = Identifier "__start_row_number"
    endRowNumberIdentifier = Identifier "__end_row_number"

    startCursorExp = mkFirstElementExp $ S.SEIdentifier cursorIdentifier
    endCursorExp = mkLastElementExp $ S.SEIdentifier cursorIdentifier

    startRowNumberExp = mkFirstElementExp $ S.SEIdentifier rowNumberIdentifier
    endRowNumberExp = mkLastElementExp $ S.SEIdentifier rowNumberIdentifier

    fromBaseSelections =
      let joinCond = mkJoinCond baseSelectAlias columnMapping
          baseSelectFrom = S.mkSelFromItem
                           (generateSQLSelect joinCond selectSource selectNode)
                           $ S.Alias $ _ssPrefix selectSource
          select =
            S.mkSelect { S.selExtr = [ S.selectStar
                                     , S.Extractor rowNumberExp $ Just $ S.Alias rowNumberIdentifier
                                     ]
                       , S.selFrom = Just $ S.FromExp [baseSelectFrom]
                       }
      in (S.Alias baseSelectIdentifier, select):fromSplitSelection

    mkStarSelect fromIdentifier =
      S.mkSelect { S.selExtr = [S.selectStar]
                 , S.selFrom = Just $ S.FromExp [S.FIIdentifier fromIdentifier]
                 }

    fromSplitSelection = case maybeSplit of
      Nothing        -> fromSliceSelection baseSelectIdentifier
      Just splitBool ->
        let select =
              (mkStarSelect baseSelectIdentifier){S.selWhere = Just $ S.WhereFrag splitBool}
        in (S.Alias splitSelectIdentifier, select):fromSliceSelection splitSelectIdentifier

    fromSliceSelection prevSelect = case maybeSlice of
      Nothing    -> fromFinalSelect prevSelect
      Just slice ->
        let select = case slice of
              SliceFirst limit ->
                (mkStarSelect prevSelect)
                {S.selLimit = (Just . S.LimitExp . S.intToSQLExp) limit}
              SliceLast limit ->
                let mkRowNumberOrderBy obType =
                      let orderByItem =
                            S.OrderByItem (S.SEIdentifier rowNumberIdentifier) (Just obType) Nothing
                      in S.OrderByExp $ orderByItem NE.:| []

                    sliceLastSelect = (mkStarSelect prevSelect)
                                      { S.selLimit = (Just . S.LimitExp . S.intToSQLExp) limit
                                      , S.selOrderBy = Just $ mkRowNumberOrderBy S.OTDesc
                                      }
                    sliceLastSelectFrom =
                      S.mkSelFromItem sliceLastSelect $ S.Alias sliceSelectIdentifier
                in S.mkSelect { S.selExtr = [S.selectStar]
                              , S.selFrom = Just $ S.FromExp [sliceLastSelectFrom]
                              , S.selOrderBy = Just $ mkRowNumberOrderBy S.OTAsc
                              }
        in (S.Alias sliceSelectIdentifier, select):fromFinalSelect sliceSelectIdentifier

    fromFinalSelect prevSelect =
      let select = mkStarSelect prevSelect
      in (S.Alias finalSelectIdentifier, select):fromCursorSelection

    fromCursorSelection =
      let extrs = [ S.Extractor startCursorExp $ Just $ S.Alias startCursorIdentifier
                  , S.Extractor endCursorExp $ Just $ S.Alias endCursorIdentifier
                  , S.Extractor startRowNumberExp $ Just $ S.Alias startRowNumberIdentifier
                  , S.Extractor endRowNumberExp $ Just $ S.Alias endRowNumberIdentifier
                  ]
          select =
            S.mkSelect { S.selExtr = extrs
                       , S.selFrom = Just $ S.FromExp [S.FIIdentifier finalSelectIdentifier]
                       }
      in (S.Alias cursorsSelectAliasIdentifier, select):fromPageInfoSelection

    fromPageInfoSelection =
      let hasPrevPage = S.SEBool $
            S.mkExists (S.FIIdentifier baseSelectIdentifier) $
            S.BECompare S.SLT (S.SEIdentifier rowNumberIdentifier) $
            S.SESelect $ S.mkSelect { S.selFrom = Just $ S.FromExp [S.FIIdentifier cursorsSelectAliasIdentifier]
                                    , S.selExtr = [S.Extractor (S.SEIdentifier startRowNumberIdentifier) Nothing]
                                    }
          hasNextPage = S.SEBool $
            S.mkExists (S.FIIdentifier baseSelectIdentifier) $
            S.BECompare S.SGT (S.SEIdentifier rowNumberIdentifier) $
            S.SESelect $ S.mkSelect { S.selFrom = Just $ S.FromExp [S.FIIdentifier cursorsSelectAliasIdentifier]
                                    , S.selExtr = [S.Extractor (S.SEIdentifier endRowNumberIdentifier) Nothing]
                                    }

          select =
            S.mkSelect { S.selExtr = [ S.Extractor hasPrevPage $ Just $ S.Alias hasPreviousPageIdentifier
                                     , S.Extractor hasNextPage $ Just $ S.Alias hasNextPageIdentifier
                                     ]
                       }
      in pure (S.Alias pageInfoSelectAliasIdentifier, select)
