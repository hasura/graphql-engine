-- | This module defines functions that translate from the Postgres IR into
-- Postgres SQL AST.
--
-- NOTE: These functions 'processAnnAggregateSelect', 'processAnnSimpleSelect',
-- 'processConnectionSelect', are all mutually recursive.
--
-- These functions are generally called from the top level functions in
-- Translate.Select, and the call stack looks like:
--
--     * 'selectQuerySQL' -> 'mkSQLSelect' -> 'processAnnSimpleSelect' -> 'processSelectParams'/'processAnnFields'
--
--     * 'selectAggregateQuerySQL' -> 'mkAggregateSelect' -> 'processAnnAggregateSelect' -> 'processSelectParams'/'processAnnFields'
--
--     * 'connetionSelectQuerySQL' -> 'mkConnectionSelect' -> 'processConnectionSelection' -> 'processSelectParams'
--
-- 'SelectSource' consists of a prefix, a source, a boolean conditional
-- expression, and info on whether sorting or slicing is done (needed to handle
-- the LIMIT optimisation)
module Hasura.Backends.Postgres.Translate.Select.Internal.Process
  ( processAnnAggregateSelect,
    processAnnSimpleSelect,
    processConnectionSelect,
  )
where

import Data.HashMap.Strict qualified as HM
import Data.List.NonEmpty qualified as NE
import Data.Text.Extended (ToTxt (toTxt))
import Hasura.Backends.Postgres.SQL.DML qualified as S
import Hasura.Backends.Postgres.SQL.Types
  ( Identifier,
    IsIdentifier (toIdentifier),
    PGCol (..),
    QualifiedObject (QualifiedObject),
    QualifiedTable,
    SchemaName (getSchemaTxt),
  )
import Hasura.Backends.Postgres.Translate.BoolExp (toSQLBoolExp)
import Hasura.Backends.Postgres.Translate.Column (toJSONableExp)
import Hasura.Backends.Postgres.Translate.Select.AnnotatedFieldJSON
import Hasura.Backends.Postgres.Translate.Select.Internal.Aliases
  ( mkAnnOrderByAlias,
    mkArrayRelationAlias,
    mkArrayRelationSourcePrefix,
    mkBaseTableAlias,
    mkBaseTableColumnAlias,
    mkComputedFieldTableAlias,
    mkObjectRelationTableAlias,
    mkOrderByFieldName,
  )
import Hasura.Backends.Postgres.Translate.Select.Internal.Extractor
  ( aggregateFieldsToExtractorExps,
    asJsonAggExtr,
    withJsonAggExtr,
  )
import Hasura.Backends.Postgres.Translate.Select.Internal.Helpers
  ( cursorIdentifier,
    cursorsSelectAliasIdentifier,
    encodeBase64,
    endCursorIdentifier,
    fromTableRowArgs,
    functionToIdentifier,
    hasNextPageIdentifier,
    hasPreviousPageIdentifier,
    pageInfoSelectAliasIdentifier,
    selectFromToFromItem,
    startCursorIdentifier,
    withForceAggregation,
  )
import Hasura.Backends.Postgres.Translate.Select.Internal.JoinTree
  ( withWriteArrayConnection,
    withWriteArrayRelation,
    withWriteComputedFieldTableSet,
    withWriteObjectRelation,
  )
import Hasura.Backends.Postgres.Translate.Select.Internal.OrderBy (processOrderByItems)
import Hasura.Backends.Postgres.Translate.Types
import Hasura.GraphQL.Schema.Common (currentNodeIdVersion, nodeIdVersionInt)
import Hasura.Prelude
import Hasura.RQL.IR.OrderBy (OrderByItemG (OrderByItemG, obiColumn))
import Hasura.RQL.IR.Select
import Hasura.RQL.Types

processSelectParams ::
  forall pgKind m.
  ( MonadReader StringifyNumbers m,
    MonadWriter JoinTree m,
    Backend ('Postgres pgKind)
  ) =>
  SourcePrefixes ->
  FieldName ->
  SimilarArrayFields ->
  SelectFrom ('Postgres pgKind) ->
  PermissionLimitSubQuery ->
  TablePerm ('Postgres pgKind) ->
  SelectArgs ('Postgres pgKind) ->
  m
    ( SelectSource,
      [(S.Alias, S.SQLExp)],
      Maybe S.SQLExp -- Order by cursor
    )
processSelectParams
  sourcePrefixes
  fieldAlias
  similarArrFields
  selectFrom
  permLimitSubQ
  tablePermissions
  tableArgs = do
    (additionalExtrs, selectSorting, cursorExp) <-
      processOrderByItems thisSourcePrefix fieldAlias similarArrFields distM orderByM
    let fromItem = selectFromToFromItem (_pfBase sourcePrefixes) selectFrom
        finalWhere =
          toSQLBoolExp (selectFromToQual selectFrom) $
            maybe permFilter (andAnnBoolExps permFilter) whereM
        sortingAndSlicing = SortingAndSlicing selectSorting selectSlicing
        selectSource =
          SelectSource
            thisSourcePrefix
            fromItem
            finalWhere
            sortingAndSlicing
    pure
      ( selectSource,
        additionalExtrs,
        cursorExp
      )
    where
      thisSourcePrefix = _pfThis sourcePrefixes
      SelectArgs whereM orderByM inpLimitM offsetM distM = tableArgs
      TablePerm permFilter permLimit = tablePermissions
      selectSlicing = SelectSlicing finalLimit offsetM
      finalLimit =
        -- if sub query is required, then only use input limit
        --    because permission limit is being applied in subquery
        -- else compare input and permission limits
        case permLimitSubQ of
          PLSQRequired _ -> inpLimitM
          PLSQNotRequired -> compareLimits

      compareLimits =
        case (inpLimitM, permLimit) of
          (inpLim, Nothing) -> inpLim
          (Nothing, permLim) -> permLim
          (Just inp, Just perm) -> Just if inp < perm then inp else perm

      -- You should be able to retrieve this information
      -- from the FromItem generated with selectFromToFromItem
      -- however given from S.FromItem is modelled, it is not
      -- possible currently.
      --
      -- More precisely, 'selectFromToFromItem' is injective but not surjective, so
      -- any S.FromItem -> S.Qual function would have to be partial.
      selectFromToQual :: SelectFrom ('Postgres pgKind) -> S.Qual
      selectFromToQual = \case
        FromTable table -> S.QualTable table
        FromIdentifier i -> S.QualifiedIdentifier (toIdentifier i) Nothing
        FromFunction qf _ _ -> S.QualifiedIdentifier (functionToIdentifier qf) Nothing

processAnnAggregateSelect ::
  forall pgKind m.
  ( MonadReader StringifyNumbers m,
    MonadWriter JoinTree m,
    Backend ('Postgres pgKind),
    PostgresAnnotatedFieldJSON pgKind
  ) =>
  SourcePrefixes ->
  FieldName ->
  AnnAggregateSelect ('Postgres pgKind) ->
  m
    ( SelectSource,
      HM.HashMap S.Alias S.SQLExp,
      S.Extractor
    )
processAnnAggregateSelect sourcePrefixes fieldAlias annAggSel = do
  (selectSource, orderByAndDistinctExtrs, _) <-
    processSelectParams
      sourcePrefixes
      fieldAlias
      similarArrayFields
      tableFrom
      permLimitSubQuery
      tablePermissions
      tableArgs
  let thisSourcePrefix = _pfThis sourcePrefixes
  processedFields <- forM aggSelFields $ \(fieldName, field) ->
    (fieldName,)
      <$> case field of
        TAFAgg aggFields ->
          pure
            ( aggregateFieldsToExtractorExps thisSourcePrefix aggFields,
              aggregateFieldToExp aggFields strfyNum
            )
        TAFNodes _ annFields -> do
          annFieldExtr <- processAnnFields thisSourcePrefix fieldName similarArrayFields annFields
          pure
            ( [annFieldExtr],
              withJsonAggExtr permLimitSubQuery (orderByForJsonAgg selectSource) $
                S.Alias $ toIdentifier fieldName
            )
        TAFExp e ->
          pure
            ( [],
              withForceAggregation S.textTypeAnn $ S.SELit e
            )

  let topLevelExtractor =
        flip S.Extractor (Just $ S.Alias $ toIdentifier fieldAlias) $
          S.applyJsonBuildObj $
            flip concatMap (map (second snd) processedFields) $
              \(FieldName fieldText, fieldExp) -> [S.SELit fieldText, fieldExp]
      nodeExtractors =
        HM.fromList $
          concatMap (fst . snd) processedFields <> orderByAndDistinctExtrs

  pure (selectSource, nodeExtractors, topLevelExtractor)
  where
    AnnSelectG aggSelFields tableFrom tablePermissions tableArgs strfyNum = annAggSel
    permLimit = _tpLimit tablePermissions
    orderBy = _saOrderBy tableArgs
    permLimitSubQuery = mkPermissionLimitSubQuery permLimit aggSelFields orderBy
    similarArrayFields = HM.unions $
      flip map (map snd aggSelFields) $ \case
        TAFAgg _ -> mempty
        TAFNodes _ annFlds ->
          mkSimilarArrayFields annFlds orderBy
        TAFExp _ -> mempty

    mkPermissionLimitSubQuery ::
      Maybe Int ->
      TableAggregateFields ('Postgres pgKind) ->
      Maybe (NE.NonEmpty (AnnotatedOrderByItem ('Postgres pgKind))) ->
      PermissionLimitSubQuery
    mkPermissionLimitSubQuery permLimit' aggFields orderBys =
      case permLimit' of
        Nothing -> PLSQNotRequired
        Just limit ->
          if hasAggregateField || hasAggOrderBy
            then PLSQRequired limit
            else PLSQNotRequired
      where
        hasAggregateField = flip any (map snd aggFields) $
          \case
            TAFAgg _ -> True
            _ -> False

        hasAggOrderBy = case orderBys of
          Nothing -> False
          Just l -> flip any (concatMap toList $ toList l) $
            \case
              AOCArrayAggregation {} -> True
              _ -> False

processAnnFields ::
  forall pgKind m.
  ( MonadReader StringifyNumbers m,
    MonadWriter JoinTree m,
    Backend ('Postgres pgKind),
    PostgresAnnotatedFieldJSON pgKind
  ) =>
  Identifier ->
  FieldName ->
  SimilarArrayFields ->
  AnnFields ('Postgres pgKind) ->
  m (S.Alias, S.SQLExp)
processAnnFields sourcePrefix fieldAlias similarArrFields annFields = do
  fieldExps <- forM annFields $ \(fieldName, field) ->
    (fieldName,)
      <$> case field of
        AFExpression t -> pure $ S.SELit t
        AFNodeId _ tn pKeys -> pure $ mkNodeId tn pKeys
        AFColumn c -> toSQLCol c
        AFObjectRelation objSel -> withWriteObjectRelation $ do
          let AnnRelationSelectG relName relMapping annObjSel = objSel
              AnnObjectSelectG objAnnFields tableFrom tableFilter = annObjSel
              objRelSourcePrefix = mkObjectRelationTableAlias sourcePrefix relName
              sourcePrefixes = mkSourcePrefixes objRelSourcePrefix
          annFieldsExtr <- processAnnFields (_pfThis sourcePrefixes) fieldName HM.empty objAnnFields
          let selectSource =
                ObjectSelectSource
                  (_pfThis sourcePrefixes)
                  (S.FISimple tableFrom Nothing)
                  (toSQLBoolExp (S.QualTable tableFrom) tableFilter)
              objRelSource = ObjectRelationSource relName relMapping selectSource
          pure
            ( objRelSource,
              HM.fromList [annFieldsExtr],
              S.mkQIdenExp objRelSourcePrefix fieldName
            )
        AFArrayRelation arrSel -> do
          let arrRelSourcePrefix = mkArrayRelationSourcePrefix sourcePrefix fieldAlias similarArrFields fieldName
              arrRelAlias = mkArrayRelationAlias fieldAlias similarArrFields fieldName
          processArrayRelation (mkSourcePrefixes arrRelSourcePrefix) fieldName arrRelAlias arrSel
          pure $ S.mkQIdenExp arrRelSourcePrefix fieldName
        AFComputedField _ _ (CFSScalar scalar caseBoolExpMaybe) -> do
          computedFieldSQLExp <- fromScalarComputedField scalar
          -- The computed field is conditionally outputed depending
          -- on the presence of `caseBoolExpMaybe` and the value it
          -- evaluates to. `caseBoolExpMaybe` will be set only in the
          -- case of an inherited role.
          -- See [SQL generation for inherited role]
          case caseBoolExpMaybe of
            Nothing -> pure computedFieldSQLExp
            Just caseBoolExp ->
              let boolExp =
                    S.simplifyBoolExp $
                      toSQLBoolExp (S.QualifiedIdentifier baseTableIdentifier Nothing) $
                        _accColCaseBoolExpField <$> caseBoolExp
               in pure $ S.SECond boolExp computedFieldSQLExp S.SENull
        AFComputedField _ _ (CFSTable selectTy sel) -> withWriteComputedFieldTableSet $ do
          let computedFieldSourcePrefix =
                mkComputedFieldTableAlias sourcePrefix fieldName
          (selectSource, nodeExtractors) <-
            processAnnSimpleSelect
              (mkSourcePrefixes computedFieldSourcePrefix)
              fieldName
              PLSQNotRequired
              sel
          let computedFieldTableSetSource = ComputedFieldTableSetSource fieldName selectSource
              extractor =
                asJsonAggExtr selectTy (S.toAlias fieldName) PLSQNotRequired $
                  orderByForJsonAgg selectSource
          pure
            ( computedFieldTableSetSource,
              extractor,
              nodeExtractors,
              S.mkQIdenExp computedFieldSourcePrefix fieldName
            )

  pure $ annRowToJson @pgKind fieldAlias fieldExps
  where
    mkSourcePrefixes newPrefix = SourcePrefixes newPrefix sourcePrefix

    baseTableIdentifier = mkBaseTableAlias sourcePrefix

    toSQLCol :: AnnColumnField ('Postgres pgKind) S.SQLExp -> m S.SQLExp
    toSQLCol (AnnColumnField col typ asText colOpM caseBoolExpMaybe) = do
      strfyNum <- ask
      let sqlExpression =
            withColumnOp colOpM $
              S.mkQIdenExp baseTableIdentifier col
          finalSQLExpression =
            -- Check out [SQL generation for inherited role]
            case caseBoolExpMaybe of
              Nothing -> sqlExpression
              Just caseBoolExp ->
                let boolExp =
                      S.simplifyBoolExp $
                        toSQLBoolExp (S.QualifiedIdentifier baseTableIdentifier Nothing) $
                          _accColCaseBoolExpField <$> caseBoolExp
                 in S.SECond boolExp sqlExpression S.SENull
      pure $ toJSONableExp strfyNum typ asText finalSQLExpression

    fromScalarComputedField :: ComputedFieldScalarSelect ('Postgres pgKind) S.SQLExp -> m S.SQLExp
    fromScalarComputedField computedFieldScalar = do
      strfyNum <- ask
      pure $
        toJSONableExp strfyNum (ColumnScalar ty) False $
          withColumnOp colOpM $
            S.SEFunction $ S.FunctionExp fn (fromTableRowArgs sourcePrefix args) Nothing
      where
        ComputedFieldScalarSelect fn args ty colOpM = computedFieldScalar

    withColumnOp :: Maybe (ColumnOp ('Postgres pgKind)) -> S.SQLExp -> S.SQLExp
    withColumnOp colOpM sqlExp = case colOpM of
      Nothing -> sqlExp
      Just (ColumnOp opText cExp) -> S.mkSQLOpExp opText sqlExp cExp

    mkNodeId :: QualifiedTable -> PrimaryKeyColumns ('Postgres pgKind) -> S.SQLExp
    mkNodeId (QualifiedObject tableSchema tableName) pkeyColumns =
      let columnInfoToSQLExp pgColumnInfo =
            toJSONableExp LeaveNumbersAlone (ciType pgColumnInfo) False $
              S.mkQIdenExp (mkBaseTableAlias sourcePrefix) $ ciColumn pgColumnInfo
       in -- See Note [Relay Node id].
          encodeBase64 $
            flip S.SETyAnn S.textTypeAnn $
              S.applyJsonBuildArray $
                [ S.intToSQLExp $ nodeIdVersionInt currentNodeIdVersion,
                  S.SELit (getSchemaTxt tableSchema),
                  S.SELit (toTxt tableName)
                ]
                  <> map columnInfoToSQLExp (toList pkeyColumns)

mkSimilarArrayFields ::
  forall pgKind v.
  (Backend ('Postgres pgKind), Eq v) =>
  AnnFieldsG ('Postgres pgKind) Void v ->
  Maybe (NE.NonEmpty (AnnotatedOrderByItemG ('Postgres pgKind) v)) ->
  SimilarArrayFields
mkSimilarArrayFields annFields maybeOrderBys =
  HM.fromList $
    flip map allTuples $
      \(relNameAndArgs, fieldName) -> (fieldName, getSimilarFields relNameAndArgs)
  where
    getSimilarFields relNameAndArgs = map snd $ filter ((== relNameAndArgs) . fst) allTuples
    allTuples = arrayRelationTuples <> aggOrderByRelationTuples
    arrayRelationTuples =
      let arrayFields = mapMaybe getAnnArr annFields
       in flip map arrayFields $
            \(f, relSel) -> (getArrayRelNameAndSelectArgs relSel, f)

    getAnnArr ::
      (a, AnnFieldG ('Postgres pgKind) r v) ->
      Maybe (a, ArraySelectG ('Postgres pgKind) r v)
    getAnnArr (f, annFld) = case annFld of
      AFArrayRelation (ASConnection _) -> Nothing
      AFArrayRelation ar -> Just (f, ar)
      _ -> Nothing

    aggOrderByRelationTuples =
      let mkItem (relName, fieldName) =
            ( (relName, noSelectArgs),
              fieldName
            )
       in map mkItem $
            maybe
              []
              (mapMaybe (fetchAggOrderByRels . obiColumn) . toList)
              maybeOrderBys

    fetchAggOrderByRels (AOCArrayAggregation ri _ _) =
      Just (riName ri, mkOrderByFieldName $ riName ri)
    fetchAggOrderByRels _ = Nothing

    getArrayRelNameAndSelectArgs ::
      ArraySelectG ('Postgres pgKind) r v ->
      (RelName, SelectArgsG ('Postgres pgKind) v)
    getArrayRelNameAndSelectArgs = \case
      ASSimple r -> (_aarRelationshipName r, _asnArgs $ _aarAnnSelect r)
      ASAggregate r -> (_aarRelationshipName r, _asnArgs $ _aarAnnSelect r)
      ASConnection r -> (_aarRelationshipName r, _asnArgs $ _csSelect $ _aarAnnSelect r)

processArrayRelation ::
  forall pgKind m.
  ( MonadReader StringifyNumbers m,
    MonadWriter JoinTree m,
    Backend ('Postgres pgKind),
    PostgresAnnotatedFieldJSON pgKind
  ) =>
  SourcePrefixes ->
  FieldName ->
  S.Alias ->
  ArraySelect ('Postgres pgKind) ->
  m ()
processArrayRelation sourcePrefixes fieldAlias relAlias arrSel =
  case arrSel of
    ASSimple annArrRel -> withWriteArrayRelation $ do
      let AnnRelationSelectG _ colMapping sel = annArrRel
          permLimitSubQuery =
            maybe PLSQNotRequired PLSQRequired $ _tpLimit $ _asnPerm sel
      (source, nodeExtractors) <-
        processAnnSimpleSelect sourcePrefixes fieldAlias permLimitSubQuery sel
      let topExtr =
            asJsonAggExtr
              JASMultipleRows
              (S.toAlias fieldAlias)
              permLimitSubQuery
              $ orderByForJsonAgg source
      pure
        ( ArrayRelationSource relAlias colMapping source,
          topExtr,
          nodeExtractors,
          ()
        )
    ASAggregate aggSel -> withWriteArrayRelation $ do
      let AnnRelationSelectG _ colMapping sel = aggSel
      (source, nodeExtractors, topExtr) <-
        processAnnAggregateSelect sourcePrefixes fieldAlias sel
      pure
        ( ArrayRelationSource relAlias colMapping source,
          topExtr,
          nodeExtractors,
          ()
        )
    ASConnection connSel -> withWriteArrayConnection $ do
      let AnnRelationSelectG _ colMapping sel = connSel
      (source, topExtractor, nodeExtractors) <-
        processConnectionSelect sourcePrefixes fieldAlias relAlias colMapping sel
      pure
        ( source,
          topExtractor,
          nodeExtractors,
          ()
        )

aggregateFieldToExp :: AggregateFields ('Postgres pgKind) -> StringifyNumbers -> S.SQLExp
aggregateFieldToExp aggFlds strfyNum = jsonRow
  where
    jsonRow = S.applyJsonBuildObj (concatMap aggToFlds aggFlds)
    withAls fldName sqlExp = [S.SELit fldName, sqlExp]
    aggToFlds (FieldName t, fld) = withAls t $ case fld of
      AFCount cty -> S.SECount cty
      AFOp aggOp -> aggOpToObj aggOp
      AFExp e -> S.SELit e

    aggOpToObj (AggregateOp opText flds) =
      S.applyJsonBuildObj $ concatMap (colFldsToExtr opText) flds

    colFldsToExtr opText (FieldName t, CFCol col ty) =
      [ S.SELit t,
        toJSONableExp strfyNum ty False $
          S.SEFnApp opText [S.SEIdentifier $ toIdentifier col] Nothing
      ]
    colFldsToExtr _ (FieldName t, CFExp e) =
      [S.SELit t, S.SELit e]

processAnnSimpleSelect ::
  forall pgKind m.
  ( MonadReader StringifyNumbers m,
    MonadWriter JoinTree m,
    Backend ('Postgres pgKind),
    PostgresAnnotatedFieldJSON pgKind
  ) =>
  SourcePrefixes ->
  FieldName ->
  PermissionLimitSubQuery ->
  AnnSimpleSelect ('Postgres pgKind) ->
  m
    ( SelectSource,
      HM.HashMap S.Alias S.SQLExp
    )
processAnnSimpleSelect sourcePrefixes fieldAlias permLimitSubQuery annSimpleSel = do
  (selectSource, orderByAndDistinctExtrs, _) <-
    processSelectParams
      sourcePrefixes
      fieldAlias
      similarArrayFields
      tableFrom
      permLimitSubQuery
      tablePermissions
      tableArgs
  annFieldsExtr <- processAnnFields (_pfThis sourcePrefixes) fieldAlias similarArrayFields annSelFields
  let allExtractors = HM.fromList $ annFieldsExtr : orderByAndDistinctExtrs
  pure (selectSource, allExtractors)
  where
    AnnSelectG annSelFields tableFrom tablePermissions tableArgs _ = annSimpleSel
    similarArrayFields =
      mkSimilarArrayFields annSelFields $ _saOrderBy tableArgs

processConnectionSelect ::
  forall pgKind m.
  ( MonadReader StringifyNumbers m,
    MonadWriter JoinTree m,
    Backend ('Postgres pgKind),
    PostgresAnnotatedFieldJSON pgKind
  ) =>
  SourcePrefixes ->
  FieldName ->
  S.Alias ->
  HM.HashMap PGCol PGCol ->
  ConnectionSelect ('Postgres pgKind) Void S.SQLExp ->
  m
    ( ArrayConnectionSource,
      S.Extractor,
      HM.HashMap S.Alias S.SQLExp
    )
processConnectionSelect sourcePrefixes fieldAlias relAlias colMapping connectionSelect = do
  (selectSource, orderByAndDistinctExtrs, maybeOrderByCursor) <-
    processSelectParams
      sourcePrefixes
      fieldAlias
      similarArrayFields
      selectFrom
      permLimitSubQuery
      tablePermissions
      tableArgs

  let mkCursorExtractor = (S.Alias cursorIdentifier,) . (`S.SETyAnn` S.textTypeAnn)
      cursorExtractors = case maybeOrderByCursor of
        Just orderByCursor -> [mkCursorExtractor orderByCursor]
        Nothing ->
          -- Extract primary key columns from base select along with cursor expression.
          -- Those columns are required to perform connection split via a WHERE clause.
          mkCursorExtractor primaryKeyColumnsObjectExp : primaryKeyColumnExtractors
  (topExtractorExp, exps) <- flip runStateT [] $ processFields selectSource
  let topExtractor = S.Extractor topExtractorExp $ Just $ S.Alias fieldIdentifier
      allExtractors = HM.fromList $ cursorExtractors <> exps <> orderByAndDistinctExtrs
      arrayConnectionSource =
        ArrayConnectionSource
          relAlias
          colMapping
          (mkSplitBoolExp <$> maybeSplit)
          maybeSlice
          selectSource
  pure
    ( arrayConnectionSource,
      topExtractor,
      allExtractors
    )
  where
    ConnectionSelect _ primaryKeyColumns maybeSplit maybeSlice select = connectionSelect
    AnnSelectG fields selectFrom tablePermissions tableArgs _ = select
    fieldIdentifier = toIdentifier fieldAlias
    thisPrefix = _pfThis sourcePrefixes
    permLimitSubQuery = PLSQNotRequired

    primaryKeyColumnsObjectExp =
      S.applyJsonBuildObj $
        flip concatMap (toList primaryKeyColumns) $
          \pgColumnInfo ->
            [ S.SELit $ getPGColTxt $ ciColumn pgColumnInfo,
              toJSONableExp LeaveNumbersAlone (ciType pgColumnInfo) False $
                S.mkQIdenExp (mkBaseTableAlias thisPrefix) $ ciColumn pgColumnInfo
            ]

    primaryKeyColumnExtractors =
      flip map (toList primaryKeyColumns) $
        \pgColumnInfo ->
          let pgColumn = ciColumn pgColumnInfo
           in ( S.Alias $ mkBaseTableColumnAlias thisPrefix pgColumn,
                S.mkQIdenExp (mkBaseTableAlias thisPrefix) pgColumn
              )

    mkSplitBoolExp (firstSplit NE.:| rest) =
      S.BEBin S.OrOp (mkSplitCompareExp firstSplit) $ mkBoolExpFromRest firstSplit rest
      where
        mkBoolExpFromRest previousSplit =
          S.BEBin S.AndOp (mkEqualityCompareExp previousSplit) . \case
            [] -> S.BELit False
            (thisSplit : remainingSplit) -> mkSplitBoolExp (thisSplit NE.:| remainingSplit)

        mkSplitCompareExp (ConnectionSplit kind v (OrderByItemG obTyM obCol _)) =
          let obAlias = mkAnnOrderByAlias thisPrefix fieldAlias similarArrayFields obCol
              obTy = fromMaybe S.OTAsc obTyM
              compareOp = case (kind, obTy) of
                (CSKAfter, S.OTAsc) -> S.SGT
                (CSKAfter, S.OTDesc) -> S.SLT
                (CSKBefore, S.OTAsc) -> S.SLT
                (CSKBefore, S.OTDesc) -> S.SGT
           in S.BECompare compareOp (S.SEIdentifier $ toIdentifier obAlias) v

        mkEqualityCompareExp (ConnectionSplit _ v orderByItem) =
          let obAlias =
                mkAnnOrderByAlias thisPrefix fieldAlias similarArrayFields $
                  obiColumn orderByItem
           in S.BECompare S.SEQ (S.SEIdentifier $ toIdentifier obAlias) v

    similarArrayFields = HM.unions $
      flip map (map snd fields) $ \case
        ConnectionTypename {} -> mempty
        ConnectionPageInfo {} -> mempty
        ConnectionEdges edges -> HM.unions $
          flip map (map snd edges) $ \case
            EdgeTypename {} -> mempty
            EdgeCursor {} -> mempty
            EdgeNode annFields ->
              mkSimilarArrayFields annFields $ _saOrderBy tableArgs

    mkSimpleJsonAgg rowExp ob =
      let jsonAggExp = S.SEFnApp "json_agg" [rowExp] ob
       in S.SEFnApp "coalesce" [jsonAggExp, S.SELit "[]"] Nothing

    processFields ::
      forall n.
      ( MonadReader StringifyNumbers n,
        MonadWriter JoinTree n,
        MonadState [(S.Alias, S.SQLExp)] n
      ) =>
      SelectSource ->
      n S.SQLExp
    processFields selectSource =
      fmap (S.applyJsonBuildObj . concat) $
        forM fields $
          \(FieldName fieldText, field) ->
            (S.SELit fieldText :) . pure
              <$> case field of
                ConnectionTypename t -> pure $ withForceAggregation S.textTypeAnn $ S.SELit t
                ConnectionPageInfo pageInfoFields -> pure $ processPageInfoFields pageInfoFields
                ConnectionEdges edges ->
                  fmap (flip mkSimpleJsonAgg (orderByForJsonAgg selectSource) . S.applyJsonBuildObj . concat) $
                    forM edges $
                      \(FieldName edgeText, edge) ->
                        (S.SELit edgeText :) . pure
                          <$> case edge of
                            EdgeTypename t -> pure $ S.SELit t
                            EdgeCursor -> pure $ encodeBase64 $ S.SEIdentifier (toIdentifier cursorIdentifier)
                            EdgeNode annFields -> do
                              let edgeFieldName =
                                    FieldName $
                                      getFieldNameTxt fieldAlias <> "." <> fieldText <> "." <> edgeText
                                  edgeFieldIdentifier = toIdentifier edgeFieldName
                              annFieldsExtrExp <- processAnnFields thisPrefix edgeFieldName similarArrayFields annFields
                              modify' (<> [annFieldsExtrExp])
                              pure $ S.SEIdentifier edgeFieldIdentifier

    processPageInfoFields infoFields =
      S.applyJsonBuildObj $
        flip concatMap infoFields $
          \(FieldName fieldText, field) -> (:) (S.SELit fieldText) $ pure case field of
            PageInfoTypename t -> withForceAggregation S.textTypeAnn $ S.SELit t
            PageInfoHasNextPage ->
              withForceAggregation S.boolTypeAnn $
                mkSingleFieldSelect (S.SEIdentifier hasNextPageIdentifier) pageInfoSelectAliasIdentifier
            PageInfoHasPreviousPage ->
              withForceAggregation S.boolTypeAnn $
                mkSingleFieldSelect (S.SEIdentifier hasPreviousPageIdentifier) pageInfoSelectAliasIdentifier
            PageInfoStartCursor ->
              withForceAggregation S.textTypeAnn $
                encodeBase64 $ mkSingleFieldSelect (S.SEIdentifier startCursorIdentifier) cursorsSelectAliasIdentifier
            PageInfoEndCursor ->
              withForceAggregation S.textTypeAnn $
                encodeBase64 $ mkSingleFieldSelect (S.SEIdentifier endCursorIdentifier) cursorsSelectAliasIdentifier
      where
        mkSingleFieldSelect field fromIdentifier =
          S.SESelect
            S.mkSelect
              { S.selExtr = [S.Extractor field Nothing],
                S.selFrom = Just $ S.FromExp [S.FIIdentifier fromIdentifier]
              }
