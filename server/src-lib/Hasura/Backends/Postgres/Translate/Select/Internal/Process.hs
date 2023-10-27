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

import Data.HashMap.Strict qualified as HashMap
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.List.NonEmpty qualified as NE
import Data.Text.Extended (ToTxt (toTxt))
import Data.Text.NonEmpty qualified as TNE
import Hasura.Backends.Postgres.SQL.DML qualified as S
import Hasura.Backends.Postgres.SQL.Types
import Hasura.Backends.Postgres.Translate.BoolExp (toSQLBoolExp, withRedactionExp)
import Hasura.Backends.Postgres.Translate.Column (toJSONableExp)
import Hasura.Backends.Postgres.Translate.Select.AnnotatedFieldJSON
import Hasura.Backends.Postgres.Translate.Select.Internal.Aliases
  ( contextualizeAggregateInput,
    contextualizeBaseTableColumn,
    contextualizeField,
    mkAnnOrderByAlias,
    mkArrayRelationAlias,
    mkArrayRelationSourcePrefix,
    mkBaseTableIdentifier,
    mkComputedFieldTableIdentifier,
    mkObjectRelationTableAlias,
    mkOrderByFieldName,
  )
import Hasura.Backends.Postgres.Translate.Select.Internal.Extractor
  ( aggregateFieldsToExtractorExps,
    asJsonAggExtr,
    mkRawComputedFieldExpression,
    withColumnOp,
    withJsonAggExtr,
  )
import Hasura.Backends.Postgres.Translate.Select.Internal.Helpers
  ( cursorIdentifier,
    cursorsSelectAliasIdentifier,
    encodeBase64,
    endCursorIdentifier,
    fromTableRowArgs,
    hasNextPageIdentifier,
    hasPreviousPageIdentifier,
    nativeQueryNameToAlias,
    pageInfoSelectAliasIdentifier,
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
import Hasura.Backends.Postgres.Types.Aggregates
import Hasura.Base.Error (QErr)
import Hasura.GraphQL.Schema.Node (currentNodeIdVersion, nodeIdVersionInt)
import Hasura.NativeQuery.IR (NativeQuery (..))
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.IR.OrderBy (OrderByItemG (OrderByItemG, obiColumn))
import Hasura.RQL.IR.Select
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.NamingCase (NamingCase)
import Hasura.RQL.Types.Relationships.Local
import Hasura.RQL.Types.Schema.Options qualified as Options
import Hasura.RQL.Types.Session (UserInfo)

processSelectParams ::
  forall pgKind m.
  ( MonadReader Options.StringifyNumbers m,
    MonadState NativeQueryFreshIdStore m,
    MonadWriter SelectWriter m,
    Backend ('Postgres pgKind),
    MonadIO m,
    MonadError QErr m
  ) =>
  UserInfo ->
  SourcePrefixes ->
  FieldName ->
  SimilarArrayFields ->
  SelectFrom ('Postgres pgKind) ->
  PermissionLimitSubQuery ->
  TablePerm ('Postgres pgKind) ->
  SelectArgs ('Postgres pgKind) ->
  m
    ( SelectSource,
      [(S.ColumnAlias, S.SQLExp)],
      Maybe S.SQLExp -- Order by cursor
    )
processSelectParams
  userInfo
  sourcePrefixes
  fieldAlias
  similarArrFields
  selectFrom
  permLimitSubQ
  tablePermissions
  tableArgs = do
    let prefix = identifierToTableIdentifier $ _pfBase sourcePrefixes
    (selectSourceQual, fromItem) <- selectFromToQual prefix selectFrom
    (additionalExtrs, selectSorting, cursorExp) <-
      processOrderByItems userInfo (identifierToTableIdentifier thisSourcePrefix) selectSourceQual fieldAlias similarArrFields distM orderByM
    finalWhere <-
      toSQLBoolExp userInfo selectSourceQual
        $ maybe permFilter (andAnnBoolExps permFilter) whereM
    let sortingAndSlicing = SortingAndSlicing selectSorting selectSlicing
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
          (Just inp, Just perm) -> Just (min inp perm)

      selectFromToQual :: TableIdentifier -> SelectFrom ('Postgres pgKind) -> m (S.Qual, S.FromItem)
      selectFromToQual prefix = \case
        FromTable table -> pure $ (S.QualTable table, S.FISimple table Nothing)
        FromIdentifier i -> do
          let ti = TableIdentifier $ unFIIdentifier i
          pure $ (S.QualifiedIdentifier ti Nothing, S.FIIdentifier ti)
        FromFunction qf args defListM -> do
          let fi =
                S.FIFunc
                  $ S.FunctionExp qf (fromTableRowArgs prefix args)
                  $ Just
                  $ S.mkFunctionAlias
                    qf
                    (fmap (fmap (first S.toColumnAlias)) defListM)
          pure $ (S.QualifiedIdentifier (TableIdentifier $ qualifiedObjectToText qf) Nothing, fi)
        FromStoredProcedure {} -> error "selectFromToQual: FromStoredProcedure"
        FromNativeQuery nq -> do
          cteName <- fromNativeQuery nq
          let ta = S.tableAliasToIdentifier cteName
          pure $ (S.QualifiedIdentifier ta Nothing, S.FIIdentifier ta)

fromNativeQuery ::
  forall pgKind m.
  ( MonadWriter SelectWriter m,
    MonadState NativeQueryFreshIdStore m
  ) =>
  NativeQuery ('Postgres pgKind) S.SQLExp ->
  m S.TableAlias
fromNativeQuery nq = do
  freshId <- nqNextFreshId <$> get
  modify succ

  -- we are going to cram our SQL in a CTE, and this is what we will call it
  let cteName = nativeQueryNameToAlias (nqRootFieldName nq) freshId

  -- emit the query itself to the Writer
  tell
    $ mempty
      { _swCustomSQLCTEs =
          CustomSQLCTEs (HashMap.singleton cteName (nqInterpolatedQuery nq))
      }

  return cteName

processAnnAggregateSelect ::
  forall pgKind m.
  ( MonadReader Options.StringifyNumbers m,
    MonadWriter SelectWriter m,
    MonadState NativeQueryFreshIdStore m,
    Backend ('Postgres pgKind),
    PostgresAnnotatedFieldJSON pgKind,
    MonadIO m,
    MonadError QErr m
  ) =>
  UserInfo ->
  SourcePrefixes ->
  FieldName ->
  AnnAggregateSelect ('Postgres pgKind) ->
  m
    ( SelectSource,
      InsOrdHashMap S.ColumnAlias S.SQLExp,
      S.Extractor
    )
processAnnAggregateSelect userInfo sourcePrefixes fieldAlias annAggSel = do
  (selectSource, orderByAndDistinctExtrs, _) <-
    processSelectParams
      userInfo
      sourcePrefixes
      fieldAlias
      similarArrayFields
      tableFrom
      permLimitSubQuery
      tablePermissions
      tableArgs
  let thisSourcePrefix = identifierToTableIdentifier $ _pfThis sourcePrefixes
  processedFields <-
    traverse
      ( \(fieldName, field) -> do
          (fieldName,)
            <$> case field of
              TAFAgg aggFields -> do
                extractorExps <- aggregateFieldsToExtractorExps thisSourcePrefix userInfo aggFields
                pure
                  ( extractorExps,
                    aggregateFieldToExp thisSourcePrefix aggFields strfyNum
                  )
              TAFNodes _ annFields -> do
                let nodesColumnAlias = contextualizeField thisSourcePrefix fieldName
                (_, nodesExtractorExp) <- processAnnFields userInfo thisSourcePrefix fieldName annFields tCase
                pure
                  ( [(nodesColumnAlias, nodesExtractorExp)],
                    withJsonAggExtr permLimitSubQuery (orderByForJsonAgg selectSource) nodesColumnAlias
                  )
              TAFGroupBy _xGroupBy _groupBy -> error "processAnnAggregateSelect: group_by is not yet supported"
              TAFExp e ->
                pure
                  ( [],
                    withForceAggregation S.textTypeAnn $ S.SELit e
                  )
      )
      aggSelFields

  let topLevelExtractor =
        flip S.Extractor (Just $ S.toColumnAlias $ toIdentifier fieldAlias)
          $ S.applyJsonBuildObj
          $ flip concatMap (map (second snd) processedFields)
          $ \(FieldName fieldText, fieldExp) -> [S.SELit fieldText, fieldExp]
      nodeExtractors =
        InsOrdHashMap.fromList
          $ concatMap (fst . snd) processedFields
          <> orderByAndDistinctExtrs

  pure (selectSource, nodeExtractors, topLevelExtractor)
  where
    AnnSelectG aggSelFields tableFrom tablePermissions tableArgs strfyNum tCase = annAggSel
    permLimit = _tpLimit tablePermissions
    orderBy = _saOrderBy tableArgs
    permLimitSubQuery = mkPermissionLimitSubQuery permLimit aggSelFields orderBy
    similarArrayFields = HashMap.unions
      $ flip map (map snd aggSelFields)
      $ \case
        TAFAgg _ -> mempty
        TAFNodes _ annFlds ->
          mkSimilarArrayFields annFlds orderBy
        TAFGroupBy _xGroupBy _groupBy -> error "similarArrayFields: group_by is not yet supported"
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
        hasAggregateField = flip any (map snd aggFields)
          $ \case
            TAFAgg _ -> True
            _ -> False

        hasAggOrderBy = case orderBys of
          Nothing -> False
          Just l -> flip any (concatMap toList $ toList l)
            $ \case
              AOCArrayAggregation {} -> True
              _ -> False

processAnnFields ::
  forall pgKind m.
  ( MonadReader Options.StringifyNumbers m,
    MonadWriter SelectWriter m,
    MonadState NativeQueryFreshIdStore m,
    Backend ('Postgres pgKind),
    PostgresAnnotatedFieldJSON pgKind,
    MonadIO m,
    MonadError QErr m
  ) =>
  UserInfo ->
  TableIdentifier ->
  FieldName ->
  AnnFields ('Postgres pgKind) ->
  Maybe NamingCase ->
  m (S.ColumnAlias, S.SQLExp)
processAnnFields userInfo sourcePrefix fieldAlias annFields tCase = do
  fieldExps <- forM annFields $ \(fieldName, field) ->
    (fieldName,)
      <$> case field of
        AFExpression t -> pure $ S.SELit t
        AFNodeId _ sn tn pKeys -> pure $ mkNodeId sn tn pKeys
        AFColumn c -> toSQLCol c
        AFObjectRelation objSel -> withWriteObjectRelation $ do
          let AnnRelationSelectG relName relMapping nullable annObjSel = objSel
              AnnObjectSelectG objAnnFields target targetFilter = annObjSel
          (objRelSourcePrefix, ident, filterExp) <- case target of
            FromNativeQuery nq -> do
              cteName <- fromNativeQuery nq
              let nativeQueryIdentifier = S.tableAliasToIdentifier cteName
              boolExp <- toSQLBoolExp userInfo (S.QualifiedIdentifier nativeQueryIdentifier Nothing) targetFilter
              pure
                ( mkObjectRelationTableAlias
                    sourcePrefix
                    ( relName
                        { getRelTxt =
                            getRelTxt relName
                              <> TNE.mkNonEmptyTextUnsafe
                                ( getIdenTxt
                                    $ S.getTableAlias cteName
                                )
                        }
                    ),
                  S.FIIdentifier nativeQueryIdentifier,
                  boolExp
                )
            FromTable tableFrom -> do
              boolExp <- toSQLBoolExp userInfo (S.QualTable tableFrom) targetFilter
              pure
                ( mkObjectRelationTableAlias sourcePrefix relName,
                  S.FISimple tableFrom Nothing,
                  boolExp
                )
            other -> error $ "processAnnFields: " <> show other
          let sourcePrefixes = mkSourcePrefixes objRelSourcePrefix
              selectSource = ObjectSelectSource (_pfThis sourcePrefixes) ident filterExp
              objRelSource = ObjectRelationSource relName relMapping selectSource nullable
          annFieldsExtr <- processAnnFields userInfo (identifierToTableIdentifier $ _pfThis sourcePrefixes) fieldName objAnnFields tCase
          pure
            ( objRelSource,
              uncurry InsOrdHashMap.singleton annFieldsExtr,
              S.mkQIdenExp objRelSourcePrefix fieldName
            )
        AFArrayRelation arrSel -> do
          let arrRelSourcePrefix = mkArrayRelationSourcePrefix sourcePrefix fieldAlias HashMap.empty fieldName
              arrRelAlias = mkArrayRelationAlias fieldAlias HashMap.empty fieldName
          processArrayRelation userInfo (mkSourcePrefixes arrRelSourcePrefix) fieldName arrRelAlias arrSel tCase
          pure $ S.mkQIdenExp arrRelSourcePrefix fieldName
        AFComputedField _ _ (CFSScalar scalar) -> do
          fromScalarComputedField scalar
        AFComputedField _ _ (CFSTable selectTy sel) -> withWriteComputedFieldTableSet $ do
          let computedFieldSourcePrefix =
                mkComputedFieldTableIdentifier sourcePrefix fieldName
          (selectSource, nodeExtractors) <-
            processAnnSimpleSelect
              userInfo
              (mkSourcePrefixes computedFieldSourcePrefix)
              fieldName
              PLSQNotRequired
              sel
          let computedFieldTableSetSource = ComputedFieldTableSetSource fieldName selectSource
              extractor =
                asJsonAggExtr selectTy (S.toColumnAlias fieldName) PLSQNotRequired
                  $ orderByForJsonAgg selectSource
          pure
            ( computedFieldTableSetSource,
              extractor,
              nodeExtractors,
              S.mkQIdenExp computedFieldSourcePrefix fieldName
            )

  pure $ annRowToJson @pgKind fieldAlias fieldExps
  where
    mkSourcePrefixes newPrefix = SourcePrefixes (tableIdentifierToIdentifier newPrefix) (tableIdentifierToIdentifier sourcePrefix)

    baseTableIdentifier = mkBaseTableIdentifier sourcePrefix

    toSQLCol :: AnnColumnField ('Postgres pgKind) S.SQLExp -> m S.SQLExp
    toSQLCol (AnnColumnField col typ asText colOpM redactionExp) = do
      strfyNum <- ask
      sqlExpression <-
        withRedactionExp (S.QualifiedIdentifier baseTableIdentifier Nothing) redactionExp userInfo
          $ withColumnOp colOpM
          $ S.mkQIdenExp baseTableIdentifier col
      pure $ toJSONableExp strfyNum typ asText tCase sqlExpression

    fromScalarComputedField :: ComputedFieldScalarSelect ('Postgres pgKind) S.SQLExp -> m S.SQLExp
    fromScalarComputedField computedFieldScalar@ComputedFieldScalarSelect {..} = do
      strfyNum <- ask
      computedFieldExp <- mkRawComputedFieldExpression sourcePrefix userInfo computedFieldScalar
      pure
        $ toJSONableExp strfyNum (ColumnScalar _cfssType) False Nothing
        $ computedFieldExp

    mkNodeId :: SourceName -> QualifiedTable -> PrimaryKeyColumns ('Postgres pgKind) -> S.SQLExp
    mkNodeId _sourceName (QualifiedObject tableSchema tableName) pkeyColumns =
      let columnInfoToSQLExp pgColumnInfo =
            toJSONableExp Options.Don'tStringifyNumbers (ciType pgColumnInfo) False Nothing
              $ S.mkQIdenExp (mkBaseTableIdentifier sourcePrefix)
              $ ciColumn pgColumnInfo
       in -- See Note [Relay Node id].
          encodeBase64
            $ flip S.SETyAnn S.textTypeAnn
            $ S.applyJsonBuildArray
            $ [ S.intToSQLExp $ nodeIdVersionInt currentNodeIdVersion,
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
  HashMap.fromList
    $ flip map allTuples
    $ \(relNameAndArgs, fieldName) -> (fieldName, getSimilarFields relNameAndArgs)
  where
    getSimilarFields relNameAndArgs = map snd $ filter ((== relNameAndArgs) . fst) allTuples
    allTuples = arrayRelationTuples <> aggOrderByRelationTuples
    arrayRelationTuples =
      let arrayFields = mapMaybe getAnnArr annFields
       in flip map arrayFields
            $ \(f, relSel) -> (getArrayRelNameAndSelectArgs relSel, f)

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
       in map mkItem
            $ maybe
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
  ( MonadReader Options.StringifyNumbers m,
    MonadWriter SelectWriter m,
    MonadState NativeQueryFreshIdStore m,
    Backend ('Postgres pgKind),
    PostgresAnnotatedFieldJSON pgKind,
    MonadIO m,
    MonadError QErr m
  ) =>
  UserInfo ->
  SourcePrefixes ->
  FieldName ->
  S.TableAlias ->
  ArraySelect ('Postgres pgKind) ->
  Maybe NamingCase ->
  m ()
processArrayRelation userInfo sourcePrefixes fieldAlias relAlias arrSel _tCase =
  case arrSel of
    ASSimple annArrRel -> withWriteArrayRelation $ do
      let AnnRelationSelectG _ colMapping _ sel = annArrRel
          permLimitSubQuery =
            maybe PLSQNotRequired PLSQRequired $ _tpLimit $ _asnPerm sel
      (source, nodeExtractors) <-
        processAnnSimpleSelect userInfo sourcePrefixes fieldAlias permLimitSubQuery sel
      let topExtr =
            asJsonAggExtr
              JASMultipleRows
              (S.toColumnAlias fieldAlias)
              permLimitSubQuery
              $ orderByForJsonAgg source
      pure
        ( ArrayRelationSource relAlias colMapping source,
          topExtr,
          nodeExtractors,
          ()
        )
    ASAggregate aggSel -> withWriteArrayRelation $ do
      let AnnRelationSelectG _ colMapping _ sel = aggSel
      (source, nodeExtractors, topExtr) <-
        processAnnAggregateSelect userInfo sourcePrefixes fieldAlias sel
      pure
        ( ArrayRelationSource relAlias colMapping source,
          topExtr,
          nodeExtractors,
          ()
        )
    ASConnection connSel -> withWriteArrayConnection $ do
      let AnnRelationSelectG _ colMapping _ sel = connSel
      (source, topExtractor, nodeExtractors) <-
        processConnectionSelect userInfo sourcePrefixes fieldAlias relAlias colMapping sel
      pure
        ( source,
          topExtractor,
          nodeExtractors,
          ()
        )

aggregateFieldToExp ::
  forall pgKind.
  TableIdentifier ->
  AggregateFields ('Postgres pgKind) S.SQLExp ->
  Options.StringifyNumbers ->
  S.SQLExp
aggregateFieldToExp sourcePrefix aggregateFields strfyNum =
  S.applyJsonBuildObj (concatMap aggregateFieldToExtractor aggregateFields)
  where
    withAlias :: FieldName -> S.SQLExp -> [S.SQLExp]
    withAlias (FieldName fldName) sqlExp = [S.SELit fldName, sqlExp]

    aggregateFieldToExtractor ::
      (FieldName, AggregateField ('Postgres pgKind) S.SQLExp) ->
      [S.SQLExp]
    aggregateFieldToExtractor (aggregateFieldName, field) = withAlias aggregateFieldName $ case field of
      AFCount countAggregate -> S.SECount $ columnToQIdentifier . fst <$> getCountType countAggregate
      AFOp aggOp -> aggregateOpToObj aggregateFieldName aggOp
      AFExp e -> S.SELit e

    aggregateOpToObj :: FieldName -> AggregateOp ('Postgres pgKind) S.SQLExp -> S.SQLExp
    aggregateOpToObj aggregateFieldName (AggregateOp opText selectionFields) =
      S.applyJsonBuildObj $ concatMap (selectionFieldToExtractor aggregateFieldName opText) selectionFields

    selectionFieldToExtractor ::
      FieldName ->
      Text ->
      (FieldName, SelectionField ('Postgres pgKind) S.SQLExp) ->
      [S.SQLExp]
    selectionFieldToExtractor aggregateFieldName opText (fieldName, selectionField) =
      withAlias fieldName $ case selectionField of
        SFCol col ty _redactionExp ->
          toJSONableExp strfyNum ty False Nothing
            $ S.SEFnApp opText [S.SEQIdentifier $ columnToQIdentifier col] Nothing
        SFComputedField _cfName ComputedFieldScalarSelect {..} ->
          toJSONableExp strfyNum (ColumnScalar _cfssType) False Nothing
            $ S.SEFnApp opText [S.SEQIdentifier $ computedFieldToQIdentifier aggregateFieldName fieldName] Nothing
        SFExp e ->
          S.SELit e

    columnToQIdentifier :: PGCol -> S.QIdentifier
    columnToQIdentifier = S.mkQIdentifier sourcePrefix . contextualizeBaseTableColumn sourcePrefix

    computedFieldToQIdentifier :: FieldName -> FieldName -> S.QIdentifier
    computedFieldToQIdentifier aggregateFieldName fieldName = S.mkQIdentifier sourcePrefix $ contextualizeAggregateInput sourcePrefix aggregateFieldName fieldName

processAnnSimpleSelect ::
  forall pgKind m.
  ( MonadReader Options.StringifyNumbers m,
    MonadState NativeQueryFreshIdStore m,
    MonadWriter SelectWriter m,
    Backend ('Postgres pgKind),
    PostgresAnnotatedFieldJSON pgKind,
    MonadIO m,
    MonadError QErr m
  ) =>
  UserInfo ->
  SourcePrefixes ->
  FieldName ->
  PermissionLimitSubQuery ->
  AnnSimpleSelect ('Postgres pgKind) ->
  m
    ( SelectSource,
      InsOrdHashMap S.ColumnAlias S.SQLExp
    )
processAnnSimpleSelect userInfo sourcePrefixes fieldAlias permLimitSubQuery annSimpleSel = do
  (selectSource, orderByAndDistinctExtrs, _) <-
    processSelectParams
      userInfo
      sourcePrefixes
      fieldAlias
      similarArrayFields
      tableFrom
      permLimitSubQuery
      tablePermissions
      tableArgs
  annFieldsExtr <-
    processAnnFields
      userInfo
      (identifierToTableIdentifier $ _pfThis sourcePrefixes)
      fieldAlias
      annSelFields
      tCase
  let allExtractors = InsOrdHashMap.fromList $ annFieldsExtr : orderByAndDistinctExtrs
  pure (selectSource, allExtractors)
  where
    AnnSelectG annSelFields tableFrom tablePermissions tableArgs _ tCase = annSimpleSel
    similarArrayFields =
      mkSimilarArrayFields annSelFields $ _saOrderBy tableArgs

processConnectionSelect ::
  forall pgKind m.
  ( MonadReader Options.StringifyNumbers m,
    MonadWriter SelectWriter m,
    MonadState NativeQueryFreshIdStore m,
    Backend ('Postgres pgKind),
    PostgresAnnotatedFieldJSON pgKind,
    MonadIO m,
    MonadError QErr m
  ) =>
  UserInfo ->
  SourcePrefixes ->
  FieldName ->
  S.TableAlias ->
  HashMap.HashMap PGCol PGCol ->
  ConnectionSelect ('Postgres pgKind) Void S.SQLExp ->
  m
    ( ArrayConnectionSource,
      S.Extractor,
      InsOrdHashMap S.ColumnAlias S.SQLExp
    )
processConnectionSelect userInfo sourcePrefixes fieldAlias relAlias colMapping connectionSelect = do
  (selectSource, orderByAndDistinctExtrs, maybeOrderByCursor) <-
    processSelectParams
      userInfo
      sourcePrefixes
      fieldAlias
      similarArrayFields
      selectFrom
      permLimitSubQuery
      tablePermissions
      tableArgs

  let mkCursorExtractor = (S.toColumnAlias cursorIdentifier,) . (`S.SETyAnn` S.textTypeAnn)
      cursorExtractors = case maybeOrderByCursor of
        Just orderByCursor -> [mkCursorExtractor orderByCursor]
        Nothing ->
          -- Extract primary key columns from base select along with cursor expression.
          -- Those columns are required to perform connection split via a WHERE clause.
          mkCursorExtractor primaryKeyColumnsObjectExp : primaryKeyColumnExtractors
  (topExtractorExp, exps) <- flip runStateT [] $ processFields selectSource
  let topExtractor = S.Extractor topExtractorExp $ Just $ S.toColumnAlias fieldIdentifier
      allExtractors = InsOrdHashMap.fromList $ cursorExtractors <> exps <> orderByAndDistinctExtrs
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
    AnnSelectG fields selectFrom tablePermissions tableArgs _ tCase = select
    fieldIdentifier = toIdentifier fieldAlias
    thisPrefix = identifierToTableIdentifier $ _pfThis sourcePrefixes
    permLimitSubQuery = PLSQNotRequired

    primaryKeyColumnsObjectExp =
      S.applyJsonBuildObj
        $ flip concatMap (toList primaryKeyColumns)
        $ \pgColumnInfo ->
          [ S.SELit $ getPGColTxt $ ciColumn pgColumnInfo,
            toJSONableExp Options.Don'tStringifyNumbers (ciType pgColumnInfo) False tCase
              $ S.mkQIdenExp (mkBaseTableIdentifier thisPrefix)
              $ ciColumn pgColumnInfo
          ]

    primaryKeyColumnExtractors =
      flip map (toList primaryKeyColumns)
        $ \pgColumnInfo ->
          let pgColumn = ciColumn pgColumnInfo
           in ( contextualizeBaseTableColumn thisPrefix pgColumn,
                S.mkQIdenExp (mkBaseTableIdentifier thisPrefix) pgColumn
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
                mkAnnOrderByAlias thisPrefix fieldAlias similarArrayFields
                  $ obiColumn orderByItem
           in S.BECompare S.SEQ (S.SEIdentifier $ toIdentifier obAlias) v

    similarArrayFields = HashMap.unions
      $ flip map (map snd fields)
      $ \case
        ConnectionTypename {} -> mempty
        ConnectionPageInfo {} -> mempty
        ConnectionEdges edges -> HashMap.unions
          $ flip map (map snd edges)
          $ \case
            EdgeTypename {} -> mempty
            EdgeCursor {} -> mempty
            EdgeNode annFields ->
              mkSimilarArrayFields annFields $ _saOrderBy tableArgs

    mkSimpleJsonAgg rowExp ob =
      let jsonAggExp = S.SEFnApp "json_agg" [rowExp] ob
       in S.SEFnApp "coalesce" [jsonAggExp, S.SELit "[]"] Nothing

    processFields ::
      forall n n' t.
      ( MonadState [(S.ColumnAlias, S.SQLExp)] n,
        -- Constraints for 'processAnnFields':
        n ~ (t n'),
        MonadTrans t,
        MonadState NativeQueryFreshIdStore n',
        MonadWriter SelectWriter n',
        MonadReader Options.StringifyNumbers n',
        MonadIO n',
        MonadError QErr n'
      ) =>
      SelectSource ->
      n S.SQLExp
    processFields selectSource =
      fmap (S.applyJsonBuildObj . concat)
        $ forM fields
        $ \(FieldName fieldText, field) ->
          (S.SELit fieldText :)
            . pure
            <$> case field of
              ConnectionTypename t -> pure $ withForceAggregation S.textTypeAnn $ S.SELit t
              ConnectionPageInfo pageInfoFields -> pure $ processPageInfoFields pageInfoFields
              ConnectionEdges edges ->
                fmap (flip mkSimpleJsonAgg (orderByForJsonAgg selectSource) . S.applyJsonBuildObj . concat)
                  $ forM edges
                  $ \(FieldName edgeText, edge) ->
                    (S.SELit edgeText :)
                      . pure
                      <$> case edge of
                        EdgeTypename t -> pure $ S.SELit t
                        EdgeCursor -> pure $ encodeBase64 $ S.SEIdentifier (toIdentifier cursorIdentifier)
                        EdgeNode annFields -> do
                          let edgeFieldName =
                                FieldName
                                  $ getFieldNameTxt fieldAlias
                                  <> "."
                                  <> fieldText
                                  <> "."
                                  <> edgeText
                              edgeFieldIdentifier = toIdentifier edgeFieldName
                          annFieldsExtrExp <- lift $ processAnnFields userInfo thisPrefix edgeFieldName annFields tCase
                          modify' (<> [annFieldsExtrExp])
                          pure $ S.SEIdentifier edgeFieldIdentifier

    processPageInfoFields infoFields =
      S.applyJsonBuildObj
        $ flip concatMap infoFields
        $ \(FieldName fieldText, field) -> (:) (S.SELit fieldText) $ pure case field of
          PageInfoTypename t -> withForceAggregation S.textTypeAnn $ S.SELit t
          PageInfoHasNextPage ->
            withForceAggregation S.boolTypeAnn
              $ mkSingleFieldSelect (S.SEIdentifier hasNextPageIdentifier) pageInfoSelectAliasIdentifier
          PageInfoHasPreviousPage ->
            withForceAggregation S.boolTypeAnn
              $ mkSingleFieldSelect (S.SEIdentifier hasPreviousPageIdentifier) pageInfoSelectAliasIdentifier
          PageInfoStartCursor ->
            withForceAggregation S.textTypeAnn
              $ encodeBase64
              $ mkSingleFieldSelect (S.SEIdentifier startCursorIdentifier) cursorsSelectAliasIdentifier
          PageInfoEndCursor ->
            withForceAggregation S.textTypeAnn
              $ encodeBase64
              $ mkSingleFieldSelect (S.SEIdentifier endCursorIdentifier) cursorsSelectAliasIdentifier
      where
        mkSingleFieldSelect field fromIdentifier =
          S.SESelect
            S.mkSelect
              { S.selExtr = [S.Extractor field Nothing],
                S.selFrom = Just $ S.FromExp [S.FIIdentifier fromIdentifier]
              }
