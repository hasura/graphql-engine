-- | Postgres Translate Select
--
-- This module is a translation layer between IR and postgres-specific select queries.
--
-- There are three main types of selects (as distinguished from the IR):
--
--     * "simple" selects
--
--     * aggregate selects
--
--     * connection selects (used for relay)
--
-- Most exports from this module showcase this distinction. The "interesting" parts
-- of the call tree of these functions is similar:
--
--     * 'selectQuerySQL' -> 'mkSQLSelect' -> 'processAnnSimpleSelect' -> 'processSelectParams'/'processAnnFields'
--
--     * 'selectAggregateQuerySQL' -> 'mkAggregateSelect' -> 'processAnnAggregateSelect' -> 'processSelectParams'/'processAnnFields'
--
--     * 'connetionSelectQuerySQL' -> 'mkConnectionSelect' -> 'processConnectionSelection' -> 'processSelectParams'
--
--
-- Random thoughts that might help when diving deeper in this module:
--
--     * Extractors are a pair of an SQL expression and an alias; they get
--         translated like "[SELECT ...] <expr> as <alias>"
--     * a 'SelectSource' consists of a prefix, a source, a boolean conditional
--         expression, and info on whether sorting or slicing is done
--         (needed to handle the LIMIT optimisation)
--     * For details on creating the selection tree for relationships via
--       @MonadWriter JoinTree@, see 'withWriteJoinTree'
module Hasura.Backends.Postgres.Translate.Select
  ( selectQuerySQL,
    selectStreamQuerySQL,
    selectAggregateQuerySQL,
    connectionSelectQuerySQL,
    asSingleRowJsonResp,
    mkSQLSelect,
    mkStreamSQLSelect,
    mkAggregateSelect,
    mkConnectionSelect,
    PostgresAnnotatedFieldJSON,
  )
where

import Control.Lens ((^?))
import Control.Monad.Writer.Strict
import Data.HashMap.Strict qualified as HM
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Text.Extended
import Database.PG.Query qualified as Q
import Hasura.Backends.Postgres.SQL.DML qualified as S
import Hasura.Backends.Postgres.SQL.IdentifierUniqueness
import Hasura.Backends.Postgres.SQL.Types
import Hasura.Backends.Postgres.SQL.Value (withConstructorFn)
import Hasura.Backends.Postgres.Translate.BoolExp
import Hasura.Backends.Postgres.Translate.Column (toJSONableExp)
import Hasura.Backends.Postgres.Translate.Types
import Hasura.Backends.Postgres.Types.Column (unsafePGColumnToBackend)
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.GraphQL.Schema.Common (currentNodeIdVersion, nodeIdVersionInt)
import Hasura.Prelude
import Hasura.RQL.DML.Internal
import Hasura.RQL.IR.OrderBy
import Hasura.RQL.IR.Select
import Hasura.RQL.Types
import Hasura.SQL.Types

-- | Translates IR to Postgres queries for simple SELECTs (select queries that
-- are not aggregations, including subscriptions).
--
-- See 'mkSQLSelect' for the Postgres AST.
selectQuerySQL ::
  forall pgKind.
  (Backend ('Postgres pgKind), PostgresAnnotatedFieldJSON pgKind) =>
  JsonAggSelect ->
  AnnSimpleSelect ('Postgres pgKind) ->
  Q.Query
selectQuerySQL jsonAggSelect sel =
  Q.fromBuilder $ toSQL $ mkSQLSelect jsonAggSelect sel

selectStreamQuerySQL ::
  forall pgKind.
  (Backend ('Postgres pgKind), PostgresAnnotatedFieldJSON pgKind) =>
  AnnSimpleStreamSelect ('Postgres pgKind) ->
  Q.Query
selectStreamQuerySQL sel =
  Q.fromBuilder $ toSQL $ mkStreamSQLSelect sel

-- | Translates IR to Postgres queries for aggregated SELECTs.
--
-- See 'mkAggregateSelect' for the Postgres AST.
selectAggregateQuerySQL ::
  forall pgKind.
  (Backend ('Postgres pgKind), PostgresAnnotatedFieldJSON pgKind) =>
  AnnAggregateSelect ('Postgres pgKind) ->
  Q.Query
selectAggregateQuerySQL =
  Q.fromBuilder . toSQL . mkAggregateSelect

-- | Translates IR to Postgres queries for "connection" queries (used for Relay).
--
-- See 'mkConnectionSelect' for the Postgres AST.
connectionSelectQuerySQL ::
  forall pgKind.
  ( Backend ('Postgres pgKind),
    PostgresAnnotatedFieldJSON pgKind
  ) =>
  ConnectionSelect ('Postgres pgKind) Void S.SQLExp ->
  Q.Query
connectionSelectQuerySQL =
  Q.fromBuilder . toSQL . mkConnectionSelect

-- | Helper function with no relation to anything else in the module.
--
-- This function is generally used on the result of 'selectQuerySQL',
-- 'selectAggregateQuerySQL' or 'connectionSelectSQL' to run said query and get
-- back the resulting JSON.
--
-- TODO: Perhaps this helper could find a new home.
asSingleRowJsonResp ::
  Q.Query ->
  [Q.PrepArg] ->
  Q.TxE QErr EncJSON
asSingleRowJsonResp query args =
  encJFromBS . runIdentity . Q.getRow
    <$> Q.rawQE dmlTxErrorHandler query args True

-- | Converts a function name to an 'Identifier'.
--
-- If the schema name is public, it will just use its name, otherwise it will
-- prefix it by the schema name.
functionToIdentifier :: QualifiedFunction -> Identifier
functionToIdentifier = Identifier . qualifiedObjectToText

selectFromToFromItem :: Identifier -> SelectFrom ('Postgres pgKind) -> S.FromItem
selectFromToFromItem pfx = \case
  FromTable tn -> S.FISimple tn Nothing
  FromIdentifier i -> S.FIIdentifier $ toIdentifier i
  FromFunction qf args defListM ->
    S.FIFunc $
      S.FunctionExp qf (fromTableRowArgs pfx args) $
        Just $ S.mkFunctionAlias (functionToIdentifier qf) defListM

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

asSingleRowExtr :: S.Alias -> S.SQLExp
asSingleRowExtr col =
  S.SEFnApp "coalesce" [jsonAgg, S.SELit "null"] Nothing
  where
    jsonAgg =
      S.SEOpApp
        (S.SQLOp "->")
        [ S.SEFnApp "json_agg" [S.SEIdentifier $ toIdentifier col] Nothing,
          S.SEUnsafe "0"
        ]

withJsonAggExtr ::
  PermissionLimitSubQuery -> Maybe S.OrderByExp -> S.Alias -> S.SQLExp
withJsonAggExtr permLimitSubQuery ordBy alias =
  -- if select has aggregations then use subquery to apply permission limit
  case permLimitSubQuery of
    PLSQRequired permLimit -> withPermLimit permLimit
    PLSQNotRequired -> simpleJsonAgg
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
          fromExp =
            S.FromExp $
              pure $
                S.mkSelFromItem subSelect $ S.Alias subSelAls
       in S.SESelect $
            S.mkSelect
              { S.selExtr = pure extr,
                S.selFrom = Just fromExp
              }

    mkSubSelect limit =
      let jsonRowExtr =
            flip S.Extractor (Just alias) $
              S.mkQIdenExp unnestTable alias
          obExtrs = flip map newOBAliases $ \a ->
            S.Extractor (S.mkQIdenExp unnestTable a) $ Just $ S.Alias a
       in S.mkSelect
            { S.selExtr = jsonRowExtr : obExtrs,
              S.selFrom = Just $ S.FromExp $ pure unnestFromItem,
              S.selLimit = Just $ S.LimitExp $ S.intToSQLExp limit,
              S.selOrderBy = newOrderBy
            }

    unnestFromItem =
      let arrayAggItems = flip map (rowIdenExp : obCols) $
            \s -> S.SEFnApp "array_agg" [s] Nothing
       in S.FIUnnest arrayAggItems (S.Alias unnestTable) $
            rowIdenExp : map S.SEIdentifier newOBAliases

    newOrderBy = S.OrderByExp <$> NE.nonEmpty newOBItems

    (newOBItems, obCols, newOBAliases) = maybe ([], [], []) transformOrderBy ordBy
    transformOrderBy (S.OrderByExp l) = unzip3 $
      flip map (zip (toList l) [1 ..]) $ \(obItem, i :: Int) ->
        let iden = Identifier $ "ob_col_" <> tshow i
         in ( obItem {S.oColumn = S.SEIdentifier iden},
              S.oColumn obItem,
              iden
            )

asJsonAggExtr ::
  JsonAggSelect -> S.Alias -> PermissionLimitSubQuery -> Maybe S.OrderByExp -> S.Extractor
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

mkOrderByFieldName :: ToTxt a => a -> FieldName
mkOrderByFieldName name =
  FieldName $ toTxt name <> "." <> "order_by"

mkAggregateOrderByAlias :: AnnotatedAggregateOrderBy ('Postgres pgKind) -> S.Alias
mkAggregateOrderByAlias =
  (S.Alias . Identifier) . \case
    AAOCount -> "count"
    AAOOp opText col -> opText <> "." <> getPGColTxt (ciColumn col)

mkArrayRelationSourcePrefix ::
  Identifier ->
  FieldName ->
  HM.HashMap FieldName [FieldName] ->
  FieldName ->
  Identifier
mkArrayRelationSourcePrefix parentSourcePrefix parentFieldName similarFieldsMap fieldName =
  mkArrayRelationTableAlias parentSourcePrefix parentFieldName $
    HM.lookupDefault [fieldName] fieldName similarFieldsMap

mkArrayRelationAlias ::
  FieldName ->
  HM.HashMap FieldName [FieldName] ->
  FieldName ->
  S.Alias
mkArrayRelationAlias parentFieldName similarFieldsMap fieldName =
  S.Alias $
    mkUniqArrayRelationAlias parentFieldName $
      HM.lookupDefault [fieldName] fieldName similarFieldsMap

fromTableRowArgs ::
  Identifier -> FunctionArgsExpTableRow S.SQLExp -> S.FunctionArgs
fromTableRowArgs prefix = toFunctionArgs . fmap toSQLExp
  where
    toFunctionArgs (FunctionArgsExp positional named) =
      S.FunctionArgs positional named
    toSQLExp =
      onArgumentExp
        (S.SERowIdentifier alias)
        (S.mkQIdenExp alias . Identifier)
    alias = mkBaseTableAlias prefix

-- uses row_to_json to build a json object
withRowToJSON ::
  FieldName -> [S.Extractor] -> (S.Alias, S.SQLExp)
withRowToJSON parAls extrs =
  (S.toAlias parAls, jsonRow)
  where
    jsonRow = S.applyRowToJson extrs

-- uses json_build_object to build a json object
withJsonBuildObj ::
  FieldName -> [S.SQLExp] -> (S.Alias, S.SQLExp)
withJsonBuildObj parAls exps =
  (S.toAlias parAls, jsonRow)
  where
    jsonRow = S.applyJsonBuildObj exps

-- | Forces aggregation
withForceAggregation :: S.TypeAnn -> S.SQLExp -> S.SQLExp
withForceAggregation tyAnn e =
  -- bool_or to force aggregation
  S.SEFnApp "coalesce" [e, S.SETyAnn (S.SEUnsafe "bool_or('true')") tyAnn] Nothing

mkAggregateOrderByExtractorAndFields ::
  forall pgKind.
  Backend ('Postgres pgKind) =>
  AnnotatedAggregateOrderBy ('Postgres pgKind) ->
  (S.Extractor, AggregateFields ('Postgres pgKind))
mkAggregateOrderByExtractorAndFields annAggOrderBy =
  case annAggOrderBy of
    AAOCount ->
      ( S.Extractor S.countStar alias,
        [(FieldName "count", AFCount S.CTStar)]
      )
    AAOOp opText pgColumnInfo ->
      let pgColumn = ciColumn pgColumnInfo
          pgType = ciType pgColumnInfo
       in ( S.Extractor (S.SEFnApp opText [S.SEIdentifier $ toIdentifier pgColumn] Nothing) alias,
            [ ( FieldName opText,
                AFOp $
                  AggregateOp
                    opText
                    [ ( fromCol @('Postgres pgKind) pgColumn,
                        CFCol pgColumn pgType
                      )
                    ]
              )
            ]
          )
  where
    alias = Just $ mkAggregateOrderByAlias annAggOrderBy

-- | Generate alias for order by extractors
mkAnnOrderByAlias ::
  Identifier -> FieldName -> SimilarArrayFields -> AnnotatedOrderByElement ('Postgres pgKind) v -> S.Alias
mkAnnOrderByAlias pfx parAls similarFields = \case
  AOCColumn pgColumnInfo ->
    let pgColumn = ciColumn pgColumnInfo
        obColAls = mkBaseTableColumnAlias pfx pgColumn
     in S.Alias obColAls
  -- "pfx.or.relname"."pfx.ob.or.relname.rest" AS "pfx.ob.or.relname.rest"
  AOCObjectRelation relInfo _ rest ->
    let rn = riName relInfo
        relPfx = mkObjectRelationTableAlias pfx rn
        ordByFldName = mkOrderByFieldName rn
        nesAls = mkAnnOrderByAlias relPfx ordByFldName mempty rest
     in nesAls
  AOCArrayAggregation relInfo _ aggOrderBy ->
    let rn = riName relInfo
        arrPfx =
          mkArrayRelationSourcePrefix pfx parAls similarFields $
            mkOrderByFieldName rn
        obAls = arrPfx <> Identifier "." <> toIdentifier (mkAggregateOrderByAlias aggOrderBy)
     in S.Alias obAls
  AOCComputedField cfOrderBy ->
    let fieldName = fromComputedField $ _cfobName cfOrderBy
     in case _cfobOrderByElement cfOrderBy of
          CFOBEScalar _ -> S.Alias $ mkComputedFieldTableAlias pfx fieldName
          CFOBETableAggregation _ _ aggOrderBy ->
            let cfPfx = mkComputedFieldTableAlias pfx fieldName
                obAls = cfPfx <> Identifier "." <> toIdentifier (mkAggregateOrderByAlias aggOrderBy)
             in S.Alias obAls

applyDistinctOnAtBase ::
  NE.NonEmpty PGCol -> S.DistinctExpr
applyDistinctOnAtBase =
  S.DistinctOn . map (S.SEIdentifier . toIdentifier) . toList

applyDistinctOnAtNode ::
  Identifier ->
  NE.NonEmpty PGCol ->
  ( S.DistinctExpr,
    [(S.Alias, S.SQLExp)] -- additional column extractors
  )
applyDistinctOnAtNode pfx neCols = (distOnExp, colExtrs)
  where
    cols = toList neCols
    distOnExp = S.DistinctOn $ map (S.SEIdentifier . toIdentifier . mkQColAls) cols
    mkQCol c = S.mkQIdenExp (mkBaseTableAlias pfx) $ toIdentifier c
    mkQColAls = S.Alias . mkBaseTableColumnAlias pfx
    colExtrs = flip map cols $ mkQColAls &&& mkQCol

type SimilarArrayFields = HM.HashMap FieldName [FieldName]

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

getAnnArr ::
  (a, AnnFieldG ('Postgres pgKind) r v) ->
  Maybe (a, ArraySelectG ('Postgres pgKind) r v)
getAnnArr (f, annFld) = case annFld of
  AFArrayRelation (ASConnection _) -> Nothing
  AFArrayRelation ar -> Just (f, ar)
  _ -> Nothing

-- | This is the lowest level function which deals with @MonadWriter JoinTree@, whose
-- purpose is to essentially create the selection tree across relationships.
--
-- Each type of relationship uses a different kind of update function; see
-- 'withWriteObjectRelation', 'withWriteArrayRelation', 'withWriteArrayConnection',
-- and 'withWriteComputedFieldTableSet'.
--
-- See the definition of 'JoinTree' for details before diving further
-- (particularly its components and Monoid instance).
withWriteJoinTree ::
  (MonadWriter JoinTree m) =>
  (JoinTree -> b -> JoinTree) ->
  m (a, b) ->
  m a
withWriteJoinTree joinTreeUpdater action =
  pass $ do
    (out, result) <- action
    let fromJoinTree joinTree =
          joinTreeUpdater joinTree result
    pure (out, fromJoinTree)

withWriteObjectRelation ::
  (MonadWriter JoinTree m) =>
  m
    ( ObjectRelationSource,
      HM.HashMap S.Alias S.SQLExp,
      a
    ) ->
  m a
withWriteObjectRelation action =
  withWriteJoinTree updateJoinTree $ do
    (source, nodeExtractors, out) <- action
    pure (out, (source, nodeExtractors))
  where
    updateJoinTree joinTree (source, nodeExtractors) =
      let selectNode = SelectNode nodeExtractors joinTree
       in mempty {_jtObjectRelations = HM.singleton source selectNode}

withWriteArrayRelation ::
  (MonadWriter JoinTree m) =>
  m
    ( ArrayRelationSource,
      S.Extractor,
      HM.HashMap S.Alias S.SQLExp,
      a
    ) ->
  m a
withWriteArrayRelation action =
  withWriteJoinTree updateJoinTree $ do
    (source, topExtractor, nodeExtractors, out) <- action
    pure (out, (source, topExtractor, nodeExtractors))
  where
    updateJoinTree joinTree (source, topExtractor, nodeExtractors) =
      let arraySelectNode =
            MultiRowSelectNode [topExtractor] $
              SelectNode nodeExtractors joinTree
       in mempty {_jtArrayRelations = HM.singleton source arraySelectNode}

withWriteArrayConnection ::
  (MonadWriter JoinTree m) =>
  m
    ( ArrayConnectionSource,
      S.Extractor,
      HM.HashMap S.Alias S.SQLExp,
      a
    ) ->
  m a
withWriteArrayConnection action =
  withWriteJoinTree updateJoinTree $ do
    (source, topExtractor, nodeExtractors, out) <- action
    pure (out, (source, topExtractor, nodeExtractors))
  where
    updateJoinTree joinTree (source, topExtractor, nodeExtractors) =
      let arraySelectNode =
            MultiRowSelectNode [topExtractor] $
              SelectNode nodeExtractors joinTree
       in mempty {_jtArrayConnections = HM.singleton source arraySelectNode}

withWriteComputedFieldTableSet ::
  (MonadWriter JoinTree m) =>
  m
    ( ComputedFieldTableSetSource,
      S.Extractor,
      HM.HashMap S.Alias S.SQLExp,
      a
    ) ->
  m a
withWriteComputedFieldTableSet action =
  withWriteJoinTree updateJoinTree $ do
    (source, topExtractor, nodeExtractors, out) <- action
    pure (out, (source, topExtractor, nodeExtractors))
  where
    updateJoinTree joinTree (source, topExtractor, nodeExtractors) =
      let selectNode = MultiRowSelectNode [topExtractor] $ SelectNode nodeExtractors joinTree
       in mempty {_jtComputedFieldTableSets = HM.singleton source selectNode}

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
mkPermissionLimitSubQuery permLimit aggFields orderBys =
  case permLimit of
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

{- Note [Optimizing queries using limit/offset]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Refer to the issue https://github.com/hasura/graphql-engine/issues/5745

Before this change, limit/offset/distinct_on is applied at outer selection
node along with order by clause. This greatly reduces query performance if
our base selection table contains many rows and relationships are selected
which joins remote tables. We need to optimize application of limit wrt to
order by input.

If "Order by" is not present:
  Apply limit/offset/distinct on at the base table selection
Else if "Order by" contains only columns:
  Apply limit/offset/distinct_on at the base table selection along with order by
Otherwise:
  Apply limit/offset/distinct_on at the node selection along with order by
-}

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

processOrderByItems ::
  forall pgKind m.
  ( MonadReader StringifyNumbers m,
    MonadWriter JoinTree m,
    Backend ('Postgres pgKind)
  ) =>
  Identifier ->
  FieldName ->
  SimilarArrayFields ->
  Maybe (NE.NonEmpty PGCol) ->
  Maybe (NE.NonEmpty (AnnotatedOrderByItem ('Postgres pgKind))) ->
  m
    ( [(S.Alias, S.SQLExp)], -- Order by Extractors
      SelectSorting,
      Maybe S.SQLExp -- The cursor expression
    )
processOrderByItems sourcePrefix' fieldAlias' similarArrayFields distOnCols = \case
  Nothing -> pure ([], NoSorting $ applyDistinctOnAtBase <$> distOnCols, Nothing)
  Just orderByItems -> do
    orderByItemExps <- forM orderByItems processAnnOrderByItem
    let (sorting, distinctOnExtractors) = generateSorting orderByItemExps
        orderByExtractors = concat $ toList $ map snd . toList <$> orderByItemExps
        cursor = mkCursorExp $ toList orderByItemExps
    pure (orderByExtractors <> distinctOnExtractors, sorting, Just cursor)
  where
    processAnnOrderByItem ::
      AnnotatedOrderByItem ('Postgres pgKind) ->
      m (OrderByItemG ('Postgres pgKind) (AnnotatedOrderByElement ('Postgres pgKind) (SQLExpression ('Postgres pgKind)), (S.Alias, SQLExpression ('Postgres pgKind))))
    processAnnOrderByItem orderByItem =
      forM orderByItem $ \ordByCol ->
        (ordByCol,)
          <$> processAnnotatedOrderByElement sourcePrefix' fieldAlias' ordByCol

    processAnnotatedOrderByElement ::
      Identifier -> FieldName -> AnnotatedOrderByElement ('Postgres pgKind) S.SQLExp -> m (S.Alias, (SQLExpression ('Postgres pgKind)))
    processAnnotatedOrderByElement sourcePrefix fieldAlias annObCol = do
      let ordByAlias = mkAnnOrderByAlias sourcePrefix fieldAlias similarArrayFields annObCol
      (ordByAlias,) <$> case annObCol of
        AOCColumn pgColInfo ->
          pure $
            S.mkQIdenExp (mkBaseTableAlias sourcePrefix) $ toIdentifier $ ciColumn pgColInfo
        AOCObjectRelation relInfo relFilter rest -> withWriteObjectRelation $ do
          let RelInfo relName _ colMapping relTable _ _ = relInfo
              relSourcePrefix = mkObjectRelationTableAlias sourcePrefix relName
              fieldName = mkOrderByFieldName relName
          (relOrderByAlias, relOrdByExp) <-
            processAnnotatedOrderByElement relSourcePrefix fieldName rest
          let selectSource =
                ObjectSelectSource
                  relSourcePrefix
                  (S.FISimple relTable Nothing)
                  (toSQLBoolExp (S.QualTable relTable) relFilter)
              relSource = ObjectRelationSource relName colMapping selectSource
          pure
            ( relSource,
              HM.singleton relOrderByAlias relOrdByExp,
              S.mkQIdenExp relSourcePrefix relOrderByAlias
            )
        AOCArrayAggregation relInfo relFilter aggOrderBy -> withWriteArrayRelation $ do
          let RelInfo relName _ colMapping relTable _ _ = relInfo
              fieldName = mkOrderByFieldName relName
              relSourcePrefix =
                mkArrayRelationSourcePrefix
                  sourcePrefix
                  fieldAlias
                  similarArrayFields
                  fieldName
              relAlias = mkArrayRelationAlias fieldAlias similarArrayFields fieldName
              (topExtractor, fields) = mkAggregateOrderByExtractorAndFields aggOrderBy
              selectSource =
                SelectSource
                  relSourcePrefix
                  (S.FISimple relTable Nothing)
                  (toSQLBoolExp (S.QualTable relTable) relFilter)
                  noSortingAndSlicing
              relSource = ArrayRelationSource relAlias colMapping selectSource
          pure
            ( relSource,
              topExtractor,
              HM.fromList $ aggregateFieldsToExtractorExps relSourcePrefix fields,
              S.mkQIdenExp relSourcePrefix (mkAggregateOrderByAlias aggOrderBy)
            )
        AOCComputedField ComputedFieldOrderBy {..} ->
          case _cfobOrderByElement of
            CFOBEScalar _ -> do
              let functionArgs = fromTableRowArgs sourcePrefix _cfobFunctionArgsExp
                  functionExp = S.FunctionExp _cfobFunction functionArgs Nothing
              pure $ S.SEFunction functionExp
            CFOBETableAggregation _ tableFilter aggOrderBy -> withWriteComputedFieldTableSet $ do
              let fieldName = mkOrderByFieldName _cfobName
                  computedFieldSourcePrefix = mkComputedFieldTableAlias sourcePrefix fieldName
                  (topExtractor, fields) = mkAggregateOrderByExtractorAndFields aggOrderBy
                  fromItem =
                    selectFromToFromItem sourcePrefix $
                      FromFunction _cfobFunction _cfobFunctionArgsExp Nothing
                  functionQual = S.QualifiedIdentifier (functionToIdentifier _cfobFunction) Nothing
                  selectSource =
                    SelectSource
                      computedFieldSourcePrefix
                      fromItem
                      (toSQLBoolExp functionQual tableFilter)
                      noSortingAndSlicing
                  source = ComputedFieldTableSetSource fieldName selectSource
              pure
                ( source,
                  topExtractor,
                  HM.fromList $ aggregateFieldsToExtractorExps computedFieldSourcePrefix fields,
                  S.mkQIdenExp computedFieldSourcePrefix (mkAggregateOrderByAlias aggOrderBy)
                )

    generateSorting ::
      NE.NonEmpty (OrderByItemG ('Postgres pgKind) (AnnotatedOrderByElement ('Postgres pgKind) (SQLExpression ('Postgres pgKind)), (S.Alias, SQLExpression ('Postgres pgKind)))) ->
      ( SelectSorting,
        [(S.Alias, SQLExpression ('Postgres pgKind))] -- 'distinct on' column extractors
      )
    generateSorting orderByExps@(firstOrderBy NE.:| restOrderBys) =
      case fst $ obiColumn firstOrderBy of
        AOCColumn columnInfo ->
          -- If rest order by expressions are all columns then apply order by clause at base selection.
          if all (isJust . getColumnOrderBy . obiColumn) restOrderBys
            then -- Collect column order by expressions from the rest.

              let restColumnOrderBys = mapMaybe (sequenceA . (getColumnOrderBy <$>)) restOrderBys
                  firstColumnOrderBy = firstOrderBy {obiColumn = columnInfo}
               in sortAtNodeAndBase $ firstColumnOrderBy NE.:| restColumnOrderBys
            else -- Else rest order by expressions contain atleast one non-column order by.
            -- So, apply order by clause at node selection.
              sortOnlyAtNode
        _ ->
          -- First order by expression is non-column. So, apply order by clause at node selection.
          sortOnlyAtNode
      where
        getColumnOrderBy = (^? _AOCColumn) . fst

        (nodeOrderBy, nodeDistinctOn, nodeDistinctOnExtractors) =
          let toOrderByExp orderByItemExp =
                let OrderByItemG obTyM expAlias obNullsM = fst . snd <$> orderByItemExp
                 in S.OrderByItem (S.SEIdentifier $ toIdentifier expAlias) obTyM obNullsM
              orderByExp = S.OrderByExp $ toOrderByExp <$> orderByExps
              (maybeDistOn, distOnExtrs) = NE.unzip $ applyDistinctOnAtNode sourcePrefix' <$> distOnCols
           in (orderByExp, maybeDistOn, fromMaybe [] distOnExtrs)

        sortOnlyAtNode =
          (Sorting $ ASorting (nodeOrderBy, nodeDistinctOn) Nothing, nodeDistinctOnExtractors)

        sortAtNodeAndBase baseColumnOrderBys =
          let mkBaseOrderByItem (OrderByItemG orderByType columnInfo nullsOrder) =
                S.OrderByItem
                  (S.SEIdentifier $ toIdentifier $ ciColumn columnInfo)
                  orderByType
                  nullsOrder
              baseOrderByExp = S.OrderByExp $ mkBaseOrderByItem <$> baseColumnOrderBys
              baseDistOnExp = applyDistinctOnAtBase <$> distOnCols
              sorting = Sorting $ ASorting (nodeOrderBy, nodeDistinctOn) $ Just (baseOrderByExp, baseDistOnExp)
           in (sorting, nodeDistinctOnExtractors)

    mkCursorExp ::
      [OrderByItemG ('Postgres pgKind) (AnnotatedOrderByElement ('Postgres pgKind) (SQLExpression ('Postgres pgKind)), (S.Alias, SQLExpression ('Postgres pgKind)))] ->
      S.SQLExp
    mkCursorExp orderByItemExps =
      S.applyJsonBuildObj $
        flip concatMap orderByItemExps $
          \orderByItemExp ->
            let OrderByItemG _ (annObCol, (_, valExp)) _ = orderByItemExp
             in annObColToJSONField valExp annObCol
      where
        mkAggOrderByValExp valExp = \case
          AAOCount -> [S.SELit "count", valExp]
          AAOOp opText colInfo ->
            [ S.SELit opText,
              S.applyJsonBuildObj [S.SELit $ getPGColTxt $ ciColumn colInfo, valExp]
            ]

        annObColToJSONField valExp = \case
          AOCColumn pgCol -> [S.SELit $ getPGColTxt $ ciColumn pgCol, valExp]
          AOCObjectRelation relInfo _ obCol ->
            [ S.SELit $ relNameToTxt $ riName relInfo,
              S.applyJsonBuildObj $ annObColToJSONField valExp obCol
            ]
          AOCArrayAggregation relInfo _ aggOrderBy ->
            [ S.SELit $ relNameToTxt (riName relInfo) <> "_aggregate",
              S.applyJsonBuildObj $ mkAggOrderByValExp valExp aggOrderBy
            ]
          AOCComputedField cfOrderBy ->
            let fieldNameText = computedFieldNameToText $ _cfobName cfOrderBy
             in case _cfobOrderByElement cfOrderBy of
                  CFOBEScalar _ -> [S.SELit fieldNameText, valExp]
                  CFOBETableAggregation _ _ aggOrderBy ->
                    [ S.SELit $ fieldNameText <> "_aggregate",
                      S.applyJsonBuildObj $ mkAggOrderByValExp valExp aggOrderBy
                    ]

aggregateFieldsToExtractorExps ::
  Identifier -> AggregateFields ('Postgres pgKind) -> [(S.Alias, S.SQLExp)]
aggregateFieldsToExtractorExps sourcePrefix aggregateFields =
  flip concatMap aggregateFields $ \(_, field) ->
    case field of
      AFCount cty -> case cty of
        S.CTStar -> []
        S.CTSimple cols -> colsToExps cols
        S.CTDistinct cols -> colsToExps cols
      AFOp aggOp -> aggOpToExps aggOp
      AFExp _ -> []
  where
    colsToExps = fmap mkColExp

    aggOpToExps = mapMaybe colToMaybeExp . _aoFields
    colToMaybeExp = \case
      (_, CFCol col _) -> Just $ mkColExp col
      _ -> Nothing

    mkColExp c =
      let qualCol = S.mkQIdenExp (mkBaseTableAlias sourcePrefix) (toIdentifier c)
          colAls = toIdentifier c
       in (S.Alias colAls, qualCol)

{- Note: [SQL generation for inherited roles]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When a query is executed by an inherited role, each column may contain a predicate
(AnnColumnCaseBoolExp ('Postgres pgKind) SQLExp) along with it. The predicate is then
converted to a BoolExp, which will be used to check if the said column should
be nullified. For example,

Suppose there are two roles, role1 gives access only to the `addr` column with
row filter P1 and role2 gives access to both addr and phone column with row
filter P2. The `OR`ing of the predicates will have already been done while
the schema has been generated. The SQL generated will look like this:

 select
    (case when (P1 or P2) then addr else null end) as addr,
    (case when P2 then phone else null end) as phone
 from employee
 where (P1 or P2)

-}

class PostgresAnnotatedFieldJSON (pgKind :: PostgresKind) where
  annRowToJson :: FieldName -> [(FieldName, S.SQLExp)] -> (S.Alias, S.SQLExp)

instance PostgresAnnotatedFieldJSON 'Vanilla where
  annRowToJson fieldAlias fieldExps =
    -- postgres ignores anything beyond 63 chars for an iden
    -- in this case, we'll need to use json_build_object function
    -- json_build_object is slower than row_to_json hence it is only
    -- used when needed
    if any ((> 63) . T.length . getFieldNameTxt . fst) fieldExps
      then withJsonBuildObj fieldAlias $ concatMap toJsonBuildObjectExps fieldExps
      else withRowToJSON fieldAlias $ map toRowToJsonExtr fieldExps
    where
      toJsonBuildObjectExps (fieldName, fieldExp) =
        [S.SELit $ getFieldNameTxt fieldName, fieldExp]

      toRowToJsonExtr (fieldName, fieldExp) =
        S.Extractor fieldExp $ Just $ S.toAlias fieldName

instance PostgresAnnotatedFieldJSON 'Citus where
  annRowToJson fieldAlias fieldExps =
    -- Due to the restrictions Citus imposes on joins between tables of various
    -- distribution types we cannot use row_to_json and have to only rely on
    -- json_build_object.
    withJsonBuildObj fieldAlias $ concatMap toJsonBuildObjectExps fieldExps
    where
      toJsonBuildObjectExps (fieldName, fieldExp) =
        [S.SELit $ getFieldNameTxt fieldName, fieldExp]

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

injectJoinCond ::
  -- | Join condition
  S.BoolExp ->
  -- | Where condition
  S.BoolExp ->
  -- | New where frag
  S.WhereFrag
injectJoinCond joinCond whereCond =
  S.WhereFrag $ S.simplifyBoolExp $ S.BEBin S.AndOp joinCond whereCond

mkJoinCond :: S.Alias -> HashMap PGCol PGCol -> S.BoolExp
mkJoinCond baseTablepfx colMapn =
  foldl' (S.BEBin S.AndOp) (S.BELit True) $
    flip map (HM.toList colMapn) $ \(lCol, rCol) ->
      S.BECompare S.SEQ (S.mkQIdenExp baseTablepfx lCol) (S.mkSIdenExp rCol)

generateSQLSelect ::
  -- | Pre join condition
  S.BoolExp ->
  SelectSource ->
  SelectNode ->
  S.Select
generateSQLSelect joinCondition selectSource selectNode =
  S.mkSelect
    { S.selExtr = [S.Extractor e $ Just a | (a, e) <- HM.toList extractors],
      S.selFrom = Just $ S.FromExp [joinedFrom],
      S.selOrderBy = nodeOrderBy,
      S.selLimit = S.LimitExp . S.intToSQLExp <$> _ssLimit nodeSlicing,
      S.selOffset = S.OffsetExp . S.int64ToSQLExp <$> _ssOffset nodeSlicing,
      S.selDistinct = nodeDistinctOn
    }
  where
    SelectSource sourcePrefix fromItem whereExp sortAndSlice = selectSource
    SelectNode extractors joinTree = selectNode
    JoinTree objectRelations arrayRelations arrayConnections computedFields = joinTree
    ApplySortingAndSlicing
      (baseOrderBy, baseSlicing, baseDistinctOn)
      (nodeOrderBy, nodeSlicing, nodeDistinctOn) = applySortingAndSlicing sortAndSlice

    -- this is the table which is aliased as "sourcePrefix.base"
    baseSelect =
      S.mkSelect
        { S.selExtr = [S.Extractor (S.SEStar Nothing) Nothing],
          S.selFrom = Just $ S.FromExp [fromItem],
          S.selWhere = Just $ injectJoinCond joinCondition whereExp,
          S.selOrderBy = baseOrderBy,
          S.selLimit = S.LimitExp . S.intToSQLExp <$> _ssLimit baseSlicing,
          S.selOffset = S.OffsetExp . S.int64ToSQLExp <$> _ssOffset baseSlicing,
          S.selDistinct = baseDistinctOn
        }
    baseSelectAlias = S.Alias $ mkBaseTableAlias sourcePrefix
    baseFromItem = S.mkSelFromItem baseSelect baseSelectAlias

    -- function to create a joined from item from two from items
    leftOuterJoin current new =
      S.FIJoin $
        S.JoinExpr current S.LeftOuter new $
          S.JoinOn $ S.BELit True

    -- this is the from eexp for the final select
    joinedFrom :: S.FromItem
    joinedFrom =
      foldl' leftOuterJoin baseFromItem $
        map objectRelationToFromItem (HM.toList objectRelations)
          <> map arrayRelationToFromItem (HM.toList arrayRelations)
          <> map arrayConnectionToFromItem (HM.toList arrayConnections)
          <> map computedFieldToFromItem (HM.toList computedFields)

    objectRelationToFromItem ::
      (ObjectRelationSource, SelectNode) -> S.FromItem
    objectRelationToFromItem (objectRelationSource, node) =
      let ObjectRelationSource _ colMapping objectSelectSource = objectRelationSource
          alias = S.Alias $ _ossPrefix objectSelectSource
          source = objectSelectSourceToSelectSource objectSelectSource
          select = generateSQLSelect (mkJoinCond baseSelectAlias colMapping) source node
       in S.mkLateralFromItem select alias

    arrayRelationToFromItem ::
      (ArrayRelationSource, MultiRowSelectNode) -> S.FromItem
    arrayRelationToFromItem (arrayRelationSource, arraySelectNode) =
      let ArrayRelationSource _ colMapping source = arrayRelationSource
          alias = S.Alias $ _ssPrefix source
          select =
            generateSQLSelectFromArrayNode source arraySelectNode $
              mkJoinCond baseSelectAlias colMapping
       in S.mkLateralFromItem select alias

    arrayConnectionToFromItem ::
      (ArrayConnectionSource, MultiRowSelectNode) -> S.FromItem
    arrayConnectionToFromItem (arrayConnectionSource, arraySelectNode) =
      let selectWith = connectionToSelectWith baseSelectAlias arrayConnectionSource arraySelectNode
          alias = S.Alias $ _ssPrefix $ _acsSource arrayConnectionSource
       in S.FISelectWith (S.Lateral True) selectWith alias

    computedFieldToFromItem ::
      (ComputedFieldTableSetSource, MultiRowSelectNode) -> S.FromItem
    computedFieldToFromItem (computedFieldTableSource, node) =
      let ComputedFieldTableSetSource _ source = computedFieldTableSource
          internalSelect = generateSQLSelect (S.BELit True) source $ _mrsnSelectNode node
          alias = S.Alias $ _ssPrefix source
          select =
            S.mkSelect
              { S.selExtr = _mrsnTopExtractors node,
                S.selFrom = Just $ S.FromExp [S.mkSelFromItem internalSelect alias]
              }
       in S.mkLateralFromItem select alias

generateSQLSelectFromArrayNode ::
  SelectSource ->
  MultiRowSelectNode ->
  S.BoolExp ->
  S.Select
generateSQLSelectFromArrayNode selectSource arraySelectNode joinCondition =
  S.mkSelect
    { S.selExtr = topExtractors,
      S.selFrom = Just $ S.FromExp [selectFrom]
    }
  where
    MultiRowSelectNode topExtractors selectNode = arraySelectNode
    selectFrom =
      S.mkSelFromItem
        (generateSQLSelect joinCondition selectSource selectNode)
        $ S.Alias $ _ssPrefix selectSource

mkAggregateSelect ::
  forall pgKind.
  ( Backend ('Postgres pgKind),
    PostgresAnnotatedFieldJSON pgKind
  ) =>
  AnnAggregateSelect ('Postgres pgKind) ->
  S.Select
mkAggregateSelect annAggSel =
  let ((selectSource, nodeExtractors, topExtractor), joinTree) =
        runWriter $
          flip runReaderT strfyNum $
            processAnnAggregateSelect sourcePrefixes rootFieldName annAggSel
      selectNode = SelectNode nodeExtractors joinTree
      arrayNode = MultiRowSelectNode [topExtractor] selectNode
   in prefixNumToAliases $
        generateSQLSelectFromArrayNode selectSource arrayNode $ S.BELit True
  where
    strfyNum = _asnStrfyNum annAggSel
    rootFieldName = FieldName "root"
    rootIdentifier = toIdentifier rootFieldName
    sourcePrefixes = SourcePrefixes rootIdentifier rootIdentifier

mkSQLSelect ::
  forall pgKind.
  ( Backend ('Postgres pgKind),
    PostgresAnnotatedFieldJSON pgKind
  ) =>
  JsonAggSelect ->
  AnnSimpleSelect ('Postgres pgKind) ->
  S.Select
mkSQLSelect jsonAggSelect annSel =
  let permLimitSubQuery = PLSQNotRequired
      ((selectSource, nodeExtractors), joinTree) =
        runWriter $
          flip runReaderT strfyNum $
            processAnnSimpleSelect sourcePrefixes rootFldName permLimitSubQuery annSel
      selectNode = SelectNode nodeExtractors joinTree
      topExtractor =
        asJsonAggExtr jsonAggSelect rootFldAls permLimitSubQuery $
          orderByForJsonAgg selectSource
      arrayNode = MultiRowSelectNode [topExtractor] selectNode
   in prefixNumToAliases $
        generateSQLSelectFromArrayNode selectSource arrayNode $ S.BELit True
  where
    strfyNum = _asnStrfyNum annSel
    rootFldIdentifier = toIdentifier rootFldName
    sourcePrefixes = SourcePrefixes rootFldIdentifier rootFldIdentifier
    rootFldName = FieldName "root"
    rootFldAls = S.Alias $ toIdentifier rootFldName

mkStreamSQLSelect ::
  forall pgKind.
  ( Backend ('Postgres pgKind),
    PostgresAnnotatedFieldJSON pgKind
  ) =>
  AnnSimpleStreamSelect ('Postgres pgKind) ->
  S.Select
mkStreamSQLSelect (AnnSelectStreamG () fields from perm args strfyNum) =
  let cursorArg = _ssaCursorArg args
      cursorColInfo = _sciColInfo cursorArg
      annOrderbyCol = AOCColumn cursorColInfo
      basicOrderType =
        bool S.OTAsc S.OTDesc $ _sciOrdering cursorArg == CODescending
      orderByItems =
        nonEmpty $ pure $ OrderByItemG (Just basicOrderType) annOrderbyCol Nothing
      cursorBoolExp =
        let orderByOpExp = bool ALT AGT $ basicOrderType == S.OTAsc
            sqlExp =
              fromResVars
                (CollectableTypeScalar $ unsafePGColumnToBackend $ cvType (_sciInitialValue cursorArg))
                ["cursor", getPGColTxt $ ciColumn cursorColInfo]
         in BoolFld $ AVColumn cursorColInfo [(orderByOpExp sqlExp)]

      selectArgs =
        noSelectArgs
          { _saWhere =
              Just $ maybe cursorBoolExp (andAnnBoolExps cursorBoolExp) $ _ssaWhere args,
            _saOrderBy = orderByItems,
            _saLimit = Just $ _ssaBatchSize args
          }
      sqlSelect = AnnSelectG fields from perm selectArgs strfyNum
      permLimitSubQuery = PLSQNotRequired
      ((selectSource, nodeExtractors), joinTree) =
        runWriter $
          flip runReaderT strfyNum $
            processAnnSimpleSelect sourcePrefixes rootFldName permLimitSubQuery sqlSelect
      selectNode = SelectNode nodeExtractors joinTree
      topExtractor =
        asJsonAggExtr JASMultipleRows rootFldAls permLimitSubQuery $
          orderByForJsonAgg selectSource
      cursorLatestValueExp :: S.SQLExp =
        let pgColumn = ciColumn cursorColInfo
            mkMaxOrMinSQLExp maxOrMin col =
              S.SEFnApp maxOrMin [S.SEIdentifier col] Nothing
            maxOrMinTxt = bool "MIN" "MAX" $ basicOrderType == S.OTAsc
            -- text encoding the cursor value while it's fetched from the DB, because
            -- we can then directly reuse this value, otherwise if this were json encoded
            -- then we'd have to parse the value and then convert it into a text encoded value
            colExp =
              [ S.SELit (getPGColTxt pgColumn),
                S.SETyAnn
                  (mkMaxOrMinSQLExp maxOrMinTxt $ mkBaseTableColumnAlias rootFldIdentifier pgColumn)
                  S.textTypeAnn
              ]
         in -- SELECT json_build_object ('col1', MAX(col1) :: text)

            S.SEFnApp "json_build_object" colExp Nothing
      cursorLatestValueExtractor = S.Extractor cursorLatestValueExp (Just $ S.Alias $ Identifier "cursor")
      arrayNode = MultiRowSelectNode [topExtractor, cursorLatestValueExtractor] selectNode
   in prefixNumToAliases $
        generateSQLSelectFromArrayNode selectSource arrayNode $ S.BELit True
  where
    rootFldIdentifier = toIdentifier rootFldName
    sourcePrefixes = SourcePrefixes rootFldIdentifier rootFldIdentifier
    rootFldName = FieldName "root"
    rootFldAls = S.Alias $ toIdentifier rootFldName

    -- TODO: these functions also exist in `resolveMultiplexedValue`, de-duplicate these!
    fromResVars pgType jPath =
      addTypeAnnotation pgType $
        S.SEOpApp
          (S.SQLOp "#>>")
          [ S.SEQIdentifier $ S.QIdentifier (S.QualifiedIdentifier (Identifier "_subs") Nothing) (Identifier "result_vars"),
            S.SEArray $ map S.SELit jPath
          ]
    addTypeAnnotation pgType =
      flip S.SETyAnn (S.mkTypeAnn pgType) . case pgType of
        CollectableTypeScalar scalarType -> withConstructorFn scalarType
        CollectableTypeArray _ -> id

mkConnectionSelect ::
  forall pgKind.
  ( Backend ('Postgres pgKind),
    PostgresAnnotatedFieldJSON pgKind
  ) =>
  ConnectionSelect ('Postgres pgKind) Void S.SQLExp ->
  S.SelectWithG S.Select
mkConnectionSelect connectionSelect =
  let ((connectionSource, topExtractor, nodeExtractors), joinTree) =
        runWriter $
          flip runReaderT strfyNum $
            processConnectionSelect
              sourcePrefixes
              rootFieldName
              (S.Alias rootIdentifier)
              mempty
              connectionSelect
      selectNode =
        MultiRowSelectNode [topExtractor] $
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

connectionToSelectWith ::
  S.Alias ->
  ArrayConnectionSource ->
  MultiRowSelectNode ->
  S.SelectWithG S.Select
connectionToSelectWith baseSelectAlias arrayConnectionSource arraySelectNode =
  let extractionSelect =
        S.mkSelect
          { S.selExtr = topExtractors,
            S.selFrom = Just $ S.FromExp [S.FIIdentifier finalSelectIdentifier]
          }
   in S.SelectWith fromBaseSelections extractionSelect
  where
    ArrayConnectionSource _ columnMapping maybeSplit maybeSlice selectSource =
      arrayConnectionSource
    MultiRowSelectNode topExtractors selectNode = arraySelectNode
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
          baseSelectFrom =
            S.mkSelFromItem
              (generateSQLSelect joinCond selectSource selectNode)
              $ S.Alias $ _ssPrefix selectSource
          select =
            S.mkSelect
              { S.selExtr =
                  [ S.selectStar,
                    S.Extractor rowNumberExp $ Just $ S.Alias rowNumberIdentifier
                  ],
                S.selFrom = Just $ S.FromExp [baseSelectFrom]
              }
       in (S.Alias baseSelectIdentifier, select) : fromSplitSelection

    mkStarSelect fromIdentifier =
      S.mkSelect
        { S.selExtr = [S.selectStar],
          S.selFrom = Just $ S.FromExp [S.FIIdentifier fromIdentifier]
        }

    fromSplitSelection = case maybeSplit of
      Nothing -> fromSliceSelection baseSelectIdentifier
      Just splitBool ->
        let select =
              (mkStarSelect baseSelectIdentifier) {S.selWhere = Just $ S.WhereFrag splitBool}
         in (S.Alias splitSelectIdentifier, select) : fromSliceSelection splitSelectIdentifier

    fromSliceSelection prevSelect = case maybeSlice of
      Nothing -> fromFinalSelect prevSelect
      Just slice ->
        let select = case slice of
              SliceFirst limit ->
                (mkStarSelect prevSelect)
                  { S.selLimit = (Just . S.LimitExp . S.intToSQLExp) limit
                  }
              SliceLast limit ->
                let mkRowNumberOrderBy obType =
                      let orderByItem =
                            S.OrderByItem (S.SEIdentifier rowNumberIdentifier) (Just obType) Nothing
                       in S.OrderByExp $ orderByItem NE.:| []

                    sliceLastSelect =
                      (mkStarSelect prevSelect)
                        { S.selLimit = (Just . S.LimitExp . S.intToSQLExp) limit,
                          S.selOrderBy = Just $ mkRowNumberOrderBy S.OTDesc
                        }
                    sliceLastSelectFrom =
                      S.mkSelFromItem sliceLastSelect $ S.Alias sliceSelectIdentifier
                 in S.mkSelect
                      { S.selExtr = [S.selectStar],
                        S.selFrom = Just $ S.FromExp [sliceLastSelectFrom],
                        S.selOrderBy = Just $ mkRowNumberOrderBy S.OTAsc
                      }
         in (S.Alias sliceSelectIdentifier, select) : fromFinalSelect sliceSelectIdentifier

    fromFinalSelect prevSelect =
      let select = mkStarSelect prevSelect
       in (S.Alias finalSelectIdentifier, select) : fromCursorSelection

    fromCursorSelection =
      let extrs =
            [ S.Extractor startCursorExp $ Just $ S.Alias startCursorIdentifier,
              S.Extractor endCursorExp $ Just $ S.Alias endCursorIdentifier,
              S.Extractor startRowNumberExp $ Just $ S.Alias startRowNumberIdentifier,
              S.Extractor endRowNumberExp $ Just $ S.Alias endRowNumberIdentifier
            ]
          select =
            S.mkSelect
              { S.selExtr = extrs,
                S.selFrom = Just $ S.FromExp [S.FIIdentifier finalSelectIdentifier]
              }
       in (S.Alias cursorsSelectAliasIdentifier, select) : fromPageInfoSelection

    fromPageInfoSelection =
      let hasPrevPage =
            S.SEBool $
              S.mkExists (S.FIIdentifier baseSelectIdentifier) $
                S.BECompare S.SLT (S.SEIdentifier rowNumberIdentifier) $
                  S.SESelect $
                    S.mkSelect
                      { S.selFrom = Just $ S.FromExp [S.FIIdentifier cursorsSelectAliasIdentifier],
                        S.selExtr = [S.Extractor (S.SEIdentifier startRowNumberIdentifier) Nothing]
                      }
          hasNextPage =
            S.SEBool $
              S.mkExists (S.FIIdentifier baseSelectIdentifier) $
                S.BECompare S.SGT (S.SEIdentifier rowNumberIdentifier) $
                  S.SESelect $
                    S.mkSelect
                      { S.selFrom = Just $ S.FromExp [S.FIIdentifier cursorsSelectAliasIdentifier],
                        S.selExtr = [S.Extractor (S.SEIdentifier endRowNumberIdentifier) Nothing]
                      }

          select =
            S.mkSelect
              { S.selExtr =
                  [ S.Extractor hasPrevPage $ Just $ S.Alias hasPreviousPageIdentifier,
                    S.Extractor hasNextPage $ Just $ S.Alias hasNextPageIdentifier
                  ]
              }
       in pure (S.Alias pageInfoSelectAliasIdentifier, select)
