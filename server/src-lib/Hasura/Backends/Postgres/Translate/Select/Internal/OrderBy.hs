-- | This module relates to the processing of Order-By clauses.
module Hasura.Backends.Postgres.Translate.Select.Internal.OrderBy
  ( processOrderByItems,
  )
where

import Control.Lens ((^?))
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.List.NonEmpty qualified as NE
import Hasura.Backends.Postgres.SQL.DML qualified as S
import Hasura.Backends.Postgres.SQL.Types
  ( IsIdentifier (toIdentifier),
    PGCol (..),
    QualifiedFunction,
    TableIdentifier (..),
    qualifiedObjectToText,
    tableIdentifierToIdentifier,
  )
import Hasura.Backends.Postgres.Translate.BoolExp (toSQLBoolExp, withRedactionExp)
import Hasura.Backends.Postgres.Translate.Select.Internal.Aliases
  ( contextualizeBaseTableColumn,
    mkAggregateOrderByAlias,
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
    mkAggregateOrderByExtractorAndFields,
  )
import Hasura.Backends.Postgres.Translate.Select.Internal.Helpers (fromTableRowArgs)
import Hasura.Backends.Postgres.Translate.Select.Internal.JoinTree
  ( withWriteArrayRelation,
    withWriteComputedFieldTableSet,
    withWriteObjectRelation,
  )
import Hasura.Backends.Postgres.Translate.Types
import Hasura.Backends.Postgres.Types.Function (ArgumentExp)
import Hasura.Function.Cache (FunctionArgsExpG (..))
import Hasura.Prelude
import Hasura.RQL.IR.OrderBy
  ( OrderByItemG (OrderByItemG, obiColumn),
  )
import Hasura.RQL.IR.Select
import Hasura.RQL.IR.Select.Lenses
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.ComputedField
import Hasura.RQL.Types.Relationships.Local
import Hasura.RQL.Types.Schema.Options qualified as Options

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

processOrderByItems ::
  forall pgKind m.
  ( MonadReader Options.StringifyNumbers m,
    MonadWriter SelectWriter m,
    Backend ('Postgres pgKind)
  ) =>
  TableIdentifier ->
  S.Qual ->
  FieldName ->
  SimilarArrayFields ->
  Maybe (NE.NonEmpty (AnnDistinctColumn ('Postgres pgKind) S.SQLExp)) ->
  Maybe (NE.NonEmpty (AnnotatedOrderByItem ('Postgres pgKind))) ->
  m
    ( [(S.ColumnAlias, S.SQLExp)], -- Order by Extractors
      SelectSorting,
      Maybe S.SQLExp -- The cursor expression
    )
processOrderByItems sourcePrefix' selectSourceQual fieldAlias' similarArrayFields distOnCols = \case
  Nothing -> pure ([], NoSorting $ applyDistinctOnAtBase selectSourceQual <$> distOnCols, Nothing)
  Just orderByItems -> do
    orderByItemExps <- forM orderByItems processAnnOrderByItem
    let (sorting, distinctOnExtractors) = generateSorting orderByItemExps
        orderByExtractors = concat $ toList $ map snd . toList <$> orderByItemExps
        cursor = mkCursorExp $ toList orderByItemExps
    pure (orderByExtractors <> distinctOnExtractors, sorting, Just cursor)
  where
    processAnnOrderByItem ::
      AnnotatedOrderByItem ('Postgres pgKind) ->
      m
        ( OrderByItemG
            ('Postgres pgKind)
            ( AnnotatedOrderByElement ('Postgres pgKind) S.SQLExp,
              (S.ColumnAlias, S.SQLExp)
            )
        )
    processAnnOrderByItem orderByItem =
      forM orderByItem $ \ordByCol ->
        (ordByCol,)
          <$> processAnnotatedOrderByElement sourcePrefix' fieldAlias' ordByCol

    processAnnotatedOrderByElement ::
      TableIdentifier -> FieldName -> AnnotatedOrderByElement ('Postgres pgKind) S.SQLExp -> m (S.ColumnAlias, S.SQLExp)
    processAnnotatedOrderByElement sourcePrefix fieldAlias annObCol = do
      let ordByAlias = mkAnnOrderByAlias sourcePrefix fieldAlias similarArrayFields annObCol
      let baseTableIdentifier = mkBaseTableIdentifier sourcePrefix
      (ordByAlias,) <$> case annObCol of
        AOCColumn pgColInfo redactionExp ->
          pure
            $ withRedactionExp (S.QualifiedIdentifier baseTableIdentifier Nothing) redactionExp
            $ S.mkQIdenExp baseTableIdentifier
            $ ciColumn pgColInfo
        AOCObjectRelation relInfo relFilter rest -> withWriteObjectRelation $ do
          let RelInfo {riName = relName, riMapping = colMapping, riTarget = relTarget} = relInfo
              relSourcePrefix = mkObjectRelationTableAlias sourcePrefix relName
              fieldName = mkOrderByFieldName relName
          case relTarget of
            RelTargetNativeQuery _ -> error "processAnnotatedOrderByElement RelTargetNativeQuery (AOCObjectRelation)"
            RelTargetTable relTable -> do
              (relOrderByAlias, relOrdByExp) <-
                processAnnotatedOrderByElement relSourcePrefix fieldName rest
              let selectSource =
                    ObjectSelectSource
                      (tableIdentifierToIdentifier relSourcePrefix)
                      (S.FISimple relTable Nothing)
                      (toSQLBoolExp (S.QualTable relTable) relFilter)
                  relSource = ObjectRelationSource relName colMapping selectSource Nullable
              pure
                ( relSource,
                  InsOrdHashMap.singleton relOrderByAlias relOrdByExp,
                  S.mkQIdenExp relSourcePrefix relOrderByAlias
                )
        AOCArrayAggregation relInfo relFilter aggOrderBy -> withWriteArrayRelation $ do
          let RelInfo {riName = relName, riMapping = colMapping, riTarget = relTarget} = relInfo
          case relTarget of
            RelTargetNativeQuery _ -> error "processAnnotatedOrderByElement RelTargetNativeQuery (AOCArrayAggregation)"
            RelTargetTable relTable -> do
              let fieldName = mkOrderByFieldName relName
                  relSourcePrefix =
                    mkArrayRelationSourcePrefix
                      sourcePrefix
                      fieldAlias
                      similarArrayFields
                      fieldName
                  relAlias = mkArrayRelationAlias fieldAlias similarArrayFields fieldName
                  (topExtractor, fields) = mkAggregateOrderByExtractorAndFields relSourcePrefix aggOrderBy
                  selectSource =
                    SelectSource
                      (tableIdentifierToIdentifier relSourcePrefix)
                      (S.FISimple relTable Nothing)
                      (toSQLBoolExp (S.QualTable relTable) relFilter)
                      noSortingAndSlicing
                  relSource = ArrayRelationSource relAlias colMapping selectSource
              pure
                ( relSource,
                  topExtractor,
                  InsOrdHashMap.fromList $ aggregateFieldsToExtractorExps relSourcePrefix fields,
                  S.mkQIdenExp relSourcePrefix (mkAggregateOrderByAlias aggOrderBy)
                )
        AOCComputedField ComputedFieldOrderBy {..} ->
          case _cfobOrderByElement of
            CFOBEScalar _ redactionExp -> do
              let functionArgs = fromTableRowArgs sourcePrefix _cfobFunctionArgsExp
                  functionExp = S.FunctionExp _cfobFunction functionArgs Nothing
              pure
                $ withRedactionExp (S.QualifiedIdentifier baseTableIdentifier Nothing) redactionExp
                $ S.SEFunction functionExp
            CFOBETableAggregation _ tableFilter aggOrderBy -> withWriteComputedFieldTableSet $ do
              let fieldName = mkOrderByFieldName _cfobName
                  computedFieldSourcePrefix = mkComputedFieldTableIdentifier sourcePrefix fieldName
                  (topExtractor, fields) = mkAggregateOrderByExtractorAndFields computedFieldSourcePrefix aggOrderBy
                  fromItem =
                    functionToFromItem sourcePrefix _cfobFunction _cfobFunctionArgsExp
                  functionQual = S.QualifiedIdentifier (TableIdentifier $ qualifiedObjectToText _cfobFunction) Nothing
                  selectSource =
                    SelectSource
                      (tableIdentifierToIdentifier computedFieldSourcePrefix)
                      fromItem
                      (toSQLBoolExp functionQual tableFilter)
                      noSortingAndSlicing
                  source = ComputedFieldTableSetSource fieldName selectSource
              pure
                ( source,
                  topExtractor,
                  InsOrdHashMap.fromList $ aggregateFieldsToExtractorExps computedFieldSourcePrefix fields,
                  S.mkQIdenExp computedFieldSourcePrefix (mkAggregateOrderByAlias aggOrderBy)
                )

    functionToFromItem :: TableIdentifier -> QualifiedFunction -> FunctionArgsExpG (ArgumentExp S.SQLExp) -> S.FromItem
    functionToFromItem prefix qf args =
      S.FIFunc
        $ S.FunctionExp qf (fromTableRowArgs prefix args)
        $ Just
        $ S.mkFunctionAlias qf Nothing

    generateSorting ::
      NE.NonEmpty (OrderByItemG ('Postgres pgKind) (AnnotatedOrderByElement ('Postgres pgKind) S.SQLExp, (S.ColumnAlias, S.SQLExp))) ->
      ( SelectSorting,
        [(S.ColumnAlias, S.SQLExp)] -- 'distinct on' column extractors
      )
    generateSorting orderByExps@(firstOrderBy NE.:| restOrderBys) =
      case fst $ obiColumn firstOrderBy of
        AOCColumn columnInfo redactionExp ->
          -- If rest order by expressions are all columns then apply order by clause at base selection.
          if all (isJust . getColumnOrderBy . obiColumn) restOrderBys
            then -- Collect column order by expressions from the rest.

              let restColumnOrderBys = mapMaybe (traverse getColumnOrderBy) restOrderBys
                  firstColumnOrderBy = firstOrderBy {obiColumn = (columnInfo, redactionExp)}
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
          let mkBaseOrderByItem (OrderByItemG orderByType (columnInfo, redactionExp) nullsOrder) =
                let columnExp =
                      withRedactionExp selectSourceQual redactionExp
                        $ S.mkSIdenExp (ciColumn columnInfo)
                 in S.OrderByItem columnExp orderByType nullsOrder
              baseOrderByExp = S.OrderByExp $ mkBaseOrderByItem <$> baseColumnOrderBys
              baseDistOnExp = applyDistinctOnAtBase selectSourceQual <$> distOnCols
              sorting = Sorting $ ASorting (nodeOrderBy, nodeDistinctOn) $ Just (baseOrderByExp, baseDistOnExp)
           in (sorting, nodeDistinctOnExtractors)

    mkCursorExp ::
      [OrderByItemG ('Postgres pgKind) (AnnotatedOrderByElement ('Postgres pgKind) (SQLExpression ('Postgres pgKind)), (S.ColumnAlias, SQLExpression ('Postgres pgKind)))] ->
      S.SQLExp
    mkCursorExp orderByItemExps =
      S.applyJsonBuildObj
        $ flip concatMap orderByItemExps
        $ \orderByItemExp ->
          let OrderByItemG _ (annObCol, (_, valExp)) _ = orderByItemExp
           in annObColToJSONField valExp annObCol
      where
        mkAggOrderByValExp valExp = \case
          AAOCount -> [S.SELit "count", valExp]
          AAOOp (AggregateOrderByColumn opText _resultType colInfo _redactionExp) ->
            [ S.SELit opText,
              S.applyJsonBuildObj [S.SELit $ getPGColTxt $ ciColumn colInfo, valExp]
            ]

        annObColToJSONField valExp = \case
          AOCColumn pgCol _redactionExp -> [S.SELit $ getPGColTxt $ ciColumn pgCol, valExp]
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
                  CFOBEScalar _ _redactionExp -> [S.SELit fieldNameText, valExp]
                  CFOBETableAggregation _ _ aggOrderBy ->
                    [ S.SELit $ fieldNameText <> "_aggregate",
                      S.applyJsonBuildObj $ mkAggOrderByValExp valExp aggOrderBy
                    ]

applyDistinctOnAtBase ::
  forall pgKind.
  (Backend ('Postgres pgKind)) =>
  S.Qual ->
  NE.NonEmpty (AnnDistinctColumn ('Postgres pgKind) S.SQLExp) ->
  S.DistinctExpr
applyDistinctOnAtBase selectSourceQual distinctColumns =
  let distinctExps =
        distinctColumns
          & toList
          <&> (\AnnDistinctColumn {..} -> withRedactionExp selectSourceQual _adcRedactionExpression $ S.mkSIdenExp _adcColumn)
   in S.DistinctOn distinctExps

applyDistinctOnAtNode ::
  forall pgKind.
  (Backend ('Postgres pgKind)) =>
  TableIdentifier ->
  NE.NonEmpty (AnnDistinctColumn ('Postgres pgKind) S.SQLExp) ->
  ( S.DistinctExpr,
    [(S.ColumnAlias, S.SQLExp)] -- additional column extractors
  )
applyDistinctOnAtNode pfx distinctColumns = (distinctOnExp, extractors)
  where
    columns = _adcColumn <$> toList distinctColumns
    distinctOnExp = S.DistinctOn $ map (S.SEIdentifier . toIdentifier . mkQColAlias) columns
    baseTableIdentifier = mkBaseTableIdentifier pfx
    mkExtractor AnnDistinctColumn {..} =
      let extractorExp =
            withRedactionExp (S.QualifiedIdentifier baseTableIdentifier Nothing) _adcRedactionExpression
              $ S.mkQIdenExp baseTableIdentifier _adcColumn
       in (mkQColAlias _adcColumn, extractorExp)
    mkQColAlias = contextualizeBaseTableColumn pfx
    extractors = mkExtractor <$> toList distinctColumns
