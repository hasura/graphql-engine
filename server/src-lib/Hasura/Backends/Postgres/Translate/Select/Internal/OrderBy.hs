-- | This module relates to the processing of Order-By clauses.
module Hasura.Backends.Postgres.Translate.Select.Internal.OrderBy
  ( processOrderByItems,
  )
where

import Control.Lens ((^?))
import Data.HashMap.Strict qualified as HM
import Data.List.NonEmpty qualified as NE
import Hasura.Backends.Postgres.SQL.DML qualified as S
import Hasura.Backends.Postgres.SQL.Types
  ( Identifier,
    IsIdentifier (toIdentifier),
    PGCol (..),
  )
import Hasura.Backends.Postgres.Translate.BoolExp (toSQLBoolExp)
import Hasura.Backends.Postgres.Translate.Select.Internal.Aliases
  ( mkAggregateOrderByAlias,
    mkAnnOrderByAlias,
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
    mkAggregateOrderByExtractorAndFields,
  )
import Hasura.Backends.Postgres.Translate.Select.Internal.Helpers
  ( fromTableRowArgs,
    functionToIdentifier,
    selectFromToFromItem,
  )
import Hasura.Backends.Postgres.Translate.Select.Internal.JoinTree
  ( withWriteArrayRelation,
    withWriteComputedFieldTableSet,
    withWriteObjectRelation,
  )
import Hasura.Backends.Postgres.Translate.Types
import Hasura.GraphQL.Schema.Options qualified as Options
import Hasura.Prelude
import Hasura.RQL.IR.OrderBy
  ( OrderByItemG (OrderByItemG, obiColumn),
  )
import Hasura.RQL.IR.Select
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.ComputedField
import Hasura.RQL.Types.Relationships.Local
import Hasura.SQL.Backend

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
    MonadWriter JoinTree m,
    Backend ('Postgres pgKind)
  ) =>
  Identifier ->
  FieldName ->
  SimilarArrayFields ->
  Maybe (NE.NonEmpty PGCol) ->
  Maybe (NE.NonEmpty (AnnotatedOrderByItem ('Postgres pgKind))) ->
  m
    ( [(S.ColumnAlias, S.SQLExp)], -- Order by Extractors
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
      m (OrderByItemG ('Postgres pgKind) (AnnotatedOrderByElement ('Postgres pgKind) (SQLExpression ('Postgres pgKind)), (S.ColumnAlias, SQLExpression ('Postgres pgKind))))
    processAnnOrderByItem orderByItem =
      forM orderByItem $ \ordByCol ->
        (ordByCol,)
          <$> processAnnotatedOrderByElement sourcePrefix' fieldAlias' ordByCol

    processAnnotatedOrderByElement ::
      Identifier -> FieldName -> AnnotatedOrderByElement ('Postgres pgKind) S.SQLExp -> m (S.ColumnAlias, (SQLExpression ('Postgres pgKind)))
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
      NE.NonEmpty (OrderByItemG ('Postgres pgKind) (AnnotatedOrderByElement ('Postgres pgKind) (SQLExpression ('Postgres pgKind)), (S.ColumnAlias, SQLExpression ('Postgres pgKind)))) ->
      ( SelectSorting,
        [(S.ColumnAlias, SQLExpression ('Postgres pgKind))] -- 'distinct on' column extractors
      )
    generateSorting orderByExps@(firstOrderBy NE.:| restOrderBys) =
      case fst $ obiColumn firstOrderBy of
        AOCColumn columnInfo ->
          -- If rest order by expressions are all columns then apply order by clause at base selection.
          if all (isJust . getColumnOrderBy . obiColumn) restOrderBys
            then -- Collect column order by expressions from the rest.

              let restColumnOrderBys = mapMaybe (traverse getColumnOrderBy) restOrderBys
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
      [OrderByItemG ('Postgres pgKind) (AnnotatedOrderByElement ('Postgres pgKind) (SQLExpression ('Postgres pgKind)), (S.ColumnAlias, SQLExpression ('Postgres pgKind)))] ->
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

applyDistinctOnAtBase ::
  NE.NonEmpty PGCol -> S.DistinctExpr
applyDistinctOnAtBase =
  S.DistinctOn . map (S.SEIdentifier . toIdentifier) . toList

applyDistinctOnAtNode ::
  Identifier ->
  NE.NonEmpty PGCol ->
  ( S.DistinctExpr,
    [(S.ColumnAlias, S.SQLExp)] -- additional column extractors
  )
applyDistinctOnAtNode pfx neCols = (distOnExp, colExtrs)
  where
    cols = toList neCols
    distOnExp = S.DistinctOn $ map (S.SEIdentifier . toIdentifier . mkQColAls) cols
    mkQCol c = S.mkQIdenExp (mkBaseTableAlias pfx) $ toIdentifier c
    mkQColAls = mkBaseTableColumnAlias pfx
    colExtrs = flip map cols $ mkQColAls &&& mkQCol
