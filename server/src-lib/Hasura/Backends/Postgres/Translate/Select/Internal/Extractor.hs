-- | Extractors are a pair of an SQL expression and an alias; they get
-- translated like "[SELECT ...] <expr> as <alias>"
module Hasura.Backends.Postgres.Translate.Select.Internal.Extractor
  ( aggregateFieldsToExtractorExps,
    mkAggregateOrderByExtractorAndFields,
    mkRawComputedFieldExpression,
    withJsonAggExtr,
    asSingleRowExtr,
    asJsonAggExtr,
    withColumnOp,
  )
where

import Control.Monad.Extra (concatMapM)
import Control.Monad.Writer.Strict
import Data.List.NonEmpty qualified as NE
import Hasura.Backends.Postgres.SQL.DML qualified as S
import Hasura.Backends.Postgres.SQL.Types
import Hasura.Backends.Postgres.Translate.BoolExp (withRedactionExp)
import Hasura.Backends.Postgres.Translate.Select.Internal.Aliases
import Hasura.Backends.Postgres.Translate.Select.Internal.Helpers (fromTableRowArgs)
import Hasura.Backends.Postgres.Translate.Types (PermissionLimitSubQuery (..))
import Hasura.Backends.Postgres.Types.Aggregates
import Hasura.Base.Error (QErr)
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.IR.Select
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Session (UserInfo)

-- | Creates node extractors for all of the columns and computed fields used in aggregated fields.
-- The ColumnAliases for all the extractors are namespaced aliases using the 'contextualize*` functions
-- so that none of the extractors names will conflict with one another (for example, if a column name
-- is the same as a field name (eg 'nodes'))
aggregateFieldsToExtractorExps ::
  forall pgKind m.
  (Backend ('Postgres pgKind), MonadIO m, MonadError QErr m) =>
  TableIdentifier ->
  UserInfo ->
  AggregateFields ('Postgres pgKind) S.SQLExp ->
  m [(S.ColumnAlias, S.SQLExp)]
aggregateFieldsToExtractorExps sourcePrefix userInfo aggregateFields =
  flip concatMapM aggregateFields $ \(aggregateFieldName, field) ->
    case field of
      AFCount cty -> case getCountType cty of
        S.CTStar -> pure []
        S.CTSimple cols -> colsToExps cols
        S.CTDistinct cols -> colsToExps cols
      AFOp aggOp -> catMaybes <$> traverse (colToMaybeExp aggregateFieldName) (_aoFields aggOp)
      AFExp _ -> pure []
  where
    colsToExps :: [(PGCol, AnnRedactionExp ('Postgres pgKind) S.SQLExp)] -> m [(S.ColumnAlias, S.SQLExp)]
    colsToExps = traverse (\(col, redactionExp) -> mkColumnExp redactionExp col)

    -- Extract the columns and computed fields we need
    colToMaybeExp ::
      FieldName ->
      (FieldName, SelectionField ('Postgres pgKind) S.SQLExp) ->
      m (Maybe (S.ColumnAlias, S.SQLExp))
    colToMaybeExp aggregateFieldName = \case
      (_fieldName, SFCol col _ redactionExp) -> Just <$> mkColumnExp redactionExp col
      (fieldName, SFComputedField _name cfss) -> Just <$> mkComputedFieldExp aggregateFieldName fieldName cfss
      (_fieldName, SFExp _text) -> pure $ Nothing

    -- Generate an alias for each column we extract.
    mkColumnExp ::
      AnnRedactionExp ('Postgres pgKind) S.SQLExp ->
      PGCol ->
      m (S.ColumnAlias, S.SQLExp)
    mkColumnExp redactionExp column = do
      let baseTableIdentifier = mkBaseTableIdentifier sourcePrefix
          baseTableQual = S.QualifiedIdentifier baseTableIdentifier Nothing
      qualifiedColumn <- withRedactionExp baseTableQual redactionExp userInfo $ S.mkQIdenExp baseTableIdentifier (toIdentifier column)
      let columnAlias = contextualizeBaseTableColumn sourcePrefix column
      pure (S.toColumnAlias columnAlias, qualifiedColumn)

    mkComputedFieldExp :: FieldName -> FieldName -> ComputedFieldScalarSelect ('Postgres pgKind) S.SQLExp -> m (S.ColumnAlias, S.SQLExp)
    mkComputedFieldExp aggregateFieldName computedFieldFieldName computedFieldScalarSelect =
      (contextualizeAggregateInput sourcePrefix aggregateFieldName computedFieldFieldName,) <$> mkRawComputedFieldExpression sourcePrefix userInfo computedFieldScalarSelect

mkAggregateOrderByExtractorAndFields ::
  forall pgKind.
  (Backend ('Postgres pgKind)) =>
  TableIdentifier ->
  AnnotatedAggregateOrderBy ('Postgres pgKind) S.SQLExp ->
  (S.Extractor, AggregateFields ('Postgres pgKind) S.SQLExp)
mkAggregateOrderByExtractorAndFields sourcePrefix annAggOrderBy =
  case annAggOrderBy of
    AAOCount ->
      ( S.Extractor S.countStar alias,
        [(FieldName "count", AFCount $ CountAggregate S.CTStar)]
      )
    AAOOp (AggregateOrderByColumn opText _resultType pgColumnInfo redactionExp) ->
      let pgColumn = ciColumn pgColumnInfo
          pgType = ciType pgColumnInfo
       in ( S.Extractor (S.SEFnApp opText [S.SEQIdentifier $ columnToQIdentifier pgColumn] Nothing) alias,
            [ ( FieldName opText,
                AFOp
                  $ AggregateOp
                    opText
                    [ ( fromCol @('Postgres pgKind) pgColumn,
                        SFCol pgColumn pgType redactionExp
                      )
                    ]
              )
            ]
          )
  where
    alias = Just $ mkAggregateOrderByAlias annAggOrderBy

    columnToQIdentifier :: PGCol -> S.QIdentifier
    columnToQIdentifier = S.mkQIdentifier sourcePrefix . contextualizeBaseTableColumn sourcePrefix

-- This invokes the computed field function, but does not apply the necessary
-- 'toJSONableExp' over the top. This allows the output of this expression to be consumed by
-- other processing functions such as aggregates
mkRawComputedFieldExpression ::
  forall pgKind m.
  (Backend ('Postgres pgKind), MonadIO m, MonadError QErr m) =>
  TableIdentifier ->
  UserInfo ->
  ComputedFieldScalarSelect ('Postgres pgKind) S.SQLExp ->
  m S.SQLExp
mkRawComputedFieldExpression sourcePrefix userInfo (ComputedFieldScalarSelect fn args _ colOpM redactionExp) =
  -- The computed field is conditionally outputted depending
  -- on the value of `redactionExp`. `redactionExp` will only specify
  -- redaction in the case of an inherited role.
  -- See [SQL generation for inherited role]
  withRedactionExp (S.QualifiedIdentifier (mkBaseTableIdentifier sourcePrefix) Nothing) redactionExp userInfo
    $ withColumnOp colOpM
    $ S.SEFunction
    $ S.FunctionExp fn (fromTableRowArgs sourcePrefix args) Nothing

withJsonAggExtr ::
  PermissionLimitSubQuery -> Maybe S.OrderByExp -> S.ColumnAlias -> S.SQLExp
withJsonAggExtr permLimitSubQuery ordBy alias =
  -- if select has aggregations then use subquery to apply permission limit
  case permLimitSubQuery of
    PLSQRequired permLimit -> withPermLimit permLimit
    PLSQNotRequired -> simpleJsonAgg
  where
    simpleJsonAgg = mkSimpleJsonAgg rowIdenExp ordBy
    rowIdenExp = S.SEIdentifier $ toIdentifier alias
    subSelAls = S.mkTableAlias "sub_query"
    subSelIdentifier = S.tableAliasToIdentifier subSelAls
    unnestTable = S.mkTableAlias "unnest_table"
    unnestTableIdentifier = S.tableAliasToIdentifier unnestTable

    mkSimpleJsonAgg rowExp ob =
      let jsonAggExp = S.SEFnApp "json_agg" [rowExp] ob
       in S.SEFnApp "coalesce" [jsonAggExp, S.SELit "[]"] Nothing

    withPermLimit limit =
      let subSelect = mkSubSelect limit
          rowIdentifier = S.mkQIdenExp subSelIdentifier alias
          extr = S.Extractor (mkSimpleJsonAgg rowIdentifier newOrderBy) Nothing
          fromExp =
            S.FromExp
              $ pure
              $ S.mkSelFromItem subSelect
              $ S.toTableAlias subSelAls
       in S.SESelect
            $ S.mkSelect
              { S.selExtr = pure extr,
                S.selFrom = Just fromExp
              }

    mkSubSelect limit =
      let jsonRowExtr =
            flip S.Extractor (Just alias)
              $ S.mkQIdenExp unnestTableIdentifier alias
          obExtrs = flip map newOBAliases $ \a ->
            S.Extractor (S.mkQIdenExp unnestTableIdentifier a) $ Just $ S.toColumnAlias a
       in S.mkSelect
            { S.selExtr = jsonRowExtr : obExtrs,
              S.selFrom = Just $ S.FromExp $ pure unnestFromItem,
              S.selLimit = Just $ S.LimitExp $ S.intToSQLExp limit,
              S.selOrderBy = newOrderBy
            }

    unnestFromItem =
      let arrayAggItems = flip map (rowIdenExp : obCols)
            $ \s -> S.SEFnApp "array_agg" [s] Nothing
       in S.FIUnnest arrayAggItems (S.toTableAlias unnestTable)
            $ alias
            : map S.toColumnAlias newOBAliases

    newOrderBy = S.OrderByExp <$> NE.nonEmpty newOBItems

    (newOBItems, obCols, newOBAliases) = maybe ([], [], []) transformOrderBy ordBy
    transformOrderBy (S.OrderByExp l) = unzip3
      $ flip map (zip (toList l) [1 ..])
      $ \(obItem, i :: Int) ->
        let iden = Identifier $ "ob_col_" <> tshow i
         in ( obItem {S.oExpression = S.SEIdentifier iden},
              S.oExpression obItem,
              iden
            )

asSingleRowExtr :: S.ColumnAlias -> S.SQLExp
asSingleRowExtr col =
  S.SEFnApp "coalesce" [jsonAgg, S.SELit "null"] Nothing
  where
    jsonAgg =
      S.SEOpApp
        (S.SQLOp "->")
        [ S.SEFnApp "json_agg" [S.SEIdentifier $ toIdentifier col] Nothing,
          S.SEUnsafe "0"
        ]

asJsonAggExtr ::
  JsonAggSelect -> S.ColumnAlias -> PermissionLimitSubQuery -> Maybe S.OrderByExp -> S.Extractor
asJsonAggExtr jsonAggSelect als permLimitSubQuery ordByExpM =
  flip S.Extractor (Just als) $ case jsonAggSelect of
    JASMultipleRows -> withJsonAggExtr permLimitSubQuery ordByExpM als
    JASSingleObject -> asSingleRowExtr als

withColumnOp :: Maybe S.ColumnOp -> S.SQLExp -> S.SQLExp
withColumnOp colOpM sqlExp = case colOpM of
  Nothing -> sqlExp
  Just (S.ColumnOp opText cExp) -> S.mkSQLOpExp opText sqlExp cExp
