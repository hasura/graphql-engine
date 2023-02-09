-- | Extractors are a pair of an SQL expression and an alias; they get
-- translated like "[SELECT ...] <expr> as <alias>"
module Hasura.Backends.Postgres.Translate.Select.Internal.Extractor
  ( aggregateFieldsToExtractorExps,
    mkAggregateOrderByExtractorAndFields,
    withJsonAggExtr,
    asSingleRowExtr,
    asJsonAggExtr,
  )
where

import Control.Monad.Writer.Strict
import Data.List.NonEmpty qualified as NE
import Hasura.Backends.Postgres.SQL.DML qualified as S
import Hasura.Backends.Postgres.SQL.Types
import Hasura.Backends.Postgres.Translate.Select.Internal.Aliases
import Hasura.Backends.Postgres.Translate.Types (PermissionLimitSubQuery (..))
import Hasura.Prelude
import Hasura.RQL.IR.Select
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.SQL.Backend

aggregateFieldsToExtractorExps ::
  TableIdentifier -> AggregateFields ('Postgres pgKind) -> [(S.ColumnAlias, S.SQLExp)]
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
      let qualCol = S.mkQIdenExp (mkBaseTableIdentifier sourcePrefix) (toIdentifier c)
          colAls = toIdentifier c
       in (S.toColumnAlias colAls, qualCol)

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
    AAOOp opText _resultType pgColumnInfo ->
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
            S.FromExp $
              pure $
                S.mkSelFromItem subSelect $
                  S.toTableAlias subSelAls
       in S.SESelect $
            S.mkSelect
              { S.selExtr = pure extr,
                S.selFrom = Just fromExp
              }

    mkSubSelect limit =
      let jsonRowExtr =
            flip S.Extractor (Just alias) $
              S.mkQIdenExp unnestTableIdentifier alias
          obExtrs = flip map newOBAliases $ \a ->
            S.Extractor (S.mkQIdenExp unnestTableIdentifier a) $ Just $ S.toColumnAlias a
       in S.mkSelect
            { S.selExtr = jsonRowExtr : obExtrs,
              S.selFrom = Just $ S.FromExp $ pure unnestFromItem,
              S.selLimit = Just $ S.LimitExp $ S.intToSQLExp limit,
              S.selOrderBy = newOrderBy
            }

    unnestFromItem =
      let arrayAggItems = flip map (rowIdenExp : obCols) $
            \s -> S.SEFnApp "array_agg" [s] Nothing
       in S.FIUnnest arrayAggItems (S.toTableAlias unnestTable) $
            alias : map S.toColumnAlias newOBAliases

    newOrderBy = S.OrderByExp <$> NE.nonEmpty newOBItems

    (newOBItems, obCols, newOBAliases) = maybe ([], [], []) transformOrderBy ordBy
    transformOrderBy (S.OrderByExp l) = unzip3 $
      flip map (zip (toList l) [1 ..]) $ \(obItem, i :: Int) ->
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
