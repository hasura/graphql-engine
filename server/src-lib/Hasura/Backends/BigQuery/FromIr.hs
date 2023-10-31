-- | Translate from the DML to the BigQuery dialect.
module Hasura.Backends.BigQuery.FromIr
  ( mkSQLSelect,
    fromRootField,
    fromSelectAggregate,
    Error (..),
    runFromIr,
    FromIr,
    FromIrWriter (..),
    FromIrConfig (..),
    defaultFromIrConfig,
    bigQuerySourceConfigToFromIrConfig,
    Top (..), -- Re-export for FromIrConfig.
  )
where

import Control.Monad.Validate
import Data.HashMap.Strict qualified as HashMap
import Data.Int qualified as Int
import Data.List.Extended (appendToNonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Hasura.Backends.BigQuery.Instances.Types ()
import Hasura.Backends.BigQuery.Source (BigQuerySourceConfig (..))
import Hasura.Backends.BigQuery.Types as BigQuery
import Hasura.Function.Cache qualified as Functions
import Hasura.NativeQuery.IR (NativeQuery (..))
import Hasura.NativeQuery.Metadata (InterpolatedQuery)
import Hasura.NativeQuery.Types (NativeQueryName (..))
import Hasura.Prelude
import Hasura.RQL.IR qualified as Ir
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Column qualified as Rql
import Hasura.RQL.Types.Common qualified as Rql
import Hasura.RQL.Types.Relationships.Local (Nullable (..))
import Hasura.RQL.Types.Relationships.Local qualified as Rql
import Language.GraphQL.Draft.Syntax qualified as G

--------------------------------------------------------------------------------
-- Types

-- | Most of these errors should be checked for legitimacy.
data Error
  = FromTypeUnsupported (Ir.SelectFromG 'BigQuery Expression)
  | NoOrderSpecifiedInOrderBy
  | MalformedAgg
  | FieldTypeUnsupportedForNow (Ir.AnnFieldG 'BigQuery Void Expression)
  | AggTypeUnsupportedForNow (Ir.TableAggregateFieldG 'BigQuery Void Expression)
  | NodesUnsupportedForNow (Ir.TableAggregateFieldG 'BigQuery Void Expression)
  | NoProjectionFields
  | NoAggregatesMustBeABug
  | UnsupportedArraySelect (Ir.ArraySelectG 'BigQuery Void Expression)
  | UnsupportedOpExpG (Ir.OpExpG 'BigQuery Expression)
  | UnsupportedSQLExp Expression
  | UnsupportedDistinctOn
  | UnexpectedEmptyList
  | InvalidIntegerishSql Expression
  | ConnectionsNotSupported
  | ActionsNotSupported
  | -- | https://github.com/hasura/graphql-engine/issues/8526
    ComputedFieldsBooleanExpressionNotSupported
  | -- | https://github.com/hasura/graphql-engine/issues/8526
    ComputedFieldsOrderByNotSupported
  | -- | https://github.com/hasura/graphql-engine/issues/8521
    ScalarComputedFieldsNotSupported
  | NoParentEntityInternalError

instance Show Error where
  show =
    \case
      FromTypeUnsupported {} -> "FromTypeUnsupported"
      NoOrderSpecifiedInOrderBy {} -> "NoOrderSpecifiedInOrderBy"
      MalformedAgg {} -> "MalformedAgg"
      FieldTypeUnsupportedForNow {} -> "FieldTypeUnsupportedForNow"
      AggTypeUnsupportedForNow {} -> "AggTypeUnsupportedForNow"
      NodesUnsupportedForNow {} -> "NodesUnsupportedForNow"
      NoProjectionFields {} -> "NoProjectionFields"
      NoAggregatesMustBeABug {} -> "NoAggregatesMustBeABug"
      UnsupportedArraySelect {} -> "UnsupportedArraySelect"
      UnsupportedOpExpG {} -> "UnsupportedOpExpG"
      UnsupportedSQLExp {} -> "UnsupportedSQLExp"
      UnsupportedDistinctOn {} -> "UnsupportedDistinctOn"
      UnexpectedEmptyList {} -> "UnexpectedEmptyList"
      InvalidIntegerishSql {} -> "InvalidIntegerishSql"
      ConnectionsNotSupported {} -> "ConnectionsNotSupported"
      ActionsNotSupported {} -> "ActionsNotSupported"
      ComputedFieldsBooleanExpressionNotSupported {} -> "ComputedFieldsBooleanExpressionNotSupported"
      ComputedFieldsOrderByNotSupported {} -> "ComputedFieldsOrderByNotSupported"
      ScalarComputedFieldsNotSupported {} -> "ScalarComputedFieldsNotSupported"
      NoParentEntityInternalError {} -> "NoParentEntityInternalError"

-- | The base monad used throughout this module for all conversion
-- functions.
--
-- It's a Validate, so it'll continue going when it encounters errors
-- to accumulate as many as possible.
--
-- It also contains a mapping from entity prefixes to counters. So if
-- my prefix is "table" then there'll be a counter that lets me
-- generate table1, table2, etc. Same for any other prefix needed
-- (e.g. names for joins).
--
-- A ReaderT is used around this in most of the module too, for
-- setting the current entity that a given field name refers to. See
-- @fromColumn@.
newtype FromIr a = FromIr
  { unFromIr ::
      ReaderT
        FromIrReader
        ( StateT
            FromIrState
            ( WriterT
                FromIrWriter
                ( Validate (NonEmpty Error)
                )
            )
        )
        a
  }
  deriving (Functor, Applicative, Monad, MonadValidate (NonEmpty Error), MonadWriter FromIrWriter)

-- | Collected from using a native query in a query.
--   Each entry here because a CTE to be prepended to the query.
newtype FromIrWriter = FromIrWriter
  { fromIrWriterNativeQueries :: Map (Aliased NativeQueryName) (InterpolatedQuery Expression)
  }
  deriving newtype (Semigroup, Monoid)

data FromIrState = FromIrState
  { indices :: Map Text Int
  }

data FromIrReader = FromIrReader
  { config :: FromIrConfig
  }

-- | Config values for the from-IR translator.
data FromIrConfig = FromIrConfig
  { -- | Applies globally to all selects, and may be reduced to
    -- something even smaller by permission/user args.
    globalSelectLimit :: Top
  }

-- | A default config.
defaultFromIrConfig :: FromIrConfig
defaultFromIrConfig = FromIrConfig {globalSelectLimit = NoTop}

-- | Alias of parent SELECT FROM.
-- Functions underlying computed fields requires column values from
-- the table that is being used in FROM clause of parent SELECT.
--
-- Example SQL:
--
-- > SELECT
-- >   `t_author1`.`id` AS `id`,
-- >   `t_author1`.`name` AS `name`,
-- >   ARRAY(
-- >     SELECT
-- >       AS STRUCT `id`,
-- >       `title`,
-- >       `content`
-- >     FROM
-- >       UNNEST(
-- >         ARRAY(
-- >           SELECT
-- >             AS STRUCT *
-- >           FROM `hasura_test`.`fetch_articles`(`id` => `t_author1`.`id`)
-- >         )
-- >       )
-- >       LIMIT 1000
-- >   ) AS `articles`
-- > FROM
-- >   `hasura_test`.`author` AS `t_author1`
--
-- Where `t_author1` is the @'ParentSelectFromIdentity'
data ParentSelectFromEntity
  = -- | There's no parent entity
    NoParentEntity
  | -- | Alias of the parent SELECT FROM
    ParentEntityAlias EntityAlias

--------------------------------------------------------------------------------
-- Runners

runFromIr :: FromIrConfig -> FromIr a -> Validate (NonEmpty Error) (a, FromIrWriter)
runFromIr config fromIr =
  runWriterT
    $ evalStateT
      (runReaderT (unFromIr fromIr) (FromIrReader {config}))
      (FromIrState {indices = mempty})

bigQuerySourceConfigToFromIrConfig :: BigQuerySourceConfig -> FromIrConfig
bigQuerySourceConfigToFromIrConfig BigQuerySourceConfig {_scGlobalSelectLimit} =
  FromIrConfig {globalSelectLimit = Top _scGlobalSelectLimit}

--------------------------------------------------------------------------------
-- Similar rendition of old API

-- | Here is where we apply a top-level annotation to the select to
-- indicate to the data loader that this select ought to produce a
-- single object or an array.
mkSQLSelect ::
  Rql.JsonAggSelect ->
  Ir.AnnSelectG 'BigQuery (Ir.AnnFieldG 'BigQuery Void) Expression ->
  FromIr BigQuery.Select
mkSQLSelect jsonAggSelect annSimpleSel = do
  select <- noExtraPartitionFields <$> fromSelectRows NoParentEntity annSimpleSel
  pure
    ( select
        { selectCardinality =
            case jsonAggSelect of
              Rql.JASMultipleRows -> Many
              Rql.JASSingleObject -> One
        }
    )

-- | Convert from the IR database query into a select.
fromRootField :: Ir.QueryDB 'BigQuery Void Expression -> FromIr Select
fromRootField =
  \case
    (Ir.QDBSingleRow s) -> mkSQLSelect Rql.JASSingleObject s
    (Ir.QDBMultipleRows s) -> mkSQLSelect Rql.JASMultipleRows s
    (Ir.QDBAggregation s) -> fromSelectAggregate Nothing s

--------------------------------------------------------------------------------
-- Top-level exported functions

fromUnnestedJSON :: Expression -> [(ColumnName, ScalarType)] -> [Rql.FieldName] -> FromIr From
fromUnnestedJSON json columns _fields = do
  alias <- generateEntityAlias UnnestTemplate
  pure
    ( FromSelectJson
        ( Aliased
            { aliasedThing =
                SelectJson
                  { selectJsonBody = json,
                    selectJsonFields = columns
                  },
              aliasedAlias = entityAliasText alias
            }
        )
    )

fromSelectRows :: ParentSelectFromEntity -> Ir.AnnSelectG 'BigQuery (Ir.AnnFieldG 'BigQuery Void) Expression -> FromIr BigQuery.PartitionableSelect
fromSelectRows parentSelectFromEntity annSelectG = do
  let Ir.AnnSelectG
        { _asnFields = fields,
          _asnFrom = from,
          _asnPerm = perm,
          _asnArgs = args,
          _asnNamingConvention = _tCase
        } = annSelectG
      Ir.TablePerm {_tpLimit = mPermLimit, _tpFilter = permFilter} = perm
      permissionBasedTop =
        maybe NoTop (Top . fromIntegral) mPermLimit
  selectFrom <-
    case from of
      Ir.FromTable qualifiedObject -> fromQualifiedTable qualifiedObject
      Ir.FromFunction nm (Functions.FunctionArgsExp [BigQuery.AEInput json] _) (Just columns)
        | functionName nm == "unnest" -> fromUnnestedJSON json columns (map fst fields)
      Ir.FromFunction functionName (Functions.FunctionArgsExp positionalArgs namedArgs) Nothing ->
        fromFunction parentSelectFromEntity functionName positionalArgs namedArgs
      Ir.FromNativeQuery nativeQuery -> fromNativeQuery nativeQuery
      _ -> refute (pure (FromTypeUnsupported from))
  Args
    { argsOrderBy,
      argsWhere,
      argsJoins,
      argsTop,
      argsDistinct,
      argsOffset,
      argsExistingJoins
    } <-
    runReaderT (fromSelectArgsG args) (fromAlias selectFrom)
  fieldSources <-
    runReaderT
      (traverse (fromAnnFieldsG argsExistingJoins) fields)
      (fromAlias selectFrom)
  filterExpression <-
    runReaderT (fromAnnBoolExp permFilter) (fromAlias selectFrom)
  selectProjections <- selectProjectionsFromFieldSources True fieldSources
  globalTop <- getGlobalTop
  let select =
        Select
          { selectWith = Nothing,
            selectCardinality = Many,
            selectAsStruct = NoAsStruct,
            selectFinalWantedFields = pure (fieldTextNames fields),
            selectGroupBy = mempty,
            selectOrderBy = argsOrderBy,
            -- We DO APPLY the global top here, because this pulls down all rows.
            selectTop = globalTop <> permissionBasedTop <> argsTop,
            selectProjections,
            selectFrom,
            selectJoins = argsJoins <> concat (mapMaybe fieldSourceJoins fieldSources),
            selectWhere = argsWhere <> Where [filterExpression],
            selectOffset = int64Expr <$> argsOffset
          }
  case argsDistinct of
    Nothing ->
      pure $ simpleSelect select
    Just distinct ->
      simulateDistinctOn select distinct argsOrderBy

-- | Simulates DISTINCT ON for BigQuery using ROW_NUMBER() partitioned over distinct fields
--
-- Example:
--
-- For a GraphQL query:
-- @
-- hasura_test_article(distinct_on: author_id, order_by: [{author_id: asc}, {created_at: asc}]) {
--   id
--   title
-- }
-- @
--
-- it should produce from a query without a `distinct_on` clause:
--
-- SELECT `id`, `title`
-- FROM `hasura_test`.`article`
-- ORDER BY `author_id` ASC, `created_at` ASC
--
-- a query of the following form:
--
-- SELECT `id`, `title`
-- FROM (SELECT *,
--              ROW_NUMBER() OVER (PARTITION BY `author_id` ORDER BY `created_at` ASC) as `idx1`
--       FROM `hasura_test`.`article`) as `t_article1`
-- WHERE (`t_article1`.`idx1` = 1)
-- ORDER BY `t_article1`.`author_id` ASC
--
-- Note: this method returns PartitionableSelect as it could be joined using an array relation
-- which requires extra fields added to the PARTITION BY clause to return proper results
simulateDistinctOn :: Select -> NonEmpty ColumnName -> Maybe (NonEmpty OrderBy) -> FromIr PartitionableSelect
simulateDistinctOn select distinctOnColumns orderByColumns = do
  rowNumAlias <- generateEntityAlias IndexTemplate
  pure
    PartitionableSelect
      { pselectFrom = selectFrom select,
        pselectFinalize = \mExtraPartitionField ->
          let -- we use the same alias both for outer and inner selects
              alias = entityAliasText (fromAlias (selectFrom select))
              distinctFields = fmap (\(ColumnName name) -> FieldName name alias) distinctOnColumns
              finalDistinctFields = case mExtraPartitionField of
                Just extraFields
                  | Just neExtraFields <- nonEmpty extraFields ->
                      neExtraFields <> distinctFields
                _ -> distinctFields
              (distinctOnOrderBy, innerOrderBy) =
                case orderByColumns of
                  Just orderBy ->
                    let (distincts, others) = NE.partition (\OrderBy {..} -> orderByFieldName `elem` distinctFields) orderBy
                     in (NE.nonEmpty distincts, NE.nonEmpty others)
                  Nothing ->
                    (Nothing, Nothing)
              innerFrom =
                FromSelect
                  Aliased
                    { aliasedAlias = alias,
                      aliasedThing =
                        select
                          { selectProjections =
                              StarProjection
                                :| [ WindowProjection
                                       ( Aliased
                                           { aliasedAlias = unEntityAlias rowNumAlias,
                                             aliasedThing =
                                               RowNumberOverPartitionBy
                                                 finalDistinctFields
                                                 innerOrderBy
                                                 -- Above: Having the order by
                                                 -- in here ensures that we get the proper
                                                 -- row as the first one we select
                                                 -- in the outer select WHERE condition
                                                 -- to simulate DISTINCT ON semantics
                                           }
                                       )
                                   ],
                            selectTop = mempty,
                            selectJoins = mempty,
                            selectOrderBy = mempty,
                            selectOffset = Nothing,
                            selectGroupBy = mempty,
                            selectFinalWantedFields = mempty
                          }
                    }
           in select
                { selectFrom = innerFrom,
                  selectWhere =
                    Where
                      [ EqualExpression
                          (ColumnExpression FieldName {fieldNameEntity = alias, fieldName = unEntityAlias rowNumAlias})
                          (int64Expr 1)
                      ],
                  selectOrderBy = distinctOnOrderBy
                }
      }

fromSelectAggregate ::
  Maybe (EntityAlias, HashMap ColumnName ColumnName) ->
  Ir.AnnSelectG 'BigQuery (Ir.TableAggregateFieldG 'BigQuery Void) Expression ->
  FromIr BigQuery.Select
fromSelectAggregate minnerJoinFields annSelectG = do
  selectFrom <-
    case from of
      Ir.FromTable qualifiedObject -> fromQualifiedTable qualifiedObject
      _ -> refute (pure (FromTypeUnsupported from))
  args'@Args {argsWhere, argsOrderBy, argsJoins, argsTop, argsOffset, argsDistinct} <-
    runReaderT (fromSelectArgsG args) (fromAlias selectFrom)
  filterExpression <-
    runReaderT (fromAnnBoolExp permFilter) (fromAlias selectFrom)
  mforeignKeyConditions <-
    for minnerJoinFields $ \(entityAlias, mapping) ->
      runReaderT
        (fromMappingFieldNames (fromAlias selectFrom) mapping)
        entityAlias
  fieldSources <-
    runReaderT
      ( traverse
          ( fromTableAggregateFieldG
              args'
              permissionBasedTop
          )
          fields
      )
      (fromAlias selectFrom)
  selectProjections <- selectProjectionsFromFieldSources True fieldSources
  indexAlias <- generateEntityAlias IndexTemplate
  let innerSelectAlias = entityAliasText (fromAlias selectFrom)
      mDistinctFields = fmap (fmap (\(ColumnName name) -> FieldName name innerSelectAlias)) argsDistinct
      mPartitionFields = (mforeignKeyConditions >>= NE.nonEmpty . map fst) <> mDistinctFields
      innerProjections =
        case mPartitionFields of
          Nothing -> pure StarProjection
          Just partitionFields ->
            StarProjection
              :|
              -- We setup an index over every row in
              -- the sub select.  Then if you look at
              -- the outer Select, you can see we apply
              -- a WHERE that uses this index for
              -- LIMIT/OFFSET or DISTINCT ON.
              [ WindowProjection
                  ( Aliased
                      { aliasedAlias = unEntityAlias indexAlias,
                        aliasedThing =
                          RowNumberOverPartitionBy
                            -- The row numbers start from 1.
                            partitionFields
                            argsOrderBy
                            -- Above: Having the order by
                            -- in here ensures that the
                            -- row numbers are ordered by
                            -- this ordering. Below, we
                            -- order again for the
                            -- general row order. Both
                            -- are needed!
                      }
                  )
              ]
      indexColumn =
        ColumnExpression
          $ FieldName
            { fieldNameEntity = innerSelectAlias,
              fieldName = unEntityAlias indexAlias
            }
  pure
    Select
      { selectWith = Nothing,
        selectCardinality = One,
        selectAsStruct = NoAsStruct,
        selectFinalWantedFields = Nothing,
        selectGroupBy = mempty,
        selectProjections,
        selectTop = NoTop,
        selectFrom =
          FromSelect
            ( Aliased
                { aliasedThing =
                    Select
                      { selectWith = Nothing,
                        selectProjections = innerProjections,
                        selectAsStruct = NoAsStruct,
                        selectFrom,
                        selectJoins = argsJoins,
                        selectWhere = argsWhere <> (Where [filterExpression]),
                        selectOrderBy = argsOrderBy,
                        -- Above: This is important to have here, because
                        -- offset/top apply AFTER ordering is applied, so
                        -- you can't put an order by in afterwards in a
                        -- parent query. Therefore be careful about
                        -- putting this elsewhere.
                        selectFinalWantedFields = Nothing,
                        selectCardinality = Many,
                        selectTop = maybe argsTop (const NoTop) mforeignKeyConditions,
                        -- we apply offset only if we don't have partitions
                        -- when we do OFFSET/LIMIT based on ROW_NUMBER()
                        selectOffset = maybe (int64Expr <$> argsOffset) (const Nothing) mPartitionFields,
                        selectGroupBy = mempty
                      },
                  aliasedAlias = innerSelectAlias
                }
            ),
        selectJoins = concat (mapMaybe fieldSourceJoins fieldSources),
        selectWhere =
          case mPartitionFields of
            Nothing -> mempty
            Just {} ->
              let offset =
                    case argsDistinct of
                      Nothing ->
                        case argsOffset of
                          Nothing -> mempty
                          Just offset' ->
                            -- Apply an offset using the row_number from above.
                            [ OpExpression
                                MoreOp
                                indexColumn
                                (int64Expr offset')
                            ]
                      Just {} ->
                        -- in case of distinct_on we need to select the row number offset+1
                        -- effectively skipping number of rows equal to offset
                        [ EqualExpression
                            indexColumn
                            (int64Expr (fromMaybe 0 argsOffset + 1))
                        ]
                  limit =
                    case argsTop of
                      NoTop -> mempty
                      Top limit' ->
                        -- Apply a limit using the row_number from above.
                        [ OpExpression
                            LessOp
                            indexColumn
                            ( int64Expr (limit' + 1) -- Because the row_number() indexing starts at 1.
                            -- So idx<l+1  means idx<2 where l = 1 i.e. "limit to 1 row".
                            )
                        ]
               in Where (offset <> limit),
        selectOrderBy = Nothing,
        selectOffset = Nothing
      }
  where
    Ir.AnnSelectG
      { _asnFields = fields,
        _asnFrom = from,
        _asnPerm = perm,
        _asnArgs = args,
        _asnNamingConvention = _tCase
      } = annSelectG
    Ir.TablePerm {_tpLimit = mPermLimit, _tpFilter = permFilter} = perm
    permissionBasedTop =
      maybe NoTop (Top . fromIntegral) mPermLimit

--------------------------------------------------------------------------------
-- GraphQL Args

data Args = Args
  { argsWhere :: Where,
    argsOrderBy :: Maybe (NonEmpty OrderBy),
    argsJoins :: [Join],
    argsTop :: Top,
    argsOffset :: Maybe Int.Int64,
    argsDistinct :: Maybe (NonEmpty ColumnName),
    argsExistingJoins :: Map TableName EntityAlias
  }
  deriving (Show)

data UnfurledJoin = UnfurledJoin
  { unfurledJoin :: Join,
    -- | Recorded if we joined onto an object relation.
    unfurledObjectTableAlias :: Maybe (TableName, EntityAlias)
  }
  deriving (Show)

fromSelectArgsG :: Ir.SelectArgsG 'BigQuery Expression -> ReaderT EntityAlias FromIr Args
fromSelectArgsG selectArgsG = do
  argsWhere <-
    maybe (pure mempty) (fmap (Where . pure) . fromAnnBoolExp) mannBoolExp
  let argsTop = maybe mempty (Top . fromIntegral) mlimit
  (argsOrderBy, joins) <-
    runWriterT (traverse fromAnnotatedOrderByItemG (maybe [] toList orders))
  -- Any object-relation joins that we generated, we record their
  -- generated names into a mapping.
  let argsExistingJoins =
        M.fromList (mapMaybe unfurledObjectTableAlias (toList joins))
  pure
    Args
      { argsJoins = toList (fmap unfurledJoin joins),
        argsOrderBy = NE.nonEmpty argsOrderBy,
        -- TODO(redactionExp): Deal with the redaction expressions in distinct
        argsDistinct = fmap Ir._adcColumn <$> mdistinct,
        ..
      }
  where
    Ir.SelectArgs
      { _saWhere = mannBoolExp,
        _saLimit = mlimit,
        _saOffset = argsOffset,
        _saDistinct = mdistinct,
        _saOrderBy = orders
      } = selectArgsG

-- | Produce a valid ORDER BY construct, telling about any joins
-- needed on the side.
fromAnnotatedOrderByItemG ::
  Ir.AnnotatedOrderByItemG 'BigQuery Expression -> WriterT (Seq UnfurledJoin) (ReaderT EntityAlias FromIr) OrderBy
fromAnnotatedOrderByItemG Ir.OrderByItemG {obiType, obiColumn, obiNulls} = do
  (orderByFieldName, orderByExpression) <- unfurlAnnotatedOrderByElement obiColumn
  let morderByOrder =
        obiType
  let orderByNullsOrder =
        fromMaybe NullsAnyOrder obiNulls
  case morderByOrder of
    Just orderByOrder -> pure OrderBy {..}
    Nothing -> refute (pure NoOrderSpecifiedInOrderBy)

-- | Unfurl the nested set of object relations (tell'd in the writer)
-- that are terminated by field name (Ir.AOCColumn and
-- Ir.AOCArrayAggregation).
unfurlAnnotatedOrderByElement ::
  Ir.AnnotatedOrderByElement 'BigQuery Expression -> WriterT (Seq UnfurledJoin) (ReaderT EntityAlias FromIr) (FieldName, Expression)
unfurlAnnotatedOrderByElement =
  \case
    Ir.AOCColumn columnInfo redactionExp -> lift do
      fieldName <- fromColumnInfo columnInfo
      expression <- applyRedaction (Rql.ciColumn columnInfo) redactionExp

      pure (fieldName, expression)
    Ir.AOCObjectRelation Rql.RelInfo {riTarget = Rql.RelTargetNativeQuery _} _annBoolExp _annOrderByElementG ->
      error "unfurlAnnotatedOrderByElement RelTargetNativeQuery"
    Ir.AOCObjectRelation Rql.RelInfo {riMapping = Rql.RelMapping mapping, riTarget = Rql.RelTargetTable tableName} annBoolExp annOrderByElementG -> do
      selectFrom <- lift (lift (fromQualifiedTable tableName))
      joinAliasEntity <-
        lift (lift (generateEntityAlias (ForOrderAlias (tableNameText tableName))))
      joinOn <- lift (fromMappingFieldNames joinAliasEntity mapping)
      whereExpression <-
        lift (local (const (fromAlias selectFrom)) (fromAnnBoolExp annBoolExp))
      tell
        ( pure
            UnfurledJoin
              { unfurledJoin =
                  Join
                    { joinSource =
                        JoinSelect
                          Select
                            { selectWith = Nothing,
                              selectCardinality = One,
                              selectAsStruct = NoAsStruct,
                              selectFinalWantedFields = Nothing,
                              selectGroupBy = mempty,
                              selectTop = NoTop,
                              selectProjections = StarProjection :| [],
                              selectFrom,
                              selectJoins = [],
                              selectWhere = Where ([whereExpression]),
                              selectOrderBy = Nothing,
                              selectOffset = Nothing
                            },
                      joinRightTable = fromAlias selectFrom,
                      joinAlias = joinAliasEntity,
                      joinOn,
                      joinProvenance = OrderByJoinProvenance,
                      joinFieldName = tableNameText tableName, -- TODO: not needed.
                      joinExtractPath = Nothing,
                      joinType = LeftOuter
                    },
                unfurledObjectTableAlias = Just (tableName, joinAliasEntity)
              }
        )
      local (const joinAliasEntity) (unfurlAnnotatedOrderByElement annOrderByElementG)
    Ir.AOCArrayAggregation Rql.RelInfo {riTarget = Rql.RelTargetNativeQuery _} _annBoolExp _annAggregateOrderBy ->
      error "unfurlAnnotatedOrderByElement RelTargetNativeQuery"
    Ir.AOCArrayAggregation Rql.RelInfo {riMapping = Rql.RelMapping mapping, riTarget = Rql.RelTargetTable tableName} annBoolExp annAggregateOrderBy -> do
      selectFrom <- lift (lift (fromQualifiedTable tableName))
      let alias = aggFieldName
      joinAlias <-
        lift (lift (generateEntityAlias (ForOrderAlias (tableNameText tableName))))
      joinOn <- lift (fromMappingFieldNames joinAlias mapping)
      innerJoinFields <-
        lift (fromMappingFieldNames (fromAlias selectFrom) mapping)
      whereExpression <-
        lift (local (const (fromAlias selectFrom)) (fromAnnBoolExp annBoolExp))
      aggregate <-
        lift
          ( local
              (const (fromAlias selectFrom))
              ( case annAggregateOrderBy of
                  Ir.AAOCount -> pure (CountAggregate StarCountable)
                  -- TODO(redactionExp): Deal with the redaction expression
                  Ir.AAOOp (Ir.AggregateOrderByColumn text _resultType columnInfo _redactionExp) -> do
                    fieldName <- fromColumnInfo columnInfo
                    pure (OpAggregate text (ColumnExpression fieldName))
              )
          )
      tell
        ( pure
            ( UnfurledJoin
                { unfurledJoin =
                    Join
                      { joinSource =
                          JoinSelect
                            Select
                              { selectWith = Nothing,
                                selectCardinality = One,
                                selectAsStruct = NoAsStruct,
                                selectFinalWantedFields = Nothing,
                                selectTop = NoTop,
                                selectProjections =
                                  AggregateProjection
                                    Aliased
                                      { aliasedThing = aggregate,
                                        aliasedAlias = alias
                                      }
                                    :|
                                    -- These are group by'd below in selectGroupBy.
                                    map
                                      ( \(fieldName', _) ->
                                          FieldNameProjection
                                            Aliased
                                              { aliasedThing = fieldName',
                                                aliasedAlias = fieldName fieldName'
                                              }
                                      )
                                      innerJoinFields,
                                selectFrom,
                                selectJoins = [],
                                selectWhere = Where [whereExpression],
                                selectOrderBy = Nothing,
                                selectOffset = Nothing,
                                -- This group by corresponds to the field name projections above.
                                selectGroupBy = map fst innerJoinFields
                              },
                        joinRightTable = fromAlias selectFrom,
                        joinProvenance = OrderByJoinProvenance,
                        joinAlias = joinAlias,
                        joinOn,
                        joinFieldName = tableNameText tableName, -- TODO: not needed.
                        joinExtractPath = Nothing,
                        joinType = LeftOuter
                      },
                  unfurledObjectTableAlias = Nothing
                }
            )
        )
      let fieldName =
            FieldName
              { fieldNameEntity = entityAliasText joinAlias,
                fieldName = alias
              }
      pure (fieldName, ColumnExpression fieldName)
    Ir.AOCComputedField {} -> refute $ pure ComputedFieldsOrderByNotSupported

--------------------------------------------------------------------------------
-- Conversion functions

tableNameText :: TableName -> Text
tableNameText (TableName {tableName = qname}) = qname

-- | This is really the start where you query the base table,
-- everything else is joins attached to it.
fromQualifiedTable :: TableName -> FromIr From
fromQualifiedTable (TableName {tableNameSchema = schemaName, tableName = qname}) = do
  alias <- generateEntityAlias (TableTemplate qname)
  pure
    ( FromQualifiedTable
        ( Aliased
            { aliasedThing =
                TableName {tableName = qname, tableNameSchema = schemaName},
              aliasedAlias = entityAliasText alias
            }
        )
    )

-- | Build a @'From' expression out of a function that returns a set of rows.
fromFunction ::
  -- | The parent's entity alias from which the column values for computed fields are referred
  ParentSelectFromEntity ->
  -- | The function
  FunctionName ->
  -- | List of positional Arguments
  [ArgumentExp Expression] ->
  -- | List of named arguments
  HashMap.HashMap Text (ArgumentExp Expression) ->
  FromIr From
fromFunction parentEntityAlias functionName positionalArgs namedArgs = do
  alias <- generateEntityAlias (FunctionTemplate functionName)
  positionalArgExps <- mapM fromArgumentExp positionalArgs
  namedArgExps <- for (HashMap.toList namedArgs) $ \(argName, argValue) -> FunctionNamedArgument argName <$> fromArgumentExp argValue
  pure
    ( FromFunction
        ( Aliased
            { aliasedThing = SelectFromFunction functionName (positionalArgExps <> namedArgExps),
              aliasedAlias = entityAliasText alias
            }
        )
    )
  where
    fromArgumentExp :: ArgumentExp Expression -> FromIr Expression
    fromArgumentExp = \case
      AEInput e -> pure e
      AETableColumn (ColumnName columnName) -> do
        case parentEntityAlias of
          NoParentEntity -> refute $ pure NoParentEntityInternalError
          ParentEntityAlias entityAlias ->
            pure
              $ ColumnExpression
              $ FieldName columnName (entityAliasText entityAlias)

fromAnnBoolExp ::
  Ir.GBoolExp 'BigQuery (Ir.AnnBoolExpFld 'BigQuery Expression) ->
  ReaderT EntityAlias FromIr Expression
fromAnnBoolExp = traverse fromAnnBoolExpFld >=> fromGBoolExp

fromNativeQuery :: NativeQuery 'BigQuery Expression -> FromIr From
fromNativeQuery NativeQuery {..} = do
  alias <- generateEntityAlias (NativeQueryTemplate nqRootFieldName)
  let nqAlias = Aliased nqRootFieldName (entityAliasText alias)
  tell (FromIrWriter (M.singleton nqAlias nqInterpolatedQuery))
  pure (FromNativeQuery nqAlias)

fromAnnBoolExpFld ::
  Ir.AnnBoolExpFld 'BigQuery Expression -> ReaderT EntityAlias FromIr Expression
fromAnnBoolExpFld =
  \case
    Ir.AVColumn columnInfo redactionExp opExpGs -> do
      expression <- applyRedaction (Rql.ciColumn columnInfo) redactionExp
      expressions <- traverse (lift . fromOpExpG expression) opExpGs

      pure (AndExpression expressions)
    Ir.AVRelationship Rql.RelInfo {riTarget = Rql.RelTargetNativeQuery _} _ ->
      error "fromAnnBoolExpFld RelTargetNativeQuery"
    Ir.AVRemoteRelationship _ ->
      error "fromAnnBoolExpFld RemoteRelationship"
    Ir.AVRelationship Rql.RelInfo {riMapping = Rql.RelMapping mapping, riTarget = Rql.RelTargetTable table} (Ir.RelationshipFilters tablePerms annBoolExp) -> do
      selectFrom <- lift (fromQualifiedTable table)
      foreignKeyConditions <- fromMapping selectFrom mapping
      whereExpression <-
        local (const (fromAlias selectFrom)) (fromAnnBoolExp (Ir.BoolAnd [tablePerms, annBoolExp]))
      pure
        ( ExistsExpression
            Select
              { selectWith = Nothing,
                selectCardinality = One,
                selectAsStruct = NoAsStruct,
                selectFinalWantedFields = Nothing,
                selectGroupBy = mempty,
                selectOrderBy = Nothing,
                selectProjections =
                  ExpressionProjection
                    ( Aliased
                        { aliasedThing = trueExpression,
                          aliasedAlias = existsFieldName
                        }
                    )
                    :| [],
                selectFrom,
                selectJoins = mempty,
                selectWhere = Where (foreignKeyConditions <> [whereExpression]),
                selectTop = NoTop,
                selectOffset = Nothing
              }
        )
    Ir.AVComputedField {} -> refute $ pure ComputedFieldsBooleanExpressionNotSupported

fromColumnInfo :: Rql.ColumnInfo 'BigQuery -> ReaderT EntityAlias FromIr FieldName
fromColumnInfo Rql.ColumnInfo {ciColumn = ColumnName column} = do
  EntityAlias {entityAliasText} <- ask
  pure
    ( FieldName
        { fieldName = column,
          fieldNameEntity = entityAliasText
        }
    )

fromGExists :: Ir.GExists 'BigQuery Expression -> ReaderT EntityAlias FromIr Select
fromGExists Ir.GExists {_geTable, _geWhere} = do
  selectFrom <- lift (fromQualifiedTable _geTable)
  whereExpression <-
    local (const (fromAlias selectFrom)) (fromGBoolExp _geWhere)
  pure
    Select
      { selectWith = Nothing,
        selectCardinality = One,
        selectAsStruct = NoAsStruct,
        selectFinalWantedFields = Nothing,
        selectGroupBy = mempty,
        selectOrderBy = Nothing,
        selectProjections =
          ExpressionProjection
            ( Aliased
                { aliasedThing = trueExpression,
                  aliasedAlias = existsFieldName
                }
            )
            :| [],
        selectFrom,
        selectJoins = mempty,
        selectWhere = Where [whereExpression],
        selectTop = NoTop,
        selectOffset = Nothing
      }

--------------------------------------------------------------------------------
-- Sources of projected fields
--
-- Because in the IR, a field projected can be a foreign object, we
-- have to both generate a projection AND on the side generate a join.
--
-- So a @FieldSource@ couples the idea of the projected thing and the
-- source of it (via 'Aliased').

data FieldSource
  = ExpressionFieldSource (Aliased Expression)
  | JoinFieldSource (Aliased Join)
  | AggregateFieldSource Text (NonEmpty (Aliased Aggregate))
  | ArrayAggFieldSource (Aliased ArrayAgg) (Maybe [FieldSource])
  deriving (Eq, Show)

-- Example:
--
-- @
-- Track_aggregate {
--   aggregate {
--     count(columns: AlbumId)
--     foo: count(columns: AlbumId)
--     max {
--       AlbumId
--       TrackId
--     }
--   }
-- }
-- @
--
-- field =
-- @
-- TAFAgg
--   [ ( FieldName {getFieldNameTxt = "count"}
--     , AFCount (NonNullFieldCountable [ColumnName {columnName = "AlbumId"}]))
--   , ( FieldName {getFieldNameTxt = "foo"}
--     , AFCount (NonNullFieldCountable [ColumnName {columnName = "AlbumId"}]))
--   , ( FieldName {getFieldNameTxt = "max"}
--     , AFOp
--         (AggregateOp
--            { _aoOp = "max"
--            , _aoFields =
--                [ ( FieldName {getFieldNameTxt = "AlbumId"}
--                  , SFCol (ColumnName {columnName = "AlbumId"} (ColumnScalar IntegerScalarType)))
--                , ( FieldName {getFieldNameTxt = "TrackId"}
--                  , SFCol (ColumnName {columnName = "TrackId"} (ColumnScalar IntegerScalarType)))
--                ]
--            }))
--   ]
-- @
--
-- should produce:
--
-- SELECT COUNT(`t_Track1`.`AlbumId`) AS `count`,
--        COUNT(`t_Track1`.`AlbumId`) AS `foo`,
--        struct(max(`t_Track1`.`AlbumId`) AS `AlbumId`, max(`t_Track1`.`TrackId`) as TrackId) as `max`
-- FROM chinook.`Track` AS `t_Track1`
--
fromTableAggregateFieldG ::
  Args ->
  Top ->
  (Rql.FieldName, Ir.TableAggregateFieldG 'BigQuery Void Expression) ->
  ReaderT EntityAlias FromIr FieldSource
fromTableAggregateFieldG args permissionBasedTop (Rql.FieldName name, field) =
  case field of
    Ir.TAFAgg (aggregateFields :: [(Rql.FieldName, Ir.AggregateField 'BigQuery Expression)]) ->
      case NE.nonEmpty aggregateFields of
        Nothing -> refute (pure NoAggregatesMustBeABug)
        Just fields -> do
          aggregates <-
            traverse
              ( \(fieldName, aggregateField) -> do
                  fmap
                    ( \aliasedThing ->
                        Aliased {aliasedAlias = Rql.getFieldNameTxt fieldName, ..}
                    )
                    (fromAggregateField aggregateField)
              )
              fields
          pure (AggregateFieldSource name aggregates)
    Ir.TAFExp text ->
      pure
        ( ExpressionFieldSource
            Aliased
              { aliasedThing = BigQuery.ValueExpression (BigQuery.TypedValue BigQuery.StringScalarType (StringValue text)),
                aliasedAlias = name
              }
        )
    Ir.TAFNodes _ (fields :: [(Rql.FieldName, Ir.AnnFieldG 'BigQuery Void Expression)]) -> do
      fieldSources <-
        traverse
          (fromAnnFieldsG (argsExistingJoins args))
          fields
      arrayAggProjections <- lift (selectProjectionsFromFieldSources False fieldSources)
      globalTop <- lift getGlobalTop
      let arrayAgg =
            Aliased
              { aliasedThing =
                  ArrayAgg
                    { arrayAggProjections,
                      arrayAggOrderBy = argsOrderBy args,
                      arrayAggTop = globalTop <> argsTop args <> permissionBasedTop
                    },
                aliasedAlias = name
              }
      pure (ArrayAggFieldSource arrayAgg (Just fieldSources))

fromAggregateField :: Ir.AggregateField 'BigQuery Expression -> ReaderT EntityAlias FromIr Aggregate
fromAggregateField aggregateField =
  case aggregateField of
    Ir.AFExp text -> pure (TextAggregate text)
    Ir.AFCount (CountType countType) ->
      CountAggregate <$> case countType of
        StarCountable -> pure StarCountable
        NonNullFieldCountable names -> NonNullFieldCountable <$> traverse (uncurry applyRedaction) names
        DistinctCountable names -> DistinctCountable <$> traverse (uncurry applyRedaction) names
    Ir.AFOp Ir.AggregateOp {_aoOp = op, _aoFields = fields} -> do
      fs <- NE.nonEmpty fields `onNothing` refute (pure MalformedAgg)
      args <-
        traverse
          ( \(Rql.FieldName fieldName, columnField) -> do
              expression' <-
                case columnField of
                  Ir.SFCol column _columnType redactionExp -> applyRedaction column redactionExp
                  Ir.SFExp text -> pure (ValueExpression (BigQuery.TypedValue BigQuery.StringScalarType (StringValue text)))
                  -- See Hasura.RQL.Types.Backend.supportsAggregateComputedFields
                  Ir.SFComputedField _ _ -> error "Aggregate computed fields aren't currently supported for BigQuery!"
              pure (fieldName, expression')
          )
          fs
      pure (OpAggregates op args)

-- | The main sources of fields, either constants, fields or via joins.
fromAnnFieldsG ::
  Map TableName EntityAlias ->
  (Rql.FieldName, Ir.AnnFieldG 'BigQuery Void Expression) ->
  ReaderT EntityAlias FromIr FieldSource
fromAnnFieldsG existingJoins (Rql.FieldName name, field) =
  case field of
    Ir.AFColumn annColumnField -> do
      expression <- fromAnnColumnField annColumnField
      pure
        ( ExpressionFieldSource
            Aliased {aliasedThing = expression, aliasedAlias = name}
        )
    Ir.AFExpression text ->
      pure
        ( ExpressionFieldSource
            Aliased
              { aliasedThing = BigQuery.ValueExpression (BigQuery.TypedValue BigQuery.StringScalarType (StringValue text)),
                aliasedAlias = name
              }
        )
    Ir.AFObjectRelation objectRelationSelectG ->
      fmap
        ( \aliasedThing ->
            JoinFieldSource (Aliased {aliasedThing, aliasedAlias = name})
        )
        (fromObjectRelationSelectG existingJoins objectRelationSelectG)
    Ir.AFArrayRelation arraySelectG ->
      fmap
        ( \aliasedThing ->
            JoinFieldSource (Aliased {aliasedThing, aliasedAlias = name})
        )
        (fromArraySelectG arraySelectG)
    Ir.AFComputedField _ _ computedFieldSelect -> do
      expression <- fromComputedFieldSelect computedFieldSelect
      pure
        ( ExpressionFieldSource
            Aliased {aliasedThing = expression, aliasedAlias = name}
        )

-- | Here is where we project a field as a column expression. If
-- number stringification is on, then we wrap it in a
-- 'ToStringExpression' so that it's casted when being projected.
fromAnnColumnField ::
  Ir.AnnColumnField 'BigQuery Expression ->
  ReaderT EntityAlias FromIr Expression
fromAnnColumnField annColumnField = do
  fieldName <- fromColumn column
  if asText || False -- TODO: (Rql.isScalarColumnWhere Psql.isBigNum typ && stringifyNumbers == Rql.StringifyNumbers)
    then pure (ToStringExpression (ColumnExpression fieldName))
    else case redactionExp of
      Ir.NoRedaction -> pure (ColumnExpression fieldName)
      Ir.RedactIfFalse ex -> do
        ex' <- (traverse fromAnnBoolExpFld >=> fromGBoolExp) (coerce ex)
        pure (ConditionalProjection ex' fieldName)
  where
    Ir.AnnColumnField
      { _acfColumn = column,
        _acfAsText = asText :: Bool,
        _acfArguments = _ :: Maybe Void,
        _acfRedactionExpression = redactionExp :: Ir.AnnRedactionExp 'BigQuery Expression
      } = annColumnField

-- | This is where a field name "foo" is resolved to a fully qualified
-- field name [table].[foo]. The table name comes from EntityAlias in
-- the ReaderT.
fromColumn :: ColumnName -> ReaderT EntityAlias FromIr FieldName
fromColumn (ColumnName txt) = do
  EntityAlias {entityAliasText} <- ask
  pure (FieldName {fieldName = txt, fieldNameEntity = entityAliasText})

fieldSourceProjections :: Bool -> FieldSource -> FromIr (NonEmpty Projection)
fieldSourceProjections keepJoinField =
  \case
    ExpressionFieldSource aliasedExpression ->
      pure (ExpressionProjection aliasedExpression :| [])
    JoinFieldSource aliasedJoin ->
      toNonEmpty
        -- Here we're producing all join fields needed later for
        -- Haskell-native joining.  They will be removed by upstream
        -- code if keepJoinField is True
        ( [ FieldNameProjection
              ( Aliased
                  { aliasedThing = right,
                    aliasedAlias = fieldNameText right
                  }
              )
            | keepJoinField,
              (_left, right) <- joinOn join'
          ]
            <>
            -- Below:
            -- When we're doing an array-aggregate, e.g.
            --
            -- query MyQuery {
            --   hasura_Artist {
            --     albums_aggregate {
            --       aggregate {
            --         count
            --       }
            --     }
            --   }
            -- }
            --
            -- we're going to do a join on the albums table, and that
            -- join query will produce a single-row result. Therefore we
            -- can grab the whole entity as a STRUCT-typed object. See
            -- also the docs for 'fromArrayRelationSelectG' and for
            -- 'fromArrayAggregateSelectG'.
            case joinProvenance join' of
              ArrayJoinProvenance fields ->
                pure
                  ( ArrayEntityProjection
                      (joinAlias join')
                      aliasedJoin
                        { aliasedThing =
                            fmap
                              ( \name ->
                                  FieldName
                                    { fieldName = name,
                                      fieldNameEntity =
                                        entityAliasText (joinAlias join')
                                    }
                              )
                              fields,
                          aliasedAlias = aliasedAlias aliasedJoin
                        }
                  )
              ObjectJoinProvenance fields ->
                pure
                  ( EntityProjection
                      aliasedJoin
                        { aliasedThing =
                            fmap
                              ( \name ->
                                  ( FieldName
                                      { fieldName = name,
                                        fieldNameEntity =
                                          entityAliasText (joinAlias join')
                                      },
                                    NoOrigin
                                  )
                              )
                              fields,
                          aliasedAlias = aliasedAlias aliasedJoin
                        }
                  )
              ArrayAggregateJoinProvenance fields ->
                pure
                  ( EntityProjection
                      aliasedJoin
                        { aliasedThing =
                            fmap
                              ( \(name, fieldOrigin) ->
                                  ( FieldName
                                      { fieldName = name,
                                        fieldNameEntity =
                                          entityAliasText (joinAlias join')
                                      },
                                    fieldOrigin
                                  )
                              )
                              fields,
                          aliasedAlias = aliasedAlias aliasedJoin
                        }
                  )
              _ -> []
        )
      where
        join' = aliasedThing aliasedJoin
    AggregateFieldSource name aggregates ->
      pure
        ( AggregateProjections
            (Aliased {aliasedThing = aggregates, aliasedAlias = name})
            :| []
        )
    ArrayAggFieldSource arrayAgg _ -> pure (ArrayAggProjection arrayAgg :| [])
  where
    fieldNameText FieldName {fieldName} = fieldName

fieldSourceJoins :: FieldSource -> Maybe [Join]
fieldSourceJoins =
  \case
    JoinFieldSource aliasedJoin -> pure [aliasedThing aliasedJoin]
    ExpressionFieldSource {} -> Nothing
    AggregateFieldSource {} -> Nothing
    ArrayAggFieldSource _ sources -> fmap (concat . mapMaybe fieldSourceJoins) sources

--------------------------------------------------------------------------------
-- Joins

-- | Produce the join for an object relation. We produce a normal
-- select, but then include join fields. Then downstream, the
-- DataLoader will execute the lhs select and rhs join in separate
-- server queries, then do a Haskell-native join on the join fields.
--
-- See also 'fromArrayRelationSelectG' for similar example.
fromObjectRelationSelectG ::
  Map TableName EntityAlias ->
  Ir.ObjectRelationSelectG 'BigQuery Void Expression ->
  ReaderT EntityAlias FromIr Join
-- We're not using existingJoins at the moment, which was used to
-- avoid re-joining on the same table twice.
fromObjectRelationSelectG _existingJoins annRelationSelectG = do
  selectFrom <- case target of
    Ir.FromTable t -> lift $ fromQualifiedTable t
    Ir.FromNativeQuery nq -> lift $ fromNativeQuery nq
    other -> error $ "fromObjectRelationSelectG: " <> show other
  let entityAlias :: EntityAlias = fromAlias selectFrom
  fieldSources <-
    local
      (const entityAlias)
      (traverse (fromAnnFieldsG mempty) fields)
  selectProjections <- lift (selectProjectionsFromFieldSources True fieldSources)
  joinFieldName <- lift (fromRelName _aarRelationshipName)
  joinAlias <-
    lift (generateEntityAlias (ObjectRelationTemplate joinFieldName))
  filterExpression <- local (const entityAlias) (fromAnnBoolExp tableFilter)

  -- @mapping@ here describes the pairs of columns that form foreign key
  -- relationships between tables. For example, when querying an "article"
  -- table for article titles and joining on an "authors" table for author
  -- names, we could end up with something like the following:
  --
  -- @
  -- [ ( ColumnName { columnName = "author_id" }
  --   , ColumnName { columnName = "id" }
  --   )
  -- ]
  -- @
  --
  -- Note that the "local" table is on the left, and the "remote" table is on
  -- the right.

  joinFields <- fromMappingFieldNames (fromAlias selectFrom) mapping
  joinOn <- fromMappingFieldNames joinAlias mapping
  joinFieldProjections <- toNonEmpty (map prepareJoinFieldProjection joinFields)

  let selectFinalWantedFields = pure (fieldTextNames fields)
  pure
    Join
      { joinAlias,
        joinSource =
          JoinSelect
            Select
              { selectWith = Nothing,
                selectCardinality = One,
                selectAsStruct = NoAsStruct,
                selectFinalWantedFields,
                selectGroupBy = mempty,
                selectOrderBy = Nothing,
                selectTop = NoTop,
                selectProjections = joinFieldProjections <> selectProjections,
                selectFrom,
                selectJoins = concat (mapMaybe fieldSourceJoins fieldSources),
                selectWhere = Where [filterExpression],
                selectOffset = Nothing
              },
        joinOn,
        joinRightTable = fromAlias selectFrom,
        joinProvenance =
          ObjectJoinProvenance
            (fromMaybe [] selectFinalWantedFields), -- TODO: OK?
            -- Above: Needed by DataLoader to determine the type of
            -- Haskell-native join to perform.
        joinFieldName,
        joinExtractPath = Nothing,
        joinType = case nullable of Nullable -> LeftOuter; NotNullable -> Inner
      }
  where
    Ir.AnnObjectSelectG
      { _aosFields = fields :: Ir.AnnFieldsG 'BigQuery Void Expression,
        _aosTarget = target :: Ir.SelectFromG 'BigQuery Expression,
        _aosTargetFilter = tableFilter :: Ir.AnnBoolExp 'BigQuery Expression
      } = annObjectSelectG
    Ir.AnnRelationSelectG
      { _aarRelationshipName,
        _aarColumnMapping = mapping :: HashMap ColumnName ColumnName,
        _aarAnnSelect = annObjectSelectG :: Ir.AnnObjectSelectG 'BigQuery Void Expression,
        _aarNullable = nullable
      } = annRelationSelectG

-- We're not using existingJoins at the moment, which was used to
-- avoid re-joining on the same table twice.
_lookupTableFrom ::
  Map TableName EntityAlias ->
  TableName ->
  FromIr (Either EntityAlias From)
_lookupTableFrom existingJoins tableFrom = do
  case M.lookup tableFrom existingJoins of
    Just entityAlias -> pure (Left entityAlias)
    Nothing -> fmap Right (fromQualifiedTable tableFrom)

fromArraySelectG :: Ir.ArraySelectG 'BigQuery Void Expression -> ReaderT EntityAlias FromIr Join
fromArraySelectG =
  \case
    Ir.ASSimple arrayRelationSelectG ->
      fromArrayRelationSelectG arrayRelationSelectG
    Ir.ASAggregate arrayAggregateSelectG ->
      fromArrayAggregateSelectG arrayAggregateSelectG

-- | Generate a select field @'Expression' for a computed field
--
-- > ARRAY(
-- >   SELECT
-- >     AS STRUCT
-- >     `column_1`,
-- >     `column_2`,
-- >     `column_3`
-- >   FROM
-- >     UNNEST(
-- >       ARRAY(
-- >           SELECT AS STRUCT *
-- >           FROM `dataset`.`function_name`(`argument_name` => `parent_entity`.`column`)
-- >       )
-- >     )
-- >   LIMIT 1000 -- global limit
-- > ) AS `field_name`
--
-- Using 'LIMIT' right after 'FROM <function>' expression raises query exception.
-- To avoid this problem, we are packing and unpacking the rows returned from the function
-- using 'ARRAY' and 'UNNEST', then applying LIMIT. Somehow this is working with exact reason
-- being unknown. See https://github.com/hasura/graphql-engine/issues/8562 for more details.
fromComputedFieldSelect ::
  Ir.ComputedFieldSelect 'BigQuery Void Expression ->
  ReaderT EntityAlias FromIr Expression
fromComputedFieldSelect = \case
  Ir.CFSScalar {} ->
    -- As of now, we don't have support for computed fields returning a scalar value.
    -- See https://github.com/hasura/graphql-engine/issues/8521
    refute $ pure ScalarComputedFieldsNotSupported
  Ir.CFSTable jsonAggSelect annSimpleSelect -> do
    entityAlias <- ask
    select <- lift $ noExtraPartitionFields <$> fromSelectRows (ParentEntityAlias entityAlias) annSimpleSelect
    let selectWithCardinality =
          select
            { selectCardinality =
                case jsonAggSelect of
                  Rql.JASMultipleRows -> Many
                  Rql.JASSingleObject -> One,
              selectAsStruct = AsStruct,
              selectFrom = wrapUnnest (selectFrom select)
            }
    pure $ applyArrayOnSelect selectWithCardinality
  where
    applyArrayOnSelect :: Select -> Expression
    applyArrayOnSelect select =
      FunctionExpression (FunctionName "ARRAY" Nothing) [SelectExpression select]

    wrapUnnest :: From -> From
    wrapUnnest from =
      let starSelect =
            Select
              { selectWith = Nothing,
                selectTop = NoTop,
                selectAsStruct = AsStruct,
                selectProjections = pure StarProjection,
                selectFrom = from,
                selectJoins = [],
                selectWhere = Where [],
                selectOrderBy = Nothing,
                selectOffset = Nothing,
                selectGroupBy = [],
                selectFinalWantedFields = Nothing,
                selectCardinality = Many
              }
          arraySelect = applyArrayOnSelect starSelect
       in FromFunction
            Aliased
              { aliasedThing = SelectFromFunction (FunctionName "UNNEST" Nothing) [arraySelect],
                aliasedAlias = entityAliasText (fromAlias from)
              }

-- | Produce the join for an array aggregate relation. We produce a
-- normal select, but then include join fields. Then downstream, the
-- DataLoader will execute the lhs select and rhs join in separate
-- server queries, then do a Haskell-native join on the join fields.
--
-- See also 'fromArrayRelationSelectG' for similar example.
fromArrayAggregateSelectG ::
  Ir.AnnRelationSelectG 'BigQuery (Ir.AnnAggregateSelectG 'BigQuery Void Expression) ->
  ReaderT EntityAlias FromIr Join
fromArrayAggregateSelectG annRelationSelectG = do
  joinFieldName <- lift (fromRelName _aarRelationshipName)
  select <- do
    lhsEntityAlias <- ask
    lift (fromSelectAggregate (pure (lhsEntityAlias, mapping)) annSelectG)
  alias <- lift (generateEntityAlias (ArrayAggregateTemplate joinFieldName))

  joinOn <- fromMappingFieldNames alias mapping
  joinFields <- fromMappingFieldNames (fromAlias (selectFrom select)) mapping
  joinFieldProjections <- toNonEmpty (map prepareJoinFieldProjection joinFields)

  let projections = selectProjections select <> joinFieldProjections
      joinSelect =
        select
          { selectWhere = selectWhere select,
            selectGroupBy = map fst joinFields,
            selectProjections = projections
          }
  pure
    Join
      { joinAlias = alias,
        joinSource = JoinSelect joinSelect,
        joinRightTable = fromAlias (selectFrom select),
        joinOn,
        joinProvenance =
          ArrayAggregateJoinProvenance
            $ mapMaybe (\p -> (,aggregateProjectionsFieldOrigin p) <$> projectionAlias p)
            . toList
            . selectProjections
            $ select,
        -- Above: Needed by DataLoader to determine the type of
        -- Haskell-native join to perform.
        joinFieldName,
        joinExtractPath = Nothing,
        joinType = case nullable of Nullable -> LeftOuter; NotNullable -> Inner
      }
  where
    Ir.AnnRelationSelectG
      { _aarRelationshipName,
        _aarColumnMapping = mapping :: HashMap ColumnName ColumnName,
        _aarAnnSelect = annSelectG,
        _aarNullable = nullable
      } = annRelationSelectG

-- | Produce a join for an array relation.
--
-- Array relations in PG/MSSQL are expressed using LEFT OUTER JOIN
-- LATERAL or OUTER APPLY, which are essentially producing for each
-- row on the left an array of the result from the right. Which is
-- absolutely what you want for the array relationship.
--
-- BigQuery doesn't support that. Therefore we are instead performing
-- one big array aggregation, for ALL rows in the table - there is no
-- join occurring on the left-hand-side table, grouped by join
-- fields. The data-loader will perform the LHS query and the RHS query
-- separately.
--
-- What we do have is a GROUP BY and make sure that the join fields
-- are included in the output. Finally, in the
-- DataLoader.Plan/DataLoader.Execute, we implement a Haskell-native
-- join of the left-hand-side table and the right-hand-side table.
--
-- Data looks like:
--
--     join_field_a | join_field_b | aggFieldName (array type)
--     1            | 1            | [ { x: 1, y: 2 }, ... ]
--     1            | 2            | [ { x: 1, y: 2 }, ... ]
--
-- etc.
--
-- We want to produce a query that looks like:
--
--     SELECT artist_other_id,  -- For joining.
--
--            array_agg(struct(album_self_id, title)) as aggFieldName
--
--            -- ^ Aggregating the actual data.
--
--     FROM (SELECT *,  -- Get everything, plus the row number:
--
--                  ROW_NUMBER() OVER(PARTITION BY artist_other_id) artist_album_index
--
--           FROM hasura.Album
--           ORDER BY album_self_id ASC
--
--           -- ^ Order by here is important for stable results.  Any
--           order by clauses for the album should appear here, NOT IN
--           THE ARRAY_AGG.
--
--           )
--
--           AS indexed_album
--
--     WHERE artist_album_index > 1
--     -- ^ Here is where offsetting occurs.
--
--     GROUP BY artist_other_id
--     -- ^ Group by for joining.
--
--     ORDER BY artist_other_id;
--     ^ Ordering for the artist table should appear here.
--
-- Note: if original select already uses a PARTITION BY internally (for distinct_on)
-- join fields are added to partition expressions to give proper semantics of distinct_on
-- combined with an array relation
fromArrayRelationSelectG ::
  Ir.ArrayRelationSelectG 'BigQuery Void Expression ->
  ReaderT EntityAlias FromIr Join
fromArrayRelationSelectG annRelationSelectG = do
  pselect <- (lift . flip fromSelectRows annSelectG . ParentEntityAlias) =<< ask -- Take the original select.
  joinFieldName <- lift (fromRelName _aarRelationshipName)
  alias <- lift (generateEntityAlias (ArrayRelationTemplate joinFieldName))
  indexAlias <- lift (generateEntityAlias IndexTemplate)
  joinOn <- fromMappingFieldNames alias mapping
  joinFields <- fromMappingFieldNames (fromAlias (pselectFrom pselect)) mapping >>= toNonEmpty
  let select = withExtraPartitionFields pselect $ NE.toList (fmap fst joinFields)
      joinFieldProjections = fmap prepareJoinFieldProjection joinFields

  let joinSelect =
        Select
          { selectWith = Nothing,
            selectCardinality = One,
            selectAsStruct = NoAsStruct,
            selectFinalWantedFields = selectFinalWantedFields select,
            selectTop = NoTop,
            selectProjections =
              joinFieldProjections
                <> pure
                  ( ArrayAggProjection
                      Aliased
                        { aliasedThing =
                            ArrayAgg
                              { arrayAggProjections =
                                  fmap
                                    (aliasToFieldProjection (fromAlias (selectFrom select)))
                                    (selectProjections select),
                                arrayAggOrderBy = selectOrderBy select,
                                arrayAggTop = selectTop select
                                -- The sub-select takes care of caring about global top.
                                --
                                -- This handles the LIMIT need.
                              },
                          aliasedAlias = aggFieldName
                        }
                  ),
            selectFrom =
              FromSelect
                ( Aliased
                    { aliasedAlias = coerce (fromAlias (selectFrom select)),
                      aliasedThing =
                        Select
                          { selectWith = Nothing,
                            selectProjections =
                              selectProjections select
                                <> joinFieldProjections
                                `appendToNonEmpty` foldMap @Maybe
                                  ( map \OrderBy {orderByExpression, orderByFieldName} ->
                                      ExpressionProjection
                                        Aliased
                                          { aliasedThing = orderByExpression,
                                            aliasedAlias = fieldName orderByFieldName
                                          }
                                  )
                                  (toList <$> selectOrderBy select)
                                -- Above: Select "order by" fields as they're being used
                                -- inside `ARRAY_AGG` function (as ORDER BY clause)
                                <> pure
                                  ( WindowProjection
                                      ( Aliased
                                          { aliasedAlias = unEntityAlias indexAlias,
                                            aliasedThing =
                                              RowNumberOverPartitionBy
                                                -- The row numbers start from 1.
                                                ( fmap fst joinFields
                                                )
                                                (selectOrderBy select)
                                                -- Above: Having the order by
                                                -- in here ensures that the
                                                -- row numbers are ordered by
                                                -- this ordering. Below, we
                                                -- order again for the
                                                -- general row order. Both
                                                -- are needed!
                                          }
                                      )
                                  ),
                            selectFrom = selectFrom select,
                            selectJoins = selectJoins select,
                            selectWhere = selectWhere select,
                            selectOrderBy = selectOrderBy select,
                            -- Above: This orders the rows themselves. In
                            -- the RowNumberOverPartitionBy, we also set
                            -- a row order for the calculation of the
                            -- indices. Both are needed!
                            selectOffset = Nothing,
                            selectFinalWantedFields =
                              selectFinalWantedFields select,
                            selectCardinality = Many,
                            selectAsStruct = NoAsStruct,
                            selectTop = NoTop,
                            selectGroupBy = mempty
                          }
                    }
                ),
            selectWhere =
              case selectOffset select of
                Nothing -> mempty
                Just offset ->
                  Where
                    [ OpExpression
                        MoreOp
                        (ColumnExpression FieldName {fieldNameEntity = coerce (fromAlias (selectFrom select)), fieldName = unEntityAlias indexAlias})
                        offset
                    ],
            selectOrderBy = Nothing, -- Not needed.
            selectJoins = mempty,
            selectOffset = Nothing,
            -- This group by corresponds to the field name projections above. E.g. artist_other_id
            selectGroupBy = map fst (NE.toList joinFields)
          }
  pure
    Join
      { joinAlias = alias,
        joinSource = JoinSelect joinSelect,
        joinRightTable = fromAlias (selectFrom select),
        joinOn,
        joinProvenance =
          ArrayJoinProvenance
            ( if True
                then (fromMaybe [] (selectFinalWantedFields select))
                else
                  ( mapMaybe
                      projectionAlias
                      (toList (selectProjections select))
                  )
            ),
        -- Above: Needed by DataLoader to determine the type of
        -- Haskell-native join to perform.
        joinFieldName,
        joinExtractPath = Just aggFieldName,
        joinType = case nullable of Nullable -> LeftOuter; NotNullable -> Inner
      }
  where
    Ir.AnnRelationSelectG
      { _aarRelationshipName,
        _aarColumnMapping = mapping :: HashMap ColumnName ColumnName,
        _aarAnnSelect = annSelectG,
        _aarNullable = nullable
      } = annRelationSelectG

-- | For entity projections, convert any entity aliases to their field
-- names. ArrayEntityProjection and ExpressionProjection get converted
-- to aliases to fields with the same names as all the expressions
-- have already aliases applied in select from ArrayAgg
-- (created in Hasura.Backends.BigQuery.ToQuery.fromArrayAgg)
aliasToFieldProjection :: EntityAlias -> Projection -> Projection
aliasToFieldProjection (EntityAlias selectAlias) =
  \case
    EntityProjection Aliased {aliasedAlias = name, aliasedThing = fields} ->
      EntityProjection
        Aliased
          { aliasedAlias = name,
            aliasedThing =
              fmap
                (\(FieldName {..}, origin) -> (FieldName {fieldNameEntity = name, ..}, origin))
                fields
          }
    ArrayEntityProjection _ aliased ->
      aliasColumn aliased
    ExpressionProjection aliased ->
      aliasColumn aliased
    p -> p
  where
    aliasColumn :: Aliased a -> Projection
    aliasColumn aliased =
      ExpressionProjection
        aliased
          { aliasedThing = ColumnExpression (FieldName {fieldName = aliasedAlias aliased, fieldNameEntity = selectAlias})
          }

fromRelName :: Rql.RelName -> FromIr Text
fromRelName relName =
  pure (Rql.relNameToTxt relName)

-- | The context given by the reader is of the previous/parent
-- "remote" table. The WHERE that we're generating goes in the child,
-- "local" query. The @From@ passed in as argument is the local table.
--
-- We should hope to see e.g. "post.category = category.id" for a
-- local table of post and a remote table of category.
--
-- The left/right columns in @HashMap ColumnName ColumnName@ corresponds
-- to the left/right of @select ... join ...@. Therefore left=remote,
-- right=local in this context.
fromMapping ::
  From ->
  HashMap ColumnName ColumnName ->
  ReaderT EntityAlias FromIr [Expression]
fromMapping localFrom =
  traverse
    ( \(remoteColumn, localColumn) -> do
        localFieldName <- local (const (fromAlias localFrom)) (fromColumn localColumn)
        remoteFieldName <- fromColumn remoteColumn
        pure
          ( EqualExpression
              (ColumnExpression localFieldName)
              (ColumnExpression remoteFieldName)
          )
    )
    . HashMap.toList

-- | Given an alias for the remote table, and a map of local-to-remote column
-- name pairings, produce 'FieldName' pairings (column names paired with their
-- associated table names).
--
-- For example, we might convert the following:
--
-- @
-- [ ( ColumnName { columnName = "author_id" }
--   , ColumnName { columnName = "id" }
--   )
-- ]
-- @
--
-- ... into something like this:
--
-- @
-- ( FieldName
--     { fieldName = "id"
--     , fieldNameEntity = "t_author1"
--     }
-- , FieldName
--     { fieldName = "author_id"
--     , fieldNameEntity = "t_article1"
--     }
-- )
-- @
--
-- Note that the columns __flip around__ for the output. The input map is
-- @(local, remote)@.
fromMappingFieldNames ::
  EntityAlias ->
  HashMap ColumnName ColumnName ->
  ReaderT EntityAlias FromIr [(FieldName, FieldName)]
fromMappingFieldNames remoteFrom = traverse go . HashMap.toList
  where
    go (localColumn, remoteColumn) = do
      remoteFieldName <- local (const remoteFrom) (fromColumn remoteColumn)
      localFieldName <- fromColumn localColumn
      pure (remoteFieldName, localFieldName)

--------------------------------------------------------------------------------
-- Basic SQL expression types

fromOpExpG :: Expression -> Ir.OpExpG 'BigQuery Expression -> FromIr Expression
fromOpExpG expression op =
  case op of
    Ir.ANISNULL -> pure (IsNullExpression expression)
    Ir.ANISNOTNULL -> pure (IsNotNullExpression expression)
    Ir.AEQ Ir.NullableComparison val -> pure (nullableBoolEquality expression val)
    Ir.AEQ Ir.NonNullableComparison val -> pure (EqualExpression expression val)
    Ir.ANE Ir.NullableComparison val -> pure (nullableBoolInequality expression val)
    Ir.ANE Ir.NonNullableComparison val -> pure (NotEqualExpression expression val)
    Ir.AIN val -> pure (OpExpression InOp expression val)
    Ir.ANIN val -> pure (OpExpression NotInOp expression val)
    Ir.AGT val -> pure (OpExpression MoreOp expression val)
    Ir.ALT val -> pure (OpExpression LessOp expression val)
    Ir.AGTE val -> pure (OpExpression MoreOrEqualOp expression val)
    Ir.ALTE val -> pure (OpExpression LessOrEqualOp expression val)
    Ir.ACast _casts -> refute (pure (UnsupportedOpExpG op))
    Ir.ALIKE val -> pure (OpExpression LikeOp expression val)
    Ir.ANLIKE val -> pure (OpExpression NotLikeOp expression val)
    Ir.ABackendSpecific op' -> pure (fromBackendSpecificOpExpG expression op')
    Ir.CEQ _rhsCol -> refute (pure (UnsupportedOpExpG op))
    Ir.CNE _rhsCol -> refute (pure (UnsupportedOpExpG op))
    Ir.CGT _rhsCol -> refute (pure (UnsupportedOpExpG op))
    Ir.CLT _rhsCol -> refute (pure (UnsupportedOpExpG op))
    Ir.CGTE _rhsCol -> refute (pure (UnsupportedOpExpG op))
    Ir.CLTE _rhsCol -> refute (pure (UnsupportedOpExpG op))

fromBackendSpecificOpExpG :: Expression -> BigQuery.BooleanOperators Expression -> Expression
fromBackendSpecificOpExpG expression op =
  let func name val = FunctionExpression (FunctionName name Nothing) [expression, val]
   in case op of
        BigQuery.ASTContains v -> func "ST_CONTAINS" v
        BigQuery.ASTEquals v -> func "ST_EQUALS" v
        BigQuery.ASTTouches v -> func "ST_TOUCHES" v
        BigQuery.ASTWithin v -> func "ST_WITHIN" v
        BigQuery.ASTIntersects v -> func "ST_INTERSECTS" v
        BigQuery.ASTDWithin (Ir.DWithinGeogOp r v sph) ->
          FunctionExpression (FunctionName "ST_DWITHIN" Nothing) [expression, v, r, sph]

nullableBoolEquality :: Expression -> Expression -> Expression
nullableBoolEquality x y =
  OrExpression
    [ EqualExpression x y,
      AndExpression [IsNullExpression x, IsNullExpression y]
    ]

nullableBoolInequality :: Expression -> Expression -> Expression
nullableBoolInequality x y =
  OrExpression
    [ NotEqualExpression x y,
      AndExpression [IsNotNullExpression x, IsNullExpression y]
    ]

fromGBoolExp :: Ir.GBoolExp 'BigQuery Expression -> ReaderT EntityAlias FromIr Expression
fromGBoolExp =
  \case
    Ir.BoolAnd expressions ->
      fmap AndExpression (traverse fromGBoolExp expressions)
    Ir.BoolOr expressions ->
      fmap OrExpression (traverse fromGBoolExp expressions)
    Ir.BoolNot expression -> fmap NotExpression (fromGBoolExp expression)
    Ir.BoolExists gExists -> fmap ExistsExpression (fromGExists gExists)
    Ir.BoolField expression -> pure expression

--------------------------------------------------------------------------------
-- Misc combinators

-- | Attempt to refine a list into a 'NonEmpty'. If the given list is empty,
-- this will 'refute' the computation with an 'UnexpectedEmptyList' error.
toNonEmpty :: (MonadValidate (NonEmpty Error) m) => [x] -> m (NonEmpty x)
toNonEmpty = \case
  [] -> refute (UnexpectedEmptyList :| [])
  x : xs -> pure (x :| xs)

-- | Get the remote field from a pair (see 'fromMappingFieldNames' for more
-- information) and produce a 'Projection'.
prepareJoinFieldProjection :: (FieldName, FieldName) -> Projection
prepareJoinFieldProjection (fieldName', _) =
  FieldNameProjection
    Aliased
      { aliasedThing = fieldName',
        aliasedAlias = fieldName fieldName'
      }

selectProjectionsFromFieldSources :: Bool -> [FieldSource] -> FromIr (NonEmpty Projection)
selectProjectionsFromFieldSources keepJoinField fieldSources = do
  projections <- tolerate do
    projections' <- traverse (fieldSourceProjections keepJoinField) fieldSources
    toNonEmpty projections'

  case projections of
    Just (x :| xs) -> pure (foldl' (<>) x xs)
    Nothing -> refute (pure NoProjectionFields)

trueExpression :: Expression
trueExpression = ValueExpression (TypedValue BoolScalarType (BoolValue True))

applyRedaction :: ColumnName -> Ir.AnnRedactionExp 'BigQuery Expression -> ReaderT EntityAlias FromIr Expression
applyRedaction columnName redaction = do
  fieldName <- fromColumn columnName

  case redaction of
    Ir.NoRedaction -> pure (ColumnExpression fieldName)
    Ir.RedactIfFalse predicate -> do
      p <- fromAnnBoolExp predicate
      pure (ConditionalProjection p fieldName)

--------------------------------------------------------------------------------
-- Constants

aggFieldName :: Text
aggFieldName = "agg"

existsFieldName :: Text
existsFieldName = "exists_placeholder"

--------------------------------------------------------------------------------
-- Name generation

data NameTemplate
  = ArrayRelationTemplate Text
  | ArrayAggregateTemplate Text
  | ObjectRelationTemplate Text
  | TableTemplate Text
  | ForOrderAlias Text
  | IndexTemplate
  | UnnestTemplate
  | FunctionTemplate FunctionName
  | NativeQueryTemplate NativeQueryName

generateEntityAlias :: NameTemplate -> FromIr EntityAlias
generateEntityAlias template = do
  FromIr
    ( modify'
        ( \FromIrState {..} ->
            FromIrState {indices = M.insertWith (+) prefix start indices, ..}
        )
    )
  i <- FromIr (gets indices)
  pure (EntityAlias (prefix <> tshow (fromMaybe start (M.lookup prefix i))))
  where
    start = 1
    prefix = T.take 20 rendered
    rendered =
      case template of
        ArrayRelationTemplate sample -> "ar_" <> sample
        ArrayAggregateTemplate sample -> "aa_" <> sample
        ObjectRelationTemplate sample -> "or_" <> sample
        TableTemplate sample -> "t_" <> sample
        ForOrderAlias sample -> "order_" <> sample
        IndexTemplate -> "idx"
        UnnestTemplate -> "unnest"
        FunctionTemplate FunctionName {..} -> functionName
        NativeQueryTemplate NativeQueryName {..} -> G.unName getNativeQueryName

fromAlias :: From -> EntityAlias
fromAlias (FromQualifiedTable Aliased {aliasedAlias}) = EntityAlias aliasedAlias
fromAlias (FromSelect Aliased {aliasedAlias}) = EntityAlias aliasedAlias
fromAlias (FromSelectJson Aliased {aliasedAlias}) = EntityAlias aliasedAlias
fromAlias (FromFunction Aliased {aliasedAlias}) = EntityAlias aliasedAlias
fromAlias (FromNativeQuery Aliased {aliasedAlias}) = EntityAlias aliasedAlias

fieldTextNames :: Ir.AnnFieldsG 'BigQuery Void Expression -> [Text]
fieldTextNames = fmap (\(Rql.FieldName name, _) -> name)

unEntityAlias :: EntityAlias -> Text
unEntityAlias (EntityAlias t) = t

--------------------------------------------------------------------------------
-- Global limit support

getGlobalTop :: FromIr Top
getGlobalTop =
  FromIr
    ( asks
        ( \FromIrReader {config = FromIrConfig {globalSelectLimit}} ->
            globalSelectLimit
        )
    )
