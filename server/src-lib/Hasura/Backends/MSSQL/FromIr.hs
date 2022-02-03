{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_HADDOCK ignore-exports #-}

-- | Translate from the DML to the TSql dialect.
--
-- We use 'StateT' (newtype 'FromIr') as the base monad for all operations, since
-- state is used to mangle names such that the scope of identifiers in the IR is
-- preserved in the resulting TSQL.
--
-- For the MSSQL backend, a supported subset of the constructs that make
-- up its TSQL dialect are represented in the form of data-types in the
-- Hasura.Backends.MSSQL.Types module. In this module, we translate from RQL to
-- those TSQL types. And in 'ToQuery' we render/serialize/print the TSQL types to
-- query-strings that are suitable to be executed on the actual MSSQL database.
--
-- In places where a series of transations are scoped under a context, we use
-- 'ReaderT'. For example, such translations as pertaining to a table with an
-- alias, will require the alias for their translation operations, like qualified
-- equality checks under where clauses, etc., perhaps below multiple layers of
-- nested function calls.
module Hasura.Backends.MSSQL.FromIr
  ( fromSelectRows,
    mkSQLSelect,
    fromRootField,
    fromSelectAggregate,
    fromGBoolExp,
    Error (..),
    runFromIr,
    FromIr,
    jsonFieldName,
    fromInsert,
    toMerge,
    fromDelete,
    fromUpdate,
    toSelectIntoTempTable,
    toInsertValuesIntoTempTable,
  )
where

import Control.Monad.Validate
import Data.Containers.ListUtils (nubOrd)
import Data.HashMap.Strict qualified as HM
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Proxy
import Data.Text qualified as T
import Database.ODBC.SQLServer qualified as ODBC
import Hasura.Backends.MSSQL.Instances.Types ()
import Hasura.Backends.MSSQL.Types.Insert as TSQL (IfMatched (..))
import Hasura.Backends.MSSQL.Types.Internal as TSQL
import Hasura.Backends.MSSQL.Types.Update as TSQL (BackendUpdate (..), Update (..))
import Hasura.Prelude
import Hasura.RQL.IR qualified as IR
import Hasura.RQL.Types.Column qualified as IR
import Hasura.RQL.Types.Common qualified as IR
import Hasura.RQL.Types.Relationships.Local qualified as IR
import Hasura.SQL.Backend
import Language.GraphQL.Draft.Syntax (unName)

--------------------------------------------------------------------------------
-- Types

-- | Most of these errors should be checked for legitimacy.
data Error
  = UnsupportedOpExpG (IR.OpExpG 'MSSQL Expression)
  | FunctionNotSupported
  | NodesUnsupportedForNow
  | ConnectionsNotSupported
  deriving (Show, Eq)

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
  { unFromIr :: StateT (Map Text Int) (Validate (NonEmpty Error)) a
  }
  deriving (Functor, Applicative, Monad, MonadValidate (NonEmpty Error))

data StringifyNumbers
  = StringifyNumbers
  | LeaveNumbersAlone
  deriving (Eq)

--------------------------------------------------------------------------------
-- Runners

runFromIr :: FromIr a -> Validate (NonEmpty Error) a
runFromIr fromIr = evalStateT (unFromIr fromIr) mempty

--------------------------------------------------------------------------------
-- Similar rendition of old API

mkSQLSelect ::
  IR.JsonAggSelect ->
  IR.AnnSelectG 'MSSQL Void (IR.AnnFieldG 'MSSQL Void) Expression ->
  FromIr TSQL.Select
mkSQLSelect jsonAggSelect annSimpleSel =
  case jsonAggSelect of
    IR.JASMultipleRows -> fromSelectRows annSimpleSel
    IR.JASSingleObject ->
      fromSelectRows annSimpleSel <&> \sel ->
        sel
          { selectFor =
              JsonFor
                ForJson {jsonCardinality = JsonSingleton, jsonRoot = NoRoot},
            selectTop = Top 1
          }

-- | Convert from the IR database query into a select.
fromRootField :: IR.QueryDB 'MSSQL Void Expression -> FromIr Select
fromRootField =
  \case
    (IR.QDBSingleRow s) -> mkSQLSelect IR.JASSingleObject s
    (IR.QDBMultipleRows s) -> mkSQLSelect IR.JASMultipleRows s
    (IR.QDBAggregation s) -> fromSelectAggregate Nothing s

--------------------------------------------------------------------------------
-- Top-level exported functions

-- | Top/root-level 'Select'. All descendent/sub-translations are collected to produce a root TSQL.Select.
fromSelectRows :: IR.AnnSelectG 'MSSQL Void (IR.AnnFieldG 'MSSQL Void) Expression -> FromIr TSQL.Select
fromSelectRows annSelectG = do
  selectFrom <-
    case from of
      IR.FromTable qualifiedObject -> fromQualifiedTable qualifiedObject
      IR.FromIdentifier identifier -> pure $ FromIdentifier $ IR.unFIIdentifier identifier
      IR.FromFunction {} -> refute $ pure FunctionNotSupported
  Args
    { argsOrderBy,
      argsWhere,
      argsJoins,
      argsTop,
      argsDistinct = Proxy,
      argsOffset,
      argsExistingJoins
    } <-
    runReaderT (fromSelectArgsG args) (fromAlias selectFrom)
  fieldSources <-
    runReaderT
      (traverse (fromAnnFieldsG argsExistingJoins stringifyNumbers) fields)
      (fromAlias selectFrom)
  filterExpression <-
    runReaderT (fromGBoolExp permFilter) (fromAlias selectFrom)
  let selectProjections = map fieldSourceProjections fieldSources
  pure $
    emptySelect
      { selectOrderBy = argsOrderBy,
        selectTop = permissionBasedTop <> argsTop,
        selectProjections,
        selectFrom = Just selectFrom,
        selectJoins = argsJoins <> mapMaybe fieldSourceJoin fieldSources,
        selectWhere = argsWhere <> Where [filterExpression],
        selectFor =
          JsonFor ForJson {jsonCardinality = JsonArray, jsonRoot = NoRoot},
        selectOffset = argsOffset
      }
  where
    IR.AnnSelectG
      { _asnFields = fields,
        _asnFrom = from,
        _asnPerm = perm,
        _asnArgs = args,
        _asnStrfyNum = num
      } = annSelectG
    IR.TablePerm {_tpLimit = mPermLimit, _tpFilter = permFilter} = perm
    permissionBasedTop =
      maybe NoTop Top mPermLimit
    stringifyNumbers =
      if num
        then StringifyNumbers
        else LeaveNumbersAlone

mkNodesSelect :: Args -> Where -> Expression -> Top -> From -> [(Int, (IR.FieldName, [FieldSource]))] -> [(Int, Projection)]
mkNodesSelect Args {..} foreignKeyConditions filterExpression permissionBasedTop selectFrom nodes =
  [ ( index,
      ExpressionProjection $
        Aliased
          { aliasedThing =
              SelectExpression $
                emptySelect
                  { selectProjections = map fieldSourceProjections fieldSources,
                    selectTop = permissionBasedTop <> argsTop,
                    selectFrom = pure selectFrom,
                    selectJoins = argsJoins <> mapMaybe fieldSourceJoin fieldSources,
                    selectWhere = argsWhere <> Where [filterExpression] <> foreignKeyConditions,
                    selectFor =
                      JsonFor ForJson {jsonCardinality = JsonArray, jsonRoot = NoRoot},
                    selectOrderBy = argsOrderBy,
                    selectOffset = argsOffset
                  },
            aliasedAlias = IR.getFieldNameTxt fieldName
          }
    )
    | (index, (fieldName, fieldSources)) <- nodes
  ]

--
-- The idea here is that LIMIT/OFFSET and aggregates don't mix
-- well. Therefore we have a nested query:
--
-- select sum(*), .. FROM (select * from x offset o limit l) p
--
-- That's why @projections@ appears on the outer, and is a
-- @StarProjection@ for the inner. But the joins, conditions, top,
-- offset are on the inner.
--
mkAggregateSelect :: Args -> Where -> Expression -> From -> [(Int, (IR.FieldName, [Projection]))] -> [(Int, Projection)]
mkAggregateSelect Args {..} foreignKeyConditions filterExpression selectFrom aggregates =
  [ ( index,
      ExpressionProjection $
        Aliased
          { aliasedThing =
              JsonQueryExpression $
                SelectExpression $
                  emptySelect
                    { selectProjections = projections,
                      selectTop = NoTop,
                      selectFrom =
                        pure $
                          FromSelect
                            Aliased
                              { aliasedAlias = aggSubselectName,
                                aliasedThing =
                                  emptySelect
                                    { selectProjections = pure StarProjection,
                                      selectTop = argsTop,
                                      selectFrom = pure selectFrom,
                                      selectJoins = argsJoins,
                                      selectWhere = argsWhere <> Where [filterExpression] <> foreignKeyConditions,
                                      selectFor = NoFor,
                                      selectOrderBy = mempty,
                                      selectOffset = argsOffset
                                    }
                              },
                      selectJoins = mempty,
                      selectWhere = mempty,
                      selectFor =
                        JsonFor
                          ForJson
                            { jsonCardinality = JsonSingleton,
                              jsonRoot = NoRoot
                            },
                      selectOrderBy = mempty,
                      selectOffset = Nothing
                    },
            aliasedAlias = IR.getFieldNameTxt fieldName
          }
    )
    | (index, (fieldName, projections)) <- aggregates
  ]

fromSelectAggregate ::
  Maybe (EntityAlias, HashMap ColumnName ColumnName) ->
  IR.AnnSelectG 'MSSQL Void (IR.TableAggregateFieldG 'MSSQL Void) Expression ->
  FromIr TSQL.Select
fromSelectAggregate
  mparentRelationship
  IR.AnnSelectG
    { _asnFields = (zip [0 ..] -> fields),
      _asnFrom = from,
      _asnPerm = IR.TablePerm {_tpLimit = (maybe NoTop Top -> permissionBasedTop), _tpFilter = permFilter},
      _asnArgs = args,
      _asnStrfyNum = (bool LeaveNumbersAlone StringifyNumbers -> stringifyNumbers)
    } =
    do
      selectFrom <- case from of
        IR.FromTable qualifiedObject -> fromQualifiedTable qualifiedObject
        IR.FromIdentifier identifier -> pure $ FromIdentifier $ IR.unFIIdentifier identifier
        IR.FromFunction {} -> refute $ pure FunctionNotSupported
      -- Below: When we're actually a RHS of a query (of CROSS APPLY),
      -- then we'll have a LHS table that we're joining on. So we get the
      -- conditions expressions from the field mappings. The LHS table is
      -- the entityAlias, and the RHS table is selectFrom.
      mforeignKeyConditions <- fmap (Where . fromMaybe []) $
        for mparentRelationship $
          \(entityAlias, mapping) ->
            runReaderT (fromMapping selectFrom mapping) entityAlias
      filterExpression <- runReaderT (fromGBoolExp permFilter) (fromAlias selectFrom)
      args'@Args {argsExistingJoins} <-
        runReaderT (fromSelectArgsG args) (fromAlias selectFrom)
      -- Although aggregates, exps and nodes could be handled in one list,
      -- we need to separately treat the subselect expressions
      expss :: [(Int, Projection)] <- flip runReaderT (fromAlias selectFrom) $ sequence $ mapMaybe fromTableExpFieldG fields
      nodes :: [(Int, (IR.FieldName, [FieldSource]))] <-
        flip runReaderT (fromAlias selectFrom) $ sequence $ mapMaybe (fromTableNodesFieldG argsExistingJoins stringifyNumbers) fields
      let aggregates :: [(Int, (IR.FieldName, [Projection]))] = mapMaybe fromTableAggFieldG fields
      pure
        emptySelect
          { selectProjections =
              map snd $
                sortBy (comparing fst) $
                  expss
                    <> mkNodesSelect args' mforeignKeyConditions filterExpression permissionBasedTop selectFrom nodes
                    <> mkAggregateSelect args' mforeignKeyConditions filterExpression selectFrom aggregates,
            selectTop = NoTop,
            selectFrom =
              pure $
                FromOpenJson $
                  Aliased
                    { aliasedThing =
                        OpenJson
                          { openJsonExpression = ValueExpression $ ODBC.TextValue "[0]",
                            openJsonWith = Nothing
                          },
                      aliasedAlias = existsFieldName
                    },
            selectJoins = mempty, -- JOINs and WHEREs are only relevant in subselects
            selectWhere = mempty,
            selectFor = JsonFor ForJson {jsonCardinality = JsonSingleton, jsonRoot = NoRoot},
            selectOrderBy = Nothing,
            selectOffset = Nothing
          }

--------------------------------------------------------------------------------
-- GraphQL Args

data Args = Args
  { argsWhere :: Where,
    argsOrderBy :: Maybe (NonEmpty OrderBy),
    argsJoins :: [Join],
    argsTop :: Top,
    argsOffset :: Maybe Expression,
    argsDistinct :: Proxy (Maybe (NonEmpty FieldName)),
    argsExistingJoins :: Map TableName EntityAlias
  }
  deriving (Show)

data UnfurledJoin = UnfurledJoin
  { unfurledJoin :: Join,
    -- | Recorded if we joined onto an object relation.
    unfurledObjectTableAlias :: Maybe (TableName, EntityAlias)
  }
  deriving (Show)

fromSelectArgsG :: IR.SelectArgsG 'MSSQL Expression -> ReaderT EntityAlias FromIr Args
fromSelectArgsG selectArgsG = do
  let argsOffset = ValueExpression . ODBC.IntValue . fromIntegral <$> moffset
  argsWhere <-
    maybe (pure mempty) (fmap (Where . pure) . fromGBoolExp) mannBoolExp
  argsTop <-
    maybe (pure mempty) (pure . Top) mlimit
  -- Not supported presently, per Vamshi:
  --
  -- > It is hardly used and we don't have to go to great lengths to support it.
  --
  -- But placeholdering the code so that when it's ready to be used,
  -- you can just drop the Proxy wrapper.
  let argsDistinct = Proxy
  (argsOrderBy, joins) <-
    runWriterT (traverse fromAnnotatedOrderByItemG (maybe [] toList orders))
  -- Any object-relation joins that we generated, we record their
  -- generated names into a mapping.
  let argsExistingJoins =
        M.fromList (mapMaybe unfurledObjectTableAlias (toList joins))
  pure
    Args
      { argsJoins = toList (fmap unfurledJoin joins),
        argsOrderBy = nonEmpty argsOrderBy,
        ..
      }
  where
    IR.SelectArgs
      { _saWhere = mannBoolExp,
        _saLimit = mlimit,
        _saOffset = moffset,
        _saOrderBy = orders
      } = selectArgsG

-- | Produce a valid ORDER BY construct, telling about any joins
-- needed on the side.
fromAnnotatedOrderByItemG ::
  IR.AnnotatedOrderByItemG 'MSSQL Expression ->
  WriterT (Seq UnfurledJoin) (ReaderT EntityAlias FromIr) OrderBy
fromAnnotatedOrderByItemG IR.OrderByItemG {obiType, obiColumn = obiColumn, obiNulls} = do
  (orderByFieldName, orderByType) <- unfurlAnnotatedOrderByElement obiColumn
  let orderByNullsOrder = fromMaybe NullsAnyOrder obiNulls
      orderByOrder = fromMaybe AscOrder obiType
  pure OrderBy {..}

-- | Unfurl the nested set of object relations (tell'd in the writer)
-- that are terminated by field name (IR.AOCColumn and
-- IR.AOCArrayAggregation).
unfurlAnnotatedOrderByElement ::
  IR.AnnotatedOrderByElement 'MSSQL Expression ->
  WriterT (Seq UnfurledJoin) (ReaderT EntityAlias FromIr) (FieldName, Maybe TSQL.ScalarType)
unfurlAnnotatedOrderByElement =
  \case
    IR.AOCColumn columnInfo -> do
      fieldName <- lift (fromColumnInfo columnInfo)
      pure
        ( fieldName,
          case IR.ciType columnInfo of
            IR.ColumnScalar t -> Just t
            -- Above: It is of interest to us whether the type is
            -- text/ntext/image. See ToQuery for more explanation.
            _ -> Nothing
        )
    IR.AOCObjectRelation IR.RelInfo {riMapping = mapping, riRTable = table} annBoolExp annOrderByElementG -> do
      selectFrom <- lift (lift (fromQualifiedTable table))
      joinAliasEntity <-
        lift (lift (generateAlias (ForOrderAlias (tableNameText table))))
      foreignKeyConditions <- lift (fromMapping selectFrom mapping)
      -- TODO: Because these object relations are re-used by regular
      -- object mapping queries, this WHERE may be unnecessarily
      -- restrictive. But I actually don't know from where such an
      -- expression arises in the source GraphQL syntax.
      --
      -- Worst case scenario, we could put the WHERE in the key of the
      -- Map in 'argsExistingJoins'. That would guarantee only equal
      -- selects are re-used.
      whereExpression <-
        lift (local (const (fromAlias selectFrom)) (fromGBoolExp annBoolExp))
      tell
        ( pure
            UnfurledJoin
              { unfurledJoin =
                  Join
                    { joinSource =
                        JoinSelect
                          emptySelect
                            { selectTop = NoTop,
                              selectProjections = [StarProjection],
                              selectFrom = Just selectFrom,
                              selectJoins = [],
                              selectWhere =
                                Where (foreignKeyConditions <> [whereExpression]),
                              selectFor = NoFor,
                              selectOrderBy = Nothing,
                              selectOffset = Nothing
                            },
                      joinJoinAlias =
                        JoinAlias {joinAliasEntity, joinAliasField = Nothing}
                    },
                unfurledObjectTableAlias = Just (table, EntityAlias joinAliasEntity)
              }
        )
      local
        (const (EntityAlias joinAliasEntity))
        (unfurlAnnotatedOrderByElement annOrderByElementG)
    IR.AOCArrayAggregation IR.RelInfo {riMapping = mapping, riRTable = tableName} annBoolExp annAggregateOrderBy -> do
      selectFrom <- lift (lift (fromQualifiedTable tableName))
      let alias = aggFieldName
      joinAliasEntity <-
        lift (lift (generateAlias (ForOrderAlias (tableNameText tableName))))
      foreignKeyConditions <- lift (fromMapping selectFrom mapping)
      whereExpression <-
        lift (local (const (fromAlias selectFrom)) (fromGBoolExp annBoolExp))
      aggregate <-
        lift
          ( local
              (const (fromAlias selectFrom))
              ( case annAggregateOrderBy of
                  IR.AAOCount -> pure (CountAggregate StarCountable)
                  IR.AAOOp text columnInfo -> do
                    fieldName <- fromColumnInfo columnInfo
                    pure (OpAggregate text (pure (ColumnExpression fieldName)))
              )
          )
      tell
        ( pure
            ( UnfurledJoin
                { unfurledJoin =
                    Join
                      { joinSource =
                          JoinSelect
                            emptySelect
                              { selectTop = NoTop,
                                selectProjections =
                                  [ AggregateProjection
                                      Aliased
                                        { aliasedThing = aggregate,
                                          aliasedAlias = alias
                                        }
                                  ],
                                selectFrom = Just selectFrom,
                                selectJoins = [],
                                selectWhere =
                                  Where
                                    (foreignKeyConditions <> [whereExpression]),
                                selectFor = NoFor,
                                selectOrderBy = Nothing,
                                selectOffset = Nothing
                              },
                        joinJoinAlias =
                          JoinAlias {joinAliasEntity, joinAliasField = Nothing}
                      },
                  unfurledObjectTableAlias = Nothing
                }
            )
        )
      pure
        ( FieldName {fieldNameEntity = joinAliasEntity, fieldName = alias},
          Nothing
        )

--------------------------------------------------------------------------------
-- Conversion functions

tableNameText :: TableName -> Text
tableNameText (TableName {tableName}) = tableName

-- | This is really the start where you query the base table,
-- everything else is joins attached to it.
fromQualifiedTable :: TableName -> FromIr From
fromQualifiedTable schemadTableName@(TableName {tableName}) = do
  alias <- generateAlias (TableTemplate tableName)
  pure
    ( FromQualifiedTable
        ( Aliased
            { aliasedThing = schemadTableName,
              aliasedAlias = alias
            }
        )
    )

fromTableName :: TableName -> FromIr EntityAlias
fromTableName TableName {tableName} = do
  alias <- generateAlias (TableTemplate tableName)
  pure (EntityAlias alias)

-- | Translate an 'AnnBoolExpFld' within an 'EntityAlias' context referring to the table the `AnnBoolExpFld` field belongs to.
--
-- This is mutually recursive with 'fromGBoolExp', mirroring the mutually recursive structure between 'AnnBoolExpFld' and 'AnnBoolExp b a' (alias of 'GBoolExp b (AnnBoolExpFld b a)').
fromAnnBoolExpFld ::
  IR.AnnBoolExpFld 'MSSQL Expression ->
  ReaderT EntityAlias FromIr Expression
fromAnnBoolExpFld =
  \case
    IR.AVColumn columnInfo opExpGs -> do
      expression <- fromColumnInfoForBoolExp columnInfo
      expressions <- traverse (lift . fromOpExpG expression) opExpGs
      pure (AndExpression expressions)
    IR.AVRelationship IR.RelInfo {riMapping = mapping, riRTable = table} annBoolExp -> do
      selectFrom <- lift (fromQualifiedTable table)
      foreignKeyConditions <- fromMapping selectFrom mapping
      whereExpression <-
        local (const (fromAlias selectFrom)) (fromGBoolExp annBoolExp)
      pure
        ( ExistsExpression
            emptySelect
              { selectOrderBy = Nothing,
                selectProjections =
                  [ ExpressionProjection
                      ( Aliased
                          { aliasedThing = trueExpression,
                            aliasedAlias = existsFieldName
                          }
                      )
                  ],
                selectFrom = Just selectFrom,
                selectJoins = mempty,
                selectWhere = Where (foreignKeyConditions <> [whereExpression]),
                selectTop = NoTop,
                selectFor = NoFor,
                selectOffset = Nothing
              }
        )

-- | For boolean operators, various comparison operators used need
-- special handling to ensure that SQL Server won't outright reject
-- the comparison. See also 'shouldCastToVarcharMax'.
fromColumnInfoForBoolExp :: IR.ColumnInfo 'MSSQL -> ReaderT EntityAlias FromIr Expression
fromColumnInfoForBoolExp IR.ColumnInfo {ciColumn = column, ciType} = do
  fieldName <- columnNameToFieldName column <$> ask
  if shouldCastToVarcharMax ciType -- See function commentary.
    then pure (CastExpression (ColumnExpression fieldName) WvarcharType DataLengthMax)
    else pure (ColumnExpression fieldName)

-- | There's a problem of comparing text fields with =, <, etc. that
-- SQL Server completely refuses to do so. So one way to workaround
-- this restriction is to automatically cast such text fields to
-- varchar(max).
shouldCastToVarcharMax :: IR.ColumnType 'MSSQL -> Bool
shouldCastToVarcharMax typ =
  typ == IR.ColumnScalar TextType || typ == IR.ColumnScalar WtextType

fromColumnInfo :: IR.ColumnInfo 'MSSQL -> ReaderT EntityAlias FromIr FieldName
fromColumnInfo IR.ColumnInfo {ciColumn = column} =
  columnNameToFieldName column <$> ask

--  entityAlias <- ask
--  pure
--    (columnNameToFieldName column entityAlias
--     FieldName
--       {fieldName = columnName column, fieldNameEntity = entityAliasText})

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
  deriving (Eq, Show)

-- | Get FieldSource from a TAFExp type table aggregate field
fromTableExpFieldG :: -- TODO: Convert function to be similar to Nodes function
  (Int, (IR.FieldName, IR.TableAggregateFieldG 'MSSQL Void Expression)) ->
  Maybe (ReaderT EntityAlias FromIr (Int, Projection))
fromTableExpFieldG = \case
  (index, (IR.FieldName name, IR.TAFExp text)) ->
    Just $
      pure $
        ( index,
          fieldSourceProjections $
            ExpressionFieldSource
              Aliased
                { aliasedThing = TSQL.ValueExpression (ODBC.TextValue text),
                  aliasedAlias = name
                }
        )
  _ -> Nothing

fromTableAggFieldG ::
  (Int, (IR.FieldName, IR.TableAggregateFieldG 'MSSQL Void Expression)) ->
  Maybe (Int, (IR.FieldName, [Projection]))
fromTableAggFieldG = \case
  (index, (fieldName, IR.TAFAgg (aggregateFields :: [(IR.FieldName, IR.AggregateField 'MSSQL)]))) ->
    Just $
      let aggregates =
            aggregateFields <&> \(fieldName', aggregateField) ->
              fromAggregateField (IR.getFieldNameTxt fieldName') aggregateField
       in (index, (fieldName, aggregates))
  _ -> Nothing

fromTableNodesFieldG ::
  Map TableName EntityAlias ->
  StringifyNumbers ->
  (Int, (IR.FieldName, IR.TableAggregateFieldG 'MSSQL Void Expression)) ->
  Maybe (ReaderT EntityAlias FromIr (Int, (IR.FieldName, [FieldSource])))
fromTableNodesFieldG argsExistingJoins stringifyNumbers = \case
  (index, (fieldName, IR.TAFNodes () (annFieldsG :: [(IR.FieldName, IR.AnnFieldG 'MSSQL Void Expression)]))) -> Just do
    fieldSources' <- fromAnnFieldsG argsExistingJoins stringifyNumbers `traverse` annFieldsG
    pure (index, (fieldName, fieldSources'))
  _ -> Nothing

fromAggregateField :: Text -> IR.AggregateField 'MSSQL -> Projection
fromAggregateField alias aggregateField =
  case aggregateField of
    IR.AFExp text -> AggregateProjection $ Aliased (TextAggregate text) alias
    IR.AFCount countType -> AggregateProjection . flip Aliased alias . CountAggregate $ case countType of
      StarCountable -> StarCountable
      NonNullFieldCountable name -> NonNullFieldCountable $ columnFieldAggEntity name
      DistinctCountable name -> DistinctCountable $ columnFieldAggEntity name
    IR.AFOp IR.AggregateOp {_aoOp = op, _aoFields = fields} ->
      let projections :: [Projection] =
            fields <&> \(fieldName, columnField) ->
              case columnField of
                IR.CFCol column _columnType ->
                  let fname = columnFieldAggEntity column
                   in AggregateProjection $ Aliased (OpAggregate op [ColumnExpression fname]) (IR.getFieldNameTxt fieldName)
                IR.CFExp text ->
                  ExpressionProjection $ Aliased (ValueExpression (ODBC.TextValue text)) (IR.getFieldNameTxt fieldName)
       in ExpressionProjection $
            flip Aliased alias $
              JsonQueryExpression $
                SelectExpression $
                  emptySelect
                    { selectProjections = projections,
                      selectFor = JsonFor $ ForJson JsonSingleton NoRoot
                    }
  where
    columnFieldAggEntity col = columnNameToFieldName col $ EntityAlias aggSubselectName

-- | The main sources of fields, either constants, fields or via joins.
fromAnnFieldsG ::
  Map TableName EntityAlias ->
  StringifyNumbers ->
  (IR.FieldName, IR.AnnFieldG 'MSSQL Void Expression) ->
  ReaderT EntityAlias FromIr FieldSource
fromAnnFieldsG existingJoins stringifyNumbers (IR.FieldName name, field) =
  case field of
    IR.AFColumn annColumnField -> do
      expression <- fromAnnColumnField stringifyNumbers annColumnField
      pure
        ( ExpressionFieldSource
            Aliased {aliasedThing = expression, aliasedAlias = name}
        )
    IR.AFExpression text ->
      pure
        ( ExpressionFieldSource
            Aliased
              { aliasedThing = TSQL.ValueExpression (ODBC.TextValue text),
                aliasedAlias = name
              }
        )
    IR.AFObjectRelation objectRelationSelectG ->
      fmap
        ( \aliasedThing ->
            JoinFieldSource (Aliased {aliasedThing, aliasedAlias = name})
        )
        (fromObjectRelationSelectG existingJoins objectRelationSelectG)
    IR.AFArrayRelation arraySelectG ->
      fmap
        ( \aliasedThing ->
            JoinFieldSource (Aliased {aliasedThing, aliasedAlias = name})
        )
        (fromArraySelectG arraySelectG)

-- | Here is where we project a field as a column expression. If
-- number stringification is on, then we wrap it in a
-- 'ToStringExpression' so that it's casted when being projected.
fromAnnColumnField ::
  StringifyNumbers ->
  IR.AnnColumnField 'MSSQL Expression ->
  ReaderT EntityAlias FromIr Expression
fromAnnColumnField _stringifyNumbers annColumnField = do
  fieldName <- fromColumn column
  -- TODO: Handle stringifying large numbers
  {-(IR.isScalarColumnWhere isBigNum typ && stringifyNumbers == StringifyNumbers)-}

  -- for geometry and geography values, the automatic json encoding on sql
  -- server would fail. So we need to convert it to a format the json encoding
  -- handles. Ideally we want this representation to be GeoJSON but sql server
  -- doesn't have any functions to convert to GeoJSON format. So we return it in
  -- WKT format
  if typ == (IR.ColumnScalar GeometryType) || typ == (IR.ColumnScalar GeographyType)
    then pure $ MethodApplicationExpression (ColumnExpression fieldName) MethExpSTAsText
    else case caseBoolExpMaybe of
      Nothing -> pure (ColumnExpression fieldName)
      Just ex -> do
        ex' <- fromGBoolExp (coerce ex)
        let nullValue = ValueExpression ODBC.NullValue
        pure (ConditionalExpression ex' (ColumnExpression fieldName) nullValue)
  where
    IR.AnnColumnField
      { _acfColumn = column,
        _acfType = typ,
        _acfAsText = _asText :: Bool,
        _acfOp = _ :: Maybe (IR.ColumnOp 'MSSQL), -- TODO: What's this?
        _acfCaseBoolExpression = caseBoolExpMaybe
      } = annColumnField

-- | This is where a field name "foo" is resolved to a fully qualified
-- field name [table].[foo]. The table name comes from EntityAlias in
-- the ReaderT.
fromColumn :: ColumnName -> ReaderT EntityAlias FromIr FieldName
fromColumn column = columnNameToFieldName column <$> ask

--  entityAlias <- ask
--  pure (columnNameToFieldName column entityAlias -- FieldName {fieldName = columnName column, fieldNameEntity = entityAliasText}
--       )

fieldSourceProjections :: FieldSource -> Projection
fieldSourceProjections =
  \case
    ExpressionFieldSource aliasedExpression ->
      ExpressionProjection aliasedExpression
    JoinFieldSource aliasedJoin ->
      ExpressionProjection
        ( aliasedJoin
            { aliasedThing =
                -- Basically a cast, to ensure that SQL Server won't
                -- double-encode the JSON but will "pass it through"
                -- untouched.
                JsonQueryExpression
                  ( ColumnExpression
                      ( joinAliasToField
                          (joinJoinAlias (aliasedThing aliasedJoin))
                      )
                  )
            }
        )

joinAliasToField :: JoinAlias -> FieldName
joinAliasToField JoinAlias {..} =
  FieldName
    { fieldNameEntity = joinAliasEntity,
      fieldName = fromMaybe (error "TODO: Eliminate this case. joinAliasToField") joinAliasField
    }

fieldSourceJoin :: FieldSource -> Maybe Join
fieldSourceJoin =
  \case
    JoinFieldSource aliasedJoin -> pure (aliasedThing aliasedJoin)
    ExpressionFieldSource {} -> Nothing

--------------------------------------------------------------------------------
-- Joins

fromObjectRelationSelectG ::
  Map TableName EntityAlias ->
  IR.ObjectRelationSelectG 'MSSQL Void Expression ->
  ReaderT EntityAlias FromIr Join
fromObjectRelationSelectG existingJoins annRelationSelectG = do
  eitherAliasOrFrom <- lift (lookupTableFrom existingJoins tableFrom)
  let entityAlias :: EntityAlias = either id fromAlias eitherAliasOrFrom
  fieldSources <-
    local
      (const entityAlias)
      (traverse (fromAnnFieldsG mempty LeaveNumbersAlone) fields)
  let selectProjections = map fieldSourceProjections fieldSources
  joinJoinAlias <-
    do
      fieldName <- lift (fromRelName aarRelationshipName)
      alias <- lift (generateAlias (ObjectRelationTemplate fieldName))
      pure
        JoinAlias
          { joinAliasEntity = alias,
            joinAliasField = pure jsonFieldName
          }
  let selectFor =
        JsonFor ForJson {jsonCardinality = JsonSingleton, jsonRoot = NoRoot}
  filterExpression <- local (const entityAlias) (fromGBoolExp tableFilter)
  case eitherAliasOrFrom of
    Right selectFrom -> do
      foreignKeyConditions <- fromMapping selectFrom mapping
      pure
        Join
          { joinJoinAlias,
            joinSource =
              JoinSelect
                emptySelect
                  { selectOrderBy = Nothing,
                    selectTop = NoTop,
                    selectProjections,
                    selectFrom = Just selectFrom,
                    selectJoins = mapMaybe fieldSourceJoin fieldSources,
                    selectWhere =
                      Where (foreignKeyConditions <> [filterExpression]),
                    selectFor,
                    selectOffset = Nothing
                  }
          }
    Left _entityAlias ->
      pure
        Join
          { joinJoinAlias,
            joinSource =
              JoinReselect
                Reselect
                  { reselectProjections = selectProjections,
                    reselectFor = selectFor,
                    reselectWhere = Where [filterExpression]
                  }
          }
  where
    IR.AnnObjectSelectG
      { _aosFields = fields :: IR.AnnFieldsG 'MSSQL Void Expression,
        _aosTableFrom = tableFrom :: TableName,
        _aosTableFilter = tableFilter :: IR.AnnBoolExp 'MSSQL Expression
      } = annObjectSelectG
    IR.AnnRelationSelectG
      { aarRelationshipName,
        aarColumnMapping = mapping :: HashMap ColumnName ColumnName,
        aarAnnSelect = annObjectSelectG :: IR.AnnObjectSelectG 'MSSQL Void Expression
      } = annRelationSelectG

lookupTableFrom ::
  Map TableName EntityAlias ->
  TableName ->
  FromIr (Either EntityAlias From)
lookupTableFrom existingJoins tableFrom = do
  case M.lookup tableFrom existingJoins of
    Just entityAlias -> pure (Left entityAlias)
    Nothing -> fmap Right (fromQualifiedTable tableFrom)

fromArraySelectG :: IR.ArraySelectG 'MSSQL Void Expression -> ReaderT EntityAlias FromIr Join
fromArraySelectG =
  \case
    IR.ASSimple arrayRelationSelectG ->
      fromArrayRelationSelectG arrayRelationSelectG
    IR.ASAggregate arrayAggregateSelectG ->
      fromArrayAggregateSelectG arrayAggregateSelectG

fromArrayAggregateSelectG ::
  IR.AnnRelationSelectG 'MSSQL (IR.AnnAggregateSelectG 'MSSQL Void Expression) ->
  ReaderT EntityAlias FromIr Join
fromArrayAggregateSelectG annRelationSelectG = do
  fieldName <- lift (fromRelName aarRelationshipName)
  joinSelect <- do
    lhsEntityAlias <- ask
    -- With this, the foreign key relations are injected automatically
    -- at the right place by fromSelectAggregate.
    lift (fromSelectAggregate (pure (lhsEntityAlias, mapping)) annSelectG)
  alias <- lift (generateAlias (ArrayAggregateTemplate fieldName))
  pure
    Join
      { joinJoinAlias =
          JoinAlias
            { joinAliasEntity = alias,
              joinAliasField = pure jsonFieldName
            },
        joinSource = JoinSelect joinSelect
      }
  where
    IR.AnnRelationSelectG
      { aarRelationshipName,
        aarColumnMapping = mapping :: HashMap ColumnName ColumnName,
        aarAnnSelect = annSelectG
      } = annRelationSelectG

fromArrayRelationSelectG :: IR.ArrayRelationSelectG 'MSSQL Void Expression -> ReaderT EntityAlias FromIr Join
fromArrayRelationSelectG annRelationSelectG = do
  fieldName <- lift (fromRelName aarRelationshipName)
  sel <- lift (fromSelectRows annSelectG)
  joinSelect <-
    do
      foreignKeyConditions <- selectFromMapping sel mapping
      pure
        sel {selectWhere = Where foreignKeyConditions <> selectWhere sel}
  alias <- lift (generateAlias (ArrayRelationTemplate fieldName))
  pure
    Join
      { joinJoinAlias =
          JoinAlias
            { joinAliasEntity = alias,
              joinAliasField = pure jsonFieldName
            },
        joinSource = JoinSelect joinSelect
      }
  where
    IR.AnnRelationSelectG
      { aarRelationshipName,
        aarColumnMapping = mapping :: HashMap ColumnName ColumnName,
        aarAnnSelect = annSelectG
      } = annRelationSelectG

fromRelName :: IR.RelName -> FromIr Text
fromRelName relName =
  pure (IR.relNameToTxt relName)

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
          ( OpExpression
              TSQL.EQ'
              (ColumnExpression localFieldName)
              (ColumnExpression remoteFieldName)
          )
    )
    . HM.toList

selectFromMapping ::
  Select ->
  HashMap ColumnName ColumnName ->
  ReaderT EntityAlias FromIr [Expression]
selectFromMapping Select {selectFrom = Nothing} = const (pure [])
selectFromMapping Select {selectFrom = Just from} = fromMapping from

--------------------------------------------------------------------------------
-- Basic SQL expression types

fromOpExpG :: Expression -> IR.OpExpG 'MSSQL Expression -> FromIr Expression
fromOpExpG expression op =
  case op of
    IR.ANISNULL -> pure $ TSQL.IsNullExpression expression
    IR.ANISNOTNULL -> pure $ TSQL.IsNotNullExpression expression
    IR.AEQ False val -> pure $ nullableBoolEquality expression val
    IR.AEQ True val -> pure $ OpExpression TSQL.EQ' expression val
    IR.ANE False val -> pure $ nullableBoolInequality expression val
    IR.ANE True val -> pure $ OpExpression TSQL.NEQ' expression val
    IR.AGT val -> pure $ OpExpression TSQL.GT expression val
    IR.ALT val -> pure $ OpExpression TSQL.LT expression val
    IR.AGTE val -> pure $ OpExpression TSQL.GTE expression val
    IR.ALTE val -> pure $ OpExpression TSQL.LTE expression val
    IR.AIN val -> pure $ OpExpression TSQL.IN expression val
    IR.ANIN val -> pure $ OpExpression TSQL.NIN expression val
    IR.ALIKE val -> pure $ OpExpression TSQL.LIKE expression val
    IR.ANLIKE val -> pure $ OpExpression TSQL.NLIKE expression val
    IR.ABackendSpecific o -> case o of
      ASTContains val -> pure $ TSQL.STOpExpression TSQL.STContains expression val
      ASTCrosses val -> pure $ TSQL.STOpExpression TSQL.STCrosses expression val
      ASTEquals val -> pure $ TSQL.STOpExpression TSQL.STEquals expression val
      ASTIntersects val -> pure $ TSQL.STOpExpression TSQL.STIntersects expression val
      ASTOverlaps val -> pure $ TSQL.STOpExpression TSQL.STOverlaps expression val
      ASTTouches val -> pure $ TSQL.STOpExpression TSQL.STTouches expression val
      ASTWithin val -> pure $ TSQL.STOpExpression TSQL.STWithin expression val
    -- As of March 2021, only geometry/geography casts are supported
    IR.ACast _casts -> refute (pure (UnsupportedOpExpG op)) -- mkCastsExp casts

    -- We do not yet support column names in permissions
    IR.CEQ _rhsCol -> refute (pure (UnsupportedOpExpG op)) -- S.BECompare S.SEQ lhs $ mkQCol rhsCol
    IR.CNE _rhsCol -> refute (pure (UnsupportedOpExpG op)) -- S.BECompare S.SNE lhs $ mkQCol rhsCol
    IR.CGT _rhsCol -> refute (pure (UnsupportedOpExpG op)) -- S.BECompare S.SGT lhs $ mkQCol rhsCol
    IR.CLT _rhsCol -> refute (pure (UnsupportedOpExpG op)) -- S.BECompare S.SLT lhs $ mkQCol rhsCol
    IR.CGTE _rhsCol -> refute (pure (UnsupportedOpExpG op)) -- S.BECompare S.SGTE lhs $ mkQCol rhsCol
    IR.CLTE _rhsCol -> refute (pure (UnsupportedOpExpG op)) -- S.BECompare S.SLTE lhs $ mkQCol rhsCol

nullableBoolEquality :: Expression -> Expression -> Expression
nullableBoolEquality x y =
  OrExpression
    [ OpExpression TSQL.EQ' x y,
      AndExpression [IsNullExpression x, IsNullExpression y]
    ]

nullableBoolInequality :: Expression -> Expression -> Expression
nullableBoolInequality x y =
  OrExpression
    [ OpExpression TSQL.NEQ' x y,
      AndExpression [IsNotNullExpression x, IsNullExpression y]
    ]

-- | Translate a 'GBoolExp' of a 'AnnBoolExpFld', within an 'EntityAlias' context.
--
-- It is mutually recursive with 'fromAnnBoolExpFld' and 'fromGExists'.
fromGBoolExp ::
  IR.GBoolExp 'MSSQL (IR.AnnBoolExpFld 'MSSQL Expression) ->
  ReaderT EntityAlias FromIr Expression
fromGBoolExp =
  \case
    IR.BoolAnd expressions ->
      fmap AndExpression (traverse fromGBoolExp expressions)
    IR.BoolOr expressions ->
      fmap OrExpression (traverse fromGBoolExp expressions)
    IR.BoolNot expression ->
      fmap NotExpression (fromGBoolExp expression)
    IR.BoolExists gExists ->
      fromGExists gExists
    IR.BoolFld expression ->
      fromAnnBoolExpFld expression
  where
    fromGExists :: IR.GExists 'MSSQL (IR.AnnBoolExpFld 'MSSQL Expression) -> ReaderT EntityAlias FromIr Expression
    fromGExists IR.GExists {_geTable, _geWhere} = do
      selectFrom <- lift (fromQualifiedTable _geTable)
      whereExpression <-
        local (const (fromAlias selectFrom)) (fromGBoolExp _geWhere)
      pure $
        ExistsExpression $
          emptySelect
            { selectOrderBy = Nothing,
              selectProjections =
                [ ExpressionProjection
                    ( Aliased
                        { aliasedThing = trueExpression,
                          aliasedAlias = existsFieldName
                        }
                    )
                ],
              selectFrom = Just selectFrom,
              selectJoins = mempty,
              selectWhere = Where [whereExpression],
              selectTop = NoTop,
              selectFor = NoFor,
              selectOffset = Nothing
            }

--------------------------------------------------------------------------------
-- Insert

fromInsert :: IR.AnnInsert 'MSSQL Void Expression -> Insert
fromInsert IR.AnnInsert {..} =
  let IR.AnnIns {..} = _aiData
      insertRows = normalizeInsertRows $ map (IR.getInsertColumns) _aiInsObj
      insertColumnNames = maybe [] (map fst) $ listToMaybe insertRows
      insertValues = map (Values . map snd) insertRows
      allColumnNames = map (ColumnName . unName . IR.ciName) _aiTableCols
      insertOutput = Output Inserted $ map OutputColumn allColumnNames
      tempTable = TempTable tempTableNameInserted allColumnNames
   in Insert _aiTableName insertColumnNames insertOutput tempTable insertValues

-- | Normalize a row by adding missing columns with @DEFAULT@ value and sort by
-- column name to make sure all rows are consistent in column values and order.
--
-- Example: A table "author" is defined as:
--
-- > CREATE TABLE author ([id] INTEGER NOT NULL PRIMARY KEY, name TEXT NOT NULL, age INTEGER)
--
-- Consider the following mutation:
--
-- > mutation {
-- >   insert_author(
-- >     objects: [{id: 1, name: "Foo", age: 21}, {id: 2, name: "Bar"}]
-- >   ){
-- >     affected_rows
-- >   }
-- > }
--
-- We consider @DEFAULT@ value for @age@ column which is missing in second
-- insert row.
--
-- The corresponding @INSERT@ statement looks like:
--
-- > INSERT INTO author (id, name, age)
-- >   OUTPUT INSERTED.id
-- >   VALUES (1, 'Foo', 21), (2, 'Bar', DEFAULT)
normalizeInsertRows ::
  [[(Column 'MSSQL, Expression)]] ->
  [[(Column 'MSSQL, Expression)]]
normalizeInsertRows insertRows =
  let insertColumns = nubOrd (concatMap (map fst) insertRows)
      allColumnsWithDefaultValue = map (,DefaultExpression) $ insertColumns
      addMissingColumns insertRow =
        HM.toList $ HM.fromList insertRow `HM.union` HM.fromList allColumnsWithDefaultValue
      sortByColumn = sortBy (\l r -> compare (fst l) (fst r))
   in map (sortByColumn . addMissingColumns) insertRows

-- | Construct a MERGE statement from AnnInsert information.
--   A MERGE statement is responsible for actually inserting and/or updating
--   the data in the table.
toMerge ::
  TableName ->
  [IR.AnnotatedInsertRow 'MSSQL Expression] ->
  [IR.ColumnInfo 'MSSQL] ->
  IfMatched Expression ->
  FromIr Merge
toMerge tableName insertRows allColumns IfMatched {..} = do
  let normalizedInsertRows = normalizeInsertRows $ map (IR.getInsertColumns) insertRows
      insertColumnNames = maybe [] (map fst) $ listToMaybe normalizedInsertRows
      allColumnNames = map (ColumnName . unName . IR.ciName) allColumns

  matchConditions <-
    flip runReaderT (EntityAlias "target") $ -- the table is aliased as "target" in MERGE sql
      fromGBoolExp _imConditions

  pure $
    Merge
      { mergeTargetTable = tableName,
        mergeUsing = MergeUsing tempTableNameValues insertColumnNames,
        mergeOn = MergeOn _imMatchColumns,
        mergeWhenMatched = MergeWhenMatched _imUpdateColumns matchConditions _imColumnPresets,
        mergeWhenNotMatched = MergeWhenNotMatched insertColumnNames,
        mergeInsertOutput = Output Inserted $ map OutputColumn allColumnNames,
        mergeOutputTempTable = TempTable tempTableNameInserted allColumnNames
      }

-- | As part of an INSERT/UPSERT process, insert VALUES into a temporary table.
--   The content of the temporary table will later be inserted into the original table
--   using a MERGE statement.
--
--   We insert the values into a temporary table first in order to replace the missing
--   fields with @DEFAULT@ in @normalizeInsertRows@, and we can't do that in a
--   MERGE statement directly.
toInsertValuesIntoTempTable :: TempTableName -> IR.AnnInsert 'MSSQL Void Expression -> InsertValuesIntoTempTable
toInsertValuesIntoTempTable tempTable IR.AnnInsert {..} =
  let IR.AnnIns {..} = _aiData
      insertRows = normalizeInsertRows $ map IR.getInsertColumns _aiInsObj
      insertColumnNames = maybe [] (map fst) $ listToMaybe insertRows
      insertValues = map (Values . map snd) insertRows
   in InsertValuesIntoTempTable
        { ivittTempTableName = tempTable,
          ivittColumns = insertColumnNames,
          ivittValues = insertValues
        }

--------------------------------------------------------------------------------
-- Delete

-- | Convert IR AST representing delete into MSSQL AST representing a delete statement
fromDelete :: IR.AnnDel 'MSSQL -> FromIr Delete
fromDelete (IR.AnnDel tableName (permFilter, whereClause) _ allColumns) = do
  tableAlias <- fromTableName tableName
  runReaderT
    ( do
        permissionsFilter <- fromGBoolExp permFilter
        whereExpression <- fromGBoolExp whereClause
        let columnNames = map (ColumnName . unName . IR.ciName) allColumns
        pure
          Delete
            { deleteTable =
                Aliased
                  { aliasedAlias = entityAliasText tableAlias,
                    aliasedThing = tableName
                  },
              deleteOutput = Output Deleted (map OutputColumn columnNames),
              deleteTempTable = TempTable tempTableNameDeleted columnNames,
              deleteWhere = Where [permissionsFilter, whereExpression]
            }
    )
    tableAlias

-- | Convert IR AST representing update into MSSQL AST representing an update statement
fromUpdate :: IR.AnnotatedUpdate 'MSSQL -> FromIr Update
fromUpdate (IR.AnnotatedUpdateG tableName (permFilter, whereClause) _ backendUpdate _ allColumns) = do
  tableAlias <- fromTableName tableName
  runReaderT
    ( do
        permissionsFilter <- fromGBoolExp permFilter
        whereExpression <- fromGBoolExp whereClause
        let columnNames = map (ColumnName . unName . IR.ciName) allColumns
        pure
          Update
            { updateTable =
                Aliased
                  { aliasedAlias = entityAliasText tableAlias,
                    aliasedThing = tableName
                  },
              updateSet = updateOperations backendUpdate,
              updateOutput = Output Inserted (map OutputColumn columnNames),
              updateTempTable = TempTable tempTableNameUpdated columnNames,
              updateWhere = Where [permissionsFilter, whereExpression]
            }
    )
    tableAlias

-- | Create a temporary table with the same schema as the given table.
toSelectIntoTempTable :: TempTableName -> TableName -> [IR.ColumnInfo 'MSSQL] -> SITTConstraints -> SelectIntoTempTable
toSelectIntoTempTable tempTableName fromTable allColumns withConstraints = do
  SelectIntoTempTable
    { sittTempTableName = tempTableName,
      sittColumns = map columnInfoToUnifiedColumn allColumns,
      sittFromTableName = fromTable,
      sittConstraints = withConstraints
    }

-- | Extracts the type and column name of a ColumnInfo
columnInfoToUnifiedColumn :: IR.ColumnInfo 'MSSQL -> UnifiedColumn
columnInfoToUnifiedColumn colInfo =
  case IR.ciType colInfo of
    IR.ColumnScalar t ->
      UnifiedColumn
        { name = unName $ IR.ciName colInfo,
          type' = t
        }
    -- Enum values are represented as text value so they will always be of type text
    IR.ColumnEnumReference {} ->
      UnifiedColumn
        { name = unName $ IR.ciName colInfo,
          type' = TextType
        }

--------------------------------------------------------------------------------
-- Misc combinators

trueExpression :: Expression
trueExpression = ValueExpression (ODBC.BoolValue True)

--------------------------------------------------------------------------------
-- Constants

jsonFieldName :: Text
jsonFieldName = "json"

aggFieldName :: Text
aggFieldName = "agg"

aggSubselectName :: Text
aggSubselectName = "agg_sub"

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

-- | Generate an alias for a given entity to remove ambiguity and naming
-- conflicts between scopes at the TSQL level. Keeps track of the increments for
-- the alias index in the 'StateT'
generateAlias :: NameTemplate -> FromIr Text
generateAlias template = do
  FromIr (modify' (M.insertWith (+) prefix start))
  i <- FromIr get
  pure (prefix <> tshow (fromMaybe start (M.lookup prefix i)))
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

fromAlias :: From -> EntityAlias
fromAlias (FromQualifiedTable Aliased {aliasedAlias}) = EntityAlias aliasedAlias
fromAlias (FromOpenJson Aliased {aliasedAlias}) = EntityAlias aliasedAlias
fromAlias (FromSelect Aliased {aliasedAlias}) = EntityAlias aliasedAlias
fromAlias (FromIdentifier identifier) = EntityAlias identifier
fromAlias (FromTempTable Aliased {aliasedAlias}) = EntityAlias aliasedAlias

columnNameToFieldName :: ColumnName -> EntityAlias -> FieldName
columnNameToFieldName (ColumnName fieldName) EntityAlias {entityAliasText = fieldNameEntity} =
  FieldName {fieldName, fieldNameEntity}
