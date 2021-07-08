{-# LANGUAGE ViewPatterns #-}
-- | Translate from the DML to the TSql dialect.

module Hasura.Backends.MSSQL.FromIr
  ( fromSelectRows
  , mkSQLSelect
  , fromRootField
  , fromSelectAggregate
  , fromAnnBoolExp
  , Error(..)
  , runFromIr
  , FromIr
  , jsonFieldName
  , fromDelete
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict                   as HM
import qualified Data.Map.Strict                       as M
import qualified Data.Text                             as T
import qualified Database.ODBC.SQLServer               as ODBC

import           Control.Monad.Validate
import           Data.Map.Strict                       (Map)
import           Data.Proxy

import qualified Hasura.RQL.IR                         as IR
import qualified Hasura.RQL.Types.Column               as IR
import qualified Hasura.RQL.Types.Common               as IR
import qualified Hasura.RQL.Types.Relationship         as IR

import           Hasura.Backends.MSSQL.Instances.Types ()
import           Hasura.Backends.MSSQL.Types           as TSQL
import           Hasura.SQL.Backend


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
-- @fromPGCol@.
newtype FromIr a = FromIr
  { unFromIr :: StateT (Map Text Int) (Validate (NonEmpty Error)) a
  } deriving (Functor, Applicative, Monad, MonadValidate (NonEmpty Error))

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

mkSQLSelect
  :: IR.JsonAggSelect
  -> IR.AnnSelectG 'MSSQL (Const Void) (IR.AnnFieldG 'MSSQL (Const Void)) Expression
  -> FromIr TSQL.Select
mkSQLSelect jsonAggSelect annSimpleSel =
  case jsonAggSelect of
    IR.JASMultipleRows -> fromSelectRows annSimpleSel
    IR.JASSingleObject ->
      fromSelectRows annSimpleSel <&> \sel ->
        sel
          { selectFor =
              JsonFor
                ForJson {jsonCardinality = JsonSingleton, jsonRoot = NoRoot}
          , selectTop = Top 1
          }

-- | Convert from the IR database query into a select.
fromRootField :: IR.QueryDB 'MSSQL (Const Void) Expression -> FromIr Select
fromRootField =
  \case
    (IR.QDBSingleRow s)    -> mkSQLSelect IR.JASSingleObject s
    (IR.QDBMultipleRows s) -> mkSQLSelect IR.JASMultipleRows s
    (IR.QDBAggregation s)  -> fromSelectAggregate Nothing s
    (IR.QDBConnection _)   -> refute $ pure ConnectionsNotSupported

--------------------------------------------------------------------------------
-- Top-level exported functions

fromSelectRows :: IR.AnnSelectG 'MSSQL (Const Void) (IR.AnnFieldG 'MSSQL (Const Void)) Expression -> FromIr TSQL.Select
fromSelectRows annSelectG = do
  selectFrom <-
    case from of
      IR.FromTable qualifiedObject -> fromQualifiedTable qualifiedObject
      IR.FromFunction {}           -> refute $ pure FunctionNotSupported
  Args { argsOrderBy
       , argsWhere
       , argsJoins
       , argsTop
       , argsDistinct = Proxy
       , argsOffset
       , argsExistingJoins
       } <- runReaderT (fromSelectArgsG args) (fromAlias selectFrom)
  fieldSources <-
    runReaderT
      (traverse (fromAnnFieldsG argsExistingJoins stringifyNumbers) fields)
      (fromAlias selectFrom)
  filterExpression <-
    runReaderT (fromAnnBoolExp permFilter) (fromAlias selectFrom)
  let selectProjections =
        concatMap (toList . fieldSourceProjections) fieldSources
  pure
    Select
      { selectOrderBy = argsOrderBy
      , selectTop = permissionBasedTop <> argsTop
      , selectProjections
      , selectFrom = Just selectFrom
      , selectJoins = argsJoins <> mapMaybe fieldSourceJoin fieldSources
      , selectWhere = argsWhere <> Where [filterExpression]
      , selectFor =
          JsonFor ForJson {jsonCardinality = JsonArray, jsonRoot = NoRoot}
      , selectOffset = argsOffset
      }
  where
    IR.AnnSelectG { _asnFields = fields
                  , _asnFrom = from
                  , _asnPerm = perm
                  , _asnArgs = args
                  , _asnStrfyNum = num
                  } = annSelectG
    IR.TablePerm {_tpLimit = mPermLimit, _tpFilter = permFilter} = perm
    permissionBasedTop =
      maybe NoTop Top mPermLimit
    stringifyNumbers =
      if num
        then StringifyNumbers
        else LeaveNumbersAlone


mkNodesSelect :: Args -> Where -> Expression -> Top -> From -> [(Int, (IR.FieldName, [Projection]))] -> [(Int, [Projection])]
mkNodesSelect Args{..} foreignKeyConditions filterExpression permissionBasedTop selectFrom nodes =
  [ (index,
      [ ExpressionProjection $ Aliased
          { aliasedThing = SelectExpression $ Select
              { selectProjections = projections
              , selectTop = permissionBasedTop <> argsTop
              , selectFrom = pure selectFrom
              , selectJoins = argsJoins
              , selectWhere = argsWhere <> Where [filterExpression] <> foreignKeyConditions
              , selectFor =
                  JsonFor ForJson {jsonCardinality = JsonArray, jsonRoot = NoRoot}
              , selectOrderBy = argsOrderBy
              , selectOffset = argsOffset
              }
          , aliasedAlias = IR.getFieldNameTxt fieldName
          }
      ] -- singleton
    )
  | (index, (fieldName, projections)) <- nodes ]


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
mkAggregateSelect :: Args -> Where -> From -> [(Int, (IR.FieldName, [Projection]))] -> [(Int, [Projection])]
mkAggregateSelect Args {..} foreignKeyConditions selectFrom aggregates =
  [ ( index
    , [ ExpressionProjection $
        Aliased
          { aliasedThing =
              JsonQueryExpression $
              SelectExpression $
              Select
                { selectProjections = reproject aggSubselectName <$> projections
                , selectTop = NoTop
                , selectFrom = pure $
                    FromSelect
                      Aliased
                        { aliasedAlias = aggSubselectName
                        , aliasedThing =
                            Select
                              { selectProjections = pure StarProjection
                              , selectTop = argsTop
                              , selectFrom = pure selectFrom
                              , selectJoins = argsJoins
                              , selectWhere = argsWhere <> foreignKeyConditions
                              , selectFor = NoFor
                              , selectOrderBy = mempty
                              , selectOffset = argsOffset
                              }
                        }
                , selectJoins = mempty
                , selectWhere = mempty
                , selectFor =
                    JsonFor
                      ForJson
                        {jsonCardinality = JsonSingleton, jsonRoot = NoRoot}
                , selectOrderBy = mempty
                , selectOffset = Nothing
                }
          , aliasedAlias = IR.getFieldNameTxt fieldName
          }
      ] -- singleton
     )
  | (index, (fieldName, projections)) <- aggregates
  ]


-- | Re-project projections in the aggSubselectName scope
--
-- For example,
--
-- [ AggregateProjection
--     (Aliased {aliasedThing = CountAggregate StarCountable, aliasedAlias = "count"})
-- , AggregateProjection
--     (Aliased
--       { aliasedThing = OpAggregate "sum"
--           [ ColumnExpression
--               (FieldName
--                 { fieldName = "id"
--                 , fieldNameEntity = "t_person1" -- <<<<< This needs to be `aggSubselectName`
--                 })
--           ]
--       , aliasedAlias = "sum"
--       })
--
reproject :: Text -> Projection -> Projection
reproject label = \case
  AggregateProjection (Aliased {aliasedThing = OpAggregate aggName expressions, ..}) ->
    AggregateProjection (Aliased {aliasedThing = OpAggregate aggName (fixColumnEntity label <$> expressions), ..})
  AggregateProjection (Aliased {aliasedThing = CountAggregate countableFieldnames, ..}) ->
    AggregateProjection (Aliased {aliasedThing = CountAggregate $ fixEntity label <$> countableFieldnames, ..})
  x -> x
  where
    fixColumnEntity entity = \case
      ColumnExpression fName ->
        ColumnExpression $ fixEntity entity fName
      x -> x
    fixEntity entity FieldName{..} = FieldName {fieldNameEntity = entity, ..}


fromSelectAggregate
  :: Maybe (EntityAlias, HashMap ColumnName ColumnName)
  -> IR.AnnSelectG 'MSSQL (Const Void) (IR.TableAggregateFieldG 'MSSQL (Const Void)) Expression
  -> FromIr TSQL.Select
fromSelectAggregate
  mparentRelationship
  IR.AnnSelectG
    { _asnFields = (zip [0..] -> fields)
    , _asnFrom = from
    , _asnPerm = IR.TablePerm {_tpLimit = (maybe NoTop Top -> permissionBasedTop), _tpFilter = permFilter}
    , _asnArgs = args
    , _asnStrfyNum = (bool LeaveNumbersAlone StringifyNumbers -> stringifyNumbers)
    }
  = do
  selectFrom <- case from of
    IR.FromTable qualifiedObject -> fromQualifiedTable qualifiedObject
    IR.FromFunction {}           -> refute $ pure FunctionNotSupported
  -- Below: When we're actually a RHS of a query (of CROSS APPLY),
  -- then we'll have a LHS table that we're joining on. So we get the
  -- conditions expressions from the field mappings. The LHS table is
  -- the entityAlias, and the RHS table is selectFrom.
  mforeignKeyConditions <- fmap (Where . fromMaybe []) $ for mparentRelationship $
    \(entityAlias, mapping) ->
      runReaderT (fromMapping selectFrom mapping) entityAlias
  filterExpression <- runReaderT (fromAnnBoolExp permFilter) (fromAlias selectFrom)
  args'@Args{argsExistingJoins} <-
    runReaderT (fromSelectArgsG args) (fromAlias selectFrom)
  -- Although aggregates, exps and nodes could be handled in one list,
  -- we need to separately treat the subselect expressions
  expss :: [(Int, [Projection])] <- flip runReaderT (fromAlias selectFrom) $ sequence $ mapMaybe fromTableExpFieldG fields
  nodes :: [(Int, (IR.FieldName, [Projection]))] <-
    flip runReaderT (fromAlias selectFrom) $ sequence $ mapMaybe (fromTableNodesFieldG argsExistingJoins stringifyNumbers) fields
  aggregates :: [(Int, (IR.FieldName, [Projection]))] <-
    flip runReaderT (fromAlias selectFrom) $ sequence $ mapMaybe fromTableAggFieldG fields
  pure
    Select
      { selectProjections =
          concatMap snd $ sortBy (comparing fst) $
            expss
            <> mkNodesSelect     args' mforeignKeyConditions filterExpression permissionBasedTop selectFrom nodes
            <> mkAggregateSelect args' mforeignKeyConditions selectFrom aggregates
      , selectTop = NoTop
      , selectFrom = pure $ FromOpenJson $ Aliased
          { aliasedThing = OpenJson
              { openJsonExpression = ValueExpression $ ODBC.TextValue "[0]"
              , openJsonWith = Nothing
              }
          , aliasedAlias = existsFieldName
          }
      , selectJoins = mempty -- JOINs and WHEREs are only relevant in subselects
      , selectWhere = mempty
      , selectFor = JsonFor ForJson {jsonCardinality = JsonSingleton, jsonRoot = NoRoot}
      , selectOrderBy = Nothing
      , selectOffset = Nothing
      }


--------------------------------------------------------------------------------
-- GraphQL Args

data Args = Args
  { argsWhere         :: Where
  , argsOrderBy       :: Maybe (NonEmpty OrderBy)
  , argsJoins         :: [Join]
  , argsTop           :: Top
  , argsOffset        :: Maybe Expression
  , argsDistinct      :: Proxy (Maybe (NonEmpty FieldName))
  , argsExistingJoins :: Map TableName EntityAlias
  } deriving (Show)

data UnfurledJoin = UnfurledJoin
  { unfurledJoin             :: Join
  , unfurledObjectTableAlias :: Maybe (TableName, EntityAlias)
    -- ^ Recorded if we joined onto an object relation.
  } deriving (Show)

fromSelectArgsG :: IR.SelectArgsG 'MSSQL Expression -> ReaderT EntityAlias FromIr Args
fromSelectArgsG selectArgsG = do
  let argsOffset = ValueExpression . ODBC.IntValue . fromIntegral <$> moffset
  argsWhere <-
    maybe (pure mempty) (fmap (Where . pure) . fromAnnBoolExp) mannBoolExp
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
    runWriterT (traverse fromAnnOrderByItemG (maybe [] toList orders))
  -- Any object-relation joins that we generated, we record their
  -- generated names into a mapping.
  let argsExistingJoins =
        M.fromList (mapMaybe unfurledObjectTableAlias (toList joins))
  pure
    Args
      { argsJoins = toList (fmap unfurledJoin joins)
      , argsOrderBy = nonEmpty argsOrderBy
      , ..
      }
  where
    IR.SelectArgs { _saWhere = mannBoolExp
                  , _saLimit = mlimit
                  , _saOffset = moffset
                  , _saOrderBy = orders
                  } = selectArgsG

-- | Produce a valid ORDER BY construct, telling about any joins
-- needed on the side.
fromAnnOrderByItemG
  :: IR.AnnOrderByItemG 'MSSQL Expression
  -> WriterT (Seq UnfurledJoin) (ReaderT EntityAlias FromIr) OrderBy
fromAnnOrderByItemG IR.OrderByItemG {obiType, obiColumn = obiColumn, obiNulls} = do
  (orderByFieldName, orderByType) <- unfurlAnnOrderByElement obiColumn
  let orderByNullsOrder = fromMaybe NullsAnyOrder obiNulls
      orderByOrder      = fromMaybe AscOrder obiType
  pure OrderBy {..}

-- | Unfurl the nested set of object relations (tell'd in the writer)
-- that are terminated by field name (IR.AOCColumn and
-- IR.AOCArrayAggregation).
unfurlAnnOrderByElement
   :: IR.AnnOrderByElement 'MSSQL Expression
   -> WriterT (Seq UnfurledJoin) (ReaderT EntityAlias FromIr) (FieldName, Maybe TSQL.ScalarType)
unfurlAnnOrderByElement =
  \case
    IR.AOCColumn pgColumnInfo -> do
      fieldName <- lift (fromPGColumnInfo pgColumnInfo)
      pure
        ( fieldName
        , case (IR.pgiType pgColumnInfo) of
            IR.ColumnScalar t -> Just t
            -- Above: It is of interest to us whether the type is
            -- text/ntext/image. See ToQuery for more explanation.
            _                 -> Nothing)
    IR.AOCObjectRelation IR.RelInfo {riMapping = mapping, riRTable = table} annBoolExp annOrderByElementG -> do
      selectFrom <- lift (lift (fromQualifiedTable table))
      joinAliasEntity <-
        lift (lift (generateEntityAlias (ForOrderAlias (tableNameText table))))
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
        lift (local (const (fromAlias selectFrom)) (fromAnnBoolExp annBoolExp))
      tell
        (pure
           UnfurledJoin
             { unfurledJoin =
                 Join
                   { joinSource =
                       JoinSelect
                         Select
                           { selectTop = NoTop
                           , selectProjections = [StarProjection]
                           , selectFrom = Just selectFrom
                           , selectJoins = []
                           , selectWhere =
                               Where (foreignKeyConditions <> [whereExpression])
                           , selectFor = NoFor
                           , selectOrderBy = Nothing
                           , selectOffset = Nothing
                           }
                   , joinJoinAlias =
                       JoinAlias {joinAliasEntity, joinAliasField = Nothing}
                   }
             , unfurledObjectTableAlias = Just (table, EntityAlias joinAliasEntity)
             })
      local
        (const (EntityAlias joinAliasEntity))
        (unfurlAnnOrderByElement annOrderByElementG)
    IR.AOCArrayAggregation IR.RelInfo {riMapping = mapping, riRTable = tableName} annBoolExp annAggregateOrderBy -> do
      selectFrom <- lift (lift (fromQualifiedTable tableName))
      let alias = aggFieldName
      joinAliasEntity <-
        lift (lift (generateEntityAlias (ForOrderAlias (tableNameText tableName))))
      foreignKeyConditions <- lift (fromMapping selectFrom mapping)
      whereExpression <-
        lift (local (const (fromAlias selectFrom)) (fromAnnBoolExp annBoolExp))
      aggregate <-
        lift
          (local
             (const (fromAlias selectFrom))
             (case annAggregateOrderBy of
                IR.AAOCount -> pure (CountAggregate StarCountable)
                IR.AAOOp text pgColumnInfo -> do
                  fieldName <- fromPGColumnInfo pgColumnInfo
                  pure (OpAggregate text (pure (ColumnExpression fieldName)))))
      tell
        (pure
           (UnfurledJoin
              { unfurledJoin =
                  Join
                    { joinSource =
                        JoinSelect
                          Select
                            { selectTop = NoTop
                            , selectProjections =
                                [ AggregateProjection
                                    Aliased
                                    { aliasedThing = aggregate
                                    , aliasedAlias = alias
                                    }
                                ]
                            , selectFrom = Just selectFrom
                            , selectJoins = []
                            , selectWhere =
                                Where
                                  (foreignKeyConditions <> [whereExpression])
                            , selectFor = NoFor
                            , selectOrderBy = Nothing
                            , selectOffset = Nothing
                            }
                    , joinJoinAlias =
                        JoinAlias {joinAliasEntity, joinAliasField = Nothing}
                    }
              , unfurledObjectTableAlias = Nothing
              }))
      pure
        ( FieldName {fieldNameEntity = joinAliasEntity, fieldName = alias}
        , Nothing)


--------------------------------------------------------------------------------
-- Conversion functions

tableNameText :: TableName -> Text
tableNameText (TableName {tableName}) = tableName

-- | This is really the start where you query the base table,
-- everything else is joins attached to it.
fromQualifiedTable :: TableName -> FromIr From
fromQualifiedTable schemadTableName@(TableName{tableName}) = do
  alias <- generateEntityAlias (TableTemplate tableName)
  pure
    (FromQualifiedTable
       (Aliased
          { aliasedThing = schemadTableName
          , aliasedAlias = alias
          }))

fromTableName :: TableName -> FromIr EntityAlias
fromTableName TableName{tableName} = do
  alias <- generateEntityAlias (TableTemplate tableName)
  pure (EntityAlias alias)

fromAnnBoolExp
  :: IR.GBoolExp 'MSSQL (IR.AnnBoolExpFld 'MSSQL Expression)
  -> ReaderT EntityAlias FromIr Expression
fromAnnBoolExp = traverse fromAnnBoolExpFld >=> fromGBoolExp

fromAnnBoolExpFld
  :: IR.AnnBoolExpFld 'MSSQL Expression
  -> ReaderT EntityAlias FromIr Expression
fromAnnBoolExpFld =
  \case
    IR.AVColumn pgColumnInfo opExpGs -> do
      expression <- fromColumnInfoForBoolExp pgColumnInfo
      expressions <- traverse (lift . fromOpExpG expression) opExpGs
      pure (AndExpression expressions)
    IR.AVRelationship IR.RelInfo {riMapping = mapping, riRTable = table} annBoolExp -> do
      selectFrom <- lift (fromQualifiedTable table)
      foreignKeyConditions <- fromMapping selectFrom mapping
      whereExpression <-
        local (const (fromAlias selectFrom)) (fromAnnBoolExp annBoolExp)
      pure
        (ExistsExpression
           Select
             { selectOrderBy = Nothing
             , selectProjections =
                 [ ExpressionProjection
                     (Aliased
                        { aliasedThing = trueExpression
                        , aliasedAlias = existsFieldName
                        })
                 ]
             , selectFrom = Just selectFrom
             , selectJoins = mempty
             , selectWhere = Where (foreignKeyConditions <> [whereExpression])
             , selectTop = NoTop
             , selectFor = NoFor
             , selectOffset = Nothing
             })

-- | For boolean operators, various comparison operators used need
-- special handling to ensure that SQL Server won't outright reject
-- the comparison. See also 'shouldCastToVarcharMax'.
fromColumnInfoForBoolExp :: IR.ColumnInfo 'MSSQL -> ReaderT EntityAlias FromIr Expression
fromColumnInfoForBoolExp IR.ColumnInfo {pgiColumn = pgCol, pgiType} = do
  fieldName <- columnNameToFieldName pgCol <$> ask
  if shouldCastToVarcharMax pgiType -- See function commentary.
     then pure (CastExpression (ColumnExpression fieldName) "VARCHAR(MAX)")
     else pure (ColumnExpression fieldName)

-- | There's a problem of comparing text fields with =, <, etc. that
-- SQL Server completely refuses to do so. So one way to workaround
-- this restriction is to automatically cast such text fields to
-- varchar(max).
shouldCastToVarcharMax :: IR.ColumnType 'MSSQL -> Bool
shouldCastToVarcharMax typ =
  typ == IR.ColumnScalar TextType || typ == IR.ColumnScalar WtextType

fromPGColumnInfo :: IR.ColumnInfo 'MSSQL -> ReaderT EntityAlias FromIr FieldName
fromPGColumnInfo IR.ColumnInfo {pgiColumn = pgCol} =
  columnNameToFieldName pgCol <$> ask
--  entityAlias <- ask
--  pure
--    (columnNameToFieldName pgCol entityAlias
--     FieldName
--       {fieldName = PG.getPGColTxt pgCol, fieldNameEntity = entityAliasText})

fromGExists :: IR.GExists 'MSSQL Expression -> ReaderT EntityAlias FromIr Select
fromGExists IR.GExists {_geTable, _geWhere} = do
  selectFrom <- lift (fromQualifiedTable _geTable)
  whereExpression <-
    local (const (fromAlias selectFrom)) (fromGBoolExp _geWhere)
  pure
    Select
      { selectOrderBy = Nothing
      , selectProjections =
          [ ExpressionProjection
              (Aliased
                 { aliasedThing = trueExpression
                 , aliasedAlias = existsFieldName
                 })
          ]
      , selectFrom = Just selectFrom
      , selectJoins = mempty
      , selectWhere = Where [whereExpression]
      , selectTop = NoTop
      , selectFor = NoFor
      , selectOffset = Nothing
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
  | AggregateFieldSource [Aliased Aggregate]
  deriving (Eq, Show)

-- | Get FieldSource from a TAFExp type table aggregate field
fromTableExpFieldG :: -- TODO: Convert function to be similar to Nodes function
  (Int, (IR.FieldName, IR.TableAggregateFieldG 'MSSQL (Const Void) Expression)) ->
  Maybe (ReaderT EntityAlias FromIr (Int, [Projection]))
fromTableExpFieldG = \case
  (index, (IR.FieldName name, IR.TAFExp text)) -> Just $
    pure $
      (index, fieldSourceProjections $
        ExpressionFieldSource
          Aliased
            { aliasedThing = TSQL.ValueExpression (ODBC.TextValue text)
            , aliasedAlias = name
            })
  _ -> Nothing

fromTableAggFieldG ::
  (Int, (IR.FieldName, IR.TableAggregateFieldG 'MSSQL (Const Void) Expression)) ->
  Maybe (ReaderT EntityAlias FromIr (Int, (IR.FieldName, [Projection])))
fromTableAggFieldG = \case
  (index, (fieldName, IR.TAFAgg (aggregateFields :: [(IR.FieldName, IR.AggregateField 'MSSQL)]))) -> Just do
    aggregates <-
      for aggregateFields \(fieldName', aggregateField) ->
        fromAggregateField aggregateField <&> \aliasedThing ->
          Aliased {aliasedAlias = IR.getFieldNameTxt fieldName', ..}
    pure (index, (fieldName, fieldSourceProjections $ AggregateFieldSource aggregates))
  _ -> Nothing


fromTableNodesFieldG ::
  Map TableName EntityAlias ->
  StringifyNumbers ->
  (Int, (IR.FieldName, IR.TableAggregateFieldG 'MSSQL (Const Void) Expression)) ->
  Maybe (ReaderT EntityAlias FromIr (Int, (IR.FieldName, [Projection])))
fromTableNodesFieldG argsExistingJoins stringifyNumbers = \case
  (index, (fieldName, IR.TAFNodes () (annFieldsG :: [(IR.FieldName, IR.AnnFieldG 'MSSQL (Const Void) Expression)]))) -> Just do
    fieldSources' <- fromAnnFieldsG argsExistingJoins stringifyNumbers `traverse` annFieldsG
    let nodesProjections' :: [Projection] = concatMap fieldSourceProjections fieldSources'
    pure (index, (fieldName, nodesProjections'))
  _ -> Nothing


fromAggregateField :: IR.AggregateField 'MSSQL -> ReaderT EntityAlias FromIr Aggregate
fromAggregateField aggregateField =
  case aggregateField of
    IR.AFExp text        -> pure (TextAggregate text)
    IR.AFCount countType -> CountAggregate <$> case countType of
      StarCountable               -> pure StarCountable
      NonNullFieldCountable names -> NonNullFieldCountable <$> traverse fromPGCol names
      DistinctCountable     names -> DistinctCountable     <$> traverse fromPGCol names

--      fmap
--        CountAggregate
--        (pure countType
--         case countType of
--           PG.CTStar -> pure StarCountable
--           PG.CTSimple fields ->
--             case nonEmpty fields of
--               Nothing -> refute (pure MalformedAgg)
--               Just fields' -> do
--                 fields'' <- traverse fromPGCol fields'
--                 pure (NonNullFieldCountable fields'')
--           PG.CTDistinct fields ->
--             case nonEmpty fields of
--               Nothing -> refute (pure MalformedAgg)
--               Just fields' -> do
--                 fields'' <- traverse fromPGCol fields'
--                 pure (DistinctCountable fields''))
    IR.AFOp IR.AggregateOp {_aoOp = op, _aoFields = fields} -> do
      args <- for fields \(_fieldName, pgColFld) ->
        case pgColFld of
          IR.CFCol pgCol _pgType -> fmap ColumnExpression (fromPGCol pgCol)
          IR.CFExp text          -> pure (ValueExpression (ODBC.TextValue text))
      pure (OpAggregate op args)

-- | The main sources of fields, either constants, fields or via joins.
fromAnnFieldsG ::
     Map TableName EntityAlias
  -> StringifyNumbers
  -> (IR.FieldName, IR.AnnFieldG 'MSSQL (Const Void) Expression)
  -> ReaderT EntityAlias FromIr FieldSource
fromAnnFieldsG existingJoins stringifyNumbers (IR.FieldName name, field) =
  case field of
    IR.AFColumn annColumnField -> do
      expression <- fromAnnColumnField stringifyNumbers annColumnField
      pure
        (ExpressionFieldSource
           Aliased {aliasedThing = expression, aliasedAlias = name})
    IR.AFExpression text ->
      pure
        (ExpressionFieldSource
           Aliased
             { aliasedThing = TSQL.ValueExpression (ODBC.TextValue text)
             , aliasedAlias = name
             })
    IR.AFObjectRelation objectRelationSelectG ->
      fmap
        (\aliasedThing ->
           JoinFieldSource (Aliased {aliasedThing, aliasedAlias = name}))
        (fromObjectRelationSelectG existingJoins objectRelationSelectG)
    IR.AFArrayRelation arraySelectG ->
      fmap
        (\aliasedThing ->
           JoinFieldSource (Aliased {aliasedThing, aliasedAlias = name}))
        (fromArraySelectG arraySelectG)
    -- this will be gone once the code which collects remote joins from the IR
    -- emits a modified IR where remote relationships can't be reached
    IR.AFRemote _ ->
      pure
        (ExpressionFieldSource
           Aliased
             { aliasedThing = TSQL.ValueExpression
                              (ODBC.TextValue "null: remote field selected")
             , aliasedAlias = name
             })
    -- TODO: implement this
    IR.AFDBRemote _ -> error "FIXME"

-- | Here is where we project a field as a column expression. If
-- number stringification is on, then we wrap it in a
-- 'ToStringExpression' so that it's casted when being projected.
fromAnnColumnField
  :: StringifyNumbers
  -> IR.AnnColumnField 'MSSQL Expression
  -> ReaderT EntityAlias FromIr Expression
fromAnnColumnField _stringifyNumbers annColumnField = do
  fieldName <- fromPGCol pgCol
  -- TODO: Handle stringifying large numbers
  {-(IR.isScalarColumnWhere PG.isBigNum typ && stringifyNumbers == StringifyNumbers)-}

  -- for geometry and geography values, the automatic json encoding on sql
  -- server would fail. So we need to convert it to a format the json encoding
  -- handles. Ideally we want this representation to be GeoJSON but sql server
  -- doesn't have any functions to convert to GeoJSON format. So we return it in
  -- WKT format
  if typ == (IR.ColumnScalar GeometryType) || typ == (IR.ColumnScalar GeographyType)
     then pure $ MethodExpression (ColumnExpression fieldName) "STAsText" []
     else case caseBoolExpMaybe of
            Nothing -> pure (ColumnExpression fieldName)
            Just ex -> do
                ex' <- (traverse fromAnnBoolExpFld >=> fromGBoolExp) (coerce ex)
                pure (ConditionalProjection ex' fieldName)
  where
    IR.AnnColumnField { _acfInfo = IR.ColumnInfo{pgiColumn=pgCol,pgiType=typ}
                      , _acfAsText = _asText :: Bool
                      , _acfOp = _ :: Maybe (IR.ColumnOp 'MSSQL) -- TODO: What's this?
                      , _acfCaseBoolExpression = caseBoolExpMaybe
                      } = annColumnField

-- | This is where a field name "foo" is resolved to a fully qualified
-- field name [table].[foo]. The table name comes from EntityAlias in
-- the ReaderT.
fromPGCol :: ColumnName -> ReaderT EntityAlias FromIr FieldName
fromPGCol pgCol = columnNameToFieldName pgCol <$> ask
--  entityAlias <- ask
--  pure (columnNameToFieldName pgCol entityAlias -- FieldName {fieldName = PG.getPGColTxt pgCol, fieldNameEntity = entityAliasText}
--       )

fieldSourceProjections :: FieldSource -> [Projection]
fieldSourceProjections =
  \case
    ExpressionFieldSource aliasedExpression ->
      pure (ExpressionProjection aliasedExpression)
    JoinFieldSource aliasedJoin ->
      pure
        (ExpressionProjection
           (aliasedJoin
              { aliasedThing =
                  -- Basically a cast, to ensure that SQL Server won't
                  -- double-encode the JSON but will "pass it through"
                  -- untouched.
                  JsonQueryExpression
                    (ColumnExpression
                       (joinAliasToField
                          (joinJoinAlias (aliasedThing aliasedJoin))))
              }))
    AggregateFieldSource aggregates -> fmap AggregateProjection aggregates

joinAliasToField :: JoinAlias -> FieldName
joinAliasToField JoinAlias {..} =
  FieldName
    { fieldNameEntity = joinAliasEntity
    , fieldName = fromMaybe (error "TODO: Eliminate this case. joinAliasToField") joinAliasField
    }

fieldSourceJoin :: FieldSource -> Maybe Join
fieldSourceJoin =
  \case
    JoinFieldSource aliasedJoin -> pure (aliasedThing aliasedJoin)
    ExpressionFieldSource {}    -> Nothing
    AggregateFieldSource {}     -> Nothing


--------------------------------------------------------------------------------
-- Joins

fromObjectRelationSelectG ::
     Map TableName {-PG.QualifiedTable-} EntityAlias
  -> IR.ObjectRelationSelectG 'MSSQL (Const Void) Expression
  -> ReaderT EntityAlias FromIr Join
fromObjectRelationSelectG existingJoins annRelationSelectG = do
  eitherAliasOrFrom <- lift (lookupTableFrom existingJoins tableFrom)
  let entityAlias :: EntityAlias = either id fromAlias eitherAliasOrFrom
  fieldSources <-
    local
      (const entityAlias)
      (traverse (fromAnnFieldsG mempty LeaveNumbersAlone) fields)
  let selectProjections =
        concatMap (toList . fieldSourceProjections) fieldSources
  joinJoinAlias <-
    do fieldName <- lift (fromRelName aarRelationshipName)
       alias <- lift (generateEntityAlias (ObjectRelationTemplate fieldName))
       pure
         JoinAlias
           {joinAliasEntity = alias, joinAliasField = pure jsonFieldName}
  let selectFor =
        JsonFor ForJson {jsonCardinality = JsonSingleton, jsonRoot = NoRoot}
  filterExpression <- local (const entityAlias) (fromAnnBoolExp tableFilter)
  case eitherAliasOrFrom of
    Right selectFrom -> do
      foreignKeyConditions <- fromMapping selectFrom mapping
      pure
        Join
          { joinJoinAlias
          , joinSource =
              JoinSelect
                Select
                  { selectOrderBy = Nothing
                  , selectTop = NoTop
                  , selectProjections
                  , selectFrom = Just selectFrom
                  , selectJoins = mapMaybe fieldSourceJoin fieldSources
                  , selectWhere =
                      Where (foreignKeyConditions <> [filterExpression])
                  , selectFor
                  , selectOffset = Nothing
                  }
          }
    Left _entityAlias ->
      pure
        Join
          { joinJoinAlias
          , joinSource =
              JoinReselect
                Reselect
                  { reselectProjections = selectProjections
                  , reselectFor = selectFor
                  , reselectWhere = Where [filterExpression]
                  }
          }
  where
    IR.AnnObjectSelectG { _aosFields = fields :: IR.AnnFieldsG 'MSSQL (Const Void) Expression
                        , _aosTableFrom = tableFrom :: TableName{-PG.QualifiedTable-}
                        , _aosTableFilter = tableFilter :: IR.AnnBoolExp 'MSSQL Expression
                        } = annObjectSelectG
    IR.AnnRelationSelectG { aarRelationshipName
                          , aarColumnMapping = mapping :: HashMap ColumnName ColumnName -- PG.PGCol PG.PGCol
                          , aarAnnSelect = annObjectSelectG :: IR.AnnObjectSelectG 'MSSQL (Const Void) Expression
                          } = annRelationSelectG

lookupTableFrom ::
     Map TableName {-PG.QualifiedTable-} EntityAlias
  -> {-PG.QualifiedTable-}TableName
  -> FromIr (Either EntityAlias From)
lookupTableFrom existingJoins tableFrom = do
  case M.lookup tableFrom existingJoins of
    Just entityAlias -> pure (Left entityAlias)
    Nothing          -> fmap Right (fromQualifiedTable tableFrom)

fromArraySelectG :: IR.ArraySelectG 'MSSQL (Const Void) Expression -> ReaderT EntityAlias FromIr Join
fromArraySelectG =
  \case
    IR.ASSimple arrayRelationSelectG ->
      fromArrayRelationSelectG arrayRelationSelectG
    IR.ASAggregate arrayAggregateSelectG ->
      fromArrayAggregateSelectG arrayAggregateSelectG

fromArrayAggregateSelectG
  :: IR.AnnRelationSelectG 'MSSQL (IR.AnnAggregateSelectG 'MSSQL (Const Void) Expression)
  -> ReaderT EntityAlias FromIr Join
fromArrayAggregateSelectG annRelationSelectG = do
  fieldName <- lift (fromRelName aarRelationshipName)
  joinSelect <- do
    lhsEntityAlias <- ask
    -- With this, the foreign key relations are injected automatically
    -- at the right place by fromSelectAggregate.
    lift (fromSelectAggregate (pure (lhsEntityAlias, mapping)) annSelectG)
  alias <- lift (generateEntityAlias (ArrayAggregateTemplate fieldName))
  pure
    Join
      { joinJoinAlias =
          JoinAlias
            {joinAliasEntity = alias, joinAliasField = pure jsonFieldName}
      , joinSource = JoinSelect joinSelect
      }
  where
    IR.AnnRelationSelectG { aarRelationshipName
                          , aarColumnMapping = mapping :: HashMap ColumnName ColumnName
                          , aarAnnSelect = annSelectG
                          } = annRelationSelectG

fromArrayRelationSelectG :: IR.ArrayRelationSelectG 'MSSQL (Const Void) Expression -> ReaderT EntityAlias FromIr Join
fromArrayRelationSelectG annRelationSelectG = do
  fieldName <- lift (fromRelName aarRelationshipName)
  sel <- lift (fromSelectRows annSelectG)
  joinSelect <-
    do foreignKeyConditions <- selectFromMapping sel mapping
       pure
         sel {selectWhere = Where foreignKeyConditions <> selectWhere sel}
  alias <- lift (generateEntityAlias (ArrayRelationTemplate fieldName))
  pure
    Join
      { joinJoinAlias =
          JoinAlias
            {joinAliasEntity = alias, joinAliasField = pure jsonFieldName}
      , joinSource = JoinSelect joinSelect
      }
  where
    IR.AnnRelationSelectG { aarRelationshipName
                          , aarColumnMapping = mapping :: HashMap ColumnName ColumnName-- PG.PGCol PG.PGCol
                          , aarAnnSelect = annSelectG
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
-- The left/right columns in @HashMap PG.PGCol PG.PGCol@ corresponds
-- to the left/right of @select ... join ...@. Therefore left=remote,
-- right=local in this context.
fromMapping ::
     From
  -> HashMap ColumnName ColumnName-- PG.PGCol PG.PGCol
  -> ReaderT EntityAlias FromIr [Expression]
fromMapping localFrom =
  traverse
    (\(remotePgCol, localPgCol) -> do
       localFieldName <- local (const (fromAlias localFrom)) (fromPGCol localPgCol)
       remoteFieldName <- fromPGCol remotePgCol
       pure
         (OpExpression TSQL.EQ'
            (ColumnExpression localFieldName)
            (ColumnExpression remoteFieldName))) .
  HM.toList

selectFromMapping :: Select
  -> HashMap ColumnName ColumnName
  -> ReaderT EntityAlias FromIr [Expression]
selectFromMapping Select {selectFrom = Nothing  } = const (pure [])
selectFromMapping Select {selectFrom = Just from} = fromMapping from

--------------------------------------------------------------------------------
-- Basic SQL expression types

fromOpExpG :: Expression -> IR.OpExpG 'MSSQL Expression -> FromIr Expression
fromOpExpG expression op =
  case op of
    IR.ANISNULL      -> pure $ TSQL.IsNullExpression    expression
    IR.ANISNOTNULL   -> pure $ TSQL.IsNotNullExpression expression
    IR.AEQ False val -> pure $ nullableBoolEquality    expression val
    IR.AEQ True  val -> pure $ OpExpression TSQL.EQ'   expression val
    IR.ANE False val -> pure $ nullableBoolInequality  expression val
    IR.ANE True  val -> pure $ OpExpression TSQL.NEQ'  expression val
    IR.AGT       val -> pure $ OpExpression TSQL.GT    expression val
    IR.ALT       val -> pure $ OpExpression TSQL.LT    expression val
    IR.AGTE      val -> pure $ OpExpression TSQL.GTE   expression val
    IR.ALTE      val -> pure $ OpExpression TSQL.LTE   expression val
    IR.AIN       val -> pure $ OpExpression TSQL.IN    expression val
    IR.ANIN      val -> pure $ OpExpression TSQL.NIN   expression val
    IR.ALIKE     val -> pure $ OpExpression TSQL.LIKE  expression val
    IR.ANLIKE    val -> pure $ OpExpression TSQL.NLIKE expression val

    IR.ABackendSpecific o -> case o of
      ASTContains   val -> pure $ TSQL.STOpExpression TSQL.STContains   expression val
      ASTCrosses    val -> pure $ TSQL.STOpExpression TSQL.STCrosses    expression val
      ASTEquals     val -> pure $ TSQL.STOpExpression TSQL.STEquals     expression val
      ASTIntersects val -> pure $ TSQL.STOpExpression TSQL.STIntersects expression val
      ASTOverlaps   val -> pure $ TSQL.STOpExpression TSQL.STOverlaps   expression val
      ASTTouches    val -> pure $ TSQL.STOpExpression TSQL.STTouches    expression val
      ASTWithin     val -> pure $ TSQL.STOpExpression TSQL.STWithin     expression val

    -- As of March 2021, only geometry/geography casts are supported
    IR.ACast _casts  -> refute (pure (UnsupportedOpExpG op)) -- mkCastsExp casts

    -- We do not yet support column names in permissions
    IR.CEQ _rhsCol   -> refute (pure (UnsupportedOpExpG op)) -- S.BECompare S.SEQ lhs $ mkQCol rhsCol
    IR.CNE _rhsCol   -> refute (pure (UnsupportedOpExpG op)) -- S.BECompare S.SNE lhs $ mkQCol rhsCol
    IR.CGT _rhsCol   -> refute (pure (UnsupportedOpExpG op)) -- S.BECompare S.SGT lhs $ mkQCol rhsCol
    IR.CLT _rhsCol   -> refute (pure (UnsupportedOpExpG op)) -- S.BECompare S.SLT lhs $ mkQCol rhsCol
    IR.CGTE _rhsCol  -> refute (pure (UnsupportedOpExpG op)) -- S.BECompare S.SGTE lhs $ mkQCol rhsCol
    IR.CLTE _rhsCol  -> refute (pure (UnsupportedOpExpG op)) -- S.BECompare S.SLTE lhs $ mkQCol rhsCol

nullableBoolEquality :: Expression -> Expression -> Expression
nullableBoolEquality x y =
  OrExpression
    [ OpExpression TSQL.EQ' x y
    , AndExpression [IsNullExpression x, IsNullExpression y]
    ]

nullableBoolInequality :: Expression -> Expression -> Expression
nullableBoolInequality x y =
  OrExpression
    [ OpExpression TSQL.NEQ' x y
    , AndExpression [IsNotNullExpression x, IsNullExpression y]
    ]

fromGBoolExp :: IR.GBoolExp 'MSSQL Expression -> ReaderT EntityAlias FromIr Expression
fromGBoolExp =
  \case
    IR.BoolAnd expressions ->
      fmap AndExpression (traverse fromGBoolExp expressions)
    IR.BoolOr expressions ->
      fmap OrExpression (traverse fromGBoolExp expressions)
    IR.BoolNot expression -> fmap NotExpression (fromGBoolExp expression)
    IR.BoolExists gExists -> fmap ExistsExpression (fromGExists gExists)
    IR.BoolFld expression -> pure expression


--------------------------------------------------------------------------------
-- Delete

fromDelete :: IR.AnnDel 'MSSQL -> FromIr Delete
fromDelete (IR.AnnDel tableName (permFilter, whereClause) _ _) = do
  tableAlias <- fromTableName tableName
  runReaderT
    (do permissionsFilter <- fromAnnBoolExp permFilter
        whereExpression <- fromAnnBoolExp whereClause
        pure
          Delete
            { deleteTable =
                Aliased
                  { aliasedAlias = entityAliasText tableAlias
                  , aliasedThing = tableName
                  }
            , deleteWhere = Where [permissionsFilter, whereExpression]
            })
    tableAlias


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

generateEntityAlias :: NameTemplate -> FromIr Text
generateEntityAlias template = do
  FromIr (modify' (M.insertWith (+) prefix start))
  i <- FromIr get
  pure (prefix <> tshow (fromMaybe start (M.lookup prefix i)))
  where
    start = 1
    prefix = T.take 20 rendered
    rendered =
      case template of
        ArrayRelationTemplate sample  -> "ar_" <> sample
        ArrayAggregateTemplate sample -> "aa_" <> sample
        ObjectRelationTemplate sample -> "or_" <> sample
        TableTemplate sample          -> "t_" <> sample
        ForOrderAlias sample          -> "order_" <> sample

fromAlias :: From -> EntityAlias
fromAlias (FromQualifiedTable Aliased {aliasedAlias}) = EntityAlias aliasedAlias
fromAlias (FromOpenJson Aliased {aliasedAlias})       = EntityAlias aliasedAlias
fromAlias (FromSelect Aliased {aliasedAlias})         = EntityAlias aliasedAlias

columnNameToFieldName :: ColumnName -> EntityAlias -> FieldName
columnNameToFieldName (ColumnName fieldName) EntityAlias {entityAliasText = fieldNameEntity} =
  FieldName {fieldName, fieldNameEntity}
