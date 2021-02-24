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

--import           Control.Monad.Trans.State.Strict   as S
import           Control.Monad.Validate
import           Data.Map.Strict                       (Map)
import           Data.Proxy

import qualified Hasura.GraphQL.Context                as GraphQL
import qualified Hasura.RQL.IR.BoolExp                 as IR
import qualified Hasura.RQL.IR.Delete                  as IR
import qualified Hasura.RQL.IR.OrderBy                 as IR
import qualified Hasura.RQL.IR.Select                  as IR
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

mkSQLSelect ::
     IR.JsonAggSelect
  -> IR.AnnSelectG 'MSSQL (IR.AnnFieldsG 'MSSQL Expression) Expression
  -> FromIr TSQL.Select
mkSQLSelect jsonAggSelect annSimpleSel =
  case jsonAggSelect of
    IR.JASMultipleRows -> fromSelectRows annSimpleSel
    IR.JASSingleObject -> do
      select <- fromSelectRows annSimpleSel
      pure
        select
          { selectFor =
              JsonFor
                ForJson {jsonCardinality = JsonSingleton, jsonRoot = NoRoot}
          , selectTop = Top 1
          }

-- | Convert from the IR database query into a select.
fromRootField :: GraphQL.QueryDB 'MSSQL Expression -> FromIr Select
fromRootField =
  \case
    (GraphQL.QDBSingleRow s)    -> mkSQLSelect IR.JASSingleObject s
    (GraphQL.QDBMultipleRows s) -> mkSQLSelect IR.JASMultipleRows s
    (GraphQL.QDBAggregation s)  -> fromSelectAggregate s

--------------------------------------------------------------------------------
-- Top-level exported functions

fromSelectRows :: IR.AnnSelectG 'MSSQL (IR.AnnFieldsG 'MSSQL Expression) Expression -> FromIr TSQL.Select
fromSelectRows annSelectG = do
  selectFrom <-
    case from of
      IR.FromTable qualifiedObject -> fromQualifiedTable qualifiedObject
      IR.FromFunction _ _ _        -> refute $ pure FunctionNotSupported
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
      , selectFrom
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
      case mPermLimit of
        Nothing    -> NoTop
        Just limit -> Top limit
    stringifyNumbers =
      if num
        then StringifyNumbers
        else LeaveNumbersAlone

fromSelectAggregate ::
     IR.AnnSelectG 'MSSQL [(IR.FieldName, IR.TableAggregateFieldG 'MSSQL Expression)] Expression
  -> FromIr TSQL.Select
fromSelectAggregate annSelectG = do
  selectFrom <-
    case from of
      IR.FromTable qualifiedObject -> fromQualifiedTable qualifiedObject
      IR.FromFunction _ _ _        -> refute $ pure FunctionNotSupported
  fieldSources <-
    runReaderT (traverse fromTableAggregateFieldG fields) (fromAlias selectFrom)
  filterExpression <-
    runReaderT (fromAnnBoolExp permFilter) (fromAlias selectFrom)
  Args { argsOrderBy
       , argsWhere
       , argsJoins
       , argsTop
       , argsDistinct = Proxy
       , argsOffset
       } <- runReaderT (fromSelectArgsG args) (fromAlias selectFrom)
  let selectProjections =
        concatMap (toList . fieldSourceProjections) fieldSources
  pure
    Select
      { selectProjections
      , selectTop = permissionBasedTop <> argsTop
      , selectFrom
      , selectJoins = argsJoins <> mapMaybe fieldSourceJoin fieldSources
      , selectWhere = argsWhere <> Where [filterExpression]
      , selectFor =
          JsonFor ForJson {jsonCardinality = JsonSingleton, jsonRoot = NoRoot}
      , selectOrderBy = argsOrderBy
      , selectOffset = argsOffset
      }
  where
    IR.AnnSelectG { _asnFields = fields
                  , _asnFrom = from
                  , _asnPerm = perm
                  , _asnArgs = args
                  , _asnStrfyNum = _num -- TODO: Do we ignore this for aggregates?
                  } = annSelectG
    IR.TablePerm {_tpLimit = mPermLimit, _tpFilter = permFilter} = perm
    permissionBasedTop =
      case mPermLimit of
        Nothing    -> NoTop
        Just limit -> Top limit


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
  argsWhere <-
    maybe (pure mempty) (fmap (Where . pure) . fromAnnBoolExp) mannBoolExp
  argsTop <- maybe (pure mempty) (pure . Top) mlimit
  argsOffset <-
    maybe (pure Nothing) (fmap Just . lift . fromSQLExpAsInt) moffset
  -- Not supported presently, per Vamshi:
  --
  -- > It is hardly used and we don't have to go to great lengths to support it.
  --
  -- But placeholdering the code so that when it's ready to be used,
  -- you can just drop the Proxy wrapper.
  argsDistinct <-
    case mdistinct of
      Nothing     -> pure Proxy
      Just (x, _) -> case x of {}
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
                  , _saDistinct = mdistinct
                  , _saOrderBy = orders
                  } = selectArgsG

-- | Produce a valid ORDER BY construct, telling about any joins
-- needed on the side.
fromAnnOrderByItemG ::
     IR.AnnOrderByItemG 'MSSQL Expression -> WriterT (Seq UnfurledJoin) (ReaderT EntityAlias FromIr) OrderBy
fromAnnOrderByItemG IR.OrderByItemG {obiType, obiColumn, obiNulls} = do
  orderByFieldName <- unfurlAnnOrderByElement obiColumn
  let orderByNullsOrder = fromMaybe NullsAnyOrder obiNulls
      orderByOrder      = fromMaybe AscOrder obiType
  pure OrderBy {..}

-- | Unfurl the nested set of object relations (tell'd in the writer)
-- that are terminated by field name (IR.AOCColumn and
-- IR.AOCArrayAggregation).
unfurlAnnOrderByElement ::
     IR.AnnOrderByElement 'MSSQL Expression -> WriterT (Seq UnfurledJoin) (ReaderT EntityAlias FromIr) FieldName
unfurlAnnOrderByElement =
  \case
    IR.AOCColumn pgColumnInfo ->
      lift (fromPGColumnInfo pgColumnInfo)
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
                           , selectFrom
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
    IR.AOCArrayAggregation IR.RelInfo {riMapping = mapping, riRTable = table} annBoolExp annAggregateOrderBy -> do
      selectFrom <- lift (lift (fromQualifiedTable table))
      let alias = aggFieldName
      joinAliasEntity <-
        lift (lift (generateEntityAlias (ForOrderAlias (tableNameText table))))
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
                            , selectFrom
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
      pure FieldName {fieldNameEntity = joinAliasEntity, fieldName = alias}


--------------------------------------------------------------------------------
-- Conversion functions

tableNameText :: {-PG.QualifiedObject-} TableName -> Text
tableNameText (TableName {tableName}) = tableName
-- tableNameText qualifiedObject = qname
--   where
--     PG.QualifiedObject {qName = PG.TableName qname} = qualifiedObject

-- | This is really the start where you query the base table,
-- everything else is joins attached to it.
fromQualifiedTable :: TableName -> FromIr From
fromQualifiedTable schemadTableName@(TableName{tableName}) = do
  alias <- generateEntityAlias (TableTemplate tableName)
  pure
    (FromQualifiedTable
       (Aliased
          { aliasedThing =
             schemadTableName {-TableName {tableName = qname, tableNameSchema = schemaName}-}
          , aliasedAlias = alias
          }))
  -- where
  --   PG.QualifiedObject { qSchema = PG.SchemaName schemaName
  --                        -- TODO: Consider many x.y.z. in schema name.
  --                       , qName = PG.TableName qname
  --                       } = qualifiedObject

fromTableName :: TableName -> FromIr EntityAlias
fromTableName TableName{tableName} = do
  alias <- generateEntityAlias (TableTemplate tableName)
  pure (EntityAlias alias)

fromAnnBoolExp ::
     IR.GBoolExp 'MSSQL (IR.AnnBoolExpFld 'MSSQL Expression)
  -> ReaderT EntityAlias FromIr Expression
fromAnnBoolExp = traverse fromAnnBoolExpFld >=> fromGBoolExp

fromAnnBoolExpFld ::
     IR.AnnBoolExpFld 'MSSQL Expression -> ReaderT EntityAlias FromIr Expression
fromAnnBoolExpFld =
  \case
    IR.AVCol pgColumnInfo opExpGs -> do
      expression <- fmap ColumnExpression (fromPGColumnInfo pgColumnInfo)
      expressions <- traverse (lift . fromOpExpG expression) opExpGs
      pure (AndExpression expressions)
    IR.AVRel IR.RelInfo {riMapping = mapping, riRTable = table} annBoolExp -> do
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
             , selectFrom
             , selectJoins = mempty
             , selectWhere = Where (foreignKeyConditions <> [whereExpression])
             , selectTop = NoTop
             , selectFor = NoFor
             , selectOffset = Nothing
             })

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
      , selectFrom
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

fromTableAggregateFieldG ::
     (IR.FieldName, IR.TableAggregateFieldG 'MSSQL Expression) -> ReaderT EntityAlias FromIr FieldSource
fromTableAggregateFieldG (IR.FieldName name, field) =
  case field of
    IR.TAFAgg (aggregateFields :: [(IR.FieldName, IR.AggregateField 'MSSQL)]) -> do
      aggregates <-
        for aggregateFields \(fieldName, aggregateField) ->
          fromAggregateField aggregateField <&> \aliasedThing ->
            Aliased {aliasedAlias = IR.getFieldNameTxt fieldName, ..}
      pure (AggregateFieldSource aggregates)
    IR.TAFExp text ->
      pure
        (ExpressionFieldSource
           Aliased
             { aliasedThing = TSQL.ValueExpression (ODBC.TextValue text)
             , aliasedAlias = name
             })
    IR.TAFNodes x _ -> case x of {}

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
  -> (IR.FieldName, IR.AnnFieldG 'MSSQL Expression)
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
    -- TODO:
    -- Vamshi said to ignore these three for now:
    IR.AFNodeId x _ _      -> case x of {}
    IR.AFRemote x _        -> case x of {}
    IR.AFComputedField x _ -> case x of {}

-- | Here is where we project a field as a column expression. If
-- number stringification is on, then we wrap it in a
-- 'ToStringExpression' so that it's casted when being projected.
fromAnnColumnField ::
     StringifyNumbers
  -> IR.AnnColumnField 'MSSQL
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
     else pure (ColumnExpression fieldName)
  where
    IR.AnnColumnField { _acfInfo = IR.ColumnInfo{pgiColumn=pgCol,pgiType=typ}
                      , _acfAsText = _asText :: Bool
                      , _acfOp = _ :: Maybe (IR.ColumnOp 'MSSQL) -- TODO: What's this?
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
  -> IR.ObjectRelationSelectG 'MSSQL Expression
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
                  , selectFrom
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
    IR.AnnObjectSelectG { _aosFields = fields :: IR.AnnFieldsG 'MSSQL Expression
                        , _aosTableFrom = tableFrom :: TableName{-PG.QualifiedTable-}
                        , _aosTableFilter = tableFilter :: IR.AnnBoolExp 'MSSQL Expression
                        } = annObjectSelectG
    IR.AnnRelationSelectG { aarRelationshipName
                          , aarColumnMapping = mapping :: HashMap ColumnName ColumnName -- PG.PGCol PG.PGCol
                          , aarAnnSelect = annObjectSelectG :: IR.AnnObjectSelectG 'MSSQL Expression
                          } = annRelationSelectG

lookupTableFrom ::
     Map TableName {-PG.QualifiedTable-} EntityAlias
  -> {-PG.QualifiedTable-}TableName
  -> FromIr (Either EntityAlias From)
lookupTableFrom existingJoins tableFrom = do
  case M.lookup tableFrom existingJoins of
    Just entityAlias -> pure (Left entityAlias)
    Nothing          -> fmap Right (fromQualifiedTable tableFrom)

fromArraySelectG :: IR.ArraySelectG 'MSSQL Expression -> ReaderT EntityAlias FromIr Join
fromArraySelectG =
  \case
    IR.ASSimple arrayRelationSelectG ->
      fromArrayRelationSelectG arrayRelationSelectG
    IR.ASAggregate arrayAggregateSelectG ->
      fromArrayAggregateSelectG arrayAggregateSelectG

fromArrayAggregateSelectG ::
     IR.AnnRelationSelectG 'MSSQL (IR.AnnAggregateSelectG 'MSSQL Expression)
  -> ReaderT EntityAlias FromIr Join
fromArrayAggregateSelectG annRelationSelectG = do
  fieldName <- lift (fromRelName aarRelationshipName)
  select <- lift (fromSelectAggregate annSelectG)
  joinSelect <-
    do foreignKeyConditions <- fromMapping (selectFrom select) mapping
       pure
         select {selectWhere = Where foreignKeyConditions <> selectWhere select}
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
                          , aarColumnMapping = mapping :: HashMap ColumnName ColumnName-- PG.PGCol PG.PGCol
                          , aarAnnSelect = annSelectG
                          } = annRelationSelectG

fromArrayRelationSelectG :: IR.ArrayRelationSelectG 'MSSQL Expression -> ReaderT EntityAlias FromIr Join
fromArrayRelationSelectG annRelationSelectG = do
  fieldName <- lift (fromRelName aarRelationshipName)
  select <- lift (fromSelectRows annSelectG)
  joinSelect <-
    do foreignKeyConditions <- fromMapping (selectFrom select) mapping
       pure
         select {selectWhere = Where foreignKeyConditions <> selectWhere select}
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
         (EqualExpression
            (ColumnExpression localFieldName)
            (ColumnExpression remoteFieldName))) .
  HM.toList


--------------------------------------------------------------------------------
-- Basic SQL expression types

fromOpExpG :: Expression -> IR.OpExpG 'MSSQL Expression -> FromIr Expression
fromOpExpG expression op =
  case op of
    IR.ANISNULL                  -> pure (IsNullExpression expression)
    IR.ANISNOTNULL               -> pure (IsNotNullExpression expression)
    IR.AEQ False val             -> pure (nullableBoolEquality expression val)
    IR.AEQ True val              -> pure (EqualExpression expression val)
    IR.ANE False val             -> pure (nullableBoolInequality expression val)
    IR.ANE True val              -> pure (NotEqualExpression expression val)
    IR.AGT val                   -> pure (OpExpression MoreOp expression val)
    IR.ALT val                   -> pure (OpExpression LessOp expression val)
    IR.AGTE val                  -> pure (OpExpression MoreOrEqualOp expression val)
    IR.ALTE val                  -> pure (OpExpression LessOrEqualOp expression val)
    IR.ACast _casts              -> refute (pure (UnsupportedOpExpG op)) -- mkCastsExp casts
    IR.AIN _val                  -> refute (pure (UnsupportedOpExpG op)) -- S.BECompareAny S.SEQ lhs val
    IR.ANIN _val                 -> refute (pure (UnsupportedOpExpG op)) -- S.BENot $ S.BECompareAny S.SEQ lhs val
    IR.ALIKE _val                -> refute (pure (UnsupportedOpExpG op)) -- S.BECompare S.SLIKE lhs val
    IR.ANLIKE _val               -> refute (pure (UnsupportedOpExpG op)) -- S.BECompare S.SNLIKE lhs val
    IR.AILIKE _ _val             -> refute (pure (UnsupportedOpExpG op)) -- S.BECompare S.SILIKE lhs val
    IR.ANILIKE _ _val            -> refute (pure (UnsupportedOpExpG op)) -- S.BECompare S.SNILIKE lhs val
    IR.ASIMILAR _val             -> refute (pure (UnsupportedOpExpG op)) -- S.BECompare S.SSIMILAR lhs val
    IR.ANSIMILAR _val            -> refute (pure (UnsupportedOpExpG op)) -- S.BECompare S.SNSIMILAR lhs val
    IR.AREGEX _val               -> refute (pure (UnsupportedOpExpG op)) -- S.BECompare S.SNSIMILAR lhs val
    IR.AIREGEX _val              -> refute (pure (UnsupportedOpExpG op)) -- S.BECompare S.SNSIMILAR lhs val
    IR.ANREGEX _val              -> refute (pure (UnsupportedOpExpG op)) -- S.BECompare S.SNSIMILAR lhs val
    IR.ANIREGEX _val             -> refute (pure (UnsupportedOpExpG op)) -- S.BECompare S.SNSIMILAR lhs val
    IR.AContains _val            -> refute (pure (UnsupportedOpExpG op)) -- S.BECompare S.SContains lhs val
    IR.AContainedIn _val         -> refute (pure (UnsupportedOpExpG op)) -- S.BECompare S.SContainedIn lhs val
    IR.AHasKey _val              -> refute (pure (UnsupportedOpExpG op)) -- S.BECompare S.SHasKey lhs val
    IR.AHasKeysAny _val          -> refute (pure (UnsupportedOpExpG op)) -- S.BECompare S.SHasKeysAny lhs val
    IR.AHasKeysAll _val          -> refute (pure (UnsupportedOpExpG op)) -- S.BECompare S.SHasKeysAll lhs val
    IR.ASTContains _val          -> refute (pure (UnsupportedOpExpG op)) -- mkGeomOpBe "ST_Contains" val
    IR.ASTCrosses _val           -> refute (pure (UnsupportedOpExpG op)) -- mkGeomOpBe "ST_Crosses" val
    IR.ASTEquals _val            -> refute (pure (UnsupportedOpExpG op)) -- mkGeomOpBe "ST_Equals" val
    IR.ASTIntersects _val        -> refute (pure (UnsupportedOpExpG op)) -- mkGeomOpBe "ST_Intersects" val
    IR.ASTOverlaps _val          -> refute (pure (UnsupportedOpExpG op)) -- mkGeomOpBe "ST_Overlaps" val
    IR.ASTTouches _val           -> refute (pure (UnsupportedOpExpG op)) -- mkGeomOpBe "ST_Touches" val
    IR.ASTWithin _val            -> refute (pure (UnsupportedOpExpG op)) -- mkGeomOpBe "ST_Within" val
    IR.ASTDWithinGeom {}         -> refute (pure (UnsupportedOpExpG op)) -- applySQLFn "ST_DWithin" [lhs, val, r]
    IR.ASTDWithinGeog {}         -> refute (pure (UnsupportedOpExpG op)) -- applySQLFn "ST_DWithin" [lhs, val, r, sph]
    IR.ASTIntersectsRast _val    -> refute (pure (UnsupportedOpExpG op)) -- applySTIntersects [lhs, val]
    IR.ASTIntersectsNbandGeom {} -> refute (pure (UnsupportedOpExpG op)) -- applySTIntersects [lhs, nband, geommin]
    IR.ASTIntersectsGeomNband {} -> refute (pure (UnsupportedOpExpG op)) -- applySTIntersects [lhs, geommin, withSQLNull mNband]
    IR.CEQ _rhsCol               -> refute (pure (UnsupportedOpExpG op)) -- S.BECompare S.SEQ lhs $ mkQCol rhsCol
    IR.CNE _rhsCol               -> refute (pure (UnsupportedOpExpG op)) -- S.BECompare S.SNE lhs $ mkQCol rhsCol
    IR.CGT _rhsCol               -> refute (pure (UnsupportedOpExpG op)) -- S.BECompare S.SGT lhs $ mkQCol rhsCol
    IR.CLT _rhsCol               -> refute (pure (UnsupportedOpExpG op)) -- S.BECompare S.SLT lhs $ mkQCol rhsCol
    IR.CGTE _rhsCol              -> refute (pure (UnsupportedOpExpG op)) -- S.BECompare S.SGTE lhs $ mkQCol rhsCol
    IR.CLTE _rhsCol              -> refute (pure (UnsupportedOpExpG op)) -- S.BECompare S.SLTE lhs $ mkQCol rhsCol

nullableBoolEquality :: Expression -> Expression -> Expression
nullableBoolEquality x y =
  OrExpression
    [ EqualExpression x y
    , AndExpression [IsNullExpression x, IsNullExpression y]
    ]

nullableBoolInequality :: Expression -> Expression -> Expression
nullableBoolInequality x y =
  OrExpression
    [ NotEqualExpression x y
    , AndExpression [IsNotNullExpression x, IsNullExpression y]
    ]

fromSQLExpAsInt :: Expression -> FromIr Expression
fromSQLExpAsInt = pure

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

columnNameToFieldName :: ColumnName -> EntityAlias -> FieldName
columnNameToFieldName (ColumnName fieldName) EntityAlias {entityAliasText = fieldNameEntity} =
  FieldName {fieldName, fieldNameEntity}
