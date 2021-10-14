-- | Translate from the DML to the MySQL dialect.
module Hasura.Backends.MySQL.FromIr
  ( fromSelectRows,
    mkSQLSelect,
    fromRootField,
    FromIr,
    Error (..),
    runFromIr,
  )
where

import Control.Monad.Validate
import Data.HashMap.Strict qualified as HM
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Proxy
import Data.Text qualified as T
import Database.MySQL.Base.Types qualified as MySQL
import Hasura.Backends.MySQL.Instances.Types ()
import Hasura.Backends.MySQL.Types
import Hasura.Prelude
import Hasura.RQL.IR qualified as IR
import Hasura.RQL.Types.Column qualified as IR
import Hasura.RQL.Types.Common qualified as IR
import Hasura.RQL.Types.Relationship qualified as IR
import Hasura.SQL.Backend

data FieldSource
  = ExpressionFieldSource (Aliased Expression)
  | JoinFieldSource (Aliased Join)
  | AggregateFieldSource [Aliased Aggregate]
  deriving (Eq, Show)

-- | Most of these errors should be checked for legitimacy.
data Error
  = UnsupportedOpExpG (IR.OpExpG 'MySQL Expression)
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
        ArrayRelationTemplate sample -> "ar_" <> sample
        ArrayAggregateTemplate sample -> "aa_" <> sample
        ObjectRelationTemplate sample -> "or_" <> sample
        TableTemplate sample -> "t_" <> sample
        ForOrderAlias sample -> "order_" <> sample

-- | This is really the start where you query the base table,
-- everything else is joins attached to it.
fromQualifiedTable :: TableName -> FromIr From
fromQualifiedTable schemadTableName@(TableName {name}) = do
  alias <- generateEntityAlias (TableTemplate name)
  pure
    ( FromQualifiedTable
        ( Aliased
            { aliasedThing =
                schemadTableName,
              aliasedAlias = alias
            }
        )
    )

fromAlias :: From -> EntityAlias
fromAlias (FromQualifiedTable Aliased {aliasedAlias}) = EntityAlias aliasedAlias
fromAlias (FromOpenJson Aliased {aliasedAlias}) = EntityAlias aliasedAlias

trueExpression :: Expression
trueExpression = ValueExpression (BitValue True)

existsFieldName :: Text
existsFieldName = "exists_placeholder"

fromGExists :: IR.GExists 'MySQL Expression -> ReaderT EntityAlias FromIr Select
fromGExists IR.GExists {_geTable, _geWhere} = do
  selectFrom <- lift (fromQualifiedTable _geTable)
  whereExpression <-
    local (const (fromAlias selectFrom)) (fromGBoolExp _geWhere)
  pure
    Select
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
        selectFor = NoFor,
        selectTop = NoTop,
        selectOffset = Nothing
      }

fromGBoolExp :: IR.GBoolExp 'MySQL Expression -> ReaderT EntityAlias FromIr Expression
fromGBoolExp = do
  \case
    IR.BoolAnd expressions ->
      fmap AndExpression (traverse fromGBoolExp expressions)
    IR.BoolOr expressions ->
      fmap OrExpression (traverse fromGBoolExp expressions)
    IR.BoolNot expression ->
      fmap NotExpression (fromGBoolExp expression)
    IR.BoolExists gExists ->
      fmap ExistsExpression (fromGExists gExists)
    IR.BoolFld expression ->
      pure expression

fromAnnBoolExp ::
  IR.GBoolExp 'MySQL (IR.AnnBoolExpFld 'MySQL Expression) ->
  ReaderT EntityAlias FromIr Expression
fromAnnBoolExp boolExp = do
  fields <- traverse fromAnnBoolExpFld boolExp
  fromGBoolExp fields

-- | For boolean operators, various comparison operators used need
-- special handling to ensure that SQL Server won't outright reject
-- the comparison. See also 'shouldCastToVarcharMax'.
fromColumnInfoForBoolExp :: IR.ColumnInfo 'MySQL -> ReaderT EntityAlias FromIr Expression
fromColumnInfoForBoolExp IR.ColumnInfo {pgiColumn = pgCol, pgiType = _pgiType} = do
  fieldName <- columnNameToFieldName pgCol <$> ask
  pure (ColumnExpression fieldName)

fromAnnBoolExpFld ::
  IR.AnnBoolExpFld 'MySQL Expression ->
  ReaderT EntityAlias FromIr Expression
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
        ( ExistsExpression
            Select
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
                selectFor = NoFor,
                selectTop = NoTop,
                selectOffset = Nothing
              }
        )

-- | The context given by the reader is of the previous/parent
-- "remote" table. The WHERE that we're generating goes in the child,
-- "local" query. The @From@ passed in as argument is the local table.
--
-- We should hope to see e.g. "post.category = category.id" for a
-- local table of post and a remote table of category.
--
-- The left/right columns in @HashMap Column Column@ corresponds
-- to the left/right of @select ... join ...@. Therefore left=remote,
-- right=local in this context.
fromMapping ::
  From ->
  HashMap Column Column ->
  ReaderT EntityAlias FromIr [Expression]
fromMapping localFrom = traverse columnsToEqs . HM.toList
  where
    columnsToEqs (remotePgCol, localPgCol) = do
      localFieldName <- local (const (fromAlias localFrom)) (fromPGCol localPgCol)
      remoteFieldName <- fromPGCol remotePgCol
      pure
        ( OpExpression
            EQ'
            (ColumnExpression localFieldName)
            (ColumnExpression remoteFieldName)
        )

fromPGCol :: Column -> ReaderT EntityAlias FromIr FieldName
fromPGCol pgCol = columnNameToFieldName pgCol <$> ask

columnNameToFieldName :: Column -> EntityAlias -> FieldName
columnNameToFieldName (Column fieldName) EntityAlias {entityAliasText = fieldNameEntity} =
  FieldName {fName = fieldName, fNameEntity = fieldNameEntity}

fromOpExpG :: Expression -> IR.OpExpG 'MySQL Expression -> FromIr Expression
fromOpExpG expression op =
  case op of
    IR.AEQ True val -> do
      pure $ OpExpression EQ' expression val
    _ -> refute (pure (UnsupportedOpExpG op))

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

fromPGColumnInfo :: IR.ColumnInfo 'MySQL -> ReaderT EntityAlias FromIr FieldName
fromPGColumnInfo IR.ColumnInfo {pgiColumn = pgCol} =
  columnNameToFieldName pgCol <$> ask

tableNameText :: TableName -> Text
tableNameText (TableName {name}) = name

aggFieldName :: Text
aggFieldName = "agg"

-- | Unfurl the nested set of object relations (tell'd in the writer)
-- that are terminated by field name (IR.AOCColumn and
-- IR.AOCArrayAggregation).
unfurlAnnOrderByElement ::
  IR.AnnotatedOrderByElement 'MySQL Expression ->
  WriterT (Seq UnfurledJoin) (ReaderT EntityAlias FromIr) (FieldName, Maybe ScalarType)
unfurlAnnOrderByElement =
  \case
    IR.AOCColumn pgColumnInfo -> do
      fieldName <- lift (fromPGColumnInfo pgColumnInfo)
      pure
        ( fieldName,
          case IR.pgiType pgColumnInfo of
            IR.ColumnScalar t -> Just t
            _ -> Nothing
        )
    IR.AOCObjectRelation IR.RelInfo {riMapping = mapping, riRTable = table} annBoolExp annOrderByElementG -> do
      selectFrom <- lift (lift (fromQualifiedTable table))
      joinAliasEntity <-
        lift (lift (generateEntityAlias (ForOrderAlias (tableNameText table))))
      foreignKeyConditions <- lift (fromMapping selectFrom mapping)
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
                            { selectProjections = [StarProjection],
                              selectFrom = Just selectFrom,
                              selectJoins = [],
                              selectWhere =
                                Where (foreignKeyConditions <> [whereExpression]),
                              selectFor = NoFor,
                              selectOrderBy = Nothing,
                              selectOffset = Nothing,
                              selectTop = NoTop
                            },
                      joinJoinAlias =
                        JoinAlias {joinAliasEntity, joinAliasField = Nothing}
                    },
                unfurledObjectTableAlias = Just (table, EntityAlias joinAliasEntity)
              }
        )
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
          ( local
              (const (fromAlias selectFrom))
              ( case annAggregateOrderBy of
                  IR.AAOCount -> pure (CountAggregate StarCountable)
                  IR.AAOOp text pgColumnInfo -> do
                    fieldName <- fromPGColumnInfo pgColumnInfo
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
                            Select
                              { selectProjections =
                                  [ AggregateProjection
                                      Aliased
                                        { aliasedThing = aggregate,
                                          aliasedAlias = alias
                                        }
                                  ],
                                selectTop = NoTop,
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
        ( FieldName {fNameEntity = joinAliasEntity, fName = alias},
          Nothing
        )

-- | Produce a valid ORDER BY construct, telling about any joins
-- needed on the side.
fromAnnOrderByItemG ::
  IR.AnnotatedOrderByItemG 'MySQL Expression ->
  WriterT (Seq UnfurledJoin) (ReaderT EntityAlias FromIr) OrderBy
fromAnnOrderByItemG IR.OrderByItemG {obiType, obiColumn = obiColumn, obiNulls} = do
  (orderByFieldName, orderByType) <- unfurlAnnOrderByElement obiColumn
  let orderByNullsOrder = fromMaybe NullsAnyOrder obiNulls
      orderByOrder = fromMaybe Asc obiType
  pure OrderBy {..}

fromSelectArgsG :: IR.SelectArgsG 'MySQL Expression -> ReaderT EntityAlias FromIr Args
fromSelectArgsG selectArgsG = do
  let argsOffset = ValueExpression . IntValue . fromIntegral <$> moffset
  argsWhere <-
    maybe (pure mempty) (fmap (Where . pure) . fromAnnBoolExp) mannBoolExp
  argsTop <-
    maybe (pure mempty) (pure . Top) mlimit
  let argsDistinct = Proxy
  (argsOrderBy, joins) <-
    runWriterT (traverse fromAnnOrderByItemG (maybe [] toList orders))
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

-- | Here is where we project a field as a column expression. If
-- number stringification is on, then we wrap it in a
-- 'ToStringExpression' so that it's casted when being projected.
fromAnnColumnField ::
  StringifyNumbers ->
  IR.AnnColumnField 'MySQL Expression ->
  ReaderT EntityAlias FromIr Expression
fromAnnColumnField _stringifyNumbers annColumnField = do
  fieldName <- fromPGCol pgCol
  if typ == IR.ColumnScalar MySQL.Geometry
    then pure $ MethodExpression (ColumnExpression fieldName) "STAsText" []
    else pure (ColumnExpression fieldName)
  where
    IR.AnnColumnField
      { _acfColumn = pgCol,
        _acfType = typ,
        _acfAsText = _asText :: Bool,
        _acfOp = _ :: Maybe (IR.ColumnOp 'MySQL)
      } = annColumnField

fromRelName :: IR.RelName -> FromIr Text
fromRelName relName =
  pure (IR.relNameToTxt relName)

jsonFieldName :: Text
jsonFieldName = "json"

-- fromAggregateField :: IR.AggregateField 'MySQL -> ReaderT EntityAlias FromIr Aggregate
-- fromAggregateField aggregateField =
--   case aggregateField of
--     IR.AFExp text        -> pure (TextAggregate text)
--     IR.AFCount countType -> CountAggregate <$> case countType of
--       StarCountable               -> pure StarCountable
--       NonNullFieldCountable names -> NonNullFieldCountable <$> traverse fromPGCol names
--       DistinctCountable     names -> DistinctCountable     <$> traverse fromPGCol names
--     IR.AFOp _ -> error "fromAggregatefield: not implemented"

fromTableAggregateFieldG ::
  (IR.FieldName, IR.TableAggregateFieldG 'MySQL (Const Void) Expression) -> ReaderT EntityAlias FromIr FieldSource
fromTableAggregateFieldG (IR.FieldName _name, _field) = error "fromTableAggregateFieldG: not implemented yet"

fieldSourceProjections :: FieldSource -> [Projection]
fieldSourceProjections =
  \case
    ExpressionFieldSource aliasedExpression ->
      pure (ExpressionProjection aliasedExpression)
    JoinFieldSource _aliasedJoin -> error "fieldSourceProjections: not implemented yet"
    AggregateFieldSource aggregates -> fmap AggregateProjection aggregates

fieldSourceJoin :: FieldSource -> Maybe Join
fieldSourceJoin =
  \case
    JoinFieldSource aliasedJoin -> pure (aliasedThing aliasedJoin)
    ExpressionFieldSource {} -> Nothing
    AggregateFieldSource {} -> Nothing

fromSelectAggregate ::
  Maybe (EntityAlias, HashMap Column Column) ->
  IR.AnnSelectG 'MySQL (Const Void) (IR.TableAggregateFieldG 'MySQL (Const Void)) Expression ->
  FromIr Select
fromSelectAggregate mparentRelationship annSelectG = do
  selectFrom <-
    case from of
      IR.FromTable qualifiedObject -> fromQualifiedTable qualifiedObject
      IR.FromFunction {} -> refute $ pure FunctionNotSupported
  _mforeignKeyConditions <- fmap (Where . fromMaybe []) $
    for mparentRelationship $
      \(entityAlias, mapping) ->
        runReaderT (fromMapping selectFrom mapping) entityAlias
  fieldSources <-
    runReaderT (traverse fromTableAggregateFieldG fields) (fromAlias selectFrom)
  filterExpression <-
    runReaderT (fromAnnBoolExp permFilter) (fromAlias selectFrom)
  Args
    { argsOrderBy,
      argsWhere,
      argsJoins,
      argsTop,
      argsDistinct = Proxy,
      argsOffset
    } <-
    runReaderT (fromSelectArgsG args) (fromAlias selectFrom)
  let selectProjections =
        concatMap (toList . fieldSourceProjections) fieldSources
  pure
    Select
      { selectProjections,
        selectFrom = Just selectFrom,
        selectJoins = argsJoins <> mapMaybe fieldSourceJoin fieldSources,
        selectWhere = argsWhere <> Where [filterExpression],
        selectFor =
          JsonFor ForJson {jsonCardinality = JsonSingleton, jsonRoot = Root "aggregate"},
        selectOrderBy = argsOrderBy,
        selectOffset = argsOffset,
        selectTop = permissionBasedTop <> argsTop
      }
  where
    permissionBasedTop =
      maybe NoTop Top mPermLimit
    IR.AnnSelectG
      { _asnFields = fields,
        _asnFrom = from,
        _asnPerm = perm,
        _asnArgs = args,
        _asnStrfyNum = _num
      } = annSelectG
    IR.TablePerm {_tpLimit = mPermLimit, _tpFilter = permFilter} = perm

-- _fromTableAggFieldG ::
--   (Int, (IR.FieldName, IR.TableAggregateFieldG 'MySQL (Const Void) Expression)) ->
--   Maybe (ReaderT EntityAlias FromIr (Int, (IR.FieldName, [Projection])))
-- _fromTableAggFieldG = \case
--   (index, (fieldName, IR.TAFAgg (aggregateFields :: [(IR.FieldName, IR.AggregateField 'MySQL)]))) -> Just do
--     aggregates <-
--       for aggregateFields \(fieldName', aggregateField) ->
--         fromAggregateField aggregateField <&> \aliasedThing ->
--           Aliased {aliasedAlias = IR.getFieldNameTxt fieldName', ..}
--     pure (index, (fieldName, fieldSourceProjections $ AggregateFieldSource aggregates))
--   _ -> Nothing

-- _fromTableNodesFieldG ::
--   Map TableName EntityAlias ->
--   StringifyNumbers ->
--   (Int, (IR.FieldName, IR.TableAggregateFieldG 'MySQL (Const Void) Expression)) ->
--   Maybe (ReaderT EntityAlias FromIr (Int, (IR.FieldName, [Projection])))
-- _fromTableNodesFieldG argsExistingJoins stringifyNumbers = \case
--   (index, (fieldName, IR.TAFNodes () (annFieldsG :: [(IR.FieldName, IR.AnnFieldG 'MySQL (Const Void) Expression)]))) -> Just do
--     fieldSources' <- fromAnnFieldsG argsExistingJoins stringifyNumbers `traverse` annFieldsG
--     let nodesProjections' :: [Projection] = concatMap fieldSourceProjections fieldSources'
--     pure (index, (fieldName, nodesProjections'))
--   _ -> Nothing

-- -- | Get FieldSource from a TAFExp type table aggregate field
-- _fromTableExpFieldG ::
--   (Int, (IR.FieldName, IR.TableAggregateFieldG 'MySQL (Const Void) Expression)) ->
--   Maybe (ReaderT EntityAlias FromIr (Int, [Projection]))
-- _fromTableExpFieldG = \case
--   (index, (IR.FieldName name, IR.TAFExp text)) -> Just $
--     pure
--       (index, fieldSourceProjections $
--         ExpressionFieldSource
--           Aliased
--             { aliasedThing = ValueExpression (TextValue text)
--             , aliasedAlias = name
--             })
--   _ -> Nothing

selectFromMapping ::
  Select ->
  HashMap Column Column ->
  ReaderT EntityAlias FromIr [Expression]
selectFromMapping Select {selectFrom = Nothing} = const (pure [])
selectFromMapping Select {selectFrom = Just from} = fromMapping from

fromArrayAggregateSelectG ::
  IR.AnnRelationSelectG 'MySQL (IR.AnnAggregateSelectG 'MySQL (Const Void) Expression) ->
  ReaderT EntityAlias FromIr Join
fromArrayAggregateSelectG annRelationSelectG = do
  fieldName <- lift (fromRelName aarRelationshipName)
  joinSelect' <- do
    lhsEntityAlias <- ask
    -- With this, the foreign key relations are injected automatically
    -- at the right place by fromSelectAggregate.
    lift (fromSelectAggregate (pure (lhsEntityAlias, mapping)) annSelectG)
  alias <- lift (generateEntityAlias (ArrayAggregateTemplate fieldName))
  pure
    Join
      { joinJoinAlias =
          JoinAlias
            { joinAliasEntity = alias,
              joinAliasField = pure jsonFieldName
            },
        joinSource = JoinSelect joinSelect'
      }
  where
    IR.AnnRelationSelectG
      { aarRelationshipName,
        aarColumnMapping = mapping :: HashMap Column Column,
        aarAnnSelect = annSelectG
      } = annRelationSelectG

fromArraySelectG :: IR.ArraySelectG 'MySQL (Const Void) Expression -> ReaderT EntityAlias FromIr Join
fromArraySelectG =
  \case
    IR.ASSimple arrayRelationSelectG ->
      fromArrayRelationSelectG arrayRelationSelectG
    IR.ASAggregate arrayAggregateSelectG ->
      fromArrayAggregateSelectG arrayAggregateSelectG

lookupTableFrom ::
  Map TableName EntityAlias ->
  TableName ->
  FromIr (Either EntityAlias From)
lookupTableFrom existingJoins tableFrom = do
  case M.lookup tableFrom existingJoins of
    Just entityAlias -> pure (Left entityAlias)
    Nothing -> fmap Right (fromQualifiedTable tableFrom)

fromObjectRelationSelectG ::
  Map TableName EntityAlias ->
  IR.ObjectRelationSelectG 'MySQL (Const Void) Expression ->
  ReaderT EntityAlias FromIr Join
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
    do
      fieldName <- lift (fromRelName aarRelationshipName)
      alias <- lift (generateEntityAlias (ObjectRelationTemplate fieldName))
      pure
        JoinAlias
          { joinAliasEntity = alias,
            joinAliasField = pure jsonFieldName
          }
  let selectFor =
        JsonFor ForJson {jsonCardinality = JsonSingleton, jsonRoot = NoRoot}
  filterExpression <- local (const entityAlias) (fromAnnBoolExp tableFilter)
  case eitherAliasOrFrom of
    Right selectFrom -> do
      foreignKeyConditions <- fromMapping selectFrom mapping
      pure
        Join
          { joinJoinAlias,
            joinSource =
              JoinSelect
                Select
                  { selectOrderBy = Nothing,
                    selectProjections,
                    selectFrom = Just selectFrom,
                    selectJoins = mapMaybe fieldSourceJoin fieldSources,
                    selectWhere =
                      Where (foreignKeyConditions <> [filterExpression]),
                    selectFor,
                    selectOffset = Nothing,
                    selectTop = NoTop
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
      { _aosFields = fields :: IR.AnnFieldsG 'MySQL (Const Void) Expression,
        _aosTableFrom = tableFrom :: TableName,
        _aosTableFilter = tableFilter :: IR.AnnBoolExp 'MySQL Expression
      } = annObjectSelectG
    IR.AnnRelationSelectG
      { aarRelationshipName,
        aarColumnMapping = mapping :: HashMap Column Column,
        aarAnnSelect = annObjectSelectG :: IR.AnnObjectSelectG 'MySQL (Const Void) Expression
      } = annRelationSelectG

isEmptyExpression :: Expression -> Bool
isEmptyExpression (AndExpression []) = True
isEmptyExpression (OrExpression []) = True
isEmptyExpression _ = False

fromSelectRows :: IR.AnnSelectG 'MySQL (Const Void) (IR.AnnFieldG 'MySQL (Const Void)) Expression -> FromIr Select
fromSelectRows annSelectG = do
  selectFrom <-
    case from of
      IR.FromTable qualifiedObject -> fromQualifiedTable qualifiedObject
      IR.FromFunction {} -> refute $ pure FunctionNotSupported
  Args
    { argsOrderBy,
      argsWhere,
      argsJoins,
      argsDistinct = Proxy,
      argsOffset,
      argsTop,
      argsExistingJoins
    } <-
    runReaderT (fromSelectArgsG args) (fromAlias selectFrom)
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
      { selectOrderBy = argsOrderBy,
        selectProjections,
        selectFrom = Just selectFrom,
        selectJoins = argsJoins <> mapMaybe fieldSourceJoin fieldSources,
        selectWhere = argsWhere <> Where (if isEmptyExpression filterExpression then [] else [filterExpression]),
        selectFor =
          JsonFor ForJson {jsonCardinality = JsonArray, jsonRoot = NoRoot},
        selectOffset = argsOffset,
        selectTop = permissionBasedTop <> argsTop
      }
  where
    permissionBasedTop =
      maybe NoTop Top mPermLimit
    IR.AnnSelectG
      { _asnFields = fields,
        _asnFrom = from,
        _asnPerm = perm,
        _asnArgs = args,
        _asnStrfyNum = num
      } = annSelectG
    IR.TablePerm {_tpLimit = mPermLimit, _tpFilter = permFilter} = perm
    stringifyNumbers =
      if num
        then StringifyNumbers
        else LeaveNumbersAlone

fromArrayRelationSelectG :: IR.ArrayRelationSelectG 'MySQL (Const Void) Expression -> ReaderT EntityAlias FromIr Join
fromArrayRelationSelectG annRelationSelectG = do
  fieldName <- lift (fromRelName aarRelationshipName)
  sel <- lift (fromSelectRows annSelectG)
  joinSelect' <-
    do
      foreignKeyConditions <- selectFromMapping sel mapping
      pure
        sel {selectWhere = Where foreignKeyConditions <> selectWhere sel}
  alias <- lift (generateEntityAlias (ArrayRelationTemplate fieldName))
  pure
    Join
      { joinJoinAlias =
          JoinAlias
            { joinAliasEntity = alias,
              joinAliasField = pure jsonFieldName
            },
        joinSource = JoinSelect joinSelect'
      }
  where
    IR.AnnRelationSelectG
      { aarRelationshipName,
        aarColumnMapping = mapping :: HashMap Column Column,
        aarAnnSelect = annSelectG
      } = annRelationSelectG

-- | The main sources of fields, either constants, fields or via joins.
fromAnnFieldsG ::
  Map TableName EntityAlias ->
  StringifyNumbers ->
  (IR.FieldName, IR.AnnFieldG 'MySQL (Const Void) Expression) ->
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
              { aliasedThing = ValueExpression (TextValue text),
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

mkSQLSelect ::
  IR.JsonAggSelect ->
  IR.AnnSelectG 'MySQL (Const Void) (IR.AnnFieldG 'MySQL (Const Void)) Expression ->
  FromIr Select
mkSQLSelect jsonAggSelect annSimpleSel = do
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
fromRootField :: IR.QueryDB 'MySQL (Const Void) Expression -> FromIr Select
fromRootField =
  \case
    (IR.QDBSingleRow s) -> mkSQLSelect IR.JASSingleObject s
    (IR.QDBMultipleRows s) -> mkSQLSelect IR.JASMultipleRows s
    (IR.QDBAggregation s) -> fromSelectAggregate Nothing s
