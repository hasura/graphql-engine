{-# LANGUAGE ViewPatterns #-}

-- | This module defines translation functions for queries which select data.
-- Principally this includes translating the @query@ root field, but parts are
-- also reused for serving the responses for mutations.
module Hasura.Backends.MSSQL.FromIr.Query
  ( fromQueryRootField,
    fromSelect,
    fromSourceRelationship,
  )
where

import Control.Applicative (getConst)
import Control.Monad.Validate
import Data.Aeson.Extended qualified as J
import Data.HashMap.Strict qualified as HashMap
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Proxy
import Data.Text.Extended qualified as T
import Data.Text.NonEmpty (mkNonEmptyTextUnsafe)
import Database.ODBC.SQLServer qualified as ODBC
import Hasura.Backends.MSSQL.FromIr
  ( Error (..),
    FromIr,
    NameTemplate (..),
    generateAlias,
    tellAfter,
    tellBefore,
    tellCTE,
  )
import Hasura.Backends.MSSQL.FromIr.Constants
import Hasura.Backends.MSSQL.FromIr.Expression
import Hasura.Backends.MSSQL.Instances.Types ()
import Hasura.Backends.MSSQL.Types.Internal as TSQL
import Hasura.LogicalModel.Common (columnsFromFields)
import Hasura.LogicalModel.IR (LogicalModel (..))
import Hasura.NativeQuery.IR qualified as IR
import Hasura.NativeQuery.InterpolatedQuery
import Hasura.NativeQuery.Types (NativeQueryName (..))
import Hasura.Prelude
import Hasura.RQL.IR qualified as IR
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Column qualified as IR
import Hasura.RQL.Types.Common qualified as IR
import Hasura.RQL.Types.Relationships.Local qualified as IR
import Hasura.StoredProcedure.IR qualified as IR

-- | This is the top-level entry point for translation of Query root fields.
fromQueryRootField :: IR.QueryDB 'MSSQL Void Expression -> FromIr Select
fromQueryRootField =
  \case
    (IR.QDBSingleRow s) -> fromSelect IR.JASSingleObject s
    (IR.QDBMultipleRows s) -> fromSelect IR.JASMultipleRows s
    (IR.QDBAggregation s) -> fromSelectAggregate Nothing s

fromSelect ::
  IR.JsonAggSelect ->
  IR.AnnSelectG 'MSSQL (IR.AnnFieldG 'MSSQL Void) Expression ->
  FromIr TSQL.Select
fromSelect jsonAggSelect annSimpleSel =
  case jsonAggSelect of
    IR.JASMultipleRows ->
      guardSelectYieldingNull emptyArrayExpression <$> fromSelectRows annSimpleSel
    IR.JASSingleObject ->
      fmap (guardSelectYieldingNull nullExpression)
        $ fromSelectRows annSimpleSel
        <&> \sel ->
          sel
            { selectFor =
                JsonFor
                  ForJson {jsonCardinality = JsonSingleton, jsonRoot = NoRoot},
              selectTop = Top 1
            }
  where
    guardSelectYieldingNull :: TSQL.Expression -> TSQL.Select -> TSQL.Select
    guardSelectYieldingNull fallbackExpression select =
      let isNullApplication = FunExpISNULL (SelectExpression select) fallbackExpression
       in emptySelect
            { selectProjections =
                [ ExpressionProjection
                    $ Aliased
                      { aliasedThing = FunctionApplicationExpression isNullApplication,
                        aliasedAlias = "root"
                      }
                ]
            }

-- | Used in 'Hasura.Backends.MSSQL.Plan.planSourceRelationship', which is in
-- turn used by to implement `mkDBRemoteRelationship' for 'BackendExecute'.
-- For more information, see the module/documentation of 'Hasura.GraphQL.Execute.RemoteJoin.Source'.
fromSourceRelationship ::
  -- | List of json objects, each of which becomes a row of the table
  NE.NonEmpty J.Object ->
  -- | The above objects have this schema
  HashMap.HashMap IR.FieldName (ColumnName, ScalarType) ->
  IR.FieldName ->
  (IR.FieldName, IR.SourceRelationshipSelection 'MSSQL Void (Const Expression)) ->
  FromIr TSQL.Select
fromSourceRelationship lhs lhsSchema argumentId relationshipField = do
  (argumentIdQualified, fieldSource) <-
    flip runReaderT (fromAlias selectFrom) $ do
      argumentIdQualified <- fromColumn (coerceToColumn argumentId)
      relationshipSource <-
        fromRemoteRelationFieldsG
          mempty
          (fst <$> joinColumns)
          relationshipField
      pure (ColumnExpression argumentIdQualified, relationshipSource)
  let selectProjections = [projectArgumentId argumentIdQualified, fieldSourceProjections fieldSource]
  pure
    Select
      { selectWith = Nothing,
        selectOrderBy = Nothing,
        selectTop = NoTop,
        selectProjections,
        selectFrom = Just selectFrom,
        selectJoins = mapMaybe fieldSourceJoin $ pure fieldSource,
        selectWhere = mempty,
        selectFor =
          JsonFor ForJson {jsonCardinality = JsonArray, jsonRoot = NoRoot},
        selectOffset = Nothing
      }
  where
    projectArgumentId column =
      ExpressionProjection
        $ Aliased
          { aliasedThing = column,
            aliasedAlias = IR.getFieldNameTxt argumentId
          }
    selectFrom =
      FromOpenJson
        Aliased
          { aliasedThing =
              OpenJson
                { openJsonExpression =
                    ValueExpression (ODBC.TextValue $ lbsToTxt $ J.encode lhs),
                  openJsonWith =
                    Just
                      $ toJsonFieldSpec argumentId IntegerType
                      NE.:| map (uncurry toJsonFieldSpec . second snd) (HashMap.toList lhsSchema)
                },
            aliasedAlias = "lhs"
          }

    joinColumns = mapKeys coerceToColumn lhsSchema

    coerceToColumn = ColumnName . IR.getFieldNameTxt

    toJsonFieldSpec (IR.FieldName lhsFieldName) scalarType =
      ScalarField scalarType DataLengthMax lhsFieldName (Just $ FieldPath RootPath lhsFieldName)

-- | Build the 'FieldSource' for the relation field, depending on whether it's
-- an object, array, or aggregate relationship.
fromRemoteRelationFieldsG ::
  Map (Either NativeQueryName TableName) EntityAlias ->
  HashMap.HashMap ColumnName ColumnName ->
  (IR.FieldName, IR.SourceRelationshipSelection 'MSSQL Void (Const Expression)) ->
  ReaderT EntityAlias FromIr FieldSource
fromRemoteRelationFieldsG existingJoins joinColumns (IR.FieldName name, field) =
  case field of
    IR.SourceRelationshipObject selectionSet ->
      fmap
        ( \aliasedThing ->
            JoinFieldSource JsonSingleton (Aliased {aliasedThing, aliasedAlias = name})
        )
        ( fromObjectRelationSelectG
            existingJoins
            ( withJoinColumns
                $ runIdentity
                $ traverse (Identity . getConst) selectionSet
            )
        )
    IR.SourceRelationshipArray selectionSet ->
      fmap
        ( \aliasedThing ->
            JoinFieldSource JsonArray (Aliased {aliasedThing, aliasedAlias = name})
        )
        ( fromArraySelectG
            ( IR.ASSimple
                $ withJoinColumns
                $ runIdentity
                $ traverse (Identity . getConst) selectionSet
            )
        )
    IR.SourceRelationshipArrayAggregate selectionSet ->
      fmap
        ( \aliasedThing ->
            JoinFieldSource JsonArray (Aliased {aliasedThing, aliasedAlias = name})
        )
        ( fromArraySelectG
            ( IR.ASAggregate
                $ withJoinColumns
                $ runIdentity
                $ traverse (Identity . getConst) selectionSet
            )
        )
  where
    withJoinColumns ::
      s -> IR.AnnRelationSelectG 'MSSQL s
    withJoinColumns annotatedRelationship =
      IR.AnnRelationSelectG
        (IR.RelName $ mkNonEmptyTextUnsafe name)
        joinColumns
        IR.Nullable
        annotatedRelationship

-- | Top/root-level 'Select'. All descendent/sub-translations are collected to produce a root TSQL.Select.
fromSelectRows :: IR.AnnSelectG 'MSSQL (IR.AnnFieldG 'MSSQL Void) Expression -> FromIr TSQL.Select
fromSelectRows annSelectG = do
  selectFrom <-
    case from of
      IR.FromTable qualifiedObject -> fromQualifiedTable qualifiedObject
      IR.FromIdentifier identifier -> pure $ FromIdentifier $ IR.unFIIdentifier identifier
      IR.FromFunction {} -> refute $ pure FunctionNotSupported
      IR.FromNativeQuery nativeQuery -> fromNativeQuery nativeQuery
      IR.FromStoredProcedure storedProcedure -> fromStoredProcedure storedProcedure
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
      (traverse (fromAnnFieldsG argsExistingJoins) fields)
      (fromAlias selectFrom)
  filterExpression <-
    runReaderT (fromGBoolExp permFilter) (fromAlias selectFrom)
  let selectProjections = map fieldSourceProjections fieldSources

  pure
    $ emptySelect
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
        _asnNamingConvention = _tCase
      } = annSelectG
    IR.TablePerm {_tpLimit = mPermLimit, _tpFilter = permFilter} = perm
    permissionBasedTop =
      maybe NoTop Top mPermLimit

mkNodesSelect :: Args -> Where -> Expression -> Top -> From -> [(Int, (IR.FieldName, [FieldSource]))] -> [(Int, Projection)]
mkNodesSelect Args {..} foreignKeyConditions filterExpression permissionBasedTop selectFrom nodes =
  [ ( index,
      ExpressionProjection
        $ Aliased
          { aliasedThing =
              SelectExpression
                $ emptySelect
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
      ExpressionProjection
        $ Aliased
          { aliasedThing =
              safeJsonQueryExpression JsonSingleton
                $ SelectExpression
                $ emptySelect
                  { selectProjections = projections,
                    selectTop = NoTop,
                    selectFrom =
                      pure
                        $ FromSelect
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

fromNativeQuery :: IR.NativeQuery 'MSSQL Expression -> FromIr TSQL.From
fromNativeQuery nativeQuery = do
  let nativeQueryName = IR.nqRootFieldName nativeQuery
      nativeQuerySql = IR.nqInterpolatedQuery nativeQuery
  cteName <- tellCTE nativeQueryName nativeQuerySql
  pure $ TSQL.FromIdentifier cteName

fromStoredProcedure :: IR.StoredProcedure 'MSSQL Expression -> FromIr TSQL.From
fromStoredProcedure storedProcedure = do
  let storedProcedureName = "hasura_sp_" <> T.toTxt (IR.spGraphqlName storedProcedure)
      declares =
        map
          (\(arg, (typ, val)) -> Declare (getArgumentName arg) typ val)
          (HashMap.toList (IR.spArgs storedProcedure))
      sql =
        InterpolatedQuery
          $ IIText ("EXECUTE " <> T.toTxt (IR.spStoredProcedure storedProcedure) <> " ")
          : intercalate
            [IIText ", "]
            ( map
                ( \(ArgumentName name) ->
                    [ IIText "@",
                      IIText (T.toTxt name),
                      IIText " = ",
                      IIText "@",
                      IIText (T.toTxt name)
                    ]
                )
                (HashMap.keys (IR.spArgs storedProcedure))
            )
      storedProcedureReturnType = IR.spLogicalModel storedProcedure
      rawTempTableName = T.toTxt storedProcedureName
      aliasedTempTableName = Aliased (TempTableName rawTempTableName) rawTempTableName

      columns =
        ( \(name, ty) ->
            UnifiedColumn
              { name = name,
                type' = nstType ty
              }
        )
          <$> InsOrdHashMap.toList (columnsFromFields $ lmFields storedProcedureReturnType)

  -- \| add create temp table to "the environment"
  tellBefore (TempTableCreate (TempTableName rawTempTableName) columns)

  -- \| add insert into temp table
  tellBefore (TempTableInsert (TempTableName rawTempTableName) declares sql)

  -- \| when we're done, drop the temp table
  tellAfter (TempTableDrop (TempTableName rawTempTableName))

  pure $ TSQL.FromTempTable aliasedTempTableName

fromSelectAggregate ::
  Maybe (EntityAlias, HashMap ColumnName ColumnName) ->
  IR.AnnSelectG 'MSSQL (IR.TableAggregateFieldG 'MSSQL Void) Expression ->
  FromIr TSQL.Select
fromSelectAggregate
  mparentRelationship
  IR.AnnSelectG
    { _asnFields = (zip [0 ..] -> fields),
      _asnFrom = from,
      _asnPerm = IR.TablePerm {_tpLimit = (maybe NoTop Top -> permissionBasedTop), _tpFilter = permFilter},
      _asnArgs = args,
      _asnNamingConvention = _tCase
    } =
    do
      selectFrom <- case from of
        IR.FromTable qualifiedObject -> fromQualifiedTable qualifiedObject
        IR.FromIdentifier identifier -> pure $ FromIdentifier $ IR.unFIIdentifier identifier
        IR.FromFunction {} -> refute $ pure FunctionNotSupported
        IR.FromNativeQuery nativeQuery -> fromNativeQuery nativeQuery
        IR.FromStoredProcedure {} -> error "fromSelectAggregate: FromStoredProcedure"
      -- Below: When we're actually a RHS of a query (of CROSS APPLY),
      -- then we'll have a LHS table that we're joining on. So we get the
      -- conditions expressions from the field mappings. The LHS table is
      -- the entityAlias, and the RHS table is selectFrom.
      mforeignKeyConditions <- fmap (Where . fromMaybe [])
        $ for mparentRelationship
        $ \(entityAlias, mapping) ->
          runReaderT (fromMapping selectFrom mapping) entityAlias
      filterExpression <- runReaderT (fromGBoolExp permFilter) (fromAlias selectFrom)
      args'@Args {argsExistingJoins} <-
        runReaderT (fromSelectArgsG args) (fromAlias selectFrom)
      -- Although aggregates, exps and nodes could be handled in one list,
      -- we need to separately treat the subselect expressions
      expss :: [(Int, Projection)] <- flip runReaderT (fromAlias selectFrom) $ sequence $ mapMaybe fromTableExpFieldG fields
      nodes :: [(Int, (IR.FieldName, [FieldSource]))] <-
        flip runReaderT (fromAlias selectFrom) $ sequence $ mapMaybe (fromTableNodesFieldG argsExistingJoins) fields
      aggregates :: [(Int, (IR.FieldName, [Projection]))] <- flip runReaderT (EntityAlias aggSubselectName) $ sequence $ mapMaybe fromTableAggFieldG fields
      pure
        emptySelect
          { selectProjections =
              map snd
                $ sortBy (comparing fst)
                $ expss
                <> mkNodesSelect args' mforeignKeyConditions filterExpression permissionBasedTop selectFrom nodes
                <> mkAggregateSelect args' mforeignKeyConditions filterExpression selectFrom aggregates,
            selectTop = NoTop,
            selectFrom =
              pure
                $ FromOpenJson
                $ Aliased
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
    argsExistingJoins :: Map (Either NativeQueryName TableName) EntityAlias
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

--------------------------------------------------------------------------------
-- Conversion functions
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
  | JoinFieldSource JsonCardinality (Aliased Join)
  deriving (Eq, Show)

-- | Get FieldSource from a TAFExp type table aggregate field
fromTableExpFieldG :: -- TODO: Convert function to be similar to Nodes function
  (Int, (IR.FieldName, IR.TableAggregateFieldG 'MSSQL Void Expression)) ->
  Maybe (ReaderT EntityAlias FromIr (Int, Projection))
fromTableExpFieldG = \case
  (index, (IR.FieldName name, IR.TAFExp text)) ->
    Just
      $ pure
      $ ( index,
          fieldSourceProjections
            $ ExpressionFieldSource
              Aliased
                { aliasedThing = TSQL.ValueExpression (ODBC.TextValue text),
                  aliasedAlias = name
                }
        )
  _ -> Nothing

fromTableAggFieldG ::
  (Int, (IR.FieldName, IR.TableAggregateFieldG 'MSSQL Void Expression)) ->
  Maybe (ReaderT EntityAlias FromIr (Int, (IR.FieldName, [Projection])))
fromTableAggFieldG = \case
  (index, (fieldName, IR.TAFAgg (aggregateFields :: [(IR.FieldName, IR.AggregateField 'MSSQL Expression)]))) ->
    Just $ do
      aggregates <-
        forM aggregateFields \(fieldName', aggregateField) ->
          fromAggregateField (IR.getFieldNameTxt fieldName') aggregateField
      pure (index, (fieldName, aggregates))
  _ -> Nothing

fromTableNodesFieldG ::
  Map (Either NativeQueryName TableName) EntityAlias ->
  (Int, (IR.FieldName, IR.TableAggregateFieldG 'MSSQL Void Expression)) ->
  Maybe (ReaderT EntityAlias FromIr (Int, (IR.FieldName, [FieldSource])))
fromTableNodesFieldG argsExistingJoins = \case
  (index, (fieldName, IR.TAFNodes () (annFieldsG :: [(IR.FieldName, IR.AnnFieldG 'MSSQL Void Expression)]))) -> Just do
    fieldSources' <- fromAnnFieldsG argsExistingJoins `traverse` annFieldsG
    pure (index, (fieldName, fieldSources'))
  _ -> Nothing

fromAggregateField :: Text -> IR.AggregateField 'MSSQL Expression -> ReaderT EntityAlias FromIr Projection
fromAggregateField alias aggregateField =
  case aggregateField of
    IR.AFExp text -> pure $ AggregateProjection $ Aliased (TextAggregate text) alias
    IR.AFCount countType ->
      AggregateProjection . flip Aliased alias . CountAggregate <$> case getCountType countType of
        StarCountable -> pure StarCountable
        NonNullFieldCountable (name, redactionExp) -> do
          ex <- potentiallyRedacted redactionExp (ColumnExpression (columnFieldAggEntity name))
          pure $ NonNullFieldCountable ex
        DistinctCountable (name, redactionExp) -> do
          ex <- potentiallyRedacted redactionExp (ColumnExpression (columnFieldAggEntity name))
          pure $ DistinctCountable ex
    IR.AFOp IR.AggregateOp {_aoOp = op, _aoFields = fields} -> do
      projections :: [Projection] <- forM fields \(fieldName, columnField) ->
        case columnField of
          IR.SFCol column _columnType redactionExp -> do
            let fname = columnFieldAggEntity column
            colExp <- potentiallyRedacted redactionExp (ColumnExpression fname)
            pure $ AggregateProjection $ Aliased (OpAggregate op [colExp]) (IR.getFieldNameTxt fieldName)
          IR.SFExp text ->
            pure $ ExpressionProjection $ Aliased (ValueExpression (ODBC.TextValue text)) (IR.getFieldNameTxt fieldName)
          -- See Hasura.RQL.Types.Backend.supportsAggregateComputedFields
          IR.SFComputedField _ _ -> error "Aggregate computed fields aren't currently supported for MSSQL!"
      pure
        $ ExpressionProjection
        $ flip Aliased alias
        $ safeJsonQueryExpression JsonSingleton
        $ SelectExpression
        $ emptySelect
          { selectProjections = projections,
            selectFor = JsonFor $ ForJson JsonSingleton NoRoot
          }
  where
    columnFieldAggEntity col = columnNameToFieldName col $ EntityAlias aggSubselectName

potentiallyRedacted :: IR.AnnRedactionExp 'MSSQL Expression -> Expression -> ReaderT EntityAlias FromIr Expression
potentiallyRedacted redactionExp ex = do
  case redactionExp of
    IR.NoRedaction -> pure ex
    IR.RedactIfFalse p -> do
      condExp <- fromGBoolExp p
      pure $ ConditionalExpression condExp ex (ValueExpression ODBC.NullValue)

-- | The main sources of fields, either constants, fields or via joins.
fromAnnFieldsG ::
  Map (Either NativeQueryName TableName) EntityAlias ->
  (IR.FieldName, IR.AnnFieldG 'MSSQL Void Expression) ->
  ReaderT EntityAlias FromIr FieldSource
fromAnnFieldsG existingJoins (IR.FieldName name, field) =
  case field of
    IR.AFColumn annColumnField -> do
      expression <- fromAnnColumnField annColumnField
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
            JoinFieldSource JsonSingleton (Aliased {aliasedThing, aliasedAlias = name})
        )
        (fromObjectRelationSelectG existingJoins objectRelationSelectG)
    IR.AFArrayRelation arraySelectG ->
      fmap
        ( \aliasedThing ->
            JoinFieldSource JsonArray (Aliased {aliasedThing, aliasedAlias = name})
        )
        (fromArraySelectG arraySelectG)

-- | Here is where we project a field as a column expression. If
-- number stringification is on, then we wrap it in a
-- 'ToStringExpression' so that it's casted when being projected.
fromAnnColumnField ::
  IR.AnnColumnField 'MSSQL Expression ->
  ReaderT EntityAlias FromIr Expression
fromAnnColumnField annColumnField = do
  fieldName <- fromColumn column
  -- TODO: Handle stringifying large numbers
  {-(IR.isScalarColumnWhere isBigNum typ && stringifyNumbers == IR.StringifyNumbers)-}

  -- for geometry and geography values, the automatic json encoding on sql
  -- server would fail. So we need to convert it to a format the json encoding
  -- handles. Ideally we want this representation to be GeoJSON but sql server
  -- doesn't have any functions to convert to GeoJSON format. So we return it in
  -- WKT format
  if typ == (IR.ColumnScalar GeometryType) || typ == (IR.ColumnScalar GeographyType)
    then pure $ MethodApplicationExpression (ColumnExpression fieldName) MethExpSTAsText
    else potentiallyRedacted redactionExp (ColumnExpression fieldName)
  where
    IR.AnnColumnField
      { _acfColumn = column,
        _acfType = typ,
        _acfAsText = _asText :: Bool,
        _acfArguments = _ :: Maybe Void,
        _acfRedactionExpression = redactionExp
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
    JoinFieldSource cardinality aliasedJoin ->
      ExpressionProjection
        ( aliasedJoin
            { aliasedThing =
                -- Basically a cast, to ensure that SQL Server won't
                -- double-encode the JSON but will "pass it through"
                -- untouched.
                safeJsonQueryExpression
                  cardinality
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
    JoinFieldSource _ aliasedJoin -> pure (aliasedThing aliasedJoin)
    ExpressionFieldSource {} -> Nothing

--------------------------------------------------------------------------------
-- Joins

fromObjectRelationSelectG ::
  Map (Either NativeQueryName TableName) EntityAlias ->
  IR.ObjectRelationSelectG 'MSSQL Void Expression ->
  ReaderT EntityAlias FromIr Join
fromObjectRelationSelectG existingJoins annRelationSelectG = do
  eitherAliasOrFrom <-
    case target of
      IR.FromTable t -> lift (lookupTableFrom existingJoins t)
      IR.FromNativeQuery q -> lift (lookupNativeQueryFrom existingJoins q)
      other -> error $ "fromObjectRelationSelectG: " <> show other
  let entityAlias :: EntityAlias = either id fromAlias eitherAliasOrFrom
  fieldSources <-
    local
      (const entityAlias)
      (traverse (fromAnnFieldsG mempty) fields)
  let selectProjections = map fieldSourceProjections fieldSources

  joinJoinAlias <-
    do
      fieldName <- lift (fromRelName _aarRelationshipName)
      alias <- lift (generateAlias (ObjectRelationTemplate fieldName))
      pure
        JoinAlias
          { joinAliasEntity = alias,
            joinAliasField = pure jsonFieldName
          }

  let selectFor =
        JsonFor ForJson {jsonCardinality = JsonSingleton, jsonRoot = NoRoot}

  filterExpression <- local (const entityAlias) (fromGBoolExp tableFilter)

  -- if the object select should be non-nullable we push an extra 'where'
  -- for the outer `select` that checks the value is not `null`
  let joinWhere = case nullable of
        IR.Nullable -> mempty
        IR.NotNullable ->
          Where
            [ IsNotNullExpression
                ( JsonQueryExpression
                    ( ColumnExpression
                        ( FieldName
                            { fieldName = jsonFieldName,
                              fieldNameEntity = joinAliasEntity joinJoinAlias
                            }
                        )
                    )
                )
            ]

  case eitherAliasOrFrom of
    Right selectFrom -> do
      foreignKeyConditions <- fromMapping selectFrom mapping
      pure
        Join
          { joinJoinAlias,
            joinWhere,
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
            joinWhere,
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
        _aosTarget = target :: IR.SelectFromG 'MSSQL Expression,
        _aosTargetFilter = tableFilter :: IR.AnnBoolExp 'MSSQL Expression
      } = annObjectSelectG
    IR.AnnRelationSelectG
      { _aarRelationshipName,
        _aarColumnMapping = mapping :: HashMap ColumnName ColumnName,
        _aarAnnSelect = annObjectSelectG :: IR.AnnObjectSelectG 'MSSQL Void Expression,
        _aarNullable = nullable
      } = annRelationSelectG

lookupTableFrom ::
  Map (Either NativeQueryName TableName) EntityAlias ->
  TableName ->
  FromIr (Either EntityAlias From)
lookupTableFrom existingJoins tableFrom = do
  case M.lookup (Right tableFrom) existingJoins of
    Just entityAlias -> pure (Left entityAlias)
    Nothing -> fmap Right (fromQualifiedTable tableFrom)

lookupNativeQueryFrom ::
  Map (Either NativeQueryName TableName) EntityAlias ->
  IR.NativeQuery 'MSSQL Expression ->
  FromIr (Either EntityAlias From)
lookupNativeQueryFrom existingJoins nativeQueryFrom = do
  case M.lookup (Left (IR.nqRootFieldName nativeQueryFrom)) existingJoins of
    Just entityAlias -> pure (Left entityAlias)
    Nothing -> fmap Right (fromNativeQuery nativeQueryFrom)

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
  fieldName <- lift (fromRelName _aarRelationshipName)
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
        joinWhere = mempty,
        joinSource = JoinSelect joinSelect
      }
  where
    IR.AnnRelationSelectG
      { _aarRelationshipName,
        _aarColumnMapping = mapping :: HashMap ColumnName ColumnName,
        _aarAnnSelect = annSelectG
      } = annRelationSelectG

fromArrayRelationSelectG :: IR.ArrayRelationSelectG 'MSSQL Void Expression -> ReaderT EntityAlias FromIr Join
fromArrayRelationSelectG annRelationSelectG = do
  fieldName <- lift (fromRelName _aarRelationshipName)
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
        joinWhere = mempty,
        joinSource = JoinSelect joinSelect
      }
  where
    IR.AnnRelationSelectG
      { _aarRelationshipName,
        _aarColumnMapping = mapping :: HashMap ColumnName ColumnName,
        _aarAnnSelect = annSelectG
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
    . HashMap.toList

selectFromMapping ::
  Select ->
  HashMap ColumnName ColumnName ->
  ReaderT EntityAlias FromIr [Expression]
selectFromMapping Select {selectFrom = Nothing} = const (pure [])
selectFromMapping Select {selectFrom = Just from} = fromMapping from

-- | A version of @JSON_QUERY(..)@ that returns a proper json literal, rather
-- than SQL null, which does not compose properly with @FOR JSON@ clauses.
safeJsonQueryExpression :: JsonCardinality -> Expression -> Expression
safeJsonQueryExpression expectedType jsonQuery =
  FunctionApplicationExpression (FunExpISNULL (JsonQueryExpression jsonQuery) jsonTypeExpression)
  where
    jsonTypeExpression = case expectedType of
      JsonSingleton -> nullExpression
      JsonArray -> emptyArrayExpression

--------------------------------------------------------------------------------
-- Constants

data UnfurledJoin = UnfurledJoin
  { unfurledJoin :: Join,
    -- | Recorded if we joined onto an object relation.
    unfurledObjectTableAlias :: Maybe (Either NativeQueryName TableName, EntityAlias)
  }
  deriving (Show)

fromAnnotatedOrderByItemG ::
  IR.AnnotatedOrderByItemG 'MSSQL Expression ->
  WriterT (Seq UnfurledJoin) (ReaderT EntityAlias FromIr) OrderBy
fromAnnotatedOrderByItemG IR.OrderByItemG {obiType, obiColumn = obiColumn, obiNulls} = do
  (orderByExpression, orderByType) <- unfurlAnnotatedOrderByElement obiColumn
  let orderByNullsOrder = fromMaybe NullsAnyOrder obiNulls
      orderByOrder = fromMaybe AscOrder obiType
  pure OrderBy {..}

-- | Unfurl the nested set of object relations (tell'd in the writer)
-- that are terminated by field name (IR.AOCColumn and
-- IR.AOCArrayAggregation).
unfurlAnnotatedOrderByElement ::
  IR.AnnotatedOrderByElement 'MSSQL Expression ->
  WriterT (Seq UnfurledJoin) (ReaderT EntityAlias FromIr) (Expression, Maybe TSQL.ScalarType)
unfurlAnnotatedOrderByElement =
  \case
    IR.AOCColumn columnInfo redactionExp -> do
      fieldName <- lift (fromColumnInfo columnInfo)
      ex <- lift $ potentiallyRedacted redactionExp (ColumnExpression fieldName)
      pure
        ( ex,
          case IR.ciType columnInfo of
            IR.ColumnScalar t -> Just t
            -- Above: It is of interest to us whether the type is
            -- text/ntext/image. See ToQuery for more explanation.
            _ -> Nothing
        )
    IR.AOCObjectRelation IR.RelInfo {riMapping = IR.RelMapping mapping, riTarget = IR.RelTargetNativeQuery nativeQueryName} annBoolExp annOrderByElementG -> do
      let name = T.toTxt (getNativeQueryName nativeQueryName)
          selectFrom = TSQL.FromIdentifier name
      joinAliasEntity <-
        lift (lift (generateAlias (ForOrderAlias name)))
      genObjectRelation mapping annBoolExp annOrderByElementG joinAliasEntity selectFrom (Left nativeQueryName)
    IR.AOCObjectRelation IR.RelInfo {riMapping = IR.RelMapping mapping, riTarget = IR.RelTargetTable table} annBoolExp annOrderByElementG -> do
      selectFrom <- lift (lift (fromQualifiedTable table))
      joinAliasEntity <-
        lift (lift (generateAlias (ForOrderAlias (tableNameText table))))
      genObjectRelation mapping annBoolExp annOrderByElementG joinAliasEntity selectFrom (Right table)
    IR.AOCArrayAggregation IR.RelInfo {riTarget = IR.RelTargetNativeQuery _} _annBoolExp _annAggregateOrderBy ->
      error "unfurlAnnotatedOrderByElement RelTargetNativeQuery"
    IR.AOCArrayAggregation IR.RelInfo {riMapping = IR.RelMapping mapping, riTarget = IR.RelTargetTable tableName} annBoolExp annAggregateOrderBy -> do
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
                  IR.AAOOp (IR.AggregateOrderByColumn text _resultType columnInfo redactionExp) -> do
                    fieldName <- fromColumnInfo columnInfo
                    ex <- potentiallyRedacted redactionExp (ColumnExpression fieldName)
                    pure (OpAggregate text (pure ex))
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
                        joinWhere = mempty,
                        joinJoinAlias =
                          JoinAlias {joinAliasEntity, joinAliasField = Nothing}
                      },
                  unfurledObjectTableAlias = Nothing
                }
            )
        )
      pure
        ( ColumnExpression $ FieldName {fieldNameEntity = joinAliasEntity, fieldName = alias},
          Nothing
        )
  where
    genObjectRelation mapping annBoolExp annOrderByElementG joinAliasEntity selectFrom table = do
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
                      joinWhere = mempty,
                      joinJoinAlias =
                        JoinAlias {joinAliasEntity, joinAliasField = Nothing}
                    },
                unfurledObjectTableAlias = Just (table, EntityAlias joinAliasEntity)
              }
        )
      local
        (const (EntityAlias joinAliasEntity))
        (unfurlAnnotatedOrderByElement annOrderByElementG)

tableNameText :: TableName -> Text
tableNameText (TableName {tableName}) = tableName

fromColumnInfo :: IR.ColumnInfo 'MSSQL -> ReaderT EntityAlias FromIr FieldName
fromColumnInfo IR.ColumnInfo {ciColumn = column} =
  columnNameToFieldName column <$> ask
