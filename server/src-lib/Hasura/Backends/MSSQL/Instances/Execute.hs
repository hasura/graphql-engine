{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | MSSQL Instances Execute
--
-- Defines a 'BackendExecute' type class instance for MSSQL.
--
-- This module implements the needed functionality for implementing a 'BackendExecute'
-- instance for MSSQL, which defines an interface for translating a root field into an execution plan
-- and interacting with a database.
--
-- This module includes the MSSQL implementation of queries, mutations, and more.
module Hasura.Backends.MSSQL.Instances.Execute
  ( MultiplexedQuery' (..),
    multiplexRootReselect,
  )
where

import Control.Exception.Lifted (bracket_)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson.Extended qualified as J
import Data.Environment qualified as Env
import Data.HashMap.Strict qualified as HashMap
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.HashSet qualified as Set
import Data.List.NonEmpty qualified as NE
import Data.Text.Extended
import Data.Text.Extended qualified as T
import Database.MSSQL.Transaction qualified as Tx
import Database.ODBC.SQLServer qualified as ODBC
import Hasura.Backends.MSSQL.Connection
import Hasura.Backends.MSSQL.Execute.Delete
import Hasura.Backends.MSSQL.Execute.Insert
import Hasura.Backends.MSSQL.Execute.QueryTags
import Hasura.Backends.MSSQL.Execute.Update
import Hasura.Backends.MSSQL.FromIr.Constants (jsonFieldName)
import Hasura.Backends.MSSQL.Plan
import Hasura.Backends.MSSQL.SQL.Error
import Hasura.Backends.MSSQL.SQL.Value (txtEncodedColVal)
import Hasura.Backends.MSSQL.ToQuery as TQ
import Hasura.Backends.MSSQL.Types.Internal as TSQL
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.GraphQL.Execute.Backend
import Hasura.GraphQL.Execute.Subscription.Plan
import Hasura.GraphQL.Namespace (RootFieldAlias (..), RootFieldMap)
import Hasura.GraphQL.Parser.Variable qualified as G
import Hasura.Logging qualified as L
import Hasura.Prelude
import Hasura.QueryTags (QueryTagsComment)
import Hasura.RQL.IR
import Hasura.RQL.IR.ModelInformation
import Hasura.RQL.Types.Backend as RQLTypes
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Column qualified as RQLColumn
import Hasura.RQL.Types.Common as RQLTypes
import Hasura.RQL.Types.Schema.Options qualified as Options
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.Session
import Language.GraphQL.Draft.Syntax qualified as G
import Network.HTTP.Client as HTTP
import Network.HTTP.Types qualified as HTTP

instance BackendExecute 'MSSQL where
  type PreparedQuery 'MSSQL = Text
  type MultiplexedQuery 'MSSQL = MultiplexedQuery'
  type ExecutionMonad 'MSSQL = ExceptT QErr

  mkDBQueryPlan = msDBQueryPlan
  mkDBMutationPlan = msDBMutationPlan
  mkLiveQuerySubscriptionPlan = msDBLiveQuerySubscriptionPlan
  mkDBStreamingSubscriptionPlan _ _ _ _ _ _ = throw500 "Streaming subscriptions are not supported for MS-SQL sources yet"
  mkDBQueryExplain = msDBQueryExplain
  mkSubscriptionExplain = msDBSubscriptionExplain

  mkDBRemoteRelationshipPlan =
    msDBRemoteRelationshipPlan

-- * Multiplexed query

data MultiplexedQuery' = MultiplexedQuery'
  { reselect :: Reselect,
    subscriptionQueryTagsComment :: QueryTagsComment
  }

instance T.ToTxt MultiplexedQuery' where
  toTxt (MultiplexedQuery' reselect queryTags) =
    T.toTxt $ toQueryPretty (fromReselect reselect) `withQueryTags` queryTags

-- * Query

msDBQueryPlan ::
  forall m.
  ( MonadError QErr m,
    MonadReader QueryTagsComment m
  ) =>
  UserInfo ->
  SourceName ->
  SourceConfig 'MSSQL ->
  QueryDB 'MSSQL Void (UnpreparedValue 'MSSQL) ->
  [HTTP.Header] ->
  Maybe G.Name ->
  m (DBStepInfo 'MSSQL, [ModelInfoPart])
msDBQueryPlan userInfo sourceName sourceConfig qrf _ _ = do
  let sessionVariables = _uiSession userInfo
  QueryWithDDL {qwdBeforeSteps, qwdAfterSteps, qwdQuery = statement} <- planQuery sessionVariables qrf
  queryTags <- ask

  -- Append Query tags comment to the select statement
  let printer = fromSelect statement `withQueryTagsPrinter` queryTags
      queryString = ODBC.renderQuery (toQueryPretty printer)
  modelNames <- irToModelInfoGen sourceName ModelSourceTypeMSSQL qrf
  let modelInfo = getModelInfoPartfromModelNames modelNames (ModelOperationType G.OperationTypeQuery)

  pure $ (DBStepInfo @'MSSQL sourceName sourceConfig (Just queryString) (runSelectQuery printer qwdBeforeSteps qwdAfterSteps) (), modelInfo)
  where
    runSelectQuery queryPrinter beforeSteps afterSteps = OnBaseMonad do
      let queryTx = do
            let executeStep = Tx.unitQueryE defaultMSSQLTxErrorHandler . toQueryFlat . TQ.fromTempTableDDL
            traverse_ executeStep beforeSteps
            result <- encJFromText <$> Tx.singleRowQueryE defaultMSSQLTxErrorHandler (toQueryFlat queryPrinter)
            traverse_ executeStep afterSteps
            pure result
      mssqlRunReadOnly (_mscExecCtx sourceConfig) (fmap withNoStatistics queryTx)

-- Runs the query in "SHOWPLAN_TEXT" mode, which instead plans the query, but
-- does not execute it.
--
-- This does not work for prepared statements, so we have to use a different
-- translation strategy. We first convert the query to an unprepared style,
-- populating it with references to variables instead of the values themselves.
--
-- We then declare all these variables, but do not set their values, and run the
-- query in SHOWPLAN_TEXT mode. We have to do this as a single transaction, so
-- we concatenate them all together.
--
-- The variables are all declared as type `NVARCHAR(MAX)`, which seems to work
-- even when we need to use them in another context, e.g. an integer.
runShowplan ::
  (MonadIO m, MonadBaseControl IO m) =>
  ODBC.Query ->
  Tx.TxET QErr m [Text]
runShowplan query = Tx.withTxET defaultMSSQLTxErrorHandler do
  bracket_ setShowplanOn setShowplanOff
    . Tx.multiRowQuery
    . ODBC.rawUnescapedText
    $ mconcat paramDeclarations
    <> "\n-- QUERY START --\n"
    <> unparameterizedQueryWithVariables
    <> "\n-- QUERY END --\n"
  where
    setShowplanOn = Tx.unitQuery "SET SHOWPLAN_TEXT ON"
    setShowplanOff = Tx.unitQuery "SET SHOWPLAN_TEXT OFF"
    paramDeclarations =
      zipWith
        (\paramIndex paramType -> "DECLARE @" <> tshow paramIndex <> " " <> paramType <> ";\n")
        [1 :: Int ..]
        (reverse reversedParameterTypes)
    -- we build up the SQL and parameter type list with a counter so we can use
    -- a fresh variable for each parameter
    unparameterizedQueryWithVariables :: Text
    reversedParameterTypes :: [Text]
    (unparameterizedQueryWithVariables, _, reversedParameterTypes) =
      foldl
        ( \(text, paramIndex, reversedParamTypes) part -> case part of
            ODBC.TextPart t -> (text <> t, paramIndex, reversedParamTypes)
            ODBC.ValuePart v ->
              case paramType of
                Just t -> (text <> "@" <> tshow paramIndex, paramIndex + 1, t : reversedParamTypes)
                Nothing -> (text <> ODBC.renderValue v, paramIndex, reversedParamTypes)
              where
                -- Copying from the ODBC library, we only use parameters for
                -- a couple of types; everything else is inlined into the query.
                paramType = case v of
                  ODBC.TextValue {} -> Just "VARCHAR(MAX)"
                  ODBC.BinaryValue {} -> Just "VARBINARY(MAX)"
                  _ -> Nothing
        )
        ("", 1 :: Int, [])
        (ODBC.queryParts query)

msDBQueryExplain ::
  (MonadError QErr m) =>
  RootFieldAlias ->
  UserInfo ->
  SourceName ->
  SourceConfig 'MSSQL ->
  QueryDB 'MSSQL Void (UnpreparedValue 'MSSQL) ->
  [HTTP.Header] ->
  Maybe G.Name ->
  m (AB.AnyBackend DBStepInfo)
msDBQueryExplain fieldName userInfo sourceName sourceConfig qrf _ _ = do
  let sessionVariables = _uiSession userInfo
  queryPlan <- planQuery sessionVariables qrf
  select <-
    case queryPlan of
      QueryWithDDL [] s [] -> pure s
      _ -> throw400 NotSupported "queries which require multiple steps cannot be explained"
  let query = toQueryPretty (fromSelect select)
      queryString = ODBC.renderQuery query
      odbcQuery = OnBaseMonad
        $ mssqlRunReadOnly
          (_mscExecCtx sourceConfig)
          do
            showplan <- runShowplan query
            pure
              $ withNoStatistics
              $ encJFromJValue
              $ ExplainPlan
                fieldName
                (Just queryString)
                (Just showplan)
  pure
    $ AB.mkAnyBackend
    $ DBStepInfo @'MSSQL sourceName sourceConfig Nothing odbcQuery ()

msDBSubscriptionExplain ::
  (MonadIO m, MonadBaseControl IO m, MonadError QErr m) =>
  SubscriptionQueryPlan 'MSSQL (MultiplexedQuery 'MSSQL) ->
  m SubscriptionQueryPlanExplanation
msDBSubscriptionExplain (SubscriptionQueryPlan plan sourceConfig cohortId _dynamicConnection variables _) = do
  let (MultiplexedQuery' reselect _queryTags) = _plqpQuery plan
      query = toQueryPretty $ fromSelect $ multiplexRootReselect [(cohortId, variables)] reselect
      mssqlExecCtx = (_mscExecCtx sourceConfig)
  explainInfo <- liftEitherM $ runExceptT $ (mssqlRunReadOnly mssqlExecCtx) (runShowplan query)
  pure $ SubscriptionQueryPlanExplanation (T.toTxt query) explainInfo variables

-- | Producing the correct SQL-level list comprehension to multiplex a query
-- Problem description:
--
-- Generate a query that repeats the same query N times but with
-- certain slots replaced:
--
-- [ Select x y | (x,y) <- [..] ]
--
-- Caution: Be aware that this query has a @FOR JSON@ clause at the top-level
-- and hence its results may be split up across multiple rows. Use
-- 'Database.MSSQL.Transaction.forJsonQueryE' to handle this.
multiplexRootReselect ::
  [(CohortId, CohortVariables)] ->
  TSQL.Reselect ->
  TSQL.Select
multiplexRootReselect variables rootReselect =
  emptySelect
    { selectTop = NoTop,
      selectProjections =
        [ FieldNameProjection
            Aliased
              { aliasedThing =
                  TSQL.FieldName
                    { fieldNameEntity = rowAlias,
                      fieldName = resultIdAlias
                    },
                aliasedAlias = resultIdAlias
              },
          ExpressionProjection
            Aliased
              { aliasedThing =
                  ColumnExpression
                    ( TSQL.FieldName
                        { fieldNameEntity = resultAlias,
                          fieldName = jsonFieldName
                        }
                    ),
                aliasedAlias = resultAlias
              }
        ],
      selectFrom =
        Just
          $ FromOpenJson
            Aliased
              { aliasedThing =
                  OpenJson
                    { openJsonExpression =
                        ValueExpression (ODBC.TextValue $ lbsToTxt $ J.encode variables),
                      openJsonWith =
                        Just
                          $ NE.fromList
                            [ ScalarField GuidType DataLengthUnspecified resultIdAlias (Just $ IndexPath RootPath 0),
                              JsonField resultVarsAlias (Just $ IndexPath RootPath 1)
                            ]
                    },
                aliasedAlias = rowAlias
              },
      selectJoins =
        [ Join
            { joinSource = JoinReselect rootReselect,
              joinWhere = mempty,
              joinJoinAlias =
                JoinAlias
                  { joinAliasEntity = resultAlias,
                    joinAliasField = Just jsonFieldName
                  }
            }
        ],
      selectWhere = Where mempty,
      selectFor =
        JsonFor ForJson {jsonCardinality = JsonArray, jsonRoot = NoRoot},
      selectOrderBy = Nothing,
      selectOffset = Nothing
    }

-- * Mutation

msDBMutationPlan ::
  forall m.
  ( MonadError QErr m,
    MonadReader QueryTagsComment m
  ) =>
  Env.Environment ->
  HTTP.Manager ->
  L.Logger L.Hasura ->
  UserInfo ->
  Options.StringifyNumbers ->
  SourceName ->
  SourceConfig 'MSSQL ->
  MutationDB 'MSSQL Void (UnpreparedValue 'MSSQL) ->
  [HTTP.Header] ->
  Maybe G.Name ->
  Maybe (HashMap G.Name (G.Value G.Variable)) ->
  m (DBStepInfo 'MSSQL, [ModelInfoPart])
msDBMutationPlan _env _manager _logger userInfo stringifyNum sourceName sourceConfig mrf _headers _gName _maybeSelSetArgs = do
  go <$> case mrf of
    MDBInsert annInsert -> executeInsert userInfo stringifyNum sourceName ModelSourceTypeMSSQL sourceConfig annInsert
    MDBDelete annDelete -> executeDelete userInfo stringifyNum sourceName ModelSourceTypeMSSQL sourceConfig annDelete
    MDBUpdate annUpdate -> executeUpdate userInfo stringifyNum sourceName ModelSourceTypeMSSQL sourceConfig annUpdate
    MDBFunction {} -> throw400 NotSupported "function mutations are not supported in MSSQL"
  where
    modelInfoList v = getModelInfoPartfromModelNames (snd v) (ModelOperationType G.OperationTypeMutation)

    go v = (DBStepInfo @'MSSQL sourceName sourceConfig Nothing (fmap withNoStatistics (fst v)) (), modelInfoList v)

-- * Subscription

msDBLiveQuerySubscriptionPlan ::
  forall m.
  ( MonadError QErr m,
    MonadIO m,
    MonadBaseControl IO m,
    MonadReader QueryTagsComment m
  ) =>
  UserInfo ->
  SourceName ->
  SourceConfig 'MSSQL ->
  Maybe G.Name ->
  RootFieldMap (QueryDB 'MSSQL Void (UnpreparedValue 'MSSQL)) ->
  [HTTP.Header] ->
  Maybe G.Name ->
  m (SubscriptionQueryPlan 'MSSQL (MultiplexedQuery 'MSSQL), [ModelInfoPart])
msDBLiveQuerySubscriptionPlan UserInfo {_uiSession, _uiRole} sourceName sourceConfig namespace rootFields _ _ = do
  (reselect, prepareState) <- planSubscription (InsOrdHashMap.mapKeys _rfaAlias rootFields) _uiSession
  cohortVariables <- prepareStateCohortVariables sourceConfig _uiSession prepareState
  queryTags <- ask
  let parameterizedPlan = ParameterizedSubscriptionQueryPlan _uiRole $ (MultiplexedQuery' reselect queryTags)
  modelNameInfo <- do
    let vals = InsOrdHashMap.elems rootFields
    pure
      $ concatMap
        ( \val -> do
            join (irToModelInfoGen sourceName ModelSourceTypeMSSQL) val
        )
        vals

  let modelInfo = getModelInfoPartfromModelNames modelNameInfo (ModelOperationType G.OperationTypeSubscription)

  pure
    $ (SubscriptionQueryPlan parameterizedPlan sourceConfig dummyCohortId () cohortVariables namespace, modelInfo)

prepareStateCohortVariables ::
  (MonadError QErr m, MonadIO m, MonadBaseControl IO m) =>
  SourceConfig 'MSSQL ->
  SessionVariables ->
  PrepareState ->
  m CohortVariables
prepareStateCohortVariables sourceConfig session prepState = do
  (namedVars, posVars) <- validateVariables sourceConfig session prepState
  let PrepareState {sessionVariables} = prepState
  pure
    $ mkCohortVariables
      sessionVariables
      session
      namedVars
      posVars
      mempty -- streaming cursor variables are kept empty because streaming subscriptions aren't yet supported for MS-SQL

-- | Ensure that the set of variables (with value instantiations) that occur in
-- a (RQL) query produce a well-formed and executable (SQL) query when
-- considered in isolation.
--
-- This helps avoiding cascading failures in multiplexed queries.
--
-- c.f. https://github.com/hasura/graphql-engine-mono/issues/1210.
validateVariables ::
  (MonadError QErr m, MonadIO m, MonadBaseControl IO m) =>
  SourceConfig 'MSSQL ->
  SessionVariables ->
  PrepareState ->
  m (ValidatedQueryVariables, ValidatedSyntheticVariables)
validateVariables sourceConfig sessionVariableValues prepState = do
  let PrepareState {sessionVariables, namedArguments, positionalArguments} = prepState

      -- We generate a single 'canary' query in the form:
      --
      -- SELECT ... [session].[x-hasura-foo] as [x-hasura-foo], ... as a, ... as b, ...
      -- FROM OPENJSON('...')
      -- WITH ([x-hasura-foo] NVARCHAR(MAX)) as [session]
      --
      -- where 'a', 'b', etc. are aliases given to positional arguments.
      -- Named arguments and session variables are aliased to themselves.
      --
      -- The idea being that if the canary query succeeds we can be
      -- reasonably confident that adding these variables to a query being
      -- polled will not crash the poller.

      occSessionVars =
        filterSessionVariables
          (\k _ -> Set.member k sessionVariables)
          sessionVariableValues

      expSes, expNamed, expPos :: [Aliased Expression]
      expSes = sessionReference <$> getSessionVariables occSessionVars
      expNamed =
        map
          ( \(n, v) -> Aliased (ValueExpression (RQLColumn.cvValue v)) (G.unName n)
          )
          $ HashMap.toList
          $ namedArguments

      -- For positional args we need to be a bit careful not to capture names
      -- from expNamed and expSes (however unlikely)
      expPos =
        zipWith
          (\n v -> Aliased (ValueExpression (RQLColumn.cvValue v)) n)
          (freshVars (expNamed <> expSes))
          positionalArguments

      projAll :: [Projection]
      projAll = map ExpressionProjection (expSes <> expNamed <> expPos)

      canaryQuery =
        if null projAll
          then Nothing
          else
            Just
              $ renderQuery
                emptySelect
                  { selectProjections = projAll,
                    selectFrom = sessionOpenJson occSessionVars
                  }

  for_
    canaryQuery
    ( \q -> do
        _ :: [[ODBC.Value]] <- liftEitherM $ runExceptT $ mssqlRunReadOnly (_mscExecCtx sourceConfig) (Tx.multiRowQueryE defaultMSSQLTxErrorHandler q)
        pure ()
    )

  pure
    ( ValidatedVariables $ txtEncodedColVal <$> namedArguments,
      ValidatedVariables $ txtEncodedColVal <$> positionalArguments
    )
  where
    renderQuery :: Select -> ODBC.Query
    renderQuery = toQueryFlat . fromSelect

    freshVars :: [Aliased a] -> [Text]
    freshVars boundNames = filter (not . (`elem` map aliasedAlias boundNames)) chars

    -- Infinite list of expression aliases.
    chars :: [Text]
    chars = [y T.<>> x | y <- [""] <|> chars, x <- ['a' .. 'z']]

    sessionOpenJson :: SessionVariables -> Maybe From
    sessionOpenJson occSessionVars =
      nonEmpty (getSessionVariables occSessionVars)
        <&> \fields ->
          FromOpenJson
            $ Aliased
              ( OpenJson
                  (ValueExpression $ ODBC.TextValue $ lbsToTxt $ J.encode occSessionVars)
                  (pure (sessField <$> fields))
              )
              "session"

    sessField :: Text -> JsonFieldSpec
    sessField var = StringField var Nothing

    sessionReference :: Text -> Aliased Expression
    sessionReference var = Aliased (ColumnExpression (TSQL.FieldName var "session")) var

-- * Remote Relationships (e.g. DB-to-DB Joins, remote schema joins, etc.)

-- | Construct an action (i.e. 'DBStepInfo') which can marshal some remote
-- relationship information into a form that SQL Server can query against.
--
-- XXX: Currently unimplemented; the Postgres implementation uses
-- @jsonb_to_recordset@ to query the remote relationship, however this
-- functionality doesn't exist in SQL Server.
--
-- NOTE: The following typeclass constraints will be necessary when implementing
-- this function for real:
--
-- @
--   MonadQueryTags m
--   Backend 'MSSQL
-- @
msDBRemoteRelationshipPlan ::
  forall m.
  ( MonadError QErr m
  ) =>
  UserInfo ->
  SourceName ->
  SourceConfig 'MSSQL ->
  -- | List of json objects, each of which becomes a row of the table.
  NonEmpty J.Object ->
  -- | The above objects have this schema
  --
  -- XXX: What is this for/what does this mean?
  HashMap RQLTypes.FieldName (RQLTypes.Column 'MSSQL, RQLTypes.ScalarType 'MSSQL) ->
  -- | This is a field name from the lhs that *has* to be selected in the
  -- response along with the relationship.
  RQLTypes.FieldName ->
  (RQLTypes.FieldName, SourceRelationshipSelection 'MSSQL Void UnpreparedValue) ->
  [HTTP.Header] ->
  Maybe G.Name ->
  Options.StringifyNumbers ->
  m (DBStepInfo 'MSSQL, [ModelInfoPart])
msDBRemoteRelationshipPlan userInfo sourceName sourceConfig lhs lhsSchema argumentId relationship _headers _gName _stringifyNumbers = do
  -- `stringifyNumbers` is not currently handled in any SQL Server operation
  statement <- planSourceRelationship (_uiSession userInfo) lhs lhsSchema argumentId relationship

  let printer = fromSelect statement
      queryString = ODBC.renderQuery $ toQueryPretty printer
      odbcQuery = runSelectQuery printer

  modelNames <- getRSModelInfoGen sourceName ModelSourceTypeMSSQL $ snd relationship
  let modelInfo = getModelInfoPartfromModelNames modelNames (ModelOperationType G.OperationTypeQuery)

  pure $ (DBStepInfo @'MSSQL sourceName sourceConfig (Just queryString) odbcQuery (), modelInfo)
  where
    runSelectQuery queryPrinter = OnBaseMonad do
      let queryTx = encJFromText <$> Tx.forJsonQueryE defaultMSSQLTxErrorHandler (toQueryFlat queryPrinter)
      mssqlRunReadOnly (_mscExecCtx sourceConfig) (fmap withNoStatistics queryTx)
