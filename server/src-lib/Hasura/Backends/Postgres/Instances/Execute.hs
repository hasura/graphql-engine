{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Postgres Instances Execute
--
-- This module implements the needed functionality for implementing a 'BackendExecute'
-- instance for Postgres, which defines an interface for translating a root field into
-- an execution plan and interacting with a database.
--
-- This module includes the Postgres implementation of queries, mutations, and more.
module Hasura.Backends.Postgres.Instances.Execute
  ( PreparedSql (..),
    pgDBQueryPlanSimple,
  )
where

import Control.Monad.Trans.Control qualified as MT
import Data.Aeson qualified as J
import Data.Environment qualified as Env
import Data.HashMap.Strict qualified as HashMap
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.IntMap qualified as IntMap
import Data.Sequence qualified as Seq
import Database.PG.Query qualified as PG
import Hasura.Backends.Postgres.Connection.MonadTx
import Hasura.Backends.Postgres.Execute.ConnectionTemplate (QueryContext (..), QueryOperationType (..))
import Hasura.Backends.Postgres.Execute.Insert (convertToSQLTransaction, validateInsertInput, validateInsertRows)
import Hasura.Backends.Postgres.Execute.Mutation qualified as PGE
import Hasura.Backends.Postgres.Execute.Prepare
  ( PlanningSt (..),
    PrepArgMap,
    initPlanningSt,
    prepareWithPlan,
    prepareWithoutPlan,
    withUserVars,
  )
import Hasura.Backends.Postgres.Execute.Subscription qualified as PGL
import Hasura.Backends.Postgres.Execute.Types
import Hasura.Backends.Postgres.SQL.DML qualified as S
import Hasura.Backends.Postgres.SQL.Types qualified as Postgres
import Hasura.Backends.Postgres.SQL.Value qualified as Postgres
import Hasura.Backends.Postgres.Translate.Select (PostgresTranslateSelect)
import Hasura.Backends.Postgres.Translate.Select qualified as DS
import Hasura.Backends.Postgres.Types.Function qualified as Postgres
import Hasura.Backends.Postgres.Types.Update qualified as Postgres
import Hasura.Base.Error (QErr)
import Hasura.EncJSON (EncJSON, encJFromJValue)
import Hasura.Function.Cache
import Hasura.GraphQL.Execute.Backend
  ( BackendExecute (..),
    DBStepInfo (..),
    ExplainPlan (..),
    OnBaseMonad (..),
    convertRemoteSourceRelationship,
    withNoStatistics,
  )
import Hasura.GraphQL.Execute.Subscription.Plan
  ( CohortId,
    CohortVariables,
    ParameterizedSubscriptionQueryPlan (..),
    SubscriptionQueryPlan (..),
    SubscriptionQueryPlanExplanation (..),
    mkCohortVariables,
    newCohortId,
  )
import Hasura.GraphQL.Namespace
  ( RootFieldAlias (..),
    RootFieldMap,
  )
import Hasura.GraphQL.Namespace qualified as G
import Hasura.GraphQL.Parser.Variable qualified as G
import Hasura.Logging qualified as L
import Hasura.Prelude
import Hasura.QueryTags
  ( QueryTagsComment (..),
    emptyQueryTagsComment,
  )
import Hasura.RQL.IR
import Hasura.RQL.IR qualified as IR
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Column
  ( ColumnType (..),
    ColumnValue (..),
    ciName,
  )
import Hasura.RQL.Types.Common
  ( FieldName (..),
    JsonAggSelect (..),
    SourceName,
  )
import Hasura.RQL.Types.Permission (ValidateInput (..), ValidateInputHttpDefinition (..))
import Hasura.RQL.Types.Schema.Options qualified as Options
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.Session (UserInfo (..))
import Hasura.Tracing qualified as Tracing
import Language.GraphQL.Draft.Syntax qualified as G
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types qualified as HTTP

data PreparedSql = PreparedSql
  { _psQuery :: PG.Query,
    _psPrepArgs :: PrepArgMap
  }
  deriving (Show)

instance
  ( Backend ('Postgres pgKind),
    PostgresTranslateSelect pgKind
  ) =>
  BackendExecute ('Postgres pgKind)
  where
  type PreparedQuery ('Postgres pgKind) = PreparedSql
  type MultiplexedQuery ('Postgres pgKind) = PGL.MultiplexedQuery
  type ExecutionMonad ('Postgres pgKind) = PG.TxET QErr

  mkDBQueryPlan = pgDBQueryPlan
  mkDBMutationPlan = pgDBMutationPlan
  mkLiveQuerySubscriptionPlan = pgDBLiveQuerySubscriptionPlan
  mkDBStreamingSubscriptionPlan = pgDBStreamingSubscriptionPlan
  mkDBQueryExplain = pgDBQueryExplain
  mkSubscriptionExplain = pgDBSubscriptionExplain
  mkDBRemoteRelationshipPlan = pgDBRemoteRelationshipPlan

-- query

pgDBQueryPlan ::
  forall pgKind m.
  ( MonadError QErr m,
    Backend ('Postgres pgKind),
    PostgresTranslateSelect pgKind,
    MonadReader QueryTagsComment m
  ) =>
  UserInfo ->
  SourceName ->
  SourceConfig ('Postgres pgKind) ->
  QueryDB ('Postgres pgKind) Void (UnpreparedValue ('Postgres pgKind)) ->
  [HTTP.Header] ->
  Maybe G.Name ->
  m (DBStepInfo ('Postgres pgKind))
pgDBQueryPlan userInfo sourceName sourceConfig qrf reqHeaders operationName = do
  (preparedQuery, PlanningSt {_psPrepped = planVals}) <-
    flip runStateT initPlanningSt $ traverse (prepareWithPlan userInfo) qrf

  queryTagsComment <- ask
  resolvedConnectionTemplate <-
    let connectionTemplateResolver =
          connectionTemplateConfigResolver (_pscConnectionTemplateConfig sourceConfig)
        queryContext =
          Just
            $ QueryContext operationName
            $ QueryOperationType G.OperationTypeQuery
     in applyConnectionTemplateResolverNonAdmin connectionTemplateResolver userInfo reqHeaders queryContext
  let preparedSQLWithQueryTags = appendPreparedSQLWithQueryTags (irToRootFieldPlan planVals preparedQuery) queryTagsComment
  let (action, preparedSQL) = mkCurPlanTx userInfo preparedSQLWithQueryTags

  pure $ DBStepInfo @('Postgres pgKind) sourceName sourceConfig preparedSQL (fmap withNoStatistics action) resolvedConnectionTemplate

-- | Used by the @dc-postgres-agent to compile a query.
pgDBQueryPlanSimple ::
  (MonadError QErr m) =>
  UserInfo ->
  QueryTagsComment ->
  QueryDB ('Postgres 'Vanilla) Void (UnpreparedValue ('Postgres 'Vanilla)) ->
  m (OnBaseMonad (PG.TxET QErr) EncJSON, Maybe PreparedSql)
pgDBQueryPlanSimple userInfo queryTagsComment query = do
  (preparedQuery, PlanningSt {_psPrepped = planVals}) <-
    flip runStateT initPlanningSt $ traverse (prepareWithPlan userInfo) query
  let preparedSQLWithQueryTags =
        appendPreparedSQLWithQueryTags (irToRootFieldPlan planVals preparedQuery) queryTagsComment
  let (action, preparedSQL) = mkCurPlanTx userInfo preparedSQLWithQueryTags
  pure (action, preparedSQL)

pgDBQueryExplain ::
  forall pgKind m.
  ( MonadError QErr m,
    Backend ('Postgres pgKind),
    PostgresTranslateSelect pgKind
  ) =>
  RootFieldAlias ->
  UserInfo ->
  SourceName ->
  SourceConfig ('Postgres pgKind) ->
  QueryDB ('Postgres pgKind) Void (UnpreparedValue ('Postgres pgKind)) ->
  [HTTP.Header] ->
  Maybe G.Name ->
  m (AB.AnyBackend DBStepInfo)
pgDBQueryExplain fieldName userInfo sourceName sourceConfig rootSelection reqHeaders operationName = do
  preparedQuery <- traverse (prepareWithoutPlan userInfo) rootSelection
  let PreparedSql querySQL _ = irToRootFieldPlan mempty preparedQuery
      textSQL = PG.getQueryText querySQL
      -- CAREFUL!: an `EXPLAIN ANALYZE` here would actually *execute* this
      -- query, maybe resulting in privilege escalation:
      withExplain = "EXPLAIN " <> textSQL
  let action = OnBaseMonad do
        PG.withQE dmlTxErrorHandler (PG.fromText withExplain) () True <&> \planList ->
          withNoStatistics $ encJFromJValue $ ExplainPlan fieldName (Just textSQL) (Just $ map runIdentity planList)
  resolvedConnectionTemplate <-
    let connectionTemplateResolver =
          connectionTemplateConfigResolver (_pscConnectionTemplateConfig sourceConfig)
        queryContext =
          Just
            $ QueryContext operationName
            $ QueryOperationType G.OperationTypeQuery
     in applyConnectionTemplateResolverNonAdmin connectionTemplateResolver userInfo reqHeaders queryContext
  pure
    $ AB.mkAnyBackend
    $ DBStepInfo @('Postgres pgKind) sourceName sourceConfig Nothing action resolvedConnectionTemplate

pgDBSubscriptionExplain ::
  ( MonadError QErr m,
    MonadIO m,
    MT.MonadBaseControl IO m
  ) =>
  SubscriptionQueryPlan ('Postgres pgKind) (MultiplexedQuery ('Postgres pgKind)) ->
  m SubscriptionQueryPlanExplanation
pgDBSubscriptionExplain plan = do
  let parameterizedPlan = _sqpParameterizedPlan plan
      pgExecCtx = _pscExecCtx $ _sqpSourceConfig plan
      queryText = PG.getQueryText . PGL.unMultiplexedQuery $ _plqpQuery parameterizedPlan
      -- CAREFUL!: an `EXPLAIN ANALYZE` here would actually *execute* this
      -- query, maybe resulting in privilege escalation:
      explainQuery = PG.fromText $ "EXPLAIN " <> queryText
      resolvedConnectionTemplate = _sqpResolvedConnectionTemplate plan
  cohortId <- newCohortId
  explanationLines <-
    liftEitherM
      $ runExceptT
      $ _pecRunTx pgExecCtx (PGExecCtxInfo (Tx PG.ReadOnly Nothing) (GraphQLQuery resolvedConnectionTemplate))
      $ map runIdentity
      <$> PGL.executeQuery explainQuery [(cohortId, _sqpVariables plan)]
  pure $ SubscriptionQueryPlanExplanation queryText explanationLines $ _sqpVariables plan

-- mutation

convertDelete ::
  forall pgKind m.
  ( MonadError QErr m,
    Backend ('Postgres pgKind),
    PostgresTranslateSelect pgKind,
    MonadReader QueryTagsComment m,
    MonadIO m,
    Tracing.MonadTrace m
  ) =>
  Env.Environment ->
  HTTP.Manager ->
  L.Logger L.Hasura ->
  UserInfo ->
  IR.AnnDelG ('Postgres pgKind) Void (UnpreparedValue ('Postgres pgKind)) ->
  Options.StringifyNumbers ->
  [HTTP.Header] ->
  Maybe (HashMap G.Name (G.Value G.Variable)) ->
  m (OnBaseMonad (PG.TxET QErr) EncJSON)
convertDelete env manager logger userInfo deleteOperation stringifyNum reqHeaders selSetArguments = do
  for_ (_adValidateInput deleteOperation) $ \(VIHttp ValidateInputHttpDefinition {..}) -> do
    PGE.validateDeleteMutation env manager logger userInfo _vihdUrl _vihdHeaders _vihdTimeout _vihdForwardClientHeaders reqHeaders deleteOperation selSetArguments
  queryTags <- ask
  preparedDelete <- traverse (prepareWithoutPlan userInfo) deleteOperation
  pure
    $ OnBaseMonad
    $ flip runReaderT queryTags
    $ PGE.execDeleteQuery stringifyNum (_adNamingConvention deleteOperation) userInfo (preparedDelete, Seq.empty)

convertUpdate ::
  forall pgKind m.
  ( MonadError QErr m,
    Backend ('Postgres pgKind),
    PostgresTranslateSelect pgKind,
    MonadReader QueryTagsComment m,
    MonadIO m,
    Tracing.MonadTrace m
  ) =>
  Env.Environment ->
  HTTP.Manager ->
  L.Logger L.Hasura ->
  UserInfo ->
  IR.AnnotatedUpdateG ('Postgres pgKind) Void (UnpreparedValue ('Postgres pgKind)) ->
  Options.StringifyNumbers ->
  [HTTP.Header] ->
  Maybe (HashMap G.Name (G.Value G.Variable)) ->
  m (OnBaseMonad (PG.TxET QErr) EncJSON)
convertUpdate env manager logger userInfo updateOperation stringifyNum reqHeaders selSetArguments = do
  for_ (_auValidateInput updateOperation) $ \(VIHttp ValidateInputHttpDefinition {..}) -> do
    PGE.validateUpdateMutation env manager logger userInfo _vihdUrl _vihdHeaders _vihdTimeout _vihdForwardClientHeaders reqHeaders updateOperation selSetArguments
  queryTags <- ask
  preparedUpdate <- traverse (prepareWithoutPlan userInfo) updateOperation
  if Postgres.updateVariantIsEmpty $ IR._auUpdateVariant updateOperation
    then pure $ OnBaseMonad $ pure $ IR.buildEmptyMutResp $ IR._auOutput preparedUpdate
    else
      pure
        $ OnBaseMonad
        $ flip runReaderT queryTags
        $ PGE.execUpdateQuery stringifyNum (_auNamingConvention updateOperation) userInfo (preparedUpdate, Seq.empty)

convertInsert ::
  forall pgKind m.
  ( MonadError QErr m,
    MonadIO m,
    Backend ('Postgres pgKind),
    PostgresTranslateSelect pgKind,
    MonadReader QueryTagsComment m,
    Tracing.MonadTrace m
  ) =>
  Env.Environment ->
  HTTP.Manager ->
  L.Logger L.Hasura ->
  UserInfo ->
  IR.AnnotatedInsert ('Postgres pgKind) Void (UnpreparedValue ('Postgres pgKind)) ->
  Options.StringifyNumbers ->
  [HTTP.Header] ->
  m (OnBaseMonad (PG.TxET QErr) EncJSON)
convertInsert env manager logger userInfo insertOperation stringifyNum reqHeaders = do
  -- Validate insert data
  (_, res) <- flip runStateT InsOrdHashMap.empty $ validateInsertInput env manager logger userInfo (IR._aiData insertOperation) reqHeaders
  for_ res $ \(rows, VIHttp ValidateInputHttpDefinition {..}) -> do
    validateInsertRows env manager logger userInfo _vihdUrl _vihdHeaders _vihdTimeout _vihdForwardClientHeaders reqHeaders rows
  queryTags <- ask
  preparedInsert <- traverse (prepareWithoutPlan userInfo) insertOperation
  pure
    $ OnBaseMonad
    $ flip runReaderT queryTags
    $ convertToSQLTransaction preparedInsert userInfo Seq.empty stringifyNum (_aiNamingConvention insertOperation)

-- | A pared-down version of 'Query.convertQuerySelSet', for use in execution of
-- special case of SQL function mutations (see 'MDBFunction').
convertFunction ::
  forall pgKind m.
  ( MonadError QErr m,
    Backend ('Postgres pgKind),
    PostgresTranslateSelect pgKind,
    MonadReader QueryTagsComment m
  ) =>
  UserInfo ->
  JsonAggSelect ->
  -- | VOLATILE function as 'SelectExp'
  IR.AnnSimpleSelectG ('Postgres pgKind) Void (UnpreparedValue ('Postgres pgKind)) ->
  m (OnBaseMonad (PG.TxET QErr) EncJSON)
convertFunction userInfo jsonAggSelect unpreparedQuery = do
  queryTags <- ask
  -- Transform the RQL AST into a prepared SQL query
  (preparedQuery, PlanningSt {_psPrepped = planVals}) <-
    flip runStateT initPlanningSt
      $ traverse (prepareWithPlan userInfo) unpreparedQuery
  let queryResultFn =
        case jsonAggSelect of
          JASMultipleRows -> QDBMultipleRows
          JASSingleObject -> QDBSingleRow
  let preparedSQLWithQueryTags = appendPreparedSQLWithQueryTags (irToRootFieldPlan planVals $ queryResultFn preparedQuery) queryTags
  pure
    $! fst
    $ mkCurPlanTx userInfo preparedSQLWithQueryTags -- forget (Maybe PreparedSql)

pgDBMutationPlan ::
  forall pgKind m.
  ( MonadError QErr m,
    MonadIO m,
    Backend ('Postgres pgKind),
    PostgresTranslateSelect pgKind,
    MonadReader QueryTagsComment m,
    Tracing.MonadTrace m
  ) =>
  Env.Environment ->
  HTTP.Manager ->
  L.Logger L.Hasura ->
  UserInfo ->
  Options.StringifyNumbers ->
  SourceName ->
  SourceConfig ('Postgres pgKind) ->
  MutationDB ('Postgres pgKind) Void (UnpreparedValue ('Postgres pgKind)) ->
  [HTTP.Header] ->
  Maybe G.Name ->
  Maybe (HashMap G.Name (G.Value G.Variable)) ->
  m (DBStepInfo ('Postgres pgKind))
pgDBMutationPlan env manager logger userInfo stringifyNum sourceName sourceConfig mrf reqHeaders operationName selSetArguments = do
  resolvedConnectionTemplate <-
    let connectionTemplateResolver =
          connectionTemplateConfigResolver (_pscConnectionTemplateConfig sourceConfig)
        queryContext =
          Just
            $ QueryContext operationName
            $ QueryOperationType G.OperationTypeMutation
     in applyConnectionTemplateResolverNonAdmin connectionTemplateResolver userInfo reqHeaders queryContext
  go resolvedConnectionTemplate <$> case mrf of
    MDBInsert s -> convertInsert env manager logger userInfo s stringifyNum reqHeaders
    MDBUpdate s -> convertUpdate env manager logger userInfo s stringifyNum reqHeaders selSetArguments
    MDBDelete s -> convertDelete env manager logger userInfo s stringifyNum reqHeaders selSetArguments
    MDBFunction returnsSet s -> convertFunction userInfo returnsSet s
  where
    go resolvedConnectionTemplate v =
      DBStepInfo
        { dbsiSourceName = sourceName,
          dbsiSourceConfig = sourceConfig,
          dbsiPreparedQuery = Nothing,
          dbsiAction = fmap withNoStatistics v,
          dbsiResolvedConnectionTemplate = resolvedConnectionTemplate
        }

-- subscription

pgDBLiveQuerySubscriptionPlan ::
  forall pgKind m.
  ( MonadError QErr m,
    MonadIO m,
    Backend ('Postgres pgKind),
    PostgresTranslateSelect pgKind,
    MonadReader QueryTagsComment m
  ) =>
  UserInfo ->
  SourceName ->
  SourceConfig ('Postgres pgKind) ->
  Maybe G.Name ->
  RootFieldMap (QueryDB ('Postgres pgKind) Void (UnpreparedValue ('Postgres pgKind))) ->
  [HTTP.Header] ->
  Maybe G.Name ->
  m (SubscriptionQueryPlan ('Postgres pgKind) (MultiplexedQuery ('Postgres pgKind)))
pgDBLiveQuerySubscriptionPlan userInfo _sourceName sourceConfig namespace unpreparedAST reqHeaders operationName = do
  (preparedAST, PGL.QueryParametersInfo {..}) <-
    flip runStateT mempty
      $ for unpreparedAST
      $ traverse (PGL.resolveMultiplexedValue (_uiSession userInfo))
  subscriptionQueryTagsComment <- ask
  let multiplexedQuery = PGL.mkMultiplexedQuery $ InsOrdHashMap.mapKeys _rfaAlias preparedAST
      multiplexedQueryWithQueryTags =
        multiplexedQuery {PGL.unMultiplexedQuery = appendSQLWithQueryTags (PGL.unMultiplexedQuery multiplexedQuery) subscriptionQueryTagsComment}
      roleName = _uiRole userInfo
      parameterizedPlan = ParameterizedSubscriptionQueryPlan roleName multiplexedQueryWithQueryTags

  resolvedConnectionTemplate <-
    let connectionTemplateResolver =
          connectionTemplateConfigResolver (_pscConnectionTemplateConfig sourceConfig)
        queryContext =
          Just
            $ QueryContext operationName
            $ QueryOperationType G.OperationTypeSubscription
     in applyConnectionTemplateResolverNonAdmin connectionTemplateResolver userInfo reqHeaders queryContext

  -- Cohort Id: Used for validating the multiplexed query. See @'testMultiplexedQueryTx'.
  -- It is disposed when the subscriber is added to existing cohort.
  cohortId <- newCohortId

  let pgExecCtxInfo = PGExecCtxInfo (Tx PG.ReadOnly Nothing) (GraphQLQuery resolvedConnectionTemplate)
  cohortVariables <- liftEitherM $ liftIO $ runExceptT $ _pecRunTx (_pscExecCtx sourceConfig) pgExecCtxInfo do
    -- We need to ensure that the values provided for variables are correct according to Postgres.
    -- Without this check an invalid value for a variable for one instance of the subscription will
    -- take down the entire multiplexed query.
    validatedQueryVars <- PGL.validateVariablesTx _qpiReusableVariableValues
    validatedSyntheticVars <- PGL.validateVariablesTx $ toList _qpiSyntheticVariableValues
    let cohortVariables =
          mkCohortVariables
            _qpiReferencedSessionVariables
            (_uiSession userInfo)
            validatedQueryVars
            validatedSyntheticVars
            mempty -- live query subscriptions don't use the streaming cursor variables

    -- Test the multiplexed query. Without this test if the query fails, the subscription will
    -- take down the entier multiplexed query affecting all subscribers.
    testMultiplexedQueryTx multiplexedQueryWithQueryTags cohortId cohortVariables
    pure cohortVariables

  pure $ SubscriptionQueryPlan parameterizedPlan sourceConfig cohortId resolvedConnectionTemplate cohortVariables namespace

pgDBStreamingSubscriptionPlan ::
  forall pgKind m.
  ( MonadError QErr m,
    MonadIO m,
    Backend ('Postgres pgKind),
    PostgresTranslateSelect pgKind,
    MonadReader QueryTagsComment m
  ) =>
  UserInfo ->
  SourceName ->
  SourceConfig ('Postgres pgKind) ->
  (RootFieldAlias, (QueryDB ('Postgres pgKind) Void (UnpreparedValue ('Postgres pgKind)))) ->
  [HTTP.Header] ->
  Maybe G.Name ->
  m (SubscriptionQueryPlan ('Postgres pgKind) (MultiplexedQuery ('Postgres pgKind)))
pgDBStreamingSubscriptionPlan userInfo _sourceName sourceConfig (rootFieldAlias, unpreparedAST) reqHeaders operationName = do
  (preparedAST, PGL.QueryParametersInfo {..}) <-
    flip runStateT mempty
      $ traverse (PGL.resolveMultiplexedValue (_uiSession userInfo)) unpreparedAST
  subscriptionQueryTagsComment <- ask
  let multiplexedQuery = PGL.mkStreamingMultiplexedQuery (G._rfaAlias rootFieldAlias, preparedAST)
      multiplexedQueryWithQueryTags =
        multiplexedQuery {PGL.unMultiplexedQuery = appendSQLWithQueryTags (PGL.unMultiplexedQuery multiplexedQuery) subscriptionQueryTagsComment}
      roleName = _uiRole userInfo
      parameterizedPlan = ParameterizedSubscriptionQueryPlan roleName multiplexedQueryWithQueryTags

  resolvedConnectionTemplate <-
    let connectionTemplateResolver =
          connectionTemplateConfigResolver (_pscConnectionTemplateConfig sourceConfig)
        queryContext =
          Just
            $ QueryContext operationName
            $ QueryOperationType G.OperationTypeSubscription
     in applyConnectionTemplateResolverNonAdmin connectionTemplateResolver userInfo reqHeaders queryContext

  -- Cohort Id: Used for validating the multiplexed query. See @'testMultiplexedQueryTx'.
  -- It is disposed when the subscriber is added to existing cohort.
  cohortId <- newCohortId

  let pgExecCtxInfo = PGExecCtxInfo (Tx PG.ReadOnly Nothing) (GraphQLQuery resolvedConnectionTemplate)
  cohortVariables <- liftEitherM $ liftIO $ runExceptT $ _pecRunTx (_pscExecCtx sourceConfig) pgExecCtxInfo do
    -- We need to ensure that the values provided for variables are correct according to Postgres.
    -- Without this check an invalid value for a variable for one instance of the subscription will
    -- take down the entire multiplexed query.
    validatedQueryVars <- PGL.validateVariablesTx _qpiReusableVariableValues
    validatedSyntheticVars <- PGL.validateVariablesTx $ toList _qpiSyntheticVariableValues
    validatedCursorVars <- PGL.validateVariablesTx $ getCursorVars unpreparedAST
    let cohortVariables =
          mkCohortVariables
            _qpiReferencedSessionVariables
            (_uiSession userInfo)
            validatedQueryVars
            validatedSyntheticVars
            validatedCursorVars

    -- Test the multiplexed query. Without this test if the query fails, the subscription will
    -- take down the entier multiplexed query affecting all subscribers.
    testMultiplexedQueryTx multiplexedQueryWithQueryTags cohortId cohortVariables
    pure cohortVariables

  pure $ SubscriptionQueryPlan parameterizedPlan sourceConfig cohortId resolvedConnectionTemplate cohortVariables $ _rfaNamespace rootFieldAlias
  where
    getCursorVars qdb =
      case qdb of
        QDBStreamMultipleRows (IR.AnnSelectStreamG () _ _ _ args _) ->
          let cursorArg = IR._ssaCursorArg args
              colInfo = IR._sciColInfo cursorArg
           in HashMap.singleton (ciName colInfo) (IR._sciInitialValue cursorArg)
        _ -> mempty

-- | Test a multiplexed query in a transaction.
testMultiplexedQueryTx ::
  (MonadTx m) =>
  PGL.MultiplexedQuery ->
  CohortId ->
  CohortVariables ->
  m ()
testMultiplexedQueryTx (PGL.MultiplexedQuery query) cohortId cohortVariables = do
  -- Run the query and discard the results
  -- NOTE: Adding `LIMIT 1` to the root selection of the query would make
  -- executing the query faster. However, it is not preferred due to the following
  -- reasons:
  -- Multiplex query validation is required for queries involving any SQL functions,
  -- computed fields and SQL functions as root fields, as the functions are bound to
  -- raise run-time SQL exception resulting in error response for all subscribers in a cohort.
  -- a. In case of computed fields, applying `LIMIT 1` to the base table selection will
  --    enforce SQL function to evaluate only on one row. There's a possibility of SQL exception
  --    on evaluating function on other rows.
  -- b. In case of SQL functions as root fields, applying `LIMIT 1` to the base SQL function selection
  --    don't have any performance impact as the limit is applied on the function result.
  PG.Discard () <- PGL.executeQuery query [(cohortId, cohortVariables)]
  pure ()

-- turn the current plan into a transaction
mkCurPlanTx ::
  UserInfo ->
  PreparedSql ->
  (OnBaseMonad (PG.TxET QErr) EncJSON, Maybe PreparedSql)
mkCurPlanTx userInfo ps@(PreparedSql q prepMap) =
  -- generate the SQL and prepared vars or the bytestring
  let args = withUserVars (_uiSession userInfo) prepMap
      -- WARNING: this quietly assumes the intmap keys are contiguous
      prepArgs = fst <$> IntMap.elems args
   in (,Just ps) $ OnBaseMonad do
        -- https://opentelemetry.io/docs/reference/specification/trace/semantic_conventions/database/#connection-level-attributes
        Tracing.attachMetadata [("db.system", "postgresql")]
        runIdentity
          . PG.getRow
          <$> PG.rawQE dmlTxErrorHandler q prepArgs True

-- convert a query from an intermediate representation to... another
irToRootFieldPlan ::
  ( Backend ('Postgres pgKind),
    PostgresTranslateSelect pgKind
  ) =>
  PrepArgMap ->
  QueryDB ('Postgres pgKind) Void S.SQLExp ->
  PreparedSql
irToRootFieldPlan prepped = \case
  QDBMultipleRows s -> mkPreparedSql (DS.selectQuerySQL JASMultipleRows) s
  QDBSingleRow s -> mkPreparedSql (DS.selectQuerySQL JASSingleObject) s
  QDBAggregation s -> mkPreparedSql DS.selectAggregateQuerySQL s
  QDBConnection s -> mkPreparedSql DS.connectionSelectQuerySQL s
  QDBStreamMultipleRows s -> mkPreparedSql DS.selectStreamQuerySQL s
  where
    mkPreparedSql :: (t -> PG.Query) -> t -> PreparedSql
    mkPreparedSql f simpleSel =
      PreparedSql (f simpleSel) prepped

-- Append Query Tags to the Prepared SQL
appendPreparedSQLWithQueryTags :: PreparedSql -> QueryTagsComment -> PreparedSql
appendPreparedSQLWithQueryTags preparedSQL queryTags =
  preparedSQL {_psQuery = appendSQLWithQueryTags query queryTags}
  where
    query = _psQuery preparedSQL

appendSQLWithQueryTags :: PG.Query -> QueryTagsComment -> PG.Query
appendSQLWithQueryTags query queryTags = query {PG.getQueryText = queryText <> _unQueryTagsComment queryTags}
  where
    queryText = PG.getQueryText query

--------------------------------------------------------------------------------
-- Remote Relationships (e.g. DB-to-DB Joins, remote schema joins, etc.)
--------------------------------------------------------------------------------

-- | Construct an action (i.e. 'DBStepInfo') which can marshal some remote
-- relationship information into a form that Postgres can query against.
pgDBRemoteRelationshipPlan ::
  forall pgKind m.
  ( MonadError QErr m,
    Backend ('Postgres pgKind),
    PostgresTranslateSelect pgKind
  ) =>
  UserInfo ->
  SourceName ->
  SourceConfig ('Postgres pgKind) ->
  -- | List of json objects, each of which becomes a row of the table.
  NonEmpty J.Object ->
  -- | The above objects have this schema
  --
  -- XXX: What is this for/what does this mean?
  HashMap FieldName (Column ('Postgres pgKind), ScalarType ('Postgres pgKind)) ->
  -- | This is a field name from the lhs that *has* to be selected in the
  -- response along with the relationship.
  FieldName ->
  (FieldName, IR.SourceRelationshipSelection ('Postgres pgKind) Void UnpreparedValue) ->
  [HTTP.Header] ->
  Maybe G.Name ->
  Options.StringifyNumbers ->
  m (DBStepInfo ('Postgres pgKind))
pgDBRemoteRelationshipPlan userInfo sourceName sourceConfig lhs lhsSchema argumentId relationship reqHeaders operationName stringifyNumbers = do
  -- NOTE: 'QueryTags' currently cannot support remote relationship queries.
  --
  -- In the future if we want to add support we'll need to add a new type of
  -- metadata (e.g. 'ParameterizedQueryHash' doesn't make sense here) and find
  -- a root field name that makes sense to attach to it.
  flip runReaderT emptyQueryTagsComment $ pgDBQueryPlan userInfo sourceName sourceConfig rootSelection reqHeaders operationName
  where
    coerceToColumn = Postgres.unsafePGCol . getFieldNameTxt
    joinColumnMapping = mapKeys coerceToColumn lhsSchema

    rowsArgument :: UnpreparedValue ('Postgres pgKind)
    rowsArgument =
      UVParameter FreshVar
        $ ColumnValue (ColumnScalar Postgres.PGJSONB)
        $ Postgres.PGValJSONB
        $ PG.JSONB
        $ J.toJSON lhs
    jsonToRecordSet :: IR.SelectFromG ('Postgres pgKind) (UnpreparedValue ('Postgres pgKind))

    recordSetDefinitionList =
      (coerceToColumn argumentId, Postgres.PGBigInt) : HashMap.toList (fmap snd joinColumnMapping)
    jsonToRecordSet =
      IR.FromFunction
        (Postgres.QualifiedObject "pg_catalog" $ Postgres.FunctionName "jsonb_to_recordset")
        (FunctionArgsExp [Postgres.AEInput rowsArgument] mempty)
        (Just recordSetDefinitionList)

    rootSelection =
      convertRemoteSourceRelationship
        (fst <$> joinColumnMapping)
        jsonToRecordSet
        (Postgres.unsafePGCol $ getFieldNameTxt argumentId)
        (ColumnScalar Postgres.PGBigInt)
        relationship
        stringifyNumbers
