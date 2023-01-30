{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Postgres Instances Transport
--
-- Defines the MSSQL instance of 'BackendTransport' and how to interact with the
-- database for running queries, mutations, subscriptions, and so on.
module Hasura.Backends.Postgres.Instances.Transport
  ( runPGMutationTransaction,
  )
where

import Data.Aeson qualified as J
import Data.ByteString qualified as B
import Data.HashMap.Strict.InsOrd qualified as OMap
import Data.Text.Extended
import Database.PG.Query qualified as PG
import Hasura.Backends.Postgres.Connection.MonadTx
import Hasura.Backends.Postgres.Execute.Subscription qualified as PGL
import Hasura.Backends.Postgres.Execute.Types
import Hasura.Backends.Postgres.Instances.Execute qualified as EQ
import Hasura.Backends.Postgres.SQL.Value
import Hasura.Backends.Postgres.Translate.Select (PostgresAnnotatedFieldJSON)
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.GraphQL.Execute.Backend
import Hasura.GraphQL.Execute.Subscription.Plan
import Hasura.GraphQL.Logging
import Hasura.GraphQL.Namespace
  ( RootFieldAlias,
    RootFieldMap,
    mkUnNamespacedRootFieldAlias,
  )
import Hasura.GraphQL.Transport.Backend
import Hasura.GraphQL.Transport.HTTP.Protocol
import Hasura.Logging qualified as L
import Hasura.Name qualified as Name
import Hasura.Prelude
import Hasura.RQL.DDL.ConnectionTemplate (BackendResolvedConnectionTemplate (..), ResolvedConnectionTemplateWrapper (..))
import Hasura.RQL.Types.Backend
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.SQL.Backend
import Hasura.Server.Types (RequestId)
import Hasura.Session
import Hasura.Tracing
import Hasura.Tracing qualified as Tracing

instance
  ( Backend ('Postgres pgKind),
    PostgresAnnotatedFieldJSON pgKind
  ) =>
  BackendTransport ('Postgres pgKind)
  where
  runDBQuery = runPGQuery
  runDBMutation = runPGMutation
  runDBSubscription = runPGSubscription
  runDBStreamingSubscription = runPGStreamingSubscription
  runDBQueryExplain = runPGQueryExplain

runPGQuery ::
  ( MonadIO m,
    MonadError QErr m,
    MonadQueryLog m,
    MonadTrace m
  ) =>
  RequestId ->
  GQLReqUnparsed ->
  RootFieldAlias ->
  UserInfo ->
  L.Logger L.Hasura ->
  SourceConfig ('Postgres pgKind) ->
  Tracing.TraceT (PG.TxET QErr IO) EncJSON ->
  Maybe EQ.PreparedSql ->
  ResolvedConnectionTemplate ('Postgres pgKind) ->
  -- | Also return the time spent in the PG query; for telemetry.
  m (DiffTime, EncJSON)
runPGQuery reqId query fieldName _userInfo logger sourceConfig tx genSql resolvedConnectionTemplate = do
  -- log the generated SQL and the graphql query
  logQueryLog logger $ mkQueryLog query fieldName genSql reqId (resolvedConnectionTemplate <$ resolvedConnectionTemplate)
  withElapsedTime $
    trace ("Postgres Query for root field " <>> fieldName) $
      Tracing.interpTraceT (runQueryTx (_pscExecCtx sourceConfig) (GraphQLQuery resolvedConnectionTemplate)) tx

runPGMutation ::
  ( MonadIO m,
    MonadError QErr m,
    MonadQueryLog m,
    MonadTrace m
  ) =>
  RequestId ->
  GQLReqUnparsed ->
  RootFieldAlias ->
  UserInfo ->
  L.Logger L.Hasura ->
  SourceConfig ('Postgres pgKind) ->
  Tracing.TraceT (PG.TxET QErr IO) EncJSON ->
  Maybe EQ.PreparedSql ->
  ResolvedConnectionTemplate ('Postgres pgKind) ->
  m (DiffTime, EncJSON)
runPGMutation reqId query fieldName userInfo logger sourceConfig tx _genSql resolvedConnectionTemplate = do
  -- log the graphql query
  logQueryLog logger $ mkQueryLog query fieldName Nothing reqId (resolvedConnectionTemplate <$ resolvedConnectionTemplate)
  ctx <- Tracing.currentContext
  withElapsedTime $
    trace ("Postgres Mutation for root field " <>> fieldName) $
      Tracing.interpTraceT
        ( liftEitherM
            . liftIO
            . runExceptT
            . _pecRunTx (_pscExecCtx sourceConfig) (PGExecCtxInfo (Tx PG.ReadWrite Nothing) (GraphQLQuery resolvedConnectionTemplate))
            . withTraceContext ctx
            . withUserInfo userInfo
        )
        tx

runPGSubscription ::
  MonadIO m =>
  SourceConfig ('Postgres pgKind) ->
  MultiplexedQuery ('Postgres pgKind) ->
  [(CohortId, CohortVariables)] ->
  ResolvedConnectionTemplate ('Postgres pgKind) ->
  m (DiffTime, Either QErr [(CohortId, B.ByteString)])
runPGSubscription sourceConfig query variables resolvedConnectionTemplate =
  withElapsedTime $
    runExceptT $
      runQueryTx (_pscExecCtx sourceConfig) (GraphQLQuery resolvedConnectionTemplate) $
        PGL.executeMultiplexedQuery query variables

runPGStreamingSubscription ::
  MonadIO m =>
  SourceConfig ('Postgres pgKind) ->
  MultiplexedQuery ('Postgres pgKind) ->
  [(CohortId, CohortVariables)] ->
  ResolvedConnectionTemplate ('Postgres pgKind) ->
  m (DiffTime, Either QErr [(CohortId, B.ByteString, CursorVariableValues)])
runPGStreamingSubscription sourceConfig query variables resolvedConnectionTemplate =
  withElapsedTime $
    runExceptT $ do
      res <- runQueryTx (_pscExecCtx sourceConfig) (GraphQLQuery resolvedConnectionTemplate) $ PGL.executeStreamingMultiplexedQuery query variables
      pure $ res <&> (\(cohortId, cohortRes, cursorVariableVals) -> (cohortId, cohortRes, PG.getViaJSON cursorVariableVals))

runPGQueryExplain ::
  forall pgKind m.
  ( MonadIO m,
    MonadError QErr m
  ) =>
  DBStepInfo ('Postgres pgKind) ->
  m EncJSON
runPGQueryExplain (DBStepInfo _ sourceConfig _ action resolvedConnectionTemplate) =
  -- All Postgres transport functions use the same monad stack: the ExecutionMonad defined in the
  -- matching instance of BackendExecute. However, Explain doesn't need tracing! Rather than
  -- introducing a separate "ExplainMonad", we simply use @runTraceTWithReporter@ to remove the
  -- TraceT.
  runQueryTx (_pscExecCtx sourceConfig) (GraphQLQuery resolvedConnectionTemplate) $ ignoreTraceT action

mkQueryLog ::
  GQLReqUnparsed ->
  RootFieldAlias ->
  Maybe EQ.PreparedSql ->
  RequestId ->
  Maybe (ResolvedConnectionTemplate ('Postgres pgKind)) ->
  QueryLog
mkQueryLog gqlQuery fieldName preparedSql requestId resolvedConnectionTemplate =
  QueryLog gqlQuery ((fieldName,) <$> generatedQuery) requestId (QueryLogKindDatabase (mkBackendResolvedConnectionTemplate <$> resolvedConnectionTemplate))
  where
    mkBackendResolvedConnectionTemplate ::
      ResolvedConnectionTemplate ('Postgres pgKind) ->
      BackendResolvedConnectionTemplate
    mkBackendResolvedConnectionTemplate =
      BackendResolvedConnectionTemplate . AB.mkAnyBackend @('Postgres 'Vanilla) . ResolvedConnectionTemplateWrapper
    generatedQuery =
      preparedSql <&> \(EQ.PreparedSql query args) ->
        GeneratedQuery (PG.getQueryText query) (J.toJSON $ pgScalarValueToJson . snd <$> args)

-- ad-hoc transaction optimisation
-- see Note [Backwards-compatible transaction optimisation]

runPGMutationTransaction ::
  ( MonadIO m,
    MonadError QErr m,
    MonadQueryLog m,
    MonadTrace m
  ) =>
  RequestId ->
  GQLReqUnparsed ->
  UserInfo ->
  L.Logger L.Hasura ->
  SourceConfig ('Postgres pgKind) ->
  ResolvedConnectionTemplate ('Postgres pgKind) ->
  RootFieldMap (DBStepInfo ('Postgres pgKind)) ->
  m (DiffTime, RootFieldMap EncJSON)
runPGMutationTransaction reqId query userInfo logger sourceConfig resolvedConnectionTemplate mutations = do
  logQueryLog logger $ mkQueryLog query (mkUnNamespacedRootFieldAlias Name._transaction) Nothing reqId (resolvedConnectionTemplate <$ resolvedConnectionTemplate)
  ctx <- Tracing.currentContext
  withElapsedTime
    $ Tracing.interpTraceT
      ( liftEitherM
          . liftIO
          . runExceptT
          . _pecRunTx (_pscExecCtx sourceConfig) (PGExecCtxInfo (Tx PG.ReadWrite Nothing) (GraphQLQuery resolvedConnectionTemplate))
          . withTraceContext ctx
          . withUserInfo userInfo
      )
    $ flip OMap.traverseWithKey mutations \fieldName dbsi ->
      trace ("Postgres Mutation for root field " <>> fieldName) $ dbsiAction dbsi
