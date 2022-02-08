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
import Database.PG.Query qualified as Q
import Hasura.Backends.Postgres.Execute.LiveQuery qualified as PGL
import Hasura.Backends.Postgres.Instances.Execute qualified as EQ
import Hasura.Backends.Postgres.SQL.Value
import Hasura.Backends.Postgres.Translate.Select (PostgresAnnotatedFieldJSON)
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.GraphQL.Execute.Backend
import Hasura.GraphQL.Execute.LiveQuery.Plan
import Hasura.GraphQL.Logging
import Hasura.GraphQL.Namespace
  ( RootFieldAlias,
    RootFieldMap,
    mkUnNamespacedRootFieldAlias,
  )
import Hasura.GraphQL.Transport.Backend
import Hasura.GraphQL.Transport.HTTP.Protocol
import Hasura.Logging qualified as L
import Hasura.Prelude
import Hasura.RQL.Types
import Hasura.Server.Types (RequestId)
import Hasura.Session
import Hasura.Tracing
import Hasura.Tracing qualified as Tracing
import Language.GraphQL.Draft.Syntax qualified as G

instance
  ( Backend ('Postgres pgKind),
    PostgresAnnotatedFieldJSON pgKind
  ) =>
  BackendTransport ('Postgres pgKind)
  where
  runDBQuery = runPGQuery
  runDBMutation = runPGMutation
  runDBSubscription = runPGSubscription
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
  Tracing.TraceT (Q.TxET QErr IO) EncJSON ->
  Maybe EQ.PreparedSql ->
  -- | Also return the time spent in the PG query; for telemetry.
  m (DiffTime, EncJSON)
runPGQuery reqId query fieldName _userInfo logger sourceConfig tx genSql = do
  -- log the generated SQL and the graphql query
  logQueryLog logger $ mkQueryLog query fieldName genSql reqId
  withElapsedTime $
    trace ("Postgres Query for root field " <>> fieldName) $
      Tracing.interpTraceT (runQueryTx $ _pscExecCtx sourceConfig) tx

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
  Tracing.TraceT (Q.TxET QErr IO) EncJSON ->
  Maybe EQ.PreparedSql ->
  m (DiffTime, EncJSON)
runPGMutation reqId query fieldName userInfo logger sourceConfig tx _genSql = do
  -- log the graphql query
  logQueryLog logger $ mkQueryLog query fieldName Nothing reqId
  ctx <- Tracing.currentContext
  withElapsedTime $
    trace ("Postgres Mutation for root field " <>> fieldName) $
      Tracing.interpTraceT
        ( liftEitherM . liftIO . runExceptT
            . runTx (_pscExecCtx sourceConfig) Q.ReadWrite
            . withTraceContext ctx
            . withUserInfo userInfo
        )
        tx

runPGSubscription ::
  MonadIO m =>
  SourceConfig ('Postgres pgKind) ->
  MultiplexedQuery ('Postgres pgKind) ->
  [(CohortId, CohortVariables)] ->
  m (DiffTime, Either QErr [(CohortId, B.ByteString)])
runPGSubscription sourceConfig query variables =
  withElapsedTime $
    runExceptT $
      runQueryTx (_pscExecCtx sourceConfig) $
        PGL.executeMultiplexedQuery query variables

runPGQueryExplain ::
  forall pgKind m.
  ( MonadIO m,
    MonadError QErr m
  ) =>
  DBStepInfo ('Postgres pgKind) ->
  m EncJSON
runPGQueryExplain (DBStepInfo _ sourceConfig _ action) =
  -- All Postgres transport functions use the same monad stack: the ExecutionMonad defined in the
  -- matching instance of BackendExecute. However, Explain doesn't need tracing! Rather than
  -- introducing a separate "ExplainMonad", we simply use @runTraceTWithReporter@ to remove the
  -- TraceT.
  runQueryTx (_pscExecCtx sourceConfig) $ runTraceTWithReporter noReporter "explain" $ action

mkQueryLog ::
  GQLReqUnparsed ->
  RootFieldAlias ->
  Maybe EQ.PreparedSql ->
  RequestId ->
  QueryLog
mkQueryLog gqlQuery fieldName preparedSql requestId =
  QueryLog gqlQuery ((fieldName,) <$> generatedQuery) requestId QueryLogKindDatabase
  where
    generatedQuery =
      preparedSql <&> \(EQ.PreparedSql query args) ->
        GeneratedQuery (Q.getQueryText query) (J.toJSON $ pgScalarValueToJson . snd <$> args)

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
  RootFieldMap (DBStepInfo ('Postgres pgKind)) ->
  m (DiffTime, RootFieldMap EncJSON)
runPGMutationTransaction reqId query userInfo logger sourceConfig mutations = do
  logQueryLog logger $ mkQueryLog query (mkUnNamespacedRootFieldAlias $$(G.litName "transaction")) Nothing reqId
  ctx <- Tracing.currentContext
  withElapsedTime $ do
    Tracing.interpTraceT
      ( liftEitherM . liftIO . runExceptT
          . runTx (_pscExecCtx sourceConfig) Q.ReadWrite
          . withTraceContext ctx
          . withUserInfo userInfo
      )
      $ flip OMap.traverseWithKey mutations \fieldName dbsi ->
        trace ("Postgres Mutation for root field " <>> fieldName) $ dbsiAction dbsi
