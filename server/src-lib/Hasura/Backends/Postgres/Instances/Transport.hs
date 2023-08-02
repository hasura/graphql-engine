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

import Control.Monad.Trans.Control
import Data.Aeson qualified as J
import Data.ByteString qualified as B
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.Text.Extended
import Database.PG.Query qualified as PG
import Hasura.Backends.DataConnector.Agent.Client (AgentLicenseKey)
import Hasura.Backends.Postgres.Connection.MonadTx
import Hasura.Backends.Postgres.Execute.Subscription qualified as PGL
import Hasura.Backends.Postgres.Execute.Types
import Hasura.Backends.Postgres.Instances.Execute qualified as EQ
import Hasura.Backends.Postgres.SQL.Value
import Hasura.Backends.Postgres.Translate.Select (PostgresTranslateSelect)
import Hasura.Base.Error
import Hasura.CredentialCache
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
import Hasura.RQL.Types.BackendTag (HasTag)
import Hasura.RQL.Types.BackendType
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.Server.Types (RequestId)
import Hasura.Session
import Hasura.Tracing

instance
  ( Backend ('Postgres pgKind),
    PostgresTranslateSelect pgKind
  ) =>
  BackendTransport ('Postgres pgKind)
  where
  runDBQuery = runPGQuery @pgKind
  runDBMutation = runPGMutation @pgKind
  runDBSubscription = runPGSubscription
  runDBStreamingSubscription = runPGStreamingSubscription
  runDBQueryExplain = runPGQueryExplain

runPGQuery ::
  forall pgKind m.
  ( HasTag ('Postgres pgKind),
    MonadIO m,
    MonadBaseControl IO m,
    MonadError QErr m,
    MonadQueryLog m,
    MonadTrace m
  ) =>
  RequestId ->
  GQLReqUnparsed ->
  RootFieldAlias ->
  UserInfo ->
  L.Logger L.Hasura ->
  Maybe (CredentialCache AgentLicenseKey) ->
  SourceConfig ('Postgres pgKind) ->
  OnBaseMonad (PG.TxET QErr) (Maybe (AB.AnyBackend ExecutionStats), EncJSON) ->
  Maybe EQ.PreparedSql ->
  ResolvedConnectionTemplate ('Postgres pgKind) ->
  -- | Also return the time spent in the PG query; for telemetry.
  m (DiffTime, EncJSON)
runPGQuery reqId query fieldName _userInfo logger _ sourceConfig tx genSql resolvedConnectionTemplate = do
  -- log the generated SQL and the graphql query
  logQueryLog logger $ mkQueryLog query fieldName genSql reqId (resolvedConnectionTemplate <$ resolvedConnectionTemplate)
  withElapsedTime
    $ newSpan ("Postgres Query for root field " <>> fieldName)
    $ (<* attachSourceConfigAttributes @('Postgres pgKind) sourceConfig)
    $ runQueryTx (_pscExecCtx sourceConfig) (GraphQLQuery resolvedConnectionTemplate)
    $ fmap snd (runOnBaseMonad tx)

runPGMutation ::
  forall pgKind m.
  ( HasTag ('Postgres pgKind),
    MonadIO m,
    MonadBaseControl IO m,
    MonadError QErr m,
    MonadQueryLog m,
    MonadTrace m
  ) =>
  RequestId ->
  GQLReqUnparsed ->
  RootFieldAlias ->
  UserInfo ->
  L.Logger L.Hasura ->
  Maybe (CredentialCache AgentLicenseKey) ->
  SourceConfig ('Postgres pgKind) ->
  OnBaseMonad (PG.TxET QErr) EncJSON ->
  Maybe EQ.PreparedSql ->
  ResolvedConnectionTemplate ('Postgres pgKind) ->
  m (DiffTime, EncJSON)
runPGMutation reqId query fieldName userInfo logger _ sourceConfig tx _genSql resolvedConnectionTemplate = do
  -- log the graphql query
  logQueryLog logger $ mkQueryLog query fieldName Nothing reqId (resolvedConnectionTemplate <$ resolvedConnectionTemplate)
  withElapsedTime
    $ newSpan ("Postgres Mutation for root field " <>> fieldName)
    $ (<* attachSourceConfigAttributes @('Postgres pgKind) sourceConfig)
    $ runTxWithCtxAndUserInfo userInfo (_pscExecCtx sourceConfig) (Tx PG.ReadWrite Nothing) (GraphQLQuery resolvedConnectionTemplate)
    $ runOnBaseMonad tx

runPGSubscription ::
  (MonadIO m, MonadBaseControl IO m) =>
  SourceConfig ('Postgres pgKind) ->
  MultiplexedQuery ('Postgres pgKind) ->
  [(CohortId, CohortVariables)] ->
  ResolvedConnectionTemplate ('Postgres pgKind) ->
  m (DiffTime, Either QErr [(CohortId, B.ByteString)])
runPGSubscription sourceConfig query variables resolvedConnectionTemplate =
  withElapsedTime
    $ runExceptT
    $ runQueryTx (_pscExecCtx sourceConfig) (GraphQLQuery resolvedConnectionTemplate)
    $ PGL.executeMultiplexedQuery query variables

runPGStreamingSubscription ::
  (MonadIO m, MonadBaseControl IO m) =>
  SourceConfig ('Postgres pgKind) ->
  MultiplexedQuery ('Postgres pgKind) ->
  [(CohortId, CohortVariables)] ->
  ResolvedConnectionTemplate ('Postgres pgKind) ->
  m (DiffTime, Either QErr [(CohortId, B.ByteString, CursorVariableValues)])
runPGStreamingSubscription sourceConfig query variables resolvedConnectionTemplate =
  withElapsedTime
    $ runExceptT
    $ do
      res <- runQueryTx (_pscExecCtx sourceConfig) (GraphQLQuery resolvedConnectionTemplate) $ PGL.executeStreamingMultiplexedQuery query variables
      pure $ res <&> (\(cohortId, cohortRes, cursorVariableVals) -> (cohortId, cohortRes, PG.getViaJSON cursorVariableVals))

runPGQueryExplain ::
  forall pgKind m.
  ( MonadIO m,
    MonadBaseControl IO m,
    MonadError QErr m,
    MonadTrace m
  ) =>
  Maybe (CredentialCache AgentLicenseKey) ->
  DBStepInfo ('Postgres pgKind) ->
  m EncJSON
runPGQueryExplain _ (DBStepInfo _ sourceConfig _ action resolvedConnectionTemplate) =
  runQueryTx (_pscExecCtx sourceConfig) (GraphQLQuery resolvedConnectionTemplate)
    $ fmap arResult (runOnBaseMonad action)

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
  forall pgKind m.
  ( HasTag ('Postgres pgKind),
    MonadIO m,
    MonadBaseControl IO m,
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
  withElapsedTime
    $ runTxWithCtxAndUserInfo userInfo (_pscExecCtx sourceConfig) (Tx PG.ReadWrite Nothing) (GraphQLQuery resolvedConnectionTemplate)
    $ flip InsOrdHashMap.traverseWithKey mutations \fieldName dbsi ->
      newSpan ("Postgres Mutation for root field " <>> fieldName)
        $ (<* attachSourceConfigAttributes @('Postgres pgKind) sourceConfig)
        $ fmap arResult
        $ runOnBaseMonad
        $ dbsiAction dbsi
