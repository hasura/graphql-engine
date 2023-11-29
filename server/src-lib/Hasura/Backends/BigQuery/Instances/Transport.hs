{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.BigQuery.Instances.Transport () where

import Control.Monad.Trans.Control
import Data.Aeson qualified as J
import Hasura.Backends.BigQuery.Instances.Execute ()
import Hasura.Backends.DataConnector.Agent.Client (AgentLicenseKey)
import Hasura.Base.Error
import Hasura.CredentialCache
import Hasura.EncJSON
import Hasura.GraphQL.Execute.Backend
import Hasura.GraphQL.Logging
  ( ExecutionLog (..),
    ExecutionStats (..),
    GeneratedQuery (..),
    MonadExecutionLog (..),
    MonadQueryLog (..),
    QueryLog (..),
    QueryLogKind (QueryLogKindDatabase),
  )
import Hasura.GraphQL.Namespace (RootFieldAlias)
import Hasura.GraphQL.Transport.Backend
import Hasura.GraphQL.Transport.HTTP.Protocol
import Hasura.Logging qualified as L
import Hasura.Prelude
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType
import Hasura.SQL.AnyBackend (AnyBackend)
import Hasura.Server.Types (RequestId)
import Hasura.Session
import Hasura.Tracing

instance BackendTransport 'BigQuery where
  runDBQuery = runQuery
  runDBQueryExplain = runQueryExplain
  runDBMutation = runMutation
  runDBSubscription = error "Not supported."
  runDBStreamingSubscription = error "Not supported"

runQuery ::
  ( MonadIO m,
    MonadBaseControl IO m,
    MonadQueryLog m,
    MonadExecutionLog m,
    MonadTrace m,
    MonadError QErr m
  ) =>
  RequestId ->
  GQLReqUnparsed ->
  RootFieldAlias ->
  UserInfo ->
  L.Logger L.Hasura ->
  Maybe (CredentialCache AgentLicenseKey) ->
  SourceConfig 'BigQuery ->
  OnBaseMonad IdentityT (Maybe (AnyBackend ExecutionStats), EncJSON) ->
  Maybe Text ->
  ResolvedConnectionTemplate 'BigQuery ->
  -- | Also return the time spent in the PG query; for telemetry.
  m (DiffTime, EncJSON)
runQuery reqId query fieldName _userInfo logger _ _sourceConfig tx genSql _ = do
  -- log the generated SQL and the graphql query
  -- FIXME: fix logging by making logQueryLog expect something backend agnostic!
  logQueryLog logger $ mkQueryLog (QueryLogKindDatabase Nothing) query fieldName genSql reqId
  (diffTime, (stats, result)) <- withElapsedTime $ run tx

  logExecutionLog logger $ mkExecutionLog reqId stats

  pure (diffTime, result)

runQueryExplain ::
  ( MonadIO m,
    MonadBaseControl IO m,
    MonadError QErr m,
    MonadTrace m
  ) =>
  Maybe (CredentialCache AgentLicenseKey) ->
  DBStepInfo 'BigQuery ->
  m EncJSON
runQueryExplain _ (DBStepInfo _ _ _ action _) = fmap arResult (run action)

runMutation ::
  ( MonadError QErr m
  ) =>
  RequestId ->
  GQLReqUnparsed ->
  RootFieldAlias ->
  UserInfo ->
  L.Logger L.Hasura ->
  Maybe (CredentialCache AgentLicenseKey) ->
  SourceConfig 'BigQuery ->
  OnBaseMonad IdentityT EncJSON ->
  Maybe Text ->
  ResolvedConnectionTemplate 'BigQuery ->
  -- | Also return 'Mutation' when the operation was a mutation, and the time
  -- spent in the PG query; for telemetry.
  m (DiffTime, EncJSON)
runMutation _reqId _query _fieldName _userInfo _logger _ _sourceConfig _tx _genSql _ =
  -- do
  throw500 "BigQuery does not support mutations!"

run ::
  ( MonadIO m,
    MonadBaseControl IO m,
    MonadError QErr m,
    MonadTrace m
  ) =>
  OnBaseMonad IdentityT a ->
  m a
run = runIdentityT . runOnBaseMonad

-- @QueryLogKindDatabase Nothing@ means that the backend doesn't support connection templates
mkQueryLog ::
  QueryLogKind ->
  GQLReqUnparsed ->
  RootFieldAlias ->
  Maybe Text ->
  RequestId ->
  QueryLog
mkQueryLog _qlKind _qlQuery fieldName preparedSql _qlRequestId = QueryLog {..}
  where
    _qlGeneratedSql = preparedSql <&> \query -> (fieldName, GeneratedQuery query J.Null)

mkExecutionLog ::
  RequestId ->
  Maybe (AnyBackend ExecutionStats) ->
  ExecutionLog
mkExecutionLog _elRequestId _elStatistics = ExecutionLog {..}
