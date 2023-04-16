{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.MySQL.Instances.Transport (runQuery) where

import Control.Monad.Trans.Control
import Data.Aeson qualified as J
import Data.Text.Extended
import Hasura.Backends.DataConnector.Agent.Client (AgentLicenseKey)
import Hasura.Backends.MySQL.Instances.Execute ()
import Hasura.Base.Error
import Hasura.CredentialCache
import Hasura.EncJSON
import Hasura.GraphQL.Execute.Backend
import Hasura.GraphQL.Logging
import Hasura.GraphQL.Namespace (RootFieldAlias)
import Hasura.GraphQL.Transport.Backend
import Hasura.GraphQL.Transport.HTTP.Protocol
import Hasura.Logging qualified as L
import Hasura.Prelude
import Hasura.RQL.Types.Backend
import Hasura.SQL.AnyBackend (AnyBackend)
import Hasura.SQL.Backend
import Hasura.Server.Types (RequestId)
import Hasura.Session
import Hasura.Tracing

instance BackendTransport 'MySQL where
  runDBQuery = runQuery
  runDBQueryExplain = runQueryExplain
  runDBMutation = error "runDBMutation: MySQL backend does not support this operation yet."
  runDBSubscription = error "runDBSubscription: MySQL backend does not support this operation yet."
  runDBStreamingSubscription = error "runDBStreamingSubscription: MySQL backend does not support this operation yet"

runQuery ::
  ( MonadIO m,
    MonadBaseControl IO m,
    MonadQueryLog m,
    MonadTrace m,
    MonadError QErr m
  ) =>
  RequestId ->
  GQLReqUnparsed ->
  RootFieldAlias ->
  UserInfo ->
  L.Logger L.Hasura ->
  Maybe (CredentialCache AgentLicenseKey) ->
  SourceConfig 'MySQL ->
  OnBaseMonad IdentityT (Maybe (AnyBackend ExecutionStats), EncJSON) ->
  Maybe (PreparedQuery 'MySQL) ->
  ResolvedConnectionTemplate 'MySQL ->
  -- | Also return the time spent in the PG query; for telemetry.
  m (DiffTime, EncJSON)
runQuery reqId query fieldName _userInfo logger _ _sourceConfig tx genSql _ = do
  logQueryLog logger $ mkQueryLog query fieldName genSql reqId
  withElapsedTime $
    newSpan ("MySQL Query for root field " <>> fieldName) $
      fmap snd (run tx)

runQueryExplain ::
  ( MonadIO m,
    MonadBaseControl IO m,
    MonadError QErr m,
    MonadTrace m
  ) =>
  Maybe (CredentialCache AgentLicenseKey) ->
  DBStepInfo 'MySQL ->
  m EncJSON
runQueryExplain _ (DBStepInfo _ _ _ action _) = fmap arResult (run action)

run :: (MonadIO m, MonadBaseControl IO m, MonadError QErr m, MonadTrace m) => OnBaseMonad IdentityT a -> m a
run = runIdentityT . runOnBaseMonad

mkQueryLog ::
  GQLReqUnparsed ->
  RootFieldAlias ->
  Maybe (PreparedQuery 'MySQL) ->
  RequestId ->
  QueryLog
mkQueryLog gqlQuery fieldName preparedSql requestId =
  -- @QueryLogKindDatabase Nothing@ means that the backend doesn't support connection templates
  QueryLog gqlQuery ((fieldName,) <$> generatedQuery) requestId (QueryLogKindDatabase Nothing)
  where
    generatedQuery =
      preparedSql <&> \queryString ->
        GeneratedQuery queryString J.Null
