{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.DataConnector.Adapter.Transport () where

--------------------------------------------------------------------------------

import Control.Concurrent.STM
import Control.Exception.Safe (throwIO)
import Control.Monad.Trans.Control
import Data.Aeson qualified as J
import Data.Text.Extended ((<>>))
import Hasura.Backends.DataConnector.Adapter.Execute (DataConnectorPreparedQuery (..), encodePreparedQueryToJsonText)
import Hasura.Backends.DataConnector.Adapter.Types (SourceConfig (..))
import Hasura.Backends.DataConnector.Agent.Client (AgentClientContext (..), AgentClientT, AgentLicenseKey (..), runAgentClientT)
import Hasura.Base.Error (QErr (..))
import Hasura.CredentialCache
import Hasura.EncJSON (EncJSON)
import Hasura.GraphQL.Execute.Backend (DBStepInfo (..), OnBaseMonad (..), arResult)
import Hasura.GraphQL.Logging qualified as HGL
import Hasura.GraphQL.Namespace (RootFieldAlias)
import Hasura.GraphQL.Transport.Backend (BackendTransport (..))
import Hasura.GraphQL.Transport.HTTP.Protocol (GQLReqUnparsed)
import Hasura.Logging (Hasura, Logger, nullLogger)
import Hasura.Prelude
import Hasura.RQL.Types.Backend (ResolvedConnectionTemplate)
import Hasura.RQL.Types.BackendType (BackendType (DataConnector))
import Hasura.SQL.AnyBackend (AnyBackend)
import Hasura.Server.Types (RequestId)
import Hasura.Session (UserInfo)
import Hasura.Tracing qualified as Tracing

--------------------------------------------------------------------------------

instance BackendTransport 'DataConnector where
  runDBQuery = runDBQuery'
  runDBQueryExplain = runDBQueryExplain'
  runDBMutation = runDBMutation'
  runDBStreamingSubscription _ _ _ _ =
    liftIO . throwIO $ userError "runDBStreamingSubscription: not implemented for the Data Connector backend."
  runDBSubscription _ _ _ _ =
    liftIO . throwIO $ userError "runDBSubscription: not implemented for the Data Connector backend."

runDBQuery' ::
  ( MonadIO m,
    MonadBaseControl IO m,
    MonadError QErr m,
    Tracing.MonadTrace m,
    HGL.MonadQueryLog m
  ) =>
  RequestId ->
  GQLReqUnparsed ->
  RootFieldAlias ->
  UserInfo ->
  Logger Hasura ->
  Maybe (CredentialCache AgentLicenseKey) ->
  SourceConfig ->
  OnBaseMonad AgentClientT (Maybe (AnyBackend HGL.ExecutionStats), EncJSON) ->
  Maybe DataConnectorPreparedQuery ->
  ResolvedConnectionTemplate 'DataConnector ->
  m (DiffTime, EncJSON)
runDBQuery' requestId query fieldName _userInfo logger licenseKeyCacheMaybe sourceConfig@SourceConfig {..} action queryRequest _ = do
  agentAuthKey <-
    for licenseKeyCacheMaybe \licenseKeyCache -> do
      (key, _requestKeyRefresh) <- liftIO $ atomically $ getCredential licenseKeyCache
      -- TODO: If the license key has expired or is otherwise invalid, request a key refresh
      pure key

  -- TODO: Re-introduce this case statement once we no longer want to
  -- allow CE to attempt GDC requests.
  -- case (_cLicensing _scCapabilities, agentAuthKey) of
  --  (Just _, Nothing) -> throw401 "EE License Key Required."
  --  _ -> do
  void $ HGL.logQueryLog logger $ mkQueryLog query fieldName queryRequest requestId
  withElapsedTime
    . Tracing.newSpan ("Data Connector backend query for root field " <>> fieldName)
    . (<* Tracing.attachSourceConfigAttributes @'DataConnector sourceConfig)
    . flip runAgentClientT (AgentClientContext logger _scEndpoint _scManager _scTimeoutMicroseconds agentAuthKey)
    . runOnBaseMonad
    . fmap snd
    $ action

mkQueryLog ::
  GQLReqUnparsed ->
  RootFieldAlias ->
  Maybe DataConnectorPreparedQuery ->
  RequestId ->
  HGL.QueryLog
mkQueryLog gqlQuery fieldName maybeQuery requestId =
  HGL.QueryLog
    gqlQuery
    ((\query -> (fieldName, HGL.GeneratedQuery (encodePreparedQueryToJsonText query) J.Null)) <$> maybeQuery)
    requestId
    -- @QueryLogKindDatabase Nothing@ means that the backend doesn't support connection templates
    (HGL.QueryLogKindDatabase Nothing)

runDBQueryExplain' ::
  ( MonadIO m,
    MonadBaseControl IO m,
    MonadError QErr m,
    Tracing.MonadTrace m
  ) =>
  Maybe (CredentialCache AgentLicenseKey) ->
  DBStepInfo 'DataConnector ->
  m EncJSON
runDBQueryExplain' licenseKeyCacheMaybe (DBStepInfo _ SourceConfig {..} _ action _) = do
  agentAuthKey <-
    for licenseKeyCacheMaybe \licenseKeyCache -> do
      (key, _requestKeyRefresh) <- liftIO $ atomically $ getCredential licenseKeyCache
      -- TODO: If the license key has expired or is otherwise invalid, request a key refresh
      pure key
  -- TODO: Re-introduce this case statement once we no longer want to
  -- allow CE to attempt GDC requests.
  -- case (_cLicensing _scCapabilities, agentAuthKey) of
  --   (Just _, Nothing) -> throw401 "EE License Key Required."
  --   _ ->
  flip runAgentClientT (AgentClientContext nullLogger _scEndpoint _scManager _scTimeoutMicroseconds agentAuthKey)
    . fmap arResult
    $ runOnBaseMonad action

runDBMutation' ::
  ( MonadIO m,
    MonadBaseControl IO m,
    MonadError QErr m,
    Tracing.MonadTrace m,
    HGL.MonadQueryLog m
  ) =>
  RequestId ->
  GQLReqUnparsed ->
  RootFieldAlias ->
  UserInfo ->
  Logger Hasura ->
  Maybe (CredentialCache AgentLicenseKey) ->
  SourceConfig ->
  OnBaseMonad AgentClientT a ->
  Maybe DataConnectorPreparedQuery ->
  ResolvedConnectionTemplate 'DataConnector ->
  m (DiffTime, a)
runDBMutation' requestId query fieldName _userInfo logger licenseKeyCacheMaybe sourceConfig@SourceConfig {..} action queryRequest _ = do
  agentAuthKey <-
    for licenseKeyCacheMaybe \licenseKeyCache -> do
      (key, _requestKeyRefresh) <- liftIO $ atomically $ getCredential licenseKeyCache
      -- TODO: If the license key has expired or is otherwise invalid, request a key refresh
      pure key

  -- TODO: Re-introduce this case statement once we no longer want to
  -- allow CE to attempt GDC requests.
  -- case (_cLicensing _scCapabilities, agentAuthKey) of
  --   (Just _, Nothing) -> throw401 "EE License Key Required."
  --   _ -> do
  void $ HGL.logQueryLog logger $ mkQueryLog query fieldName queryRequest requestId
  withElapsedTime
    . Tracing.newSpan ("Data Connector backend mutation for root field " <>> fieldName)
    . (<* Tracing.attachSourceConfigAttributes @'DataConnector sourceConfig)
    . flip runAgentClientT (AgentClientContext logger _scEndpoint _scManager _scTimeoutMicroseconds agentAuthKey)
    . runOnBaseMonad
    $ action
