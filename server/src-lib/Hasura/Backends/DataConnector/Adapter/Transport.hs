{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.DataConnector.Adapter.Transport () where

--------------------------------------------------------------------------------

import Control.Exception.Safe (throwIO)
import Data.Aeson qualified as J
import Data.Text.Extended ((<>>))
import Hasura.Backends.DataConnector.Adapter.Execute ()
import Hasura.Backends.DataConnector.Adapter.Types (SourceConfig (..))
import Hasura.Backends.DataConnector.Agent.Client (AgentClientContext (..), AgentClientT, runAgentClientT)
import Hasura.Backends.DataConnector.IR.Query qualified as IR.Q
import Hasura.Backends.DataConnector.Plan qualified as DC
import Hasura.Base.Error (Code (NotSupported), QErr, throw400)
import Hasura.EncJSON (EncJSON)
import Hasura.GraphQL.Execute.Backend (DBStepInfo (..))
import Hasura.GraphQL.Logging qualified as HGL
import Hasura.GraphQL.Namespace (RootFieldAlias)
import Hasura.GraphQL.Transport.Backend (BackendTransport (..))
import Hasura.GraphQL.Transport.HTTP.Protocol (GQLReqUnparsed)
import Hasura.Logging (Hasura, Logger, nullLogger)
import Hasura.Prelude
import Hasura.SQL.Backend (BackendType (DataConnector))
import Hasura.Server.Types (RequestId)
import Hasura.Session (UserInfo)
import Hasura.Tracing qualified as Tracing

--------------------------------------------------------------------------------

instance BackendTransport 'DataConnector where
  runDBQuery = runDBQuery'
  runDBQueryExplain = runDBQueryExplain'
  runDBMutation _ _ _ _ _ _ _ _ =
    throw400 NotSupported "runDBMutation: not implemented for the Data Connector backend."
  runDBStreamingSubscription _ _ _ =
    liftIO . throwIO $ userError "runDBStreamingSubscription: not implemented for the Data Connector backend."
  runDBSubscription _ _ _ =
    liftIO . throwIO $ userError "runDBSubscription: not implemented for the Data Connector backend."

runDBQuery' ::
  ( MonadIO m,
    MonadError QErr m,
    Tracing.MonadTrace m,
    HGL.MonadQueryLog m
  ) =>
  RequestId ->
  GQLReqUnparsed ->
  RootFieldAlias ->
  UserInfo ->
  Logger Hasura ->
  SourceConfig ->
  AgentClientT (Tracing.TraceT (ExceptT QErr IO)) a ->
  Maybe IR.Q.QueryRequest ->
  m (DiffTime, a)
runDBQuery' requestId query fieldName _userInfo logger SourceConfig {..} action queryRequest = do
  void $ HGL.logQueryLog logger $ mkQueryLog query fieldName queryRequest requestId
  withElapsedTime
    . Tracing.trace ("Data Connector backend query for root field " <>> fieldName)
    . Tracing.interpTraceT (liftEitherM . liftIO . runExceptT)
    . flip runAgentClientT (AgentClientContext logger _scEndpoint _scManager _scTimeoutMicroseconds)
    $ action

mkQueryLog ::
  GQLReqUnparsed ->
  RootFieldAlias ->
  Maybe IR.Q.QueryRequest ->
  RequestId ->
  HGL.QueryLog
mkQueryLog gqlQuery fieldName maybeQuery requestId =
  HGL.QueryLog
    gqlQuery
    ((\query -> (fieldName, HGL.GeneratedQuery (DC.renderQuery query) J.Null)) <$> maybeQuery)
    requestId
    HGL.QueryLogKindDatabase

runDBQueryExplain' ::
  (MonadIO m, MonadError QErr m) =>
  DBStepInfo 'DataConnector ->
  m EncJSON
runDBQueryExplain' (DBStepInfo _ SourceConfig {..} _ action) =
  liftEitherM . liftIO
    . runExceptT
    . Tracing.runTraceTWithReporter Tracing.noReporter "explain"
    . flip runAgentClientT (AgentClientContext nullLogger _scEndpoint _scManager _scTimeoutMicroseconds)
    $ action
