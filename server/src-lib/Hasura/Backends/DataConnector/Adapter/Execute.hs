{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.DataConnector.Adapter.Execute
  ( DataConnectorPreparedQuery (..),
    encodePreparedQueryToJsonText,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson qualified as J
import Data.ByteString.Lazy qualified as BL
import Data.Text.Encoding qualified as TE
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Backends.DataConnector.Adapter.ConfigTransform (transformSourceConfig)
import Hasura.Backends.DataConnector.Adapter.Types (SourceConfig (..))
import Hasura.Backends.DataConnector.Agent.Client (AgentClientT)
import Hasura.Backends.DataConnector.Agent.Client qualified as Client
import Hasura.Backends.DataConnector.Plan.Common (Plan (..))
import Hasura.Backends.DataConnector.Plan.MutationPlan qualified as Plan
import Hasura.Backends.DataConnector.Plan.QueryPlan qualified as Plan
import Hasura.Backends.DataConnector.Plan.RemoteRelationshipPlan qualified as Plan
import Hasura.Base.Error (Code (..), QErr, throw400)
import Hasura.EncJSON (EncJSON, encJFromJEncoding, encJFromJValue)
import Hasura.GraphQL.Execute.Backend (BackendExecute (..), DBStepInfo (..), ExplainPlan (..), OnBaseMonad (..), withNoStatistics)
import Hasura.GraphQL.Namespace qualified as GQL
import Hasura.Prelude
import Hasura.RQL.Types.BackendType (BackendType (DataConnector))
import Hasura.RQL.Types.Common qualified as RQL
import Hasura.SQL.AnyBackend (mkAnyBackend)
import Hasura.Session
import Hasura.Tracing (MonadTrace)
import Hasura.Tracing qualified as Tracing

data DataConnectorPreparedQuery
  = QueryRequest API.QueryRequest
  | MutationRequest API.MutationRequest

encodePreparedQueryToJsonText :: DataConnectorPreparedQuery -> Text
encodePreparedQueryToJsonText = \case
  QueryRequest req -> encodeToJsonText req
  MutationRequest req -> encodeToJsonText req

encodeToJsonText :: (J.ToJSON a) => a -> Text
encodeToJsonText =
  TE.decodeUtf8 . BL.toStrict . J.encode

--------------------------------------------------------------------------------

instance BackendExecute 'DataConnector where
  type PreparedQuery 'DataConnector = DataConnectorPreparedQuery
  type MultiplexedQuery 'DataConnector = Void
  type ExecutionMonad 'DataConnector = AgentClientT

  mkDBQueryPlan UserInfo {..} sourceName sourceConfig ir _headers _gName = do
    queryPlan@Plan {..} <- Plan.mkQueryPlan _uiSession sourceConfig ir
    transformedSourceConfig <- transformSourceConfig sourceConfig (Just _uiSession)
    pure
      DBStepInfo
        { dbsiSourceName = sourceName,
          dbsiSourceConfig = transformedSourceConfig,
          dbsiPreparedQuery = Just $ QueryRequest _pRequest,
          dbsiAction = OnBaseMonad $ fmap withNoStatistics (buildQueryAction sourceName transformedSourceConfig queryPlan),
          dbsiResolvedConnectionTemplate = ()
        }

  mkDBQueryExplain fieldName UserInfo {..} sourceName sourceConfig ir _headers _gName = do
    queryPlan@Plan {..} <- Plan.mkQueryPlan _uiSession sourceConfig ir
    transformedSourceConfig <- transformSourceConfig sourceConfig (Just _uiSession)
    pure
      $ mkAnyBackend @'DataConnector
        DBStepInfo
          { dbsiSourceName = sourceName,
            dbsiSourceConfig = transformedSourceConfig,
            dbsiPreparedQuery = Just $ QueryRequest _pRequest,
            dbsiAction = OnBaseMonad $ fmap withNoStatistics (buildExplainAction fieldName sourceName transformedSourceConfig queryPlan),
            dbsiResolvedConnectionTemplate = ()
          }

  mkDBMutationPlan UserInfo {..} _stringifyNum sourceName sourceConfig mutationDB _headers _gName = do
    mutationPlan@Plan {..} <- Plan.mkMutationPlan _uiSession mutationDB
    transformedSourceConfig <- transformSourceConfig sourceConfig (Just _uiSession)
    pure
      DBStepInfo
        { dbsiSourceName = sourceName,
          dbsiSourceConfig = transformedSourceConfig,
          dbsiPreparedQuery = Just $ MutationRequest _pRequest,
          dbsiAction = OnBaseMonad $ fmap withNoStatistics (buildMutationAction sourceName transformedSourceConfig mutationPlan),
          dbsiResolvedConnectionTemplate = ()
        }

  mkLiveQuerySubscriptionPlan _ _ _ _ _ _ _ =
    throw400 NotSupported "mkLiveQuerySubscriptionPlan: not implemented for the Data Connector backend."

  mkDBStreamingSubscriptionPlan _ _ _ _ _ _ =
    throw400 NotSupported "mkLiveQuerySubscriptionPlan: not implemented for the Data Connector backend."

  mkDBRemoteRelationshipPlan UserInfo {..} sourceName sourceConfig joinIds joinIdsSchema argumentIdFieldName (resultFieldName, ir) _ _ _ = do
    remoteRelationshipPlan@Plan {..} <- Plan.mkRemoteRelationshipPlan _uiSession sourceConfig joinIds joinIdsSchema argumentIdFieldName resultFieldName ir
    transformedSourceConfig <- transformSourceConfig sourceConfig (Just _uiSession)
    pure
      DBStepInfo
        { dbsiSourceName = sourceName,
          dbsiSourceConfig = transformedSourceConfig,
          dbsiPreparedQuery = Just $ QueryRequest _pRequest,
          dbsiAction = OnBaseMonad $ fmap withNoStatistics (buildQueryAction sourceName transformedSourceConfig remoteRelationshipPlan),
          dbsiResolvedConnectionTemplate = ()
        }

  mkSubscriptionExplain _ =
    throw400 NotSupported "mkSubscriptionExplain: not implemented for the Data Connector backend."

buildQueryAction :: (MonadIO m, MonadTrace m, MonadError QErr m) => RQL.SourceName -> SourceConfig -> Plan API.QueryRequest API.QueryResponse -> AgentClientT m EncJSON
buildQueryAction sourceName SourceConfig {..} Plan {..} = do
  queryResponse <- Client.query sourceName _scConfig _pRequest
  reshapedResponse <- Tracing.newSpan "QueryResponse reshaping" $ _pResponseReshaper queryResponse
  pure $ encJFromJEncoding reshapedResponse

-- Delegates the generation to the Agent's /explain endpoint if it has that capability,
-- otherwise, returns the IR sent to the agent.
buildExplainAction :: (MonadIO m, MonadTrace m, MonadError QErr m) => GQL.RootFieldAlias -> RQL.SourceName -> SourceConfig -> Plan API.QueryRequest API.QueryResponse -> AgentClientT m EncJSON
buildExplainAction fieldName sourceName SourceConfig {..} Plan {..} =
  case API._cExplain _scCapabilities of
    Nothing -> pure . encJFromJValue . toExplainPlan fieldName $ _pRequest
    Just API.ExplainCapabilities -> do
      explainResponse <- Client.explain sourceName _scConfig _pRequest
      pure
        . encJFromJValue
        $ ExplainPlan
          fieldName
          (Just (API._erQuery explainResponse))
          (Just (API._erLines explainResponse))

toExplainPlan :: GQL.RootFieldAlias -> API.QueryRequest -> ExplainPlan
toExplainPlan fieldName queryRequest =
  ExplainPlan fieldName (Just "") (Just [encodeToJsonText queryRequest])

buildMutationAction :: (MonadIO m, MonadTrace m, MonadError QErr m) => RQL.SourceName -> SourceConfig -> Plan API.MutationRequest API.MutationResponse -> AgentClientT m EncJSON
buildMutationAction sourceName SourceConfig {..} Plan {..} = do
  mutationResponse <- Client.mutation sourceName _scConfig _pRequest
  reshapedResponse <- Tracing.newSpan "MutationResponse reshaping" $ _pResponseReshaper mutationResponse
  pure $ encJFromJEncoding reshapedResponse
