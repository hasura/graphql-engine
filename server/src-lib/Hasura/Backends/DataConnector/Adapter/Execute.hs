{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.DataConnector.Adapter.Execute
  (
  )
where

--------------------------------------------------------------------------------

import Data.Aeson qualified as J
import Data.ByteString.Lazy qualified as BL
import Data.Text.Encoding qualified as TE
import Data.Text.Extended (toTxt)
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Backends.DataConnector.Adapter.Types (SourceConfig (_scCapabilities))
import Hasura.Backends.DataConnector.Agent.Client
import Hasura.Backends.DataConnector.IR.Export as IR
import Hasura.Backends.DataConnector.IR.Query qualified as IR.Q
import Hasura.Backends.DataConnector.Plan qualified as DC
import Hasura.Base.Error (Code (..), QErr, throw400, throw500)
import Hasura.EncJSON (EncJSON, encJFromJValue)
import Hasura.GraphQL.Execute.Backend (BackendExecute (..), DBStepInfo (..), ExplainPlan (..))
import Hasura.GraphQL.Namespace qualified as GQL
import Hasura.Prelude
import Hasura.RQL.Types.Common qualified as RQL
import Hasura.SQL.AnyBackend (mkAnyBackend)
import Hasura.SQL.Backend (BackendType (DataConnector))
import Hasura.Session
import Hasura.Tracing (MonadTrace)
import Hasura.Tracing qualified as Tracing
import Servant.Client.Generic (genericClient)

--------------------------------------------------------------------------------

instance BackendExecute 'DataConnector where
  type PreparedQuery 'DataConnector = IR.Q.QueryRequest
  type MultiplexedQuery 'DataConnector = Void
  type ExecutionMonad 'DataConnector = AgentClientT (Tracing.TraceT (ExceptT QErr IO))

  mkDBQueryPlan UserInfo {..} sourceName sourceConfig ir = do
    queryRequest <- DC.mkPlan _uiSession sourceConfig ir
    pure
      DBStepInfo
        { dbsiSourceName = sourceName,
          dbsiSourceConfig = sourceConfig,
          dbsiPreparedQuery = Just queryRequest,
          dbsiAction = buildAction sourceName sourceConfig queryRequest
        }

  mkDBQueryExplain fieldName UserInfo {..} sourceName sourceConfig ir = do
    queryRequest <- DC.mkPlan _uiSession sourceConfig ir
    pure $
      mkAnyBackend @'DataConnector
        DBStepInfo
          { dbsiSourceName = sourceName,
            dbsiSourceConfig = sourceConfig,
            dbsiPreparedQuery = Just queryRequest,
            dbsiAction = pure . encJFromJValue . toExplainPlan fieldName $ queryRequest
          }
  mkDBMutationPlan _ _ _ _ _ =
    throw400 NotSupported "mkDBMutationPlan: not implemented for the Data Connector backend."
  mkLiveQuerySubscriptionPlan _ _ _ _ _ =
    throw400 NotSupported "mkLiveQuerySubscriptionPlan: not implemented for the Data Connector backend."
  mkDBStreamingSubscriptionPlan _ _ _ _ =
    throw400 NotSupported "mkLiveQuerySubscriptionPlan: not implemented for the Data Connector backend."
  mkDBRemoteRelationshipPlan _ _ _ _ _ _ _ =
    throw500 "mkDBRemoteRelationshipPlan: not implemented for the Data Connector backend."
  mkSubscriptionExplain _ =
    throw400 NotSupported "mkSubscriptionExplain: not implemented for the Data Connector backend."

toExplainPlan :: GQL.RootFieldAlias -> IR.Q.QueryRequest -> ExplainPlan
toExplainPlan fieldName queryRequest =
  ExplainPlan fieldName (Just "") (Just [TE.decodeUtf8 $ BL.toStrict $ J.encode $ queryRequest])

buildAction :: (MonadIO m, MonadTrace m, MonadError QErr m) => RQL.SourceName -> DC.SourceConfig -> IR.Q.QueryRequest -> AgentClientT m EncJSON
buildAction sourceName DC.SourceConfig {..} queryRequest = do
  -- NOTE: Should this check occur during query construction in 'mkPlan'?
  when (DC.queryHasRelations queryRequest && isNothing (API.cRelationships _scCapabilities)) $
    throw400 NotSupported "Agents must provide their own dataloader."
  let API.Routes {..} = genericClient
  case IR.queryRequestToAPI queryRequest of
    Right queryRequest' -> do
      queryResponse <- _query (toTxt sourceName) _scConfig queryRequest'
      pure $ encJFromJValue queryResponse
    Left (IR.ExposedLiteral lit) ->
      throw500 $ "Invalid query constructed: Exposed IR Literal '" <> lit <> "'."
