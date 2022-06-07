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
import Hasura.Tracing qualified as Tracing

--------------------------------------------------------------------------------

instance BackendExecute 'DataConnector where
  type PreparedQuery 'DataConnector = IR.Q.Query
  type MultiplexedQuery 'DataConnector = Void
  type ExecutionMonad 'DataConnector = Tracing.TraceT (ExceptT QErr IO)

  mkDBQueryPlan UserInfo {..} sourceName sourceConfig ir = do
    query' <- DC.mkPlan _uiSession sourceConfig ir
    pure
      DBStepInfo
        { dbsiSourceName = sourceName,
          dbsiSourceConfig = sourceConfig,
          dbsiPreparedQuery = Just query',
          dbsiAction = buildAction sourceName sourceConfig query'
        }

  mkDBQueryExplain fieldName UserInfo {..} sourceName sourceConfig ir = do
    query' <- DC.mkPlan _uiSession sourceConfig ir
    pure $
      mkAnyBackend @'DataConnector
        DBStepInfo
          { dbsiSourceName = sourceName,
            dbsiSourceConfig = sourceConfig,
            dbsiPreparedQuery = Just query',
            dbsiAction = pure . encJFromJValue . toExplainPlan fieldName $ query'
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

toExplainPlan :: GQL.RootFieldAlias -> IR.Q.Query -> ExplainPlan
toExplainPlan fieldName query' =
  ExplainPlan fieldName (Just "") (Just [TE.decodeUtf8 $ BL.toStrict $ J.encode $ query'])

buildAction :: RQL.SourceName -> DC.SourceConfig -> IR.Q.Query -> Tracing.TraceT (ExceptT QErr IO) EncJSON
buildAction sourceName DC.SourceConfig {..} query = do
  -- NOTE: Should this check occur during query construction in 'mkPlan'?
  when (DC.queryHasRelations query && not (API.dcRelationships _scCapabilities)) $
    throw400 NotSupported "Agents must provide their own dataloader."
  API.Routes {..} <- liftIO $ client @(Tracing.TraceT (ExceptT QErr IO)) _scManager _scEndpoint
  case IR.queryToAPI query of
    Right queryRequest -> do
      queryResponse <- _query (toTxt sourceName) _scConfig queryRequest
      pure $ encJFromJValue queryResponse
    Left (IR.ExposedLiteral lit) ->
      throw500 $ "Invalid query constructed: Exposed IR Literal '" <> lit <> "'."
