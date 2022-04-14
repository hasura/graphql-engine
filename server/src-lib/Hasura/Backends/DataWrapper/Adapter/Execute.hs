{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.DataWrapper.Adapter.Execute
  (
  )
where

--------------------------------------------------------------------------------

import Data.Aeson qualified as J
import Data.ByteString.Lazy qualified as BL
import Data.Text.Encoding qualified as TE
import Hasura.Backends.DataWrapper.API (Capabilities (dcRelationships), Routes (..), SchemaResponse (srCapabilities))
import Hasura.Backends.DataWrapper.Agent.Client
import Hasura.Backends.DataWrapper.IR.Query qualified as IR
import Hasura.Backends.DataWrapper.Plan qualified as GDW
import Hasura.Base.Error (Code (NotSupported), QErr, throw400, throw500)
import Hasura.EncJSON (EncJSON, encJFromJValue)
import Hasura.GraphQL.Execute.Backend (BackendExecute (..), DBStepInfo (..), ExplainPlan (..))
import Hasura.GraphQL.Namespace qualified as GQL
import Hasura.Prelude
import Hasura.SQL.AnyBackend (mkAnyBackend)
import Hasura.SQL.Backend (BackendType (DataWrapper))
import Hasura.Session
import Hasura.Tracing qualified as Tracing
import Witch qualified (from)

--------------------------------------------------------------------------------

instance BackendExecute 'DataWrapper where
  type PreparedQuery 'DataWrapper = GDW.Plan
  type MultiplexedQuery 'DataWrapper = Void
  type ExecutionMonad 'DataWrapper = Tracing.TraceT (ExceptT QErr IO)

  mkDBQueryPlan UserInfo {..} sourceName sourceConfig ir = do
    plan' <- GDW.mkPlan _uiSession sourceConfig ir
    pure
      DBStepInfo
        { dbsiSourceName = sourceName,
          dbsiSourceConfig = sourceConfig,
          dbsiPreparedQuery = Just plan',
          dbsiAction = buildAction sourceConfig (GDW.query plan')
        }

  mkDBQueryExplain fieldName UserInfo {..} sourceName sourceConfig ir = do
    plan' <- GDW.mkPlan _uiSession sourceConfig ir
    pure $
      mkAnyBackend @'DataWrapper
        DBStepInfo
          { dbsiSourceName = sourceName,
            dbsiSourceConfig = sourceConfig,
            dbsiPreparedQuery = Just plan',
            dbsiAction = pure . encJFromJValue . toExplainPlan fieldName $ plan'
          }
  mkDBMutationPlan _ _ _ _ _ =
    throw400 NotSupported "mkDBMutationPlan: not implemented for GraphQL Data Wrappers."
  mkLiveQuerySubscriptionPlan _ _ _ _ _ =
    throw400 NotSupported "mkLiveQuerySubscriptionPlan: not implemented for GraphQL Data Wrappers."
  mkDBStreamingSubscriptionPlan _ _ _ _ =
    throw400 NotSupported "mkLiveQuerySubscriptionPlan: not implemented for GraphQL Data Wrappers."
  mkDBRemoteRelationshipPlan _ _ _ _ _ _ _ =
    throw500 "mkDBRemoteRelationshipPlan: not implemented for GraphQL Data Wrappers."
  mkSubscriptionExplain _ =
    throw400 NotSupported "mkSubscriptionExplain: not implemented for GraphQL Data Wrappers."

toExplainPlan :: GQL.RootFieldAlias -> GDW.Plan -> ExplainPlan
toExplainPlan fieldName plan_ =
  ExplainPlan fieldName (Just "") (Just [TE.decodeUtf8 $ BL.toStrict $ J.encode $ GDW.query $ plan_])

buildAction :: GDW.SourceConfig -> IR.Query -> Tracing.TraceT (ExceptT QErr IO) EncJSON
buildAction GDW.SourceConfig {..} query = do
  -- TODO(SOLOMON): Should this check occur during query construction in 'mkPlan'?
  when (GDW.queryHasRelations query && not (dcRelationships (srCapabilities dscSchema))) $
    throw400 NotSupported "Agents must provide their own dataloader."
  Routes {..} <- liftIO $ client @(Tracing.TraceT (ExceptT QErr IO)) dscManager (ConnSourceConfig dscEndpoint)
  queryResponse <- _query $ Witch.from query
  pure $ encJFromJValue queryResponse
