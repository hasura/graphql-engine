-- | The RQL metadata query ('/v1/metadata')
module Hasura.Server.API.Metadata
  ( RQLMetadata,
    RQLMetadataV1 (..),
    runMetadataQuery,
  )
where

import Control.Lens (_Just)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson
import Data.Environment qualified as Env
import Data.Has (Has)
import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as T
import GHC.Generics.Extended (constrName)
import Hasura.App.State
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.Eventing.Backend
import Hasura.Function.API qualified as Functions
import Hasura.GraphQL.Schema.Common (SchemaSampledFeatureFlags)
import Hasura.GraphQL.Transport.WebSocket qualified as WS
import Hasura.Logging qualified as L
import Hasura.LogicalModel.API qualified as LogicalModel
import Hasura.Metadata.Class
import Hasura.NativeQuery.API qualified as NativeQueries
import Hasura.Prelude hiding (first)
import Hasura.RQL.DDL.Action
import Hasura.RQL.DDL.Action.Lenses (caDefinition, uaDefinition)
import Hasura.RQL.DDL.ApiLimit
import Hasura.RQL.DDL.ComputedField
import Hasura.RQL.DDL.ConnectionTemplate
import Hasura.RQL.DDL.CustomTypes
import Hasura.RQL.DDL.DataConnector
import Hasura.RQL.DDL.Endpoint
import Hasura.RQL.DDL.EventTrigger
import Hasura.RQL.DDL.GraphqlSchemaIntrospection
import Hasura.RQL.DDL.InheritedRoles
import Hasura.RQL.DDL.Metadata
import Hasura.RQL.DDL.Network
import Hasura.RQL.DDL.OpenTelemetry
import Hasura.RQL.DDL.Permission
import Hasura.RQL.DDL.QueryCollection
import Hasura.RQL.DDL.QueryTags
import Hasura.RQL.DDL.Relationship
import Hasura.RQL.DDL.Relationship.Rename
import Hasura.RQL.DDL.Relationship.Suggest
import Hasura.RQL.DDL.RemoteRelationship
import Hasura.RQL.DDL.ScheduledTrigger
import Hasura.RQL.DDL.Schema
import Hasura.RQL.DDL.Schema.Cache.Config
import Hasura.RQL.DDL.Schema.Source
import Hasura.RQL.DDL.SourceKinds
import Hasura.RQL.DDL.Webhook.Transform.Validation
import Hasura.RQL.Types.Action
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Metadata (Metadata, MetadataModifier (MetadataModifier), emptyMetadataDefaults)
import Hasura.RQL.Types.Metadata.Backend
import Hasura.RQL.Types.Metadata.Object (MetadataObjId)
import Hasura.RQL.Types.Permission
import Hasura.RQL.Types.ScheduledTrigger
import Hasura.RQL.Types.Schema.Options qualified as Options
import Hasura.RQL.Types.SchemaCache
import Hasura.RQL.Types.SchemaCache.Build
import Hasura.RQL.Types.Source
import Hasura.RemoteSchema.MetadataAPI
import Hasura.SQL.AnyBackend
import Hasura.Server.API.Instances ()
import Hasura.Server.API.Metadata.Instances ()
import Hasura.Server.API.Metadata.Types
import Hasura.Server.Init.FeatureFlag (HasFeatureFlagChecker)
import Hasura.Server.Logging (SchemaSyncLog (..), SchemaSyncThreadType (TTMetadataApi))
import Hasura.Server.Types
import Hasura.Services
import Hasura.Session
import Hasura.StoredProcedure.API qualified as StoredProcedures
import Hasura.Tracing qualified as Tracing

-- | The payload for the @/v1/metadata@ endpoint. See:
--
-- https://hasura.io/docs/latest/graphql/core/api-reference/metadata-api/index/
data RQLMetadata = RQLMetadata
  { _rqlMetadataResourceVersion :: !(Maybe MetadataResourceVersion),
    _rqlMetadata :: !RQLMetadataRequest
  }

instance FromJSON RQLMetadata where
  parseJSON = withObject "RQLMetadata" $ \o -> do
    _rqlMetadataResourceVersion <- o .:? "resource_version"
    _rqlMetadata <- parseJSON $ Object o
    pure RQLMetadata {..}

runMetadataQuery ::
  ( MonadIO m,
    MonadError QErr m,
    MonadBaseControl IO m,
    HasAppEnv m,
    HasCacheStaticConfig m,
    Tracing.MonadTrace m,
    MonadMetadataStorage m,
    MonadResolveSource m,
    MonadEventLogCleanup m,
    ProvidesHasuraServices m,
    MonadGetPolicies m,
    UserInfoM m
  ) =>
  AppContext ->
  RebuildableSchemaCache ->
  WS.WebsocketCloseOnMetadataChangeAction ->
  RQLMetadata ->
  m (EncJSON, RebuildableSchemaCache)
runMetadataQuery appContext schemaCache closeWebsocketsOnMetadataChange RQLMetadata {..} = do
  AppEnv {..} <- askAppEnv
  let logger = _lsLogger appEnvLoggers
  MetadataWithResourceVersion metadata currentResourceVersion <- Tracing.newSpan "fetchMetadata" $ liftEitherM fetchMetadata
  let exportsMetadata = \case
        RMV1 (RMExportMetadata _) -> True
        RMV2 (RMV2ExportMetadata _) -> True
        _ -> False
      metadataDefaults =
        -- Note: The following check is performed to determine if the metadata defaults can
        --       be safely merged into the reader at this point.
        --
        -- We want to prevent scenarios:
        -- \* Exporting defaults - Contradicting the "roundtrip" principle of metadata operations
        -- \* Serializing defaults into the metadata storage - Putting data into the users hdb_catalog
        --
        -- While this check does have the desired effect it relies on the fact that the only
        -- operations that need access to the defaults here do not export or modify metadata.
        -- If at some point in future an operation needs access to the defaults and also needs to
        -- export/modify metadata, then another approach will need to be taken.
        --
        -- Luckily, most actual need for defaults access exists within the schema cache build phase,
        -- so metadata operations don't need "smarts" that require defaults access.
        --
        if (exportsMetadata _rqlMetadata || queryModifiesMetadata _rqlMetadata)
          then emptyMetadataDefaults
          else acMetadataDefaults appContext
  let dynamicConfig = buildCacheDynamicConfig appContext
  ((r, modMetadata), modSchemaCache, cacheInvalidations, sourcesIntrospection, schemaRegistryAction) <-
    runMetadataQueryM
      (acEnvironment appContext)
      (acSchemaSampledFeatureFlags appContext)
      (acRemoteSchemaPermsCtx appContext)
      currentResourceVersion
      _rqlMetadata
      -- TODO: remove this straight runReaderT that provides no actual new info
      & flip runReaderT logger
      & runMetadataT metadata metadataDefaults
      & runCacheRWT dynamicConfig schemaCache
  -- set modified metadata in storage
  if queryModifiesMetadata _rqlMetadata
    then case (appEnvEnableMaintenanceMode, appEnvEnableReadOnlyMode) of
      (MaintenanceModeDisabled, ReadOnlyModeDisabled) -> do
        -- set modified metadata in storage
        L.unLoggerTracing logger
          $ SchemaSyncLog L.LevelInfo TTMetadataApi
          $ String
          $ "Attempting to insert new metadata in storage"
        newResourceVersion <-
          Tracing.newSpan "setMetadata"
            $ liftEitherM
            $ setMetadata (fromMaybe currentResourceVersion _rqlMetadataResourceVersion) modMetadata
        L.unLoggerTracing logger
          $ SchemaSyncLog L.LevelInfo TTMetadataApi
          $ String
          $ "Successfully inserted new metadata in storage with resource version: "
          <> showMetadataResourceVersion newResourceVersion

        -- save sources introspection to stored-introspection DB
        Tracing.newSpan "storeSourcesIntrospection"
          $ saveSourcesIntrospection logger sourcesIntrospection newResourceVersion

        -- run the schema registry action
        Tracing.newSpan "runSchemaRegistryAction"
          $ for_ schemaRegistryAction
          $ \action -> do
            liftIO $ action newResourceVersion (scInconsistentObjs (lastBuiltSchemaCache modSchemaCache)) modMetadata

        -- notify schema cache sync
        Tracing.newSpan "notifySchemaCacheSync"
          $ liftEitherM
          $ notifySchemaCacheSync newResourceVersion appEnvInstanceId cacheInvalidations
        L.unLoggerTracing logger
          $ SchemaSyncLog L.LevelInfo TTMetadataApi
          $ String
          $ "Inserted schema cache sync notification at resource version:"
          <> showMetadataResourceVersion newResourceVersion

        (_, modSchemaCache', _, _, _) <-
          Tracing.newSpan "setMetadataResourceVersionInSchemaCache"
            $ setMetadataResourceVersionInSchemaCache newResourceVersion
            & runCacheRWT dynamicConfig modSchemaCache

        -- Close all subscriptions with 1012 code (subscribers should reconnect)
        -- and close poller threads
        when ((_cdcCloseWebsocketsOnMetadataChangeStatus dynamicConfig) == CWMCEnabled)
          $ Tracing.newSpan "closeWebsocketsOnMetadataChange"
          $ liftIO
          $ WS.runWebsocketCloseOnMetadataChangeAction closeWebsocketsOnMetadataChange

        pure (r, modSchemaCache')
      (MaintenanceModeEnabled (), ReadOnlyModeDisabled) ->
        throw500 "metadata cannot be modified in maintenance mode"
      (MaintenanceModeDisabled, ReadOnlyModeEnabled) ->
        throw400 NotSupported "metadata cannot be modified in read-only mode"
      (MaintenanceModeEnabled (), ReadOnlyModeEnabled) ->
        throw500 "metadata cannot be modified in maintenance mode"
    else pure (r, modSchemaCache)

queryModifiesMetadata :: RQLMetadataRequest -> Bool
queryModifiesMetadata = \case
  RMV1 q ->
    case q of
      RMRedeliverEvent _ -> False
      RMInvokeEventTrigger _ -> False
      RMGetEventLogs _ -> False
      RMGetEventInvocationLogs _ -> False
      RMGetEventById _ -> False
      RMGetInconsistentMetadata _ -> False
      RMIntrospectRemoteSchema _ -> False
      RMDumpInternalState _ -> False
      RMSetCatalogState _ -> False
      RMGetCatalogState _ -> False
      RMExportMetadata _ -> False
      RMGetScheduledEventInvocations _ -> False
      RMGetCronTriggers -> False
      RMGetScheduledEvents _ -> False
      RMCreateScheduledEvent _ -> False
      RMDeleteScheduledEvent _ -> False
      RMTestWebhookTransform _ -> False
      RMGetSourceKindCapabilities _ -> False
      RMListSourceKinds _ -> False
      RMGetSourceTables _ -> False
      RMGetSourceTrackables _ -> False
      RMGetTableInfo _ -> False
      RMGetTableInfo_ _ -> False
      RMTestConnectionTemplate _ -> False
      RMSuggestRelationships _ -> False
      RMGetNativeQuery _ -> False
      RMTrackNativeQuery _ -> True
      RMUntrackNativeQuery _ -> True
      RMGetStoredProcedure _ -> False
      RMTrackStoredProcedure _ -> True
      RMUntrackStoredProcedure _ -> True
      RMGetLogicalModel _ -> False
      RMTrackLogicalModel _ -> True
      RMUntrackLogicalModel _ -> True
      RMCreateSelectLogicalModelPermission _ -> True
      RMDropSelectLogicalModelPermission _ -> True
      RMBulk qs -> any queryModifiesMetadata qs
      RMBulkKeepGoing qs -> any queryModifiesMetadata qs
      RMBulkAtomic qs -> any queryModifiesMetadata qs
      -- We used to assume that the fallthrough was True,
      -- but it is better to be explicit here to warn when new constructors are added.
      RMAddSource _ -> True
      RMDropSource _ -> True
      RMRenameSource _ -> True
      RMUpdateSource _ -> True
      RMTrackTable _ -> True
      RMTrackTables _ -> True
      RMUntrackTable _ -> True
      RMUntrackTables _ -> True
      RMSetTableCustomization _ -> True
      RMSetApolloFederationConfig _ -> True
      RMPgSetTableIsEnum _ -> True
      RMCreateInsertPermission _ -> True
      RMCreateSelectPermission _ -> True
      RMCreateUpdatePermission _ -> True
      RMCreateDeletePermission _ -> True
      RMDropInsertPermission _ -> True
      RMDropSelectPermission _ -> True
      RMDropUpdatePermission _ -> True
      RMDropDeletePermission _ -> True
      RMSetPermissionComment _ -> True
      RMCreateObjectRelationship _ -> True
      RMCreateArrayRelationship _ -> True
      RMDropRelationship _ -> True
      RMSetRelationshipComment _ -> True
      RMRenameRelationship _ -> True
      RMCreateRemoteRelationship _ -> True
      RMUpdateRemoteRelationship _ -> True
      RMDeleteRemoteRelationship _ -> True
      RMTrackFunction _ -> True
      RMUntrackFunction _ -> True
      RMSetFunctionCustomization _ -> True
      RMCreateFunctionPermission _ -> True
      RMDropFunctionPermission _ -> True
      RMAddComputedField _ -> True
      RMDropComputedField _ -> True
      RMCreateEventTrigger _ -> True
      RMDeleteEventTrigger _ -> True
      RMCleanupEventTriggerLog _ -> True
      RMResumeEventTriggerCleanup _ -> True
      RMPauseEventTriggerCleanup _ -> True
      RMAddRemoteSchema _ -> True
      RMUpdateRemoteSchema _ -> True
      RMRemoveRemoteSchema _ -> True
      RMReloadRemoteSchema _ -> True
      RMAddRemoteSchemaPermissions _ -> True
      RMDropRemoteSchemaPermissions _ -> True
      RMCreateRemoteSchemaRemoteRelationship _ -> True
      RMUpdateRemoteSchemaRemoteRelationship _ -> True
      RMDeleteRemoteSchemaRemoteRelationship _ -> True
      RMCreateCronTrigger _ -> True
      RMDeleteCronTrigger _ -> True
      RMCreateAction _ -> True
      RMDropAction _ -> True
      RMUpdateAction _ -> True
      RMCreateActionPermission _ -> True
      RMDropActionPermission _ -> True
      RMCreateQueryCollection _ -> True
      RMRenameQueryCollection _ -> True
      RMDropQueryCollection _ -> True
      RMAddQueryToCollection _ -> True
      RMDropQueryFromCollection _ -> True
      RMAddCollectionToAllowlist _ -> True
      RMDropCollectionFromAllowlist _ -> True
      RMUpdateScopeOfCollectionInAllowlist _ -> True
      RMCreateRestEndpoint _ -> True
      RMDropRestEndpoint _ -> True
      RMDCAddAgent _ -> True
      RMDCDeleteAgent _ -> True
      RMSetCustomTypes _ -> True
      RMSetApiLimits _ -> True
      RMRemoveApiLimits -> True
      RMSetMetricsConfig _ -> True
      RMRemoveMetricsConfig -> True
      RMAddInheritedRole _ -> True
      RMDropInheritedRole _ -> True
      RMReplaceMetadata _ -> True
      RMClearMetadata _ -> True
      RMReloadMetadata _ -> True
      RMDropInconsistentMetadata _ -> True
      RMSetGraphqlSchemaIntrospectionOptions _ -> True
      RMAddHostToTLSAllowlist _ -> True
      RMDropHostFromTLSAllowlist _ -> True
      RMSetQueryTagsConfig _ -> True
      RMSetOpenTelemetryConfig _ -> True
      RMSetOpenTelemetryStatus _ -> True
  RMV2 q ->
    case q of
      RMV2ExportMetadata _ -> False
      _ -> True

runMetadataQueryM ::
  ( MonadIO m,
    MonadBaseControl IO m,
    CacheRWM m,
    Tracing.MonadTrace m,
    UserInfoM m,
    MetadataM m,
    MonadMetadataStorage m,
    MonadReader r m,
    Has (L.Logger L.Hasura) r,
    MonadError QErr m,
    MonadEventLogCleanup m,
    ProvidesHasuraServices m,
    MonadGetPolicies m,
    HasFeatureFlagChecker m
  ) =>
  Env.Environment ->
  SchemaSampledFeatureFlags ->
  Options.RemoteSchemaPermissions ->
  MetadataResourceVersion ->
  RQLMetadataRequest ->
  m EncJSON
runMetadataQueryM env schemaSampledFeatureFlags remoteSchemaPerms currentResourceVersion =
  withPathK "args" . \case
    -- NOTE: This is a good place to install tracing, since it's involved in
    -- the recursive case via "bulk":
    RMV1 q ->
      Tracing.newSpan ("v1 " <> T.pack (constrName q))
        $ runMetadataQueryV1M env schemaSampledFeatureFlags remoteSchemaPerms currentResourceVersion q
    RMV2 q ->
      Tracing.newSpan ("v2 " <> T.pack (constrName q))
        $ runMetadataQueryV2M currentResourceVersion q

runMetadataQueryV1M ::
  forall m r.
  ( MonadIO m,
    MonadBaseControl IO m,
    CacheRWM m,
    Tracing.MonadTrace m,
    UserInfoM m,
    MetadataM m,
    MonadMetadataStorage m,
    MonadReader r m,
    Has (L.Logger L.Hasura) r,
    MonadError QErr m,
    MonadEventLogCleanup m,
    ProvidesHasuraServices m,
    MonadGetPolicies m,
    HasFeatureFlagChecker m
  ) =>
  Env.Environment ->
  SchemaSampledFeatureFlags ->
  Options.RemoteSchemaPermissions ->
  MetadataResourceVersion ->
  RQLMetadataV1 ->
  m EncJSON
runMetadataQueryV1M env schemaSampledFeatureFlags remoteSchemaPerms currentResourceVersion = \case
  RMAddSource q -> dispatchMetadata (runAddSource env) q
  RMDropSource q -> runDropSource q
  RMRenameSource q -> runRenameSource q
  RMUpdateSource q -> dispatchMetadata runUpdateSource q
  RMListSourceKinds q -> runListSourceKinds q
  RMGetSourceKindCapabilities q -> runGetSourceKindCapabilities q
  RMGetSourceTables q -> dispatchMetadata runGetSourceTables q
  RMGetSourceTrackables q -> dispatchMetadata runGetSourceTrackables q
  RMGetTableInfo q -> dispatchMetadata runGetTableInfo q
  RMGetTableInfo_ q -> runGetTableInfo_ q
  RMTrackTable q -> dispatchMetadata runTrackTableV2Q q
  RMTrackTables q -> dispatchMetadata runTrackTablesQ q
  RMUntrackTable q -> dispatchMetadataAndEventTrigger runUntrackTableQ q
  RMUntrackTables q -> dispatchMetadataAndEventTrigger runUntrackTablesQ q
  RMSetFunctionCustomization q -> dispatchMetadata Functions.runSetFunctionCustomization q
  RMSetTableCustomization q -> dispatchMetadata runSetTableCustomization q
  RMSetApolloFederationConfig q -> dispatchMetadata runSetApolloFederationConfig q
  RMPgSetTableIsEnum q -> dispatchMetadata runSetExistingTableIsEnumQ q
  RMCreateInsertPermission q -> dispatchMetadata runCreatePerm q
  RMCreateSelectPermission q -> dispatchMetadata runCreatePerm q
  RMCreateUpdatePermission q -> dispatchMetadata runCreatePerm q
  RMCreateDeletePermission q -> dispatchMetadata runCreatePerm q
  RMDropInsertPermission q -> dispatchMetadata (runDropPerm PTInsert) q
  RMDropSelectPermission q -> dispatchMetadata (runDropPerm PTSelect) q
  RMDropUpdatePermission q -> dispatchMetadata (runDropPerm PTUpdate) q
  RMDropDeletePermission q -> dispatchMetadata (runDropPerm PTDelete) q
  RMSetPermissionComment q -> dispatchMetadata runSetPermComment q
  RMCreateObjectRelationship q -> dispatchMetadata (runCreateRelationship ObjRel . unCreateObjRel) q
  RMCreateArrayRelationship q -> dispatchMetadata (runCreateRelationship ArrRel . unCreateArrRel) q
  RMDropRelationship q -> dispatchMetadata runDropRel q
  RMSetRelationshipComment q -> dispatchMetadata runSetRelComment q
  RMRenameRelationship q -> dispatchMetadata runRenameRel q
  RMSuggestRelationships q -> dispatchMetadata runSuggestRels q
  RMCreateRemoteRelationship q -> dispatchMetadata runCreateRemoteRelationship q
  RMUpdateRemoteRelationship q -> dispatchMetadata runUpdateRemoteRelationship q
  RMDeleteRemoteRelationship q -> dispatchMetadata runDeleteRemoteRelationship q
  RMTrackFunction q -> dispatchMetadata Functions.runTrackFunctionV2 q
  RMUntrackFunction q -> dispatchMetadata Functions.runUntrackFunc q
  RMCreateFunctionPermission q -> dispatchMetadata Functions.runCreateFunctionPermission q
  RMDropFunctionPermission q -> dispatchMetadata Functions.runDropFunctionPermission q
  RMAddComputedField q -> dispatchMetadata runAddComputedField q
  RMDropComputedField q -> dispatchMetadata runDropComputedField q
  RMTestConnectionTemplate q -> dispatchMetadata runTestConnectionTemplate q
  RMGetNativeQuery q -> dispatchMetadata NativeQueries.runGetNativeQuery q
  RMTrackNativeQuery q -> dispatchMetadata (runSingleExec NativeQueries.execTrackNativeQuery) q
  RMUntrackNativeQuery q -> dispatchMetadata (runSingleExec NativeQueries.execUntrackNativeQuery) q
  RMGetStoredProcedure q -> dispatchMetadata StoredProcedures.runGetStoredProcedure q
  RMTrackStoredProcedure q -> dispatchMetadata (runSingleExec StoredProcedures.execTrackStoredProcedure) q
  RMUntrackStoredProcedure q -> dispatchMetadata (runSingleExec StoredProcedures.execUntrackStoredProcedure) q
  RMGetLogicalModel q -> dispatchMetadata LogicalModel.runGetLogicalModel q
  RMTrackLogicalModel q -> dispatchMetadata (runSingleExec LogicalModel.execTrackLogicalModel) q
  RMUntrackLogicalModel q -> dispatchMetadata (runSingleExec LogicalModel.execUntrackLogicalModel) q
  RMCreateSelectLogicalModelPermission q -> dispatchMetadata LogicalModel.runCreateSelectLogicalModelPermission q
  RMDropSelectLogicalModelPermission q -> dispatchMetadata LogicalModel.runDropSelectLogicalModelPermission q
  RMCreateEventTrigger q ->
    dispatchMetadataAndEventTrigger
      ( validateTransforms
          (unUnvalidate1 . cetqRequestTransform . _Just)
          (unUnvalidate1 . cetqResponseTrasnform . _Just)
          (runCreateEventTriggerQuery . _unUnvalidate1)
      )
      q
  RMDeleteEventTrigger q -> dispatchMetadataAndEventTrigger runDeleteEventTriggerQuery q
  RMRedeliverEvent q -> dispatchEventTrigger runRedeliverEvent q
  RMInvokeEventTrigger q -> dispatchEventTrigger runInvokeEventTrigger q
  RMCleanupEventTriggerLog q -> runCleanupEventTriggerLog q
  RMResumeEventTriggerCleanup q -> runEventTriggerResumeCleanup q
  RMPauseEventTriggerCleanup q -> runEventTriggerPauseCleanup q
  RMGetEventLogs q -> dispatchEventTrigger runGetEventLogs q
  RMGetEventInvocationLogs q -> dispatchEventTrigger runGetEventInvocationLogs q
  RMGetEventById q -> dispatchEventTrigger runGetEventById q
  RMAddRemoteSchema q -> runAddRemoteSchema env schemaSampledFeatureFlags q
  RMUpdateRemoteSchema q -> runUpdateRemoteSchema env schemaSampledFeatureFlags q
  RMRemoveRemoteSchema q -> runRemoveRemoteSchema q
  RMReloadRemoteSchema q -> runReloadRemoteSchema q
  RMIntrospectRemoteSchema q -> runIntrospectRemoteSchema q
  RMAddRemoteSchemaPermissions q -> runAddRemoteSchemaPermissions remoteSchemaPerms q
  RMDropRemoteSchemaPermissions q -> runDropRemoteSchemaPermissions q
  RMCreateRemoteSchemaRemoteRelationship q -> runCreateRemoteSchemaRemoteRelationship q
  RMUpdateRemoteSchemaRemoteRelationship q -> runUpdateRemoteSchemaRemoteRelationship q
  RMDeleteRemoteSchemaRemoteRelationship q -> runDeleteRemoteSchemaRemoteRelationship q
  RMCreateCronTrigger q ->
    validateTransforms
      (unUnvalidate . cctRequestTransform . _Just)
      (unUnvalidate . cctResponseTransform . _Just)
      (runCreateCronTrigger . _unUnvalidate)
      q
  RMDeleteCronTrigger q -> runDeleteCronTrigger q
  RMCreateScheduledEvent q -> runCreateScheduledEvent q
  RMDeleteScheduledEvent q -> runDeleteScheduledEvent q
  RMGetScheduledEvents q -> runGetScheduledEvents q
  RMGetScheduledEventInvocations q -> runGetScheduledEventInvocations q
  RMGetCronTriggers -> runGetCronTriggers
  RMCreateAction q ->
    validateTransforms
      (unUnvalidate . caDefinition . adRequestTransform . _Just)
      (unUnvalidate . caDefinition . adResponseTransform . _Just)
      (runCreateAction . _unUnvalidate)
      q
  RMDropAction q -> runDropAction q
  RMUpdateAction q ->
    validateTransforms
      (unUnvalidate . uaDefinition . adRequestTransform . _Just)
      (unUnvalidate . uaDefinition . adResponseTransform . _Just)
      (runUpdateAction . _unUnvalidate)
      q
  RMCreateActionPermission q -> runCreateActionPermission q
  RMDropActionPermission q -> runDropActionPermission q
  RMCreateQueryCollection q -> runCreateCollection q
  RMRenameQueryCollection q -> runRenameCollection q
  RMDropQueryCollection q -> runDropCollection q
  RMAddQueryToCollection q -> runAddQueryToCollection q
  RMDropQueryFromCollection q -> runDropQueryFromCollection q
  RMAddCollectionToAllowlist q -> runAddCollectionToAllowlist q
  RMDropCollectionFromAllowlist q -> runDropCollectionFromAllowlist q
  RMUpdateScopeOfCollectionInAllowlist q -> runUpdateScopeOfCollectionInAllowlist q
  RMCreateRestEndpoint q -> runCreateEndpoint q
  RMDropRestEndpoint q -> runDropEndpoint q
  RMDCAddAgent q -> runAddDataConnectorAgent q
  RMDCDeleteAgent q -> runDeleteDataConnectorAgent q
  RMSetCustomTypes q -> runSetCustomTypes q
  RMSetApiLimits q -> runSetApiLimits q
  RMRemoveApiLimits -> runRemoveApiLimits
  RMSetMetricsConfig q -> runSetMetricsConfig q
  RMRemoveMetricsConfig -> runRemoveMetricsConfig
  RMAddInheritedRole q -> runAddInheritedRole q
  RMDropInheritedRole q -> runDropInheritedRole q
  RMReplaceMetadata q -> runReplaceMetadata q
  RMExportMetadata q -> runExportMetadata q
  RMClearMetadata q -> runClearMetadata q
  RMReloadMetadata q -> runReloadMetadata q
  RMGetInconsistentMetadata q -> runGetInconsistentMetadata q
  RMDropInconsistentMetadata q -> runDropInconsistentMetadata q
  RMSetGraphqlSchemaIntrospectionOptions q -> runSetGraphqlSchemaIntrospectionOptions q
  RMAddHostToTLSAllowlist q -> runAddHostToTLSAllowlist q
  RMDropHostFromTLSAllowlist q -> runDropHostFromTLSAllowlist q
  RMDumpInternalState q -> runDumpInternalState q
  RMGetCatalogState q -> runGetCatalogState q
  RMSetCatalogState q -> runSetCatalogState q
  RMTestWebhookTransform q ->
    validateTransforms
      (unUnvalidate . twtRequestTransformer)
      (unUnvalidate . twtResponseTransformer . _Just)
      (runTestWebhookTransform . _unUnvalidate)
      q
  RMSetQueryTagsConfig q -> runSetQueryTagsConfig q
  RMSetOpenTelemetryConfig q -> runSetOpenTelemetryConfig q
  RMSetOpenTelemetryStatus q -> runSetOpenTelemetryStatus q
  RMBulk q -> encJFromList <$> indexedMapM (runMetadataQueryM env schemaSampledFeatureFlags remoteSchemaPerms currentResourceVersion) q
  RMBulkKeepGoing commands -> do
    results <-
      commands & indexedMapM \command ->
        runMetadataQueryM env schemaSampledFeatureFlags remoteSchemaPerms currentResourceVersion command
          -- Because changes to the metadata are maintained in MetadataT, which is a state monad
          -- that is layered above the QErr error monad, this catchError causes any changes to
          -- the metadata made during running the failed API function to be rolled back
          `catchError` \qerr -> pure (encJFromJValue qerr)

    pure (encJFromList results)
  RMBulkAtomic commands -> runBulkAtomic commands
  where
    dispatchEventTrigger :: (forall b. (BackendEventTrigger b) => i b -> a) -> AnyBackend i -> a
    dispatchEventTrigger f x = dispatchAnyBackend @BackendEventTrigger x f

    dispatchMetadataAndEventTrigger ::
      (forall b. (BackendMetadata b, BackendEventTrigger b) => i b -> a) ->
      AnyBackend i ->
      a
    dispatchMetadataAndEventTrigger f x = dispatchAnyBackendWithTwoConstraints @BackendMetadata @BackendEventTrigger x f

dispatchMetadata ::
  (forall b. (BackendMetadata b) => i b -> a) ->
  AnyBackend i ->
  a
dispatchMetadata f x = dispatchAnyBackend @BackendMetadata x f

-- | the atomic commands work slightly differently
-- each one just returns the metadata modifier, we then chain them all and
-- run the schema cache validation once. This allows us to combine drop and
-- re-add commands to do edits, or add two interdependent items at once.
runBulkAtomic ::
  forall m.
  ( HasFeatureFlagChecker m,
    MonadError QErr m,
    CacheRWM m,
    MetadataM m
  ) =>
  [RQLMetadataRequest] ->
  m EncJSON
runBulkAtomic cmds = do
  -- get the metadata modifiers for all our commands
  (mdModifiers :: [Metadata -> m Metadata]) <- do
    (mods :: [Metadata -> m MetadataModifier]) <- traverse getMetadataModifierForCommand cmds
    pure
      $ map
        ( \checker metadata -> do
            MetadataModifier modifier <- checker metadata
            pure $ modifier metadata
        )
        mods

  -- Try building the schema cache using the combined modifiers. If we run into
  -- any inconsistencies, we should fail and roll back.
  inconsistencies <- tryBuildSchemaCacheWithModifiers mdModifiers

  unless (null inconsistencies)
    $ throw400WithDetail BadRequest "Schema inconsistency"
    $ toJSON (HashMap.elems inconsistencies)

  pure successMsg
  where
    forgetMetadataObjId ::
      (command -> Metadata -> m (MetadataObjId, MetadataModifier)) ->
      command ->
      Metadata ->
      m MetadataModifier
    forgetMetadataObjId f x y = fmap snd (f x y)

    getMetadataModifierForCommand :: RQLMetadataRequest -> m (Metadata -> m MetadataModifier)
    getMetadataModifierForCommand = \case
      RMV1 v -> case v of
        -- Whoa there, cowboy! Chances are you're here to add table tracking to
        -- the list of things that bulk_atomic can do. Before you do that,
        -- though, there is a big, particularly-Citus-shaped problem you might
        -- need to consider:
        --
        -- \* There are specific validation rules around how Citus handles
        --   relationships (see 'validateRel' in 'PostgresMetadata'), which
        --   will either need to be deferred until the end of the bulk /or/
        --   moved to the schema cache.
        -- \* This would also introduce the possibility of a table state that is
        --   eventually consistent but currently inconsistent: I add table X, a
        --   relationship between X and Y, and then I add table Y. Currently,
        --   this can't be done, so all validation checks in
        --   'execCreateRelationship' and 'execDropRelationship' remain as they
        --   are in the @run@ versions.

        RMCreateObjectRelationship q -> pure $ dispatchMetadata (forgetMetadataObjId $ execCreateRelationship ObjRel . unCreateObjRel) q
        RMCreateArrayRelationship q -> pure $ dispatchMetadata (forgetMetadataObjId $ execCreateRelationship ArrRel . unCreateArrRel) q
        RMDropRelationship q -> pure $ dispatchMetadata (const . execDropRel) q
        RMDeleteRemoteRelationship q -> pure $ dispatchMetadata (forgetMetadataObjId $ const . execDeleteRemoteRelationship) q
        RMTrackNativeQuery q -> pure $ dispatchMetadata (forgetMetadataObjId NativeQueries.execTrackNativeQuery) q
        RMUntrackNativeQuery q -> pure $ dispatchMetadata (forgetMetadataObjId NativeQueries.execUntrackNativeQuery) q
        RMTrackLogicalModel q -> pure $ dispatchMetadata (forgetMetadataObjId LogicalModel.execTrackLogicalModel) q
        RMUntrackLogicalModel q -> pure $ dispatchMetadata (forgetMetadataObjId LogicalModel.execUntrackLogicalModel) q
        RMTrackStoredProcedure q -> pure $ dispatchMetadata (forgetMetadataObjId StoredProcedures.execTrackStoredProcedure) q
        RMUntrackStoredProcedure q -> pure $ dispatchMetadata (forgetMetadataObjId StoredProcedures.execUntrackStoredProcedure) q
        _ -> throw500 "Bulk atomic does not support this command"
      RMV2 _ -> throw500 $ "Bulk atomic does not support this command"

runMetadataQueryV2M ::
  ( MonadIO m,
    CacheRWM m,
    MonadBaseControl IO m,
    MetadataM m,
    MonadMetadataStorage m,
    MonadReader r m,
    Has (L.Logger L.Hasura) r,
    MonadError QErr m,
    MonadEventLogCleanup m,
    MonadGetPolicies m
  ) =>
  MetadataResourceVersion ->
  RQLMetadataV2 ->
  m EncJSON
runMetadataQueryV2M currentResourceVersion = \case
  RMV2ReplaceMetadata q -> runReplaceMetadataV2 q
  RMV2ExportMetadata q -> runExportMetadataV2 currentResourceVersion q

runSingleExec ::
  forall m request.
  ( MonadError QErr m,
    CacheRWM m,
    MetadataM m
  ) =>
  (request -> Metadata -> m (MetadataObjId, MetadataModifier)) ->
  request ->
  m EncJSON
runSingleExec exec request = do
  metadata <- getMetadata
  (obj, modifier) <- exec request metadata
  buildSchemaCacheFor obj modifier
  pure successMsg
