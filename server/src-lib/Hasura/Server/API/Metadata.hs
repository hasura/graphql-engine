{-# LANGUAGE ViewPatterns #-}

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
import Data.Aeson.Casing
import Data.Aeson.Types qualified as A
import Data.Environment qualified as Env
import Data.Has (Has)
import Data.Text qualified as T
import Data.Text.Extended qualified as T
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.Logging qualified as L
import Hasura.Metadata.Class
import Hasura.Prelude hiding (first)
import Hasura.RQL.DDL.Action
import Hasura.RQL.DDL.ApiLimit
import Hasura.RQL.DDL.ComputedField
import Hasura.RQL.DDL.CustomTypes
import Hasura.RQL.DDL.Endpoint
import Hasura.RQL.DDL.EventTrigger
import Hasura.RQL.DDL.GraphqlSchemaIntrospection
import Hasura.RQL.DDL.InheritedRoles
import Hasura.RQL.DDL.Metadata
import Hasura.RQL.DDL.Network
import Hasura.RQL.DDL.Permission
import Hasura.RQL.DDL.QueryCollection
import Hasura.RQL.DDL.QueryTags
import Hasura.RQL.DDL.Relationship
import Hasura.RQL.DDL.Relationship.Rename
import Hasura.RQL.DDL.RemoteRelationship
import Hasura.RQL.DDL.RemoteSchema
import Hasura.RQL.DDL.ScheduledTrigger
import Hasura.RQL.DDL.Schema
import Hasura.RQL.DDL.Schema.Source
import Hasura.RQL.DDL.Webhook.Transform.Validation
import Hasura.RQL.Types.Action
import Hasura.RQL.Types.Allowlist
import Hasura.RQL.Types.ApiLimit
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.CustomTypes
import Hasura.RQL.Types.Endpoint
import Hasura.RQL.Types.Eventing.Backend
import Hasura.RQL.Types.GraphqlSchemaIntrospection
import Hasura.RQL.Types.Metadata
import Hasura.RQL.Types.Metadata.Backend
import Hasura.RQL.Types.Network
import Hasura.RQL.Types.Permission
import Hasura.RQL.Types.QueryCollection
import Hasura.RQL.Types.RemoteSchema
import Hasura.RQL.Types.Roles
import Hasura.RQL.Types.Run
import Hasura.RQL.Types.ScheduledTrigger
import Hasura.RQL.Types.SchemaCache
import Hasura.RQL.Types.SchemaCache.Build
import Hasura.RQL.Types.Source
import Hasura.SQL.AnyBackend
import Hasura.SQL.Backend
import Hasura.Server.API.Backend
import Hasura.Server.API.Instances ()
import Hasura.Server.Types
import Hasura.Server.Utils (APIVersion (..))
import Hasura.Session
import Hasura.Tracing qualified as Tracing
import Network.HTTP.Client.Manager qualified as HTTP

data RQLMetadataV1
  = -- Sources
    RMAddSource !(AnyBackend AddSource)
  | RMDropSource DropSource
  | RMRenameSource !RenameSource
  | RMUpdateSource !(AnyBackend UpdateSource)
  | -- Tables
    RMTrackTable !(AnyBackend TrackTableV2)
  | RMUntrackTable !(AnyBackend UntrackTable)
  | RMSetTableCustomization !(AnyBackend SetTableCustomization)
  | -- Tables (PG-specific)
    RMPgSetTableIsEnum !SetTableIsEnum
  | -- Tables permissions
    RMCreateInsertPermission !(AnyBackend (CreatePerm InsPerm))
  | RMCreateSelectPermission !(AnyBackend (CreatePerm SelPerm))
  | RMCreateUpdatePermission !(AnyBackend (CreatePerm UpdPerm))
  | RMCreateDeletePermission !(AnyBackend (CreatePerm DelPerm))
  | RMDropInsertPermission !(AnyBackend DropPerm)
  | RMDropSelectPermission !(AnyBackend DropPerm)
  | RMDropUpdatePermission !(AnyBackend DropPerm)
  | RMDropDeletePermission !(AnyBackend DropPerm)
  | RMSetPermissionComment !(AnyBackend SetPermComment)
  | -- Tables relationships
    RMCreateObjectRelationship !(AnyBackend CreateObjRel)
  | RMCreateArrayRelationship !(AnyBackend CreateArrRel)
  | RMDropRelationship !(AnyBackend DropRel)
  | RMSetRelationshipComment !(AnyBackend SetRelComment)
  | RMRenameRelationship !(AnyBackend RenameRel)
  | -- Tables remote relationships
    RMCreateRemoteRelationship !(AnyBackend CreateFromSourceRelationship)
  | RMUpdateRemoteRelationship !(AnyBackend CreateFromSourceRelationship)
  | RMDeleteRemoteRelationship !(AnyBackend DeleteFromSourceRelationship)
  | -- Functions
    RMTrackFunction !(AnyBackend TrackFunctionV2)
  | RMUntrackFunction !(AnyBackend UnTrackFunction)
  | RMSetFunctionCustomization (AnyBackend SetFunctionCustomization)
  | -- Functions permissions
    RMCreateFunctionPermission !(AnyBackend FunctionPermissionArgument)
  | RMDropFunctionPermission !(AnyBackend FunctionPermissionArgument)
  | -- Computed fields
    RMAddComputedField !(AnyBackend AddComputedField)
  | RMDropComputedField !(AnyBackend DropComputedField)
  | -- Tables event triggers
    RMCreateEventTrigger !(AnyBackend (Unvalidated1 CreateEventTriggerQuery))
  | RMDeleteEventTrigger !(AnyBackend DeleteEventTriggerQuery)
  | RMRedeliverEvent !(AnyBackend RedeliverEventQuery)
  | RMInvokeEventTrigger !(AnyBackend InvokeEventTriggerQuery)
  | -- Remote schemas
    RMAddRemoteSchema !AddRemoteSchemaQuery
  | RMUpdateRemoteSchema !AddRemoteSchemaQuery
  | RMRemoveRemoteSchema !RemoteSchemaNameQuery
  | RMReloadRemoteSchema !RemoteSchemaNameQuery
  | RMIntrospectRemoteSchema !RemoteSchemaNameQuery
  | -- Remote schemas permissions
    RMAddRemoteSchemaPermissions !AddRemoteSchemaPermission
  | RMDropRemoteSchemaPermissions !DropRemoteSchemaPermissions
  | -- Remote Schema remote relationships
    RMCreateRemoteSchemaRemoteRelationship CreateRemoteSchemaRemoteRelationship
  | RMUpdateRemoteSchemaRemoteRelationship CreateRemoteSchemaRemoteRelationship
  | RMDeleteRemoteSchemaRemoteRelationship DeleteRemoteSchemaRemoteRelationship
  | -- Scheduled triggers
    RMCreateCronTrigger !(Unvalidated CreateCronTrigger)
  | RMDeleteCronTrigger !ScheduledTriggerName
  | RMCreateScheduledEvent !CreateScheduledEvent
  | RMDeleteScheduledEvent !DeleteScheduledEvent
  | RMGetScheduledEvents !GetScheduledEvents
  | RMGetEventInvocations !GetEventInvocations
  | RMGetCronTriggers
  | -- Actions
    RMCreateAction !(Unvalidated CreateAction)
  | RMDropAction !DropAction
  | RMUpdateAction !(Unvalidated UpdateAction)
  | RMCreateActionPermission !CreateActionPermission
  | RMDropActionPermission !DropActionPermission
  | -- Query collections, allow list related
    RMCreateQueryCollection !CreateCollection
  | RMDropQueryCollection !DropCollection
  | RMAddQueryToCollection !AddQueryToCollection
  | RMDropQueryFromCollection !DropQueryFromCollection
  | RMAddCollectionToAllowlist !AllowlistEntry
  | RMDropCollectionFromAllowlist !DropCollectionFromAllowlist
  | RMUpdateScopeOfCollectionInAllowlist !UpdateScopeOfCollectionInAllowlist
  | -- Rest endpoints
    RMCreateRestEndpoint !CreateEndpoint
  | RMDropRestEndpoint !DropEndpoint
  | -- Custom types
    RMSetCustomTypes !CustomTypes
  | -- Api limits
    RMSetApiLimits !ApiLimit
  | RMRemoveApiLimits
  | -- Metrics config
    RMSetMetricsConfig !MetricsConfig
  | RMRemoveMetricsConfig
  | -- Inherited roles
    RMAddInheritedRole !InheritedRole
  | RMDropInheritedRole !DropInheritedRole
  | -- Metadata management
    RMReplaceMetadata !ReplaceMetadata
  | RMExportMetadata !ExportMetadata
  | RMClearMetadata !ClearMetadata
  | RMReloadMetadata !ReloadMetadata
  | RMGetInconsistentMetadata !GetInconsistentMetadata
  | RMDropInconsistentMetadata !DropInconsistentMetadata
  | -- Introspection options
    RMSetGraphqlSchemaIntrospectionOptions !SetGraphqlIntrospectionOptions
  | -- Network
    RMAddHostToTLSAllowlist !AddHostToTLSAllowlist
  | RMDropHostFromTLSAllowlist !DropHostFromTLSAllowlist
  | -- QueryTags
    RMSetQueryTagsConfig !SetQueryTagsConfig
  | -- Debug
    RMDumpInternalState !DumpInternalState
  | RMGetCatalogState !GetCatalogState
  | RMSetCatalogState !SetCatalogState
  | RMTestWebhookTransform !(Unvalidated TestWebhookTransform)
  | -- Bulk metadata queries
    RMBulk [RQLMetadataRequest]

instance FromJSON RQLMetadataV1 where
  parseJSON = withObject "RQLMetadataV1" \o -> do
    queryType <- o .: "type"
    let args :: forall a. FromJSON a => A.Parser a
        args = o .: "args"
    case queryType of
      -- backend agnostic
      "rename_source" -> RMRenameSource <$> args
      "add_remote_schema" -> RMAddRemoteSchema <$> args
      "update_remote_schema" -> RMUpdateRemoteSchema <$> args
      "remove_remote_schema" -> RMRemoveRemoteSchema <$> args
      "reload_remote_schema" -> RMReloadRemoteSchema <$> args
      "introspect_remote_schema" -> RMIntrospectRemoteSchema <$> args
      "add_remote_schema_permissions" -> RMAddRemoteSchemaPermissions <$> args
      "drop_remote_schema_permissions" -> RMDropRemoteSchemaPermissions <$> args
      "create_remote_schema_remote_relationship" -> RMCreateRemoteSchemaRemoteRelationship <$> args
      "update_remote_schema_remote_relationship" -> RMUpdateRemoteSchemaRemoteRelationship <$> args
      "delete_remote_schema_remote_relationship" -> RMDeleteRemoteSchemaRemoteRelationship <$> args
      "create_cron_trigger" -> RMCreateCronTrigger <$> args
      "delete_cron_trigger" -> RMDeleteCronTrigger <$> args
      "create_scheduled_event" -> RMCreateScheduledEvent <$> args
      "delete_scheduled_event" -> RMDeleteScheduledEvent <$> args
      "get_scheduled_events" -> RMGetScheduledEvents <$> args
      "get_event_invocations" -> RMGetEventInvocations <$> args
      "get_cron_triggers" -> pure RMGetCronTriggers
      "create_action" -> RMCreateAction <$> args
      "drop_action" -> RMDropAction <$> args
      "update_action" -> RMUpdateAction <$> args
      "create_action_permission" -> RMCreateActionPermission <$> args
      "drop_action_permission" -> RMDropActionPermission <$> args
      "create_query_collection" -> RMCreateQueryCollection <$> args
      "drop_query_collection" -> RMDropQueryCollection <$> args
      "add_query_to_collection" -> RMAddQueryToCollection <$> args
      "drop_query_from_collection" -> RMDropQueryFromCollection <$> args
      "add_collection_to_allowlist" -> RMAddCollectionToAllowlist <$> args
      "drop_collection_from_allowlist" -> RMDropCollectionFromAllowlist <$> args
      "update_scope_of_collection_in_allowlist" -> RMUpdateScopeOfCollectionInAllowlist <$> args
      "create_rest_endpoint" -> RMCreateRestEndpoint <$> args
      "drop_rest_endpoint" -> RMDropRestEndpoint <$> args
      "set_custom_types" -> RMSetCustomTypes <$> args
      "set_api_limits" -> RMSetApiLimits <$> args
      "remove_api_limits" -> pure RMRemoveApiLimits
      "set_metrics_config" -> RMSetMetricsConfig <$> args
      "remove_metrics_config" -> pure RMRemoveMetricsConfig
      "add_inherited_role" -> RMAddInheritedRole <$> args
      "drop_inherited_role" -> RMDropInheritedRole <$> args
      "replace_metadata" -> RMReplaceMetadata <$> args
      "export_metadata" -> RMExportMetadata <$> args
      "clear_metadata" -> RMClearMetadata <$> args
      "reload_metadata" -> RMReloadMetadata <$> args
      "get_inconsistent_metadata" -> RMGetInconsistentMetadata <$> args
      "drop_inconsistent_metadata" -> RMDropInconsistentMetadata <$> args
      "add_host_to_tls_allowlist" -> RMAddHostToTLSAllowlist <$> args
      "drop_host_from_tls_allowlist" -> RMDropHostFromTLSAllowlist <$> args
      "dump_internal_state" -> RMDumpInternalState <$> args
      "get_catalog_state" -> RMGetCatalogState <$> args
      "set_catalog_state" -> RMSetCatalogState <$> args
      "set_graphql_schema_introspection_options" -> RMSetGraphqlSchemaIntrospectionOptions <$> args
      "test_webhook_transform" -> RMTestWebhookTransform <$> args
      "set_query_tags" -> RMSetQueryTagsConfig <$> args
      "bulk" -> RMBulk <$> args
      -- backend specific
      _ -> do
        (backendSourceKind, cmd) <- parseQueryType queryType
        dispatchAnyBackend @BackendAPI backendSourceKind \(backendSourceKind' :: BackendSourceKind b) -> do
          argValue <- args
          command <- choice <$> sequenceA [p backendSourceKind' cmd argValue | p <- metadataV1CommandParsers @b]
          onNothing command $
            fail $
              "unknown metadata command \"" <> T.unpack cmd
                <> "\" for backend "
                <> T.unpack (T.toTxt backendSourceKind')
    where
      parseQueryType :: MonadFail m => Text -> m (AnyBackend BackendSourceKind, Text)
      parseQueryType queryType =
        let (prefix, T.drop 1 -> cmd) = T.breakOn "_" queryType
         in (,cmd) <$> backendSourceKindFromText prefix
              `onNothing` fail
                ( "unknown metadata command \"" <> T.unpack queryType
                    <> "\"; \""
                    <> T.unpack prefix
                    <> "\" was not recognized as a valid backend name"
                )

data RQLMetadataV2
  = RMV2ReplaceMetadata !ReplaceMetadataV2
  | RMV2ExportMetadata !ExportMetadata
  deriving (Generic)

instance FromJSON RQLMetadataV2 where
  parseJSON =
    genericParseJSON $
      defaultOptions
        { constructorTagModifier = snakeCase . drop 4,
          sumEncoding = TaggedObject "type" "args"
        }

data RQLMetadataRequest
  = RMV1 !RQLMetadataV1
  | RMV2 !RQLMetadataV2

instance FromJSON RQLMetadataRequest where
  parseJSON = withObject "RQLMetadataRequest" $ \o -> do
    version <- o .:? "version" .!= VIVersion1
    let val = Object o
    case version of
      VIVersion1 -> RMV1 <$> parseJSON val
      VIVersion2 -> RMV2 <$> parseJSON val

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
    MonadBaseControl IO m,
    Tracing.MonadTrace m,
    MonadMetadataStorage m,
    MonadResolveSource m
  ) =>
  Env.Environment ->
  L.Logger L.Hasura ->
  InstanceId ->
  UserInfo ->
  HTTP.Manager ->
  ServerConfigCtx ->
  RebuildableSchemaCache ->
  RQLMetadata ->
  m (EncJSON, RebuildableSchemaCache)
runMetadataQuery env logger instanceId userInfo httpManager serverConfigCtx schemaCache RQLMetadata {..} = do
  (metadata, currentResourceVersion) <- fetchMetadata
  ((r, modMetadata), modSchemaCache, cacheInvalidations) <-
    runMetadataQueryM env currentResourceVersion _rqlMetadata
      & flip runReaderT logger
      & runMetadataT metadata
      & runCacheRWT schemaCache
      & peelRun (RunCtx userInfo httpManager serverConfigCtx)
      & runExceptT
      & liftEitherM
  -- set modified metadata in storage
  if queryModifiesMetadata _rqlMetadata
    then case (_sccMaintenanceMode serverConfigCtx, _sccReadOnlyMode serverConfigCtx) of
      (MaintenanceModeDisabled, ReadOnlyModeDisabled) -> do
        -- set modified metadata in storage
        newResourceVersion <- setMetadata (fromMaybe currentResourceVersion _rqlMetadataResourceVersion) modMetadata

        -- notify schema cache sync
        notifySchemaCacheSync newResourceVersion instanceId cacheInvalidations
        (_, modSchemaCache', _) <-
          setMetadataResourceVersionInSchemaCache newResourceVersion
            & runCacheRWT modSchemaCache
            & peelRun (RunCtx userInfo httpManager serverConfigCtx)
            & runExceptT
            & liftEitherM

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
      RMGetInconsistentMetadata _ -> False
      RMIntrospectRemoteSchema _ -> False
      RMDumpInternalState _ -> False
      RMSetCatalogState _ -> False
      RMGetCatalogState _ -> False
      RMExportMetadata _ -> False
      RMGetEventInvocations _ -> False
      RMGetCronTriggers -> False
      RMGetScheduledEvents _ -> False
      RMCreateScheduledEvent _ -> False
      RMDeleteScheduledEvent _ -> False
      RMTestWebhookTransform _ -> False
      RMBulk qs -> any queryModifiesMetadata qs
      _ -> True
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
    HTTP.HasHttpManagerM m,
    MetadataM m,
    MonadMetadataStorageQueryAPI m,
    HasServerConfigCtx m,
    MonadReader r m,
    Has (L.Logger L.Hasura) r
  ) =>
  Env.Environment ->
  MetadataResourceVersion ->
  RQLMetadataRequest ->
  m EncJSON
runMetadataQueryM env currentResourceVersion =
  withPathK "args" . \case
    RMV1 q -> runMetadataQueryV1M env currentResourceVersion q
    RMV2 q -> runMetadataQueryV2M currentResourceVersion q

runMetadataQueryV1M ::
  forall m r.
  ( MonadIO m,
    MonadBaseControl IO m,
    CacheRWM m,
    Tracing.MonadTrace m,
    UserInfoM m,
    HTTP.HasHttpManagerM m,
    MetadataM m,
    MonadMetadataStorageQueryAPI m,
    HasServerConfigCtx m,
    MonadReader r m,
    Has (L.Logger L.Hasura) r
  ) =>
  Env.Environment ->
  MetadataResourceVersion ->
  RQLMetadataV1 ->
  m EncJSON
runMetadataQueryV1M env currentResourceVersion = \case
  RMAddSource q -> dispatchMetadata runAddSource q
  RMDropSource q -> runDropSource q
  RMRenameSource q -> runRenameSource q
  RMUpdateSource q -> dispatchMetadata runUpdateSource q
  RMTrackTable q -> dispatchMetadata runTrackTableV2Q q
  RMUntrackTable q -> dispatchMetadata runUntrackTableQ q
  RMSetFunctionCustomization q -> dispatchMetadata runSetFunctionCustomization q
  RMSetTableCustomization q -> dispatchMetadata runSetTableCustomization q
  RMPgSetTableIsEnum q -> runSetExistingTableIsEnumQ q
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
  RMCreateRemoteRelationship q -> dispatchMetadata runCreateRemoteRelationship q
  RMUpdateRemoteRelationship q -> dispatchMetadata runUpdateRemoteRelationship q
  RMDeleteRemoteRelationship q -> dispatchMetadata runDeleteRemoteRelationship q
  RMTrackFunction q -> dispatchMetadata runTrackFunctionV2 q
  RMUntrackFunction q -> dispatchMetadata runUntrackFunc q
  RMCreateFunctionPermission q -> dispatchMetadata runCreateFunctionPermission q
  RMDropFunctionPermission q -> dispatchMetadata runDropFunctionPermission q
  RMAddComputedField q -> dispatchMetadata runAddComputedField q
  RMDropComputedField q -> dispatchMetadata runDropComputedField q
  RMCreateEventTrigger q ->
    dispatchMetadataAndEventTrigger
      ( validateTransforms
          (unUnvalidate1 . cetqRequestTransform . _Just)
          (runCreateEventTriggerQuery . _unUnvalidate1)
      )
      q
  RMDeleteEventTrigger q -> dispatchMetadataAndEventTrigger runDeleteEventTriggerQuery q
  RMRedeliverEvent q -> dispatchEventTrigger runRedeliverEvent q
  RMInvokeEventTrigger q -> dispatchEventTrigger runInvokeEventTrigger q
  RMAddRemoteSchema q -> runAddRemoteSchema env q
  RMUpdateRemoteSchema q -> runUpdateRemoteSchema env q
  RMRemoveRemoteSchema q -> runRemoveRemoteSchema q
  RMReloadRemoteSchema q -> runReloadRemoteSchema q
  RMIntrospectRemoteSchema q -> runIntrospectRemoteSchema q
  RMAddRemoteSchemaPermissions q -> runAddRemoteSchemaPermissions q
  RMDropRemoteSchemaPermissions q -> runDropRemoteSchemaPermissions q
  RMCreateRemoteSchemaRemoteRelationship q -> runCreateRemoteSchemaRemoteRelationship q
  RMUpdateRemoteSchemaRemoteRelationship q -> runUpdateRemoteSchemaRemoteRelationship q
  RMDeleteRemoteSchemaRemoteRelationship q -> runDeleteRemoteSchemaRemoteRelationship q
  RMCreateCronTrigger q ->
    validateTransforms
      (unUnvalidate . cctRequestTransform . _Just)
      (runCreateCronTrigger . _unUnvalidate)
      q
  RMDeleteCronTrigger q -> runDeleteCronTrigger q
  RMCreateScheduledEvent q -> runCreateScheduledEvent q
  RMDeleteScheduledEvent q -> runDeleteScheduledEvent q
  RMGetScheduledEvents q -> runGetScheduledEvents q
  RMGetEventInvocations q -> runGetEventInvocations q
  RMGetCronTriggers -> runGetCronTriggers
  RMCreateAction q ->
    validateTransforms
      (unUnvalidate . caDefinition . adRequestTransform . _Just)
      (runCreateAction . _unUnvalidate)
      q
  RMDropAction q -> runDropAction q
  RMUpdateAction q ->
    validateTransforms
      (unUnvalidate . uaDefinition . adRequestTransform . _Just)
      (runUpdateAction . _unUnvalidate)
      q
  RMCreateActionPermission q -> runCreateActionPermission q
  RMDropActionPermission q -> runDropActionPermission q
  RMCreateQueryCollection q -> runCreateCollection q
  RMDropQueryCollection q -> runDropCollection q
  RMAddQueryToCollection q -> runAddQueryToCollection q
  RMDropQueryFromCollection q -> runDropQueryFromCollection q
  RMAddCollectionToAllowlist q -> runAddCollectionToAllowlist q
  RMDropCollectionFromAllowlist q -> runDropCollectionFromAllowlist q
  RMUpdateScopeOfCollectionInAllowlist q -> runUpdateScopeOfCollectionInAllowlist q
  RMCreateRestEndpoint q -> runCreateEndpoint q
  RMDropRestEndpoint q -> runDropEndpoint q
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
      (unUnvalidate . twtTransformer)
      (runTestWebhookTransform . _unUnvalidate)
      q
  RMSetQueryTagsConfig q -> runSetQueryTagsConfig q
  RMBulk q -> encJFromList <$> indexedMapM (runMetadataQueryM env currentResourceVersion) q
  where
    dispatchMetadata ::
      (forall b. BackendMetadata b => i b -> a) ->
      AnyBackend i ->
      a
    dispatchMetadata f x = dispatchAnyBackend @BackendMetadata x f

    dispatchEventTrigger :: (forall b. BackendEventTrigger b => i b -> a) -> AnyBackend i -> a
    dispatchEventTrigger f x = dispatchAnyBackend @BackendEventTrigger x f

    dispatchMetadataAndEventTrigger ::
      (forall b. (BackendMetadata b, BackendEventTrigger b) => i b -> a) ->
      AnyBackend i ->
      a
    dispatchMetadataAndEventTrigger f x = dispatchAnyBackendWithTwoConstraints @BackendMetadata @BackendEventTrigger x f

runMetadataQueryV2M ::
  ( MonadIO m,
    CacheRWM m,
    MetadataM m,
    MonadMetadataStorageQueryAPI m,
    MonadReader r m,
    Has (L.Logger L.Hasura) r
  ) =>
  MetadataResourceVersion ->
  RQLMetadataV2 ->
  m EncJSON
runMetadataQueryV2M currentResourceVersion = \case
  RMV2ReplaceMetadata q -> runReplaceMetadataV2 q
  RMV2ExportMetadata q -> runExportMetadataV2 currentResourceVersion q
