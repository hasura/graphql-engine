{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | `FromJSON` instances for API.Metadata. Kept separately to discourage
-- becoming a dumping ground for orphan instances
module Hasura.Server.API.Metadata.Instances () where

import Data.Aeson
import Data.Aeson.Casing
import Data.Aeson.Types qualified as A
import Data.Text qualified as T
import Data.Text.Extended qualified as T
import Hasura.Prelude hiding (first)
import Hasura.RQL.Types.BackendType
import Hasura.SQL.AnyBackend
import Hasura.Server.API.Backend
import Hasura.Server.API.Instances ()
import Hasura.Server.API.Metadata.Types
import Hasura.Server.Utils (APIVersion (..))

-- | Note! You're seeing some orphan instances for `FromJSON` for types in
-- `Metadata.Types`. This is because we need to use `dispatchAnyBackend`.
-- Keeping these here avoids us needing an hs-boot file and a load of cyclical
-- hell.
instance FromJSON RQLMetadataV1 where
  parseJSON = withObject "RQLMetadataV1" \o -> do
    queryType <- o .: "type"
    let args :: forall a. (FromJSON a) => A.Parser a
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
      "cleanup_event_trigger_logs" -> RMCleanupEventTriggerLog <$> args
      "resume_event_trigger_cleanups" -> RMResumeEventTriggerCleanup <$> args
      "pause_event_trigger_cleanups" -> RMPauseEventTriggerCleanup <$> args
      "create_cron_trigger" -> RMCreateCronTrigger <$> args
      "delete_cron_trigger" -> RMDeleteCronTrigger <$> args
      "create_scheduled_event" -> RMCreateScheduledEvent <$> args
      "delete_scheduled_event" -> RMDeleteScheduledEvent <$> args
      "get_scheduled_events" -> RMGetScheduledEvents <$> args
      "get_scheduled_event_invocations" -> RMGetScheduledEventInvocations <$> args
      "get_cron_triggers" -> pure RMGetCronTriggers
      "create_action" -> RMCreateAction <$> args
      "drop_action" -> RMDropAction <$> args
      "update_action" -> RMUpdateAction <$> args
      "create_action_permission" -> RMCreateActionPermission <$> args
      "drop_action_permission" -> RMDropActionPermission <$> args
      "create_query_collection" -> RMCreateQueryCollection <$> args
      "rename_query_collection" -> RMRenameQueryCollection <$> args
      "drop_query_collection" -> RMDropQueryCollection <$> args
      "add_query_to_collection" -> RMAddQueryToCollection <$> args
      "drop_query_from_collection" -> RMDropQueryFromCollection <$> args
      "add_collection_to_allowlist" -> RMAddCollectionToAllowlist <$> args
      "drop_collection_from_allowlist" -> RMDropCollectionFromAllowlist <$> args
      "update_scope_of_collection_in_allowlist" -> RMUpdateScopeOfCollectionInAllowlist <$> args
      "create_rest_endpoint" -> RMCreateRestEndpoint <$> args
      "drop_rest_endpoint" -> RMDropRestEndpoint <$> args
      "dc_add_agent" -> RMDCAddAgent <$> args
      "dc_delete_agent" -> RMDCDeleteAgent <$> args
      "list_source_kinds" -> RMListSourceKinds <$> args
      "get_source_kind_capabilities" -> RMGetSourceKindCapabilities <$> args
      "get_table_info" -> RMGetTableInfo_ <$> args
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
      "set_opentelemetry_config" -> RMSetOpenTelemetryConfig <$> args
      "set_opentelemetry_status" -> RMSetOpenTelemetryStatus <$> args
      "bulk" -> RMBulk <$> args
      "bulk_keep_going" -> RMBulkKeepGoing <$> args
      "bulk_atomic" -> RMBulkAtomic <$> args
      -- Backend prefixed metadata actions:
      _ -> do
        -- 1) Parse the backend source kind and metadata command:
        (backendSourceKind, cmd) <- parseQueryType queryType
        dispatchAnyBackend @BackendAPI backendSourceKind \(backendSourceKind' :: BackendSourceKind b) -> do
          -- 2) Parse the args field:
          argValue <- args
          -- 2) Attempt to run all the backend specific command parsers against the source kind, cmd, and arg:
          -- NOTE: If parsers succeed then this will pick out the first successful one.
          command <- choice <$> sequenceA [p backendSourceKind' cmd argValue | p <- metadataV1CommandParsers @b]
          onNothing command
            $ fail
            $ "unknown metadata command \""
            <> T.unpack cmd
            <> "\" for backend "
            <> T.unpack (T.toTxt backendSourceKind')

instance FromJSON RQLMetadataV2 where
  parseJSON =
    genericParseJSON
      $ defaultOptions
        { constructorTagModifier = snakeCase . drop 4,
          sumEncoding = TaggedObject "type" "args"
        }

instance FromJSON RQLMetadataRequest where
  parseJSON = withObject "RQLMetadataRequest" $ \o -> do
    version <- o .:? "version" .!= VIVersion1
    let val = Object o
    case version of
      VIVersion1 -> RMV1 <$> parseJSON val
      VIVersion2 -> RMV2 <$> parseJSON val

-- | end of orphan instances

-- | Parse the Metadata API action type returning a tuple of the
-- 'BackendSourceKind' and the action suffix.
--
-- For example: @"pg_add_source"@ parses as @(PostgresVanillaValue, "add_source")@
parseQueryType :: (MonadFail m) => Text -> m (AnyBackend BackendSourceKind, Text)
parseQueryType queryType =
  let (prefix, T.drop 1 -> cmd) = T.breakOn "_" queryType
   in (,cmd)
        <$> backendSourceKindFromText prefix
        `onNothing` fail
          ( "unknown metadata command \""
              <> T.unpack queryType
              <> "\"; \""
              <> T.unpack prefix
              <> "\" was not recognized as a valid backend name"
          )
