-- | The RQL metadata query ('/v1/metadata')

{-# LANGUAGE ViewPatterns #-}

module Hasura.Server.API.Metadata where

import           Hasura.Prelude

import qualified Data.Aeson.Types                          as A
import qualified Data.Environment                          as Env
import qualified Data.Text                                 as T
import qualified Data.Text.Extended                        as T
import qualified Network.HTTP.Client.Extended              as HTTP

import           Control.Monad.Trans.Control               (MonadBaseControl)
import           Control.Monad.Unique
import           Data.Aeson
import           Data.Aeson.Casing

import qualified Hasura.Tracing                            as Tracing

import           Hasura.Base.Error
import           Hasura.EncJSON
import           Hasura.Metadata.Class
import           Hasura.RQL.DDL.Action
import           Hasura.RQL.DDL.ApiLimit
import           Hasura.RQL.DDL.ComputedField
import           Hasura.RQL.DDL.CustomTypes
import           Hasura.RQL.DDL.Endpoint
import           Hasura.RQL.DDL.EventTrigger
import           Hasura.RQL.DDL.GraphqlSchemaIntrospection
import           Hasura.RQL.DDL.InheritedRoles
import           Hasura.RQL.DDL.Metadata
import           Hasura.RQL.DDL.Permission
import           Hasura.RQL.DDL.QueryCollection
import           Hasura.RQL.DDL.Relationship
import           Hasura.RQL.DDL.Relationship.Rename
import           Hasura.RQL.DDL.RemoteRelationship
import           Hasura.RQL.DDL.RemoteSchema
import           Hasura.RQL.DDL.ScheduledTrigger
import           Hasura.RQL.DDL.Schema
import           Hasura.RQL.DDL.Schema.Source
import           Hasura.RQL.Types
import           Hasura.RQL.Types.Run
import           Hasura.SQL.AnyBackend
import           Hasura.SQL.Tag
import           Hasura.Server.API.Backend
import           Hasura.Server.API.Instances               ()
import           Hasura.Server.Types                       (InstanceId (..), MaintenanceMode (..))
import           Hasura.Server.Utils                       (APIVersion (..))
import           Hasura.Server.Version                     (HasVersion)
import           Hasura.Session


data RQLMetadataV1
  -- Sources
  = RMAddSource  !(AnyBackend AddSource)
  | RMDropSource DropSource
  | RMRenameSource !RenameSource

  -- Tables
  | RMTrackTable            !(AnyBackend TrackTableV2)
  | RMUntrackTable          !(AnyBackend UntrackTable)
  | RMSetTableCustomization !(AnyBackend SetTableCustomization)

  -- Tables (PG-specific)
  | RMPgSetTableIsEnum        !SetTableIsEnum

  -- Tables permissions
  | RMCreateInsertPermission !(AnyBackend (CreatePerm InsPerm))
  | RMCreateSelectPermission !(AnyBackend (CreatePerm SelPerm))
  | RMCreateUpdatePermission !(AnyBackend (CreatePerm UpdPerm))
  | RMCreateDeletePermission !(AnyBackend (CreatePerm DelPerm))
  | RMDropInsertPermission   !(AnyBackend (DropPerm InsPerm))
  | RMDropSelectPermission   !(AnyBackend (DropPerm SelPerm))
  | RMDropUpdatePermission   !(AnyBackend (DropPerm UpdPerm))
  | RMDropDeletePermission   !(AnyBackend (DropPerm DelPerm))
  | RMSetPermissionComment   !(AnyBackend SetPermComment)

  -- Tables relationships
  | RMCreateObjectRelationship !(AnyBackend CreateObjRel)
  | RMCreateArrayRelationship  !(AnyBackend CreateArrRel)
  | RMDropRelationship         !(AnyBackend DropRel)
  | RMSetRelationshipComment   !(AnyBackend SetRelComment)
  | RMRenameRelationship       !(AnyBackend RenameRel)

  -- Tables remote relationships
  | RMCreateRemoteRelationship !(AnyBackend RemoteRelationship)
  | RMUpdateRemoteRelationship !(AnyBackend RemoteRelationship)
  | RMDeleteRemoteRelationship !(DeleteRemoteRelationship ('Postgres 'Vanilla))

  -- Functions
  | RMTrackFunction   !(AnyBackend TrackFunctionV2)
  | RMUntrackFunction !(AnyBackend UnTrackFunction)

  -- Functions permissions
  | RMCreateFunctionPermission !(AnyBackend CreateFunctionPermission)
  | RMDropFunctionPermission   !(AnyBackend DropFunctionPermission)

  -- Computed fields (PG-specific)
  | RMAddComputedField  !(AddComputedField  ('Postgres 'Vanilla))
  | RMDropComputedField !(DropComputedField ('Postgres 'Vanilla))

  -- Tables event triggers (PG-specific)
  | RMPgCreateEventTrigger !(CreateEventTriggerQuery ('Postgres 'Vanilla))
  | RMPgDeleteEventTrigger !(DeleteEventTriggerQuery ('Postgres 'Vanilla))
  | RMPgRedeliverEvent     !(RedeliverEventQuery     ('Postgres 'Vanilla))
  | RMPgInvokeEventTrigger !(InvokeEventTriggerQuery ('Postgres 'Vanilla))

  -- Remote schemas
  | RMAddRemoteSchema        !AddRemoteSchemaQuery
  | RMUpdateRemoteSchema     !AddRemoteSchemaQuery
  | RMRemoveRemoteSchema     !RemoteSchemaNameQuery
  | RMReloadRemoteSchema     !RemoteSchemaNameQuery
  | RMIntrospectRemoteSchema !RemoteSchemaNameQuery

  -- Remote schemas permissions
  | RMAddRemoteSchemaPermissions  !AddRemoteSchemaPermissions
  | RMDropRemoteSchemaPermissions !DropRemoteSchemaPermissions

  -- Scheduled triggers
  | RMCreateCronTrigger    !CreateCronTrigger
  | RMDeleteCronTrigger    !ScheduledTriggerName
  | RMCreateScheduledEvent !CreateScheduledEvent
  | RMDeleteScheduledEvent !DeleteScheduledEvent
  | RMGetScheduledEvents   !GetScheduledEvents
  | RMGetEventInvocations  !GetEventInvocations

  -- Actions
  | RMCreateAction           !CreateAction
  | RMDropAction             !DropAction
  | RMUpdateAction           !UpdateAction
  | RMCreateActionPermission !CreateActionPermission
  | RMDropActionPermission   !DropActionPermission

  -- Query collections, allow list related
  | RMCreateQueryCollection       !CreateCollection
  | RMDropQueryCollection         !DropCollection
  | RMAddQueryToCollection        !AddQueryToCollection
  | RMDropQueryFromCollection     !DropQueryFromCollection
  | RMAddCollectionToAllowlist    !CollectionReq
  | RMDropCollectionFromAllowlist !CollectionReq

  -- Rest endpoints
  | RMCreateRestEndpoint !CreateEndpoint
  | RMDropRestEndpoint   !DropEndpoint

  -- Custom types
  | RMSetCustomTypes !CustomTypes

  -- Api limits
  | RMSetApiLimits !ApiLimit
  | RMRemoveApiLimits

  -- Metrics config
  | RMSetMetricsConfig !MetricsConfig
  | RMRemoveMetricsConfig

  -- Inherited roles
  | RMAddInheritedRole !InheritedRole
  | RMDropInheritedRole !DropInheritedRole

  -- Metadata management
  | RMReplaceMetadata          !ReplaceMetadata
  | RMExportMetadata           !ExportMetadata
  | RMClearMetadata            !ClearMetadata
  | RMReloadMetadata           !ReloadMetadata
  | RMGetInconsistentMetadata  !GetInconsistentMetadata
  | RMDropInconsistentMetadata !DropInconsistentMetadata

  -- Introspection options
  | RMSetGraphqlSchemaIntrospectionOptions !SetGraphqlIntrospectionOptions

  -- Debug
  | RMDumpInternalState !DumpInternalState
  | RMGetCatalogState  !GetCatalogState
  | RMSetCatalogState  !SetCatalogState

  -- Bulk metadata queries
  | RMBulk [RQLMetadataRequest]
  deriving (Eq)

instance FromJSON RQLMetadataV1 where
  parseJSON =  withObject "RQLMetadataV1" \o -> do
    queryType <- o .: "type"
    let
      args :: forall a. FromJSON a => A.Parser a
      args = o .: "args"
    case queryType of
      -- backend agnostic
      "rename_source"                            -> RMRenameSource                         <$> args

      "add_remote_schema"                        -> RMAddRemoteSchema                      <$> args
      "update_remote_schema"                     -> RMUpdateRemoteSchema                   <$> args
      "remove_remote_schema"                     -> RMRemoveRemoteSchema                   <$> args
      "reload_remote_schema"                     -> RMReloadRemoteSchema                   <$> args
      "introspect_remote_schema"                 -> RMIntrospectRemoteSchema               <$> args

      "add_remote_schema_permissions"            -> RMAddRemoteSchemaPermissions           <$> args
      "drop_remote_schema_permissions"           -> RMDropRemoteSchemaPermissions          <$> args

      "create_cron_trigger"                      -> RMCreateCronTrigger                    <$> args
      "delete_cron_trigger"                      -> RMDeleteCronTrigger                    <$> args
      "create_scheduled_event"                   -> RMCreateScheduledEvent                 <$> args
      "delete_scheduled_event"                   -> RMDeleteScheduledEvent                 <$> args
      "get_scheduled_events"                     -> RMGetScheduledEvents                   <$> args
      "get_event_invocations"                    -> RMGetEventInvocations                  <$> args

      "create_action"                            -> RMCreateAction                         <$> args
      "drop_action"                              -> RMDropAction                           <$> args
      "update_action"                            -> RMUpdateAction                         <$> args
      "create_action_permission"                 -> RMCreateActionPermission               <$> args
      "drop_action_permission"                   -> RMDropActionPermission                 <$> args

      "create_query_collection"                  -> RMCreateQueryCollection                <$> args
      "drop_query_collection"                    -> RMDropQueryCollection                  <$> args
      "add_query_to_collection"                  -> RMAddQueryToCollection                 <$> args
      "drop_query_from_collection"               -> RMDropQueryFromCollection              <$> args
      "add_collection_to_allowlist"              -> RMAddCollectionToAllowlist             <$> args
      "drop_collection_from_allowlist"           -> RMDropCollectionFromAllowlist          <$> args

      "create_rest_endpoint"                     -> RMCreateRestEndpoint                   <$> args
      "drop_rest_endpoint"                       -> RMDropRestEndpoint                     <$> args

      "set_custom_types"                         -> RMSetCustomTypes                       <$> args

      "set_api_limits"                           -> RMSetApiLimits                         <$> args
      "remove_api_limits"                        -> pure RMRemoveApiLimits
      "set_metrics_config"                       -> RMSetMetricsConfig                     <$> args
      "remove_metrics_config"                    -> pure RMRemoveMetricsConfig
      "add_inherited_role"                       -> RMAddInheritedRole                     <$> args
      "drop_inherited_role"                      -> RMDropInheritedRole                    <$> args

      "replace_metadata"                         -> RMReplaceMetadata                      <$> args
      "export_metadata"                          -> RMExportMetadata                       <$> args
      "clear_metadata"                           -> RMClearMetadata                        <$> args
      "reload_metadata"                          -> RMReloadMetadata                       <$> args
      "get_inconsistent_metadata"                -> RMGetInconsistentMetadata              <$> args
      "drop_inconsistent_metadata"               -> RMDropInconsistentMetadata             <$> args

      "dump_internal_state"                      -> RMDumpInternalState                    <$> args
      "get_catalog_state"                        -> RMGetCatalogState                      <$> args
      "set_catalog_state"                        -> RMSetCatalogState                      <$> args

      "set_graphql_schema_introspection_options" -> RMSetGraphqlSchemaIntrospectionOptions <$> args

      "bulk"                                     -> RMBulk                                 <$> args

      -- backend specific
      _ -> do
        let (prefix, T.drop 1 -> cmd) = T.breakOn "_" queryType
        backendType <- runAesonParser parseJSON (String prefix)
          `onLeft` \_ -> fail (
            "unknown metadata command \"" <> T.unpack queryType <>
            "\"; \"" <> T.unpack prefix <> "\" was not recognized as a valid backend name"
          )
        dispatchAnyBackend @BackendAPI (liftTag backendType) \(_ :: BackendTag b) -> do
          argValue <- args
          command  <- choice <$> sequenceA [p cmd argValue | p <- metadataV1CommandParsers @b]
          onNothing command $ fail $
            "unknown metadata command \"" <> T.unpack cmd <>
            "\" for backend " <> T.unpack (T.toTxt backendType)


data RQLMetadataV2
  = RMV2ReplaceMetadata !ReplaceMetadataV2
  | RMV2ExportMetadata  !ExportMetadata
  deriving (Eq, Generic)

instance FromJSON RQLMetadataV2 where
  parseJSON = genericParseJSON $
    defaultOptions { constructorTagModifier = snakeCase . drop 4
                   , sumEncoding = TaggedObject "type" "args"
                   }


data RQLMetadataRequest
  = RMV1 !RQLMetadataV1
  | RMV2 !RQLMetadataV2
  deriving (Eq)

instance FromJSON RQLMetadataRequest where
  parseJSON = withObject "RQLMetadataRequest" $ \o -> do
    version <- o .:? "version" .!= VIVersion1
    let val = Object o
    case version of
      VIVersion1 -> RMV1 <$> parseJSON val
      VIVersion2 -> RMV2 <$> parseJSON val


data RQLMetadata
  = RQLMetadata
  { _rqlMetadataResourceVersion :: !(Maybe MetadataResourceVersion)
  , _rqlMetadata                :: !RQLMetadataRequest
  } deriving (Eq)

instance FromJSON RQLMetadata where
  parseJSON = withObject "RQLMetadata" $ \o -> do
    _rqlMetadataResourceVersion <- o .:? "resource_version"
    _rqlMetadata <- parseJSON $ Object o
    pure RQLMetadata{..}


runMetadataQuery
  :: ( HasVersion
     , MonadIO m
     , MonadBaseControl IO m
     , Tracing.MonadTrace m
     , MonadMetadataStorage m
     , MonadResolveSource m
     )
  => Env.Environment
  -> InstanceId
  -> UserInfo
  -> HTTP.Manager
  -> ServerConfigCtx
  -> RebuildableSchemaCache
  -> RQLMetadata
  -> m (EncJSON, RebuildableSchemaCache)
runMetadataQuery env instanceId userInfo httpManager serverConfigCtx schemaCache RQLMetadata{..} = do
  (metadata, currentResourceVersion) <- fetchMetadata
  ((r, modMetadata), modSchemaCache, cacheInvalidations) <-
    runMetadataQueryM env currentResourceVersion _rqlMetadata
    & runMetadataT metadata
    & runCacheRWT schemaCache
    & peelRun (RunCtx userInfo httpManager serverConfigCtx)
    & runExceptT
    & liftEitherM
  -- set modified metadata in storage
  if queryModifiesMetadata _rqlMetadata
    then
      case _sccMaintenanceMode serverConfigCtx of
        MaintenanceModeDisabled -> do
          -- set modified metadata in storage
          newResourceVersion <- setMetadata (fromMaybe currentResourceVersion _rqlMetadataResourceVersion) modMetadata
          -- notify schema cache sync
          notifySchemaCacheSync newResourceVersion instanceId cacheInvalidations
          (_, modSchemaCache', _) <- setMetadataResourceVersionInSchemaCache newResourceVersion
            & runCacheRWT modSchemaCache
            & peelRun (RunCtx userInfo httpManager serverConfigCtx)
            & runExceptT
            & liftEitherM
          pure (r, modSchemaCache')
        MaintenanceModeEnabled ->
          throw500 "metadata cannot be modified in maintenance mode"
    else
      pure (r, modSchemaCache)

queryModifiesMetadata :: RQLMetadataRequest -> Bool
queryModifiesMetadata = \case
  RMV1 q ->
    case q of
      RMPgRedeliverEvent _        -> False
      RMPgInvokeEventTrigger _    -> False
      RMGetInconsistentMetadata _ -> False
      RMIntrospectRemoteSchema _  -> False
      RMDumpInternalState _       -> False
      RMSetCatalogState _         -> False
      RMGetCatalogState _         -> False
      RMExportMetadata _          -> False
      RMGetEventInvocations _     -> False
      RMGetScheduledEvents _      -> False
      RMCreateScheduledEvent _    -> False
      RMDeleteScheduledEvent _    -> False
      RMBulk qs                   -> any queryModifiesMetadata qs
      _                           -> True
  RMV2 q ->
    case q of
      RMV2ExportMetadata _ -> False
      _                    -> True

runMetadataQueryM
  :: ( HasVersion
     , MonadIO m
     , MonadBaseControl IO m
     , CacheRWM m
     , Tracing.MonadTrace m
     , UserInfoM m
     , MonadUnique m
     , HTTP.HasHttpManagerM m
     , MetadataM m
     , MonadMetadataStorageQueryAPI m
     , HasServerConfigCtx m
     )
  => Env.Environment
  -> MetadataResourceVersion
  -> RQLMetadataRequest
  -> m EncJSON
runMetadataQueryM env currentResourceVersion = withPathK "args" . \case
  RMV1 q -> runMetadataQueryV1M env currentResourceVersion q
  RMV2 q -> runMetadataQueryV2M currentResourceVersion q

runMetadataQueryV1M
  :: forall m
   . ( HasVersion
     , MonadIO m
     , MonadBaseControl IO m
     , CacheRWM m
     , Tracing.MonadTrace m
     , UserInfoM m
     , MonadUnique m
     , HTTP.HasHttpManagerM m
     , MetadataM m
     , MonadMetadataStorageQueryAPI m
     , HasServerConfigCtx m
     )
  => Env.Environment
  -> MetadataResourceVersion
  -> RQLMetadataV1
  -> m EncJSON
runMetadataQueryV1M env currentResourceVersion = \case
  RMAddSource q                            -> dispatch runAddSource  q
  RMDropSource q                           -> runDropSource q
  RMRenameSource   q                       -> runRenameSource q

  RMTrackTable q                           -> dispatch runTrackTableV2Q q
  RMUntrackTable q                         -> dispatch runUntrackTableQ q
  RMSetTableCustomization q                -> dispatch runSetTableCustomization q

  RMPgSetTableIsEnum q                     -> runSetExistingTableIsEnumQ q

  RMCreateInsertPermission q               -> dispatch runCreatePerm q
  RMCreateSelectPermission q               -> dispatch runCreatePerm q
  RMCreateUpdatePermission q               -> dispatch runCreatePerm q
  RMCreateDeletePermission q               -> dispatch runCreatePerm q
  RMDropInsertPermission q                 -> dispatch runDropPerm q
  RMDropSelectPermission q                 -> dispatch runDropPerm q
  RMDropUpdatePermission q                 -> dispatch runDropPerm q
  RMDropDeletePermission q                 -> dispatch runDropPerm q
  RMSetPermissionComment q                 -> dispatch runSetPermComment q

  RMCreateObjectRelationship q             -> dispatch (runCreateRelationship ObjRel . unCreateObjRel) q
  RMCreateArrayRelationship q              -> dispatch (runCreateRelationship ArrRel . unCreateArrRel) q
  RMDropRelationship q                     -> dispatch runDropRel q
  RMSetRelationshipComment q               -> dispatch runSetRelComment q
  RMRenameRelationship q                   -> dispatch runRenameRel q

  RMCreateRemoteRelationship q             -> dispatch runCreateRemoteRelationship q
  RMUpdateRemoteRelationship q             -> dispatch runUpdateRemoteRelationship q
  RMDeleteRemoteRelationship q             -> runDeleteRemoteRelationship q

  RMTrackFunction q                        -> dispatch runTrackFunctionV2 q
  RMUntrackFunction q                      -> dispatch runUntrackFunc q

  RMCreateFunctionPermission q             -> dispatch runCreateFunctionPermission q
  RMDropFunctionPermission q               -> dispatch runDropFunctionPermission q

  RMAddComputedField q                     -> runAddComputedField q
  RMDropComputedField q                    -> runDropComputedField q

  RMPgCreateEventTrigger q                 -> runCreateEventTriggerQuery q
  RMPgDeleteEventTrigger q                 -> runDeleteEventTriggerQuery q
  RMPgRedeliverEvent     q                 -> runRedeliverEvent q
  RMPgInvokeEventTrigger q                 -> runInvokeEventTrigger q

  RMAddRemoteSchema q                      -> runAddRemoteSchema env q
  RMUpdateRemoteSchema q                   -> runUpdateRemoteSchema env q
  RMRemoveRemoteSchema q                   -> runRemoveRemoteSchema q
  RMReloadRemoteSchema q                   -> runReloadRemoteSchema q
  RMIntrospectRemoteSchema q               -> runIntrospectRemoteSchema q

  RMAddRemoteSchemaPermissions q           -> runAddRemoteSchemaPermissions q
  RMDropRemoteSchemaPermissions q          -> runDropRemoteSchemaPermissions q

  RMCreateCronTrigger q                    -> runCreateCronTrigger q
  RMDeleteCronTrigger q                    -> runDeleteCronTrigger q
  RMCreateScheduledEvent q                 -> runCreateScheduledEvent q
  RMDeleteScheduledEvent q                 -> runDeleteScheduledEvent q
  RMGetScheduledEvents q                   -> runGetScheduledEvents q
  RMGetEventInvocations q                  -> runGetEventInvocations q

  RMCreateAction q                         -> runCreateAction q
  RMDropAction q                           -> runDropAction q
  RMUpdateAction q                         -> runUpdateAction q
  RMCreateActionPermission q               -> runCreateActionPermission q
  RMDropActionPermission q                 -> runDropActionPermission q

  RMCreateQueryCollection q                -> runCreateCollection q
  RMDropQueryCollection q                  -> runDropCollection q
  RMAddQueryToCollection q                 -> runAddQueryToCollection q
  RMDropQueryFromCollection q              -> runDropQueryFromCollection q
  RMAddCollectionToAllowlist q             -> runAddCollectionToAllowlist q
  RMDropCollectionFromAllowlist q          -> runDropCollectionFromAllowlist q

  RMCreateRestEndpoint q                   -> runCreateEndpoint q
  RMDropRestEndpoint q                     -> runDropEndpoint q

  RMSetCustomTypes q                       -> runSetCustomTypes q

  RMSetApiLimits q                         -> runSetApiLimits q
  RMRemoveApiLimits                        -> runRemoveApiLimits

  RMSetMetricsConfig q                     -> runSetMetricsConfig q
  RMRemoveMetricsConfig                    -> runRemoveMetricsConfig

  RMAddInheritedRole q                     -> runAddInheritedRole q
  RMDropInheritedRole q                    -> runDropInheritedRole q

  RMReplaceMetadata q                      -> runReplaceMetadata q
  RMExportMetadata q                       -> runExportMetadata q
  RMClearMetadata q                        -> runClearMetadata q
  RMReloadMetadata q                       -> runReloadMetadata q
  RMGetInconsistentMetadata q              -> runGetInconsistentMetadata q
  RMDropInconsistentMetadata q             -> runDropInconsistentMetadata q

  RMSetGraphqlSchemaIntrospectionOptions q -> runSetGraphqlSchemaIntrospectionOptions q

  RMDumpInternalState q                    -> runDumpInternalState q
  RMGetCatalogState q                      -> runGetCatalogState q
  RMSetCatalogState q                      -> runSetCatalogState q

  RMBulk q                                 -> encJFromList <$> indexedMapM (runMetadataQueryM env currentResourceVersion) q
  where
    dispatch
      :: (forall b. BackendMetadata b => i b -> a)
      -> AnyBackend i
      -> a
    dispatch f x = dispatchAnyBackend @BackendMetadata x f


runMetadataQueryV2M
  :: ( MonadIO m
     , CacheRWM m
     , MetadataM m
     , MonadMetadataStorageQueryAPI m
     , HasServerConfigCtx m
     )
  => MetadataResourceVersion
  -> RQLMetadataV2
  -> m EncJSON
runMetadataQueryV2M currentResourceVersion = \case
  RMV2ReplaceMetadata q -> runReplaceMetadataV2 q
  RMV2ExportMetadata q  -> runExportMetadataV2 currentResourceVersion q
