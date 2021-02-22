-- | The RQL metadata query ('/v1/metadata')
module Hasura.Server.API.Metadata where

import           Hasura.Prelude

import qualified Data.Environment                   as Env
import qualified Network.HTTP.Client.Extended       as HTTP

import           Control.Monad.Trans.Control        (MonadBaseControl)
import           Control.Monad.Unique
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH

import qualified Hasura.Tracing                     as Tracing

import           Hasura.EncJSON
import           Hasura.Metadata.Class
import           Hasura.RQL.DDL.Action
import           Hasura.RQL.DDL.ApiLimit
import           Hasura.RQL.DDL.ComputedField
import           Hasura.RQL.DDL.CustomTypes
import           Hasura.RQL.DDL.Endpoint
import           Hasura.RQL.DDL.EventTrigger
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
import           Hasura.Server.Types                (InstanceId (..), MaintenanceMode (..))
import           Hasura.Server.Utils                (APIVersion (..))
import           Hasura.Server.Version              (HasVersion)
import           Hasura.Session


data RQLMetadataV1
  = RMPgAddSource !AddPgSource
  | RMPgDropSource !DropPgSource

  | RMPgTrackTable !TrackTableV2
  | RMPgUntrackTable !(UntrackTable 'Postgres)
  | RMPgSetTableIsEnum !SetTableIsEnum
  | RMPgSetTableCustomization !SetTableCustomization

  -- Postgres functions
  | RMPgTrackFunction !TrackFunctionV2
  | RMPgUntrackFunction !(UnTrackFunction 'Postgres)

  -- Postgres function permissions
  | RMPgCreateFunctionPermission !(CreateFunctionPermission 'Postgres)
  | RMPgDropFunctionPermission !(DropFunctionPermission 'Postgres)

  -- Postgres table relationships
  | RMPgCreateObjectRelationship !(CreateObjRel 'Postgres)
  | RMPgCreateArrayRelationship !(CreateArrRel 'Postgres)
  | RMPgDropRelationship !DropRel
  | RMPgSetRelationshipComment !SetRelComment
  | RMPgRenameRelationship !RenameRel

  -- Postgres computed fields
  | RMPgAddComputedField !(AddComputedField 'Postgres)
  | RMPgDropComputedField !(DropComputedField 'Postgres)

  -- Postgres tables remote relationships
  | RMPgCreateRemoteRelationship !(RemoteRelationship 'Postgres)
  | RMPgUpdateRemoteRelationship !(RemoteRelationship 'Postgres)
  | RMPgDeleteRemoteRelationship !DeleteRemoteRelationship

  -- Postgres tables permissions
  | RMPgCreateInsertPermission !(CreateInsPerm 'Postgres)
  | RMPgCreateSelectPermission !(CreateSelPerm 'Postgres)
  | RMPgCreateUpdatePermission !(CreateUpdPerm 'Postgres)
  | RMPgCreateDeletePermission !(CreateDelPerm 'Postgres)

  | RMPgDropInsertPermission !(DropPerm 'Postgres (InsPerm 'Postgres))
  | RMPgDropSelectPermission !(DropPerm 'Postgres (SelPerm 'Postgres))
  | RMPgDropUpdatePermission !(DropPerm 'Postgres (UpdPerm 'Postgres))
  | RMPgDropDeletePermission !(DropPerm 'Postgres (DelPerm 'Postgres))
  | RMPgSetPermissionComment !SetPermComment

  -- Postgres tables event triggers
  | RMPgCreateEventTrigger !CreateEventTriggerQuery
  | RMPgDeleteEventTrigger !DeleteEventTriggerQuery
  | RMPgRedeliverEvent     !RedeliverEventQuery
  | RMPgInvokeEventTrigger !InvokeEventTriggerQuery

  -- Inconsistent metadata
  | RMGetInconsistentMetadata !GetInconsistentMetadata
  | RMDropInconsistentMetadata !DropInconsistentMetadata

  -- Remote schemas
  | RMAddRemoteSchema !AddRemoteSchemaQuery
  | RMRemoveRemoteSchema !RemoteSchemaNameQuery
  | RMReloadRemoteSchema !RemoteSchemaNameQuery
  | RMIntrospectRemoteSchema !RemoteSchemaNameQuery

  -- remote-schema permissions
  | RMAddRemoteSchemaPermissions !AddRemoteSchemaPermissions
  | RMDropRemoteSchemaPermissions !DropRemoteSchemaPermissions

  -- scheduled triggers
  | RMCreateCronTrigger !CreateCronTrigger
  | RMDeleteCronTrigger !ScheduledTriggerName
  | RMCreateScheduledEvent !CreateScheduledEvent
  | RMDeleteScheduledEvent !DeleteScheduledEvent
  | RMGetScheduledEvents !GetScheduledEvents
  | RMGetEventInvocations !GetEventInvocations

  -- query collections, allow list related
  | RMCreateQueryCollection !CreateCollection
  | RMDropQueryCollection !DropCollection
  | RMAddQueryToCollection !AddQueryToCollection
  | RMDropQueryFromCollection !DropQueryFromCollection
  | RMAddCollectionToAllowlist !CollectionReq
  | RMDropCollectionFromAllowlist !CollectionReq

  -- basic metadata management
  | RMReplaceMetadata !ReplaceMetadata
  | RMExportMetadata !ExportMetadata
  | RMClearMetadata !ClearMetadata
  | RMReloadMetadata !ReloadMetadata

  -- actions
  | RMCreateAction !CreateAction
  | RMDropAction !DropAction
  | RMUpdateAction !UpdateAction
  | RMCreateActionPermission !CreateActionPermission
  | RMDropActionPermission !DropActionPermission

  | RMCreateRestEndpoint !CreateEndpoint
  | RMDropRestEndpoint !DropEndpoint

  | RMSetCustomTypes !CustomTypes

  | RMDumpInternalState !DumpInternalState

  | RMGetCatalogState !GetCatalogState
  | RMSetCatalogState !SetCatalogState

  -- 'ApiLimit' related
  | RMSetApiLimits !ApiLimit
  | RMRemoveApiLimits

  -- 'MetricsConfig' related
  | RMSetMetricsConfig !MetricsConfig
  | RMRemoveMetricsConfig

  -- bulk metadata queries
  | RMBulk [RQLMetadataRequest]
  deriving (Eq)

data RQLMetadataV2
  = RMV2ReplaceMetadata !ReplaceMetadataV2
  | RMV2ExportMetadata !ExportMetadata
  deriving (Eq)

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

instance ToJSON RQLMetadataRequest where
  toJSON = \case
    RMV1 q -> embedVersion VIVersion1 $ toJSON q
    RMV2 q -> embedVersion VIVersion2 $ toJSON q
    where
      embedVersion version (Object o) =
        Object $ o <> "version" .= version
      -- never happens since JSON value of RQL queries are always objects
      embedVersion _ _ = error "Unexpected: toJSON of RQLMetadtaV is not an object"

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

instance ToJSON RQLMetadata where
  toJSON RQLMetadata{..} =
    embedResourceVersion $ toJSON _rqlMetadata
    where
      embedResourceVersion (Object o) =
        Object $ o <> "resource_version" .= _rqlMetadataResourceVersion
      -- never happens since JSON value of RQL queries are always objects
      embedResourceVersion _ = error "Unexpected: toJSON of RQLMetadata is not an object"

$(deriveJSON
  defaultOptions { constructorTagModifier = snakeCase . drop 2
                 , sumEncoding = TaggedObject "type" "args"
                 }
  ''RQLMetadataV1)

$(deriveJSON
  defaultOptions { constructorTagModifier = snakeCase . drop 4
                 , sumEncoding = TaggedObject "type" "args"
                 }
  ''RQLMetadataV2)

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
  when (queryModifiesMetadata _rqlMetadata) $
    case (_sccMaintenanceMode serverConfigCtx) of
      MaintenanceModeDisabled ->
        -- set modified metadata in storage
        setMetadata (fromMaybe currentResourceVersion _rqlMetadataResourceVersion) modMetadata
      MaintenanceModeEnabled ->
        throw500 "metadata cannot be modified in maintenance mode"
  -- notify schema cache sync
  notifySchemaCacheSync instanceId cacheInvalidations

  pure (r, modSchemaCache)

queryModifiesMetadata :: RQLMetadataRequest -> Bool
queryModifiesMetadata = \case
  RMV1 q ->
    case q of
      RMGetCatalogState _      -> False
      RMExportMetadata _       -> False
      RMGetEventInvocations _  -> False
      RMGetScheduledEvents _   -> False

      RMCreateScheduledEvent _ -> False
      RMDeleteScheduledEvent _ -> False
      RMBulk qs                -> any queryModifiesMetadata qs
      _                        -> True
  RMV2 q ->
    case q of
      RMV2ExportMetadata _  -> False
      RMV2ReplaceMetadata _ -> True

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
  -> RQLMetadataV1
  -> m EncJSON
runMetadataQueryV1M env currentResourceVersion = \case
  RMPgAddSource q                 -> runAddPgSource q
  RMPgDropSource q                -> runDropPgSource q

  RMPgTrackTable q                -> runTrackTableV2Q q
  RMPgUntrackTable q              -> runUntrackTableQ q
  RMPgSetTableIsEnum q            -> runSetExistingTableIsEnumQ q
  RMPgSetTableCustomization q     -> runSetTableCustomization q

  RMPgTrackFunction q             -> runTrackFunctionV2 q
  RMPgUntrackFunction q           -> runUntrackFunc q

  RMPgCreateFunctionPermission q  -> runCreateFunctionPermission q
  RMPgDropFunctionPermission   q  -> runDropFunctionPermission q

  RMPgCreateObjectRelationship q  -> runCreateRelationship ObjRel q
  RMPgCreateArrayRelationship q   -> runCreateRelationship ArrRel q
  RMPgDropRelationship q          -> runDropRel q
  RMPgSetRelationshipComment q    -> runSetRelComment q
  RMPgRenameRelationship q        -> runRenameRel q

  RMPgAddComputedField q          -> runAddComputedField q
  RMPgDropComputedField q         -> runDropComputedField q

  RMPgCreateRemoteRelationship q  -> runCreateRemoteRelationship q
  RMPgUpdateRemoteRelationship q  -> runUpdateRemoteRelationship q
  RMPgDeleteRemoteRelationship q  -> runDeleteRemoteRelationship q

  RMPgCreateInsertPermission q    -> runCreatePerm q
  RMPgCreateSelectPermission q    -> runCreatePerm q
  RMPgCreateUpdatePermission q    -> runCreatePerm q
  RMPgCreateDeletePermission q    -> runCreatePerm q

  RMPgDropInsertPermission q      -> runDropPerm q
  RMPgDropSelectPermission q      -> runDropPerm q
  RMPgDropUpdatePermission q      -> runDropPerm q
  RMPgDropDeletePermission q      -> runDropPerm q
  RMPgSetPermissionComment q      -> runSetPermComment q

  RMPgCreateEventTrigger q        -> runCreateEventTriggerQuery q
  RMPgDeleteEventTrigger q        -> runDeleteEventTriggerQuery q
  RMPgRedeliverEvent     q        -> runRedeliverEvent q
  RMPgInvokeEventTrigger q        -> runInvokeEventTrigger q

  RMGetInconsistentMetadata q     -> runGetInconsistentMetadata q
  RMDropInconsistentMetadata q    -> runDropInconsistentMetadata q

  RMAddRemoteSchema q             -> runAddRemoteSchema env q
  RMRemoveRemoteSchema q          -> runRemoveRemoteSchema q
  RMReloadRemoteSchema q          -> runReloadRemoteSchema q
  RMIntrospectRemoteSchema q      -> runIntrospectRemoteSchema q

  RMAddRemoteSchemaPermissions q  -> runAddRemoteSchemaPermissions q
  RMDropRemoteSchemaPermissions q -> runDropRemoteSchemaPermissions q

  RMCreateCronTrigger q           -> runCreateCronTrigger q
  RMDeleteCronTrigger q           -> runDeleteCronTrigger q
  RMCreateScheduledEvent q        -> runCreateScheduledEvent q
  RMDeleteScheduledEvent q        -> runDeleteScheduledEvent q
  RMGetScheduledEvents q          -> runGetScheduledEvents q
  RMGetEventInvocations q         -> runGetEventInvocations q

  RMCreateQueryCollection q       -> runCreateCollection q
  RMDropQueryCollection q         -> runDropCollection q
  RMAddQueryToCollection q        -> runAddQueryToCollection q
  RMDropQueryFromCollection q     -> runDropQueryFromCollection q
  RMAddCollectionToAllowlist q    -> runAddCollectionToAllowlist q
  RMDropCollectionFromAllowlist q -> runDropCollectionFromAllowlist q

  RMReplaceMetadata q             -> runReplaceMetadata q
  RMExportMetadata q              -> runExportMetadata q
  RMClearMetadata q               -> runClearMetadata q
  RMReloadMetadata q              -> runReloadMetadata q

  RMCreateAction q                -> runCreateAction q
  RMDropAction q                  -> runDropAction q
  RMUpdateAction q                -> runUpdateAction q
  RMCreateActionPermission q      -> runCreateActionPermission q
  RMDropActionPermission q        -> runDropActionPermission q

  RMCreateRestEndpoint q          -> runCreateEndpoint q
  RMDropRestEndpoint q            -> runDropEndpoint q

  RMSetCustomTypes q              -> runSetCustomTypes q

  RMDumpInternalState q           -> runDumpInternalState q

  RMGetCatalogState q             -> runGetCatalogState q
  RMSetCatalogState q             -> runSetCatalogState q

  RMSetApiLimits q                -> runSetApiLimits q
  RMRemoveApiLimits               -> runRemoveApiLimits

  RMSetMetricsConfig q            -> runSetMetricsConfig q
  RMRemoveMetricsConfig           -> runRemoveMetricsConfig

  RMBulk q                        -> encJFromList <$> indexedMapM (runMetadataQueryM env currentResourceVersion) q

runMetadataQueryV2M
  :: ( MonadIO m
     , CacheRWM m
     , MetadataM m
     , MonadMetadataStorageQueryAPI m
     )
  => MetadataResourceVersion
  -> RQLMetadataV2
  -> m EncJSON
runMetadataQueryV2M currentResourceVersion = \case
  RMV2ReplaceMetadata q -> runReplaceMetadataV2 q
  RMV2ExportMetadata q  -> runExportMetadataV2 currentResourceVersion q
