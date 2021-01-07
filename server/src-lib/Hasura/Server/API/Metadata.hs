-- | The RQL metadata query ('/v1/metadata')
module Hasura.Server.API.Metadata where

import           Control.Lens
import           Control.Monad.Trans.Control        (MonadBaseControl)
import           Control.Monad.Unique
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH

import qualified Data.Environment                   as Env
import qualified Network.HTTP.Client                as HTTP

import           Hasura.EncJSON
import           Hasura.Metadata.Class
import           Hasura.Prelude
import           Hasura.RQL.DDL.Action
import           Hasura.RQL.DDL.ComputedField
import           Hasura.RQL.DDL.CustomTypes
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
import           Hasura.Server.Types                (InstanceId (..))
import           Hasura.Server.Version              (HasVersion)
import           Hasura.Session

import qualified Hasura.Tracing                     as Tracing

data RQLMetadata
  = RMPgAddSource !AddPgSource
  | RMPgDropSource !DropPgSource

  | RMPgTrackTable !TrackTableV2
  | RMPgUntrackTable !UntrackTable
  | RMPgSetTableIsEnum !SetTableIsEnum
  | RMPgSetTableCustomFields !SetTableCustomFields

  -- Postgres functions
  | RMPgTrackFunction !TrackFunctionV2
  | RMPgUntrackFunction !UnTrackFunction

  -- Postgres table relationships
  | RMPgCreateObjectRelationship !CreateObjRel
  | RMPgCreateArrayRelationship !CreateArrRel
  | RMPgDropRelationship !DropRel
  | RMPgSetRelationshipComment !SetRelComment
  | RMPgRenameRelationship !RenameRel

  -- Postgres computed fields
  | RMPgAddComputedField !AddComputedField
  | RMPgDropComputedField !DropComputedField

  -- Postgres tables remote relationships
  | RMPgCreateRemoteRelationship !RemoteRelationship
  | RMPgUpdateRemoteRelationship !RemoteRelationship
  | RMPgDeleteRemoteRelationship !DeleteRemoteRelationship

  -- Postgres tables permissions
  | RMPgCreateInsertPermission !(CreateInsPerm 'Postgres)
  | RMPgCreateSelectPermission !(CreateSelPerm 'Postgres)
  | RMPgCreateUpdatePermission !(CreateUpdPerm 'Postgres)
  | RMPgCreateDeletePermission !(CreateDelPerm 'Postgres)

  | RMPgDropInsertPermission !(DropPerm (InsPerm 'Postgres))
  | RMPgDropSelectPermission !(DropPerm (SelPerm 'Postgres))
  | RMPgDropUpdatePermission !(DropPerm (UpdPerm 'Postgres))
  | RMPgDropDeletePermission !(DropPerm (DelPerm 'Postgres))
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

  | RMSetCustomTypes !CustomTypes

  | RMDumpInternalState !DumpInternalState

  | RMGetCatalogState !GetCatalogState
  | RMSetCatalogState !SetCatalogState

  -- bulk metadata queries
  | RMBulk [RQLMetadata]
  deriving (Show, Eq)

$(deriveJSON
  defaultOptions { constructorTagModifier = snakeCase . drop 2
                 , sumEncoding = TaggedObject "type" "args"
                 }
  ''RQLMetadata)

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
  -> SQLGenCtx
  -> RemoteSchemaPermsCtx
  -> RebuildableSchemaCache
  -> RQLMetadata
  -> m (EncJSON, RebuildableSchemaCache)
runMetadataQuery env instanceId userInfo httpManager sqlGenCtx remoteSchemaPermsCtx schemaCache query = do
  metadata <- fetchMetadata
  ((r, modMetadata), modSchemaCache, cacheInvalidations) <-
    runMetadataQueryM env query
    & runMetadataT metadata
    & runCacheRWT schemaCache
    & peelRun (RunCtx userInfo httpManager sqlGenCtx remoteSchemaPermsCtx)
    & runExceptT
    & liftEitherM

  -- set modified metadata in storage
  setMetadata modMetadata
  -- notify schema cache sync
  notifySchemaCacheSync instanceId cacheInvalidations

  pure (r, modSchemaCache)

runMetadataQueryM
  :: ( HasVersion
     , MonadIO m
     , MonadBaseControl IO m
     , CacheRWM m
     , Tracing.MonadTrace m
     , UserInfoM m
     , MonadUnique m
     , HasHttpManager m
     , MetadataM m
     , MonadMetadataStorageQueryAPI m
     )
  => Env.Environment
  -> RQLMetadata
  -> m EncJSON
runMetadataQueryM env = \case
  RMPgAddSource q    -> runAddPgSource q
  RMPgDropSource q   -> runDropPgSource q

  RMPgTrackTable q           -> runTrackTableV2Q q
  RMPgUntrackTable q         -> runUntrackTableQ q
  RMPgSetTableIsEnum q       -> runSetExistingTableIsEnumQ q
  RMPgSetTableCustomFields q -> runSetTableCustomFieldsQV2 q

  RMPgTrackFunction q   -> runTrackFunctionV2 q
  RMPgUntrackFunction q -> runUntrackFunc q

  RMPgCreateObjectRelationship q -> runCreateRelationship ObjRel q
  RMPgCreateArrayRelationship q  -> runCreateRelationship ArrRel q
  RMPgDropRelationship q         -> runDropRel q
  RMPgSetRelationshipComment q   -> runSetRelComment q
  RMPgRenameRelationship q       -> runRenameRel q

  RMPgAddComputedField q  -> runAddComputedField q
  RMPgDropComputedField q -> runDropComputedField q

  RMPgCreateRemoteRelationship q -> runCreateRemoteRelationship q
  RMPgUpdateRemoteRelationship q -> runUpdateRemoteRelationship q
  RMPgDeleteRemoteRelationship q -> runDeleteRemoteRelationship q

  RMPgCreateInsertPermission q -> runCreatePerm q
  RMPgCreateSelectPermission q -> runCreatePerm q
  RMPgCreateUpdatePermission q -> runCreatePerm q
  RMPgCreateDeletePermission q -> runCreatePerm q

  RMPgDropInsertPermission q -> runDropPerm q
  RMPgDropSelectPermission q -> runDropPerm q
  RMPgDropUpdatePermission q -> runDropPerm q
  RMPgDropDeletePermission q -> runDropPerm q
  RMPgSetPermissionComment q -> runSetPermComment q

  RMPgCreateEventTrigger q -> runCreateEventTriggerQuery q
  RMPgDeleteEventTrigger q -> runDeleteEventTriggerQuery q
  RMPgRedeliverEvent     q -> runRedeliverEvent q
  RMPgInvokeEventTrigger q -> runInvokeEventTrigger q

  RMGetInconsistentMetadata q -> runGetInconsistentMetadata q
  RMDropInconsistentMetadata q -> runDropInconsistentMetadata q

  RMAddRemoteSchema q -> runAddRemoteSchema env q
  RMRemoveRemoteSchema q -> runRemoveRemoteSchema q
  RMReloadRemoteSchema q -> runReloadRemoteSchema q
  RMIntrospectRemoteSchema q -> runIntrospectRemoteSchema q

  RMCreateCronTrigger q    -> runCreateCronTrigger q
  RMDeleteCronTrigger q    -> runDeleteCronTrigger q
  RMCreateScheduledEvent q -> runCreateScheduledEvent q
  RMDeleteScheduledEvent q -> runDeleteScheduledEvent q
  RMGetScheduledEvents q   -> runGetScheduledEvents q
  RMGetEventInvocations q  -> runGetEventInvocations q

  RMCreateQueryCollection q -> runCreateCollection q
  RMDropQueryCollection q -> runDropCollection q
  RMAddQueryToCollection q -> runAddQueryToCollection q
  RMDropQueryFromCollection q -> runDropQueryFromCollection q
  RMAddCollectionToAllowlist q -> runAddCollectionToAllowlist q
  RMDropCollectionFromAllowlist q -> runDropCollectionFromAllowlist q

  RMReplaceMetadata q -> runReplaceMetadata q
  RMExportMetadata q -> runExportMetadata q
  RMClearMetadata q -> runClearMetadata q
  RMReloadMetadata q -> runReloadMetadata q

  RMCreateAction q -> runCreateAction q
  RMDropAction q -> runDropAction q
  RMUpdateAction q -> runUpdateAction q
  RMCreateActionPermission q -> runCreateActionPermission q
  RMDropActionPermission q -> runDropActionPermission q

  RMSetCustomTypes q -> runSetCustomTypes q

  RMDumpInternalState q -> runDumpInternalState q

  RMGetCatalogState q -> runGetCatalogState q
  RMSetCatalogState q -> runSetCatalogState q

  RMBulk q -> encJFromList <$> indexedMapM (runMetadataQueryM env) q
