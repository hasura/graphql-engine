-- | The RQL metadata query ('/v1/metadata')
module Hasura.Server.API.Metadata where

import           Hasura.Class
import           Hasura.EncJSON
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
import           Hasura.Server.Version              (HasVersion)
import           Hasura.Session

import           Control.Monad.Unique
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH

import qualified Data.Environment                   as Env
import qualified Network.HTTP.Client                as HTTP

data RQLMetadata
  = RMPgAddSource !AddPgSource
  | RMPgDropSource !DropPgSource
  | RMPgReloadSource !PGSourceName

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
  | RMPgCreateInsertPermission !CreateInsPerm
  | RMPgCreateSelectPermission !CreateSelPerm
  | RMPgCreateUpdatePermission !CreateUpdPerm
  | RMPgCreateDeletePermission !CreateDelPerm

  | RMPgDropInsertPermission !(DropPerm InsPerm)
  | RMPgDropSelectPermission !(DropPerm SelPerm)
  | RMPgDropUpdatePermission !(DropPerm UpdPerm)
  | RMPgDropDeletePermission !(DropPerm DelPerm)
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

runMetadataRequest
  :: ( HasVersion
     , MonadIO m
     , MonadError QErr m
     , MonadUnique m
     , MonadMetadataStorage m
     )
  => Env.Environment
  -> UserInfo
  -> HTTP.Manager
  -> SQLGenCtx
  -> RebuildableSchemaCache
  -> Metadata
  -> RQLMetadata
  -> m (EncJSON, MetadataStateResult)
runMetadataRequest env userInfo httpManager sqlGenCtx schemaCache metadata request = do
  ((r, modSchemaCache, cacheInvalidations), modMetadata) <-
    runMetadataRequestM env request
    & runCacheRWT schemaCache
    & peelMetadataRun (RunCtx userInfo httpManager sqlGenCtx) metadata
    & runExceptT
    & liftEitherM
  pure (r, MetadataStateResult modSchemaCache cacheInvalidations modMetadata)

runMetadataRequestM
  :: ( HasVersion
     , MonadIO m
     , CacheRWM m
     , UserInfoM m
     , MonadUnique m
     , MonadMetadata m
     , MonadScheduledEvents m
     , MonadCatalogState m
     , HasHttpManager m
     , MonadError QErr m
     )
  => Env.Environment
  -> RQLMetadata
  -> m EncJSON
runMetadataRequestM env = \case
  RMPgAddSource q    -> runAddPgSource q
  RMPgDropSource q   -> runDropPgSource q
  RMPgReloadSource q -> runReloadPgSource q

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

  RMBulk q -> encJFromList <$> indexedMapM (runMetadataRequestM env) q
