{-# LANGUAGE TemplateHaskell #-}

-- | The RQL query ('/v1/query')
module Hasura.Server.API.Query
  ( RQLQuery,
    queryModifiesSchemaCache,
    requiresAdmin,
    runQuery,
  )
where

import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson
import Data.Aeson.Casing
import Data.Aeson.TH
import Data.Environment qualified as Env
import Data.Has (Has)
import Hasura.Backends.Postgres.DDL.RunSQL
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.GraphQL.Execute.Backend
import Hasura.Logging qualified as L
import Hasura.Metadata.Class
import Hasura.Prelude
import Hasura.RQL.DDL.Action
import Hasura.RQL.DDL.ComputedField
import Hasura.RQL.DDL.CustomTypes
import Hasura.RQL.DDL.Endpoint
import Hasura.RQL.DDL.EventTrigger
import Hasura.RQL.DDL.Metadata
import Hasura.RQL.DDL.Permission
import Hasura.RQL.DDL.QueryCollection
import Hasura.RQL.DDL.Relationship
import Hasura.RQL.DDL.Relationship.Rename
import Hasura.RQL.DDL.RemoteRelationship
import Hasura.RQL.DDL.RemoteSchema
import Hasura.RQL.DDL.ScheduledTrigger
import Hasura.RQL.DDL.Schema
import Hasura.RQL.DML.Count
import Hasura.RQL.DML.Delete
import Hasura.RQL.DML.Insert
import Hasura.RQL.DML.Select
import Hasura.RQL.DML.Types
import Hasura.RQL.DML.Update
import Hasura.RQL.Types
import Hasura.RQL.Types.Endpoint
import Hasura.RQL.Types.Run
import Hasura.Server.Types
import Hasura.Server.Utils
import Hasura.Session
import Hasura.Tracing qualified as Tracing
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.Manager (HasHttpManagerM (..))

data RQLQueryV1
  = RQAddExistingTableOrView !(TrackTable ('Postgres 'Vanilla))
  | RQTrackTable !(TrackTable ('Postgres 'Vanilla))
  | RQUntrackTable !(UntrackTable ('Postgres 'Vanilla))
  | RQSetTableIsEnum !SetTableIsEnum
  | RQSetTableCustomization !(SetTableCustomization ('Postgres 'Vanilla))
  | RQTrackFunction !(TrackFunction ('Postgres 'Vanilla))
  | RQUntrackFunction !(UnTrackFunction ('Postgres 'Vanilla))
  | RQCreateObjectRelationship !(CreateObjRel ('Postgres 'Vanilla))
  | RQCreateArrayRelationship !(CreateArrRel ('Postgres 'Vanilla))
  | RQDropRelationship !(DropRel ('Postgres 'Vanilla))
  | RQSetRelationshipComment !(SetRelComment ('Postgres 'Vanilla))
  | RQRenameRelationship !(RenameRel ('Postgres 'Vanilla))
  | -- computed fields related
    RQAddComputedField !(AddComputedField ('Postgres 'Vanilla))
  | RQDropComputedField !(DropComputedField ('Postgres 'Vanilla))
  | RQCreateRemoteRelationship !(CreateFromSourceRelationship ('Postgres 'Vanilla))
  | RQUpdateRemoteRelationship !(CreateFromSourceRelationship ('Postgres 'Vanilla))
  | RQDeleteRemoteRelationship !(DeleteFromSourceRelationship ('Postgres 'Vanilla))
  | RQCreateInsertPermission !(CreatePerm InsPerm ('Postgres 'Vanilla))
  | RQCreateSelectPermission !(CreatePerm SelPerm ('Postgres 'Vanilla))
  | RQCreateUpdatePermission !(CreatePerm UpdPerm ('Postgres 'Vanilla))
  | RQCreateDeletePermission !(CreatePerm DelPerm ('Postgres 'Vanilla))
  | RQDropInsertPermission !(DropPerm ('Postgres 'Vanilla))
  | RQDropSelectPermission !(DropPerm ('Postgres 'Vanilla))
  | RQDropUpdatePermission !(DropPerm ('Postgres 'Vanilla))
  | RQDropDeletePermission !(DropPerm ('Postgres 'Vanilla))
  | RQSetPermissionComment !(SetPermComment ('Postgres 'Vanilla))
  | RQGetInconsistentMetadata !GetInconsistentMetadata
  | RQDropInconsistentMetadata !DropInconsistentMetadata
  | RQInsert !InsertQuery
  | RQSelect !SelectQuery
  | RQUpdate !UpdateQuery
  | RQDelete !DeleteQuery
  | RQCount !CountQuery
  | RQBulk ![RQLQuery]
  | -- schema-stitching, custom resolver related
    RQAddRemoteSchema !AddRemoteSchemaQuery
  | RQUpdateRemoteSchema !AddRemoteSchemaQuery
  | RQRemoveRemoteSchema !RemoteSchemaNameQuery
  | RQReloadRemoteSchema !RemoteSchemaNameQuery
  | RQIntrospectRemoteSchema !RemoteSchemaNameQuery
  | RQCreateEventTrigger !(CreateEventTriggerQuery ('Postgres 'Vanilla))
  | RQDeleteEventTrigger !(DeleteEventTriggerQuery ('Postgres 'Vanilla))
  | RQRedeliverEvent !(RedeliverEventQuery ('Postgres 'Vanilla))
  | RQInvokeEventTrigger !(InvokeEventTriggerQuery ('Postgres 'Vanilla))
  | -- scheduled triggers
    RQCreateCronTrigger !CreateCronTrigger
  | RQDeleteCronTrigger !ScheduledTriggerName
  | RQCreateScheduledEvent !CreateScheduledEvent
  | -- query collections, allow list related
    RQCreateQueryCollection !CreateCollection
  | RQDropQueryCollection !DropCollection
  | RQAddQueryToCollection !AddQueryToCollection
  | RQDropQueryFromCollection !DropQueryFromCollection
  | RQAddCollectionToAllowlist !AllowlistEntry
  | RQDropCollectionFromAllowlist !DropCollectionFromAllowlist
  | RQRunSql !RunSQL
  | RQReplaceMetadata !ReplaceMetadata
  | RQExportMetadata !ExportMetadata
  | RQClearMetadata !ClearMetadata
  | RQReloadMetadata !ReloadMetadata
  | RQCreateAction !CreateAction
  | RQDropAction !DropAction
  | RQUpdateAction !UpdateAction
  | RQCreateActionPermission !CreateActionPermission
  | RQDropActionPermission !DropActionPermission
  | RQCreateRestEndpoint !CreateEndpoint
  | RQDropRestEndpoint !DropEndpoint
  | RQDumpInternalState !DumpInternalState
  | RQSetCustomTypes !CustomTypes

data RQLQueryV2
  = RQV2TrackTable !(TrackTableV2 ('Postgres 'Vanilla))
  | RQV2SetTableCustomFields !SetTableCustomFields -- deprecated
  | RQV2TrackFunction !(TrackFunctionV2 ('Postgres 'Vanilla))
  | RQV2ReplaceMetadata !ReplaceMetadataV2

data RQLQuery
  = RQV1 !RQLQueryV1
  | RQV2 !RQLQueryV2

instance FromJSON RQLQuery where
  parseJSON = withObject "Object" $ \o -> do
    mVersion <- o .:? "version"
    let version = fromMaybe VIVersion1 mVersion
        val = Object o
    case version of
      VIVersion1 -> RQV1 <$> parseJSON val
      VIVersion2 -> RQV2 <$> parseJSON val

$( deriveFromJSON
     defaultOptions
       { constructorTagModifier = snakeCase . drop 2,
         sumEncoding = TaggedObject "type" "args"
       }
     ''RQLQueryV1
 )

$( deriveFromJSON
     defaultOptions
       { constructorTagModifier = snakeCase . drop 4,
         sumEncoding = TaggedObject "type" "args",
         tagSingleConstructors = True
       }
     ''RQLQueryV2
 )

runQuery ::
  ( MonadIO m,
    Tracing.MonadTrace m,
    MonadBaseControl IO m,
    MonadMetadataStorage m,
    MonadResolveSource m,
    MonadQueryTags m
  ) =>
  Env.Environment ->
  L.Logger L.Hasura ->
  InstanceId ->
  UserInfo ->
  RebuildableSchemaCache ->
  HTTP.Manager ->
  ServerConfigCtx ->
  RQLQuery ->
  m (EncJSON, RebuildableSchemaCache)
runQuery env logger instanceId userInfo sc hMgr serverConfigCtx query = do
  when ((_sccReadOnlyMode serverConfigCtx == ReadOnlyModeEnabled) && queryModifiesUserDB query) $
    throw400 NotSupported "Cannot run write queries when read-only mode is enabled"

  (metadata, currentResourceVersion) <- fetchMetadata
  result <-
    runReaderT (runQueryM env query) logger & \x -> do
      ((js, meta), rsc, ci) <-
        x & runMetadataT metadata
          & runCacheRWT sc
          & peelRun runCtx
          & runExceptT
          & liftEitherM
      pure (js, rsc, ci, meta)
  withReload currentResourceVersion result
  where
    runCtx = RunCtx userInfo hMgr serverConfigCtx

    withReload currentResourceVersion (result, updatedCache, invalidations, updatedMetadata) = do
      when (queryModifiesSchemaCache query) $ do
        case (_sccMaintenanceMode serverConfigCtx) of
          MaintenanceModeDisabled -> do
            -- set modified metadata in storage
            newResourceVersion <- setMetadata currentResourceVersion updatedMetadata
            -- notify schema cache sync
            notifySchemaCacheSync newResourceVersion instanceId invalidations
          MaintenanceModeEnabled ->
            throw500 "metadata cannot be modified in maintenance mode"
      pure (result, updatedCache)

-- | A predicate that determines whether the given query might modify/rebuild the schema cache. If
-- so, it needs to acquire the global lock on the schema cache so that other queries do not modify
-- it concurrently.
--
-- Ideally, we would enforce this using the type system — queries for which this function returns
-- 'False' should not be allowed to modify the schema cache. But for now we just ensure consistency
-- by hand.
queryModifiesSchemaCache :: RQLQuery -> Bool
queryModifiesSchemaCache (RQV1 qi) = case qi of
  RQAddExistingTableOrView _ -> True
  RQTrackTable _ -> True
  RQUntrackTable _ -> True
  RQTrackFunction _ -> True
  RQUntrackFunction _ -> True
  RQSetTableIsEnum _ -> True
  RQCreateObjectRelationship _ -> True
  RQCreateArrayRelationship _ -> True
  RQDropRelationship _ -> True
  RQSetRelationshipComment _ -> False
  RQRenameRelationship _ -> True
  RQAddComputedField _ -> True
  RQDropComputedField _ -> True
  RQCreateRemoteRelationship _ -> True
  RQUpdateRemoteRelationship _ -> True
  RQDeleteRemoteRelationship _ -> True
  RQCreateInsertPermission _ -> True
  RQCreateSelectPermission _ -> True
  RQCreateUpdatePermission _ -> True
  RQCreateDeletePermission _ -> True
  RQDropInsertPermission _ -> True
  RQDropSelectPermission _ -> True
  RQDropUpdatePermission _ -> True
  RQDropDeletePermission _ -> True
  RQSetPermissionComment _ -> False
  RQGetInconsistentMetadata _ -> False
  RQDropInconsistentMetadata _ -> True
  RQInsert _ -> False
  RQSelect _ -> False
  RQUpdate _ -> False
  RQDelete _ -> False
  RQCount _ -> False
  RQAddRemoteSchema _ -> True
  RQUpdateRemoteSchema _ -> True
  RQRemoveRemoteSchema _ -> True
  RQReloadRemoteSchema _ -> True
  RQIntrospectRemoteSchema _ -> False
  RQCreateEventTrigger _ -> True
  RQDeleteEventTrigger _ -> True
  RQRedeliverEvent _ -> False
  RQInvokeEventTrigger _ -> False
  RQCreateCronTrigger _ -> True
  RQDeleteCronTrigger _ -> True
  RQCreateScheduledEvent _ -> False
  RQCreateQueryCollection _ -> True
  RQDropQueryCollection _ -> True
  RQAddQueryToCollection _ -> True
  RQDropQueryFromCollection _ -> True
  RQAddCollectionToAllowlist _ -> True
  RQDropCollectionFromAllowlist _ -> True
  RQRunSql q -> isSchemaCacheBuildRequiredRunSQL q
  RQReplaceMetadata _ -> True
  RQExportMetadata _ -> False
  RQClearMetadata _ -> True
  RQReloadMetadata _ -> True
  RQCreateRestEndpoint _ -> True
  RQDropRestEndpoint _ -> True
  RQCreateAction _ -> True
  RQDropAction _ -> True
  RQUpdateAction _ -> True
  RQCreateActionPermission _ -> True
  RQDropActionPermission _ -> True
  RQDumpInternalState _ -> False
  RQSetCustomTypes _ -> True
  RQSetTableCustomization _ -> True
  RQBulk qs -> any queryModifiesSchemaCache qs
queryModifiesSchemaCache (RQV2 qi) = case qi of
  RQV2TrackTable _ -> True
  RQV2SetTableCustomFields _ -> True
  RQV2TrackFunction _ -> True
  RQV2ReplaceMetadata _ -> True

-- | A predicate that determines whether the given query might modify user's Database. If
-- so, when the server is run in safe mode, we should not proceed with those operations.
queryModifiesUserDB :: RQLQuery -> Bool
queryModifiesUserDB (RQV1 qi) = case qi of
  RQAddExistingTableOrView _ -> False
  RQTrackTable _ -> False
  RQUntrackTable _ -> False
  RQTrackFunction _ -> False
  RQUntrackFunction _ -> False
  RQSetTableIsEnum _ -> False
  RQCreateObjectRelationship _ -> False
  RQCreateArrayRelationship _ -> False
  RQDropRelationship _ -> False
  RQSetRelationshipComment _ -> False
  RQRenameRelationship _ -> False
  RQAddComputedField _ -> False
  RQDropComputedField _ -> False
  RQCreateRemoteRelationship _ -> False
  RQUpdateRemoteRelationship _ -> False
  RQDeleteRemoteRelationship _ -> False
  RQCreateInsertPermission _ -> False
  RQCreateSelectPermission _ -> False
  RQCreateUpdatePermission _ -> False
  RQCreateDeletePermission _ -> False
  RQDropInsertPermission _ -> False
  RQDropSelectPermission _ -> False
  RQDropUpdatePermission _ -> False
  RQDropDeletePermission _ -> False
  RQSetPermissionComment _ -> False
  RQGetInconsistentMetadata _ -> False
  RQDropInconsistentMetadata _ -> False
  RQInsert _ -> True
  RQSelect _ -> False
  RQUpdate _ -> True
  RQDelete _ -> True
  RQCount _ -> False
  RQAddRemoteSchema _ -> False
  RQUpdateRemoteSchema _ -> False
  RQRemoveRemoteSchema _ -> False
  RQReloadRemoteSchema _ -> False
  RQIntrospectRemoteSchema _ -> False
  RQCreateEventTrigger _ -> True
  RQDeleteEventTrigger _ -> True
  RQRedeliverEvent _ -> False
  RQInvokeEventTrigger _ -> False
  RQCreateCronTrigger _ -> False
  RQDeleteCronTrigger _ -> False
  RQCreateScheduledEvent _ -> False
  RQCreateQueryCollection _ -> False
  RQDropQueryCollection _ -> False
  RQAddQueryToCollection _ -> False
  RQDropQueryFromCollection _ -> False
  RQAddCollectionToAllowlist _ -> False
  RQDropCollectionFromAllowlist _ -> False
  RQRunSql _ -> True
  RQReplaceMetadata _ -> True
  RQExportMetadata _ -> False
  RQClearMetadata _ -> False
  RQReloadMetadata _ -> False
  RQCreateRestEndpoint _ -> False
  RQDropRestEndpoint _ -> False
  RQCreateAction _ -> False
  RQDropAction _ -> False
  RQUpdateAction _ -> False
  RQCreateActionPermission _ -> False
  RQDropActionPermission _ -> False
  RQDumpInternalState _ -> False
  RQSetCustomTypes _ -> False
  RQSetTableCustomization _ -> False
  RQBulk qs -> any queryModifiesUserDB qs
queryModifiesUserDB (RQV2 qi) = case qi of
  RQV2TrackTable _ -> False
  RQV2SetTableCustomFields _ -> False
  RQV2TrackFunction _ -> False
  RQV2ReplaceMetadata _ -> True

runQueryM ::
  ( CacheRWM m,
    UserInfoM m,
    MonadBaseControl IO m,
    MonadIO m,
    HasHttpManagerM m,
    HasServerConfigCtx m,
    Tracing.MonadTrace m,
    MetadataM m,
    MonadMetadataStorageQueryAPI m,
    MonadQueryTags m,
    MonadReader r m,
    Has (L.Logger L.Hasura) r
  ) =>
  Env.Environment ->
  RQLQuery ->
  m EncJSON
runQueryM env rq = withPathK "args" $ case rq of
  RQV1 q -> runQueryV1M q
  RQV2 q -> runQueryV2M q
  where
    runQueryV1M = \case
      RQAddExistingTableOrView q -> runTrackTableQ q
      RQTrackTable q -> runTrackTableQ q
      RQUntrackTable q -> runUntrackTableQ q
      RQSetTableIsEnum q -> runSetExistingTableIsEnumQ q
      RQSetTableCustomization q -> runSetTableCustomization q
      RQTrackFunction q -> runTrackFunc q
      RQUntrackFunction q -> runUntrackFunc q
      RQCreateObjectRelationship q -> runCreateRelationship ObjRel $ unCreateObjRel q
      RQCreateArrayRelationship q -> runCreateRelationship ArrRel $ unCreateArrRel q
      RQDropRelationship q -> runDropRel q
      RQSetRelationshipComment q -> runSetRelComment q
      RQRenameRelationship q -> runRenameRel q
      RQAddComputedField q -> runAddComputedField q
      RQDropComputedField q -> runDropComputedField q
      RQCreateInsertPermission q -> runCreatePerm q
      RQCreateSelectPermission q -> runCreatePerm q
      RQCreateUpdatePermission q -> runCreatePerm q
      RQCreateDeletePermission q -> runCreatePerm q
      RQDropInsertPermission q -> runDropPerm PTInsert q
      RQDropSelectPermission q -> runDropPerm PTSelect q
      RQDropUpdatePermission q -> runDropPerm PTUpdate q
      RQDropDeletePermission q -> runDropPerm PTDelete q
      RQSetPermissionComment q -> runSetPermComment q
      RQGetInconsistentMetadata q -> runGetInconsistentMetadata q
      RQDropInconsistentMetadata q -> runDropInconsistentMetadata q
      RQInsert q -> runInsert q
      RQSelect q -> runSelect q
      RQUpdate q -> runUpdate q
      RQDelete q -> runDelete q
      RQCount q -> runCount q
      RQAddRemoteSchema q -> runAddRemoteSchema env q
      RQUpdateRemoteSchema q -> runUpdateRemoteSchema env q
      RQRemoveRemoteSchema q -> runRemoveRemoteSchema q
      RQReloadRemoteSchema q -> runReloadRemoteSchema q
      RQIntrospectRemoteSchema q -> runIntrospectRemoteSchema q
      RQCreateRemoteRelationship q -> runCreateRemoteRelationship q
      RQUpdateRemoteRelationship q -> runUpdateRemoteRelationship q
      RQDeleteRemoteRelationship q -> runDeleteRemoteRelationship q
      RQCreateEventTrigger q -> runCreateEventTriggerQuery q
      RQDeleteEventTrigger q -> runDeleteEventTriggerQuery q
      RQRedeliverEvent q -> runRedeliverEvent q
      RQInvokeEventTrigger q -> runInvokeEventTrigger q
      RQCreateCronTrigger q -> runCreateCronTrigger q
      RQDeleteCronTrigger q -> runDeleteCronTrigger q
      RQCreateScheduledEvent q -> runCreateScheduledEvent q
      RQCreateQueryCollection q -> runCreateCollection q
      RQDropQueryCollection q -> runDropCollection q
      RQAddQueryToCollection q -> runAddQueryToCollection q
      RQDropQueryFromCollection q -> runDropQueryFromCollection q
      RQAddCollectionToAllowlist q -> runAddCollectionToAllowlist q
      RQDropCollectionFromAllowlist q -> runDropCollectionFromAllowlist q
      RQReplaceMetadata q -> runReplaceMetadata q
      RQClearMetadata q -> runClearMetadata q
      RQExportMetadata q -> runExportMetadata q
      RQReloadMetadata q -> runReloadMetadata q
      RQCreateAction q -> runCreateAction q
      RQDropAction q -> runDropAction q
      RQUpdateAction q -> runUpdateAction q
      RQCreateActionPermission q -> runCreateActionPermission q
      RQDropActionPermission q -> runDropActionPermission q
      RQCreateRestEndpoint q -> runCreateEndpoint q
      RQDropRestEndpoint q -> runDropEndpoint q
      RQDumpInternalState q -> runDumpInternalState q
      RQRunSql q -> runRunSQL @'Vanilla q
      RQSetCustomTypes q -> runSetCustomTypes q
      RQBulk qs -> encJFromList <$> indexedMapM (runQueryM env) qs

    runQueryV2M = \case
      RQV2TrackTable q -> runTrackTableV2Q q
      RQV2SetTableCustomFields q -> runSetTableCustomFieldsQV2 q
      RQV2TrackFunction q -> runTrackFunctionV2 q
      RQV2ReplaceMetadata q -> runReplaceMetadataV2 q

requiresAdmin :: RQLQuery -> Bool
requiresAdmin = \case
  RQV1 q -> case q of
    RQAddExistingTableOrView _ -> True
    RQTrackTable _ -> True
    RQUntrackTable _ -> True
    RQSetTableIsEnum _ -> True
    RQSetTableCustomization _ -> True
    RQTrackFunction _ -> True
    RQUntrackFunction _ -> True
    RQCreateObjectRelationship _ -> True
    RQCreateArrayRelationship _ -> True
    RQDropRelationship _ -> True
    RQSetRelationshipComment _ -> True
    RQRenameRelationship _ -> True
    RQAddComputedField _ -> True
    RQDropComputedField _ -> True
    RQCreateRemoteRelationship _ -> True
    RQUpdateRemoteRelationship _ -> True
    RQDeleteRemoteRelationship _ -> True
    RQCreateInsertPermission _ -> True
    RQCreateSelectPermission _ -> True
    RQCreateUpdatePermission _ -> True
    RQCreateDeletePermission _ -> True
    RQDropInsertPermission _ -> True
    RQDropSelectPermission _ -> True
    RQDropUpdatePermission _ -> True
    RQDropDeletePermission _ -> True
    RQSetPermissionComment _ -> True
    RQGetInconsistentMetadata _ -> True
    RQDropInconsistentMetadata _ -> True
    RQInsert _ -> False
    RQSelect _ -> False
    RQUpdate _ -> False
    RQDelete _ -> False
    RQCount _ -> False
    RQAddRemoteSchema _ -> True
    RQUpdateRemoteSchema _ -> True
    RQRemoveRemoteSchema _ -> True
    RQReloadRemoteSchema _ -> True
    RQIntrospectRemoteSchema _ -> True
    RQCreateEventTrigger _ -> True
    RQDeleteEventTrigger _ -> True
    RQRedeliverEvent _ -> True
    RQInvokeEventTrigger _ -> True
    RQCreateCronTrigger _ -> True
    RQDeleteCronTrigger _ -> True
    RQCreateScheduledEvent _ -> True
    RQCreateQueryCollection _ -> True
    RQDropQueryCollection _ -> True
    RQAddQueryToCollection _ -> True
    RQDropQueryFromCollection _ -> True
    RQAddCollectionToAllowlist _ -> True
    RQDropCollectionFromAllowlist _ -> True
    RQReplaceMetadata _ -> True
    RQClearMetadata _ -> True
    RQExportMetadata _ -> True
    RQReloadMetadata _ -> True
    RQCreateRestEndpoint _ -> True
    RQDropRestEndpoint _ -> True
    RQCreateAction _ -> True
    RQDropAction _ -> True
    RQUpdateAction _ -> True
    RQCreateActionPermission _ -> True
    RQDropActionPermission _ -> True
    RQDumpInternalState _ -> True
    RQSetCustomTypes _ -> True
    RQRunSql _ -> True
    RQBulk qs -> any requiresAdmin qs
  RQV2 q -> case q of
    RQV2TrackTable _ -> True
    RQV2SetTableCustomFields _ -> True
    RQV2TrackFunction _ -> True
    RQV2ReplaceMetadata _ -> True
