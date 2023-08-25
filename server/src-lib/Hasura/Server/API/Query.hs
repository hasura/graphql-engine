-- | The RQL query ('/v1/query')
module Hasura.Server.API.Query
  ( RQLQuery,
    queryModifiesSchemaCache,
    requiresAdmin,
    runQuery,
  )
where

import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson qualified as J
import Data.Aeson.Casing qualified as J (snakeCase)
import Data.Environment qualified as Env
import Data.Has (Has)
import Hasura.App.State
import Hasura.Backends.Postgres.DDL.RunSQL
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.Function.API qualified as Functions
import Hasura.GraphQL.Schema.Common (SchemaSampledFeatureFlags)
import Hasura.Logging qualified as L
import Hasura.Metadata.Class
import Hasura.Prelude
import Hasura.QueryTags
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
import Hasura.RQL.DDL.ScheduledTrigger
import Hasura.RQL.DDL.Schema
import Hasura.RQL.DDL.Schema.Cache.Config
import Hasura.RQL.DML.Count
import Hasura.RQL.DML.Delete
import Hasura.RQL.DML.Insert
import Hasura.RQL.DML.Select
import Hasura.RQL.DML.Types
import Hasura.RQL.DML.Update
import Hasura.RQL.Types.Allowlist
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.CustomTypes
import Hasura.RQL.Types.Endpoint
import Hasura.RQL.Types.Metadata
import Hasura.RQL.Types.Permission
import Hasura.RQL.Types.QueryCollection
import Hasura.RQL.Types.ScheduledTrigger
import Hasura.RQL.Types.SchemaCache
import Hasura.RQL.Types.SchemaCache.Build
import Hasura.RQL.Types.Source
import Hasura.RemoteSchema.MetadataAPI
import Hasura.Server.Types
import Hasura.Server.Utils
import Hasura.Services
import Hasura.Session
import Hasura.Tracing qualified as Tracing

data RQLQueryV1
  = RQAddExistingTableOrView !(TrackTable ('Postgres 'Vanilla))
  | RQTrackTable !(TrackTable ('Postgres 'Vanilla))
  | RQUntrackTable !(UntrackTable ('Postgres 'Vanilla))
  | RQSetTableIsEnum !(SetTableIsEnum ('Postgres 'Vanilla))
  | RQSetTableCustomization !(SetTableCustomization ('Postgres 'Vanilla))
  | RQTrackFunction !(Functions.TrackFunction ('Postgres 'Vanilla))
  | RQUntrackFunction !(Functions.UnTrackFunction ('Postgres 'Vanilla))
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
  | RQRenameQueryCollection !RenameCollection
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
  deriving stock (Generic)

data RQLQueryV2
  = RQV2TrackTable !(TrackTableV2 ('Postgres 'Vanilla))
  | RQV2SetTableCustomFields !SetTableCustomFields -- deprecated
  | RQV2TrackFunction !(Functions.TrackFunctionV2 ('Postgres 'Vanilla))
  | RQV2ReplaceMetadata !ReplaceMetadataV2
  deriving stock (Generic)

data RQLQuery
  = RQV1 !RQLQueryV1
  | RQV2 !RQLQueryV2

instance J.FromJSON RQLQuery where
  parseJSON = J.withObject "Object" $ \o -> do
    mVersion <- o J..:? "version"
    let version = fromMaybe VIVersion1 mVersion
        val = J.Object o
    case version of
      VIVersion1 -> RQV1 <$> J.parseJSON val
      VIVersion2 -> RQV2 <$> J.parseJSON val

instance J.FromJSON RQLQueryV1 where
  parseJSON =
    J.genericParseJSON
      J.defaultOptions
        { J.constructorTagModifier = J.snakeCase . drop 2,
          J.sumEncoding = J.TaggedObject "type" "args"
        }

instance J.FromJSON RQLQueryV2 where
  parseJSON =
    J.genericParseJSON
      J.defaultOptions
        { J.constructorTagModifier = J.snakeCase . drop 4,
          J.sumEncoding = J.TaggedObject "type" "args",
          J.tagSingleConstructors = True
        }

runQuery ::
  ( MonadIO m,
    MonadError QErr m,
    HasAppEnv m,
    HasCacheStaticConfig m,
    Tracing.MonadTrace m,
    MonadBaseControl IO m,
    MonadMetadataStorage m,
    MonadResolveSource m,
    MonadQueryTags m,
    MonadEventLogCleanup m,
    ProvidesHasuraServices m,
    MonadGetPolicies m,
    UserInfoM m
  ) =>
  AppContext ->
  RebuildableSchemaCache ->
  RQLQuery ->
  m (EncJSON, RebuildableSchemaCache)
runQuery appContext sc query = do
  AppEnv {..} <- askAppEnv
  let logger = _lsLogger appEnvLoggers
  when ((appEnvEnableReadOnlyMode == ReadOnlyModeEnabled) && queryModifiesUserDB query)
    $ throw400 NotSupported "Cannot run write queries when read-only mode is enabled"

  let exportsMetadata = \case
        RQV1 (RQExportMetadata _) -> True
        _ -> False
      metadataDefaults =
        if (exportsMetadata query)
          then emptyMetadataDefaults
          else acMetadataDefaults appContext
  let dynamicConfig = buildCacheDynamicConfig appContext

  MetadataWithResourceVersion metadata currentResourceVersion <- liftEitherM fetchMetadata
  ((result, updatedMetadata), modSchemaCache, invalidations, sourcesIntrospection, schemaRegistryAction) <-
    runQueryM (acEnvironment appContext) (acSchemaSampledFeatureFlags appContext) (acSQLGenCtx appContext) query
      -- TODO: remove this straight runReaderT that provides no actual new info
      & flip runReaderT logger
      & runMetadataT metadata metadataDefaults
      & runCacheRWT dynamicConfig sc
  if queryModifiesSchemaCache query
    then case appEnvEnableMaintenanceMode of
      MaintenanceModeDisabled -> do
        -- set modified metadata in storage
        newResourceVersion <- liftEitherM $ setMetadata currentResourceVersion updatedMetadata

        (_, modSchemaCache', _, _, _) <-
          Tracing.newSpan "setMetadataResourceVersionInSchemaCache"
            $ setMetadataResourceVersionInSchemaCache newResourceVersion
            & runCacheRWT dynamicConfig modSchemaCache

        -- save sources introspection to stored-introspection DB
        saveSourcesIntrospection logger sourcesIntrospection newResourceVersion
        -- run schema registry action
        for_ schemaRegistryAction $ \action -> do
          liftIO $ action newResourceVersion (scInconsistentObjs (lastBuiltSchemaCache modSchemaCache')) updatedMetadata
        -- notify schema cache sync
        liftEitherM $ notifySchemaCacheSync newResourceVersion appEnvInstanceId invalidations

        pure (result, modSchemaCache')
      MaintenanceModeEnabled () ->
        throw500 "metadata cannot be modified in maintenance mode"
    else pure (result, modSchemaCache)

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
  RQRenameQueryCollection _ -> True
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
  RQRenameQueryCollection _ -> False
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
    Tracing.MonadTrace m,
    MetadataM m,
    MonadMetadataStorage m,
    MonadQueryTags m,
    MonadReader r m,
    MonadError QErr m,
    Has (L.Logger L.Hasura) r,
    MonadEventLogCleanup m,
    ProvidesHasuraServices m,
    MonadGetPolicies m
  ) =>
  Env.Environment ->
  SchemaSampledFeatureFlags ->
  SQLGenCtx ->
  RQLQuery ->
  m EncJSON
runQueryM env schemaSampledFeatureFlags sqlGen rq = withPathK "args" $ case rq of
  RQV1 q -> runQueryV1M q
  RQV2 q -> runQueryV2M q
  where
    runQueryV1M = \case
      RQAddExistingTableOrView q -> runTrackTableQ q
      RQTrackTable q -> runTrackTableQ q
      RQUntrackTable q -> runUntrackTableQ q
      RQSetTableIsEnum q -> runSetExistingTableIsEnumQ q
      RQSetTableCustomization q -> runSetTableCustomization q
      RQTrackFunction q -> Functions.runTrackFunc q
      RQUntrackFunction q -> Functions.runUntrackFunc q
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
      RQInsert q -> runInsert sqlGen q
      RQSelect q -> runSelect sqlGen q
      RQUpdate q -> runUpdate sqlGen q
      RQDelete q -> runDelete sqlGen q
      RQCount q -> runCount q
      RQAddRemoteSchema q -> runAddRemoteSchema env schemaSampledFeatureFlags q
      RQUpdateRemoteSchema q -> runUpdateRemoteSchema env schemaSampledFeatureFlags q
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
      RQRenameQueryCollection q -> runRenameCollection q
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
      RQRunSql q -> runRunSQL @'Vanilla sqlGen q
      RQSetCustomTypes q -> runSetCustomTypes q
      RQBulk qs -> encJFromList <$> indexedMapM (runQueryM env schemaSampledFeatureFlags sqlGen) qs

    runQueryV2M = \case
      RQV2TrackTable q -> runTrackTableV2Q q
      RQV2SetTableCustomFields q -> runSetTableCustomFieldsQV2 q
      RQV2TrackFunction q -> Functions.runTrackFunctionV2 q
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
    RQRenameQueryCollection _ -> True
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
