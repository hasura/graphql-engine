{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Top-level functions concerned specifically with operations on the schema cache, such as
-- rebuilding it from the catalog and incorporating schema changes. See the module documentation for
-- "Hasura.RQL.DDL.Schema" for more details.
--
-- __Note__: this module is __mutually recursive__ with other @Hasura.RQL.DDL.Schema.*@ modules, which
-- both define pieces of the implementation of building the schema cache and define handlers that
-- trigger schema cache rebuilds.
module Hasura.RQL.DDL.Schema.Cache
  ( RebuildableSchemaCache,
    lastBuiltSchemaCache,
    buildRebuildableSchemaCache,
    buildRebuildableSchemaCacheWithReason,
    CacheRWT,
    runCacheRWT,
    mkBooleanPermissionMap,
  )
where

import Control.Arrow.Extended
import Control.Lens hiding ((.=))
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Retry qualified as Retry
import Data.Aeson
import Data.Either (isLeft)
import Data.Environment qualified as Env
import Data.HashMap.Strict.Extended qualified as M
import Data.HashMap.Strict.InsOrd.Extended qualified as OMap
import Data.HashSet qualified as HS
import Data.Proxy
import Data.Set qualified as S
import Data.Text.Extended
import Hasura.Base.Error
import Hasura.GraphQL.Schema (buildGQLContext)
import Hasura.GraphQL.Schema.NamingCase
import Hasura.Incremental qualified as Inc
import Hasura.Logging
import Hasura.Metadata.Class
import Hasura.Prelude
import Hasura.RQL.DDL.Action
import Hasura.RQL.DDL.CustomTypes
import Hasura.RQL.DDL.EventTrigger (MonadEventLogCleanup (..), buildEventTriggerInfo)
import Hasura.RQL.DDL.InheritedRoles (resolveInheritedRole)
import Hasura.RQL.DDL.RemoteRelationship (CreateRemoteSchemaRemoteRelationship (..), PartiallyResolvedSource (..), buildRemoteFieldInfo, getRemoteSchemaEntityJoinColumns)
import Hasura.RQL.DDL.ScheduledTrigger
import Hasura.RQL.DDL.Schema.Cache.Common
import Hasura.RQL.DDL.Schema.Cache.Dependencies
import Hasura.RQL.DDL.Schema.Cache.Fields
import Hasura.RQL.DDL.Schema.Cache.Permission
import Hasura.RQL.DDL.Schema.Function
import Hasura.RQL.DDL.Schema.Table
import Hasura.RQL.Types.Action
import Hasura.RQL.Types.Allowlist
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.CustomTypes
import Hasura.RQL.Types.Endpoint
import Hasura.RQL.Types.EventTrigger
import Hasura.RQL.Types.Eventing.Backend
import Hasura.RQL.Types.Function
import Hasura.RQL.Types.Metadata hiding (fmFunction, tmTable)
import Hasura.RQL.Types.Metadata.Backend
import Hasura.RQL.Types.Metadata.Object
import Hasura.RQL.Types.Network
import Hasura.RQL.Types.OpenTelemetry
import Hasura.RQL.Types.QueryCollection
import Hasura.RQL.Types.Relationships.Remote
import Hasura.RQL.Types.Roles
import Hasura.RQL.Types.ScheduledTrigger
import Hasura.RQL.Types.SchemaCache
import Hasura.RQL.Types.SchemaCache.Build
import Hasura.RQL.Types.SchemaCache.Instances ()
import Hasura.RQL.Types.SchemaCacheTypes
import Hasura.RQL.Types.Source
import Hasura.RQL.Types.SourceCustomization
import Hasura.RQL.Types.Table
import Hasura.RemoteSchema.Metadata
import Hasura.RemoteSchema.SchemaCache
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.SQL.Backend
import Hasura.SQL.BackendMap (BackendMap)
import Hasura.SQL.BackendMap qualified as BackendMap
import Hasura.SQL.Tag
import Hasura.Server.Migrate.Version
import Hasura.Server.Types
import Hasura.Session
import Hasura.Tracing qualified as Tracing
import Language.GraphQL.Draft.Syntax qualified as G
import Network.HTTP.Client.Manager (HasHttpManagerM (..))

{- Note [Roles Inheritance]
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Roles may have parent roles defined from which they can inherit permission and this is
called as roles inheritance. Roles which have parents can also be parents of other roles.
So, cycle in roles should be disallowed and this is done in the `orderRoles` function.

When the metadata contains a permission for a role for a entity, then it will override the
inherited permission, if any.

Roles inheritance work differently for different features:

1. Select permissions
~~~~~~~~~~~~~~~~~~~~~

See note [Inherited roles architecture for read queries]

2. Mutation permissions and remote schema permissions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For mutation and remote schema permissions, an inherited role can only inherit permission
from its parent roles when the relevant parts of the permissions are equal i.e. the non-relevant
parts are discarded for the equality, for example, in two remote schema permissions the order
of the fields in an Object type is discarded.

When an inherited role cannot inherit permission from its parents due to a conflict, then we mark
the inherited role and the entity (remote schema or table) combination as inconsistent in the metadata.

3. Actions and Custom function permissions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Currently, actions and custom function permissions can be thought of as a boolean. Either a role has
permission to the entity or it doesn't, so in these cases there's no possiblity of a conflict. An inherited
role will have access to the action/function if any one of the parents have permission to access the
action/function.

-}

buildRebuildableSchemaCache ::
  Logger Hasura ->
  Env.Environment ->
  Metadata ->
  CacheBuild RebuildableSchemaCache
buildRebuildableSchemaCache =
  buildRebuildableSchemaCacheWithReason CatalogSync

buildRebuildableSchemaCacheWithReason ::
  BuildReason ->
  Logger Hasura ->
  Env.Environment ->
  Metadata ->
  CacheBuild RebuildableSchemaCache
buildRebuildableSchemaCacheWithReason reason logger env metadata = do
  result <-
    flip runReaderT reason $
      Inc.build (buildSchemaCacheRule logger env) (metadata, initialInvalidationKeys)

  pure $ RebuildableSchemaCache (Inc.result result) initialInvalidationKeys (Inc.rebuildRule result)

newtype CacheRWT m a
  = -- The CacheInvalidations component of the state could actually be collected using WriterT, but
    -- WriterT implementations prior to transformers-0.5.6.0 (which added
    -- Control.Monad.Trans.Writer.CPS) are leaky, and we don’t have that yet.
    CacheRWT (StateT (RebuildableSchemaCache, CacheInvalidations) m a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader r,
      MonadError e,
      UserInfoM,
      HasHttpManagerM,
      MonadMetadataStorage,
      MonadMetadataStorageQueryAPI,
      Tracing.MonadTrace,
      HasServerConfigCtx,
      MonadBase b,
      MonadBaseControl b
    )

instance (MonadEventLogCleanup m) => MonadEventLogCleanup (CacheRWT m) where
  runLogCleaner conf = lift $ runLogCleaner conf
  generateCleanupSchedules sourceInfo triggerName cleanupConfig = lift $ generateCleanupSchedules sourceInfo triggerName cleanupConfig

runCacheRWT ::
  Functor m =>
  RebuildableSchemaCache ->
  CacheRWT m a ->
  m (a, RebuildableSchemaCache, CacheInvalidations)
runCacheRWT cache (CacheRWT m) =
  runStateT m (cache, mempty) <&> \(v, (newCache, invalidations)) -> (v, newCache, invalidations)

instance MonadTrans CacheRWT where
  lift = CacheRWT . lift

instance (Monad m) => CacheRM (CacheRWT m) where
  askSchemaCache = CacheRWT $ gets (lastBuiltSchemaCache . (^. _1))

instance
  ( MonadIO m,
    MonadError QErr m,
    HasHttpManagerM m,
    MonadResolveSource m,
    HasServerConfigCtx m
  ) =>
  CacheRWM (CacheRWT m)
  where
  buildSchemaCacheWithOptions buildReason invalidations metadata = CacheRWT do
    (RebuildableSchemaCache lastBuiltSC invalidationKeys rule, oldInvalidations) <- get
    let metadataVersion = scMetadataResourceVersion lastBuiltSC
        newInvalidationKeys = invalidateKeys invalidations invalidationKeys
    result <-
      lift $
        runCacheBuildM $
          flip runReaderT buildReason $
            Inc.build rule (metadata, newInvalidationKeys)
    let schemaCache = (Inc.result result) {scMetadataResourceVersion = metadataVersion}
        prunedInvalidationKeys = pruneInvalidationKeys schemaCache newInvalidationKeys
        !newCache = RebuildableSchemaCache schemaCache prunedInvalidationKeys (Inc.rebuildRule result)
        !newInvalidations = oldInvalidations <> invalidations
    put (newCache, newInvalidations)
    where
      -- Prunes invalidation keys that no longer exist in the schema to avoid leaking memory by
      -- hanging onto unnecessary keys.
      pruneInvalidationKeys schemaCache = over ikRemoteSchemas $ M.filterWithKey \name _ ->
        -- see Note [Keep invalidation keys for inconsistent objects]
        name `elem` getAllRemoteSchemas schemaCache

  setMetadataResourceVersionInSchemaCache resourceVersion = CacheRWT $ do
    (rebuildableSchemaCache, invalidations) <- get
    put
      ( rebuildableSchemaCache
          { lastBuiltSchemaCache =
              (lastBuiltSchemaCache rebuildableSchemaCache)
                { scMetadataResourceVersion = Just resourceVersion
                }
          },
        invalidations
      )

-- | Generate health checks related cache from sources metadata
buildHealthCheckCache :: Sources -> SourceHealthCheckCache
buildHealthCheckCache sources =
  catMaybes $ M.fromList $ map (second mkSourceHealthCheck) (OMap.toList sources)
  where
    mkSourceHealthCheck :: BackendSourceMetadata -> Maybe BackendSourceHealthCheckInfo
    mkSourceHealthCheck (BackendSourceMetadata sourceMetadata) =
      AB.traverseBackend @Backend sourceMetadata mkSourceHealthCheckBackend

    mkSourceHealthCheckBackend :: SourceMetadata b -> Maybe (SourceHealthCheckInfo b)
    mkSourceHealthCheckBackend sourceMetadata =
      let sourceName = _smName sourceMetadata
          connection = _smConfiguration sourceMetadata
          healthCheck = _smHealthCheckConfig sourceMetadata
       in SourceHealthCheckInfo sourceName connection <$> healthCheck

-- | Generate cache of source connection details so that we can ping sources for
-- attribution
buildSourcePingCache :: Sources -> SourcePingCache
buildSourcePingCache sources =
  M.fromList $ map (second mkSourcePing) (OMap.toList sources)
  where
    mkSourcePing :: BackendSourceMetadata -> BackendSourcePingInfo
    mkSourcePing (BackendSourceMetadata sourceMetadata) =
      AB.mapBackend sourceMetadata mkSourcePingBackend

    mkSourcePingBackend :: SourceMetadata b -> SourcePingInfo b
    mkSourcePingBackend sourceMetadata =
      let sourceName = _smName sourceMetadata
          connection = _smConfiguration sourceMetadata
       in SourcePingInfo sourceName connection

{- Note [Avoiding GraphQL schema rebuilds when changing irrelevant Metadata]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There are many Metadata operations that don't influence the GraphQL schema.  So
we should be caching its construction.

The `Hasura.Incremental` framework allows us to cache such constructions:
whenever we have an arrow `Rule m a b`, where `a` is the input to the arrow and
`b` the output, we can use the `Inc.cache` combinator to obtain a new arrow
which is only re-executed when the input `a` changes in a material way.  To test
this, `a` needs an `Eq` instance.

We can't simply apply `Inc.cache` to the GraphQL schema cache building phase
(`buildGQLContext`), because the inputs (components of `BuildOutputs` such as
`SourceCache`) don't have an `Eq` instance.

So the purpose of `buildOutputsAndSchema` is that we cach already at an earlier
point, encompassing more computation.  The Metadata and invalidation keys (which
have `Eq` instances) are used as a caching key, and `Inc.cache` can be applied
to the whole sequence of steps.

But because of the all-or-nothing nature of caching, it's important that
`buildOutputsAndSchema` is re-run as little as possible.  So the exercise
becomes to minimize the amount of stuff stored in `BuildOutputs`, so that as
many Metadata operations as possible can be handled outside of this codepath
that produces a GraphQL schema.
-}

buildSchemaCacheRule ::
  -- Note: by supplying BuildReason via MonadReader, it does not participate in caching, which is
  -- what we want!
  ( ArrowChoice arr,
    Inc.ArrowDistribute arr,
    Inc.ArrowCache m arr,
    MonadIO m,
    MonadBaseControl IO m,
    MonadError QErr m,
    MonadReader BuildReason m,
    HasHttpManagerM m,
    MonadResolveSource m,
    HasServerConfigCtx m
  ) =>
  Logger Hasura ->
  Env.Environment ->
  (Metadata, InvalidationKeys) `arr` SchemaCache
buildSchemaCacheRule logger env = proc (metadataNoDefaults, invalidationKeys) -> do
  invalidationKeysDep <- Inc.newDependency -< invalidationKeys
  metadataDefaults <- bindA -< askMetadataDefaults
  let metadata@Metadata {..} = overrideMetadataDefaults metadataNoDefaults metadataDefaults
  metadataDep <- Inc.newDependency -< metadata

  (inconsistentObjects, (resolvedOutputs, dependencyInconsistentObjects, resolvedDependencies), ((adminIntrospection, gqlContext, gqlContextUnauth, inconsistentRemoteSchemas), (relayContext, relayContextUnauth))) <-
    Inc.cache buildOutputsAndSchema -< (metadataDep, invalidationKeysDep)

  (resolvedEndpoints, endpointCollectedInfo) <- runWriterA $ buildInfoMap fst mkEndpointMetadataObject buildEndpoint -< (_metaQueryCollections, OMap.toList _metaRestEndpoints)
  (cronTriggersMap, cronTriggersCollectedInfo) <- runWriterA buildCronTriggers -< ((), OMap.elems _metaCronTriggers)
  (openTelemetryInfo, openTelemetryCollectedInfo) <- runWriterA buildOpenTelemetry -< _metaOpenTelemetryConfig

  let duplicateVariables :: EndpointMetadata a -> Bool
      duplicateVariables m = any ((> 1) . length) $ group $ sort $ catMaybes $ splitPath Just (const Nothing) (_ceUrl m)

      endpointObjId :: EndpointMetadata q -> MetadataObjId
      endpointObjId md = MOEndpoint (_ceName md)

      endpointObject :: EndpointMetadata q -> MetadataObject
      endpointObject md = MetadataObject (endpointObjId md) (toJSON $ OMap.lookup (_ceName md) _metaRestEndpoints)

      listedQueryObjects :: (CollectionName, ListedQuery) -> MetadataObject
      listedQueryObjects (cName, lq) = MetadataObject (MOQueryCollectionsQuery cName lq) (toJSON lq)

      --  Cases of urls that generate invalid segments:

      hasInvalidSegments :: EndpointMetadata query -> Bool
      hasInvalidSegments m = any (`elem` ["", ":"]) (splitPath id id (_ceUrl m))

      ceUrlTxt = toTxt . _ceUrl

      endpoints = buildEndpointsTrie (M.elems resolvedEndpoints)

      duplicateF md = DuplicateRestVariables (ceUrlTxt md) (endpointObject md)
      duplicateRestVariables = map duplicateF $ filter duplicateVariables (M.elems resolvedEndpoints)

      invalidF md = InvalidRestSegments (ceUrlTxt md) (endpointObject md)
      invalidRestSegments = map invalidF $ filter hasInvalidSegments (M.elems resolvedEndpoints)

      ambiguousF' ep = MetadataObject (endpointObjId ep) (toJSON ep)
      ambiguousF mds = AmbiguousRestEndpoints (commaSeparated $ map _ceUrl mds) (map ambiguousF' mds)
      ambiguousRestEndpoints = map (ambiguousF . S.elems . snd) $ ambiguousPathsGrouped endpoints

      inlinedAllowlist = inlineAllowlist _metaQueryCollections _metaAllowlist
      globalAllowLists = HS.toList . iaGlobal $ inlinedAllowlist

      -- Endpoints don't generate any dependencies
      endpointInconsistencies = either id absurd <$> toList endpointCollectedInfo

      -- Cron triggers don't generate any dependencies
      cronTriggersInconsistencies = either id absurd <$> toList cronTriggersCollectedInfo

      -- OpenTelemerty doesn't generate any dependencies
      openTelemetryInconsistencies = either id absurd <$> toList openTelemetryCollectedInfo

  inconsistentQueryCollections <- bindA -< do getInconsistentQueryCollections adminIntrospection _metaQueryCollections listedQueryObjects endpoints globalAllowLists

  returnA
    -<
      SchemaCache
        { scSources = _boSources resolvedOutputs,
          scActions = _boActions resolvedOutputs,
          -- TODO this is not the right value: we should track what part of the schema
          -- we can stitch without consistencies, I think.
          scRemoteSchemas = fmap fst (_boRemoteSchemas resolvedOutputs), -- remoteSchemaMap
          scAllowlist = inlinedAllowlist,
          -- , scCustomTypes = _boCustomTypes resolvedOutputs
          scAdminIntrospection = adminIntrospection,
          scGQLContext = gqlContext,
          scUnauthenticatedGQLContext = gqlContextUnauth,
          scRelayContext = relayContext,
          scUnauthenticatedRelayContext = relayContextUnauth,
          -- , scGCtxMap = gqlSchema
          -- , scDefaultRemoteGCtx = remoteGQLSchema
          scDepMap = resolvedDependencies,
          scCronTriggers = cronTriggersMap,
          scEndpoints = endpoints,
          scInconsistentObjs =
            inconsistentObjects
              <> dependencyInconsistentObjects
              <> toList inconsistentRemoteSchemas
              <> duplicateRestVariables
              <> invalidRestSegments
              <> ambiguousRestEndpoints
              <> endpointInconsistencies
              <> cronTriggersInconsistencies
              <> openTelemetryInconsistencies
              <> inconsistentQueryCollections,
          scApiLimits = _metaApiLimits,
          scMetricsConfig = _metaMetricsConfig,
          scMetadataResourceVersion = Nothing,
          scSetGraphqlIntrospectionOptions = _metaSetGraphqlIntrospectionOptions,
          scTlsAllowlist = networkTlsAllowlist _metaNetwork,
          scQueryCollections = _metaQueryCollections,
          scBackendCache = _boBackendCache resolvedOutputs,
          scSourceHealthChecks = buildHealthCheckCache _metaSources,
          scSourcePingConfig = buildSourcePingCache _metaSources,
          scOpenTelemetryConfig = openTelemetryInfo
        }
  where
    -- See Note [Avoiding GraphQL schema rebuilds when changing irrelevant Metadata]
    buildOutputsAndSchema = proc (metadataDep, invalidationKeysDep) -> do
      (outputs, collectedInfo) <- runWriterA buildAndCollectInfo -< (metadataDep, invalidationKeysDep)
      let (inconsistentObjects, unresolvedDependencies) = partitionEithers $ toList collectedInfo
      out2@(resolvedOutputs, _dependencyInconsistentObjects, _resolvedDependencies) <- resolveDependencies -< (outputs, unresolvedDependencies)
      out3 <-
        bindA
          -< do
            cxt <- askServerConfigCtx
            buildGQLContext
              cxt
              (_boSources resolvedOutputs)
              (_boRemoteSchemas resolvedOutputs)
              (_boActions resolvedOutputs)
              (_boCustomTypes resolvedOutputs)
      returnA -< (inconsistentObjects, out2, out3)

    resolveBackendInfo' ::
      forall arr m b.
      ( BackendMetadata b,
        ArrowChoice arr,
        Inc.ArrowCache m arr,
        Inc.ArrowDistribute arr,
        ArrowWriter (Seq (Either InconsistentMetadata MetadataDependency)) arr,
        MonadIO m,
        HasHttpManagerM m
      ) =>
      (BackendConfigWrapper b, Inc.Dependency (BackendMap BackendInvalidationKeysWrapper)) `arr` BackendCache
    resolveBackendInfo' = proc (backendConfigWrapper, backendInvalidationMap) -> do
      let backendInvalidationKeys =
            Inc.selectD #unBackendInvalidationKeysWrapper $
              Inc.selectMaybeD $
                BackendMap.lookupD @b backendInvalidationMap
      backendInfo <- resolveBackendInfo @b logger -< (backendInvalidationKeys, unBackendConfigWrapper backendConfigWrapper)
      returnA -< BackendMap.singleton (BackendInfoWrapper @b backendInfo)

    resolveBackendCache ::
      forall arr m.
      ( ArrowChoice arr,
        Inc.ArrowCache m arr,
        Inc.ArrowDistribute arr,
        ArrowWriter (Seq (Either InconsistentMetadata MetadataDependency)) arr,
        MonadIO m,
        HasHttpManagerM m
      ) =>
      (Inc.Dependency (BackendMap BackendInvalidationKeysWrapper), [AB.AnyBackend BackendConfigWrapper]) `arr` BackendCache
    resolveBackendCache = proc (backendInvalidationMap, backendConfigs) -> do
      case backendConfigs of
        [] -> returnA -< mempty
        (anyBackendConfig : backendConfigs') -> do
          backendInfo <-
            AB.dispatchAnyBackendArrow @BackendMetadata @HasTag resolveBackendInfo' -< (anyBackendConfig, backendInvalidationMap)
          backendInfos <- resolveBackendCache -< (backendInvalidationMap, backendConfigs')
          returnA -< backendInfo <> backendInfos

    getSourceConfigIfNeeded ::
      forall b arr m.
      ( ArrowChoice arr,
        Inc.ArrowCache m arr,
        ArrowWriter (Seq (Either InconsistentMetadata MetadataDependency)) arr,
        MonadIO m,
        MonadResolveSource m,
        HasHttpManagerM m,
        BackendMetadata b
      ) =>
      ( Inc.Dependency (HashMap SourceName Inc.InvalidationKey),
        SourceName,
        SourceConnConfiguration b,
        BackendSourceKind b,
        BackendInfo b
      )
        `arr` Maybe (SourceConfig b)
    getSourceConfigIfNeeded = Inc.cache proc (invalidationKeys, sourceName, sourceConfig, backendKind, backendInfo) -> do
      let metadataObj = MetadataObject (MOSource sourceName) $ toJSON sourceName
      httpMgr <- bindA -< askHttpManager
      Inc.dependOn -< Inc.selectKeyD sourceName invalidationKeys
      (|
        withRecordInconsistency
          ( liftEitherA <<< bindA -< resolveSourceConfig @b logger sourceName sourceConfig backendKind backendInfo env httpMgr
          )
        |) metadataObj

    resolveSourceIfNeeded ::
      forall b arr m.
      ( ArrowChoice arr,
        Inc.ArrowCache m arr,
        ArrowWriter (Seq (Either InconsistentMetadata MetadataDependency)) arr,
        MonadIO m,
        MonadBaseControl IO m,
        MonadResolveSource m,
        HasHttpManagerM m,
        BackendMetadata b
      ) =>
      ( Inc.Dependency (HashMap SourceName Inc.InvalidationKey),
        BackendInfoAndSourceMetadata b
      )
        `arr` Maybe (ResolvedSource b)
    resolveSourceIfNeeded = Inc.cache proc (invalidationKeys, BackendInfoAndSourceMetadata {..}) -> do
      let sourceName = _smName _bcasmSourceMetadata
          metadataObj = MetadataObject (MOSource sourceName) $ toJSON sourceName

      maybeSourceConfig <- getSourceConfigIfNeeded @b -< (invalidationKeys, sourceName, _smConfiguration _bcasmSourceMetadata, _smKind _bcasmSourceMetadata, _bcasmBackendInfo)
      case maybeSourceConfig of
        Nothing -> returnA -< Nothing
        Just sourceConfig ->
          (|
            withRecordInconsistency
              ( liftEitherA <<< bindA
                  -< do
                    resSource <- resolveDatabaseMetadata _bcasmSourceMetadata sourceConfig (getSourceTypeCustomization $ _smCustomization _bcasmSourceMetadata)
                    for_ resSource $ liftIO . unLogger logger
                    pure resSource
              )
          |) metadataObj

    -- impl notes (swann):
    --
    -- as our cache invalidation key, we use the fact of the availability of event triggers
    -- present, rerunning catalog init when this changes. i.e we invalidate the cache and
    -- rebuild it with the catalog only when there is at least one event trigger present.
    -- This is correct, because we only care about the transition from zero event triggers
    -- to nonzero (not necessarily one, as Anon has observed, because replace_metadata can
    -- add multiple event triggers in one go)
    --
    -- a future optimisation would be to cache, on a per-source basis, whether or not
    -- the event catalog itself exists, and to then trigger catalog init when an event
    -- trigger is created _but only if_ this cached information says the event catalog
    -- doesn't already exist.

    initCatalogIfNeeded ::
      forall b arr m.
      ( ArrowChoice arr,
        Inc.ArrowCache m arr,
        MonadIO m,
        BackendMetadata b,
        HasServerConfigCtx m,
        MonadError QErr m,
        MonadBaseControl IO m
      ) =>
      (Proxy b, Bool, SourceConfig b) `arr` (RecreateEventTriggers, SourceCatalogMigrationState)
    initCatalogIfNeeded = Inc.cache proc (Proxy, atleastOneTrigger, sourceConfig) -> do
      bindA
        -< do
          if atleastOneTrigger
            then do
              maintenanceMode <- _sccMaintenanceMode <$> askServerConfigCtx
              eventingMode <- _sccEventingMode <$> askServerConfigCtx
              readOnlyMode <- _sccReadOnlyMode <$> askServerConfigCtx

              if
                  -- when safe mode is enabled, don't perform any migrations
                  | readOnlyMode == ReadOnlyModeEnabled -> pure (RETDoNothing, SCMSMigrationOnHold "read-only mode enabled")
                  -- when eventing mode is disabled, don't perform any migrations
                  | eventingMode == EventingDisabled -> pure (RETDoNothing, SCMSMigrationOnHold "eventing mode disabled")
                  -- when maintenance mode is enabled, don't perform any migrations
                  | maintenanceMode == (MaintenanceModeEnabled ()) -> pure (RETDoNothing, SCMSMigrationOnHold "maintenance mode enabled")
                  | otherwise -> do
                      -- The `initCatalogForSource` action is retried here because
                      -- in cloud there will be multiple workers (graphql-engine instances)
                      -- trying to migrate the source catalog, when needed. This introduces
                      -- a race condition as both the workers try to migrate the source catalog
                      -- concurrently and when one of them succeeds the other ones will fail
                      -- and be in an inconsistent state. To avoid the inconsistency, we retry
                      -- migrating the catalog on error and in the retry `initCatalogForSource`
                      -- will see that the catalog is already migrated, so it won't attempt the
                      -- migration again
                      liftEither
                        =<< Retry.retrying
                          ( Retry.constantDelay (fromIntegral $ diffTimeToMicroSeconds $ seconds $ Seconds 10)
                              <> Retry.limitRetries 3
                          )
                          (const $ return . isLeft)
                          (const $ runExceptT $ prepareCatalog @b sourceConfig)
            else pure (RETDoNothing, SCMSUninitializedSource)

    buildSource ::
      forall b arr m.
      ( ArrowChoice arr,
        Inc.ArrowDistribute arr,
        Inc.ArrowCache m arr,
        ArrowWriter (Seq (Either InconsistentMetadata MetadataDependency)) arr,
        HasServerConfigCtx m,
        MonadError QErr m,
        BackendMetadata b,
        GetAggregationPredicatesDeps b
      ) =>
      ( HashMap SourceName (AB.AnyBackend PartiallyResolvedSource),
        SourceMetadata b,
        SourceConfig b,
        HashMap (TableName b) (TableCoreInfoG b (ColumnInfo b) (ColumnInfo b)),
        HashMap (TableName b) (EventTriggerInfoMap b),
        DBTablesMetadata b,
        DBFunctionsMetadata b,
        PartiallyResolvedRemoteSchemaMap,
        OrderedRoles
      )
        `arr` BackendSourceInfo
    buildSource = proc (allSources, sourceMetadata, sourceConfig, tablesRawInfo, eventTriggerInfoMaps, _dbTables, dbFunctions, remoteSchemaMap, orderedRoles) -> do
      let SourceMetadata sourceName _backendKind tables functions _ queryTagsConfig sourceCustomization _healthCheckConfig = sourceMetadata
          tablesMetadata = OMap.elems tables
          (_, nonColumnInputs, permissions) = unzip3 $ map mkTableInputs tablesMetadata
          alignTableMap :: HashMap (TableName b) a -> HashMap (TableName b) c -> HashMap (TableName b) (a, c)
          alignTableMap = M.intersectionWith (,)

      -- relationships and computed fields
      let nonColumnsByTable = mapFromL _nctiTable nonColumnInputs
      tableCoreInfos :: HashMap (TableName b) (TableCoreInfo b) <-
        (|
          Inc.keyed
            ( \_ (tableRawInfo, nonColumnInput) -> do
                let columns = _tciFieldInfoMap tableRawInfo
                allFields :: FieldInfoMap (FieldInfo b) <- addNonColumnFields -< (allSources, sourceName, tablesRawInfo, columns, remoteSchemaMap, dbFunctions, nonColumnInput)
                returnA -< (tableRawInfo {_tciFieldInfoMap = allFields})
            )
          |) (tablesRawInfo `alignTableMap` nonColumnsByTable)

      tableCoreInfosDep <- Inc.newDependency -< tableCoreInfos

      -- permissions
      tableCache <-
        (|
          Inc.keyed
            ( \_ ((tableCoreInfo, permissionInputs), eventTriggerInfos) -> do
                let tableFields = _tciFieldInfoMap tableCoreInfo
                permissionInfos <-
                  buildTablePermissions
                    -<
                      (Proxy :: Proxy b, sourceName, tableCoreInfosDep, tableFields, permissionInputs, orderedRoles)
                returnA -< TableInfo tableCoreInfo permissionInfos eventTriggerInfos (mkAdminRolePermInfo tableCoreInfo)
            )
          |) (tableCoreInfos `alignTableMap` mapFromL _tpiTable permissions `alignTableMap` eventTriggerInfoMaps)

      -- not forcing the evaluation here results in a measurable negative impact
      -- on memory residency as measured by our benchmark
      !defaultNC <- bindA -< _sccDefaultNamingConvention <$> askServerConfigCtx
      !isNamingConventionEnabled <- bindA -< ((EFNamingConventions `elem`) . _sccExperimentalFeatures) <$> askServerConfigCtx
      !namingConv <-
        bindA
          -<
            if isNamingConventionEnabled
              then getNamingCase sourceCustomization (namingConventionSupport @b) defaultNC
              else pure HasuraCase
      let resolvedCustomization = mkResolvedSourceCustomization sourceCustomization namingConv

      -- sql functions
      functionCacheMaybes <-
        (|
          Inc.keyed
            ( \_ (FunctionMetadata qf config functionPermissions comment) -> do
                let systemDefined = SystemDefined False
                    definition = toJSON $ TrackFunction @b qf
                    metadataObject =
                      MetadataObject
                        ( MOSourceObjId sourceName $
                            AB.mkAnyBackend $
                              SMOFunction @b qf
                        )
                        definition
                    schemaObject =
                      SOSourceObj sourceName $
                        AB.mkAnyBackend $
                          SOIFunction @b qf
                    addFunctionContext e = "in function " <> qf <<> ": " <> e
                (|
                  withRecordInconsistency
                    ( do
                        (functionInfo, dep) <-
                          bindErrorA
                            -< modifyErr addFunctionContext do
                              let funcDefs = fromMaybe [] $ M.lookup qf dbFunctions
                                  metadataPermissions = mapFromL _fpmRole functionPermissions
                                  permissionsMap = mkBooleanPermissionMap FunctionPermissionInfo metadataPermissions orderedRoles
                              rawfunctionInfo <- handleMultipleFunctions @b qf funcDefs
                              buildFunctionInfo sourceName qf systemDefined config permissionsMap rawfunctionInfo comment namingConv
                        recordDependencies -< (metadataObject, schemaObject, [dep])
                        returnA -< functionInfo
                    )
                  |) metadataObject
            )
          |) (mapFromL _fmFunction (OMap.elems functions))
      let functionCache = catMaybes functionCacheMaybes

      returnA -< AB.mkAnyBackend $ SourceInfo sourceName tableCache functionCache sourceConfig queryTagsConfig resolvedCustomization

    buildAndCollectInfo ::
      forall arr m.
      ( ArrowChoice arr,
        Inc.ArrowDistribute arr,
        Inc.ArrowCache m arr,
        ArrowWriter (Seq (Either InconsistentMetadata MetadataDependency)) arr,
        MonadIO m,
        MonadError QErr m,
        MonadReader BuildReason m,
        MonadBaseControl IO m,
        HasHttpManagerM m,
        HasServerConfigCtx m,
        MonadResolveSource m
      ) =>
      (Inc.Dependency Metadata, Inc.Dependency InvalidationKeys) `arr` BuildOutputs
    buildAndCollectInfo = proc (metadataDep, invalidationKeys) -> do
      sources <- Inc.dependOn -< Inc.selectD #_metaSources metadataDep
      remoteSchemas <- Inc.dependOn -< Inc.selectD #_metaRemoteSchemas metadataDep
      customTypes <- Inc.dependOn -< Inc.selectD #_metaCustomTypes metadataDep
      actions <- Inc.dependOn -< Inc.selectD #_metaActions metadataDep
      inheritedRoles <- Inc.dependOn -< Inc.selectD #_metaInheritedRoles metadataDep
      backendConfigs <- Inc.dependOn -< Inc.selectD #_metaBackendConfigs metadataDep
      let actionRoles = map _apmRole . _amPermissions =<< OMap.elems actions
          remoteSchemaRoles = map _rspmRole . _rsmPermissions =<< OMap.elems remoteSchemas
          sourceRoles =
            HS.fromList $
              concat $
                OMap.elems sources >>= \(BackendSourceMetadata e) ->
                  AB.dispatchAnyBackend @Backend e \(SourceMetadata _ _ tables _functions _ _ _ _) -> do
                    table <- OMap.elems tables
                    pure $
                      OMap.keys (_tmInsertPermissions table)
                        <> OMap.keys (_tmSelectPermissions table)
                        <> OMap.keys (_tmUpdatePermissions table)
                        <> OMap.keys (_tmDeletePermissions table)
          inheritedRoleNames = OMap.keys inheritedRoles
          allRoleNames = sourceRoles <> HS.fromList (remoteSchemaRoles <> actionRoles <> inheritedRoleNames)

      -- roles which have some kind of permission (action/remote schema/table/function) set in the metadata
      let metadataRoles = mapFromL _rRoleName $ (`Role` ParentRoles mempty) <$> toList allRoleNames

      resolvedInheritedRoles <- buildInheritedRoles -< (allRoleNames, OMap.elems inheritedRoles)

      let allRoles = resolvedInheritedRoles `M.union` metadataRoles

      orderedRoles <- bindA -< orderRoles $ M.elems allRoles

      -- remote schemas
      let remoteSchemaInvalidationKeys = Inc.selectD #_ikRemoteSchemas invalidationKeys
      remoteSchemaMap <- buildRemoteSchemas env -< ((remoteSchemaInvalidationKeys, orderedRoles), OMap.elems remoteSchemas)
      let remoteSchemaCtxMap = M.map fst remoteSchemaMap

      !defaultNC <- bindA -< _sccDefaultNamingConvention <$> askServerConfigCtx
      !isNamingConventionEnabled <- bindA -< ((EFNamingConventions `elem`) . _sccExperimentalFeatures) <$> askServerConfigCtx

      let backendInvalidationKeys = Inc.selectD #_ikBackends invalidationKeys
      backendCache <- resolveBackendCache -< (backendInvalidationKeys, BackendMap.elems backendConfigs)

      let backendInfoAndSourceMetadata = joinBackendInfosToSources backendCache sources

      -- sources are build in two steps
      -- first we resolve them, and build the table cache
      partiallyResolvedSourcesMaybes <-
        (|
          Inc.keyed
            ( \_ exists ->
                AB.dispatchAnyBackendArrow @BackendMetadata @BackendEventTrigger
                  ( proc (backendInfoAndSourceMetadata, (invalidationKeys, defaultNC, isNamingConventionEnabled)) -> do
                      let sourceMetadata = _bcasmSourceMetadata backendInfoAndSourceMetadata
                          sourceName = _smName sourceMetadata
                          sourceInvalidationsKeys = Inc.selectD #_ikSources invalidationKeys
                      maybeResolvedSource <- resolveSourceIfNeeded -< (sourceInvalidationsKeys, backendInfoAndSourceMetadata)
                      case maybeResolvedSource of
                        Nothing -> returnA -< Nothing
                        Just (source :: ResolvedSource b) -> do
                          let metadataInvalidationKey = Inc.selectD #_ikMetadata invalidationKeys
                              (tableInputs, _, _) = unzip3 $ map mkTableInputs $ OMap.elems $ _smTables sourceMetadata
                              !namingConv = if isNamingConventionEnabled then getNamingConvention (_smCustomization sourceMetadata) defaultNC else HasuraCase
                          tablesCoreInfo <-
                            buildTableCache
                              -<
                                ( sourceName,
                                  _rsConfig source,
                                  _rsTables source,
                                  tableInputs,
                                  metadataInvalidationKey,
                                  namingConv
                                )

                          let tablesMetadata = OMap.elems $ _smTables sourceMetadata
                              eventTriggers = map (_tmTable &&& OMap.elems . _tmEventTriggers) tablesMetadata
                              numEventTriggers = sum $ map (length . snd) eventTriggers
                              sourceConfig = _rsConfig source

                          (recreateEventTriggers, sourceCatalogMigrationState) <- initCatalogIfNeeded -< (Proxy :: Proxy b, numEventTriggers > 0, sourceConfig)

                          bindA -< unLogger logger (sourceName, sourceCatalogMigrationState)

                          let alignTableMap :: HashMap (TableName b) a -> HashMap (TableName b) c -> HashMap (TableName b) (a, c)
                              alignTableMap = M.intersectionWith (,)

                          eventTriggerInfoMaps <-
                            (|
                              Inc.keyed
                                ( \_ (tableCoreInfo, (_, eventTriggerConfs)) ->
                                    buildTableEventTriggers -< (sourceName, sourceConfig, tableCoreInfo, eventTriggerConfs, metadataInvalidationKey, recreateEventTriggers)
                                )
                              |) (tablesCoreInfo `alignTableMap` mapFromL fst eventTriggers)

                          returnA
                            -<
                              Just $
                                AB.mkAnyBackend @b $
                                  PartiallyResolvedSource sourceMetadata source tablesCoreInfo eventTriggerInfoMaps
                  )
                  -<
                    (exists, (invalidationKeys, defaultNC, isNamingConventionEnabled))
            )
          |) (M.fromList $ OMap.toList backendInfoAndSourceMetadata)
      let partiallyResolvedSources = catMaybes partiallyResolvedSourcesMaybes

      -- then we can build the entire source output
      -- we need to have the table cache of all sources to build cross-sources relationships
      sourcesOutput <-
        (|
          Inc.keyed
            ( \_ exists ->
                -- Note that it's a bit of a coincidence that
                -- 'AB.dispatchAnyBackendArrow' accepts exactly two constraints,
                -- and that we happen to want to apply to exactly two
                -- constraints.
                -- Ideally the function should be able to take an arbitrary
                -- number of constraints.
                AB.dispatchAnyBackendArrow @BackendMetadata @GetAggregationPredicatesDeps
                  ( proc
                      ( partiallyResolvedSource :: PartiallyResolvedSource b,
                        (allResolvedSources, remoteSchemaCtxMap, orderedRoles)
                        )
                    -> do
                      let PartiallyResolvedSource sourceMetadata resolvedSource tablesInfo eventTriggers = partiallyResolvedSource
                          ResolvedSource sourceConfig _sourceCustomization tablesMeta functionsMeta scalars = resolvedSource
                      so <-
                        buildSource
                          -<
                            ( allResolvedSources,
                              sourceMetadata,
                              sourceConfig,
                              tablesInfo,
                              eventTriggers,
                              tablesMeta,
                              functionsMeta,
                              remoteSchemaCtxMap,
                              orderedRoles
                            )
                      returnA -< (so, BackendMap.singleton scalars)
                  )
                  -<
                    ( exists,
                      (partiallyResolvedSources, remoteSchemaCtxMap, orderedRoles)
                    )
            )
          |) partiallyResolvedSources

      remoteSchemaCache <-
        (|
          Inc.keyed
            ( \_ (partiallyResolvedRemoteSchemaCtx, metadataObj) -> do
                let remoteSchemaIntrospection = irDoc $ _rscIntroOriginal partiallyResolvedRemoteSchemaCtx
                resolvedSchemaCtx <-
                  (|
                    traverseA
                      ( \PartiallyResolvedRemoteRelationship {..} ->
                          buildRemoteSchemaRemoteRelationship
                            -<
                              ( (partiallyResolvedSources, remoteSchemaCtxMap),
                                (_rscName partiallyResolvedRemoteSchemaCtx, remoteSchemaIntrospection, _prrrTypeName, _prrrDefinition)
                              )
                      )
                    |) partiallyResolvedRemoteSchemaCtx
                returnA -< (catMaybes $ resolvedSchemaCtx, metadataObj)
            )
          |) remoteSchemaMap

      -- custom types
      let scalarsMap = mconcat $ map snd $ M.elems sourcesOutput
          sourcesCache = M.map fst sourcesOutput
      maybeResolvedCustomTypes <-
        (|
          withRecordInconsistency
            ( bindErrorA -< resolveCustomTypes sourcesCache customTypes scalarsMap
            )
          |) (MetadataObject MOCustomTypes $ toJSON customTypes)

      -- actions
      let actionList = OMap.elems actions
      (actionCache, annotatedCustomTypes) <- case maybeResolvedCustomTypes of
        Just resolvedCustomTypes -> do
          actionCache' <- buildActions -< ((resolvedCustomTypes, scalarsMap, orderedRoles), actionList)
          returnA -< (actionCache', resolvedCustomTypes)

        -- If the custom types themselves are inconsistent, we can’t really do
        -- anything with actions, so just mark them all inconsistent.
        Nothing -> do
          recordInconsistencies
            -<
              ( map mkActionMetadataObject actionList,
                "custom types are inconsistent"
              )
          returnA -< (mempty, mempty)

      returnA
        -<
          BuildOutputs
            { _boSources = M.map fst sourcesOutput,
              _boActions = actionCache,
              _boRemoteSchemas = remoteSchemaCache,
              _boCustomTypes = annotatedCustomTypes,
              _boRoles = mapFromL _rRoleName $ _unOrderedRoles orderedRoles,
              _boBackendCache = backendCache
            }

    mkEndpointMetadataObject (name, createEndpoint) =
      let objectId = MOEndpoint name
       in MetadataObject objectId (toJSON createEndpoint)

    buildOpenTelemetry ::
      ( ArrowChoice arr,
        Inc.ArrowCache m arr,
        ArrowWriter (Seq (Either InconsistentMetadata md)) arr,
        MonadError QErr m
      ) =>
      OpenTelemetryConfig `arr` OpenTelemetryInfo
    buildOpenTelemetry = proc openTelemetryConfig -> do
      case _ocStatus openTelemetryConfig of
        OtelDisabled ->
          -- Disable all components if OpenTelemetry export not enabled
          returnA -< OpenTelemetryInfo Nothing Nothing
        OtelEnabled -> do
          mOtelExporterInfo <-
            let exporterOtlp = _ocExporterOtlp openTelemetryConfig
             in (|
                  withRecordInconsistency
                    ( bindErrorA -< liftEither (parseOtelExporterConfig env exporterOtlp)
                    )
                |) (MetadataObject (MOOpenTelemetry OtelSubobjectExporterOtlp) (toJSON exporterOtlp))
          mOtelBatchSpanProcessorInfo <-
            let batchSpanProcessor = _ocBatchSpanProcessor openTelemetryConfig
             in (|
                  withRecordInconsistency
                    ( bindErrorA -< liftEither (parseOtelBatchSpanProcessorConfig batchSpanProcessor)
                    )
                |) (MetadataObject (MOOpenTelemetry OtelSubobjectBatchSpanProcessor) (toJSON batchSpanProcessor))
          returnA
            -<
              OpenTelemetryInfo
                mOtelExporterInfo
                -- Disable data types if they are not in the enabled set
                ( if OtelTraces `S.member` _ocEnabledDataTypes openTelemetryConfig
                    then mOtelBatchSpanProcessorInfo
                    else Nothing
                )

    buildEndpoint ::
      (ArrowChoice arr, ArrowKleisli m arr, MonadError QErr m, ArrowWriter (Seq (Either InconsistentMetadata md)) arr) =>
      (InsOrdHashMap CollectionName CreateCollection, (EndpointName, CreateEndpoint)) `arr` Maybe (EndpointMetadata GQLQueryWithText)
    buildEndpoint = proc (collections, e@(name, createEndpoint)) -> do
      let endpoint = createEndpoint
          -- QueryReference collName queryName = _edQuery endpoint
          addContext err = "in endpoint " <> toTxt (unEndpointName name) <> ": " <> err
      (|
        withRecordInconsistency
          (bindErrorA -< modifyErr addContext $ resolveEndpoint collections endpoint)
        |) (mkEndpointMetadataObject e)

    resolveEndpoint ::
      QErrM m =>
      InsOrdHashMap CollectionName CreateCollection ->
      EndpointMetadata QueryReference ->
      m (EndpointMetadata GQLQueryWithText)
    resolveEndpoint collections = traverse $ \(QueryReference collName queryName) -> do
      collection <-
        onNothing
          (OMap.lookup collName collections)
          (throw400 NotExists $ "collection with name " <> toTxt collName <> " does not exist")
      listedQuery <-
        flip
          onNothing
          ( throw400 NotExists $
              "query with name "
                <> toTxt queryName
                <> " does not exist in collection "
                <> toTxt collName
          )
          $ find ((== queryName) . _lqName) (_cdQueries (_ccDefinition collection))

      let lq@(GQLQueryWithText lqq) = _lqQuery listedQuery
          ds = G.getExecutableDefinitions $ unGQLQuery $ snd lqq

      case ds of
        [G.ExecutableDefinitionOperation (G.OperationDefinitionTyped d)]
          | G._todType d == G.OperationTypeSubscription ->
              throw405 $ "query with name " <> toTxt queryName <> " is a subscription"
          | otherwise -> pure ()
        [] -> throw400 BadRequest $ "query with name " <> toTxt queryName <> " has no definitions."
        _ -> throw400 BadRequest $ "query with name " <> toTxt queryName <> " has multiple definitions."

      pure lq

    mkEventTriggerMetadataObject ::
      forall b a c.
      Backend b =>
      (a, SourceName, c, TableName b, RecreateEventTriggers, EventTriggerConf b) ->
      MetadataObject
    mkEventTriggerMetadataObject (_, source, _, table, _, eventTriggerConf) =
      let objectId =
            MOSourceObjId source $
              AB.mkAnyBackend $
                SMOTableObj @b table $
                  MTOTrigger $
                    etcName eventTriggerConf
          definition = object ["table" .= table, "configuration" .= eventTriggerConf]
       in MetadataObject objectId definition

    mkCronTriggerMetadataObject catalogCronTrigger =
      let definition = toJSON catalogCronTrigger
       in MetadataObject
            (MOCronTrigger (ctName catalogCronTrigger))
            definition

    mkActionMetadataObject (ActionMetadata name comment defn _) =
      MetadataObject (MOAction name) (toJSON $ CreateAction name defn comment)

    mkInheritedRoleMetadataObject inheritedRole@(Role roleName _) =
      MetadataObject (MOInheritedRole roleName) (toJSON inheritedRole)

    buildTableEventTriggers ::
      forall arr m b.
      ( ArrowChoice arr,
        Inc.ArrowDistribute arr,
        ArrowWriter (Seq (Either InconsistentMetadata MetadataDependency)) arr,
        Inc.ArrowCache m arr,
        MonadIO m,
        MonadError QErr m,
        MonadBaseControl IO m,
        MonadReader BuildReason m,
        HasServerConfigCtx m,
        BackendMetadata b,
        BackendEventTrigger b
      ) =>
      ( SourceName,
        SourceConfig b,
        TableCoreInfoG b (ColumnInfo b) (ColumnInfo b),
        [EventTriggerConf b],
        Inc.Dependency Inc.InvalidationKey,
        RecreateEventTriggers
      )
        `arr` (EventTriggerInfoMap b)
    buildTableEventTriggers = proc (sourceName, sourceConfig, tableInfo, eventTriggerConfs, metadataInvalidationKey, migrationRecreateEventTriggers) ->
      buildInfoMap (etcName . (^. _6)) (mkEventTriggerMetadataObject @b) buildEventTrigger
        -<
          (tableInfo, map (metadataInvalidationKey,sourceName,sourceConfig,_tciName tableInfo,migrationRecreateEventTriggers,) eventTriggerConfs)
      where
        buildEventTrigger = proc (tableInfo, (metadataInvalidationKey, source, sourceConfig, table, migrationRecreateEventTriggers, eventTriggerConf)) -> do
          let triggerName = etcName eventTriggerConf
              metadataObject = mkEventTriggerMetadataObject @b (metadataInvalidationKey, source, sourceConfig, table, migrationRecreateEventTriggers, eventTriggerConf)
              schemaObjectId =
                SOSourceObj source $
                  AB.mkAnyBackend $
                    SOITableObj @b table $
                      TOTrigger triggerName
              addTriggerContext e = "in event trigger " <> triggerName <<> ": " <> e
          buildReason <- bindA -< ask
          let reloadMetadataRecreateEventTrigger =
                case buildReason of
                  CatalogSync -> RETDoNothing
                  CatalogUpdate Nothing -> RETDoNothing
                  CatalogUpdate (Just sources) -> if source `elem` sources then RETRecreate else RETDoNothing
          (|
            withRecordInconsistency
              ( (|
                  modifyErrA
                    ( do
                        (info, dependencies) <- bindErrorA -< buildEventTriggerInfo @b env source table eventTriggerConf
                        serverConfigCtx <- bindA -< askServerConfigCtx
                        let isCatalogUpdate =
                              case buildReason of
                                CatalogUpdate _ -> True
                                CatalogSync -> False
                            tableColumns = M.elems $ _tciFieldInfoMap tableInfo
                        if ( _sccMaintenanceMode serverConfigCtx == MaintenanceModeDisabled
                               && _sccReadOnlyMode serverConfigCtx == ReadOnlyModeDisabled
                           )
                          then do
                            bindA
                              -<
                                when (reloadMetadataRecreateEventTrigger == RETRecreate) $
                                  -- This is the case when the user sets `recreate_event_triggers`
                                  -- to `true` in `reload_metadata`, in this case, we recreate
                                  -- the SQL trigger by force, even if it may not be necessary
                                  liftEitherM $
                                    createTableEventTrigger
                                      @b
                                      serverConfigCtx
                                      sourceConfig
                                      table
                                      tableColumns
                                      triggerName
                                      (etcDefinition eventTriggerConf)
                                      (_tciPrimaryKey tableInfo)
                            if isCatalogUpdate || migrationRecreateEventTriggers == RETRecreate
                              then do
                                recreateTriggerIfNeeded
                                  -<
                                    ( table,
                                      tableColumns,
                                      triggerName,
                                      etcDefinition eventTriggerConf,
                                      sourceConfig,
                                      (_tciPrimaryKey tableInfo)
                                    )
                                -- We check if the SQL triggers for the event triggers
                                -- are present. If any SQL triggers are missing, those are
                                -- created.
                                bindA
                                  -<
                                    createMissingSQLTriggers
                                      sourceConfig
                                      table
                                      (tableColumns, _tciPrimaryKey tableInfo)
                                      triggerName
                                      (etcDefinition eventTriggerConf)
                              else bindA -< pure ()
                          else bindA -< pure ()
                        recordDependencies -< (metadataObject, schemaObjectId, dependencies)
                        returnA -< info
                    )
                |) (addTableContext @b table . addTriggerContext)
              )
            |) metadataObject

        recreateTriggerIfNeeded =
          -- using `Inc.cache` here means that the response will be cached for the given output and the
          -- next time this arrow recieves the same input, the cached response will be returned and the
          -- computation will not be done again.
          Inc.cache
            proc
              ( tableName,
                tableColumns,
                triggerName,
                triggerDefinition,
                sourceConfig,
                primaryKey
                )
            -> do
              bindA
                -< do
                  serverConfigCtx <- askServerConfigCtx
                  liftEitherM $
                    createTableEventTrigger @b
                      serverConfigCtx
                      sourceConfig
                      tableName
                      tableColumns
                      triggerName
                      triggerDefinition
                      primaryKey

    buildCronTriggers ::
      ( ArrowChoice arr,
        Inc.ArrowDistribute arr,
        ArrowWriter (Seq (Either InconsistentMetadata md)) arr,
        Inc.ArrowCache m arr,
        MonadError QErr m
      ) =>
      ((), [CronTriggerMetadata])
        `arr` HashMap TriggerName CronTriggerInfo
    buildCronTriggers = buildInfoMap ctName mkCronTriggerMetadataObject buildCronTrigger
      where
        buildCronTrigger = proc (_, cronTrigger) -> do
          let triggerName = triggerNameToTxt $ ctName cronTrigger
              addCronTriggerContext e = "in cron trigger " <> triggerName <> ": " <> e
          (|
            withRecordInconsistency
              (bindErrorA -< modifyErr addCronTriggerContext $ resolveCronTrigger env cronTrigger)
            |) (mkCronTriggerMetadataObject cronTrigger)

    buildInheritedRoles ::
      ( ArrowChoice arr,
        Inc.ArrowDistribute arr,
        ArrowWriter (Seq (Either InconsistentMetadata MetadataDependency)) arr,
        Inc.ArrowCache m arr,
        MonadError QErr m
      ) =>
      (HashSet RoleName, [InheritedRole])
        `arr` HashMap RoleName Role
    buildInheritedRoles = buildInfoMap _rRoleName mkInheritedRoleMetadataObject buildInheritedRole
      where
        buildInheritedRole = proc (allRoles, inheritedRole) -> do
          let addInheritedRoleContext e = "in inherited role " <> roleNameToTxt (_rRoleName inheritedRole) <> ": " <> e
              metadataObject = mkInheritedRoleMetadataObject inheritedRole
              schemaObject = SORole $ _rRoleName inheritedRole
          (|
            withRecordInconsistency
              ( do
                  (resolvedInheritedRole, dependencies) <- bindErrorA -< modifyErr addInheritedRoleContext $ resolveInheritedRole allRoles inheritedRole
                  recordDependencies -< (metadataObject, schemaObject, dependencies)
                  returnA -< resolvedInheritedRole
              )
            |) metadataObject

    buildActions ::
      ( ArrowChoice arr,
        Inc.ArrowDistribute arr,
        Inc.ArrowCache m arr,
        ArrowWriter (Seq (Either InconsistentMetadata MetadataDependency)) arr,
        MonadError QErr m
      ) =>
      ( (AnnotatedCustomTypes, BackendMap ScalarMap, OrderedRoles),
        [ActionMetadata]
      )
        `arr` HashMap ActionName ActionInfo
    buildActions = buildInfoMap _amName mkActionMetadataObject buildAction
      where
        buildAction = proc ((resolvedCustomTypes, scalarsMap, orderedRoles), action) -> do
          let ActionMetadata name comment def actionPermissions = action
              addActionContext e = "in action " <> name <<> "; " <> e
          (|
            withRecordInconsistency
              ( bindErrorA
                  -< modifyErr addActionContext $ do
                    (resolvedDef, outObject) <-
                      resolveAction env resolvedCustomTypes def scalarsMap
                    let permissionInfos = map (ActionPermissionInfo . _apmRole) actionPermissions
                        metadataPermissionMap = mapFromL _apiRole permissionInfos
                        permissionsMap = mkBooleanPermissionMap ActionPermissionInfo metadataPermissionMap orderedRoles
                        forwardClientHeaders = _adForwardClientHeaders resolvedDef
                        outputType = unGraphQLType $ _adOutputType def
                    return $ ActionInfo name (outputType, outObject) resolvedDef permissionsMap forwardClientHeaders comment
              )
            |) (mkActionMetadataObject action)

buildRemoteSchemaRemoteRelationship ::
  forall arr m.
  ( ArrowChoice arr,
    ArrowWriter (Seq (Either InconsistentMetadata MetadataDependency)) arr,
    ArrowKleisli m arr,
    MonadError QErr m
  ) =>
  ( (HashMap SourceName (AB.AnyBackend PartiallyResolvedSource), PartiallyResolvedRemoteSchemaMap),
    (RemoteSchemaName, RemoteSchemaIntrospection, G.Name, RemoteRelationship)
  )
    `arr` Maybe (RemoteFieldInfo G.Name)
buildRemoteSchemaRemoteRelationship =
  proc
    ( (allSources, remoteSchemaMap),
      (remoteSchema, remoteSchemaIntrospection, typeName, rr@RemoteRelationship {..})
      )
  -> do
    let metadataObject = mkRemoteSchemaRemoteRelationshipMetadataObject (remoteSchema, typeName, rr)
        schemaObj = SORemoteSchemaRemoteRelationship remoteSchema typeName _rrName
        addRemoteRelationshipContext e = "in remote relationship" <> _rrName <<> ": " <> e
    (|
      withRecordInconsistency
        ( do
            (remoteField, rhsDependencies) <-
              bindErrorA
                -< modifyErr addRemoteRelationshipContext do
                  allowedLHSJoinFields <- getRemoteSchemaEntityJoinColumns remoteSchema remoteSchemaIntrospection typeName
                  buildRemoteFieldInfo (remoteSchemaToLHSIdentifier remoteSchema) allowedLHSJoinFields rr allSources remoteSchemaMap
            -- buildRemoteFieldInfo only knows how to construct dependencies on the RHS of the join condition,
            -- so the dependencies on the remote relationship on the LHS entity have to be computed here
            let lhsDependencies =
                  -- a direct dependency on the remote schema on which this is defined
                  [SchemaDependency (SORemoteSchema remoteSchema) DRRemoteRelationship]
            recordDependencies -< (metadataObject, schemaObj, lhsDependencies <> rhsDependencies)
            returnA -< remoteField
        )
      |) metadataObject

mkRemoteSchemaRemoteRelationshipMetadataObject ::
  (RemoteSchemaName, G.Name, RemoteRelationship) ->
  MetadataObject
mkRemoteSchemaRemoteRelationshipMetadataObject (remoteSchemaName, typeName, RemoteRelationship {..}) =
  let objectId =
        MORemoteSchemaRemoteRelationship remoteSchemaName typeName _rrName
   in MetadataObject objectId $
        toJSON $
          CreateRemoteSchemaRemoteRelationship remoteSchemaName typeName _rrName _rrDefinition

data BackendInfoAndSourceMetadata b = BackendInfoAndSourceMetadata
  { _bcasmBackendInfo :: BackendInfo b,
    _bcasmSourceMetadata :: SourceMetadata b
  }
  deriving stock (Generic)

deriving instance (Backend b) => Show (BackendInfoAndSourceMetadata b)

deriving instance (Backend b) => Eq (BackendInfoAndSourceMetadata b)

joinBackendInfosToSources ::
  BackendCache ->
  InsOrdHashMap SourceName BackendSourceMetadata ->
  InsOrdHashMap SourceName (AB.AnyBackend BackendInfoAndSourceMetadata)
joinBackendInfosToSources backendInfos sources =
  flip OMap.map sources $ \abSourceMetadata ->
    AB.dispatchAnyBackend @Backend (unBackendSourceMetadata abSourceMetadata) $ \(sourceMetadata :: SourceMetadata b) ->
      let _bcasmBackendInfo = maybe mempty unBackendInfoWrapper (BackendMap.lookup @b backendInfos)
          _bcasmSourceMetadata = sourceMetadata
       in AB.mkAnyBackend @b BackendInfoAndSourceMetadata {..}

{- Note [Keep invalidation keys for inconsistent objects]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
After building the schema cache, we prune InvalidationKeys for objects
that no longer exist in the schema to avoid leaking memory for objects
that have been dropped. However, note that we *don’t* want to drop
keys for objects that are simply inconsistent!

Why? The object is still in the metadata, so next time we reload it,
we’ll reprocess that object. We want to reuse the cache if its
definition hasn’t changed, but if we dropped the invalidation key, it
will incorrectly be reprocessed (since the invalidation key changed
from present to absent). -}
