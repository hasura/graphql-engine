{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveAnyClass #-}
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
import Control.Arrow.Interpret
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
import Data.Sequence qualified as Seq
import Data.Set qualified as S
import Data.Text.Extended
import Hasura.Base.Error
import Hasura.CustomReturnType.Cache (CustomReturnTypeCache, CustomReturnTypeInfo (..))
import Hasura.CustomReturnType.Common (toFieldInfo)
import Hasura.CustomReturnType.Metadata (CustomReturnTypeMetadata (..))
import Hasura.EncJSON
import Hasura.Function.API
import Hasura.Function.Cache
import Hasura.Function.Metadata (FunctionMetadata (..))
import Hasura.GraphQL.Schema (buildGQLContext)
import Hasura.GraphQL.Schema.NamingCase
import Hasura.Incremental qualified as Inc
import Hasura.Logging
import Hasura.Metadata.Class
import Hasura.NativeQuery.Cache (NativeQueryCache, NativeQueryInfo (..))
import Hasura.NativeQuery.Metadata (NativeQueryMetadata (..))
import Hasura.Prelude
import Hasura.QueryTags
import Hasura.RQL.DDL.Action
import Hasura.RQL.DDL.ApiLimit (MonadGetApiTimeLimit (..))
import Hasura.RQL.DDL.CustomTypes
import Hasura.RQL.DDL.EventTrigger (MonadEventLogCleanup (..), buildEventTriggerInfo)
import Hasura.RQL.DDL.InheritedRoles (resolveInheritedRole)
import Hasura.RQL.DDL.RemoteRelationship (CreateRemoteSchemaRemoteRelationship (..), PartiallyResolvedSource (..), buildRemoteFieldInfo, getRemoteSchemaEntityJoinColumns)
import Hasura.RQL.DDL.ScheduledTrigger
import Hasura.RQL.DDL.Schema.Cache.Common
import Hasura.RQL.DDL.Schema.Cache.Config
import Hasura.RQL.DDL.Schema.Cache.Dependencies
import Hasura.RQL.DDL.Schema.Cache.Fields
import Hasura.RQL.DDL.Schema.Cache.Permission
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
import Hasura.RQL.Types.Metadata hiding (tmTable)
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
import Hasura.Server.Init.FeatureFlag qualified as FF
import Hasura.Server.Migrate.Version
import Hasura.Server.Types
import Hasura.Services
import Hasura.Session
import Hasura.Tracing qualified as Tracing
import Language.GraphQL.Draft.Syntax qualified as G

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
  MetadataWithResourceVersion ->
  CacheDynamicConfig ->
  CacheBuild RebuildableSchemaCache
buildRebuildableSchemaCache =
  buildRebuildableSchemaCacheWithReason CatalogSync

buildRebuildableSchemaCacheWithReason ::
  BuildReason ->
  Logger Hasura ->
  Env.Environment ->
  MetadataWithResourceVersion ->
  CacheDynamicConfig ->
  CacheBuild RebuildableSchemaCache
buildRebuildableSchemaCacheWithReason reason logger env metadataWithVersion dynamicConfig = do
  result <-
    flip runReaderT reason $
      Inc.build (buildSchemaCacheRule logger env) (metadataWithVersion, dynamicConfig, initialInvalidationKeys, Nothing)

  pure $ RebuildableSchemaCache (Inc.result result) initialInvalidationKeys (Inc.rebuildRule result)

newtype CacheRWT m a
  = -- The CacheInvalidations component of the state could actually be collected
    -- using WriterT, but WriterT implementations prior to transformers-0.5.6.0
    -- (which added Control.Monad.Trans.Writer.CPS) are leaky, and we don’t have
    -- that yet.
    --
    -- The use of 'ReaderT CacheDynamicConfig' is only here to avoid manually
    -- passing the 'CacheDynamicConfig' to every function that builds the cache. It
    -- should ultimately be reduced to 'AppContext', or even better a relevant
    -- subset thereof.
    CacheRWT (ReaderT CacheDynamicConfig (StateT (RebuildableSchemaCache, CacheInvalidations) m) a)
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadError e,
      UserInfoM,
      MonadMetadataStorage,
      Tracing.MonadTrace,
      MonadBase b,
      MonadBaseControl b,
      ProvidesNetwork,
      FF.HasFeatureFlagChecker
    )
  deriving anyclass (MonadQueryTags)

instance MonadReader r m => MonadReader r (CacheRWT m) where
  ask = lift ask
  local f (CacheRWT m) = CacheRWT $ mapReaderT (local f) m

instance (MonadEventLogCleanup m) => MonadEventLogCleanup (CacheRWT m) where
  runLogCleaner sourceCache conf = lift $ runLogCleaner sourceCache conf
  generateCleanupSchedules sourceInfo triggerName cleanupConfig = lift $ generateCleanupSchedules sourceInfo triggerName cleanupConfig
  updateTriggerCleanupSchedules logger oldSources newSources schemaCache = lift $ updateTriggerCleanupSchedules logger oldSources newSources schemaCache

instance (MonadGetApiTimeLimit m) => MonadGetApiTimeLimit (CacheRWT m) where
  runGetApiTimeLimit = lift $ runGetApiTimeLimit

runCacheRWT ::
  Monad m =>
  CacheDynamicConfig ->
  RebuildableSchemaCache ->
  CacheRWT m a ->
  m (a, RebuildableSchemaCache, CacheInvalidations)
runCacheRWT config cache (CacheRWT m) = do
  (v, (newCache, invalidations)) <-
    runStateT (runReaderT m config) (cache, mempty)
  pure (v, newCache, invalidations)

instance MonadTrans CacheRWT where
  lift = CacheRWT . lift . lift

instance (Monad m) => CacheRM (CacheRWT m) where
  askSchemaCache = CacheRWT $ gets (lastBuiltSchemaCache . (^. _1))

instance
  ( MonadIO m,
    MonadError QErr m,
    ProvidesNetwork m,
    MonadResolveSource m,
    HasCacheStaticConfig m
  ) =>
  CacheRWM (CacheRWT m)
  where
  buildSchemaCacheWithOptions buildReason invalidations metadata = CacheRWT do
    dynamicConfig <- ask
    (RebuildableSchemaCache lastBuiltSC invalidationKeys rule, oldInvalidations) <- get
    let metadataWithVersion = MetadataWithResourceVersion metadata $ scMetadataResourceVersion lastBuiltSC
        newInvalidationKeys = invalidateKeys invalidations invalidationKeys
    result <-
      runCacheBuildM $
        flip runReaderT buildReason $
          Inc.build rule (metadataWithVersion, dynamicConfig, newInvalidationKeys, Nothing)
    let schemaCache = Inc.result result
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
                { scMetadataResourceVersion = resourceVersion
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
    ProvidesNetwork m,
    MonadResolveSource m,
    HasCacheStaticConfig m
  ) =>
  Logger Hasura ->
  Env.Environment ->
  (MetadataWithResourceVersion, CacheDynamicConfig, InvalidationKeys, Maybe StoredIntrospection) `arr` SchemaCache
buildSchemaCacheRule logger env = proc (MetadataWithResourceVersion metadataNoDefaults resourceVersion, dynamicConfig, invalidationKeys, storedIntrospection) -> do
  invalidationKeysDep <- Inc.newDependency -< invalidationKeys
  let metadataDefaults = _cdcMetadataDefaults dynamicConfig
      metadata@Metadata {..} = overrideMetadataDefaults metadataNoDefaults metadataDefaults
  metadataDep <- Inc.newDependency -< metadata

  (inconsistentObjects, (resolvedOutputs, dependencyInconsistentObjects, resolvedDependencies), ((adminIntrospection, gqlContext, gqlContextUnauth, inconsistentRemoteSchemas), (relayContext, relayContextUnauth))) <-
    Inc.cache buildOutputsAndSchema -< (metadataDep, dynamicConfig, invalidationKeysDep, storedIntrospection)

  let (resolvedEndpoints, endpointCollectedInfo) = runIdentity $ runWriterT $ buildRESTEndpoints _metaQueryCollections (OMap.elems _metaRestEndpoints)
      (cronTriggersMap, cronTriggersCollectedInfo) = runIdentity $ runWriterT $ buildCronTriggers (OMap.elems _metaCronTriggers)
      (openTelemetryInfo, openTelemetryCollectedInfo) = runIdentity $ runWriterT $ buildOpenTelemetry _metaOpenTelemetryConfig

      duplicateVariables :: EndpointMetadata a -> Bool
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

      inconsistentQueryCollections = getInconsistentQueryCollections adminIntrospection _metaQueryCollections listedQueryObjects endpoints globalAllowLists

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
          scMetadataResourceVersion = resourceVersion,
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
    buildOutputsAndSchema = proc (metadataDep, dynamicConfig, invalidationKeysDep, storedIntrospection) -> do
      (outputs, collectedInfo) <- runWriterA buildAndCollectInfo -< (dynamicConfig, metadataDep, invalidationKeysDep, storedIntrospection)
      let (inconsistentObjects, unresolvedDependencies) = partitionEithers $ toList collectedInfo
      out2@(resolvedOutputs, _dependencyInconsistentObjects, _resolvedDependencies) <- resolveDependencies -< (outputs, unresolvedDependencies)
      out3 <-
        bindA
          -< do
            buildGQLContext
              (_cdcFunctionPermsCtx dynamicConfig)
              (_cdcRemoteSchemaPermsCtx dynamicConfig)
              (_cdcExperimentalFeatures dynamicConfig)
              (_cdcSQLGenCtx dynamicConfig)
              (_cdcApolloFederationStatus dynamicConfig)
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
        MonadBaseControl IO m,
        ProvidesNetwork m
      ) =>
      (BackendConfigWrapper b, Inc.Dependency (BackendMap BackendInvalidationKeysWrapper)) `arr` BackendCache
    resolveBackendInfo' = proc (backendConfigWrapper, backendInvalidationMap) -> do
      let backendInvalidationKeys =
            Inc.selectMaybeD #unBackendInvalidationKeysWrapper $
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
        MonadBaseControl IO m,
        ProvidesNetwork m,
        HasCacheStaticConfig m
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

    tryGetSourceConfig ::
      forall b arr m.
      ( ArrowChoice arr,
        Inc.ArrowCache m arr,
        ArrowWriter (Seq (Either InconsistentMetadata MetadataDependency)) arr,
        MonadIO m,
        MonadBaseControl IO m,
        MonadResolveSource m,
        ProvidesNetwork m,
        BackendMetadata b
      ) =>
      ( Inc.Dependency (HashMap SourceName Inc.InvalidationKey),
        SourceName,
        SourceConnConfiguration b,
        BackendSourceKind b,
        BackendInfo b
      )
        `arr` Maybe (SourceConfig b)
    tryGetSourceConfig = Inc.cache proc (invalidationKeys, sourceName, sourceConfig, backendKind, backendInfo) -> do
      let metadataObj = MetadataObject (MOSource sourceName) $ toJSON sourceName
      -- TODO: if we make all of 'resolveSourceConfig' a Service, we could
      -- delegate to it the responsibility of extracting the HTTP manager, and
      -- avoid having to thread 'ProvidesNetwork' throughout the cache building
      -- code.
      httpMgr <- bindA -< askHTTPManager
      Inc.dependOn -< Inc.selectKeyD sourceName invalidationKeys
      (|
        withRecordInconsistency
          ( bindErrorA -< ExceptT $ resolveSourceConfig @b sourceName sourceConfig backendKind backendInfo env httpMgr
          )
        |) metadataObj

    tryResolveSource ::
      forall b arr m.
      ( ArrowChoice arr,
        Inc.ArrowCache m arr,
        ArrowWriter (Seq (Either InconsistentMetadata MetadataDependency)) arr,
        MonadIO m,
        MonadBaseControl IO m,
        MonadResolveSource m,
        ProvidesNetwork m,
        BackendMetadata b
      ) =>
      ( Inc.Dependency (HashMap SourceName Inc.InvalidationKey),
        Maybe (BackendIntrospection b),
        BackendInfoAndSourceMetadata b
      )
        `arr` Maybe (SourceConfig b, DBObjectsIntrospection b)
    tryResolveSource = Inc.cache proc (invalidationKeys, sourceIntrospection, BackendInfoAndSourceMetadata {..}) -> do
      let sourceName = _smName _bcasmSourceMetadata
          metadataObj = MetadataObject (MOSource sourceName) $ toJSON sourceName

      maybeSourceConfig <- tryGetSourceConfig @b -< (invalidationKeys, sourceName, _smConfiguration _bcasmSourceMetadata, _smKind _bcasmSourceMetadata, _bcasmBackendInfo)
      case maybeSourceConfig of
        Nothing -> returnA -< Nothing
        Just sourceConfig -> do
          case biMetadata <$> sourceIntrospection of
            Just rs -> returnA -< Just (sourceConfig, rs)
            _ ->
              (|
                withRecordInconsistency
                  ( bindErrorA
                      -< ExceptT do
                        resSource <- resolveDatabaseMetadata logger _bcasmSourceMetadata sourceConfig
                        for_ resSource $ liftIO . unLogger logger
                        pure $ (sourceConfig,) <$> resSource
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
        MonadError QErr m,
        MonadBaseControl IO m,
        HasCacheStaticConfig m
      ) =>
      (Proxy b, Bool, SourceConfig b) `arr` (RecreateEventTriggers, SourceCatalogMigrationState)
    initCatalogIfNeeded = Inc.cache proc (Proxy, atleastOneTrigger, sourceConfig) -> do
      bindA
        -< do
          if atleastOneTrigger
            then do
              cacheStaticConfig <- askCacheStaticConfig
              let maintenanceMode = _cscMaintenanceMode cacheStaticConfig
                  eventingMode = _cscEventingMode cacheStaticConfig
                  readOnlyMode = _cscReadOnlyMode cacheStaticConfig

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
        ArrowKleisli m arr,
        ArrowWriter (Seq (Either InconsistentMetadata MetadataDependency)) arr,
        MonadError QErr m,
        BackendMetadata b,
        GetAggregationPredicatesDeps b
      ) =>
      ( CacheDynamicConfig,
        HashMap SourceName (AB.AnyBackend PartiallyResolvedSource),
        SourceMetadata b,
        SourceConfig b,
        HashMap (TableName b) (TableCoreInfoG b (ColumnInfo b) (ColumnInfo b)),
        HashMap (TableName b) (EventTriggerInfoMap b),
        DBTablesMetadata b,
        DBFunctionsMetadata b,
        PartiallyResolvedRemoteSchemaMap,
        OrderedRoles
      )
        `arr` (SourceInfo b)
    buildSource = proc (dynamicConfig, allSources, sourceMetadata, sourceConfig, tablesRawInfo, eventTriggerInfoMaps, _dbTables, dbFunctions, remoteSchemaMap, orderedRoles) -> do
      let SourceMetadata sourceName _backendKind tables functions nativeQueries customReturnTypes _ queryTagsConfig sourceCustomization _healthCheckConfig = sourceMetadata
          tablesMetadata = OMap.elems tables
          (_, nonColumnInputs, permissions) = unzip3 $ map mkTableInputs tablesMetadata
          alignTableMap :: HashMap (TableName b) a -> HashMap (TableName b) c -> HashMap (TableName b) (a, c)
          alignTableMap = M.intersectionWith (,)

      -- relationships and computed fields
      let nonColumnsByTable = mapFromL _nctiTable nonColumnInputs
      tableCoreInfos :: HashMap (TableName b) (TableCoreInfo b) <-
        interpretWriter
          -< for (tablesRawInfo `alignTableMap` nonColumnsByTable) \(tableRawInfo, nonColumnInput) -> do
            let columns = _tciFieldInfoMap tableRawInfo
                customObjectTypes = _tciCustomObjectTypes tableRawInfo
            allFields :: FieldInfoMap (FieldInfo b) <- addNonColumnFields allSources sourceName customObjectTypes tablesRawInfo columns remoteSchemaMap dbFunctions nonColumnInput
            pure $ tableRawInfo {_tciFieldInfoMap = allFields}

      -- permissions
      result <-
        interpretWriter
          -< runExceptT $
            for
              (tableCoreInfos `alignTableMap` mapFromL _tpiTable permissions `alignTableMap` eventTriggerInfoMaps)
              \((tableCoreInfo, permissionInputs), eventTriggerInfos) -> do
                let tableFields = _tciFieldInfoMap tableCoreInfo
                permissionInfos <-
                  buildTablePermissions
                    sourceName
                    tableCoreInfos
                    tableFields
                    permissionInputs
                    orderedRoles
                pure $ TableInfo tableCoreInfo permissionInfos eventTriggerInfos (mkAdminRolePermInfo tableCoreInfo)
      -- Generate a non-recoverable error when inherited roles were not ordered in a way that allows for building permissions to succeed
      tableCache <- bindA -< liftEither result

      -- not forcing the evaluation here results in a measurable negative impact
      -- on memory residency as measured by our benchmark
      let !defaultNC = _cdcDefaultNamingConvention dynamicConfig
          !isNamingConventionEnabled = EFNamingConventions `elem` (_cdcExperimentalFeatures dynamicConfig)
      !namingConv <-
        bindA
          -<
            if isNamingConventionEnabled
              then getNamingCase sourceCustomization (namingConventionSupport @b) defaultNC
              else pure HasuraCase
      let resolvedCustomization = mkResolvedSourceCustomization sourceCustomization namingConv

      -- sql functions
      functionCacheMaybes <-
        interpretWriter
          -< for
            (OMap.elems functions)
            \case
              FunctionMetadata qf config functionPermissions comment -> do
                let systemDefined = SystemDefined False
                    definition = TrackFunction @b qf
                    metadataObject =
                      MetadataObject
                        ( MOSourceObjId sourceName $
                            AB.mkAnyBackend $
                              SMOFunction @b qf
                        )
                        (toJSON definition)
                    schemaObject =
                      SOSourceObj sourceName $
                        AB.mkAnyBackend $
                          SOIFunction @b qf
                    addFunctionContext e = "in function " <> qf <<> ": " <> e
                    metadataPermissions = mapFromL _fpmRole functionPermissions
                    permissionsMap = mkBooleanPermissionMap FunctionPermissionInfo metadataPermissions orderedRoles
                withRecordInconsistencyM metadataObject $ modifyErr addFunctionContext do
                  funcDefs <-
                    onNothing
                      (M.lookup qf dbFunctions)
                      (throw400 NotExists $ "no such function exists: " <>> qf)

                  rawfunctionInfo <- getSingleUniqueFunctionOverload @b qf funcDefs
                  (functionInfo, dep) <- buildFunctionInfo sourceName qf systemDefined config permissionsMap rawfunctionInfo comment namingConv
                  recordDependenciesM metadataObject schemaObject (Seq.singleton dep)
                  pure functionInfo

      let functionCache = mapFromL _fiSQLName $ catMaybes functionCacheMaybes

      let mkCustomReturnTypeMetadataObject :: CustomReturnTypeMetadata b -> MetadataObject
          mkCustomReturnTypeMetadataObject crtm =
            ( MetadataObject
                ( MOSourceObjId sourceName $
                    AB.mkAnyBackend $
                      SMOCustomReturnType @b (_crtmName crtm)
                )
                (toJSON crtm)
            )

      customReturnTypeCacheMaybes <-
        interpretWriter
          -< for
            (OMap.elems customReturnTypes)
            \crtm@CustomReturnTypeMetadata {..} ->
              withRecordInconsistencyM (mkCustomReturnTypeMetadataObject crtm) $ do
                unless (_cdcAreNativeQueriesEnabled dynamicConfig) $
                  throw400 InvalidConfiguration "The Custom Return Type feature is disabled"

                fieldInfoMap <- case toFieldInfo _crtmFields of
                  Nothing -> pure mempty
                  Just fields -> pure (mapFromL fieldInfoName fields)

                customTypePermissions <-
                  buildCustomReturnTypePermissions sourceName tableCoreInfos _crtmName fieldInfoMap _crtmSelectPermissions orderedRoles

                pure
                  CustomReturnTypeInfo
                    { _crtiName = _crtmName,
                      _crtiFields = _crtmFields,
                      _crtiPermissions = customTypePermissions,
                      _crtiDescription = _crtmDescription
                    }

      let customReturnTypesCache :: CustomReturnTypeCache b
          customReturnTypesCache = mapFromL _crtiName (catMaybes customReturnTypeCacheMaybes)

      nativeQueryCacheMaybes <-
        interpretWriter
          -< for
            (OMap.elems nativeQueries)
            \nqm@NativeQueryMetadata {..} -> do
              let metadataObject :: MetadataObject
                  metadataObject =
                    MetadataObject
                      ( MOSourceObjId sourceName $
                          AB.mkAnyBackend $
                            SMONativeQuery @b _nqmRootFieldName
                      )
                      (toJSON nqm)

                  schemaObjId :: SchemaObjId
                  schemaObjId =
                    SOSourceObj sourceName $
                      AB.mkAnyBackend $
                        SOINativeQuery @b _nqmRootFieldName

                  dependency :: SchemaDependency
                  dependency =
                    SchemaDependency
                      { sdObjId =
                          SOSourceObj sourceName $
                            AB.mkAnyBackend $
                              SOICustomReturnType @b _nqmReturns,
                        sdReason = DRCustomReturnType
                      }

              withRecordInconsistencyM metadataObject $ do
                unless (_cdcAreNativeQueriesEnabled dynamicConfig) $
                  throw400 InvalidConfiguration "The Native Queries feature is disabled"

                customReturnType <-
                  onNothing
                    (M.lookup _nqmReturns customReturnTypesCache)
                    (throw400 InvalidConfiguration ("The custom return type " <> toTxt _nqmReturns <> " could not be found"))

                recordDependenciesM metadataObject schemaObjId $
                  Seq.singleton dependency

                pure
                  NativeQueryInfo
                    { _nqiRootFieldName = _nqmRootFieldName,
                      _nqiCode = _nqmCode,
                      _nqiReturns = customReturnType,
                      _nqiArguments = _nqmArguments,
                      _nqiDescription = _nqmDescription
                    }

      let nativeQueryCache :: NativeQueryCache b
          nativeQueryCache = mapFromL _nqiRootFieldName (catMaybes nativeQueryCacheMaybes)

      returnA -< SourceInfo sourceName tableCache functionCache nativeQueryCache customReturnTypesCache sourceConfig queryTagsConfig resolvedCustomization

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
        ProvidesNetwork m,
        MonadResolveSource m,
        HasCacheStaticConfig m
      ) =>
      (CacheDynamicConfig, Inc.Dependency Metadata, Inc.Dependency InvalidationKeys, Maybe StoredIntrospection) `arr` BuildOutputs
    buildAndCollectInfo = proc (dynamicConfig, metadataDep, invalidationKeys, storedIntrospection) -> do
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
                  AB.dispatchAnyBackend @Backend e \(SourceMetadata _ _ tables _functions _nativeQueries _customReturnTypes _ _ _ _) -> do
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

      resolvedInheritedRoles <- interpretWriter -< buildInheritedRoles allRoleNames (OMap.elems inheritedRoles)

      let allRoles = resolvedInheritedRoles `M.union` metadataRoles

      orderedRoles <- bindA -< orderRoles $ M.elems allRoles

      -- remote schemas
      let remoteSchemaInvalidationKeys = Inc.selectD #_ikRemoteSchemas invalidationKeys
      remoteSchemaMap <- buildRemoteSchemas env -< ((remoteSchemaInvalidationKeys, orderedRoles, fmap encJToLBS . siRemotes <$> storedIntrospection), OMap.elems remoteSchemas)
      let remoteSchemaCtxMap = M.map fst remoteSchemaMap
          !defaultNC = _cdcDefaultNamingConvention dynamicConfig
          !isNamingConventionEnabled = EFNamingConventions `elem` (_cdcExperimentalFeatures dynamicConfig)

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
                  ( proc (backendInfoAndSourceMetadata :: BackendInfoAndSourceMetadata b, (dynamicConfig, invalidationKeys, storedIntrospection, defaultNC, isNamingConventionEnabled)) -> do
                      let sourceMetadata = _bcasmSourceMetadata backendInfoAndSourceMetadata
                          sourceName = _smName sourceMetadata
                          sourceInvalidationsKeys = Inc.selectD #_ikSources invalidationKeys
                          sourceIntrospection = AB.unpackAnyBackend @b =<< M.lookup sourceName =<< siBackendIntrospection <$> storedIntrospection
                      maybeResolvedSource <- tryResolveSource -< (sourceInvalidationsKeys, sourceIntrospection, backendInfoAndSourceMetadata)
                      case maybeResolvedSource of
                        Nothing -> returnA -< Nothing
                        Just (sourceConfig, source) -> do
                          let metadataInvalidationKey = Inc.selectD #_ikMetadata invalidationKeys
                              (tableInputs, _, _) = unzip3 $ map mkTableInputs $ OMap.elems $ _smTables sourceMetadata
                              scNamingConvention = _scNamingConvention $ _smCustomization sourceMetadata
                              !namingConv = if isNamingConventionEnabled then fromMaybe defaultNC scNamingConvention else HasuraCase
                          tablesCoreInfo <-
                            buildTableCache
                              -<
                                ( sourceName,
                                  sourceConfig,
                                  _rsTables source,
                                  tableInputs,
                                  metadataInvalidationKey,
                                  sourceIntrospection,
                                  namingConv
                                )

                          let tablesMetadata = OMap.elems $ _smTables sourceMetadata
                              eventTriggers = map (_tmTable &&& OMap.elems . _tmEventTriggers) tablesMetadata
                              numEventTriggers = sum $ map (length . snd) eventTriggers

                          (recreateEventTriggers, sourceCatalogMigrationState) <- initCatalogIfNeeded -< (Proxy :: Proxy b, numEventTriggers > 0, sourceConfig)

                          bindA -< unLogger logger (sourceName, sourceCatalogMigrationState)

                          let alignTableMap :: HashMap (TableName b) a -> HashMap (TableName b) c -> HashMap (TableName b) (a, c)
                              alignTableMap = M.intersectionWith (,)

                          eventTriggerInfoMaps <-
                            (|
                              Inc.keyed
                                ( \_ (tableCoreInfo, (_, eventTriggerConfs)) ->
                                    buildTableEventTriggers -< (dynamicConfig, sourceName, sourceConfig, tableCoreInfo, eventTriggerConfs, metadataInvalidationKey, recreateEventTriggers)
                                )
                              |) (tablesCoreInfo `alignTableMap` mapFromL fst eventTriggers)

                          returnA
                            -<
                              Just $
                                AB.mkAnyBackend @b $
                                  PartiallyResolvedSource sourceMetadata sourceConfig source tablesCoreInfo eventTriggerInfoMaps
                  )
                  -<
                    (exists, (dynamicConfig, invalidationKeys, storedIntrospection, defaultNC, isNamingConventionEnabled))
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
                        (dynamicConfig, allResolvedSources, remoteSchemaCtxMap, orderedRoles)
                        )
                    -> do
                      let PartiallyResolvedSource sourceMetadata sourceConfig introspection tablesInfo eventTriggers = partiallyResolvedSource
                          DBObjectsIntrospection tablesMeta functionsMeta scalars = introspection
                      so <-
                        Inc.cache buildSource
                          -<
                            ( dynamicConfig,
                              allResolvedSources,
                              sourceMetadata,
                              sourceConfig,
                              tablesInfo,
                              eventTriggers,
                              tablesMeta,
                              functionsMeta,
                              remoteSchemaCtxMap,
                              orderedRoles
                            )
                      returnA -< (AB.mkAnyBackend so, BackendMap.singleton scalars)
                  )
                  -<
                    ( exists,
                      (dynamicConfig, partiallyResolvedSources, remoteSchemaCtxMap, orderedRoles)
                    )
            )
          |) partiallyResolvedSources

      remoteSchemaCache <-
        interpretWriter
          -< for remoteSchemaMap \(partiallyResolvedRemoteSchemaCtx, metadataObj) -> do
            let remoteSchemaIntrospection = irDoc $ _rscIntroOriginal partiallyResolvedRemoteSchemaCtx
            resolvedSchemaCtx <- for partiallyResolvedRemoteSchemaCtx \PartiallyResolvedRemoteRelationship {..} ->
              buildRemoteSchemaRemoteRelationship partiallyResolvedSources remoteSchemaCtxMap (_rscName partiallyResolvedRemoteSchemaCtx) remoteSchemaIntrospection _prrrTypeName _prrrDefinition
            pure $ (catMaybes resolvedSchemaCtx, metadataObj)

      -- actions
      (actionCache, annotatedCustomTypes) <-
        interpretWriter
          -< do
            -- custom types
            let scalarsMap = mconcat $ map snd $ M.elems sourcesOutput
                sourcesCache = M.map fst sourcesOutput
                actionList = OMap.elems actions
            maybeResolvedCustomTypes <-
              withRecordInconsistencyM (MetadataObject MOCustomTypes $ toJSON customTypes) $
                resolveCustomTypes sourcesCache customTypes scalarsMap
            case maybeResolvedCustomTypes of
              Just resolvedCustomTypes -> do
                actionCache' <- buildActions resolvedCustomTypes scalarsMap orderedRoles actionList
                pure (actionCache', resolvedCustomTypes)
              -- If the custom types themselves are inconsistent, we can’t really do
              -- anything with actions, so just mark them all inconsistent.
              Nothing -> do
                recordInconsistenciesM
                  (map mkActionMetadataObject actionList)
                  "custom types are inconsistent"
                pure (mempty, mempty)

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

    buildOpenTelemetry ::
      MonadWriter (Seq (Either InconsistentMetadata md)) m =>
      OpenTelemetryConfig ->
      m OpenTelemetryInfo
    buildOpenTelemetry OpenTelemetryConfig {..} = do
      -- Always perform validation, even if OpenTelemetry is disabled
      mOtelExporterInfo <-
        fmap join $
          withRecordInconsistencyM (MetadataObject (MOOpenTelemetry OtelSubobjectExporterOtlp) (toJSON _ocExporterOtlp)) $
            liftEither $
              parseOtelExporterConfig _ocStatus env _ocExporterOtlp
      mOtelBatchSpanProcessorInfo <-
        withRecordInconsistencyM (MetadataObject (MOOpenTelemetry OtelSubobjectBatchSpanProcessor) (toJSON _ocBatchSpanProcessor)) $
          liftEither $
            parseOtelBatchSpanProcessorConfig _ocBatchSpanProcessor
      pure $
        case _ocStatus of
          OtelDisabled ->
            -- Disable all components if OpenTelemetry export not enabled
            OpenTelemetryInfo Nothing Nothing
          OtelEnabled ->
            OpenTelemetryInfo
              mOtelExporterInfo
              -- Disable data types if they are not in the enabled set
              ( if OtelTraces `S.member` _ocEnabledDataTypes
                  then mOtelBatchSpanProcessorInfo
                  else Nothing
              )

    buildRESTEndpoints ::
      MonadWriter (Seq (Either InconsistentMetadata md)) m =>
      QueryCollections ->
      [CreateEndpoint] ->
      m (HashMap EndpointName (EndpointMetadata GQLQueryWithText))
    buildRESTEndpoints collections endpoints = buildInfoMapM _ceName mkEndpointMetadataObject buildEndpoint endpoints
      where
        mkEndpointMetadataObject createEndpoint@EndpointMetadata {..} =
          let objectId = MOEndpoint _ceName
           in MetadataObject objectId (toJSON createEndpoint)

        buildEndpoint createEndpoint@EndpointMetadata {..} = do
          let -- QueryReference collName queryName = _edQuery endpoint
              addContext err = "in endpoint " <> toTxt _ceName <> ": " <> err
          withRecordInconsistencyM (mkEndpointMetadataObject createEndpoint) $ modifyErr addContext $ resolveEndpoint collections createEndpoint

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
      (CacheDynamicConfig, a, SourceName, c, TableName b, RecreateEventTriggers, EventTriggerConf b) ->
      MetadataObject
    mkEventTriggerMetadataObject (_, _, source, _, table, _, eventTriggerConf) =
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
        BackendMetadata b,
        BackendEventTrigger b,
        HasCacheStaticConfig m
      ) =>
      ( CacheDynamicConfig,
        SourceName,
        SourceConfig b,
        TableCoreInfoG b (ColumnInfo b) (ColumnInfo b),
        [EventTriggerConf b],
        Inc.Dependency Inc.InvalidationKey,
        RecreateEventTriggers
      )
        `arr` (EventTriggerInfoMap b)
    buildTableEventTriggers = proc (dynamicConfig, sourceName, sourceConfig, tableInfo, eventTriggerConfs, metadataInvalidationKey, migrationRecreateEventTriggers) ->
      buildInfoMap (etcName . (^. _7)) (mkEventTriggerMetadataObject @b) buildEventTrigger
        -<
          (tableInfo, map (dynamicConfig,metadataInvalidationKey,sourceName,sourceConfig,_tciName tableInfo,migrationRecreateEventTriggers,) eventTriggerConfs)
      where
        buildEventTrigger = proc (tableInfo, (dynamicConfig, metadataInvalidationKey, source, sourceConfig, table, migrationRecreateEventTriggers, eventTriggerConf)) -> do
          let triggerName = etcName eventTriggerConf
              triggerOnReplication = etcTriggerOnReplication eventTriggerConf
              metadataObject = mkEventTriggerMetadataObject @b (dynamicConfig, metadataInvalidationKey, source, sourceConfig, table, migrationRecreateEventTriggers, eventTriggerConf)
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
              ( do
                  (info, dependencies) <- bindErrorA -< modifyErr (addTableContext @b table . addTriggerContext) $ buildEventTriggerInfo @b env source table eventTriggerConf
                  staticConfig <- bindA -< askCacheStaticConfig
                  let isCatalogUpdate =
                        case buildReason of
                          CatalogUpdate _ -> True
                          CatalogSync -> False
                      tableColumns = M.elems $ _tciFieldInfoMap tableInfo
                  if ( _cscMaintenanceMode staticConfig == MaintenanceModeDisabled
                         && _cscReadOnlyMode staticConfig == ReadOnlyModeDisabled
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
                                (_cdcSQLGenCtx dynamicConfig)
                                sourceConfig
                                table
                                tableColumns
                                triggerName
                                triggerOnReplication
                                (etcDefinition eventTriggerConf)
                                (_tciPrimaryKey tableInfo)
                      if isCatalogUpdate || migrationRecreateEventTriggers == RETRecreate
                        then do
                          recreateTriggerIfNeeded
                            -<
                              ( dynamicConfig,
                                table,
                                tableColumns,
                                triggerName,
                                triggerOnReplication,
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
                                (_cdcSQLGenCtx dynamicConfig)
                                sourceConfig
                                table
                                (tableColumns, _tciPrimaryKey tableInfo)
                                triggerName
                                triggerOnReplication
                                (etcDefinition eventTriggerConf)
                        else returnA -< ()
                    else returnA -< ()
                  recordDependencies -< (metadataObject, schemaObjectId, dependencies)
                  returnA -< info
              )
            |) metadataObject

        recreateTriggerIfNeeded =
          -- using `Inc.cache` here means that the response will be cached for the given output and the
          -- next time this arrow recieves the same input, the cached response will be returned and the
          -- computation will not be done again.
          Inc.cache
            proc
              ( dynamicConfig,
                tableName,
                tableColumns,
                triggerName,
                triggerOnReplication,
                triggerDefinition,
                sourceConfig,
                primaryKey
                )
            -> do
              bindA
                -< do
                  liftEitherM $
                    createTableEventTrigger @b
                      (_cdcSQLGenCtx dynamicConfig)
                      sourceConfig
                      tableName
                      tableColumns
                      triggerName
                      triggerOnReplication
                      triggerDefinition
                      primaryKey

    buildCronTriggers ::
      MonadWriter (Seq (Either InconsistentMetadata md)) m =>
      [CronTriggerMetadata] ->
      m (HashMap TriggerName CronTriggerInfo)
    buildCronTriggers = buildInfoMapM ctName mkCronTriggerMetadataObject buildCronTrigger
      where
        buildCronTrigger cronTrigger = do
          let triggerName = triggerNameToTxt $ ctName cronTrigger
              addCronTriggerContext e = "in cron trigger " <> triggerName <> ": " <> e
          withRecordInconsistencyM (mkCronTriggerMetadataObject cronTrigger) $
            modifyErr addCronTriggerContext $
              resolveCronTrigger env cronTrigger

    buildInheritedRoles ::
      MonadWriter (Seq (Either InconsistentMetadata MetadataDependency)) m =>
      HashSet RoleName ->
      [InheritedRole] ->
      m (HashMap RoleName Role)
    buildInheritedRoles allRoles = buildInfoMapM _rRoleName mkInheritedRoleMetadataObject buildInheritedRole
      where
        buildInheritedRole inheritedRole = do
          let addInheritedRoleContext e = "in inherited role " <> roleNameToTxt (_rRoleName inheritedRole) <> ": " <> e
              metadataObject = mkInheritedRoleMetadataObject inheritedRole
              schemaObject = SORole $ _rRoleName inheritedRole
          withRecordInconsistencyM metadataObject $ modifyErr addInheritedRoleContext do
            (resolvedInheritedRole, dependencies) <- resolveInheritedRole allRoles inheritedRole
            recordDependenciesM metadataObject schemaObject dependencies
            pure resolvedInheritedRole

    buildActions ::
      MonadWriter (Seq (Either InconsistentMetadata MetadataDependency)) m =>
      AnnotatedCustomTypes ->
      BackendMap ScalarMap ->
      OrderedRoles ->
      [ActionMetadata] ->
      m (HashMap ActionName ActionInfo)
    buildActions resolvedCustomTypes scalarsMap orderedRoles = buildInfoMapM _amName mkActionMetadataObject buildAction
      where
        buildAction action@(ActionMetadata name comment def actionPermissions) = do
          let addActionContext e = "in action " <> name <<> "; " <> e
              permissionInfos = map (ActionPermissionInfo . _apmRole) actionPermissions
              metadataPermissionMap = mapFromL _apiRole permissionInfos
              permissionsMap = mkBooleanPermissionMap ActionPermissionInfo metadataPermissionMap orderedRoles
              outputType = unGraphQLType $ _adOutputType def
          withRecordInconsistencyM (mkActionMetadataObject action) $ modifyErr addActionContext do
            (resolvedDef, outObject) <- resolveAction env resolvedCustomTypes def scalarsMap
            let forwardClientHeaders = _adForwardClientHeaders resolvedDef
            return $ ActionInfo name (outputType, outObject) resolvedDef permissionsMap forwardClientHeaders comment

buildRemoteSchemaRemoteRelationship ::
  MonadWriter (Seq (Either InconsistentMetadata MetadataDependency)) m =>
  HashMap SourceName (AB.AnyBackend PartiallyResolvedSource) ->
  PartiallyResolvedRemoteSchemaMap ->
  RemoteSchemaName ->
  RemoteSchemaIntrospection ->
  G.Name ->
  RemoteRelationship ->
  m (Maybe (RemoteFieldInfo G.Name))
buildRemoteSchemaRemoteRelationship allSources remoteSchemaMap remoteSchema remoteSchemaIntrospection typeName rr@RemoteRelationship {..} = do
  let metadataObject =
        MetadataObject (MORemoteSchemaRemoteRelationship remoteSchema typeName _rrName) $
          toJSON $
            CreateRemoteSchemaRemoteRelationship remoteSchema typeName _rrName _rrDefinition
      schemaObj = SORemoteSchemaRemoteRelationship remoteSchema typeName _rrName
      addRemoteRelationshipContext e = "in remote relationship " <> _rrName <<> ": " <> e
      -- buildRemoteFieldInfo only knows how to construct dependencies on the RHS of the join condition,
      -- so the dependencies on the remote relationship on the LHS entity have to be computed here
      lhsDependency =
        -- a direct dependency on the remote schema on which this is defined
        SchemaDependency (SORemoteSchema remoteSchema) DRRemoteRelationship
  withRecordInconsistencyM metadataObject $ modifyErr addRemoteRelationshipContext do
    allowedLHSJoinFields <- getRemoteSchemaEntityJoinColumns remoteSchema remoteSchemaIntrospection typeName
    (remoteField, rhsDependencies) <-
      buildRemoteFieldInfo (remoteSchemaToLHSIdentifier remoteSchema) allowedLHSJoinFields rr allSources remoteSchemaMap
    recordDependenciesM metadataObject schemaObj (lhsDependency Seq.:<| rhsDependencies)
    pure remoteField

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
