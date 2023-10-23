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
    CacheRWT,
    runCacheRWT,
    mkBooleanPermissionMap,
    saveSourcesIntrospection,
  )
where

import Control.Arrow.Extended
import Control.Arrow.Interpret
import Control.Lens hiding ((.=))
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Retry qualified as Retry
import Data.Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Either (isLeft)
import Data.Environment qualified as Env
import Data.Has
import Data.HashMap.Strict.Extended qualified as HashMap
import Data.HashMap.Strict.InsOrd.Extended qualified as InsOrdHashMap
import Data.HashSet qualified as HS
import Data.Proxy
import Data.Sequence qualified as Seq
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.Extended
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.Eventing.Backend
import Hasura.Function.API
import Hasura.Function.Cache
import Hasura.Function.Metadata (FunctionMetadata (..))
import Hasura.GraphQL.Schema (buildGQLContext)
import Hasura.GraphQL.Schema.Common
import Hasura.Incremental qualified as Inc
import Hasura.Logging
import Hasura.LogicalModel.Cache (LogicalModelCache, LogicalModelInfo (..))
import Hasura.LogicalModel.Fields (runLogicalModelFieldsLookup)
import Hasura.LogicalModel.Metadata (LogicalModelMetadata (..))
import Hasura.LogicalModel.Types (LogicalModelField (..), LogicalModelLocation (..), LogicalModelName (..), LogicalModelType (..), LogicalModelTypeArray (..), LogicalModelTypeReference (..))
import Hasura.LogicalModelResolver.Lenses (ilmmSelectPermissions, _LMIInlineLogicalModel)
import Hasura.LogicalModelResolver.Metadata (InlineLogicalModelMetadata (..), LogicalModelIdentifier (..))
import Hasura.Metadata.Class
import Hasura.NativeQuery.Cache (NativeQueryCache, NativeQueryInfo (..))
import Hasura.NativeQuery.Lenses (nqmReturns)
import Hasura.NativeQuery.Metadata (NativeQueryMetadata (..), getNativeQueryName)
import Hasura.Prelude
import Hasura.QueryTags
import Hasura.RQL.DDL.Action
import Hasura.RQL.DDL.CustomTypes
import Hasura.RQL.DDL.EventTrigger (MonadEventLogCleanup (..), buildEventTriggerInfo)
import Hasura.RQL.DDL.InheritedRoles (resolveInheritedRole)
import Hasura.RQL.DDL.OpenTelemetry (parseOtelBatchSpanProcessorConfig, parseOtelExporterConfig)
import Hasura.RQL.DDL.Relationship
import Hasura.RQL.DDL.RemoteRelationship (CreateRemoteSchemaRemoteRelationship (..), PartiallyResolvedSource (..), buildRemoteFieldInfo, getRemoteSchemaEntityJoinColumns)
import Hasura.RQL.DDL.ScheduledTrigger
import Hasura.RQL.DDL.Schema.Cache.Common
import Hasura.RQL.DDL.Schema.Cache.Config
import Hasura.RQL.DDL.Schema.Cache.Dependencies
import Hasura.RQL.DDL.Schema.Cache.Fields
import Hasura.RQL.DDL.Schema.Cache.Permission
import Hasura.RQL.DDL.SchemaRegistry
import Hasura.RQL.Types.Action
import Hasura.RQL.Types.Allowlist
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendTag
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.CustomTypes
import Hasura.RQL.Types.Endpoint
import Hasura.RQL.Types.EventTrigger
import Hasura.RQL.Types.Metadata
import Hasura.RQL.Types.Metadata.Backend
import Hasura.RQL.Types.Metadata.Object
import Hasura.RQL.Types.NamingCase
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
import Hasura.RemoteSchema.Metadata
import Hasura.RemoteSchema.SchemaCache
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.SQL.BackendMap (BackendMap)
import Hasura.SQL.BackendMap qualified as BackendMap
import Hasura.Server.Init.FeatureFlag qualified as FF
import Hasura.Server.Migrate.Version
import Hasura.Server.Types
import Hasura.Services
import Hasura.Session
import Hasura.StoredProcedure.Cache (StoredProcedureCache, StoredProcedureInfo (..))
import Hasura.StoredProcedure.Metadata (StoredProcedureMetadata (..))
import Hasura.Table.API
import Hasura.Table.Cache
import Hasura.Table.Metadata (TableMetadata (..))
import Hasura.Tracing qualified as Tracing
import Language.GraphQL.Draft.Syntax qualified as G
import Network.Types.Extended

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
  Maybe SchemaRegistryContext ->
  CacheBuild RebuildableSchemaCache
buildRebuildableSchemaCache logger env metadataWithVersion dynamicConfig mSchemaRegistryContext = do
  result <-
    flip runReaderT CatalogSync
      $ Inc.build (buildSchemaCacheRule logger env mSchemaRegistryContext) (metadataWithVersion, dynamicConfig, initialInvalidationKeys, Nothing)

  pure $ RebuildableSchemaCache (fst $ Inc.result result) initialInvalidationKeys (Inc.rebuildRule result)

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
    CacheRWT (ReaderT CacheDynamicConfig (StateT (RebuildableSchemaCache, CacheInvalidations, SourcesIntrospectionStatus, SchemaRegistryAction) m) a)
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadError e,
      UserInfoM,
      MonadMetadataStorage,
      Tracing.MonadTrace,
      Tracing.MonadTraceContext,
      MonadBase b,
      MonadBaseControl b,
      ProvidesNetwork
    )
  deriving anyclass (MonadQueryTags)

-- | Since 'CacheRWT' runs in a context where we have sampled the feature flags
-- we intentionally use those for 'HasFeatureFlagChecker', so that we always give
-- coherent results consistent with the 'CacheDynamicConfig'.
instance (Monad m) => FF.HasFeatureFlagChecker (CacheRWT m) where
  checkFlag ff = do
    ffs <- CacheRWT $ asks _cdcSchemaSampledFeatureFlags
    withSchemaSampledFeatureFlags ffs (FF.checkFlag ff)

instance (MonadReader r m) => MonadReader r (CacheRWT m) where
  ask = lift ask
  local f (CacheRWT m) = CacheRWT $ mapReaderT (local f) m

instance (MonadEventLogCleanup m) => MonadEventLogCleanup (CacheRWT m) where
  runLogCleaner sourceCache conf = lift $ runLogCleaner sourceCache conf
  generateCleanupSchedules sourceInfo triggerName cleanupConfig = lift $ generateCleanupSchedules sourceInfo triggerName cleanupConfig
  updateTriggerCleanupSchedules logger oldSources newSources schemaCache = lift $ updateTriggerCleanupSchedules logger oldSources newSources schemaCache

instance (MonadGetPolicies m) => MonadGetPolicies (CacheRWT m) where
  runGetApiTimeLimit = lift $ runGetApiTimeLimit
  runGetPrometheusMetricsGranularity = lift $ runGetPrometheusMetricsGranularity
  runGetModelInfoLogStatus = lift $ runGetModelInfoLogStatus

runCacheRWT ::
  (Monad m) =>
  CacheDynamicConfig ->
  RebuildableSchemaCache ->
  CacheRWT m a ->
  m (a, RebuildableSchemaCache, CacheInvalidations, SourcesIntrospectionStatus, SchemaRegistryAction)
runCacheRWT config cache (CacheRWT m) = do
  (v, (newCache, invalidations, introspection, schemaRegistryAction)) <-
    runStateT (runReaderT m config) (cache, mempty, SourcesIntrospectionUnchanged, Nothing)
  pure (v, newCache, invalidations, introspection, schemaRegistryAction)

instance MonadTrans CacheRWT where
  lift = CacheRWT . lift . lift

instance (Monad m) => CacheRM (CacheRWT m) where
  askSchemaCache = CacheRWT $ gets (lastBuiltSchemaCache . (^. _1))

-- | Note: Use these functions over 'fetchSourceIntrospection' and
-- 'storeSourceIntrospection' from 'MonadMetadataStorage' class.
-- These are wrapper function over 'MonadMetadataStorage' methods. These functions
-- handles errors, if any, logs them and returns empty stored introspection.
-- This is to ensure we do not accidentally throw errors (related to
-- fetching/storing stored introspection) in the critical code path of building
-- the 'SchemaCache'.
loadStoredIntrospection ::
  (Tracing.MonadTraceContext m, MonadMetadataStorage m, MonadIO m) =>
  Logger Hasura ->
  MetadataResourceVersion ->
  m (Maybe StoredIntrospection)
loadStoredIntrospection logger metadataVersion = do
  fetchSourceIntrospection metadataVersion `onLeftM` \err -> do
    unLoggerTracing logger
      $ StoredIntrospectionStorageLog "Could not load stored-introspection. Continuing without it" err
    pure Nothing

saveSourcesIntrospection ::
  (Tracing.MonadTraceContext m, MonadIO m, MonadMetadataStorage m) =>
  Logger Hasura ->
  SourcesIntrospectionStatus ->
  MetadataResourceVersion ->
  m ()
saveSourcesIntrospection logger sourcesIntrospection metadataVersion = do
  -- store the collected source introspection result only if we were able
  -- to introspect all sources successfully
  case sourcesIntrospection of
    SourcesIntrospectionUnchanged -> pure ()
    SourcesIntrospectionChangedPartial _ -> pure ()
    SourcesIntrospectionChangedFull introspection ->
      storeSourceIntrospection introspection metadataVersion `onLeftM` \err ->
        unLoggerTracing logger $ StoredIntrospectionStorageLog "Could not save source introspection" err

instance
  ( Tracing.MonadTraceContext m,
    MonadIO m,
    MonadError QErr m,
    ProvidesNetwork m,
    MonadResolveSource m,
    HasCacheStaticConfig m,
    MonadMetadataStorage m
  ) =>
  CacheRWM (CacheRWT m)
  where
  tryBuildSchemaCacheWithOptions buildReason invalidations newMetadata metadataResourceVersion validateNewSchemaCache = CacheRWT do
    dynamicConfig <- ask
    staticConfig <- askCacheStaticConfig
    (RebuildableSchemaCache lastBuiltSC invalidationKeys rule, oldInvalidations, _, _) <- get
    let oldMetadataVersion = scMetadataResourceVersion lastBuiltSC
        -- We are purposely putting (-1) as the metadata resource version here. This is because we want to
        -- catch error cases in `withSchemaCache(Read)Update`
        metadataWithVersion = MetadataWithResourceVersion newMetadata $ fromMaybe (MetadataResourceVersion (-1)) metadataResourceVersion
        newInvalidationKeys = invalidateKeys invalidations invalidationKeys
    storedIntrospection <- loadStoredIntrospection (_cscLogger staticConfig) oldMetadataVersion
    result <-
      runCacheBuildM
        $ flip runReaderT buildReason
        $ Inc.build rule (metadataWithVersion, dynamicConfig, newInvalidationKeys, storedIntrospection)

    let (schemaCache, (storedIntrospectionStatus, schemaRegistryAction)) = Inc.result result
        prunedInvalidationKeys = pruneInvalidationKeys schemaCache newInvalidationKeys
        !newCache = RebuildableSchemaCache schemaCache prunedInvalidationKeys (Inc.rebuildRule result)
        !newInvalidations = oldInvalidations <> invalidations

    case validateNewSchemaCache lastBuiltSC schemaCache of
      (KeepNewSchemaCache, valueToReturn) -> put (newCache, newInvalidations, storedIntrospectionStatus, schemaRegistryAction) >> pure valueToReturn
      (DiscardNewSchemaCache, valueToReturn) -> pure valueToReturn
    where
      -- Prunes invalidation keys that no longer exist in the schema to avoid leaking memory by
      -- hanging onto unnecessary keys.
      pruneInvalidationKeys schemaCache = over ikRemoteSchemas $ HashMap.filterWithKey \name _ ->
        -- see Note [Keep invalidation keys for inconsistent objects]
        name `elem` getAllRemoteSchemas schemaCache

  setMetadataResourceVersionInSchemaCache resourceVersion = CacheRWT $ do
    (rebuildableSchemaCache, invalidations, introspection, schemaRegistryAction) <- get
    put
      ( rebuildableSchemaCache
          { lastBuiltSchemaCache =
              (lastBuiltSchemaCache rebuildableSchemaCache)
                { scMetadataResourceVersion = resourceVersion
                }
          },
        invalidations,
        introspection,
        schemaRegistryAction
      )

-- | Generate health checks related cache from sources metadata
buildHealthCheckCache :: Sources -> SourceHealthCheckCache
buildHealthCheckCache sources =
  catMaybes $ HashMap.fromList $ map (second mkSourceHealthCheck) (InsOrdHashMap.toList sources)
  where
    mkSourceHealthCheck :: BackendSourceMetadata -> Maybe BackendSourceHealthCheckInfo
    mkSourceHealthCheck (BackendSourceMetadata sourceMetadata) =
      AB.traverseBackend @Backend sourceMetadata mkSourceHealthCheckBackend

    -- 'Nothing' when no health check is defined. See:
    -- https://hasura.io/docs/latest/deployment/health-checks/source-health-check/
    -- We likely choose not to install a default `SELECT 1` health check here,
    -- since we don't want to spam serverless databases.
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
  HashMap.fromList $ map (second mkSourcePing) (InsOrdHashMap.toList sources)
  where
    mkSourcePing :: BackendSourceMetadata -> BackendSourcePingInfo
    mkSourcePing (BackendSourceMetadata sourceMetadata) =
      AB.mapBackend sourceMetadata mkSourcePingBackend

    mkSourcePingBackend :: SourceMetadata b -> SourcePingInfo b
    mkSourcePingBackend sourceMetadata =
      let sourceName = _smName sourceMetadata
          connection = _smConfiguration sourceMetadata
       in SourcePingInfo sourceName connection

partitionCollectedInfo ::
  Seq CollectItem -> ([InconsistentMetadata], [MetadataDependency], [StoredIntrospectionItem])
partitionCollectedInfo =
  let go item = case item of
        CollectInconsistentMetadata inconsistentMetadata ->
          _1 %~ ([inconsistentMetadata] <>)
        CollectMetadataDependency dependency ->
          _2 %~ ([dependency] <>)
        CollectStoredIntrospection storedIntrospection ->
          _3 %~ ([storedIntrospection] <>)
   in foldr go ([], [], []) . toList

buildSourcesIntrospectionStatus ::
  Sources -> RemoteSchemas -> [StoredIntrospectionItem] -> SourcesIntrospectionStatus
buildSourcesIntrospectionStatus sourcesMetadata remoteSchemasMetadata = \case
  [] -> SourcesIntrospectionUnchanged
  items ->
    let go item (sources, remoteSchemas) = case item of
          SourceIntrospectionItem name introspection ->
            (sources <> [(name, introspection)], remoteSchemas)
          RemoteSchemaIntrospectionItem name introspection ->
            (sources, remoteSchemas <> [(name, introspection)])
        (allSources, allRemoteSchemas) = foldr go ([], []) items
        storedIntrospection = StoredIntrospection (HashMap.fromList allSources) (HashMap.fromList allRemoteSchemas)
     in if allSourcesAndRemoteSchemasCollected allSources allRemoteSchemas
          then SourcesIntrospectionChangedFull storedIntrospection
          else SourcesIntrospectionChangedPartial storedIntrospection
  where
    allSourcesAndRemoteSchemasCollected ::
      [(SourceName, sourceIntrospection)] ->
      [(RemoteSchemaName, remoteSchemaIntrospection)] ->
      Bool
    allSourcesAndRemoteSchemasCollected sources remoteSchemas =
      allPresent (map fst sources) sourcesMetadata
        && allPresent (map fst remoteSchemas) remoteSchemasMetadata

    allPresent :: (Hashable a) => [a] -> InsOrdHashMap a b -> Bool
    allPresent list = all (`elem` list) . InsOrdHashMap.keys

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
  Maybe SchemaRegistryContext ->
  (MetadataWithResourceVersion, CacheDynamicConfig, InvalidationKeys, Maybe StoredIntrospection)
    `arr` (SchemaCache, (SourcesIntrospectionStatus, SchemaRegistryAction))
buildSchemaCacheRule logger env mSchemaRegistryContext = proc (MetadataWithResourceVersion metadataNoDefaults interimMetadataResourceVersion, dynamicConfig, invalidationKeys, storedIntrospection) -> do
  invalidationKeysDep <- Inc.newDependency -< invalidationKeys
  let metadataDefaults = _cdcMetadataDefaults dynamicConfig
      metadata@Metadata {..} = overrideMetadataDefaults metadataNoDefaults metadataDefaults
  metadataDep <- Inc.newDependency -< metadata

  (inconsistentObjects, storedIntrospections, (resolvedOutputs, dependencyInconsistentObjects, resolvedDependencies), ((adminIntrospection, gqlContext, gqlContextUnauth, inconsistentRemoteSchemas), (relayContext, relayContextUnauth), schemaRegistryAction)) <-
    Inc.cache buildOutputsAndSchema -< (metadataDep, dynamicConfig, invalidationKeysDep, storedIntrospection)

  let storedIntrospectionStatus = buildSourcesIntrospectionStatus _metaSources _metaRemoteSchemas storedIntrospections
      (resolvedEndpoints, endpointCollectedInfo) = runIdentity $ runWriterT $ buildRESTEndpoints _metaQueryCollections (InsOrdHashMap.elems _metaRestEndpoints)
      (cronTriggersMap, cronTriggersCollectedInfo) = runIdentity $ runWriterT $ buildCronTriggers (InsOrdHashMap.elems _metaCronTriggers)
      (openTelemetryInfo, openTelemetryCollectedInfo) = runIdentity $ runWriterT $ buildOpenTelemetry _metaOpenTelemetryConfig

      duplicateVariables :: EndpointMetadata a -> Bool
      duplicateVariables m = any ((> 1) . length) $ group $ sort $ catMaybes $ splitPath Just (const Nothing) (_ceUrl m)

      endpointObjId :: EndpointMetadata q -> MetadataObjId
      endpointObjId md = MOEndpoint (_ceName md)

      endpointObject :: EndpointMetadata q -> MetadataObject
      endpointObject md = MetadataObject (endpointObjId md) (toJSON $ InsOrdHashMap.lookup (_ceName md) _metaRestEndpoints)

      listedQueryObjects :: (CollectionName, ListedQuery) -> MetadataObject
      listedQueryObjects (cName, lq) = MetadataObject (MOQueryCollectionsQuery cName lq) (toJSON lq)

      --  Cases of urls that generate invalid segments:

      hasInvalidSegments :: EndpointMetadata query -> Bool
      hasInvalidSegments m = any (`elem` ["", ":"]) (splitPath id id (_ceUrl m))

      ceUrlTxt = toTxt . _ceUrl

      endpoints = buildEndpointsTrie (HashMap.elems resolvedEndpoints)

      duplicateF md = DuplicateRestVariables (ceUrlTxt md) (endpointObject md)
      duplicateRestVariables = map duplicateF $ filter duplicateVariables (HashMap.elems resolvedEndpoints)

      invalidF md = InvalidRestSegments (ceUrlTxt md) (endpointObject md)
      invalidRestSegments = map invalidF $ filter hasInvalidSegments (HashMap.elems resolvedEndpoints)

      ambiguousF' ep = MetadataObject (endpointObjId ep) (toJSON ep)
      ambiguousF mds = AmbiguousRestEndpoints (commaSeparated $ map _ceUrl mds) (map ambiguousF' mds)
      ambiguousRestEndpoints = map (ambiguousF . S.elems . snd) $ ambiguousPathsGrouped endpoints

      inlinedAllowlist = inlineAllowlist _metaQueryCollections _metaAllowlist
      globalAllowLists = HS.toList . iaGlobal $ inlinedAllowlist

      -- Endpoints don't generate any dependencies
      (endpointInconsistencies, _, _) = partitionCollectedInfo endpointCollectedInfo

      -- Cron triggers don't generate any dependencies
      (cronTriggersInconsistencies, _, _) = partitionCollectedInfo cronTriggersCollectedInfo

      -- OpenTelemerty doesn't generate any dependencies
      (openTelemetryInconsistencies, _, _) = partitionCollectedInfo openTelemetryCollectedInfo

      inconsistentQueryCollections = getInconsistentQueryCollections adminIntrospection _metaQueryCollections listedQueryObjects endpoints globalAllowLists

  let schemaCache =
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
            -- Please note that we are setting the metadata resource version to the last known metadata resource version
            -- for `CatalogSync` or to an invalid metadata resource version (-1) for `CatalogUpdate`.
            --
            -- For, CatalogUpdate, we update the metadata resource version to the latest value after the metadata
            -- operation is complete (see the usage of `setMetadataResourceVersionInSchemaCache`).
            scMetadataResourceVersion = interimMetadataResourceVersion,
            scSetGraphqlIntrospectionOptions = _metaSetGraphqlIntrospectionOptions,
            scTlsAllowlist = networkTlsAllowlist _metaNetwork,
            scQueryCollections = _metaQueryCollections,
            scBackendCache = _boBackendCache resolvedOutputs,
            scSourceHealthChecks = buildHealthCheckCache _metaSources,
            scSourcePingConfig = buildSourcePingCache _metaSources,
            scOpenTelemetryConfig = openTelemetryInfo
          }

  -- Write the Project Schema information to schema registry service
  _ <-
    bindA
      -< do
        buildReason <- ask
        case buildReason of
          -- If this is a catalog sync then we know for sure that the schema has more chances of being committed as some
          -- other instance of Hasura has already committed the schema. So we can safely write the schema to the registry
          -- service.
          CatalogSync ->
            for_ schemaRegistryAction $ \action -> do
              liftIO $ action interimMetadataResourceVersion (scInconsistentObjs schemaCache) metadata
          -- If this is a metadata event then we cannot be sure that the schema will be committed. So we write the schema
          -- to the registry service only after the schema is committed.
          CatalogUpdate _ -> pure ()

  returnA -< (schemaCache, (storedIntrospectionStatus, schemaRegistryAction))
  where
    -- See Note [Avoiding GraphQL schema rebuilds when changing irrelevant Metadata]
    buildOutputsAndSchema = proc (metadataDep, dynamicConfig, invalidationKeysDep, storedIntrospection) -> do
      (outputs, collectedInfo) <- runWriterA buildAndCollectInfo -< (dynamicConfig, metadataDep, invalidationKeysDep, storedIntrospection)
      let (inconsistentObjects, unresolvedDependencies, storedIntrospections) = partitionCollectedInfo collectedInfo
      out2@(resolvedOutputs, _dependencyInconsistentObjects, _resolvedDependencies) <- resolveDependencies -< (outputs, unresolvedDependencies)
      out3 <-
        bindA
          -< do
            buildGQLContext
              (_cdcSchemaSampledFeatureFlags dynamicConfig)
              (_cdcFunctionPermsCtx dynamicConfig)
              (_cdcRemoteSchemaPermsCtx dynamicConfig)
              (_cdcExperimentalFeatures dynamicConfig)
              (_cdcSQLGenCtx dynamicConfig)
              (_cdcApolloFederationStatus dynamicConfig)
              (_boSources resolvedOutputs)
              (_boRemoteSchemas resolvedOutputs)
              (_boActions resolvedOutputs)
              (_boCustomTypes resolvedOutputs)
              mSchemaRegistryContext
              logger
      returnA -< (inconsistentObjects, storedIntrospections, out2, out3)

    resolveBackendInfo' ::
      forall arr m b.
      ( BackendMetadata b,
        ArrowChoice arr,
        Inc.ArrowCache m arr,
        Inc.ArrowDistribute arr,
        ArrowWriter (Seq CollectItem) arr,
        MonadIO m,
        MonadBaseControl IO m,
        ProvidesNetwork m
      ) =>
      (BackendConfigWrapper b, Inc.Dependency (BackendMap BackendInvalidationKeysWrapper)) `arr` BackendCache
    resolveBackendInfo' = proc (backendConfigWrapper, backendInvalidationMap) -> do
      let backendInvalidationKeys =
            Inc.selectMaybeD #unBackendInvalidationKeysWrapper
              $ BackendMap.lookupD @b backendInvalidationMap
      backendInfo <- resolveBackendInfo @b logger -< (backendInvalidationKeys, unBackendConfigWrapper backendConfigWrapper)
      returnA -< BackendMap.singleton (BackendInfoWrapper @b backendInfo)

    resolveBackendCache ::
      forall arr m.
      ( ArrowChoice arr,
        Inc.ArrowCache m arr,
        Inc.ArrowDistribute arr,
        ArrowWriter (Seq CollectItem) arr,
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
        ArrowWriter (Seq CollectItem) arr,
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
        |)
        metadataObj

    tryResolveSource ::
      forall b arr m.
      ( ArrowChoice arr,
        Inc.ArrowCache m arr,
        ArrowWriter (Seq CollectItem) arr,
        MonadIO m,
        MonadBaseControl IO m,
        MonadResolveSource m,
        ProvidesNetwork m,
        BackendMetadata b
      ) =>
      ( Inc.Dependency (HashMap SourceName Inc.InvalidationKey),
        Maybe LBS.ByteString,
        BackendInfoAndSourceMetadata b,
        SchemaSampledFeatureFlags
      )
        `arr` Maybe (SourceConfig b, DBObjectsIntrospection b)
    tryResolveSource =
      Inc.cache
        proc
          ( invalidationKeys,
            sourceIntrospection,
            BackendInfoAndSourceMetadata {..},
            schemaSampledFeatureFlags
            )
        -> do
          let sourceName = _smName _bcasmSourceMetadata
              metadataObj = MetadataObject (MOSource sourceName) $ toJSON sourceName

          maybeSourceConfig <- tryGetSourceConfig @b -< (invalidationKeys, sourceName, _smConfiguration _bcasmSourceMetadata, _smKind _bcasmSourceMetadata, _bcasmBackendInfo)
          case maybeSourceConfig of
            Nothing -> returnA -< Nothing
            Just sourceConfig -> do
              databaseResponse <- bindA -< withSchemaSampledFeatureFlags schemaSampledFeatureFlags (resolveDatabaseMetadata logger _bcasmSourceMetadata sourceConfig)
              case databaseResponse of
                Right databaseMetadata -> do
                  -- Collect database introspection to persist in the storage
                  tellA -< pure (CollectStoredIntrospection $ SourceIntrospectionItem sourceName $ encJFromJValue databaseMetadata)
                  returnA -< Just (sourceConfig, databaseMetadata)
                Left databaseError ->
                  -- If database exception occurs, try to lookup from stored introspection
                  case sourceIntrospection >>= decode' of
                    Nothing ->
                      -- If no stored introspection exist, re-throw the database exception
                      (| withRecordInconsistency (throwA -< databaseError) |) metadataObj
                    Just storedMetadata -> do
                      let inconsistencyMessage =
                            T.unwords
                              [ "source " <>> sourceName,
                                " is inconsistent because of stale database introspection is used.",
                                "The source couldn't be reached for a fresh introspection",
                                "because we got error: " <> qeError databaseError
                              ]
                      -- Still record inconsistency to notify the user obout the usage of stored stale data
                      recordInconsistencies -< ((Just $ toJSON (qeInternal databaseError), [metadataObj]), inconsistencyMessage)
                      bindA -< unLogger logger $ StoredIntrospectionLog ("Using stored introspection for database source " <>> sourceName) databaseError
                      returnA -< Just (sourceConfig, storedMetadata)

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
        ArrowWriter (Seq CollectItem) arr,
        MonadIO m,
        BackendMetadata b,
        MonadBaseControl IO m,
        HasCacheStaticConfig m
      ) =>
      (Proxy b, [(TableName b, [EventTriggerConf b])], SourceConfig b, SourceName) `arr` (RecreateEventTriggers, SourceCatalogMigrationState)
    initCatalogIfNeeded = Inc.cache proc (Proxy, eventTriggers, sourceConfig, sourceName) -> do
      res <-
        (|
          withRecordInconsistencies
            ( bindErrorA
                -< do
                  if sum (map (length . snd) eventTriggers) > 0
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
            )
          |)
          (concatMap (\(tableName, events) -> map (mkEventTriggerMetadataObject' sourceName tableName) events) eventTriggers)

      case res of
        Nothing ->
          returnA -< (RETDoNothing, SCMSUninitializedSource)
        Just (recreateEventTriggers, catalogMigrationState) -> returnA -< (recreateEventTriggers, catalogMigrationState)

    buildSource ::
      forall b arr m.
      ( ArrowChoice arr,
        ArrowKleisli m arr,
        ArrowWriter (Seq CollectItem) arr,
        MonadError QErr m,
        HasCacheStaticConfig m,
        MonadIO m,
        BackendMetadata b,
        GetAggregationPredicatesDeps b
      ) =>
      ( CacheDynamicConfig,
        HashMap SourceName (AB.AnyBackend PartiallyResolvedSource),
        SourceMetadata b,
        SourceConfig b,
        HashMap (TableName b) (TableCoreInfoG b (StructuredColumnInfo b) (ColumnInfo b)),
        HashMap (TableName b) (EventTriggerInfoMap b),
        DBObjectsIntrospection b,
        PartiallyResolvedRemoteSchemaMap,
        OrderedRoles
      )
        `arr` (SourceInfo b)
    buildSource = proc (dynamicConfig, allSources, sourceMetadata, sourceConfig, tablesRawInfo, eventTriggerInfoMaps, dbObjectsIntrospection, remoteSchemaMap, orderedRoles) -> do
      let DBObjectsIntrospection _dbTables dbFunctions _scalars introspectedLogicalModels = dbObjectsIntrospection
          SourceMetadata sourceName backendSourceKind tables functions nativeQueries storedProcedures logicalModels _ queryTagsConfig sourceCustomization _healthCheckConfig = sourceMetadata
          tablesMetadata = InsOrdHashMap.elems tables
          (_, nonColumnInputs, permissions) = unzip3 $ map mkTableInputs tablesMetadata
          alignTableMap :: HashMap (TableName b) a -> HashMap (TableName b) c -> HashMap (TableName b) (a, c)
          alignTableMap = HashMap.intersectionWith (,)

      -- relationships and computed fields
      let nonColumnsByTable = mapFromL _nctiTable nonColumnInputs
      tableCoreInfos :: HashMap (TableName b) (TableCoreInfo b) <-
        interpretWriter
          -< for (tablesRawInfo `alignTableMap` nonColumnsByTable) \(tableRawInfo, nonColumnInput) -> do
            let columns = _tciFieldInfoMap tableRawInfo
            allFields :: FieldInfoMap (FieldInfo b) <- addNonColumnFields allSources sourceName sourceConfig tablesRawInfo columns remoteSchemaMap dbFunctions nonColumnInput
            pure $ tableRawInfo {_tciFieldInfoMap = allFields}

      -- Combine logical models that come from DB schema introspection with logical models
      -- provided via metadata. If two logical models have the same name the one from metadata is preferred.
      let unifiedLogicalModels = logicalModels <> introspectedLogicalModels
          unifiedLogicalModelsHashMap = InsOrdHashMap.toHashMap unifiedLogicalModels

      -- permissions
      result <-
        interpretWriter
          -< runExceptT
            $ for
              (tableCoreInfos `alignTableMap` mapFromL _tpiTable permissions `alignTableMap` eventTriggerInfoMaps)
              \((tableCoreInfo, permissionInputs), eventTriggerInfos) -> do
                let tableFields = _tciFieldInfoMap tableCoreInfo
                permissionInfos <-
                  buildTablePermissions
                    env
                    sourceName
                    sourceConfig
                    tableCoreInfos
                    (_tciName tableCoreInfo)
                    tableFields
                    permissionInputs
                    orderedRoles
                    unifiedLogicalModelsHashMap
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
            (InsOrdHashMap.elems functions)
            \case
              FunctionMetadata qf config functionPermissions comment -> do
                let systemDefined = SystemDefined False
                    definition = TrackFunction @b qf
                    metadataObject =
                      MetadataObject
                        ( MOSourceObjId sourceName
                            $ AB.mkAnyBackend
                            $ SMOFunction @b qf
                        )
                        (toJSON definition)
                    schemaObject =
                      SOSourceObj sourceName
                        $ AB.mkAnyBackend
                        $ SOIFunction @b qf
                    addFunctionContext e = "in function " <> qf <<> ": " <> e
                    metadataPermissions = mapFromL _fpmRole functionPermissions
                    permissionsMap = mkBooleanPermissionMap FunctionPermissionInfo metadataPermissions orderedRoles
                withRecordInconsistencyM metadataObject $ modifyErr addFunctionContext do
                  funcDefs <-
                    onNothing
                      (HashMap.lookup qf dbFunctions)
                      (throw400 NotExists $ "no such function exists: " <>> qf)

                  rawfunctionInfo <- getSingleUniqueFunctionOverload @b qf funcDefs
                  (functionInfo, dep) <- buildFunctionInfo sourceName qf systemDefined config permissionsMap rawfunctionInfo comment namingConv
                  recordDependenciesM metadataObject schemaObject (Seq.singleton dep)
                  pure functionInfo

      let functionCache = mapFromL _fiSQLName $ catMaybes functionCacheMaybes

      let mkLogicalModelMetadataObject :: LogicalModelMetadata b -> MetadataObject
          mkLogicalModelMetadataObject lmm =
            ( MetadataObject
                ( MOSourceObjId sourceName
                    $ AB.mkAnyBackend
                    $ SMOLogicalModel @b (_lmmName lmm)
                )
                (toJSON lmm)
            )

      -- fetch static config
      cacheStaticConfig <- bindA -< askCacheStaticConfig

      let getLogicalModelTypeDependencies ::
            LogicalModelType b ->
            S.Set LogicalModelName
          getLogicalModelTypeDependencies = \case
            LogicalModelTypeScalar _ -> mempty
            LogicalModelTypeArray (LogicalModelTypeArrayC ltmaArray _) ->
              getLogicalModelTypeDependencies ltmaArray
            LogicalModelTypeReference (LogicalModelTypeReferenceC lmtrr _) -> S.singleton lmtrr

      let logicalModelDepObjects metadataJSON rootLogicalModelLocation logicalModelName =
            let metadataObject =
                  MetadataObject
                    ( MOSourceObjId sourceName
                        $ AB.mkAnyBackend
                        $ SMOLogicalModelObj @b rootLogicalModelLocation
                        $ LMMOReferencedLogicalModel logicalModelName
                    )
                    metadataJSON

                sourceObject :: SchemaObjId
                sourceObject =
                  SOSourceObj sourceName
                    $ AB.mkAnyBackend
                    $ SOILogicalModelObj @b rootLogicalModelLocation
                    $ LMOReferencedLogicalModel logicalModelName
             in (metadataObject, sourceObject)

      logicalModelCacheMaybes <-
        interpretWriter
          -< for
            (InsOrdHashMap.elems unifiedLogicalModels)
            \lmm@LogicalModelMetadata {..} ->
              withRecordInconsistencyM (mkLogicalModelMetadataObject lmm) $ do
                logicalModelPermissions <-
                  runLogicalModelFieldsLookup Hasura.LogicalModel.Metadata._lmmFields unifiedLogicalModelsHashMap
                    $ buildLogicalModelPermissions sourceName sourceConfig tableCoreInfos (LMLLogicalModel _lmmName) _lmmFields _lmmSelectPermissions orderedRoles

                let recordDep (metadataObject, sourceObject) =
                      recordDependenciesM metadataObject sourceObject
                        $ Seq.singleton (SchemaDependency sourceObject DRReferencedLogicalModel)

                -- record a dependency with each Logical Model our types reference
                mapM_
                  (recordDep . logicalModelDepObjects (toJSON lmm) (LMLLogicalModel _lmmName))
                  (concatMap (S.toList . getLogicalModelTypeDependencies . lmfType) _lmmFields)

                pure
                  LogicalModelInfo
                    { _lmiName = _lmmName,
                      _lmiFields = _lmmFields,
                      _lmiPermissions = logicalModelPermissions,
                      _lmiDescription = _lmmDescription
                    }

      let logicalModelsCache :: LogicalModelCache b
          logicalModelsCache = mapFromL _lmiName (catMaybes logicalModelCacheMaybes)

      nativeQueryCacheMaybes <-
        interpretWriterT
          -< for
            (InsOrdHashMap.elems nativeQueries)
            \preValidationNativeQuery@NativeQueryMetadata {_nqmRootFieldName, _nqmReturns, _nqmArguments, _nqmDescription} -> do
              let metadataObject :: MetadataObject
                  metadataObject =
                    MetadataObject
                      ( MOSourceObjId sourceName
                          $ AB.mkAnyBackend
                          $ SMONativeQuery @b _nqmRootFieldName
                      )
                      (toJSON preValidationNativeQuery)

                  schemaObjId :: SchemaObjId
                  schemaObjId =
                    SOSourceObj sourceName
                      $ AB.mkAnyBackend
                      $ SOINativeQuery @b _nqmRootFieldName

                  -- we only have a dependency if we used a named Logical Model
                  maybeDependency :: Maybe SchemaDependency
                  maybeDependency = case _nqmReturns of
                    LMILogicalModelName logicalModelName ->
                      Just
                        $ SchemaDependency
                          { sdObjId =
                              SOSourceObj sourceName
                                $ AB.mkAnyBackend
                                $ SOILogicalModel @b logicalModelName,
                            sdReason = DRLogicalModel
                          }
                    LMIInlineLogicalModel _ -> Nothing

              withRecordInconsistencyM metadataObject $ do
                unless (_cscAreNativeQueriesEnabled cacheStaticConfig (reify $ backendTag @b))
                  $ throw400 InvalidConfiguration "The Native Queries feature is disabled"

                logicalModel <- case _nqmReturns of
                  LMILogicalModelName logicalModelName ->
                    onNothing
                      (HashMap.lookup logicalModelName logicalModelsCache)
                      (throw400 InvalidConfiguration ("The logical model " <> toTxt logicalModelName <> " could not be found"))
                  LMIInlineLogicalModel (InlineLogicalModelMetadata {_ilmmFields, _ilmmSelectPermissions}) -> do
                    logicalModelPermissions <-
                      runLogicalModelFieldsLookup Hasura.LogicalModel.Metadata._lmmFields unifiedLogicalModelsHashMap
                        $ buildLogicalModelPermissions sourceName sourceConfig tableCoreInfos (LMLNativeQuery _nqmRootFieldName) _ilmmFields _ilmmSelectPermissions orderedRoles

                    let recordDep (metadataObject', sourceObject) =
                          recordDependenciesM metadataObject' sourceObject
                            $ Seq.singleton (SchemaDependency sourceObject DRReferencedLogicalModel)

                    -- record a dependency with each Logical Model our types reference
                    mapM_
                      (recordDep . logicalModelDepObjects (toJSON preValidationNativeQuery) (LMLNativeQuery _nqmRootFieldName))
                      (concatMap (S.toList . getLogicalModelTypeDependencies . lmfType) _ilmmFields)

                    pure
                      $ LogicalModelInfo
                        { _lmiName = LogicalModelName (getNativeQueryName _nqmRootFieldName),
                          _lmiFields = _ilmmFields,
                          _lmiDescription = _nqmDescription,
                          _lmiPermissions = logicalModelPermissions
                        }
                nqmCode <- validateNativeQuery @b env sourceName (_smConfiguration sourceMetadata) sourceConfig logicalModel preValidationNativeQuery

                case maybeDependency of
                  Just dependency ->
                    recordDependenciesM metadataObject schemaObjId
                      $ Seq.singleton dependency
                  Nothing -> pure ()

                arrayRelationships <-
                  traverse
                    (nativeQueryRelationshipSetup sourceName _nqmRootFieldName ArrRel)
                    (_nqmArrayRelationships preValidationNativeQuery)

                objectRelationships <-
                  traverse
                    (nativeQueryRelationshipSetup sourceName _nqmRootFieldName ObjRel)
                    (_nqmObjectRelationships preValidationNativeQuery)

                let duplicates =
                      S.intersection
                        (S.fromList $ InsOrdHashMap.keys arrayRelationships)
                        (S.fromList $ InsOrdHashMap.keys objectRelationships)

                -- it is possible to have the same field name in both `array`
                -- and `object`, let's stop that
                unless (S.null duplicates)
                  $ throw400 InvalidConfiguration
                  $ "The native query '"
                  <> toTxt _nqmRootFieldName
                  <> "' has duplicate relationships: "
                  <> T.intercalate "," (toTxt <$> S.toList duplicates)

                let sourceObject =
                      SOSourceObj sourceName
                        $ AB.mkAnyBackend
                        $ SOINativeQuery @b _nqmRootFieldName

                let dependencies =
                      mconcat (snd <$> InsOrdHashMap.elems arrayRelationships)
                        <> mconcat (snd <$> InsOrdHashMap.elems objectRelationships)

                recordDependenciesM metadataObject sourceObject dependencies

                pure
                  NativeQueryInfo
                    { _nqiRootFieldName = _nqmRootFieldName,
                      _nqiCode = nqmCode,
                      _nqiReturns = logicalModel,
                      _nqiArguments = _nqmArguments,
                      _nqiRelationships = fst <$> (arrayRelationships <> objectRelationships),
                      _nqiDescription = _nqmDescription
                    }

      let nativeQueryCache :: NativeQueryCache b
          nativeQueryCache = mapFromL _nqiRootFieldName (catMaybes nativeQueryCacheMaybes)

      storedProcedureCacheMaybes <-
        interpretWriterT
          -< for
            (InsOrdHashMap.elems storedProcedures)
            \spm@StoredProcedureMetadata {..} -> do
              let metadataObject :: MetadataObject
                  metadataObject =
                    MetadataObject
                      ( MOSourceObjId sourceName
                          $ AB.mkAnyBackend
                          $ SMOStoredProcedure @b _spmStoredProcedure
                      )
                      (toJSON spm)

                  schemaObjId :: SchemaObjId
                  schemaObjId =
                    SOSourceObj sourceName
                      $ AB.mkAnyBackend
                      $ SOIStoredProcedure @b _spmStoredProcedure

                  dependency :: SchemaDependency
                  dependency =
                    SchemaDependency
                      { sdObjId =
                          SOSourceObj sourceName
                            $ AB.mkAnyBackend
                            $ SOILogicalModel @b _spmReturns,
                        sdReason = DRLogicalModel
                      }

              withRecordInconsistencyM metadataObject $ do
                unless (_cscAreStoredProceduresEnabled cacheStaticConfig)
                  $ throw400 InvalidConfiguration "The Stored Procedure feature is disabled"

                logicalModel <-
                  onNothing
                    (HashMap.lookup _spmReturns logicalModelsCache)
                    (throw400 InvalidConfiguration ("The logical model " <> toTxt _spmReturns <> " could not be found"))

                validateStoredProcedure @b env (_smConfiguration sourceMetadata) logicalModel spm

                recordDependenciesM metadataObject schemaObjId
                  $ Seq.singleton dependency

                graphqlName <- getStoredProcedureGraphqlName @b _spmStoredProcedure _spmConfig

                pure
                  StoredProcedureInfo
                    { _spiStoredProcedure = _spmStoredProcedure,
                      _spiGraphqlName = graphqlName,
                      _spiConfig = _spmConfig,
                      _spiReturns = logicalModel,
                      _spiArguments = _spmArguments,
                      _spiDescription = _spmDescription
                    }

      let storedProcedureCache :: StoredProcedureCache b
          storedProcedureCache = mapFromL _spiStoredProcedure (catMaybes storedProcedureCacheMaybes)

      returnA -< SourceInfo sourceName backendSourceKind tableCache functionCache nativeQueryCache storedProcedureCache logicalModelsCache sourceConfig queryTagsConfig resolvedCustomization dbObjectsIntrospection

    buildAndCollectInfo ::
      forall arr m.
      ( ArrowChoice arr,
        Inc.ArrowDistribute arr,
        Inc.ArrowCache m arr,
        ArrowWriter (Seq CollectItem) arr,
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
      let actionRoles = map _apmRole . _amPermissions =<< InsOrdHashMap.elems actions
          remoteSchemaRoles = map _rspmRole . _rsmPermissions =<< InsOrdHashMap.elems remoteSchemas
          sourceRoles =
            HS.fromList
              $ InsOrdHashMap.elems sources
              >>= \(BackendSourceMetadata e) ->
                AB.dispatchAnyBackend @Backend e \(SourceMetadata _ _ tables _functions nativeQueries _storedProcedures logicalModels _ _ _ _) ->
                  let tableRoles = do
                        table <- InsOrdHashMap.elems tables
                        InsOrdHashMap.keys (_tmInsertPermissions table)
                          <> InsOrdHashMap.keys (_tmSelectPermissions table)
                          <> InsOrdHashMap.keys (_tmUpdatePermissions table)
                          <> InsOrdHashMap.keys (_tmDeletePermissions table)

                      logicalModelRoles = do
                        logicalModel <- InsOrdHashMap.elems logicalModels
                        InsOrdHashMap.keys (_lmmSelectPermissions logicalModel)

                      nativeQueryRoles = do
                        nativeQuery <- InsOrdHashMap.elems nativeQueries
                        concat . maybeToList $ InsOrdHashMap.keys <$> nativeQuery ^? nqmReturns . _LMIInlineLogicalModel . ilmmSelectPermissions
                   in tableRoles <> logicalModelRoles <> nativeQueryRoles
          inheritedRoleNames = InsOrdHashMap.keys inheritedRoles
          allRoleNames = sourceRoles <> HS.fromList (remoteSchemaRoles <> actionRoles <> inheritedRoleNames)

      -- roles which have some kind of permission (action/remote schema/table/function) set in the metadata
      let metadataRoles = mapFromL _rRoleName $ (`Role` ParentRoles mempty) <$> toList allRoleNames

      resolvedInheritedRoles <- interpretWriter -< buildInheritedRoles allRoleNames (InsOrdHashMap.elems inheritedRoles)

      let allRoles = resolvedInheritedRoles `HashMap.union` metadataRoles

      orderedRoles <- bindA -< orderRoles $ HashMap.elems allRoles

      -- remote schemas
      let remoteSchemaInvalidationKeys = Inc.selectD #_ikRemoteSchemas invalidationKeys
      remoteSchemaMap <-
        buildRemoteSchemas logger env
          -<
            ((remoteSchemaInvalidationKeys, orderedRoles, fmap encJToLBS . siRemotes <$> storedIntrospection, _cdcSchemaSampledFeatureFlags dynamicConfig), InsOrdHashMap.elems remoteSchemas)
      let remoteSchemaCtxMap = HashMap.map fst remoteSchemaMap
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
                          sourceIntrospection = HashMap.lookup sourceName =<< siBackendIntrospection <$> storedIntrospection
                      maybeResolvedSource <-
                        tryResolveSource
                          -<
                            ( sourceInvalidationsKeys,
                              encJToLBS <$> sourceIntrospection,
                              backendInfoAndSourceMetadata,
                              _cdcSchemaSampledFeatureFlags dynamicConfig
                            )
                      case maybeResolvedSource of
                        Nothing -> returnA -< Nothing
                        Just (sourceConfig, source) -> do
                          let metadataInvalidationKey = Inc.selectD #_ikMetadata invalidationKeys
                              (tableInputs, _, _) = unzip3 $ map mkTableInputs $ InsOrdHashMap.elems $ _smTables sourceMetadata
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
                                  namingConv,
                                  _smLogicalModels sourceMetadata
                                )

                          let tablesMetadata = InsOrdHashMap.elems $ _smTables sourceMetadata
                              eventTriggers = map (_tmTable &&& InsOrdHashMap.elems . _tmEventTriggers) tablesMetadata

                          (recreateEventTriggers, sourceCatalogMigrationState) <- initCatalogIfNeeded -< (Proxy :: Proxy b, eventTriggers, sourceConfig, sourceName)

                          bindA -< unLogger logger (sourceName, sourceCatalogMigrationState)

                          let alignTableMap :: HashMap (TableName b) a -> HashMap (TableName b) c -> HashMap (TableName b) (a, c)
                              alignTableMap = HashMap.intersectionWith (,)

                          eventTriggerInfoMaps <-
                            (|
                              Inc.keyed
                                ( \_ (tableCoreInfo, (_, eventTriggerConfs)) ->
                                    buildTableEventTriggers -< (dynamicConfig, sourceName, sourceConfig, tableCoreInfo, eventTriggerConfs, metadataInvalidationKey, recreateEventTriggers)
                                )
                              |)
                              (tablesCoreInfo `alignTableMap` mapFromL fst eventTriggers)

                          returnA
                            -<
                              Just
                                $ AB.mkAnyBackend @b
                                $ PartiallyResolvedSource sourceMetadata sourceConfig source tablesCoreInfo eventTriggerInfoMaps
                  )
                  -<
                    (exists, (dynamicConfig, invalidationKeys, storedIntrospection, defaultNC, isNamingConventionEnabled))
            )
          |)
          (HashMap.fromList $ InsOrdHashMap.toList backendInfoAndSourceMetadata)
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
                      so <-
                        buildSource
                          -<
                            ( dynamicConfig,
                              allResolvedSources,
                              sourceMetadata,
                              sourceConfig,
                              tablesInfo,
                              eventTriggers,
                              introspection,
                              remoteSchemaCtxMap,
                              orderedRoles
                            )
                      let scalarParsingContext = getter sourceConfig
                          ScalarMap scalarMap = _rsScalars introspection
                          scalarParsingMap = ScalarParsingMap $ (flip (ScalarWrapper @b) scalarParsingContext) <$> scalarMap
                      returnA -< (AB.mkAnyBackend so, BackendMap.singleton scalarParsingMap)
                  )
                  -<
                    ( exists,
                      (dynamicConfig, partiallyResolvedSources, remoteSchemaCtxMap, orderedRoles)
                    )
            )
          |)
          partiallyResolvedSources

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
            let scalarsMap = mconcat $ map snd $ HashMap.elems sourcesOutput
                sourcesCache = HashMap.map fst sourcesOutput
                actionList = InsOrdHashMap.elems actions
            maybeResolvedCustomTypes <-
              withRecordInconsistencyM (MetadataObject MOCustomTypes $ toJSON customTypes)
                $ resolveCustomTypes sourcesCache customTypes scalarsMap
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
            { _boSources = HashMap.map fst sourcesOutput,
              _boActions = actionCache,
              _boRemoteSchemas = remoteSchemaCache,
              _boCustomTypes = annotatedCustomTypes,
              _boRoles = mapFromL _rRoleName $ _unOrderedRoles orderedRoles,
              _boBackendCache = backendCache
            }

    buildOpenTelemetry ::
      (MonadWriter (Seq CollectItem) m) =>
      OpenTelemetryConfig ->
      m OpenTelemetryInfo
    buildOpenTelemetry OpenTelemetryConfig {..} = do
      -- Always perform validation, even if OpenTelemetry is disabled
      _otiBatchSpanProcessorInfo <-
        withRecordAndDefaultM OtelSubobjectBatchSpanProcessor _ocBatchSpanProcessor defaultOtelBatchSpanProcessorInfo
          $ parseOtelBatchSpanProcessorConfig _ocBatchSpanProcessor
      case _ocStatus of
        OtelDisabled ->
          pure
            $
            -- Disable all components if OpenTelemetry export not enabled
            OpenTelemetryInfo emptyOtelExporterInfo defaultOtelBatchSpanProcessorInfo
        OtelEnabled -> do
          -- Do no validation here if OpenTelemetry is disabled (formerly we
          -- validated headers, but not URIs, but this was not worth the
          -- complexity)
          _otiExporterOtlp <-
            withRecordAndDefaultM OtelSubobjectExporterOtlp _ocExporterOtlp emptyOtelExporterInfo
              $ parseOtelExporterConfig env _ocEnabledDataTypes _ocExporterOtlp
          pure $ OpenTelemetryInfo {..}
      where
        -- record a validation failure and return a default object in case validation fails, so we can continue:
        withRecordAndDefaultM objTy fld dfault =
          fmap (fromMaybe dfault)
            . withRecordInconsistencyM (MetadataObject (MOOpenTelemetry objTy) (toJSON fld))
            . liftEither

    buildRESTEndpoints ::
      (MonadWriter (Seq CollectItem) m) =>
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
      (QErrM m) =>
      InsOrdHashMap CollectionName CreateCollection ->
      EndpointMetadata QueryReference ->
      m (EndpointMetadata GQLQueryWithText)
    resolveEndpoint collections = traverse $ \(QueryReference collName queryName) -> do
      collection <-
        onNothing
          (InsOrdHashMap.lookup collName collections)
          (throw400 NotExists $ "collection with name " <> toTxt collName <> " does not exist")
      listedQuery <-
        flip
          onNothing
          ( throw400 NotExists
              $ "query with name "
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
      (Backend b) =>
      (CacheDynamicConfig, a, SourceName, c, TableName b, RecreateEventTriggers, EventTriggerConf b) ->
      MetadataObject
    mkEventTriggerMetadataObject (_, _, source, _, table, _, eventTriggerConf) =
      mkEventTriggerMetadataObject' source table eventTriggerConf

    mkEventTriggerMetadataObject' ::
      forall b.
      (Backend b) =>
      SourceName ->
      TableName b ->
      EventTriggerConf b ->
      MetadataObject
    mkEventTriggerMetadataObject' source table eventTriggerConf =
      let objectId =
            MOSourceObjId source
              $ AB.mkAnyBackend
              $ SMOTableObj @b table
              $ MTOTrigger
              $ etcName eventTriggerConf
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
        ArrowWriter (Seq CollectItem) arr,
        Inc.ArrowCache m arr,
        MonadIO m,
        MonadBaseControl IO m,
        MonadReader BuildReason m,
        BackendMetadata b,
        BackendEventTrigger b,
        HasCacheStaticConfig m
      ) =>
      ( CacheDynamicConfig,
        SourceName,
        SourceConfig b,
        TableCoreInfoG b (StructuredColumnInfo b) (ColumnInfo b),
        [EventTriggerConf b],
        Inc.Dependency Inc.InvalidationKey,
        RecreateEventTriggers
      )
        `arr` (EventTriggerInfoMap b)
    buildTableEventTriggers = proc (dynamicConfig, sourceName, sourceConfig, tableInfo, eventTriggerConfs, metadataInvalidationKey, migrationRecreateEventTriggers) ->
      buildInfoMap (etcName . (^. _7)) (mkEventTriggerMetadataObject @b) buildEventTrigger
        -<
          (tableInfo & tciFieldInfoMap %~ HashMap.mapMaybe toScalarColumnInfo, map (dynamicConfig,metadataInvalidationKey,sourceName,sourceConfig,_tciName tableInfo,migrationRecreateEventTriggers,) eventTriggerConfs)
      where
        buildEventTrigger = proc (tableInfo, (dynamicConfig, _metadataInvalidationKey, source, sourceConfig, table, migrationRecreateEventTriggers, eventTriggerConf)) -> do
          let triggerName = etcName eventTriggerConf
              triggerOnReplication = etcTriggerOnReplication eventTriggerConf
              metadataObject = mkEventTriggerMetadataObject' @b source table eventTriggerConf
              schemaObjectId =
                SOSourceObj source
                  $ AB.mkAnyBackend
                  $ SOITableObj @b table
                  $ TOTrigger triggerName
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
                  let tableColumns = HashMap.elems $ _tciFieldInfoMap tableInfo
                  if ( _cscMaintenanceMode staticConfig
                         == MaintenanceModeDisabled
                         && _cscReadOnlyMode staticConfig
                         == ReadOnlyModeDisabled
                     )
                    then do
                      bindErrorA
                        -<
                          when (reloadMetadataRecreateEventTrigger == RETRecreate)
                            $
                            -- This is the case when the user sets `recreate_event_triggers`
                            -- to `true` in `reload_metadata`, in this case, we recreate
                            -- the SQL trigger by force, even if it may not be necessary
                            -- TODO: Should we also mark the event trigger as inconsistent here?
                            liftEitherM
                            $ createTableEventTrigger
                              @b
                              (_cdcSQLGenCtx dynamicConfig)
                              sourceConfig
                              table
                              tableColumns
                              triggerName
                              triggerOnReplication
                              (etcDefinition eventTriggerConf)
                              (_tciPrimaryKey tableInfo)
                      recreateTriggerIfNeeded
                        -<
                          ( dynamicConfig,
                            source,
                            table,
                            tableColumns,
                            triggerName,
                            triggerOnReplication,
                            etcDefinition eventTriggerConf,
                            sourceConfig,
                            (_tciPrimaryKey tableInfo)
                          )
                      if migrationRecreateEventTriggers == RETRecreate
                        then do
                          -- We check if the SQL triggers for the event triggers
                          -- are present. If any SQL triggers are missing, those are
                          -- created.
                          bindErrorA
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
            |)
            metadataObject

        recreateTriggerIfNeeded =
          -- using `Inc.cache` here means that the response will be cached for the given output and the
          -- next time this arrow recieves the same input, the cached response will be returned and the
          -- computation will not be done again.
          -- The `buildReason` is passed through the MonadReader because we don't want it to participate
          -- in the caching.

          -- By default, the SQL triggers are not recreated. They can be recreated, under the following conditions:
          -- 1. There is no existing cache present for the given set of arguments and the build reason is not `CatalogSync`.
          --      The build reason `CatalogSync` can be due to either Hasura being start up or the metadata is getting
          --      synced with another Hasura instance that is connected to the same metadata database.
          --
          --      This case includes renaming of event trigger, adding/dropping of table columns etc.
          --
          -- 2. The SQL triggers of a source can be explicitly recreated by specifying the source name
          --    in `reload_metadata`'s `recreate_event_triggers`.
          Inc.cache
            proc
              ( dynamicConfig,
                sourceName,
                tableName,
                tableColumns,
                triggerName,
                triggerOnReplication,
                triggerDefinition,
                sourceConfig,
                primaryKey
                )
            -> do
              buildReason <- bindA -< ask

              let shouldRecreateEventTrigger =
                    case buildReason of
                      -- Build reason is CatalogSync when Hasura is started or when the metadata is being synced.
                      -- We don't want to recreate SQL triggers in such case because in both cases, we expect
                      -- the SQL trigger to already be present.
                      CatalogSync -> False
                      -- Handles the case of a metadata update and `run_sql`
                      CatalogUpdate Nothing -> True
                      -- Handles the case of `reload_metadata` with `recreate_event_triggers` containing
                      -- the current source
                      CatalogUpdate (Just sources) -> sourceName `elem` sources
              bindErrorA
                -< do
                  when shouldRecreateEventTrigger
                    $ liftEitherM
                    $ createTableEventTrigger @b
                      (_cdcSQLGenCtx dynamicConfig)
                      sourceConfig
                      tableName
                      tableColumns
                      triggerName
                      triggerOnReplication
                      triggerDefinition
                      primaryKey

    buildCronTriggers ::
      (MonadWriter (Seq CollectItem) m) =>
      [CronTriggerMetadata] ->
      m (HashMap TriggerName CronTriggerInfo)
    buildCronTriggers = buildInfoMapM ctName mkCronTriggerMetadataObject buildCronTrigger
      where
        buildCronTrigger cronTrigger = do
          let triggerName = triggerNameToTxt $ ctName cronTrigger
              addCronTriggerContext e = "in cron trigger " <> triggerName <> ": " <> e
          withRecordInconsistencyM (mkCronTriggerMetadataObject cronTrigger)
            $ modifyErr addCronTriggerContext
            $ resolveCronTrigger env cronTrigger

    buildInheritedRoles ::
      (MonadWriter (Seq CollectItem) m) =>
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
      (MonadWriter (Seq CollectItem) m) =>
      AnnotatedCustomTypes ->
      BackendMap ScalarParsingMap ->
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
  (MonadWriter (Seq CollectItem) m) =>
  HashMap SourceName (AB.AnyBackend PartiallyResolvedSource) ->
  PartiallyResolvedRemoteSchemaMap ->
  RemoteSchemaName ->
  RemoteSchemaIntrospection ->
  G.Name ->
  RemoteRelationship ->
  m (Maybe (RemoteFieldInfo G.Name))
buildRemoteSchemaRemoteRelationship allSources remoteSchemaMap remoteSchema remoteSchemaIntrospection typeName rr@RemoteRelationship {..} = do
  let metadataObject =
        MetadataObject (MORemoteSchemaRemoteRelationship remoteSchema typeName _rrName)
          $ toJSON
          $ CreateRemoteSchemaRemoteRelationship remoteSchema typeName _rrName _rrDefinition
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
  flip InsOrdHashMap.map sources $ \abSourceMetadata ->
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
