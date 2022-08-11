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
import Control.Arrow.Interpret
import Control.Concurrent.Extended (forConcurrentlyEIO)
import Control.Lens hiding ((.=))
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Retry qualified as Retry
import Data.Aeson
import Data.Align (align)
import Data.Either (isLeft)
import Data.Environment qualified as Env
import Data.HashMap.Strict.Extended qualified as M
import Data.HashMap.Strict.InsOrd.Extended qualified as OMap
import Data.HashSet qualified as HS
import Data.Proxy
import Data.Set qualified as S
import Data.Text.Extended
import Data.These (These (..))
import Hasura.Base.Error
import Hasura.GraphQL.Execute.Types
import Hasura.GraphQL.Schema (buildGQLContext)
import Hasura.GraphQL.Schema.NamingCase
import Hasura.Incremental qualified as Inc
import Hasura.Logging
import Hasura.Metadata.Class
import Hasura.Prelude
import Hasura.RQL.DDL.Action
import Hasura.RQL.DDL.CustomTypes
import Hasura.RQL.DDL.EventTrigger (buildEventTriggerInfo)
import Hasura.RQL.DDL.InheritedRoles (resolveInheritedRole)
import Hasura.RQL.DDL.RemoteRelationship (CreateRemoteSchemaRemoteRelationship (..), PartiallyResolvedSource (..), buildRemoteFieldInfo, getRemoteSchemaEntityJoinColumns)
import Hasura.RQL.DDL.RemoteSchema
import Hasura.RQL.DDL.RemoteSchema.Permission (resolveRoleBasedRemoteSchema)
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
import Hasura.RQL.Types.QueryCollection
import Hasura.RQL.Types.Relationships.Remote
import Hasura.RQL.Types.Relationships.ToSchema
import Hasura.RQL.Types.RemoteSchema
import Hasura.RQL.Types.Roles
import Hasura.RQL.Types.Roles.Internal (CheckPermission (..))
import Hasura.RQL.Types.ScheduledTrigger
import Hasura.RQL.Types.SchemaCache
import Hasura.RQL.Types.SchemaCache.Build
import Hasura.RQL.Types.SchemaCacheTypes
import Hasura.RQL.Types.Source
import Hasura.RQL.Types.SourceCustomization
import Hasura.RQL.Types.Table
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.SQL.Backend
import Hasura.SQL.BackendMap (BackendMap)
import Hasura.SQL.BackendMap qualified as BackendMap
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
buildSchemaCacheRule logger env = proc (metadata, invalidationKeys) -> do
  invalidationKeysDep <- Inc.newDependency -< invalidationKeys

  -- Step 1: Process metadata and collect dependency information.
  (outputs, collectedInfo) <-
    runWriterA buildAndCollectInfo -< (metadata, invalidationKeysDep)
  let (inconsistentObjects, unresolvedDependencies) = partitionCollectedInfo collectedInfo

  -- Step 2: Resolve dependency information and drop dangling dependents.
  (resolvedOutputs, dependencyInconsistentObjects, resolvedDependencies) <-
    resolveDependencies -< (outputs, unresolvedDependencies)

  -- Steps 3 and 4: Build the regular and relay GraphQL schemas in parallel
  [(adminIntrospection, gqlContext, gqlContextUnauth, inconsistentRemoteSchemas), (_, relayContext, relayContextUnauth, _)] <-
    bindA
      -< do
        cxt <- askServerConfigCtx
        forConcurrentlyEIO 1 [QueryHasura, QueryRelay] $ \queryType -> do
          buildGQLContext
            cxt
            queryType
            (_boSources resolvedOutputs)
            (_boRemoteSchemas resolvedOutputs)
            (_boActions resolvedOutputs)
            (_boCustomTypes resolvedOutputs)

  let duplicateVariables :: EndpointMetadata a -> Bool
      duplicateVariables m = any ((> 1) . length) $ group $ sort $ catMaybes $ splitPath Just (const Nothing) (_ceUrl m)

      endpointObjId :: EndpointMetadata q -> MetadataObjId
      endpointObjId md = MOEndpoint (_ceName md)

      endpointObject :: EndpointMetadata q -> MetadataObject
      endpointObject md = MetadataObject (endpointObjId md) (toJSON $ OMap.lookup (_ceName md) $ _metaRestEndpoints metadata)

      listedQueryObjects :: (CollectionName, ListedQuery) -> MetadataObject
      listedQueryObjects (cName, lq) = MetadataObject (MOQueryCollectionsQuery cName lq) (toJSON lq)

      --  Cases of urls that generate invalid segments:

      hasInvalidSegments :: EndpointMetadata query -> Bool
      hasInvalidSegments m = any (`elem` ["", ":"]) (splitPath id id (_ceUrl m))

      ceUrlTxt = toTxt . _ceUrl

      endpoints = buildEndpointsTrie (M.elems $ _boEndpoints resolvedOutputs)

      duplicateF md = DuplicateRestVariables (ceUrlTxt md) (endpointObject md)
      duplicateRestVariables = map duplicateF $ filter duplicateVariables (M.elems $ _boEndpoints resolvedOutputs)

      invalidF md = InvalidRestSegments (ceUrlTxt md) (endpointObject md)
      invalidRestSegments = map invalidF $ filter hasInvalidSegments (M.elems $ _boEndpoints resolvedOutputs)

      ambiguousF' ep = MetadataObject (endpointObjId ep) (toJSON ep)
      ambiguousF mds = AmbiguousRestEndpoints (commaSeparated $ map _ceUrl mds) (map ambiguousF' mds)
      ambiguousRestEndpoints = map (ambiguousF . S.elems . snd) $ ambiguousPathsGrouped endpoints

      queryCollections = _boQueryCollections resolvedOutputs
      allowLists = HS.toList . iaGlobal . _boAllowlist $ resolvedOutputs

  inconsistentQueryCollections <- bindA -< do getInconsistentQueryCollections adminIntrospection queryCollections listedQueryObjects endpoints allowLists

  returnA
    -<
      SchemaCache
        { scSources = _boSources resolvedOutputs,
          scActions = _boActions resolvedOutputs,
          -- TODO this is not the right value: we should track what part of the schema
          -- we can stitch without consistencies, I think.
          scRemoteSchemas = fmap fst (_boRemoteSchemas resolvedOutputs), -- remoteSchemaMap
          scAllowlist = _boAllowlist resolvedOutputs,
          -- , scCustomTypes = _boCustomTypes resolvedOutputs
          scAdminIntrospection = adminIntrospection,
          scGQLContext = gqlContext,
          scUnauthenticatedGQLContext = gqlContextUnauth,
          scRelayContext = relayContext,
          scUnauthenticatedRelayContext = relayContextUnauth,
          -- , scGCtxMap = gqlSchema
          -- , scDefaultRemoteGCtx = remoteGQLSchema
          scDepMap = resolvedDependencies,
          scCronTriggers = _boCronTriggers resolvedOutputs,
          scEndpoints = endpoints,
          scInconsistentObjs =
            inconsistentObjects
              <> dependencyInconsistentObjects
              <> toList inconsistentRemoteSchemas
              <> duplicateRestVariables
              <> invalidRestSegments
              <> ambiguousRestEndpoints
              <> inconsistentQueryCollections,
          scApiLimits = _boApiLimits resolvedOutputs,
          scMetricsConfig = _boMetricsConfig resolvedOutputs,
          scMetadataResourceVersion = Nothing,
          scSetGraphqlIntrospectionOptions = _metaSetGraphqlIntrospectionOptions metadata,
          scTlsAllowlist = _boTlsAllowlist resolvedOutputs,
          scQueryCollections = _boQueryCollections resolvedOutputs
        }
  where
    getSourceConfigIfNeeded ::
      forall b arr m.
      ( ArrowChoice arr,
        Inc.ArrowCache m arr,
        ArrowWriter (Seq CollectedInfo) arr,
        MonadIO m,
        MonadResolveSource m,
        HasHttpManagerM m,
        BackendMetadata b
      ) =>
      ( Inc.Dependency (HashMap SourceName Inc.InvalidationKey),
        SourceName,
        SourceConnConfiguration b,
        BackendSourceKind b,
        BackendConfig b
      )
        `arr` Maybe (SourceConfig b)
    getSourceConfigIfNeeded = Inc.cache proc (invalidationKeys, sourceName, sourceConfig, backendKind, backendConfig) -> do
      let metadataObj = MetadataObject (MOSource sourceName) $ toJSON sourceName
      httpMgr <- bindA -< askHttpManager
      Inc.dependOn -< Inc.selectKeyD sourceName invalidationKeys
      (|
        withRecordInconsistency
          ( liftEitherA <<< bindA -< resolveSourceConfig @b logger sourceName sourceConfig backendKind backendConfig env httpMgr
          )
        |) metadataObj

    resolveSourceIfNeeded ::
      forall b arr m.
      ( ArrowChoice arr,
        Inc.ArrowCache m arr,
        ArrowWriter (Seq CollectedInfo) arr,
        MonadIO m,
        MonadBaseControl IO m,
        MonadResolveSource m,
        HasHttpManagerM m,
        BackendMetadata b
      ) =>
      ( Inc.Dependency (HashMap SourceName Inc.InvalidationKey),
        BackendConfigAndSourceMetadata b
      )
        `arr` Maybe (ResolvedSource b)
    resolveSourceIfNeeded = Inc.cache proc (invalidationKeys, BackendConfigAndSourceMetadata {..}) -> do
      let sourceName = _smName _bcasmSourceMetadata
          metadataObj = MetadataObject (MOSource sourceName) $ toJSON sourceName
          logAndResolveDatabaseMetadata :: SourceConfig b -> SourceTypeCustomization -> m (Either QErr (ResolvedSource b))
          logAndResolveDatabaseMetadata scConfig sType = do
            resSource <- resolveDatabaseMetadata _bcasmSourceMetadata scConfig sType
            for_ resSource $ liftIO . unLogger logger
            pure resSource

      maybeSourceConfig <- getSourceConfigIfNeeded @b -< (invalidationKeys, sourceName, _smConfiguration _bcasmSourceMetadata, _smKind _bcasmSourceMetadata, _bcasmBackendConfig)
      case maybeSourceConfig of
        Nothing -> returnA -< Nothing
        Just sourceConfig ->
          (|
            withRecordInconsistency
              ( liftEitherA <<< bindA -< logAndResolveDatabaseMetadata sourceConfig (getSourceTypeCustomization $ _smCustomization _bcasmSourceMetadata)
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
      (Proxy b, Bool, SourceConfig b) `arr` RecreateEventTriggers
    initCatalogIfNeeded = Inc.cache proc (Proxy, atleastOneTrigger, sourceConfig) -> do
      arrM id
        -< do
          if atleastOneTrigger
            then do
              maintenanceMode <- _sccMaintenanceMode <$> askServerConfigCtx
              eventingMode <- _sccEventingMode <$> askServerConfigCtx
              readOnlyMode <- _sccReadOnlyMode <$> askServerConfigCtx

              if
                  -- when safe mode is enabled, don't perform any migrations
                  | readOnlyMode == ReadOnlyModeEnabled -> pure RETDoNothing
                  -- when eventing mode is disabled, don't perform any migrations
                  | eventingMode == EventingDisabled -> pure RETDoNothing
                  -- when maintenance mode is enabled, don't perform any migrations
                  | maintenanceMode == (MaintenanceModeEnabled ()) -> pure RETDoNothing
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
            else pure RETDoNothing

    buildSource ::
      forall b arr m.
      ( ArrowChoice arr,
        Inc.ArrowDistribute arr,
        Inc.ArrowCache m arr,
        ArrowWriter (Seq CollectedInfo) arr,
        HasServerConfigCtx m,
        MonadError QErr m,
        BackendMetadata b
      ) =>
      ( HashMap SourceName (AB.AnyBackend PartiallyResolvedSource),
        SourceMetadata b,
        SourceConfig b,
        HashMap (TableName b) (TableCoreInfoG b (ColumnInfo b) (ColumnInfo b)),
        HashMap (TableName b) (EventTriggerInfoMap b),
        DBTablesMetadata b,
        DBFunctionsMetadata b,
        RemoteSchemaMap,
        OrderedRoles
      )
        `arr` BackendSourceInfo
    buildSource = proc (allSources, sourceMetadata, sourceConfig, tablesRawInfo, eventTriggerInfoMaps, _dbTables, dbFunctions, remoteSchemaMap, orderedRoles) -> do
      let SourceMetadata sourceName _backendKind tables functions _ queryTagsConfig sourceCustomization = sourceMetadata
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

      !defaultNC <- bindA -< _sccDefaultNamingConvention <$> askServerConfigCtx
      !isNamingConventionEnabled <- bindA -< ((EFNamingConventions `elem`) . _sccExperimentalFeatures) <$> askServerConfigCtx

      -- sql functions
      functionCache <-
        (mapFromL _fmFunction (OMap.elems functions) >- returnA)
          >-> (|
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
                          ( (|
                              modifyErrA
                                ( do
                                    let funcDefs = fromMaybe [] $ M.lookup qf dbFunctions
                                    rawfunctionInfo <- bindErrorA -< handleMultipleFunctions @b qf funcDefs
                                    let metadataPermissions = mapFromL _fpmRole functionPermissions
                                        permissionsMap = mkBooleanPermissionMap FunctionPermissionInfo metadataPermissions orderedRoles
                                    let !namingConv = if isNamingConventionEnabled then getNamingConvention sourceCustomization defaultNC else HasuraCase
                                    (functionInfo, dep) <- bindErrorA -< buildFunctionInfo sourceName qf systemDefined config permissionsMap rawfunctionInfo comment namingConv
                                    recordDependencies -< (metadataObject, schemaObject, [dep])
                                    returnA -< functionInfo
                                )
                            |) addFunctionContext
                          )
                        |) metadataObject
                  )
              |)
          >-> (\infos -> catMaybes infos >- returnA)

      returnA -< AB.mkAnyBackend $ SourceInfo sourceName tableCache functionCache sourceConfig queryTagsConfig sourceCustomization

    buildAndCollectInfo ::
      forall arr m.
      ( ArrowChoice arr,
        Inc.ArrowDistribute arr,
        Inc.ArrowCache m arr,
        ArrowWriter (Seq CollectedInfo) arr,
        MonadIO m,
        MonadError QErr m,
        MonadReader BuildReason m,
        MonadBaseControl IO m,
        HasHttpManagerM m,
        HasServerConfigCtx m,
        MonadResolveSource m
      ) =>
      (Metadata, Inc.Dependency InvalidationKeys) `arr` BuildOutputs
    buildAndCollectInfo = proc (metadata, invalidationKeys) -> do
      let Metadata
            sources
            remoteSchemas
            collections
            metadataAllowlist
            customTypes
            actions
            cronTriggers
            endpoints
            apiLimits
            metricsConfig
            inheritedRoles
            _introspectionDisabledRoles
            networkConfig
            backendConfigs = metadata
          backendConfigAndSourceMetadata = joinBackendConfigsToSources backendConfigs sources
          actionRoles = map _apmRole . _amPermissions =<< OMap.elems actions
          remoteSchemaRoles = map _rspmRole . _rsmPermissions =<< OMap.elems remoteSchemas
          sourceRoles =
            HS.fromList $
              concat $
                OMap.elems sources >>= \e ->
                  AB.dispatchAnyBackend @Backend e \(SourceMetadata _ _ tables _functions _ _ _) -> do
                    table <- OMap.elems tables
                    pure $
                      OMap.keys (_tmInsertPermissions table)
                        <> OMap.keys (_tmSelectPermissions table)
                        <> OMap.keys (_tmUpdatePermissions table)
                        <> OMap.keys (_tmDeletePermissions table)
          inheritedRoleNames = OMap.keys inheritedRoles
          allRoleNames = sourceRoles <> HS.fromList (remoteSchemaRoles <> actionRoles <> inheritedRoleNames)

          remoteSchemaPermissions =
            let remoteSchemaPermsList = OMap.toList $ _rsmPermissions <$> remoteSchemas
             in concat $
                  flip map remoteSchemaPermsList $
                    ( \(remoteSchemaName, remoteSchemaPerms) ->
                        flip map remoteSchemaPerms $ \(RemoteSchemaPermissionMetadata role defn comment) ->
                          AddRemoteSchemaPermission remoteSchemaName role defn comment
                    )

      -- roles which have some kind of permission (action/remote schema/table/function) set in the metadata
      let metadataRoles = mapFromL _rRoleName $ (`Role` ParentRoles mempty) <$> toList allRoleNames

      resolvedInheritedRoles <- buildInheritedRoles -< (allRoleNames, OMap.elems inheritedRoles)

      let allRoles = resolvedInheritedRoles `M.union` metadataRoles

      orderedRoles <- bindA -< orderRoles $ M.elems allRoles

      -- remote schemas
      let remoteSchemaInvalidationKeys = Inc.selectD #_ikRemoteSchemas invalidationKeys
      remoteSchemaMap <- buildRemoteSchemas -< (remoteSchemaInvalidationKeys, OMap.elems remoteSchemas)
      let remoteSchemaCtxMap = M.map (fst . fst) remoteSchemaMap

      !defaultNC <- bindA -< _sccDefaultNamingConvention <$> askServerConfigCtx
      !isNamingConventionEnabled <- bindA -< ((EFNamingConventions `elem`) . _sccExperimentalFeatures) <$> askServerConfigCtx

      -- sources are build in two steps
      -- first we resolve them, and build the table cache
      partiallyResolvedSources <-
        (|
          Inc.keyed
            ( \_ exists ->
                AB.dispatchAnyBackendArrow @BackendMetadata @BackendEventTrigger
                  ( proc (backendConfigAndSourceMetadata, (invalidationKeys, defaultNC, isNamingConventionEnabled)) -> do
                      let sourceMetadata = _bcasmSourceMetadata backendConfigAndSourceMetadata
                          sourceName = _smName sourceMetadata
                          sourceInvalidationsKeys = Inc.selectD #_ikSources invalidationKeys
                      maybeResolvedSource <- resolveSourceIfNeeded -< (sourceInvalidationsKeys, backendConfigAndSourceMetadata)
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

                          recreateEventTriggers <- initCatalogIfNeeded -< (Proxy :: Proxy b, numEventTriggers > 0, sourceConfig)

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
        |) (M.fromList $ OMap.toList backendConfigAndSourceMetadata)
          >-> (\infos -> catMaybes infos >- returnA)

      -- then we can build the entire source output
      -- we need to have the table cache of all sources to build cross-sources relationships
      sourcesOutput <-
        (|
          Inc.keyed
            ( \_ exists ->
                AB.dispatchAnyBackendArrow @BackendMetadata @BackendMetadata
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
        (remoteSchemaMap >- returnA)
          >-> ( \info ->
                  (info, M.groupOn _arspRemoteSchema remoteSchemaPermissions)
                    >-
                      alignExtraRemoteSchemaInfo mkRemoteSchemaPermissionMetadataObject
              )
          >-> (|
                Inc.keyed
                  ( \_ (((remoteSchemaCtx, relationships), metadataObj), remoteSchemaPerms) -> do
                      metadataPermissionsMap <-
                        buildRemoteSchemaPermissions -< (remoteSchemaCtx, remoteSchemaPerms)
                      -- convert to the intermediate form `CheckPermission` whose `Semigroup`
                      -- instance is used to combine permissions
                      let metadataCheckPermissionsMap = CPDefined <$> metadataPermissionsMap
                      allRolesUnresolvedPermissionsMap <-
                        bindA
                          -<
                            foldM
                              ( \accumulatedRolePermMap (Role roleName (ParentRoles parentRoles)) -> do
                                  rolePermission <- onNothing (M.lookup roleName accumulatedRolePermMap) $ do
                                    parentRolePermissions <-
                                      for (toList parentRoles) $ \role ->
                                        onNothing (M.lookup role accumulatedRolePermMap) $
                                          throw500 $
                                            "remote schema permissions: bad ordering of roles, could not find the permission of role: " <>> role
                                    let combinedPermission = sconcat <$> nonEmpty parentRolePermissions
                                    pure $ fromMaybe CPUndefined combinedPermission
                                  pure $ M.insert roleName rolePermission accumulatedRolePermMap
                              )
                              metadataCheckPermissionsMap
                              (_unOrderedRoles orderedRoles)
                      -- traverse through `allRolesUnresolvedPermissionsMap` to record any inconsistencies (if exists)
                      resolvedPermissions <-
                        (|
                          traverseA
                            ( \(roleName, checkPermission) -> do
                                let inconsistentRoleEntity = InconsistentRemoteSchemaPermission $ _rscName remoteSchemaCtx
                                resolvedCheckPermission <- interpretWriter -< resolveCheckPermission checkPermission roleName inconsistentRoleEntity
                                returnA -< (roleName, resolvedCheckPermission)
                            )
                          |) (M.toList allRolesUnresolvedPermissionsMap)
                      let remoteSchemaIntrospection = irDoc $ _rscIntroOriginal remoteSchemaCtx
                      resolvedRelationships <-
                        (|
                          traverseA
                            ( \(typeName, typeRelationships) -> do
                                resolvedRelationships <-
                                  (|
                                    traverseA
                                      ( \fromSchemaDef ->
                                          buildRemoteSchemaRemoteRelationship
                                            -<
                                              ( (partiallyResolvedSources, remoteSchemaCtxMap),
                                                (_rscName remoteSchemaCtx, remoteSchemaIntrospection, typeName, fromSchemaDef)
                                              )
                                      )
                                    |) (_rstrsRelationships typeRelationships)
                                returnA -< (typeName, resolvedRelationships)
                            )
                          |) (OMap.toList relationships)
                      returnA
                        -<
                          ( remoteSchemaCtx
                              { _rscPermissions = catMaybes $ M.fromList resolvedPermissions,
                                _rscRemoteRelationships = OMap.catMaybes <$> OMap.fromList resolvedRelationships
                              },
                            metadataObj
                          )
                  )
              |)

      -- allowlist
      let inlinedAllowlist = inlineAllowlist collections metadataAllowlist

      resolvedEndpoints <- buildInfoMap fst mkEndpointMetadataObject buildEndpoint -< (collections, OMap.toList endpoints)

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

      cronTriggersMap <- buildCronTriggers -< ((), OMap.elems cronTriggers)

      returnA
        -<
          BuildOutputs
            { _boSources = M.map fst sourcesOutput,
              _boActions = actionCache,
              _boRemoteSchemas = remoteSchemaCache,
              _boAllowlist = inlinedAllowlist,
              _boCustomTypes = annotatedCustomTypes,
              _boCronTriggers = cronTriggersMap,
              _boEndpoints = resolvedEndpoints,
              _boApiLimits = apiLimits,
              _boMetricsConfig = metricsConfig,
              _boRoles = mapFromL _rRoleName $ _unOrderedRoles orderedRoles,
              _boTlsAllowlist = (networkTlsAllowlist networkConfig),
              _boQueryCollections = collections
            }

    mkEndpointMetadataObject (name, createEndpoint) =
      let objectId = MOEndpoint name
       in MetadataObject objectId (toJSON createEndpoint)

    buildEndpoint ::
      (ArrowChoice arr, ArrowKleisli m arr, MonadError QErr m, ArrowWriter (Seq CollectedInfo) arr) =>
      (InsOrdHashMap CollectionName CreateCollection, (EndpointName, CreateEndpoint)) `arr` Maybe (EndpointMetadata GQLQueryWithText)
    buildEndpoint = proc (collections, e@(name, createEndpoint)) -> do
      let endpoint = createEndpoint
          -- QueryReference collName queryName = _edQuery endpoint
          addContext err = "in endpoint " <> toTxt (unEndpointName name) <> ": " <> err
      (|
        withRecordInconsistency
          ( (|
              modifyErrA
                (bindErrorA -< resolveEndpoint collections endpoint)
            |) addContext
          )
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

    mkRemoteSchemaMetadataObject remoteSchema =
      MetadataObject (MORemoteSchema (_rsmName remoteSchema)) (toJSON remoteSchema)

    mkInheritedRoleMetadataObject inheritedRole@(Role roleName _) =
      MetadataObject (MOInheritedRole roleName) (toJSON inheritedRole)

    alignExtraRemoteSchemaInfo ::
      forall a b arr.
      (ArrowChoice arr, Inc.ArrowDistribute arr, ArrowWriter (Seq CollectedInfo) arr) =>
      (b -> MetadataObject) ->
      ( M.HashMap RemoteSchemaName a,
        M.HashMap RemoteSchemaName [b]
      )
        `arr` M.HashMap RemoteSchemaName (a, [b])
    alignExtraRemoteSchemaInfo mkMetadataObject = proc (baseInfo, extraInfo) -> do
      combinedInfo <-
        (|
          Inc.keyed
            (\remoteSchemaName infos -> combine -< (remoteSchemaName, infos))
          |) (align baseInfo extraInfo)
      returnA -< catMaybes combinedInfo
      where
        combine :: (RemoteSchemaName, These a [b]) `arr` Maybe (a, [b])
        combine = proc (remoteSchemaName, infos) -> case infos of
          This base -> returnA -< Just (base, [])
          These base extras -> returnA -< Just (base, extras)
          That extras -> do
            let errorMessage = "remote schema  " <> unRemoteSchemaName remoteSchemaName <<> " does not exist"
            recordInconsistencies -< (map mkMetadataObject extras, errorMessage)
            returnA -< Nothing

    buildRemoteSchemaPermissions ::
      ( ArrowChoice arr,
        Inc.ArrowDistribute arr,
        ArrowWriter (Seq CollectedInfo) arr,
        Inc.ArrowCache m arr,
        MonadError QErr m
      ) =>
      (RemoteSchemaCtx, [AddRemoteSchemaPermission]) `arr` M.HashMap RoleName IntrospectionResult
    buildRemoteSchemaPermissions = buildInfoMap _arspRole mkRemoteSchemaPermissionMetadataObject buildRemoteSchemaPermission
      where
        buildRemoteSchemaPermission = proc (remoteSchemaCtx, remoteSchemaPerm) -> do
          let AddRemoteSchemaPermission rsName roleName defn _ = remoteSchemaPerm
              metadataObject = mkRemoteSchemaPermissionMetadataObject remoteSchemaPerm
              schemaObject = SORemoteSchemaPermission rsName roleName
              providedSchemaDoc = _rspdSchema defn
              addPermContext err = "in remote schema permission for role " <> roleName <<> ": " <> err
          (|
            withRecordInconsistency
              ( (|
                  modifyErrA
                    ( do
                        bindErrorA
                          -<
                            when (roleName == adminRoleName) $
                              throw400 ConstraintViolation $ "cannot define permission for admin role"
                        (resolvedSchemaIntrospection, dependencies) <-
                          bindErrorA -< resolveRoleBasedRemoteSchema providedSchemaDoc remoteSchemaCtx
                        recordDependencies -< (metadataObject, schemaObject, dependencies)
                        returnA -< resolvedSchemaIntrospection
                    )
                |) addPermContext
              )
            |) metadataObject

    buildTableEventTriggers ::
      forall arr m b.
      ( ArrowChoice arr,
        Inc.ArrowDistribute arr,
        ArrowWriter (Seq CollectedInfo) arr,
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
        ArrowWriter (Seq CollectedInfo) arr,
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
              ( (|
                  modifyErrA
                    (bindErrorA -< resolveCronTrigger env cronTrigger)
                |) addCronTriggerContext
              )
            |) (mkCronTriggerMetadataObject cronTrigger)

    buildInheritedRoles ::
      ( ArrowChoice arr,
        Inc.ArrowDistribute arr,
        ArrowWriter (Seq CollectedInfo) arr,
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
              ( (|
                  modifyErrA
                    ( do
                        (resolvedInheritedRole, dependencies) <- bindA -< resolveInheritedRole allRoles inheritedRole
                        recordDependencies -< (metadataObject, schemaObject, dependencies)
                        returnA -< resolvedInheritedRole
                    )
                |) addInheritedRoleContext
              )
            |) metadataObject

    buildActions ::
      ( ArrowChoice arr,
        Inc.ArrowDistribute arr,
        Inc.ArrowCache m arr,
        ArrowWriter (Seq CollectedInfo) arr
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
              ( (|
                  modifyErrA
                    ( do
                        (resolvedDef, outObject) <-
                          liftEitherA <<< bindA
                            -<
                              runExceptT $ resolveAction env resolvedCustomTypes def scalarsMap
                        let permissionInfos = map (ActionPermissionInfo . _apmRole) actionPermissions
                            metadataPermissionMap = mapFromL _apiRole permissionInfos
                            permissionsMap = mkBooleanPermissionMap ActionPermissionInfo metadataPermissionMap orderedRoles
                            forwardClientHeaders = _adForwardClientHeaders resolvedDef
                            outputType = unGraphQLType $ _adOutputType def
                        returnA -< ActionInfo name (outputType, outObject) resolvedDef permissionsMap forwardClientHeaders comment
                    )
                |) addActionContext
              )
            |) (mkActionMetadataObject action)

    buildRemoteSchemas ::
      ( ArrowChoice arr,
        Inc.ArrowDistribute arr,
        ArrowWriter (Seq CollectedInfo) arr,
        Inc.ArrowCache m arr,
        MonadIO m,
        HasHttpManagerM m
      ) =>
      ( Inc.Dependency (HashMap RemoteSchemaName Inc.InvalidationKey),
        [RemoteSchemaMetadata]
      )
        `arr` HashMap RemoteSchemaName ((RemoteSchemaCtx, SchemaRemoteRelationships), MetadataObject)
    buildRemoteSchemas =
      buildInfoMapPreservingMetadata _rsmName mkRemoteSchemaMetadataObject buildRemoteSchema
      where
        -- We want to cache this call because it fetches the remote schema over HTTP, and we don’t
        -- want to re-run that if the remote schema definition hasn’t changed.
        buildRemoteSchema = Inc.cache proc (invalidationKeys, remoteSchema@(RemoteSchemaMetadata name defn comment _ relationships)) -> do
          -- TODO is it strange how we convert from RemoteSchemaMetadata back
          --      to AddRemoteSchemaQuery here? Document types please.
          let addRemoteSchemaQuery = AddRemoteSchemaQuery name defn comment
          Inc.dependOn -< Inc.selectKeyD name invalidationKeys
          (|
            withRecordInconsistency
              ( liftEitherA <<< bindA
                  -<
                    (fmap . fmap) (,relationships) $
                      runExceptT $ noopTrace $ addRemoteSchemaP2Setup env addRemoteSchemaQuery
              )
            |) (mkRemoteSchemaMetadataObject remoteSchema)
        -- TODO continue propagating MonadTrace up calls so that we can get tracing for remote schema introspection.
        -- This will require modifying CacheBuild.
        noopTrace = Tracing.runTraceTWithReporter Tracing.noReporter "buildSchemaCacheRule"

buildRemoteSchemaRemoteRelationship ::
  forall arr m.
  ( ArrowChoice arr,
    ArrowWriter (Seq CollectedInfo) arr,
    ArrowKleisli m arr,
    MonadError QErr m
  ) =>
  ( (HashMap SourceName (AB.AnyBackend PartiallyResolvedSource), RemoteSchemaMap),
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
        ( (|
            modifyErrA
              ( do
                  allowedLHSJoinFields <-
                    bindErrorA
                      -<
                        getRemoteSchemaEntityJoinColumns remoteSchema remoteSchemaIntrospection typeName
                  (remoteField, rhsDependencies) <-
                    bindErrorA
                      -<
                        buildRemoteFieldInfo (remoteSchemaToLHSIdentifier remoteSchema) allowedLHSJoinFields rr allSources remoteSchemaMap
                  -- buildRemoteFieldInfo only knows how to construct dependencies on the RHS of the join condition,
                  -- so the dependencies on the remote relationship on the LHS entity have to be computed here
                  let lhsDependencies =
                        -- a direct dependency on the remote schema on which this is defined
                        [SchemaDependency (SORemoteSchema remoteSchema) DRRemoteRelationship]
                  recordDependencies -< (metadataObject, schemaObj, lhsDependencies <> rhsDependencies)
                  returnA -< remoteField
              )
          |) addRemoteRelationshipContext
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

data BackendConfigAndSourceMetadata b = BackendConfigAndSourceMetadata
  { _bcasmBackendConfig :: BackendConfig b,
    _bcasmSourceMetadata :: SourceMetadata b
  }
  deriving stock (Generic)

deriving instance (Backend b) => Show (BackendConfigAndSourceMetadata b)

deriving instance (Backend b) => Eq (BackendConfigAndSourceMetadata b)

instance (Backend b) => Inc.Cacheable (BackendConfigAndSourceMetadata b)

joinBackendConfigsToSources ::
  BackendMap BackendConfigWrapper ->
  InsOrdHashMap SourceName (AB.AnyBackend SourceMetadata) ->
  InsOrdHashMap SourceName (AB.AnyBackend BackendConfigAndSourceMetadata)
joinBackendConfigsToSources backendConfigs sources =
  flip OMap.map sources $ \abSourceMetadata ->
    AB.dispatchAnyBackend @Backend abSourceMetadata $ \(sourceMetadata :: SourceMetadata b) ->
      let _bcasmBackendConfig = maybe mempty unBackendConfigWrapper (BackendMap.lookup @b backendConfigs)
          _bcasmSourceMetadata = sourceMetadata
       in AB.mkAnyBackend @b BackendConfigAndSourceMetadata {..}

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
