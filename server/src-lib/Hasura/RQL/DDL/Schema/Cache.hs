{-# LANGUAGE Arrows               #-}
{-# LANGUAGE OverloadedLabels     #-}
{-# LANGUAGE UndecidableInstances #-}

{-| Top-level functions concerned specifically with operations on the schema cache, such as
rebuilding it from the catalog and incorporating schema changes. See the module documentation for
"Hasura.RQL.DDL.Schema" for more details.

__Note__: this module is __mutually recursive__ with other @Hasura.RQL.DDL.Schema.*@ modules, which
both define pieces of the implementation of building the schema cache and define handlers that
trigger schema cache rebuilds. -}
module Hasura.RQL.DDL.Schema.Cache
  ( RebuildableSchemaCache
  , lastBuiltSchemaCache
  , buildRebuildableSchemaCache
  , buildRebuildableSchemaCacheWithReason
  , CacheRWT
  , runCacheRWT
  ) where

import           Hasura.Prelude

import qualified Data.Dependent.Map                       as DMap
import qualified Data.Environment                         as Env
import qualified Data.HashMap.Strict.Extended             as M
import qualified Data.HashMap.Strict.InsOrd               as OMap
import qualified Data.HashSet                             as HS
import qualified Data.HashSet.InsOrd                      as HSIns
import qualified Data.Set                                 as S
import qualified Language.GraphQL.Draft.Syntax            as G

import           Control.Arrow.Extended
import           Control.Lens                             hiding ((.=))
import           Control.Monad.Trans.Control              (MonadBaseControl)
import           Control.Monad.Unique
import           Data.Aeson
import           Data.Text.Extended
import           Network.HTTP.Client.Extended

import qualified Hasura.Incremental                       as Inc
import qualified Hasura.SQL.AnyBackend                    as AB
import qualified Hasura.Tracing                           as Tracing

import           Hasura.Backends.Postgres.Connection
import           Hasura.GraphQL.Execute.Types
import           Hasura.GraphQL.Schema                    (buildGQLContext)
import           Hasura.Metadata.Class
import           Hasura.RQL.DDL.Action
import           Hasura.RQL.DDL.CustomTypes
import           Hasura.RQL.DDL.RemoteSchema
import           Hasura.RQL.DDL.RemoteSchema.Permission   (resolveRoleBasedRemoteSchema)
import           Hasura.RQL.DDL.ScheduledTrigger
import           Hasura.RQL.DDL.Schema.Cache.Common
import           Hasura.RQL.DDL.Schema.Cache.Dependencies
import           Hasura.RQL.DDL.Schema.Cache.Fields
import           Hasura.RQL.DDL.Schema.Cache.Permission
import           Hasura.RQL.DDL.Schema.Function
import           Hasura.RQL.DDL.Schema.Table
import           Hasura.RQL.Types                         hiding (fmFunction, tmTable)
import           Hasura.SQL.Tag
import           Hasura.Server.Types                      (MaintenanceMode (..))
import           Hasura.Server.Version                    (HasVersion)
import           Hasura.Session


buildRebuildableSchemaCache
  :: HasVersion
  => Env.Environment
  -> Metadata
  -> CacheBuild RebuildableSchemaCache
buildRebuildableSchemaCache =
  buildRebuildableSchemaCacheWithReason CatalogSync

buildRebuildableSchemaCacheWithReason
  :: HasVersion
  => BuildReason
  -> Env.Environment
  -> Metadata
  -> CacheBuild RebuildableSchemaCache
buildRebuildableSchemaCacheWithReason reason env metadata = do
  result <- flip runReaderT reason $
    Inc.build (buildSchemaCacheRule env) (metadata, initialInvalidationKeys)
  pure $ RebuildableSchemaCache (Inc.result result) initialInvalidationKeys (Inc.rebuildRule result)

newtype CacheRWT m a
  -- The CacheInvalidations component of the state could actually be collected using WriterT, but
  -- WriterT implementations prior to transformers-0.5.6.0 (which added
  -- Control.Monad.Trans.Writer.CPS) are leaky, and we don’t have that yet.
  = CacheRWT (StateT (RebuildableSchemaCache, CacheInvalidations) m a)
  deriving
    ( Functor, Applicative, Monad, MonadIO, MonadUnique, MonadReader r, MonadError e, MonadTx
    , UserInfoM, HasHttpManagerM, HasSystemDefined, MonadMetadataStorage
    , MonadMetadataStorageQueryAPI, Tracing.MonadTrace, HasServerConfigCtx)

deriving instance (MonadBase IO m) => MonadBase IO (CacheRWT m)
deriving instance (MonadBaseControl IO m) => MonadBaseControl IO (CacheRWT m)

runCacheRWT
  :: Functor m
  => RebuildableSchemaCache -> CacheRWT m a -> m (a, RebuildableSchemaCache, CacheInvalidations)
runCacheRWT cache (CacheRWT m) =
  runStateT m (cache, mempty) <&> \(v, (newCache, invalidations)) -> (v, newCache, invalidations)

instance MonadTrans CacheRWT where
  lift = CacheRWT . lift

instance (Monad m) => CacheRM (CacheRWT m) where
  askSchemaCache = CacheRWT $ gets (lastBuiltSchemaCache . (^. _1))

instance (MonadIO m, MonadError QErr m, HasHttpManagerM m
         , MonadResolveSource m, HasServerConfigCtx m) => CacheRWM (CacheRWT m) where
  buildSchemaCacheWithOptions buildReason invalidations metadata = CacheRWT do
    (RebuildableSchemaCache lastBuiltSC invalidationKeys rule, oldInvalidations) <- get
    let metadataVersion = scMetadataResourceVersion lastBuiltSC
        newInvalidationKeys = invalidateKeys invalidations invalidationKeys
    result <- lift $ runCacheBuildM $ flip runReaderT buildReason $
              Inc.build rule (metadata, newInvalidationKeys)
    let schemaCache = (Inc.result result) { scMetadataResourceVersion = metadataVersion }
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
    put (rebuildableSchemaCache
          { lastBuiltSchemaCache = (lastBuiltSchemaCache rebuildableSchemaCache)
            { scMetadataResourceVersion = Just resourceVersion} }
        , invalidations)



buildSchemaCacheRule
  -- Note: by supplying BuildReason via MonadReader, it does not participate in caching, which is
  -- what we want!
  :: ( HasVersion, ArrowChoice arr, Inc.ArrowDistribute arr, Inc.ArrowCache m arr
     , MonadIO m, MonadUnique m, MonadBaseControl IO m, MonadError QErr m
     , MonadReader BuildReason m, HasHttpManagerM m, MonadResolveSource m
     , HasServerConfigCtx m)
  => Env.Environment
  -> (Metadata, InvalidationKeys) `arr` SchemaCache
buildSchemaCacheRule env = proc (metadata, invalidationKeys) -> do
  invalidationKeysDep <- Inc.newDependency -< invalidationKeys

  -- Step 1: Process metadata and collect dependency information.
  (outputs, collectedInfo) <-
    runWriterA buildAndCollectInfo -< (metadata, invalidationKeysDep)
  let (inconsistentObjects, unresolvedDependencies) = partitionCollectedInfo collectedInfo

  -- Step 2: Resolve dependency information and drop dangling dependents.
  (resolvedOutputs, dependencyInconsistentObjects, resolvedDependencies) <-
    resolveDependencies -< (outputs, unresolvedDependencies)

  -- Step 3: Build the GraphQL schema.
  (gqlContext, gqlSchemaInconsistentObjects) <- runWriterA buildGQLContext -<
    ( QueryHasura
    , _boSources resolvedOutputs
    , _boRemoteSchemas resolvedOutputs
    , _boActions resolvedOutputs
    , _actNonObjects $ _boCustomTypes resolvedOutputs
    )

  -- Step 4: Build the relay GraphQL schema
  (relayContext, relaySchemaInconsistentObjects) <- runWriterA buildGQLContext -<
    ( QueryRelay
    , _boSources resolvedOutputs
    , _boRemoteSchemas resolvedOutputs
    , _boActions resolvedOutputs
    , _actNonObjects $ _boCustomTypes resolvedOutputs
    )

  let
    duplicateVariables :: EndpointMetadata a -> Bool
    duplicateVariables m = any ((>1) . length) $ group $ sort $ catMaybes $ splitPath Just (const Nothing) (_ceUrl m)

    endpointObjId :: EndpointMetadata q -> MetadataObjId
    endpointObjId md = MOEndpoint (_ceName md)

    endpointObject :: EndpointMetadata q -> MetadataObject
    endpointObject md = MetadataObject (endpointObjId md) (toJSON $ OMap.lookup (_ceName md) $ _metaRestEndpoints metadata)

    --  Cases of urls that generate invalid segments:
    -- * "" -> Error: "Empty URL"
    -- * "/asdf" -> Error: "Leading slash not allowed"
    -- * "asdf/" -> Error: "Trailing slash not allowed"
    -- * "asdf//qwer" -> Error: "Double Slash not allowed"
    -- * "asdf/:/qwer" -> Error: "Variables must be named"
    -- * "asdf/:x/qwer/:x" -> Error: "Duplicate Variable: x"
    hasInvalidSegments :: EndpointMetadata query -> Bool
    hasInvalidSegments m = any (`elem` ["",":"]) (splitPath id id (_ceUrl m))

    ceUrlTxt               = toTxt . _ceUrl

    endpoints              = buildEndpointsTrie (M.elems $ _boEndpoints resolvedOutputs)

    duplicateF md          = DuplicateRestVariables ("Duplicate variables found in endpoint path " <> (ceUrlTxt md)) (endpointObject md)
    duplicateRestVariables = map duplicateF $ filter duplicateVariables (M.elems $ _boEndpoints resolvedOutputs)

    invalidF md            = InvalidRestSegments ("Empty segments or unnamed variables are not allowed: " <> (ceUrlTxt md)) (endpointObject md)
    invalidRestSegments    = map invalidF $ filter hasInvalidSegments (M.elems $ _boEndpoints resolvedOutputs)

    ambiguousF' ep         = MetadataObject (endpointObjId ep) (toJSON ep)
    ambiguousF mds         = AmbiguousRestEndpoints ("Ambiguous URL paths: " <> commaSeparated (map _ceUrl mds)) (map ambiguousF' mds)
    ambiguousRestEndpoints = map (ambiguousF . S.elems . snd) $ ambiguousPathsGrouped endpoints

  returnA -< SchemaCache
    { scSources = _boSources resolvedOutputs
    , scActions = _boActions resolvedOutputs
    -- TODO this is not the right value: we should track what part of the schema
    -- we can stitch without consistencies, I think.
    , scRemoteSchemas = fmap fst (_boRemoteSchemas resolvedOutputs) -- remoteSchemaMap
    , scAllowlist = _boAllowlist resolvedOutputs
    -- , scCustomTypes = _boCustomTypes resolvedOutputs
    , scGQLContext = fst gqlContext
    , scUnauthenticatedGQLContext = snd gqlContext
    , scRelayContext = fst relayContext
    , scUnauthenticatedRelayContext = snd relayContext
    -- , scGCtxMap = gqlSchema
    -- , scDefaultRemoteGCtx = remoteGQLSchema
    , scDepMap = resolvedDependencies
    , scCronTriggers = _boCronTriggers resolvedOutputs
    , scEndpoints = endpoints
    , scInconsistentObjs =
           inconsistentObjects
        <> dependencyInconsistentObjects
        <> toList gqlSchemaInconsistentObjects
        <> toList relaySchemaInconsistentObjects
        <> duplicateRestVariables
        <> invalidRestSegments
        <> ambiguousRestEndpoints
    , scApiLimits = _boApiLimits resolvedOutputs
    , scMetricsConfig = _boMetricsConfig resolvedOutputs
    , scMetadataResourceVersion = Nothing
    }
  where
    getSourceConfigIfNeeded
      :: ( ArrowChoice arr, Inc.ArrowCache m arr
         , ArrowWriter (Seq CollectedInfo) arr
         , MonadIO m, MonadBaseControl IO m
         , MonadResolveSource m
         , BackendMetadata b
         )
      => ( Inc.Dependency (HashMap SourceName Inc.InvalidationKey)
         , SourceName, (SourceConnConfiguration b)
         ) `arr` Maybe (SourceConfig b)
    getSourceConfigIfNeeded = Inc.cache proc (invalidationKeys, sourceName, sourceConfig) -> do
      let metadataObj = MetadataObject (MOSource sourceName) $ toJSON sourceName
      Inc.dependOn -< Inc.selectKeyD sourceName invalidationKeys
      (| withRecordInconsistency (
           liftEitherA <<< bindA -< resolveSourceConfig sourceName sourceConfig)
       |) metadataObj

    resolveSourceIfNeeded
      :: ( ArrowChoice arr, Inc.ArrowCache m arr
         , ArrowWriter (Seq CollectedInfo) arr
         , MonadIO m, MonadBaseControl IO m
         , MonadResolveSource m
         , BackendMetadata b
         )
      => ( Inc.Dependency (HashMap SourceName Inc.InvalidationKey)
         , SourceMetadata b
         ) `arr` Maybe (ResolvedSource b)
    resolveSourceIfNeeded = Inc.cache proc (invalidationKeys, sourceMetadata) -> do
      let sourceName = _smName sourceMetadata
          metadataObj = MetadataObject (MOSource sourceName) $ toJSON sourceName
      maybeSourceConfig <- getSourceConfigIfNeeded -< (invalidationKeys, sourceName, _smConfiguration sourceMetadata)
      case maybeSourceConfig of
        Nothing -> returnA -< Nothing
        Just sourceConfig ->
          (| withRecordInconsistency (
             liftEitherA <<< bindA -< resolveDatabaseMetadata sourceConfig)
          |) metadataObj

    buildSource
      :: forall arr m b
      .  ( ArrowChoice arr, Inc.ArrowDistribute arr, Inc.ArrowCache m arr
         , ArrowWriter (Seq CollectedInfo) arr, MonadBaseControl IO m
         , HasServerConfigCtx m, MonadIO m, MonadError QErr m, MonadReader BuildReason m
         , BackendMetadata b)
      => ( SourceMetadata b
         , SourceConfig b
         , DBTablesMetadata b
         , DBFunctionsMetadata b
         , RemoteSchemaMap
         , Inc.Dependency InvalidationKeys
         , InheritedRoles
         ) `arr` BackendSourceInfo
    buildSource = proc (sourceMetadata, sourceConfig, dbTables, dbFunctions, remoteSchemaMap, invalidationKeys, inheritedRoles) -> do
      let SourceMetadata source tables functions _ = sourceMetadata
          tablesMetadata = OMap.elems tables
          (tableInputs, nonColumnInputs, permissions) = unzip3 $ map mkTableInputs tablesMetadata
          eventTriggers = map (_tmTable &&& (OMap.elems . _tmEventTriggers)) tablesMetadata
          alignTableMap :: HashMap (TableName b) a -> HashMap (TableName b) c -> HashMap (TableName b) (a, c)
          alignTableMap = M.intersectionWith (,)
          metadataInvalidationKey = Inc.selectD #_ikMetadata invalidationKeys

      -- tables
      tableRawInfos <- buildTableCache -< ( source, sourceConfig, dbTables
                                          , tableInputs, metadataInvalidationKey
                                          )
      -- relationships and computed fields
      let nonColumnsByTable = mapFromL _nctiTable nonColumnInputs
      tableCoreInfos :: HashMap (TableName b) (TableCoreInfo b) <-
        (| Inc.keyed (\_ (tableRawInfo, nonColumnInput) -> do
             let columns = _tciFieldInfoMap tableRawInfo
             allFields :: FieldInfoMap (FieldInfo b) <- addNonColumnFields -< (source, tableRawInfos, columns, remoteSchemaMap, dbFunctions, nonColumnInput)
             returnA -< (tableRawInfo {_tciFieldInfoMap = allFields}))
         |) (tableRawInfos `alignTableMap` nonColumnsByTable)

      tableCoreInfosDep <- Inc.newDependency -< tableCoreInfos
      -- permissions and event triggers
      tableCache <-
        (| Inc.keyed (\_ ((tableCoreInfo, permissionInputs), (_, eventTriggerConfs)) -> do
             let tableFields = _tciFieldInfoMap tableCoreInfo
             permissionInfos <-
                buildTablePermissions
                -< (source, tableCoreInfosDep, tableFields, permissionInputs, inheritedRoles)
             eventTriggerInfos <- buildTableEventTriggers -< (source, sourceConfig, tableCoreInfo, eventTriggerConfs, metadataInvalidationKey)
             returnA -< TableInfo tableCoreInfo permissionInfos eventTriggerInfos
            )
         |) (tableCoreInfos `alignTableMap` mapFromL _tpiTable permissions `alignTableMap` mapFromL fst eventTriggers)

      -- sql functions
      functionCache <- (mapFromL _fmFunction (OMap.elems functions) >- returnA)
        >-> (| Inc.keyed (\_ (FunctionMetadata qf config functionPermissions) -> do
                 let systemDefined = SystemDefined False
                     definition = toJSON $ TrackFunction qf
                     metadataObject =
                       MetadataObject
                         (MOSourceObjId source
                           $ AB.mkAnyBackend
                           $ SMOFunction qf)
                       definition
                     schemaObject =
                       SOSourceObj source
                         $ AB.mkAnyBackend
                         $ SOIFunction qf
                     addFunctionContext e = "in function " <> qf <<> ": " <> e
                 (| withRecordInconsistency (
                    (| modifyErrA (do
                         let funcDefs = fromMaybe [] $ M.lookup qf dbFunctions
                         rawfi <- bindErrorA -< handleMultipleFunctions qf funcDefs
                         (fi, dep) <- bindErrorA -< buildFunctionInfo source qf systemDefined config functionPermissions rawfi
                         recordDependencies -< (metadataObject, schemaObject, [dep])
                         returnA -< fi)
                    |) addFunctionContext)
                  |) metadataObject) |)
        >-> (\infos -> M.catMaybes infos >- returnA)

      returnA -< AB.mkAnyBackend $ SourceInfo source tableCache functionCache sourceConfig

    buildSourceOutput
      :: forall arr m b
       . ( ArrowChoice arr, Inc.ArrowDistribute arr, Inc.ArrowCache m arr
         , ArrowWriter (Seq CollectedInfo) arr, MonadIO m, MonadError QErr m
         , MonadReader BuildReason m, MonadBaseControl IO m
         , HasServerConfigCtx m, MonadResolveSource m
         , BackendMetadata b
         )
      => ( Inc.Dependency InvalidationKeys
         , HashMap RemoteSchemaName RemoteSchemaCtx
         , SourceMetadata b
         , InheritedRoles
         ) `arr` Maybe (BackendSourceInfo, DMap.DMap BackendTag ScalarSet)
    buildSourceOutput = proc (invalidationKeys, remoteSchemaCtxMap, sourceMetadata, inheritedRoles) -> do
      let sourceInvalidationsKeys = Inc.selectD #_ikSources invalidationKeys
      maybeResolvedSource <- resolveSourceIfNeeded -< (sourceInvalidationsKeys, sourceMetadata)
      case maybeResolvedSource of
        Nothing -> returnA -< Nothing
        Just (ResolvedSource sourceConfig tablesMeta functionsMeta scalars) -> do
          so <- buildSource -< ( sourceMetadata, sourceConfig, tablesMeta, functionsMeta
                               , remoteSchemaCtxMap, invalidationKeys, inheritedRoles
                               )
          returnA -< Just (so, DMap.singleton (backendTag @b) $ ScalarSet scalars)

    buildAndCollectInfo
      :: forall arr m
       . ( ArrowChoice arr, Inc.ArrowDistribute arr, Inc.ArrowCache m arr
         , ArrowWriter (Seq CollectedInfo) arr, MonadIO m, MonadUnique m, MonadError QErr m
         , MonadReader BuildReason m, MonadBaseControl IO m
         , HasHttpManagerM m, HasServerConfigCtx m, MonadResolveSource m)
      => (Metadata, Inc.Dependency InvalidationKeys) `arr` BuildOutputs
    buildAndCollectInfo = proc (metadata, invalidationKeys) -> do
      let Metadata sources remoteSchemas collections allowlists
            customTypes actions cronTriggers endpoints apiLimits metricsConfig inheritedRoles = metadata
          actionRoles = map _apmRole . _amPermissions =<< OMap.elems actions
          remoteSchemaRoles = map _rspmRole . _rsmPermissions =<< OMap.elems remoteSchemas
          sourceRoles =
            HS.fromList $ concat $
            OMap.elems sources >>= \e ->
               AB.dispatchAnyBackend @Backend e \(SourceMetadata _ tables _functions _) -> do
                 table <- OMap.elems tables
                 pure $ OMap.keys (_tmInsertPermissions table) <>
                        OMap.keys (_tmSelectPermissions table) <>
                        OMap.keys (_tmUpdatePermissions table) <>
                        OMap.keys (_tmDeletePermissions table)

          remoteSchemaPermissions =
            let remoteSchemaPermsList = OMap.toList $ _rsmPermissions <$> remoteSchemas
            in concat $ flip map remoteSchemaPermsList $
                 (\(remoteSchemaName, remoteSchemaPerms) ->
                    flip map remoteSchemaPerms $ \(RemoteSchemaPermissionMetadata role defn comment) ->
                     AddRemoteSchemaPermissions remoteSchemaName role defn comment
                 )
          nonInheritedRoles = sourceRoles <> HS.fromList (actionRoles <> remoteSchemaRoles)


      let commonInheritedRoles = HS.intersection (HS.fromList (OMap.keys inheritedRoles)) nonInheritedRoles


      bindA -< do
        unless (HS.null commonInheritedRoles) $ do
          throw400 AlreadyExists $
            "role " <> commaSeparated (map toTxt $ toList commonInheritedRoles) <> " already exists"
        for_ (toList inheritedRoles) $ \(AddInheritedRole _ roleSet) ->
          for_ roleSet $ \role -> do
            unless (role `elem` nonInheritedRoles) $
              throw400 NotFound $ role <<> " not found. An inherited role can only be created out of existing roles"
            when (role `OMap.member` inheritedRoles) $
              throw400 ConstraintError $  role <<> " is an inherited role. An inherited role can only be created out of non-inherited roles"

      -- remote schemas
      let remoteSchemaInvalidationKeys = Inc.selectD #_ikRemoteSchemas invalidationKeys
      remoteSchemaMap <- buildRemoteSchemas -< (remoteSchemaInvalidationKeys, (OMap.elems remoteSchemas))

      -- remote schema permissions
      remoteSchemaCache <- (remoteSchemaMap >- returnA)
        >-> (\info -> (info, M.groupOn _arspRemoteSchema remoteSchemaPermissions)
                     >- alignExtraRemoteSchemaInfo mkRemoteSchemaPermissionMetadataObject)
        >-> (| Inc.keyed (\_ ((remoteSchemaCtx, metadataObj), remoteSchemaPerms) -> do
                   permissionInfo <-
                     buildRemoteSchemaPermissions -< (remoteSchemaCtx, remoteSchemaPerms)
                   returnA -< (remoteSchemaCtx
                               { _rscPermissions = permissionInfo
                               }
                              , metadataObj)
                   )
             |)

      let remoteSchemaCtxMap = M.map fst remoteSchemaMap
      sourcesOutput <-
        (| Inc.keyed (\_ exists ->
             AB.dispatchAnyBackendArrow @BackendMetadata (buildSourceOutput @arr @m)
               -< (invalidationKeys, remoteSchemaCtxMap, exists, inheritedRoles)
           )
        |) (M.fromList $ OMap.toList sources)
        >-> (\infos -> M.catMaybes infos >- returnA)

      -- allow list
      let allowList = allowlists
            & HSIns.toList
            & map _crCollection
            & map (\cn -> maybe [] (_cdQueries . _ccDefinition) $ OMap.lookup cn collections)
            & concat
            & map (queryWithoutTypeNames . getGQLQuery . _lqQuery)
            & HS.fromList

      resolvedEndpoints <- buildInfoMap fst mkEndpointMetadataObject buildEndpoint -< (collections, OMap.toList endpoints)

      -- custom types
      let scalarsMap = mconcat $ map snd $ M.elems sourcesOutput
          sourcesCache = M.map fst sourcesOutput
      maybeResolvedCustomTypes <-
        (| withRecordInconsistency
             ( bindErrorA -< resolveCustomTypes sourcesCache customTypes scalarsMap
             )
         |) (MetadataObject MOCustomTypes $ toJSON customTypes)

      -- actions
      let actionList = OMap.elems actions
      (actionCache, annotatedCustomTypes) <- case maybeResolvedCustomTypes of
        Just resolvedCustomTypes -> do
          actionCache' <- buildActions -< ((resolvedCustomTypes, scalarsMap), actionList)
          returnA -< (actionCache', resolvedCustomTypes)

        -- If the custom types themselves are inconsistent, we can’t really do
        -- anything with actions, so just mark them all inconsistent.
        Nothing -> do
          recordInconsistencies -< ( map mkActionMetadataObject actionList
                                   , "custom types are inconsistent" )
          returnA -< (M.empty, emptyAnnotatedCustomTypes)

      cronTriggersMap <- buildCronTriggers -< ((), OMap.elems cronTriggers)

      let inheritedRolesCache = OMap.toHashMap $ fmap _adrRoleSet inheritedRoles

      returnA -< BuildOutputs
        { _boSources = M.map fst sourcesOutput
        , _boActions = actionCache
        , _boRemoteSchemas = remoteSchemaCache
        , _boAllowlist = allowList
        , _boCustomTypes = annotatedCustomTypes
        , _boCronTriggers = cronTriggersMap
        , _boInheritedRoles = inheritedRolesCache
        , _boEndpoints = resolvedEndpoints
        , _boApiLimits = apiLimits
        , _boMetricsConfig = metricsConfig
        }

    mkEndpointMetadataObject (name, createEndpoint) =
          let objectId = MOEndpoint name
          in MetadataObject objectId (toJSON createEndpoint)

    buildEndpoint
      :: (ArrowChoice arr, ArrowKleisli m arr, MonadError QErr m, ArrowWriter (Seq CollectedInfo) arr)
      => (InsOrdHashMap CollectionName CreateCollection, (EndpointName, CreateEndpoint)) `arr` Maybe (EndpointMetadata GQLQueryWithText)
    buildEndpoint = proc (collections, e@(name, createEndpoint)) -> do
      let endpoint = createEndpoint
          -- QueryReference collName queryName = _edQuery endpoint
          addContext err = "in endpoint " <> toTxt (unEndpointName name) <> ": " <> err
      (| withRecordInconsistency (
        (| modifyErrA (bindErrorA -< resolveEndpoint collections endpoint)
         |) addContext)
       |) (mkEndpointMetadataObject e)

    resolveEndpoint
      :: QErrM m
      => InsOrdHashMap CollectionName CreateCollection
      -> EndpointMetadata QueryReference
      -> m (EndpointMetadata GQLQueryWithText)
    resolveEndpoint collections = traverse $ \(QueryReference collName queryName) -> do
      collection <-
        onNothing
          (OMap.lookup collName collections)
          (throw400 NotExists $ "collection with name " <> toTxt collName <> " does not exist")
      listedQuery <-
        flip onNothing
               (throw400 NotExists
                $ "query with name "
                <> toTxt queryName
                <> " does not exist in collection " <> toTxt collName)
               $ find ((== queryName) . _lqName) (_cdQueries (_ccDefinition collection))

      let
        lq@(GQLQueryWithText lqq) = _lqQuery listedQuery
        ds = G.getExecutableDefinitions $ unGQLQuery $ snd lqq

      case ds of
        [G.ExecutableDefinitionOperation (G.OperationDefinitionTyped d)]
          | G._todType d == G.OperationTypeSubscription ->
              throw405 $ "query with name " <> toTxt queryName <> " is a subscription"
          | otherwise -> pure ()
        [] -> throw400 BadRequest $ "query with name " <> toTxt queryName <> " has no definitions."
        _  -> throw400 BadRequest $ "query with name " <> toTxt queryName <> " has multiple definitions."

      pure lq

    mkEventTriggerMetadataObject
      :: forall b a c
       . Backend b
      => (a, SourceName, c, TableName b, EventTriggerConf)
      -> MetadataObject
    mkEventTriggerMetadataObject (_, source, _, table, eventTriggerConf) =
      let objectId = MOSourceObjId source
                       $ AB.mkAnyBackend
                       $ SMOTableObj table
                       $ MTOTrigger
                       $ etcName eventTriggerConf
          definition = object ["table" .= table, "configuration" .= eventTriggerConf]
      in MetadataObject objectId definition

    mkCronTriggerMetadataObject catalogCronTrigger =
      let definition = toJSON catalogCronTrigger
      in MetadataObject (MOCronTrigger (ctName catalogCronTrigger))
                        definition

    mkActionMetadataObject (ActionMetadata name comment defn _) =
      MetadataObject (MOAction name) (toJSON $ CreateAction name defn comment)

    mkRemoteSchemaMetadataObject remoteSchema =
      MetadataObject (MORemoteSchema (_rsmName remoteSchema)) (toJSON remoteSchema)

    alignExtraRemoteSchemaInfo
      :: forall a b arr
       . (ArrowChoice arr, Inc.ArrowDistribute arr, ArrowWriter (Seq CollectedInfo) arr)
      => (b -> MetadataObject)
      -> ( M.HashMap RemoteSchemaName a
         , M.HashMap RemoteSchemaName [b]
         ) `arr` M.HashMap RemoteSchemaName (a, [b])
    alignExtraRemoteSchemaInfo mkMetadataObject = proc (baseInfo, extraInfo) -> do
      combinedInfo <-
        (| Inc.keyed (\remoteSchemaName infos -> combine -< (remoteSchemaName, infos))
        |) (align baseInfo extraInfo)
      returnA -< M.catMaybes combinedInfo
      where
        combine :: (RemoteSchemaName, These a [b]) `arr` Maybe (a, [b])
        combine = proc (remoteSchemaName, infos) -> case infos of
          This  base        -> returnA -< Just (base, [])
          These base extras -> returnA -< Just (base, extras)
          That       extras -> do
            let errorMessage = "remote schema  " <> unRemoteSchemaName remoteSchemaName <<> " does not exist"
            recordInconsistencies -< (map mkMetadataObject extras, errorMessage)
            returnA -< Nothing

    buildRemoteSchemaPermissions
      :: ( ArrowChoice arr, Inc.ArrowDistribute arr, ArrowWriter (Seq CollectedInfo) arr
         , Inc.ArrowCache m arr, MonadError QErr m)
      => (RemoteSchemaCtx, [AddRemoteSchemaPermissions]) `arr` (M.HashMap RoleName IntrospectionResult)
    buildRemoteSchemaPermissions = buildInfoMap _arspRole mkRemoteSchemaPermissionMetadataObject buildRemoteSchemaPermission
      where
        buildRemoteSchemaPermission = proc (remoteSchemaCtx, remoteSchemaPerm) -> do
          let AddRemoteSchemaPermissions rsName roleName defn _ = remoteSchemaPerm
              metadataObject = mkRemoteSchemaPermissionMetadataObject remoteSchemaPerm
              schemaObject = SORemoteSchemaPermission rsName roleName
              providedSchemaDoc = _rspdSchema defn
              addPermContext err = "in remote schema permission for role " <> roleName <<> ": " <> err
          (| withRecordInconsistency (
             (| modifyErrA (do
                 bindErrorA -< when (roleName == adminRoleName) $
                   throw400 ConstraintViolation $ "cannot define permission for admin role"
                 (resolvedSchemaIntrospection, dependencies) <-
                    bindErrorA -< resolveRoleBasedRemoteSchema providedSchemaDoc remoteSchemaCtx
                 recordDependencies -< (metadataObject, schemaObject, dependencies)
                 returnA -< resolvedSchemaIntrospection)
             |) addPermContext)
           |) metadataObject

    buildTableEventTriggers
      :: forall arr m b
       . ( ArrowChoice arr, Inc.ArrowDistribute arr, ArrowWriter (Seq CollectedInfo) arr
         , Inc.ArrowCache m arr, MonadIO m, MonadError QErr m, MonadBaseControl IO m
         , MonadReader BuildReason m, HasServerConfigCtx m, BackendMetadata b)
      => ( SourceName, SourceConfig b, TableCoreInfo b
         , [EventTriggerConf], Inc.Dependency Inc.InvalidationKey
         ) `arr` EventTriggerInfoMap
    buildTableEventTriggers = proc (source, sourceConfig, tableInfo, eventTriggerConfs, metadataInvalidationKey) ->
      buildInfoMap (etcName . (^. _5)) mkEventTriggerMetadataObject buildEventTrigger
        -< (tableInfo, map (metadataInvalidationKey, source, sourceConfig, _tciName tableInfo,) eventTriggerConfs)
      where
        buildEventTrigger = proc (tableInfo, (metadataInvalidationKey, source, sourceConfig, table, eventTriggerConf)) -> do
          let triggerName = etcName eventTriggerConf
              metadataObject = mkEventTriggerMetadataObject (metadataInvalidationKey, source, sourceConfig, table, eventTriggerConf)
              schemaObjectId = SOSourceObj source
                                 $ AB.mkAnyBackend
                                 $ SOITableObj table
                                 $ TOTrigger triggerName
              addTriggerContext e = "in event trigger " <> triggerName <<> ": " <> e
          (| withRecordInconsistency (
             (| modifyErrA (do
                  (info, dependencies) <- bindErrorA -< buildEventTriggerInfo env source table eventTriggerConf
                  let tableColumns = M.mapMaybe (^? _FIColumn) (_tciFieldInfoMap tableInfo)
                  recreateTriggerIfNeeded -< (metadataInvalidationKey, table, M.elems tableColumns, triggerName, etcDefinition eventTriggerConf, sourceConfig)
                  recordDependencies -< (metadataObject, schemaObjectId, dependencies)
                  returnA -< info)
             |) (addTableContext table . addTriggerContext))
           |) metadataObject

        recreateTriggerIfNeeded = Inc.cache proc (metadataInvalidationKey, tableName, tableColumns
                                                 , triggerName, triggerDefinition, sourceConfig) -> do
          -- We want to make sure we re-create event triggers in postgres database on
          -- `reload_metadata` metadata query request
          Inc.dependOn -< metadataInvalidationKey
          bindA -< do
            buildReason <- ask
            serverConfigCtx <- askServerConfigCtx
            -- we don't modify the existing event trigger definitions in the maintenance mode
            when (buildReason == CatalogUpdate && _sccMaintenanceMode serverConfigCtx == MaintenanceModeDisabled)
              $ liftEitherM
              $ createTableEventTrigger
                  serverConfigCtx
                  sourceConfig
                  tableName
                  tableColumns
                  triggerName
                  triggerDefinition

    buildCronTriggers
      :: ( ArrowChoice arr
         , Inc.ArrowDistribute arr
         , ArrowWriter (Seq CollectedInfo) arr
         , Inc.ArrowCache m arr
         , MonadError QErr m)
      => ((),[CronTriggerMetadata])
         `arr` HashMap TriggerName CronTriggerInfo
    buildCronTriggers = buildInfoMap ctName mkCronTriggerMetadataObject buildCronTrigger
      where
        buildCronTrigger = proc (_,cronTrigger) -> do
          let triggerName = triggerNameToTxt $ ctName cronTrigger
              addCronTriggerContext e = "in cron trigger " <> triggerName <> ": " <> e
          (| withRecordInconsistency (
            (| modifyErrA (bindErrorA -< resolveCronTrigger env cronTrigger)
             |) addCronTriggerContext)
           |) (mkCronTriggerMetadataObject cronTrigger)

    buildActions
      :: ( ArrowChoice arr, Inc.ArrowDistribute arr, Inc.ArrowCache m arr
         , ArrowWriter (Seq CollectedInfo) arr)
      => ( (AnnotatedCustomTypes, DMap.DMap BackendTag ScalarSet)
         , [ActionMetadata]
         ) `arr` HashMap ActionName ActionInfo
    buildActions = buildInfoMap _amName mkActionMetadataObject buildAction
      where
        buildAction = proc ((resolvedCustomTypes, scalarsMap), action) -> do
          let ActionMetadata name comment def actionPermissions = action
              addActionContext e = "in action " <> name <<> "; " <> e
          (| withRecordInconsistency (
             (| modifyErrA (do
                  (resolvedDef, outObject) <- liftEitherA <<< bindA -<
                    runExceptT $ resolveAction env resolvedCustomTypes def scalarsMap
                  let permissionInfos = map (ActionPermissionInfo . _apmRole) actionPermissions
                      permissionMap = mapFromL _apiRole permissionInfos
                      forwardClientHeaders = _adForwardClientHeaders resolvedDef
                      outputType = unGraphQLType $ _adOutputType def
                  returnA -< ActionInfo name (outputType, outObject) resolvedDef permissionMap forwardClientHeaders comment)
              |) addActionContext)
           |) (mkActionMetadataObject action)

    buildRemoteSchemas
      :: ( ArrowChoice arr, Inc.ArrowDistribute arr, ArrowWriter (Seq CollectedInfo) arr
         , Inc.ArrowCache m arr , MonadIO m, MonadUnique m, HasHttpManagerM m )
      => ( Inc.Dependency (HashMap RemoteSchemaName Inc.InvalidationKey)
         , [RemoteSchemaMetadata]
         ) `arr` HashMap RemoteSchemaName (RemoteSchemaCtx, MetadataObject)
    buildRemoteSchemas =
      buildInfoMapPreservingMetadata _rsmName mkRemoteSchemaMetadataObject buildRemoteSchema
      where
        -- We want to cache this call because it fetches the remote schema over HTTP, and we don’t
        -- want to re-run that if the remote schema definition hasn’t changed.
        buildRemoteSchema = Inc.cache proc (invalidationKeys, remoteSchema@(RemoteSchemaMetadata name defn comment _)) -> do
          let addRemoteSchemaQuery = AddRemoteSchemaQuery name defn comment
          Inc.dependOn -< Inc.selectKeyD name invalidationKeys
          (| withRecordInconsistency (liftEitherA <<< bindA -<
               runExceptT $ addRemoteSchemaP2Setup env addRemoteSchemaQuery)
           |) (mkRemoteSchemaMetadataObject remoteSchema)

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
