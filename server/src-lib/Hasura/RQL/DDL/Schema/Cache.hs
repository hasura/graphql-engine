{-# LANGUAGE Arrows           #-}
{-# LANGUAGE OverloadedLabels #-}

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
  , CacheRWT
  , runCacheRWT

  , withMetadataCheck
  ) where

import           Hasura.Prelude

import qualified Data.Environment                         as Env
import qualified Data.HashMap.Strict.Extended             as M
import qualified Data.HashMap.Strict.InsOrd               as OMap
import qualified Data.HashSet                             as HS
import qualified Data.HashSet.InsOrd                      as HSIns

import           Control.Arrow.Extended
import           Control.Lens                             hiding ((.=))
import           Control.Monad.Unique
import           Data.Aeson
import           Data.Text.Extended

import qualified Hasura.Incremental                       as Inc

import           Hasura.Backends.Postgres.Connection
import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.GraphQL.Execute.Types
import           Hasura.GraphQL.Schema                    (buildGQLContext)
import           Hasura.Metadata.Class
import           Hasura.RQL.DDL.Action
import           Hasura.RQL.DDL.CustomTypes
import           Hasura.RQL.DDL.Deps
import           Hasura.RQL.DDL.EventTrigger
import           Hasura.RQL.DDL.RemoteSchema
import           Hasura.RQL.DDL.RemoteSchema.Permission     (resolveRoleBasedRemoteSchema)
import           Hasura.RQL.DDL.ScheduledTrigger
import           Hasura.RQL.DDL.Schema.Cache.Common
import           Hasura.RQL.DDL.Schema.Cache.Dependencies
import           Hasura.RQL.DDL.Schema.Cache.Fields
import           Hasura.RQL.DDL.Schema.Cache.Permission
import           Hasura.RQL.DDL.Schema.Common
import           Hasura.RQL.DDL.Schema.Diff
import           Hasura.RQL.DDL.Schema.Function
import           Hasura.RQL.DDL.Schema.Table
import           Hasura.RQL.Types                         hiding (fmFunction, tmTable)
import           Hasura.Server.Version                    (HasVersion)

import           Hasura.Session

buildRebuildableSchemaCache
  :: ( HasVersion
     , MonadIO m
     , MonadTx m
     , HasHttpManager m
     , HasSQLGenCtx m
     , HasRemoteSchemaPermsCtx m
     )
  => Env.Environment
  -> Metadata
  -> m RebuildableSchemaCache
buildRebuildableSchemaCache env metadata = do
  result <-  runCacheBuild $ flip runReaderT CatalogSync $
    Inc.build (buildSchemaCacheRule env) (metadata, initialInvalidationKeys)
  pure $ RebuildableSchemaCache (Inc.result result) initialInvalidationKeys (Inc.rebuildRule result)

newtype CacheRWT m a
  -- The CacheInvalidations component of the state could actually be collected using WriterT, but
  -- WriterT implementations prior to transformers-0.5.6.0 (which added
  -- Control.Monad.Trans.Writer.CPS) are leaky, and we don’t have that yet.
  = CacheRWT (StateT (RebuildableSchemaCache, CacheInvalidations) m a)
  deriving
    ( Functor, Applicative, Monad, MonadIO, MonadUnique, MonadReader r, MonadError e, MonadTx
    , UserInfoM, HasHttpManager, HasSQLGenCtx, HasSystemDefined, MonadMetadataStorage
    , HasRemoteSchemaPermsCtx, MonadScheduledEvents)

runCacheRWT
  :: Functor m
  => RebuildableSchemaCache -> CacheRWT m a -> m (a, RebuildableSchemaCache, CacheInvalidations)
runCacheRWT cache (CacheRWT m) =
  runStateT m (cache, mempty) <&> \(v, (newCache, invalidations)) -> (v, newCache, invalidations)

instance MonadTrans CacheRWT where
  lift = CacheRWT . lift

instance (Monad m) => TableCoreInfoRM 'Postgres (CacheRWT m)
instance (Monad m) => CacheRM (CacheRWT m) where
  askSchemaCache = CacheRWT $ gets (lastBuiltSchemaCache . fst)

instance (MonadIO m, MonadTx m, HasHttpManager m, HasSQLGenCtx m, HasRemoteSchemaPermsCtx m) => CacheRWM (CacheRWT m) where
  buildSchemaCacheWithOptions buildReason invalidations metadata = CacheRWT do
    (RebuildableSchemaCache _ invalidationKeys rule, oldInvalidations) <- get
    let newInvalidationKeys = invalidateKeys invalidations invalidationKeys
    result <- lift $ runCacheBuild $ flip runReaderT buildReason $
      Inc.build rule (metadata, newInvalidationKeys)
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

buildSchemaCacheRule
  -- Note: by supplying BuildReason via MonadReader, it does not participate in caching, which is
  -- what we want!
  :: ( HasVersion, ArrowChoice arr, Inc.ArrowDistribute arr, Inc.ArrowCache m arr
     , MonadIO m, MonadUnique m, MonadTx m
     , MonadReader BuildReason m, HasHttpManager m, HasSQLGenCtx m, HasRemoteSchemaPermsCtx m)
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
    , _boTables    resolvedOutputs
    , _boFunctions resolvedOutputs
    , _boRemoteSchemas resolvedOutputs
    , _boActions resolvedOutputs
    , _actNonObjects $ _boCustomTypes resolvedOutputs
    )

  -- Step 4: Build the relay GraphQL schema
  (relayContext, relaySchemaInconsistentObjects) <- runWriterA buildGQLContext -<
    ( QueryRelay
    , _boTables    resolvedOutputs
    , _boFunctions resolvedOutputs
    , _boRemoteSchemas resolvedOutputs
    , _boActions resolvedOutputs
    , _actNonObjects $ _boCustomTypes resolvedOutputs
    )

  returnA -< SchemaCache
    { scTables = _boTables resolvedOutputs
    , scActions = _boActions resolvedOutputs
    , scFunctions = _boFunctions resolvedOutputs
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
    , scInconsistentObjs =
           inconsistentObjects
        <> dependencyInconsistentObjects
        <> toList gqlSchemaInconsistentObjects
        <> toList relaySchemaInconsistentObjects
    }
  where
    buildAndCollectInfo
      :: ( ArrowChoice arr, Inc.ArrowDistribute arr, Inc.ArrowCache m arr
         , ArrowWriter (Seq CollectedInfo) arr, MonadIO m, MonadUnique m, MonadTx m, MonadReader BuildReason m
         , HasHttpManager m, HasSQLGenCtx m )
      => (Metadata, Inc.Dependency InvalidationKeys) `arr` BuildOutputs
    buildAndCollectInfo = proc (metadata, invalidationKeys) -> do
      let Metadata tables functions remoteSchemas collections allowlists
            customTypes actions cronTriggers = metadata
          (tableInputs, nonColumnInputs, permissions) = unzip3 $ map mkTableInputs $ OMap.elems tables
          eventTriggers = map (_tmTable &&& (OMap.elems . _tmEventTriggers)) (OMap.elems tables)
          -- HashMap k a -> HashMap k b -> HashMap k (a, b)
          alignTableMap = M.intersectionWith (,)
          remoteSchemaPermissions =
            let remoteSchemaPermsList = OMap.toList $ _rsmPermissions <$> remoteSchemas
            in concat $ flip map remoteSchemaPermsList $
                 (\(remoteSchemaName, remoteSchemaPerms) ->
                    flip map remoteSchemaPerms $ \(RemoteSchemaPermissionMetadata role defn comment) ->
                     AddRemoteSchemaPermissions remoteSchemaName role defn comment
                 )

      pgTables <- bindA -< fetchTableMetadata
      pgFunctions <- bindA -< fetchFunctionMetadata
      pgScalars <- bindA -< fetchPgScalars

      -- tables
      tableRawInfos <- buildTableCache -< (pgTables, tableInputs, Inc.selectD #_ikMetadata invalidationKeys)

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

      -- relationships and computed fields
      let nonColumnsByTable = mapFromL _nctiTable nonColumnInputs
      tableCoreInfos <-
        (| Inc.keyed (\_ (tableRawInfo, nonColumnInput) -> do
             let columns = _tciFieldInfoMap tableRawInfo
             allFields <- addNonColumnFields -< (tableRawInfos, columns, M.map fst remoteSchemaMap, pgFunctions, nonColumnInput)
             returnA -< (tableRawInfo {_tciFieldInfoMap = allFields}))
         |) (tableRawInfos `alignTableMap` nonColumnsByTable)

      -- permissions and event triggers
      tableCoreInfosDep <- Inc.newDependency -< tableCoreInfos
      tableCache <-
        (| Inc.keyed (\_ ((tableCoreInfo, permissionInputs), (_, eventTriggerConfs)) -> do
             let tableFields = _tciFieldInfoMap tableCoreInfo
             permissionInfos <- buildTablePermissions -< (tableCoreInfosDep, tableFields, permissionInputs)
             eventTriggerInfos <- buildTableEventTriggers -< (tableCoreInfo, eventTriggerConfs)
             returnA -< TableInfo tableCoreInfo permissionInfos eventTriggerInfos
            )
         |) (tableCoreInfos `alignTableMap` mapFromL _tpiTable permissions `alignTableMap` mapFromL fst eventTriggers)

      -- sql functions
      functionCache <- (mapFromL _fmFunction (OMap.elems functions) >- returnA)
        >-> (| Inc.keyed (\_ (FunctionMetadata qf config) -> do
                 let systemDefined = SystemDefined False
                     definition = toJSON $ TrackFunction qf
                     metadataObject = MetadataObject (MOFunction qf) definition
                     schemaObject = SOFunction qf
                     addFunctionContext e = "in function " <> qf <<> ": " <> e
                 (| withRecordInconsistency (
                    (| modifyErrA (do
                         let funcDefs = fromMaybe [] $ M.lookup qf pgFunctions
                         rawfi <- bindErrorA -< handleMultipleFunctions qf funcDefs
                         (fi, dep) <- bindErrorA -< mkFunctionInfo qf systemDefined config rawfi
                         recordDependencies -< (metadataObject, schemaObject, [dep])
                         returnA -< fi)
                    |) addFunctionContext)
                  |) metadataObject) |)
        >-> (\infos -> M.catMaybes infos >- returnA)

      -- allow list
      let allowList = allowlists
            & HSIns.toList
            & map _crCollection
            & map (\cn -> maybe [] (_cdQueries . _ccDefinition) $ OMap.lookup cn collections)
            & concat
            & map (queryWithoutTypeNames . getGQLQuery . _lqQuery)
            & HS.fromList

      -- custom types
      maybeResolvedCustomTypes <-
        (| withRecordInconsistency
             (bindErrorA -< resolveCustomTypes tableCache customTypes pgScalars)
         |) (MetadataObject MOCustomTypes $ toJSON customTypes)

      -- -- actions
      let actionList = OMap.elems actions
      (actionCache, annotatedCustomTypes) <- case maybeResolvedCustomTypes of
        Just resolvedCustomTypes -> do
          actionCache' <- buildActions -< ((resolvedCustomTypes, pgScalars), actionList)
          returnA -< (actionCache', resolvedCustomTypes)

        -- If the custom types themselves are inconsistent, we can’t really do
        -- anything with actions, so just mark them all inconsistent.
        Nothing -> do
          recordInconsistencies -< ( map mkActionMetadataObject actionList
                                   , "custom types are inconsistent" )
          returnA -< (M.empty, emptyAnnotatedCustomTypes)

      cronTriggersMap <- buildCronTriggers -< ((), OMap.elems cronTriggers)

      returnA -< BuildOutputs
        { _boTables = tableCache
        , _boActions = actionCache
        , _boFunctions = functionCache
        , _boRemoteSchemas = remoteSchemaCache
        , _boAllowlist = allowList
        , _boCustomTypes = annotatedCustomTypes
        , _boCronTriggers = cronTriggersMap
        }

    mkEventTriggerMetadataObject (table, eventTriggerConf) =
      let objectId = MOTableObj table $ MTOTrigger $ etcName eventTriggerConf
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
         , Inc.ArrowCache m arr, MonadTx m)
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
      :: ( ArrowChoice arr, Inc.ArrowDistribute arr, ArrowWriter (Seq CollectedInfo) arr
         , Inc.ArrowCache m arr, MonadTx m, MonadReader BuildReason m, HasSQLGenCtx m )
      => (TableCoreInfo 'Postgres, [EventTriggerConf]) `arr` EventTriggerInfoMap
    buildTableEventTriggers = proc (tableInfo, eventTriggerConfs) ->
      buildInfoMap (etcName . snd) mkEventTriggerMetadataObject buildEventTrigger
        -< (tableInfo, map (_tciName tableInfo,) eventTriggerConfs)
      where
        buildEventTrigger = proc (tableInfo, (table, eventTriggerConf)) -> do
          let triggerName = etcName eventTriggerConf
              metadataObject = mkEventTriggerMetadataObject (table, eventTriggerConf)
              schemaObjectId = SOTableObj table $ TOTrigger triggerName
              addTriggerContext e = "in event trigger " <> triggerName <<> ": " <> e
          (| withRecordInconsistency (
             (| modifyErrA (do
                  (info, dependencies) <- bindErrorA -< mkEventTriggerInfo env table eventTriggerConf
                  let tableColumns = M.mapMaybe (^? _FIColumn) (_tciFieldInfoMap tableInfo)
                  recreateViewIfNeeded -< (table, tableColumns, triggerName, etcDefinition eventTriggerConf)
                  recordDependencies -< (metadataObject, schemaObjectId, dependencies)
                  returnA -< info)
             |) (addTableContext table . addTriggerContext))
           |) metadataObject

        recreateViewIfNeeded = Inc.cache $
          arrM \(tableName, tableColumns, triggerName, triggerDefinition) -> do
            buildReason <- ask
            when (buildReason == CatalogUpdate) $ do
              liftTx $ delTriggerQ triggerName -- executes DROP IF EXISTS.. sql
              mkAllTriggersQ triggerName tableName (M.elems tableColumns) triggerDefinition

    buildCronTriggers
      :: ( ArrowChoice arr
         , Inc.ArrowDistribute arr
         , ArrowWriter (Seq CollectedInfo) arr
         , Inc.ArrowCache m arr
         , MonadTx m)
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
      => ( (AnnotatedCustomTypes 'Postgres, HashSet PGScalarType)
         , [ActionMetadata]
         ) `arr` HashMap ActionName (ActionInfo 'Postgres)
    buildActions = buildInfoMap _amName mkActionMetadataObject buildAction
      where
        buildAction = proc ((resolvedCustomTypes, pgScalars), action) -> do
          let ActionMetadata name comment def actionPermissions = action
              addActionContext e = "in action " <> name <<> "; " <> e
          (| withRecordInconsistency (
             (| modifyErrA (do
                  (resolvedDef, outObject) <- liftEitherA <<< bindA -<
                    runExceptT $ resolveAction env resolvedCustomTypes def pgScalars
                  let permissionInfos = map (ActionPermissionInfo . _apmRole) actionPermissions
                      permissionMap = mapFromL _apiRole permissionInfos
                  returnA -< ActionInfo name outObject resolvedDef permissionMap comment)
              |) addActionContext)
           |) (mkActionMetadataObject action)

    buildRemoteSchemas
      :: ( ArrowChoice arr, Inc.ArrowDistribute arr, ArrowWriter (Seq CollectedInfo) arr
         , Inc.ArrowCache m arr , MonadIO m, MonadUnique m, HasHttpManager m )
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

-- | @'withMetadataCheck' cascade action@ runs @action@ and checks if the schema changed as a
-- result. If it did, it checks to ensure the changes do not violate any integrity constraints, and
-- if not, incorporates them into the schema cache.
withMetadataCheck
  :: (MonadTx m, CacheRWM m, HasSQLGenCtx m, MetadataM m)
  => Bool -> m a -> m a
withMetadataCheck cascade action = do
  sc <- askSchemaCache
  let preActionTables = scTables sc
      preActionFunctions = scFunctions sc
  -- Drop event triggers so no interference is caused to the sql query
  forM_ (M.elems preActionTables) $ \tableInfo -> do
    let eventTriggers = _tiEventTriggerInfoMap tableInfo
    forM_ (M.keys eventTriggers) (liftTx . delTriggerQ)

  -- Get the metadata before the sql query, everything, need to filter this
  (preActionTableMeta, preActionFunctionMeta) <- fetchMeta preActionTables preActionFunctions

  -- Run the action
  actionResult <- action

  -- Get the metadata after the sql query
  (postActionTableMeta, postActionFunctionMeta) <- fetchMeta preActionTables preActionFunctions

  let preActionTableMeta' = filter (flip M.member preActionTables . tmTable) preActionTableMeta
      schemaDiff = getSchemaDiff preActionTableMeta' postActionTableMeta
      FunctionDiff droppedFuncs alteredFuncs = getFuncDiff preActionFunctionMeta postActionFunctionMeta
      overloadedFuncs = getOverloadedFuncs (M.keys preActionFunctions) postActionFunctionMeta

  -- Do not allow overloading functions
  unless (null overloadedFuncs) $
    throw400 NotSupported $ "the following tracked function(s) cannot be overloaded: "
    <> commaSeparated overloadedFuncs

  indirectDeps <- getSchemaChangeDeps schemaDiff

  -- Report back with an error if cascade is not set
  when (indirectDeps /= [] && not cascade) $ reportDepsExt indirectDeps []

  metadataUpdater <- execWriterT $ do
    -- Purge all the indirect dependents from state
    mapM_ (purgeDependentObject >=> tell) indirectDeps

    -- Purge all dropped functions
    let purgedFuncs = flip mapMaybe indirectDeps $ \case
            SOFunction qf -> Just qf
            _             -> Nothing

    forM_ (droppedFuncs \\ purgedFuncs) $ tell . dropFunctionInMetadata

    -- Process altered functions
    forM_ alteredFuncs $ \(qf, newTy) -> do
      when (newTy == FTVOLATILE) $
        throw400 NotSupported $
        "type of function " <> qf <<> " is altered to \"VOLATILE\" which is not supported now"

    -- update the metadata with the changes
    processSchemaChanges preActionTables schemaDiff

  withNewInconsistentObjsCheck $ buildSchemaCache metadataUpdater

  postActionSchemaCache <- askSchemaCache

  -- Recreate event triggers in hdb_catalog
  let postActionTables = scTables postActionSchemaCache
  forM_ (M.elems postActionTables) $ \(TableInfo coreInfo _ eventTriggers) -> do
          let table = _tciName coreInfo
              columns = getCols $ _tciFieldInfoMap coreInfo
          forM_ (M.toList eventTriggers) $ \(triggerName, eti) -> do
            let opsDefinition = etiOpsDef eti
            mkAllTriggersQ triggerName table columns opsDefinition

  pure actionResult
  where
    processSchemaChanges
      :: ( MonadError QErr m
         , CacheRM m
         , MonadWriter MetadataModifier m
         )
      => TableCache 'Postgres -> SchemaDiff 'Postgres -> m ()
    processSchemaChanges preActionTables schemaDiff = do
      -- Purge the dropped tables
      forM_ droppedTables $
        \tn -> tell $ MetadataModifier $ metaTables %~ OMap.delete tn

      for_ alteredTables $ \(oldQtn, tableDiff) -> do
        ti <- onNothing
          (M.lookup oldQtn preActionTables)
          (throw500 $ "old table metadata not found in cache : " <>> oldQtn)
        processTableChanges (_tiCoreInfo ti) tableDiff
      where
        SchemaDiff droppedTables alteredTables = schemaDiff

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
