{-# LANGUAGE Arrows #-}

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
  , purgeDependentObject
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict.Extended       as M
import qualified Data.HashSet                       as HS
import qualified Data.Text                          as T
import qualified Data.Sequence as Seq
import qualified Database.PG.Query                  as Q

import           Data.Aeson
import Control.Arrow
import Control.Lens hiding ((.=))
import Control.Monad.Unique

import qualified Hasura.GraphQL.Schema              as GS
import qualified Hasura.GraphQL.Context as GC
import qualified Hasura.Incremental                 as Inc

import           Hasura.Db
import           Hasura.GraphQL.RemoteServer
import           Hasura.RQL.DDL.ComputedField
import           Hasura.RQL.DDL.Deps
import           Hasura.RQL.DDL.EventTrigger
import           Hasura.RQL.DDL.Permission
import           Hasura.RQL.DDL.Permission.Internal
import           Hasura.RQL.DDL.Relationship
import           Hasura.RQL.DDL.RemoteSchema
import           Hasura.RQL.DDL.Schema.Catalog
import           Hasura.RQL.DDL.Schema.Diff
import           Hasura.RQL.DDL.Schema.Function
import           Hasura.RQL.DDL.Schema.Table
import           Hasura.RQL.DDL.Utils
import           Hasura.RQL.Types
import           Hasura.RQL.Types.Catalog
import           Hasura.RQL.Types.QueryCollection
import           Hasura.SQL.Types

-- | A map used to explicitly invalidate part of the build cache, which is most useful for external
-- resources (currently only remote schemas). The 'InvalidationKey' values it contains are used as
-- inputs to build rules, so setting an entry to a fresh 'InvalidationKey' forces it to be
-- re-executed.
type InvalidationMap = HashMap RemoteSchemaName InvalidationKey
type InvalidationKey = Unique

data BuildInputs
  = BuildInputs
  { _biReason :: !BuildReason
  , _biCatalogMetadata :: !CatalogMetadata
  , _biInvalidationMap :: !InvalidationMap
  } deriving (Eq)

-- | The direct output of 'buildSchemaCacheRule'. Contains most of the things necessary to build a
-- schema cache, but dependencies and inconsistent metadata objects are collected via a separate
-- 'MonadWriter' side channel.
data BuildOutputs
  = BuildOutputs
  { _boTables            :: !TableCache
  , _boFunctions         :: !FunctionCache
  , _boRemoteSchemas     :: !RemoteSchemaMap
  , _boAllowlist         :: !(HS.HashSet GQLQuery)
  , _boGCtxMap           :: !GC.GCtxMap
  , _boDefaultRemoteGCtx :: !GC.GCtx
  } deriving (Show, Eq)
$(makeLensesFor
  [ ("_boTables", "boTables")
  , ("_boFunctions", "boFunctions")
  , ("_boRemoteSchemas", "boRemoteSchemas")
  ] ''BuildOutputs)

data RebuildableSchemaCache m
  = RebuildableSchemaCache
  { lastBuiltSchemaCache :: !SchemaCache
  , _rscInvalidationMap :: !InvalidationMap
  , _rscRebuild :: !(Inc.Rule (ReaderT BuildReason m) (CatalogMetadata, InvalidationMap) SchemaCache)
  }
$(makeLensesFor [("_rscInvalidationMap", "rscInvalidationMap")] ''RebuildableSchemaCache)

buildRebuildableSchemaCache
  :: (MonadIO m, MonadTx m, HasHttpManager m, HasSQLGenCtx m)
  => m (RebuildableSchemaCache m)
buildRebuildableSchemaCache = do
  catalogMetadata <- liftTx fetchCatalogData
  result <- flip runReaderT CatalogSync $ Inc.build buildSchemaCacheRule (catalogMetadata, M.empty)
  pure $ RebuildableSchemaCache (Inc.result result) M.empty (Inc.rebuildRule result)

newtype CacheRWT m a
  = CacheRWT { unCacheRWT :: StateT (RebuildableSchemaCache m) m a }
  deriving
    ( Functor, Applicative, Monad, MonadIO, MonadReader r, MonadError e, MonadWriter w, MonadTx
    , UserInfoM, HasHttpManager, HasSQLGenCtx, HasSystemDefined )

runCacheRWT :: RebuildableSchemaCache m -> CacheRWT m a -> m (a, RebuildableSchemaCache m)
runCacheRWT cache = flip runStateT cache . unCacheRWT

instance MonadTrans CacheRWT where
  lift = CacheRWT . lift

instance (Monad m) => TableCoreInfoRM (CacheRWT m)
instance (Monad m) => CacheRM (CacheRWT m) where
  askSchemaCache = CacheRWT $ gets lastBuiltSchemaCache

instance (MonadIO m, MonadTx m, MonadUnique m) => CacheRWM (CacheRWT m) where
  buildSchemaCacheWithOptions buildReason = CacheRWT do
    RebuildableSchemaCache _ invalidationMap rule <- get
    catalogMetadata <- liftTx fetchCatalogData
    result <- lift $ flip runReaderT buildReason $ Inc.build rule (catalogMetadata, invalidationMap)
    let schemaCache = Inc.result result
        prunedInvalidationMap = pruneInvalidationMap schemaCache invalidationMap
    put $ RebuildableSchemaCache schemaCache prunedInvalidationMap (Inc.rebuildRule result)
    where
      pruneInvalidationMap schemaCache = M.filterWithKey \name _ ->
        M.member name (scRemoteSchemas schemaCache)

  invalidateCachedRemoteSchema name = CacheRWT do
    unique <- newUnique
    assign (rscInvalidationMap . at name) (Just unique)

buildSchemaCacheRule
  -- Note: by supplying BuildReason via MonadReader, it does not participate in caching, which is
  -- what we want!
  :: (MonadIO m, MonadTx m, MonadReader BuildReason m, HasHttpManager m, HasSQLGenCtx m)
  => Inc.Rule m (CatalogMetadata, InvalidationMap) SchemaCache
buildSchemaCacheRule = proc inputs -> do
  (outputs, collectedInfo) <- Inc.mapRuleS runWriterT buildAndCollectInfo -< inputs
  let (inconsistentObjects, unresolvedDependencies) = partitionCollectedInfo collectedInfo
  (resolvedOutputs, extraInconsistentObjects, resolvedDependencies) <-
    resolveDependencies -< (outputs, unresolvedDependencies)
  returnA -< SchemaCache
    { scTables = _boTables resolvedOutputs
    , scFunctions = _boFunctions resolvedOutputs
    , scRemoteSchemas = _boRemoteSchemas resolvedOutputs
    , scAllowlist = _boAllowlist resolvedOutputs
    , scGCtxMap = _boGCtxMap resolvedOutputs
    , scDefaultRemoteGCtx = _boDefaultRemoteGCtx resolvedOutputs
    , scDepMap = resolvedDependencies
    , scInconsistentObjs = inconsistentObjects <> extraInconsistentObjects
    }
  where
    buildAndCollectInfo
      :: ( MonadIO m
         , MonadTx m
         , MonadReader BuildReason m
         , MonadWriter (Seq CollectedInfo) m
         , HasHttpManager m
         , HasSQLGenCtx m
         )
      => Inc.Rule m (CatalogMetadata, InvalidationMap) BuildOutputs
    buildAndCollectInfo = Inc.rule \(catalogMetadata, invalidationMap) -> do
      buildReason <- ask
      when (buildReason == CatalogUpdate) $
        liftTx $ Q.catchE defaultTxErrorHandler clearHdbViews

      let CatalogMetadata tables relationships permissions
            eventTriggers remoteSchemas functions fkeys' allowlistDefs
            computedFields = catalogMetadata

      -- tables
      tableRawInfos <- buildTableCache tables
      let tableNames = HS.fromList $ M.keys tableRawInfos

      -- relationships and computed fields
      let relationshipsByTable = M.groupOn _crTable relationships
          computedFieldsByTable = M.groupOn (_afcTable . _cccComputedField) computedFields
          fkeys = HS.fromList fkeys'
      tableCoreInfos <- tableRawInfos
        &   alignExtraTableInfo mkRelationshipMetadataObject relationshipsByTable
        >>= alignExtraTableInfo mkComputedFieldMetadataObject computedFieldsByTable
        >>= traverse \((tableRawInfo, tableRelationships), tableComputedFields) -> do
              let name = _tciName tableRawInfo
                  columns = _tciFieldInfoMap tableRawInfo
              allFields <- addNonColumnFields fkeys tableNames name columns tableRelationships tableComputedFields
              pure tableRawInfo { _tciFieldInfoMap = allFields }

      -- permissions and event triggers
      tableCache <- tableCoreInfos
        &   alignExtraTableInfo mkPermissionMetadataObject (M.groupOn _cpTable permissions)
        >>= alignExtraTableInfo mkEventTriggerMetadataObject (M.groupOn _cetTable eventTriggers)
        >>= traverse \((tableCoreInfo, tablePermissions), tableEventTriggers) -> do
              permissionInfos <- buildTablePermissions tableCoreInfos tableCoreInfo tablePermissions
              eventTriggerInfos <- buildTableEventTriggers tableCoreInfos tableEventTriggers
              pure TableInfo
                { _tiCoreInfo = tableCoreInfo
                , _tiRolePermInfoMap = permissionInfos
                , _tiEventTriggerInfoMap = eventTriggerInfos
                }

      -- sql functions
      functionCache <- M.catMaybes <$>
        for (mapFromL _cfFunction functions) \(CatalogFunction qf systemDefined config funcDefs) -> do
          let definition = toJSON $ TrackFunction qf
              metadataObject = MetadataObject (MOFunction qf) definition
              schemaObject = SOFunction qf
          withRecordInconsistency metadataObject $
            modifyErr (\e -> "in function " <> qf <<> ": " <> e) do
              rawfi <- handleMultipleFunctions qf funcDefs
              (fi, deps) <- trackFunctionP2Setup tableNames qf systemDefined config rawfi
              recordDependencies metadataObject schemaObject [deps]
              pure fi

      -- allow list
      let allowList = allowlistDefs
            & concatMap _cdQueries
            & map (queryWithoutTypeNames . getGQLQuery . _lqQuery)
            & HS.fromList

      -- build GraphQL context with tables and functions
      baseGQLSchema <- GS.mkGCtxMap tableCache functionCache

      -- remote schemas
      let invalidatedRemoteSchemas = flip map remoteSchemas \remoteSchema ->
            (M.lookup (_arsqName remoteSchema) invalidationMap, remoteSchema)
      (remoteSchemaMap, gqlSchema, remoteGQLSchema) <-
        foldM addRemoteSchema (M.empty, baseGQLSchema, GC.emptyGCtx) invalidatedRemoteSchemas

      pure BuildOutputs
        { _boTables = tableCache
        , _boFunctions = functionCache
        , _boRemoteSchemas = remoteSchemaMap
        , _boAllowlist = allowList
        , _boGCtxMap = gqlSchema
        , _boDefaultRemoteGCtx = remoteGQLSchema
        }

    mkRelationshipMetadataObject (CatalogRelation qt rn rt rDef cmnt) =
      let objectId = MOTableObj qt $ MTORel rn rt
          definition = toJSON $ WithTable qt $ RelDef rn rDef cmnt
      in MetadataObject objectId definition

    mkComputedFieldMetadataObject (CatalogComputedField column _) =
      let AddComputedField qt name _ _ = column
          objectId = MOTableObj qt $ MTOComputedField name
      in MetadataObject objectId (toJSON column)

    mkPermissionMetadataObject (CatalogPermission qt rn pt pDef cmnt) =
      let objectId = MOTableObj qt $ MTOPerm rn pt
          definition = toJSON $ WithTable qt $ PermDef rn pDef cmnt
      in MetadataObject objectId definition

    mkEventTriggerMetadataObject (CatalogEventTrigger qt trn configuration) =
      let objectId = MOTableObj qt $ MTOTrigger trn
          definition = object ["table" .= qt, "configuration" .= configuration]
      in MetadataObject objectId definition

    withRecordDependencies
      :: (MonadWriter (Seq CollectedInfo) m)
      => MetadataObject -> SchemaObjId -> WriterT (Seq SchemaDependency) m a -> m a
    withRecordDependencies metadataObject schemaObjectId m = do
      (result, dependencies) <- runWriterT m
      recordDependencies metadataObject schemaObjectId (toList dependencies)
      pure result

    withTableContext :: (QErrM m) => QualifiedTable -> m a -> m a
    withTableContext tableName = modifyErr \e -> "in table " <> tableName <<> ": " <> e

    -- Given a map of table info, “folds in” another map of information, accumulating inconsistent
    -- metadata objects for any entries in the second map that don’t appear in the first map. This
    -- is used to “line up” the metadata for relationships, computed fields, permissions, etc. with
    -- the tracked table info.
    alignExtraTableInfo
      :: forall a b m
       . (MonadWriter (Seq CollectedInfo) m)
      => (b -> MetadataObject)
      -> M.HashMap QualifiedTable [b]
      -> M.HashMap QualifiedTable a
      -> m (M.HashMap QualifiedTable (a, [b]))
    alignExtraTableInfo mkMetadataObject extraInfo baseInfo =
      fmap M.catMaybes $ sequence $ alignWithKey combine baseInfo extraInfo
      where
        combine :: QualifiedTable -> These a [b] -> m (Maybe (a, [b]))
        combine tableName = \case
          This  base        -> pure $ Just (base, [])
          These base extras -> pure $ Just (base, extras)
          That       extras -> do
            let errorMessage = "table " <> tableName <<> " does not exist"
            Nothing <$ traverse_ (flip recordInconsistency errorMessage . mkMetadataObject) extras

    addNonColumnFields
      :: (QErrM m, MonadWriter (Seq CollectedInfo) m)
      => HashSet ForeignKey -- ^ all foreign keys
      -> HashSet QualifiedTable -- ^ the names of all tracked tables
      -> QualifiedTable
      -> FieldInfoMap PGColumnInfo
      -> [CatalogRelation]
      -> [CatalogComputedField]
      -> m (FieldInfoMap FieldInfo)
    addNonColumnFields foreignKeys trackedTableNames tableName columns relationships computedFields = do
      columnsAndRelationships <- foldM addRelationship (FIColumn <$> columns) relationships
      foldM addComputedField columnsAndRelationships computedFields
      where
        -- TODO: share code between addRelationship and addComputedField
        addRelationship fields relationship@(CatalogRelation _ rn rt rDef _) = do
          let metadataObject = mkRelationshipMetadataObject relationship
              schemaObject = SOTableObj tableName $ TORel rn
          fmap (fromMaybe fields) $
            withRecordInconsistency metadataObject $
            withTableContext tableName $
            modifyErr (\e -> "in relationship " <> rn <<> ": " <> e) do
              (field, dependencies) <- case rt of
                ObjRel -> do
                  using <- decodeValue rDef
                  let relDef = RelDef rn using Nothing
                  objRelP2Setup tableName foreignKeys relDef
                ArrRel -> do
                  using <- decodeValue rDef
                  let relDef = RelDef rn using Nothing
                  arrRelP2Setup tableName foreignKeys relDef
              recordDependencies metadataObject schemaObject dependencies
              insertField fields (FIRelationship field)

        addComputedField fields computedField@(CatalogComputedField column funcDefs) = do
          let AddComputedField qt name def comment = column
          fmap (fromMaybe fields) $
            withRecordInconsistency (mkComputedFieldMetadataObject computedField) $
            withTableContext tableName $
            modifyErr (\e -> "in computed field " <> name <<> ": " <> e) do
              rawfi <- handleMultipleFunctions (_cfdFunction def) funcDefs
              field <- addComputedFieldP2Setup trackedTableNames qt name def rawfi comment
              insertField fields (FIComputedField field)

        insertField
          :: (QErrM m)
          => FieldInfoMap FieldInfo
          -> FieldInfo
          -> m (FieldInfoMap FieldInfo)
        insertField fields fieldInfo = case M.lookup name fields of
          Just existingFieldInfo -> throw400 AlreadyExists $
            "conflicting definitions for field " <> name <<> ":\n"
            <> "  " <> showFieldType existingFieldInfo <> "\n  " <> showFieldType fieldInfo
          Nothing -> pure $ M.insert name fieldInfo fields
          where
            name = fieldInfoName fieldInfo
            showFieldType = \case
              FIColumn _ -> "postgres column"
              FIRelationship _ -> "relationship"
              FIComputedField _ -> "computed field"

    buildTablePermissions
      :: (MonadTx m, MonadReader BuildReason m, MonadWriter (Seq CollectedInfo) m)
      => TableCoreCache
      -> TableCoreInfo FieldInfo
      -> [CatalogPermission]
      -> m RolePermInfoMap
    buildTablePermissions tableCache tableInfo permissions = flip runTableCoreCacheRT tableCache $
      traverse (foldM buildAndInsertPermission emptyRolePermInfo) (M.groupOn _cpRole permissions)
      where
        tableName = _tciName tableInfo

        buildAndInsertPermission existingInfo permission@(CatalogPermission _ rn pt pDef _) = do
          let metadataObject = mkPermissionMetadataObject permission
              schemaObject = SOTableObj tableName $ TOPerm rn pt
          fmap (fromMaybe existingInfo) $
            withRecordInconsistency metadataObject $
            withRecordDependencies metadataObject schemaObject $
            withTableContext tableName $
            modifyErr (\e -> "in permission for role " <> rn <<> ": " <> e) $
              case pt of
                PTInsert -> insertPermission rn existingInfo PAInsert =<< buildPermission rn pDef
                PTSelect -> insertPermission rn existingInfo PASelect =<< buildPermission rn pDef
                PTUpdate -> insertPermission rn existingInfo PAUpdate =<< buildPermission rn pDef
                PTDelete -> insertPermission rn existingInfo PADelete =<< buildPermission rn pDef

        buildPermission
          :: ( MonadTx m
             , MonadReader BuildReason m
             , MonadWriter (Seq SchemaDependency) m
             , TableCoreInfoRM m
             , IsPerm a
             , FromJSON a
             )
          => RoleName -> Value -> m (PermInfo a)
        buildPermission rn pDef = do
          perm <- decodeValue pDef
          let permDef = PermDef rn perm Nothing
          (info, dependencies) <- buildPermInfo tableInfo permDef
          buildReason <- ask
          when (buildReason == CatalogUpdate) $
            addPermP2Setup tableName permDef info
          tell $ Seq.fromList dependencies
          pure info

        insertPermission
          :: (QErrM m) => RoleName -> RolePermInfo -> PermAccessor a -> a -> m RolePermInfo
        insertPermission roleName existingInfo accessor newInfo
          | roleName == adminRole || has (permAccToLens accessor . _Just) existingInfo =
              throw400 AlreadyExists $ "duplicate definition for " <> permissionType <> " permission"
          | otherwise = pure (existingInfo & permAccToLens accessor ?~ newInfo)
          where
            permissionType = case accessor of
              PAInsert -> "insert"
              PASelect -> "select"
              PAUpdate -> "update"
              PADelete -> "delete"

    buildTableEventTriggers
      :: ( MonadIO m
         , MonadTx m
         , MonadReader BuildReason m
         , MonadWriter (Seq CollectedInfo) m
         , HasSQLGenCtx m
         )
      => TableCoreCache
      -> [CatalogEventTrigger]
      -> m EventTriggerInfoMap
    buildTableEventTriggers tableCache = flip runTableCoreCacheRT tableCache .
      flip foldM M.empty \existingTriggers eventTrigger@(CatalogEventTrigger qt trn configuration) -> do
        buildReason <- ask
        let metadataObject = mkEventTriggerMetadataObject eventTrigger
            schemaObjectId = SOTableObj qt $ TOTrigger trn
        fmap (fromMaybe existingTriggers) $
          withRecordInconsistency metadataObject $
          withTableContext qt $
          modifyErr (\e -> "in event trigger " <> trn <<> ": " <> e) do
            case M.lookup trn existingTriggers of
              Just _ -> throw400 AlreadyExists "duplicate definition for event trigger"
              Nothing -> do
                etc <- decodeValue configuration
                (info, dependencies) <- subTableP2Setup qt etc
                when (buildReason == CatalogUpdate) $
                  mkAllTriggersQ trn qt (etcDefinition etc)
                recordDependencies metadataObject schemaObjectId dependencies
                pure $ M.insert trn info existingTriggers

    addRemoteSchema
      :: (MonadIO m, QErrM m, MonadWriter (Seq CollectedInfo) m, HasHttpManager m)
      => (RemoteSchemaMap, GS.GCtxMap, GS.GCtx)
      -> (Maybe InvalidationKey, AddRemoteSchemaQuery)
      -> m (RemoteSchemaMap, GS.GCtxMap, GS.GCtx)
    addRemoteSchema (remoteSchemas, gCtxMap, defGCtx) (_, rs@(AddRemoteSchemaQuery name _ _)) = do
      fmap (fromMaybe (remoteSchemas, gCtxMap, defGCtx)) $
        withRecordInconsistency (MetadataObject (MORemoteSchema name) (toJSON rs))
          case M.lookup name remoteSchemas of
            Just _ -> throw400 AlreadyExists "duplicate definition for remote schema"
            Nothing -> do
              rsCtx <- addRemoteSchemaP2Setup rs
              let rGCtx = convRemoteGCtx $ rscGCtx rsCtx
              mergedGCtxMap <- mergeRemoteSchema gCtxMap rGCtx
              mergedDefGCtx <- mergeGCtx defGCtx rGCtx
              pure (M.insert name rsCtx remoteSchemas, mergedGCtxMap, mergedDefGCtx)

-- | @'withMetadataCheck' cascade action@ runs @action@ and checks if the schema changed as a
-- result. If it did, it checks to ensure the changes do not violate any integrity constraints, and
-- if not, incorporates them into the schema cache.
withMetadataCheck :: (MonadTx m, CacheRWM m) => Bool -> m a -> m a
withMetadataCheck cascade action = do
  -- Drop hdb_views so no interference is caused to the sql query
  liftTx $ Q.catchE defaultTxErrorHandler clearHdbViews

  -- Get the metadata before the sql query, everything, need to filter this
  oldMetaU <- liftTx $ Q.catchE defaultTxErrorHandler fetchTableMeta
  oldFuncMetaU <- liftTx $ Q.catchE defaultTxErrorHandler fetchFunctionMeta

  -- Run the action
  res <- action

  -- Get the metadata after the sql query
  newMeta <- liftTx $ Q.catchE defaultTxErrorHandler fetchTableMeta
  newFuncMeta <- liftTx $ Q.catchE defaultTxErrorHandler fetchFunctionMeta
  sc <- askSchemaCache
  let existingInconsistentObjs = scInconsistentObjs sc
      existingTables = M.keys $ scTables sc
      oldMeta = flip filter oldMetaU $ \tm -> tmTable tm `elem` existingTables
      schemaDiff = getSchemaDiff oldMeta newMeta
      existingFuncs = M.keys $ scFunctions sc
      oldFuncMeta = flip filter oldFuncMetaU $ \fm -> fmFunction fm `elem` existingFuncs
      FunctionDiff droppedFuncs alteredFuncs = getFuncDiff oldFuncMeta newFuncMeta
      overloadedFuncs = getOverloadedFuncs existingFuncs newFuncMeta

  -- Do not allow overloading functions
  unless (null overloadedFuncs) $
    throw400 NotSupported $ "the following tracked function(s) cannot be overloaded: "
    <> reportFuncs overloadedFuncs

  indirectDeps <- getSchemaChangeDeps schemaDiff

  -- Report back with an error if cascade is not set
  when (indirectDeps /= [] && not cascade) $ reportDepsExt indirectDeps []

  -- Purge all the indirect dependents from state
  mapM_ purgeDependentObject indirectDeps

  -- Purge all dropped functions
  let purgedFuncs = flip mapMaybe indirectDeps $ \dep ->
        case dep of
          SOFunction qf -> Just qf
          _             -> Nothing

  forM_ (droppedFuncs \\ purgedFuncs) $ \qf -> do
    liftTx $ delFunctionFromCatalog qf

  -- Process altered functions
  forM_ alteredFuncs $ \(qf, newTy) -> do
    when (newTy == FTVOLATILE) $
      throw400 NotSupported $
      "type of function " <> qf <<> " is altered to \"VOLATILE\" which is not supported now"

  -- update the schema cache and hdb_catalog with the changes
  processSchemaChanges schemaDiff

  buildSchemaCache
  currentInconsistentObjs <- scInconsistentObjs <$> askSchemaCache
  checkNewInconsistentMeta existingInconsistentObjs currentInconsistentObjs

  return res
  where
    reportFuncs = T.intercalate ", " . map dquoteTxt

    processSchemaChanges :: (MonadTx m, CacheRM m) => SchemaDiff -> m ()
    processSchemaChanges schemaDiff = do
      -- Purge the dropped tables
      mapM_ delTableAndDirectDeps droppedTables

      sc <- askSchemaCache
      for_ alteredTables $ \(oldQtn, tableDiff) -> do
        ti <- case M.lookup oldQtn $ scTables sc of
          Just ti -> return ti
          Nothing -> throw500 $ "old table metadata not found in cache : " <>> oldQtn
        processTableChanges (_tiCoreInfo ti) tableDiff
      where
        SchemaDiff droppedTables alteredTables = schemaDiff

checkNewInconsistentMeta
  :: (QErrM m)
  => [InconsistentMetadataObj] -> [InconsistentMetadataObj] -> m ()
checkNewInconsistentMeta originalInconsMeta currentInconsMeta =
  unless (null newInconsMetaObjects) $
    throwError (err500 Unexpected "cannot continue due to newly found inconsistent metadata")
      { qeInternal = Just $ toJSON newInconsMetaObjects }
  where
    newInconsMetaObjects =
      M.elems $ M.differenceOn (_moId . _imoObject) currentInconsMeta originalInconsMeta

purgeDependentObject :: (MonadTx m) => SchemaObjId -> m ()
purgeDependentObject = \case
  SOTableObj tn (TOPerm rn pt) -> liftTx $ dropPermFromCatalog tn rn pt
  SOTableObj qt (TORel rn) -> liftTx $ delRelFromCatalog qt rn
  SOFunction qf -> liftTx $ delFunctionFromCatalog qf
  SOTableObj _ (TOTrigger trn) -> liftTx $ delEventTriggerFromCatalog trn
  SOTableObj qt (TOComputedField ccn) -> dropComputedFieldFromCatalog qt ccn
  schemaObjId -> throw500 $ "unexpected dependent object: " <> reportSchemaObj schemaObjId

-- | Processes collected 'CIDependency' values into a 'DepMap', performing integrity checking to
-- ensure the dependencies actually exist. If a dependency is missing, its transitive dependents are
-- removed from the cache, and 'InconsistentMetadataObj's are returned.
resolveDependencies
  :: forall m. (QErrM m)
  => Inc.Rule m
   ( BuildOutputs
   , [(MetadataObject, SchemaObjId, SchemaDependency)]
   ) (BuildOutputs, [InconsistentMetadataObj], DepMap)
resolveDependencies = Inc.rule \(cache, dependencies) -> do
  let dependencyMap = dependencies
        & M.groupOn (view _2)
        & fmap (map \(metadataObject, _, schemaDependency) -> (metadataObject, schemaDependency))
  performIteration 0 cache [] dependencyMap
  where
    -- Processes dependencies using an iterative process that alternates between two steps:
    --
    --   1. First, pruneDanglingDependents searches for any dependencies that do not exist in the
    --      current cache and removes their dependents from the dependency map, returning an
    --      InconsistentMetadataObj for each dependent that was removed. This step does not change
    --      the schema cache in any way.
    --
    --   2. Second, deleteMetadataObject drops the pruned dependent objects from the cache. It does
    --      not alter (or consult) the dependency map, so transitive dependents are /not/ removed.
    --
    -- By iterating the above process until pruneDanglingDependents does not discover any new
    -- inconsistencies, all missing dependencies will eventually be removed, and since dependency
    -- graphs between schema objects are unlikely to be very deep, it will usually terminate in just
    -- a few iterations.
    performIteration
      :: Int
      -> BuildOutputs
      -> [InconsistentMetadataObj]
      -> HashMap SchemaObjId [(MetadataObject, SchemaDependency)]
      -> m (BuildOutputs, [InconsistentMetadataObj], DepMap)
    performIteration iterationNumber cache inconsistencies dependencies = do
      let (newInconsistencies, prunedDependencies) = pruneDanglingDependents cache dependencies
      case newInconsistencies of
        [] -> pure (cache, inconsistencies, HS.fromList . map snd <$> prunedDependencies)
        _ | iterationNumber < 100 -> do
              let inconsistentIds = _moId . _imoObject <$> newInconsistencies
                  prunedCache = foldl' (flip deleteMetadataObject) cache inconsistentIds
                  allInconsistencies = inconsistencies <> newInconsistencies
              performIteration (iterationNumber + 1) prunedCache allInconsistencies prunedDependencies
          | otherwise ->
              -- Running for 100 iterations without terminating is (hopefully) enormously unlikely
              -- unless we did something very wrong, so halt the process and abort with some
              -- debugging information.
              throwError (err500 Unexpected "schema dependency resolution failed to terminate")
                { qeInternal = Just $ object
                    [ "inconsistent_objects" .= object
                      [ "old" .= inconsistencies
                      , "new" .= newInconsistencies ]
                    , "pruned_dependencies" .= (map snd <$> prunedDependencies) ] }

    pruneDanglingDependents
      :: BuildOutputs
      -> HashMap SchemaObjId [(MetadataObject, SchemaDependency)]
      -> ([InconsistentMetadataObj], HashMap SchemaObjId [(MetadataObject, SchemaDependency)])
    pruneDanglingDependents cache = fmap (M.filter (not . null)) . traverse do
      partitionEithers . map \(metadataObject, dependency) -> case resolveDependency dependency of
        Right ()          -> Right (metadataObject, dependency)
        Left errorMessage -> Left (InconsistentMetadataObj metadataObject errorMessage)
      where
        resolveDependency :: SchemaDependency -> Either Text ()
        resolveDependency (SchemaDependency objectId _) = case objectId of
          SOTable tableName -> void $ resolveTable tableName
          SOFunction functionName -> unless (functionName `M.member` _boFunctions cache) $
            Left $ "function " <> functionName <<> " is not tracked"
          SOTableObj tableName tableObjectId -> do
            tableInfo <- resolveTable tableName
            -- let coreInfo = _tiCoreInfo tableInfo
            case tableObjectId of
              TOCol columnName ->
                void $ resolveField tableInfo (fromPGCol columnName) _FIColumn "column"
              TORel relName ->
                void $ resolveField tableInfo (fromRel relName) _FIRelationship "relationship"
              TOComputedField fieldName ->
                void $ resolveField tableInfo (fromComputedField fieldName) _FIComputedField "computed field"
              TOCons _constraintName ->
                -- FIXME: foreign key constraints
                pure ()
                -- unless (constraintName `elem` _tciUniqueOrPrimaryKeyConstraints coreInfo) $ do
                --   Left $ "no unique or primary key constraint named " <> constraintName <<> " is "
                --     <> "defined for table " <>> tableName
              TOPerm roleName permType -> withPermType permType \accessor -> do
                let permLens = permAccToLens accessor
                unless (has (tiRolePermInfoMap.ix roleName.permLens._Just) tableInfo) $
                  Left $ "no " <> permTypeToCode permType <> " permission defined on table "
                    <> tableName <<> " for role " <>> roleName
              TOTrigger triggerName ->
                unless (M.member triggerName (_tiEventTriggerInfoMap tableInfo)) $ Left $
                  "no event trigger named " <> triggerName <<> " is defined for table " <>> tableName

        resolveTable tableName = M.lookup tableName (_boTables cache) `onNothing`
          Left ("table " <> tableName <<> " is not tracked")

        resolveField :: TableInfo -> FieldName -> Getting (First a) FieldInfo a -> Text -> Either Text a
        resolveField tableInfo fieldName fieldType fieldTypeName = do
          let coreInfo = _tiCoreInfo tableInfo
              tableName = _tciName coreInfo
          fieldInfo <- M.lookup fieldName (_tciFieldInfoMap coreInfo) `onNothing` Left
            ("table " <> tableName <<> " has no field named " <>> fieldName)
          (fieldInfo ^? fieldType) `onNothing` Left
            ("field " <> fieldName <<> "of table " <> tableName <<> " is not a " <> fieldTypeName)

    deleteMetadataObject :: MetadataObjId -> BuildOutputs -> BuildOutputs
    deleteMetadataObject objectId = case objectId of
      MOTable        name -> boTables        %~ M.delete name
      MOFunction     name -> boFunctions     %~ M.delete name
      MORemoteSchema name -> boRemoteSchemas %~ M.delete name
      MOTableObj tableName tableObjectId -> boTables.ix tableName %~ case tableObjectId of
        MTORel           name _ -> tiCoreInfo.tciFieldInfoMap %~ M.delete (fromRel name)
        MTOComputedField name   -> tiCoreInfo.tciFieldInfoMap %~ M.delete (fromComputedField name)
        MTOPerm roleName permType -> withPermType permType \accessor ->
          tiRolePermInfoMap.ix roleName.permAccToLens accessor .~ Nothing
        MTOTrigger name -> tiEventTriggerInfoMap %~ M.delete name
