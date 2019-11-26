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
import qualified Data.Sequence                      as Seq
import qualified Data.Text                          as T
import qualified Database.PG.Query                  as Q

import           Control.Arrow.Extended
import           Control.Lens                       hiding ((.=))
import           Control.Monad.Unique
import           Data.Aeson
-- import           Data.IORef
import           Data.List                          (nub)
-- import           Data.Time.Clock

import qualified Hasura.GraphQL.Context             as GC
import qualified Hasura.GraphQL.Schema              as GS
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
  { _biReason          :: !BuildReason
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

{-# INLINABLE buildRebuildableSchemaCache #-} -- see Note [Specialization of buildRebuildableSchemaCache]
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
    -- startTime <- liftIO getCurrentTime
    RebuildableSchemaCache _ invalidationMap rule <- get
    catalogMetadata <- liftTx fetchCatalogData
    -- afterFetchTime <- liftIO getCurrentTime
    -- liftIO $ putStrLn $ "--> [fetch] " <> show (afterFetchTime `diffUTCTime` startTime)
    result <- lift $ flip runReaderT buildReason $ Inc.build rule (catalogMetadata, invalidationMap)
    let !schemaCache = Inc.result result
    -- afterBuildTime <- liftIO getCurrentTime
    -- liftIO $ putStrLn $ "--> [build] " <> show (afterBuildTime `diffUTCTime` afterFetchTime)
    let !prunedInvalidationMap = pruneInvalidationMap schemaCache invalidationMap
    -- afterPruneTime <- liftIO getCurrentTime
    -- liftIO $ putStrLn $ "--> [prune] " <> show (afterPruneTime `diffUTCTime` afterBuildTime)
    -- liftIO $ putStrLn $ "[TOTAL] " <> show (afterPruneTime `diffUTCTime` startTime)
    put $ RebuildableSchemaCache schemaCache prunedInvalidationMap (Inc.rebuildRule result)
    where
      pruneInvalidationMap schemaCache = M.filterWithKey \name _ ->
        M.member name (scRemoteSchemas schemaCache)

  invalidateCachedRemoteSchema name = CacheRWT do
    unique <- newUnique
    assign (rscInvalidationMap . at name) (Just unique)

{-# INLINABLE buildSchemaCacheRule #-} -- see Note [Specialization of buildRebuildableSchemaCache]
buildSchemaCacheRule
  -- Note: by supplying BuildReason via MonadReader, it does not participate in caching, which is
  -- what we want!
  :: ( Inc.ArrowCache arr, Inc.ArrowDistribute arr, ArrowKleisli m arr
     , MonadIO m, MonadTx m, MonadReader BuildReason m, HasHttpManager m, HasSQLGenCtx m )
  => (CatalogMetadata, InvalidationMap) `arr` SchemaCache
buildSchemaCacheRule = proc inputs -> do
  (outputs, collectedInfo) <- runWriterA buildAndCollectInfo -< inputs
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
      :: ( Inc.ArrowCache arr, Inc.ArrowDistribute arr, ArrowKleisli m arr
         , ArrowWriter (Seq CollectedInfo) arr, MonadIO m, MonadTx m, MonadReader BuildReason m
         , HasHttpManager m, HasSQLGenCtx m )
      => (CatalogMetadata, InvalidationMap) `arr` BuildOutputs
    buildAndCollectInfo = proc (catalogMetadata, invalidationMap) -> do
      let CatalogMetadata tables relationships permissions
            eventTriggers remoteSchemas functions fkeys' allowlistDefs
            computedFields = catalogMetadata

      -- tables
      tableRawInfos <- buildTableCache -< tables
      let tableNames = HS.fromList $ M.keys tableRawInfos

      -- relationships and computed fields
      let relationshipsByTable = M.groupOn _crTable relationships
          computedFieldsByTable = M.groupOn (_afcTable . _cccComputedField) computedFields
          fkeys = HS.fromList fkeys'
      tableCoreInfos <- (tableRawInfos >- returnA)
        >-> (\info -> (info, relationshipsByTable) >- alignExtraTableInfo mkRelationshipMetadataObject)
        >-> (\info -> (info, computedFieldsByTable) >- alignExtraTableInfo mkComputedFieldMetadataObject)
        >-> (| Inc.keyed (\_ ((tableRawInfo, tableRelationships), tableComputedFields) -> do
                 let columns = _tciFieldInfoMap tableRawInfo
                 allFields <- addNonColumnFields -<
                   (fkeys, tableNames, columns, tableRelationships, tableComputedFields)
                 returnA -< tableRawInfo { _tciFieldInfoMap = allFields }) |)

      -- permissions and event triggers
      -- permTimeRef <- bindA -< liftIO $ newIORef 0
      -- eventTimeRef <- bindA -< liftIO $ newIORef 0
      tableCache <- (tableCoreInfos >- returnA)
        >-> (\info -> (info, M.groupOn _cpTable permissions) >- alignExtraTableInfo mkPermissionMetadataObject)
        >-> (\info -> (info, M.groupOn _cetTable eventTriggers) >- alignExtraTableInfo mkEventTriggerMetadataObject)
        >-> (| Inc.keyed (\_ ((tableCoreInfo, tablePermissions), tableEventTriggers) -> do
                 -- startTime <- bindA -< liftIO getCurrentTime
                 permissionInfos <- buildTablePermissions -< (tableCoreInfos, tableCoreInfo, tablePermissions)
                 -- afterPermTime <- bindA -< liftIO getCurrentTime
                 eventTriggerInfos <- buildTableEventTriggers -< (tableCoreInfos, tableEventTriggers)
                 -- afterEventsTime <- bindA -< liftIO getCurrentTime
                 -- bindA -< liftIO $ modifyIORef' permTimeRef (+ (afterPermTime `diffUTCTime` startTime))
                 -- bindA -< liftIO $ modifyIORef' eventTimeRef (+ (afterEventsTime `diffUTCTime` afterPermTime))
                 returnA -< TableInfo
                   { _tiCoreInfo = tableCoreInfo
                   , _tiRolePermInfoMap = permissionInfos
                   , _tiEventTriggerInfoMap = eventTriggerInfos
                   }) |)
      -- permTime <- bindA -< liftIO $ readIORef permTimeRef
      -- eventTime <- bindA -< liftIO $ readIORef eventTimeRef
      -- bindA -< liftIO $ putStrLn $ "----> [build/perms] " <> show permTime
      -- bindA -< liftIO $ putStrLn $ "----> [build/events] " <> show eventTime

      -- sql functions
      functionCache <- (mapFromL _cfFunction functions >- returnA)
        >-> (| Inc.keyed (\_ (CatalogFunction qf systemDefined config funcDefs) -> do
                 let definition = toJSON $ TrackFunction qf
                     metadataObject = MetadataObject (MOFunction qf) definition
                     schemaObject = SOFunction qf
                     addFunctionContext e = "in function " <> qf <<> ": " <> e
                 (| withRecordInconsistency (
                    (| modifyErrA (do
                         rawfi <- bindErrorA -< handleMultipleFunctions qf funcDefs
                         (fi, dep) <- bindErrorA -<
                           trackFunctionP2Setup tableNames qf systemDefined config rawfi
                         recordDependencies -< (metadataObject, schemaObject, [dep])
                         returnA -< fi)
                    |) addFunctionContext)
                  |) metadataObject) |)
        >-> (\infos -> M.catMaybes infos >- returnA)

      -- allow list
      let allowList = allowlistDefs
            & concatMap _cdQueries
            & map (queryWithoutTypeNames . getGQLQuery . _lqQuery)
            & HS.fromList

      -- build GraphQL context with tables and functions
      baseGQLSchema <- bindA -< GS.mkGCtxMap tableCache functionCache

      -- remote schemas
      let invalidatedRemoteSchemas = flip map remoteSchemas \remoteSchema ->
            (M.lookup (_arsqName remoteSchema) invalidationMap, remoteSchema)
      (remoteSchemaMap, gqlSchema, remoteGQLSchema) <-
        (| foldlA' (\schemas schema -> (schemas, schema) >- addRemoteSchema)
        |) (M.empty, baseGQLSchema, GC.emptyGCtx) invalidatedRemoteSchemas

      returnA -< BuildOutputs
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

    bindErrorA
      :: (ArrowChoice arr, ArrowKleisli m arr, ArrowError e arr, MonadError e m)
      => arr (m a) a
    bindErrorA = (throwA ||| returnA) <<< arrM \m -> (Right <$> m) `catchError` (pure . Left)

    withRecordDependencies
      :: (ArrowWriter (Seq CollectedInfo) arr)
      => WriterA (Seq SchemaDependency) arr (e, s) a
      -> arr (e, (MetadataObject, (SchemaObjId, s))) a
    withRecordDependencies f = proc (e, (metadataObject, (schemaObjectId, s))) -> do
      (result, dependencies) <- runWriterA f -< (e, s)
      recordDependencies -< (metadataObject, schemaObjectId, toList dependencies)
      returnA -< result

    noDuplicates
      :: (ArrowChoice arr, ArrowWriter (Seq CollectedInfo) arr)
      => (a -> MetadataObject)
      -> [a] `arr` Maybe a
    noDuplicates mkMetadataObject = proc values -> case values of
      []      -> returnA -< Nothing
      [value] -> returnA -< Just value
      value:_ -> do
        let objectId = _moId $ mkMetadataObject value
            definitions = map (_moDefinition . mkMetadataObject) values
        tellA -< Seq.singleton $ CIInconsistency (DuplicateObjects objectId definitions)
        returnA -< Nothing

    addTableContext tableName e = "in table " <> tableName <<> ": " <> e

    -- Given a map of table info, “folds in” another map of information, accumulating inconsistent
    -- metadata objects for any entries in the second map that don’t appear in the first map. This
    -- is used to “line up” the metadata for relationships, computed fields, permissions, etc. with
    -- the tracked table info.
    alignExtraTableInfo
      :: forall a b arr
       . (Inc.ArrowDistribute arr, ArrowWriter (Seq CollectedInfo) arr)
      => (b -> MetadataObject)
      -> ( M.HashMap QualifiedTable a
         , M.HashMap QualifiedTable [b]
         ) `arr` M.HashMap QualifiedTable (a, [b])
    alignExtraTableInfo mkMetadataObject = proc (baseInfo, extraInfo) -> do
      combinedInfo <-
        (| Inc.keyed (\tableName infos -> combine -< (tableName, infos))
        |) (align baseInfo extraInfo)
      returnA -< M.catMaybes combinedInfo
      where
        combine :: (QualifiedTable, These a [b]) `arr` Maybe (a, [b])
        combine = proc (tableName, infos) -> case infos of
          This  base        -> returnA -< Just (base, [])
          These base extras -> returnA -< Just (base, extras)
          That       extras -> do
            let errorMessage = "table " <> tableName <<> " does not exist"
            recordInconsistencies -< (map mkMetadataObject extras, errorMessage)
            returnA -< Nothing

    addNonColumnFields
      :: ( Inc.ArrowDistribute arr, ArrowWriter (Seq CollectedInfo) arr
         , ArrowKleisli m arr, MonadError QErr m )
      => ( HashSet ForeignKey -- ^ all foreign keys
         , HashSet QualifiedTable -- ^ the names of all tracked tables
         , FieldInfoMap PGColumnInfo
         , [CatalogRelation]
         , [CatalogComputedField]
         ) `arr` FieldInfoMap FieldInfo
    addNonColumnFields =
      proc (foreignKeys, trackedTableNames, columns, relationships, computedFields) -> do
        relationshipInfos <-
          (| Inc.keyed (\_ relationshipsByName -> do
               maybeRelationship <- noDuplicates mkRelationshipMetadataObject -< relationshipsByName
               (\info -> join info >- returnA) <-<
                 (| traverseA (\relationship -> do
                      info <- buildRelationship -< (foreignKeys, relationship)
                      returnA -< info <&> (, mkRelationshipMetadataObject relationship))
                 |) maybeRelationship)
          |) (M.groupOn _crRelName relationships)

        computedFieldInfos <-
          (| Inc.keyed (\_ computedFieldsByName -> do
               maybeComputedField <- noDuplicates mkComputedFieldMetadataObject -< computedFieldsByName
               (\info -> join info >- returnA) <-<
                 (| traverseA (\computedField -> do
                      info <- buildComputedField -< (trackedTableNames, computedField)
                      returnA -< info <&> (, mkComputedFieldMetadataObject computedField))
                 |) maybeComputedField)
          |) (M.groupOn (_afcName . _cccComputedField) computedFields)

        let mapKey f = M.fromList . map (first f) . M.toList
            relationshipFields = mapKey fromRel $ M.catMaybes relationshipInfos
            computedFieldFields = mapKey fromComputedField $ M.catMaybes computedFieldInfos
        nonColumnFields <-
          (| Inc.keyed (\fieldName fields -> noFieldConflicts -< (fieldName, fields))
          |) (align relationshipFields computedFieldFields)

        (| Inc.keyed (\_ fields -> noColumnConflicts -< fields)
         |) (align columns (M.catMaybes nonColumnFields))
      where
        buildRelationship = proc (foreignKeys, relationship) -> do
          let CatalogRelation tableName rn rt rDef _ = relationship
              metadataObject = mkRelationshipMetadataObject relationship
              schemaObject = SOTableObj tableName $ TORel rn
              addRelationshipContext e = "in relationship " <> rn <<> ": " <> e
          (| withRecordInconsistency (
             (| modifyErrA (do
                  (info, dependencies) <- bindErrorA -< case rt of
                    ObjRel -> do
                      using <- decodeValue rDef
                      objRelP2Setup tableName foreignKeys (RelDef rn using Nothing)
                    ArrRel -> do
                      using <- decodeValue rDef
                      arrRelP2Setup tableName foreignKeys (RelDef rn using Nothing)
                  recordDependencies -< (metadataObject, schemaObject, dependencies)
                  returnA -< info)
             |) (addTableContext tableName . addRelationshipContext))
           |) metadataObject

        buildComputedField = proc (trackedTableNames, computedField) -> do
          let CatalogComputedField column funcDefs = computedField
              AddComputedField qt name def comment = column
              addComputedFieldContext e = "in computed field " <> name <<> ": " <> e
          (| withRecordInconsistency (
             (| modifyErrA (do
                  rawfi <- bindErrorA -< handleMultipleFunctions (_cfdFunction def) funcDefs
                  bindErrorA -< addComputedFieldP2Setup trackedTableNames qt name def rawfi comment)
             |) (addTableContext qt . addComputedFieldContext))
           |) (mkComputedFieldMetadataObject computedField)

        noFieldConflicts = proc (fieldName, fields) -> case fields of
          This (relationship, metadata) -> returnA -< Just (FIRelationship relationship, metadata)
          That (computedField, metadata) -> returnA -< Just (FIComputedField computedField, metadata)
          These (_, relationshipMetadata) (_, computedFieldMetadata) -> do
            tellA -< Seq.singleton $ CIInconsistency $ ConflictingObjects
              ("conflicting definitions for field " <>> fieldName)
              [relationshipMetadata, computedFieldMetadata]
            returnA -< Nothing

        noColumnConflicts = proc fields -> case fields of
          This columnInfo -> returnA -< FIColumn columnInfo
          That (fieldInfo, _) -> returnA -< fieldInfo
          These columnInfo (_, fieldMetadata) -> do
            recordInconsistency -< (fieldMetadata, "field definition conflicts with postgres column")
            returnA -< FIColumn columnInfo

    buildTablePermissions
      :: ( Inc.ArrowCache arr, Inc.ArrowDistribute arr, ArrowKleisli m arr
         , ArrowWriter (Seq CollectedInfo) arr, MonadTx m, MonadReader BuildReason m )
      => ( TableCoreCache
         , TableCoreInfo FieldInfo
         , [CatalogPermission]
         ) `arr` RolePermInfoMap
    buildTablePermissions = proc (tableCache, tableInfo, tablePermissions) ->
      (| Inc.keyed (\_ rolePermissions -> do
           let (insertPerms, selectPerms, updatePerms, deletePerms) =
                 partitionPermissions rolePermissions

           insertPermInfo <- buildPermission -< (tableCache, tableInfo, insertPerms)
           selectPermInfo <- buildPermission -< (tableCache, tableInfo, selectPerms)
           updatePermInfo <- buildPermission -< (tableCache, tableInfo, updatePerms)
           deletePermInfo <- buildPermission -< (tableCache, tableInfo, deletePerms)

           returnA -< RolePermInfo
             { _permIns = insertPermInfo
             , _permSel = selectPermInfo
             , _permUpd = updatePermInfo
             , _permDel = deletePermInfo
             })
      |) (M.groupOn _cpRole tablePermissions)
      where
        partitionPermissions = flip foldr ([], [], [], []) $
          \perm (insertPerms, selectPerms, updatePerms, deletePerms) -> case _cpPermType perm of
            PTInsert -> (perm:insertPerms, selectPerms, updatePerms, deletePerms)
            PTSelect -> (insertPerms, perm:selectPerms, updatePerms, deletePerms)
            PTUpdate -> (insertPerms, selectPerms, perm:updatePerms, deletePerms)
            PTDelete -> (insertPerms, selectPerms, updatePerms, perm:deletePerms)

        withPermission
          :: (ArrowChoice arr, ArrowWriter (Seq CollectedInfo) arr)
          => WriterA (Seq SchemaDependency) (ErrorA QErr arr) (a, s) b
          -> arr (a, (CatalogPermission, s)) (Maybe b)
        withPermission f = proc (e, (permission, s)) -> do
          let CatalogPermission tableName roleName permType _ _ = permission
              metadataObject = mkPermissionMetadataObject permission
              schemaObject = SOTableObj tableName $ TOPerm roleName permType
              addPermContext err = "in permission for role " <> roleName <<> ": " <> err
          (| withRecordInconsistency (
             (| withRecordDependencies (
                (| modifyErrA (f -< (e, s))
                |) (addTableContext tableName . addPermContext))
             |) metadataObject schemaObject)
           |) metadataObject

        buildPermission
          :: ( Inc.ArrowCache arr, Inc.ArrowDistribute arr, ArrowKleisli m arr
             , ArrowWriter (Seq CollectedInfo) arr, MonadTx m, MonadReader BuildReason m
             , Eq a, IsPerm a, FromJSON a, Eq (PermInfo a) )
          => ( TableCoreCache
             , TableCoreInfo FieldInfo
             , [CatalogPermission]
             ) `arr` Maybe (PermInfo a)
        buildPermission = proc (tableCache, tableInfo, permissions) ->
              (permissions >- noDuplicates mkPermissionMetadataObject)
          >-> (| traverseA (\permission@(CatalogPermission _ roleName _ pDef _) ->
                 (| withPermission (do
                      bindErrorA -< when (roleName == adminRole) $
                        throw400 ConstraintViolation "cannot define permission for admin role"
                      perm <- bindErrorA -< decodeValue pDef
                      let permDef = PermDef roleName perm Nothing
                      (info, dependencies) <- bindErrorA -<
                        runTableCoreCacheRT (buildPermInfo tableInfo permDef) tableCache
                      tellA -< Seq.fromList dependencies
                      rebuildViewsIfNeeded -< (_tciName tableInfo, permDef, info)
                      returnA -< info)
                 |) permission) |)
          >-> (\info -> join info >- returnA)

        rebuildViewsIfNeeded
          :: ( Inc.ArrowCache arr, ArrowKleisli m arr, MonadTx m, MonadReader BuildReason m
             , Eq a, IsPerm a, Eq (PermInfo a) )
          => (QualifiedTable, PermDef a, PermInfo a) `arr` ()
        rebuildViewsIfNeeded = Inc.cache $ arrM \(tableName, permDef, info) -> do
          buildReason <- ask
          when (buildReason == CatalogUpdate) $
            addPermP2Setup tableName permDef info

    buildTableEventTriggers
      :: ( Inc.ArrowDistribute arr, ArrowKleisli m arr, ArrowWriter (Seq CollectedInfo) arr
         , MonadIO m, MonadTx m, MonadReader BuildReason m
         , HasSQLGenCtx m )
      => (TableCoreCache, [CatalogEventTrigger]) `arr` EventTriggerInfoMap
    buildTableEventTriggers = proc (tableCache, eventTriggers) ->
      (\infos -> M.catMaybes infos >- returnA) <-<
        (| Inc.keyed (\_ duplicateEventTriggers -> do
             maybeEventTrigger <- noDuplicates mkEventTriggerMetadataObject -< duplicateEventTriggers
             (\info -> join info >- returnA) <-<
               (| traverseA (\eventTrigger -> buildEventTrigger -< (tableCache, eventTrigger))
               |) maybeEventTrigger)
        |) (M.groupOn _cetName eventTriggers)
      where
        buildEventTrigger = proc (tableCache, eventTrigger) -> do
          let CatalogEventTrigger qt trn configuration = eventTrigger
              metadataObject = mkEventTriggerMetadataObject eventTrigger
              schemaObjectId = SOTableObj qt $ TOTrigger trn
              addTriggerContext e = "in event trigger " <> trn <<> ": " <> e
          (| withRecordInconsistency (
             (| modifyErrA (do
                  etc <- bindErrorA -< decodeValue configuration
                  (info, dependencies) <- bindErrorA -< subTableP2Setup qt etc
                  bindErrorA -< flip runTableCoreCacheRT tableCache $ do
                    buildReason <- ask
                    when (buildReason == CatalogUpdate) $
                      mkAllTriggersQ trn qt (etcDefinition etc)
                  recordDependencies -< (metadataObject, schemaObjectId, dependencies)
                  returnA -< info)
             |) (addTableContext qt . addTriggerContext))
           |) metadataObject

    addRemoteSchema
      :: ( ArrowChoice arr, ArrowWriter (Seq CollectedInfo) arr, ArrowKleisli m arr
         , MonadIO m, HasHttpManager m )
      => ( (RemoteSchemaMap, GS.GCtxMap, GS.GCtx)
         , (Maybe InvalidationKey, AddRemoteSchemaQuery)
         ) `arr` (RemoteSchemaMap, GS.GCtxMap, GS.GCtx)
    addRemoteSchema = proc ((remoteSchemas, gCtxMap, defGCtx), (_, remoteSchema)) -> do
      let name = _arsqName remoteSchema
      (| onNothingA (returnA -< (remoteSchemas, gCtxMap, defGCtx)) |) <-<
         (| withRecordInconsistency (case M.lookup name remoteSchemas of
              Just _ -> throwA -< err400 AlreadyExists "duplicate definition for remote schema"
              Nothing -> (throwA ||| returnA) <<< bindA -< runExceptT do
                rsCtx <- addRemoteSchemaP2Setup remoteSchema
                let rGCtx = convRemoteGCtx $ rscGCtx rsCtx
                mergedGCtxMap <- mergeRemoteSchema gCtxMap rGCtx
                mergedDefGCtx <- mergeGCtx defGCtx rGCtx
                pure (M.insert name rsCtx remoteSchemas, mergedGCtxMap, mergedDefGCtx))
         |) (MetadataObject (MORemoteSchema name) (toJSON remoteSchema))

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
      => [InconsistentMetadata] -> [InconsistentMetadata] -> m ()
    checkNewInconsistentMeta originalInconsMeta currentInconsMeta =
      unless (null newInconsistentObjects) $
        throwError (err500 Unexpected "cannot continue due to newly found inconsistent metadata")
          { qeInternal = Just $ toJSON newInconsistentObjects }
      where
        diffInconsistentObjects = M.difference `on` groupInconsistentMetadataById
        newInconsistentObjects = nub $ concatMap toList $
          M.elems (currentInconsMeta `diffInconsistentObjects` originalInconsMeta)

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
-- removed from the cache, and 'InconsistentMetadata's are returned.
resolveDependencies
  :: forall arr m. (ArrowKleisli m arr, QErrM m)
  => ( BuildOutputs
     , [(MetadataObject, SchemaObjId, SchemaDependency)]
     ) `arr` (BuildOutputs, [InconsistentMetadata], DepMap)
resolveDependencies = arrM \(cache, dependencies) -> do
  let dependencyMap = dependencies
        & M.groupOn (view _2)
        & fmap (map \(metadataObject, _, schemaDependency) -> (metadataObject, schemaDependency))
  performIteration 0 cache [] dependencyMap
  where
    -- Processes dependencies using an iterative process that alternates between two steps:
    --
    --   1. First, pruneDanglingDependents searches for any dependencies that do not exist in the
    --      current cache and removes their dependents from the dependency map, returning an
    --      InconsistentMetadata for each dependent that was removed. This step does not change
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
      -> [InconsistentMetadata]
      -> HashMap SchemaObjId [(MetadataObject, SchemaDependency)]
      -> m (BuildOutputs, [InconsistentMetadata], DepMap)
    performIteration iterationNumber cache inconsistencies dependencies = do
      let (newInconsistencies, prunedDependencies) = pruneDanglingDependents cache dependencies
      case newInconsistencies of
        [] -> pure (cache, inconsistencies, HS.fromList . map snd <$> prunedDependencies)
        _ | iterationNumber < 100 -> do
              let inconsistentIds = nub $ concatMap imObjectIds newInconsistencies
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
      -> ([InconsistentMetadata], HashMap SchemaObjId [(MetadataObject, SchemaDependency)])
    pruneDanglingDependents cache = fmap (M.filter (not . null)) . traverse do
      partitionEithers . map \(metadataObject, dependency) -> case resolveDependency dependency of
        Right ()          -> Right (metadataObject, dependency)
        Left errorMessage -> Left (InconsistentObject errorMessage metadataObject)
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

{- Note [Specialization of buildRebuildableSchemaCache]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
As mentioned in Note [Arrow rewrite rules] in Control.Arrow.Extended and Note [Rule rewrite rules]
in Hasura.Incremental, it is very important that `buildRebuildableSchemaCache` be specialized to
ensure the relevant rules fire. This is a bit subtle, as GHC will only specialize non-class methods
across modules under the following conditions:

    (1) The definition is marked INLINABLE.
    (2) The use site is not overloaded; i.e. all typeclass constraints are satisfied.

This means that even if we mark `buildRebuildableSchemaCache` INLINABLE, GHC still won’t be able to
specialize it unless its immediate use site has a concrete type. If we were to have some polymorphic
function

    foo :: (MonadFoo m) => m Bar
    foo = do { ...; cache <- buildRebuildableSchemaCache; ... }

then GHC would not be able to specialize `buildRebuildableSchemaCache` unless `foo` is also
specialized, since that’s the only way it is able to know which type to specialize it at!

Fortunately, this cross-module specialization is transitive, so as long as we mark `foo` INLINABLE
as well, then when `foo` is specialized, `buildRebuildableSchemaCache` is also specialized. The only
downside to this approach is it means the eventual top-level caller that instantiates the
constraints ends up having to specialize an enormous amount of code all at once, which tends to
bring compile times to a crawl (and may even run out of memory).

A better solution, where possible, is to insert explicit SPECIALIZE pragmas to encourage GHC to do
the specialization early. For example, we could write

    {-# SPECIALIZE foo :: FooM Bar #-}

alongside the definition of `foo`, and GHC will immediately produce a specialized version of `foo`
on `FooM`. If a caller then uses `foo` in `FooM`, it will use the specialized version.

I regret this being necessary, but I don’t see a way around it. -}
