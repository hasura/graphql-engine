{-# LANGUAGE TypeApplications #-}
module Hasura.RQL.DDL.Metadata
  ( runReplaceMetadata
  , runExportMetadata
  , fetchMetadata
  , runClearMetadata
  , runReloadMetadata
  , runDumpInternalState
  , runGetInconsistentMetadata
  , runDropInconsistentMetadata

  , module Hasura.RQL.DDL.Metadata.Types
  ) where

import           Control.Lens                       hiding ((.=))
import           Data.Aeson

import qualified Data.Aeson.Ordered                 as AO
import qualified Data.HashMap.Strict.InsOrd         as HMIns
import qualified Data.HashSet                       as HS
import qualified Data.List                          as L
import qualified Data.Text                          as T

import           Hasura.EncJSON
import           Hasura.Prelude
import           Hasura.RQL.DDL.ComputedField       (dropComputedFieldFromCatalog)
import           Hasura.RQL.DDL.EventTrigger        (delEventTriggerFromCatalog,
                                                     subTableP2)
import           Hasura.RQL.DDL.Metadata.Types
import           Hasura.RQL.DDL.Permission.Internal (dropPermFromCatalog)
import           Hasura.RQL.DDL.RemoteSchema        (addRemoteSchemaP2,
                                                     buildGCtxMap,
                                                     removeRemoteSchemaFromCatalog)
import           Hasura.RQL.Types
import           Hasura.SQL.Types

import qualified Database.PG.Query                  as Q
import qualified Hasura.RQL.DDL.ComputedField       as ComputedField
import qualified Hasura.RQL.DDL.Permission          as Permission
import qualified Hasura.RQL.DDL.QueryCollection     as Collection
import qualified Hasura.RQL.DDL.Relationship        as Relationship
import qualified Hasura.RQL.DDL.Schema              as Schema

clearMetadata :: Q.TxE QErr ()
clearMetadata = Q.catchE defaultTxErrorHandler $ do
  Q.unitQ "DELETE FROM hdb_catalog.hdb_function WHERE is_system_defined <> 'true'" () False
  Q.unitQ "DELETE FROM hdb_catalog.hdb_permission WHERE is_system_defined <> 'true'" () False
  Q.unitQ "DELETE FROM hdb_catalog.hdb_relationship WHERE is_system_defined <> 'true'" () False
  Q.unitQ "DELETE FROM hdb_catalog.event_triggers" () False
  Q.unitQ "DELETE FROM hdb_catalog.hdb_computed_field" () False
  Q.unitQ "DELETE FROM hdb_catalog.hdb_table WHERE is_system_defined <> 'true'" () False
  Q.unitQ "DELETE FROM hdb_catalog.remote_schemas" () False
  Q.unitQ "DELETE FROM hdb_catalog.hdb_allowlist" () False
  Q.unitQ "DELETE FROM hdb_catalog.hdb_query_collection WHERE is_system_defined <> 'true'" () False

runClearMetadata
  :: (QErrM m, CacheRWM m, MonadTx m, MonadIO m, HasHttpManager m, HasSQLGenCtx m)
  => ClearMetadata -> m EncJSON
runClearMetadata _ = do
  liftTx clearMetadata
  Schema.buildSchemaCacheStrict
  return successMsg

applyQP1
  :: (QErrM m)
  => ReplaceMetadata -> m ()
applyQP1 (ReplaceMetadata _ tables functionsMeta schemas collections allowlist) = do

  withPathK "tables" $ do

    checkMultipleDecls "tables" $ map _tmTable tables

    -- process each table
    void $ indexedForM tables $ \table -> withTableName (table ^. tmTable) $ do
      let allRels  = map Relationship.rdName (table ^. tmObjectRelationships) <>
                     map Relationship.rdName (table ^. tmArrayRelationships)

          insPerms = map Permission.pdRole $ table ^. tmInsertPermissions
          selPerms = map Permission.pdRole $ table ^. tmSelectPermissions
          updPerms = map Permission.pdRole $ table ^. tmUpdatePermissions
          delPerms = map Permission.pdRole $ table ^. tmDeletePermissions
          eventTriggers = map etcName $ table ^. tmEventTriggers
          computedFields = map _cfmName $ table ^. tmComputedFields

      checkMultipleDecls "relationships" allRels
      checkMultipleDecls "insert permissions" insPerms
      checkMultipleDecls "select permissions" selPerms
      checkMultipleDecls "update permissions" updPerms
      checkMultipleDecls "delete permissions" delPerms
      checkMultipleDecls "event triggers" eventTriggers
      checkMultipleDecls "computed fields" computedFields

  withPathK "functions" $
    case functionsMeta of
      FMVersion1 qualifiedFunctions ->
        checkMultipleDecls "functions" qualifiedFunctions
      FMVersion2 functionsV2 ->
        checkMultipleDecls "functions" $ map Schema._tfv2Function functionsV2

  withPathK "remote_schemas" $
    checkMultipleDecls "remote schemas" $ map _arsqName schemas

  withPathK "query_collections" $
    checkMultipleDecls "query collections" $ map Collection._ccName collections

  withPathK "allowlist" $
    checkMultipleDecls "allow list" $ map Collection._crCollection allowlist

  where
    withTableName qt = withPathK (qualObjectToText qt)

    checkMultipleDecls t l = do
      let dups = getDups l
      unless (null dups) $
        throw400 AlreadyExists $ "multiple declarations exist for the following " <> t <> " : "
        <> T.pack (show dups)

    getDups l =
      l L.\\ HS.toList (HS.fromList l)

applyQP2
  :: ( UserInfoM m
     , CacheRWM m
     , MonadTx m
     , MonadIO m
     , HasHttpManager m
     , HasSQLGenCtx m
     , HasSystemDefined m
     )
  => ReplaceMetadata
  -> m EncJSON
applyQP2 (ReplaceMetadata _ tables functionsMeta schemas collections allowlist) = do

  liftTx clearMetadata
  Schema.buildSchemaCacheStrict

  systemDefined <- askSystemDefined
  withPathK "tables" $ do
    -- tables and views
    indexedForM_ tables $ \tableMeta -> do
      let tableName = tableMeta ^. tmTable
          isEnum = tableMeta ^. tmIsEnum
          config = tableMeta ^. tmConfiguration
      void $ Schema.trackExistingTableOrViewP2 tableName systemDefined isEnum config

    indexedForM_ tables $ \table -> do
      -- Relationships
      withPathK "object_relationships" $
        indexedForM_ (table ^. tmObjectRelationships) $ \objRel ->
        Relationship.objRelP2 (table ^. tmTable) objRel
      withPathK "array_relationships" $
        indexedForM_ (table ^. tmArrayRelationships) $ \arrRel ->
        Relationship.arrRelP2 (table ^. tmTable) arrRel
      -- Computed Fields
      withPathK "computed_fields" $
        indexedForM_ (table ^. tmComputedFields) $
          \(ComputedFieldMeta name definition comment) ->
            void $ ComputedField.addComputedFieldP2 $
              ComputedField.AddComputedField (table ^. tmTable) name definition comment

    -- Permissions
    indexedForM_ tables $ \table -> do
      let tableName = table ^. tmTable
      tabInfo <- modifyErrAndSet500 ("apply " <> ) $ askTabInfo tableName
      withPathK "insert_permissions" $ processPerms tabInfo $
        table ^. tmInsertPermissions
      withPathK "select_permissions" $ processPerms tabInfo $
        table ^. tmSelectPermissions
      withPathK "update_permissions" $ processPerms tabInfo $
        table ^. tmUpdatePermissions
      withPathK "delete_permissions" $ processPerms tabInfo $
        table ^. tmDeletePermissions

    indexedForM_ tables $ \table ->
      withPathK "event_triggers" $
        indexedForM_ (table ^. tmEventTriggers) $ \etc ->
        subTableP2 (table ^. tmTable) False etc

  -- sql functions
  withPathK "functions" $ case functionsMeta of
      FMVersion1 qualifiedFunctions -> indexedForM_ qualifiedFunctions $
        \qf -> void $ Schema.trackFunctionP2 qf Schema.emptyFunctionConfig
      FMVersion2 functionsV2 -> indexedForM_ functionsV2 $
        \(Schema.TrackFunctionV2 function config) -> void $ Schema.trackFunctionP2 function config

  -- query collections
  withPathK "query_collections" $
    indexedForM_ collections $ \c -> liftTx $ Collection.addCollectionToCatalog c systemDefined

  -- allow list
  withPathK "allowlist" $ do
    indexedForM_ allowlist $
      \(Collection.CollectionReq name) -> liftTx $ Collection.addCollectionToAllowlistCatalog name
    -- add to cache
    Collection.refreshAllowlist

  -- remote schemas
  withPathK "remote_schemas" $
    indexedMapM_ (void . addRemoteSchemaP2) schemas

  -- build GraphQL Context with Remote schemas
  buildGCtxMap

  return successMsg

  where
    processPerms tabInfo perms =
      indexedForM_ perms $ \permDef -> do
        permInfo <- Permission.addPermP1 tabInfo permDef
        Permission.addPermP2 (_tiName tabInfo) permDef permInfo

runReplaceMetadata
  :: ( QErrM m, UserInfoM m, CacheRWM m, MonadTx m
     , MonadIO m, HasHttpManager m, HasSQLGenCtx m
     , HasSystemDefined m
     )
  => ReplaceMetadata -> m EncJSON
runReplaceMetadata q = do
  applyQP1 q
  applyQP2 q

fetchMetadata :: Q.TxE QErr ReplaceMetadata
fetchMetadata = do
  tables <- Q.catchE defaultTxErrorHandler fetchTables
  let tableMetaMap = HMIns.fromList . flip map tables $
        \(schema, name, isEnum, maybeConfig) ->
          let qualifiedName = QualifiedObject schema name
              configuration = maybe emptyTableConfig Q.getAltJ maybeConfig
          in (qualifiedName, mkTableMeta qualifiedName isEnum configuration)

  -- Fetch all the relationships
  relationships <- Q.catchE defaultTxErrorHandler fetchRelationships

  objRelDefs <- mkRelDefs ObjRel relationships
  arrRelDefs <- mkRelDefs ArrRel relationships

  -- Fetch all the permissions
  permissions <- Q.catchE defaultTxErrorHandler fetchPermissions

  -- Parse all the permissions
  insPermDefs <- mkPermDefs PTInsert permissions
  selPermDefs <- mkPermDefs PTSelect permissions
  updPermDefs <- mkPermDefs PTUpdate permissions
  delPermDefs <- mkPermDefs PTDelete permissions

  -- Fetch all event triggers
  eventTriggers <- Q.catchE defaultTxErrorHandler fetchEventTriggers
  triggerMetaDefs <- mkTriggerMetaDefs eventTriggers

  -- Fetch all computed fields
  computedFields <- fetchComputedFields

  let (_, postRelMap) = flip runState tableMetaMap $ do
        modMetaMap tmObjectRelationships objRelDefs
        modMetaMap tmArrayRelationships arrRelDefs
        modMetaMap tmInsertPermissions insPermDefs
        modMetaMap tmSelectPermissions selPermDefs
        modMetaMap tmUpdatePermissions updPermDefs
        modMetaMap tmDeletePermissions delPermDefs
        modMetaMap tmEventTriggers triggerMetaDefs
        modMetaMap tmComputedFields computedFields

  -- fetch all functions
  functions <- FMVersion2 <$> Q.catchE defaultTxErrorHandler fetchFunctions

  -- fetch all custom resolvers
  remoteSchemas <- fetchRemoteSchemas

  -- fetch all collections
  collections <- fetchCollections

  -- fetch allow list
  allowlist <- map Collection.CollectionReq <$> fetchAllowlists

  return $ ReplaceMetadata currentMetadataVersion (HMIns.elems postRelMap) functions
                           remoteSchemas collections allowlist

  where

    modMetaMap l xs = do
      st <- get
      put $ foldr (\(qt, dfn) b -> b & at qt._Just.l %~ (:) dfn) st xs

    mkPermDefs pt = mapM permRowToDef . filter (\pr -> pr ^. _4 == pt)

    permRowToDef (sn, tn, rn, _, Q.AltJ pDef, mComment) = do
      perm <- decodeValue pDef
      return (QualifiedObject sn tn,  Permission.PermDef rn perm mComment)

    mkRelDefs rt = mapM relRowToDef . filter (\rr -> rr ^. _4 == rt)

    relRowToDef (sn, tn, rn, _, Q.AltJ rDef, mComment) = do
      using <- decodeValue rDef
      return (QualifiedObject sn tn, Relationship.RelDef rn using mComment)

    mkTriggerMetaDefs = mapM trigRowToDef

    trigRowToDef (sn, tn, Q.AltJ configuration) = do
      conf <- decodeValue configuration
      return (QualifiedObject sn tn, conf::EventTriggerConf)

    fetchTables =
      Q.listQ [Q.sql|
                SELECT table_schema, table_name, is_enum, configuration::json
                FROM hdb_catalog.hdb_table
                 WHERE is_system_defined = 'false'
                ORDER BY table_schema ASC, table_name ASC
                    |] () False

    fetchRelationships =
      Q.listQ [Q.sql|
                SELECT table_schema, table_name, rel_name, rel_type, rel_def::json, comment
                  FROM hdb_catalog.hdb_relationship
                 WHERE is_system_defined = 'false'
                ORDER BY table_schema ASC, table_name ASC, rel_name ASC
                    |] () False

    fetchPermissions =
      Q.listQ [Q.sql|
                SELECT table_schema, table_name, role_name, perm_type, perm_def::json, comment
                  FROM hdb_catalog.hdb_permission
                 WHERE is_system_defined = 'false'
                ORDER BY table_schema ASC, table_name ASC, role_name ASC, perm_type ASC
                    |] () False

    fetchEventTriggers =
     Q.listQ [Q.sql|
              SELECT e.schema_name, e.table_name, e.configuration::json
               FROM hdb_catalog.event_triggers e
              ORDER BY e.schema_name ASC, e.table_name ASC, e.name ASC
              |] () False

    fetchFunctions = do
      l <- Q.listQ [Q.sql|
                SELECT function_schema, function_name, configuration::json
                FROM hdb_catalog.hdb_function
                WHERE is_system_defined = 'false'
                ORDER BY function_schema ASC, function_name ASC
                    |] () False
      pure $ flip map l $ \(sn, fn, Q.AltJ config) ->
                            Schema.TrackFunctionV2 (QualifiedObject sn fn) config

    fetchCollections =
      map fromRow <$> Q.listQE defaultTxErrorHandler [Q.sql|
               SELECT collection_name, collection_defn::json, comment
                 FROM hdb_catalog.hdb_query_collection
                 WHERE is_system_defined = 'false'
               ORDER BY collection_name ASC
              |] () False
      where
        fromRow (name, Q.AltJ defn, mComment) =
          Collection.CreateCollection name defn mComment

    fetchAllowlists = map runIdentity <$>
      Q.listQE defaultTxErrorHandler [Q.sql|
          SELECT collection_name
            FROM hdb_catalog.hdb_allowlist
          ORDER BY collection_name ASC
         |] () False

    fetchRemoteSchemas =
      map fromRow <$> Q.listQE defaultTxErrorHandler
        [Q.sql|
         SELECT name, definition, comment
           FROM hdb_catalog.remote_schemas
         ORDER BY name ASC
         |] () True
      where
        fromRow (name, Q.AltJ def, comment) =
          AddRemoteSchemaQuery name def comment

    fetchComputedFields = do
      r <- Q.listQE defaultTxErrorHandler [Q.sql|
              SELECT table_schema, table_name, computed_field_name,
                     definition::json, comment
                FROM hdb_catalog.hdb_computed_field
             |] () False
      pure $ flip map r $ \(schema, table, name, Q.AltJ definition, comment) ->
                          ( QualifiedObject schema table
                          , ComputedFieldMeta name definition comment
                          )

runExportMetadata
  :: (QErrM m, MonadTx m)
  => ExportMetadata -> m EncJSON
runExportMetadata _ =
  (AO.toEncJSON . replaceMetadataToOrdJSON) <$> liftTx fetchMetadata

runReloadMetadata
  :: (QErrM m, CacheRWM m, MonadTx m, MonadIO m, HasHttpManager m, HasSQLGenCtx m)
  => ReloadMetadata -> m EncJSON
runReloadMetadata _ = do
  Schema.buildSchemaCache
  return successMsg

runDumpInternalState
  :: (QErrM m, CacheRM m)
  => DumpInternalState -> m EncJSON
runDumpInternalState _ =
  encJFromJValue <$> askSchemaCache


runGetInconsistentMetadata
  :: (QErrM m, CacheRM m)
  => GetInconsistentMetadata -> m EncJSON
runGetInconsistentMetadata _ = do
  inconsObjs <- scInconsistentObjs <$> askSchemaCache
  return $ encJFromJValue $ object
                [ "is_consistent" .= null inconsObjs
                , "inconsistent_objects" .= inconsObjs
                ]

runDropInconsistentMetadata
  :: (QErrM m, CacheRWM m, MonadTx m)
  => DropInconsistentMetadata -> m EncJSON
runDropInconsistentMetadata _ = do
  sc <- askSchemaCache
  let inconsSchObjs = map _moId $ scInconsistentObjs sc
  mapM_ purgeMetadataObj inconsSchObjs
  writeSchemaCache sc{scInconsistentObjs = []}
  return successMsg

purgeMetadataObj :: MonadTx m => MetadataObjId -> m ()
purgeMetadataObj = liftTx . \case
  (MOTable qt)                            -> Schema.deleteTableFromCatalog qt
  (MOFunction qf)                         -> Schema.delFunctionFromCatalog qf
  (MORemoteSchema rsn)                    -> removeRemoteSchemaFromCatalog rsn
  (MOTableObj qt (MTORel rn _))           -> Relationship.delRelFromCatalog qt rn
  (MOTableObj qt (MTOPerm rn pt))         -> dropPermFromCatalog qt rn pt
  (MOTableObj _ (MTOTrigger trn))         -> delEventTriggerFromCatalog trn
  (MOTableObj qt (MTOComputedField ccn))  -> dropComputedFieldFromCatalog qt ccn
