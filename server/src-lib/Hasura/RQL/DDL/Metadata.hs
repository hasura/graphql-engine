module Hasura.RQL.DDL.Metadata
  ( TableMeta

  , ReplaceMetadata(..)
  , runReplaceMetadata

  , ExportMetadata(..)
  , runExportMetadata
  , fetchMetadata

  , ClearMetadata(..)
  , runClearMetadata

  , ReloadMetadata(..)
  , runReloadMetadata

  , DumpInternalState(..)
  , runDumpInternalState

  , GetInconsistentMetadata
  , runGetInconsistentMetadata

  , DropInconsistentMetadata
  , runDropInconsistentMetadata
  ) where

import           Control.Lens                       hiding ((.=))
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Language.Haskell.TH.Syntax         (Lift)

import qualified Data.HashMap.Strict                as M
import qualified Data.HashSet                       as HS
import qualified Data.List                          as L
import qualified Data.Text                          as T

import           Hasura.EncJSON
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.SQL.Types

import qualified Database.PG.Query                  as Q
import qualified Hasura.RQL.DDL.EventTrigger        as DE
import qualified Hasura.RQL.DDL.Permission          as DP
import qualified Hasura.RQL.DDL.Permission.Internal as DP
import qualified Hasura.RQL.DDL.QueryCollection     as DQC
import qualified Hasura.RQL.DDL.Relationship        as DR
import qualified Hasura.RQL.DDL.RemoteSchema        as DRS
import qualified Hasura.RQL.DDL.Schema.Function     as DF
import qualified Hasura.RQL.DDL.Schema.Table        as DT
import qualified Hasura.RQL.Types.EventTrigger      as DTS
import qualified Hasura.RQL.Types.RemoteSchema      as TRS

data TableMeta
  = TableMeta
  { _tmTable               :: !QualifiedTable
  , _tmObjectRelationships :: ![DR.ObjRelDef]
  , _tmArrayRelationships  :: ![DR.ArrRelDef]
  , _tmInsertPermissions   :: ![DP.InsPermDef]
  , _tmSelectPermissions   :: ![DP.SelPermDef]
  , _tmUpdatePermissions   :: ![DP.UpdPermDef]
  , _tmDeletePermissions   :: ![DP.DelPermDef]
  , _tmEventTriggers       :: ![DTS.EventTriggerConf]
  } deriving (Show, Eq, Lift)

mkTableMeta :: QualifiedTable -> TableMeta
mkTableMeta qt =
  TableMeta qt [] [] [] [] [] [] []

makeLenses ''TableMeta

instance FromJSON TableMeta where
  parseJSON (Object o) = do
    unless (null unexpectedKeys) $
      fail $ "unexpected keys when parsing TableMetadata : "
      <> show (HS.toList unexpectedKeys)

    TableMeta
     <$> o .: tableKey
     <*> o .:? orKey .!= []
     <*> o .:? arKey .!= []
     <*> o .:? ipKey .!= []
     <*> o .:? spKey .!= []
     <*> o .:? upKey .!= []
     <*> o .:? dpKey .!= []
     <*> o .:? etKey .!= []

    where
      tableKey = "table"
      orKey = "object_relationships"
      arKey = "array_relationships"
      ipKey = "insert_permissions"
      spKey = "select_permissions"
      upKey = "update_permissions"
      dpKey = "delete_permissions"
      etKey = "event_triggers"

      unexpectedKeys =
        HS.fromList (M.keys o) `HS.difference` expectedKeySet

      expectedKeySet =
        HS.fromList [ tableKey, orKey, arKey, ipKey
                    , spKey, upKey, dpKey, etKey
                    ]

  parseJSON _ =
    fail "expecting an Object for TableMetadata"

$(deriveToJSON (aesonDrop 3 snakeCase){omitNothingFields=True} ''TableMeta)

data ClearMetadata
  = ClearMetadata
  deriving (Show, Eq, Lift)
$(deriveToJSON defaultOptions ''ClearMetadata)

instance FromJSON ClearMetadata where
  parseJSON _ = return ClearMetadata

clearMetadata :: Q.TxE QErr ()
clearMetadata = Q.catchE defaultTxErrorHandler $ do
  Q.unitQ "DELETE FROM hdb_catalog.hdb_function WHERE is_system_defined <> 'true'" () False
  Q.unitQ "DELETE FROM hdb_catalog.hdb_permission WHERE is_system_defined <> 'true'" () False
  Q.unitQ "DELETE FROM hdb_catalog.hdb_relationship WHERE is_system_defined <> 'true'" () False
  Q.unitQ "DELETE FROM hdb_catalog.event_triggers" () False
  Q.unitQ "DELETE FROM hdb_catalog.hdb_table WHERE is_system_defined <> 'true'" () False
  Q.unitQ "DELETE FROM hdb_catalog.remote_schemas" () False
  Q.unitQ "DELETE FROM hdb_catalog.hdb_allowlist" () False
  Q.unitQ "DELETE FROM hdb_catalog.hdb_query_collection WHERE is_system_defined <> 'true'" () False

runClearMetadata
  :: ( QErrM m, UserInfoM m, CacheRWM m, MonadTx m
     , MonadIO m, HasHttpManager m, HasSQLGenCtx m
     )
  => ClearMetadata -> m EncJSON
runClearMetadata _ = do
  adminOnly
  liftTx clearMetadata
  DT.buildSchemaCacheStrict
  return successMsg

data ReplaceMetadata
  = ReplaceMetadata
  { aqTables           :: ![TableMeta]
  , aqFunctions        :: !(Maybe [QualifiedFunction])
  , aqRemoteSchemas    :: !(Maybe [TRS.AddRemoteSchemaQuery])
  , aqQueryCollections :: !(Maybe [DQC.CreateCollection])
  , aqAllowlist        :: !(Maybe [DQC.CollectionReq])
  } deriving (Show, Eq, Lift)

$(deriveJSON (aesonDrop 2 snakeCase){omitNothingFields=True} ''ReplaceMetadata)

applyQP1
  :: (QErrM m, UserInfoM m)
  => ReplaceMetadata -> m ()
applyQP1 (ReplaceMetadata tables mFunctions mSchemas mCollections mAllowlist) = do

  adminOnly

  withPathK "tables" $ do

    checkMultipleDecls "tables" $ map _tmTable tables

    -- process each table
    void $ indexedForM tables $ \table -> withTableName (table ^. tmTable) $ do
      let allRels  = map DR.rdName (table ^. tmObjectRelationships) <>
                     map DR.rdName (table ^. tmArrayRelationships)

          insPerms = map DP.pdRole $ table ^. tmInsertPermissions
          selPerms = map DP.pdRole $ table ^. tmSelectPermissions
          updPerms = map DP.pdRole $ table ^. tmUpdatePermissions
          delPerms = map DP.pdRole $ table ^. tmDeletePermissions
          eventTriggers = map DTS.etcName $ table ^. tmEventTriggers

      checkMultipleDecls "relationships" allRels
      checkMultipleDecls "insert permissions" insPerms
      checkMultipleDecls "select permissions" selPerms
      checkMultipleDecls "update permissions" updPerms
      checkMultipleDecls "delete permissions" delPerms
      checkMultipleDecls "event triggers" eventTriggers

  withPathK "functions" $
    checkMultipleDecls "functions" functions

  onJust mSchemas $ \schemas ->
      withPathK "remote_schemas" $
        checkMultipleDecls "remote schemas" $ map TRS._arsqName schemas

  onJust mCollections $ \collections ->
    withPathK "query_collections" $
        checkMultipleDecls "query collections" $ map DQC._ccName collections

  onJust mAllowlist $ \allowlist ->
    withPathK "allowlist" $
        checkMultipleDecls "allow list" $ map DQC._crCollection allowlist

  where
    withTableName qt = withPathK (qualObjectToText qt)
    functions = fromMaybe [] mFunctions

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
     )
  => ReplaceMetadata
  -> m EncJSON
applyQP2 (ReplaceMetadata tables mFunctions mSchemas mCollections mAllowlist) = do

  liftTx clearMetadata
  DT.buildSchemaCacheStrict

  withPathK "tables" $ do

    -- tables and views
    indexedForM_ (map _tmTable tables) $ \tableName ->
      void $ DT.trackExistingTableOrViewP2 tableName False

    -- Relationships
    indexedForM_ tables $ \table -> do
      withPathK "object_relationships" $
        indexedForM_ (table ^. tmObjectRelationships) $ \objRel ->
        DR.objRelP2 (table ^. tmTable) objRel
      withPathK "array_relationships" $
        indexedForM_ (table ^. tmArrayRelationships) $ \arrRel ->
        DR.arrRelP2 (table ^. tmTable) arrRel

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
        DE.subTableP2 (table ^. tmTable) False etc

  -- sql functions
  withPathK "functions" $
    indexedMapM_ (void . DF.trackFunctionP2) functions

  -- query collections
  withPathK "query_collections" $
    indexedForM_ collections $ \c -> do
    liftTx $ DQC.addCollectionToCatalog c

  -- allow list
  withPathK "allowlist" $ do
    indexedForM_ allowlist $ \(DQC.CollectionReq name) -> do
      liftTx $ DQC.addCollectionToAllowlistCatalog name
    -- add to cache
    DQC.refreshAllowlist

  -- remote schemas
  onJust mSchemas $ \schemas ->
    withPathK "remote_schemas" $
      indexedMapM_ (void . DRS.addRemoteSchemaP2) schemas

  -- build GraphQL Context with Remote schemas
  DRS.buildGCtxMap

  return successMsg

  where
    functions = fromMaybe [] mFunctions
    collections = fromMaybe [] mCollections
    allowlist = fromMaybe [] mAllowlist
    processPerms tabInfo perms =
      indexedForM_ perms $ \permDef -> do
        permInfo <- DP.addPermP1 tabInfo permDef
        DP.addPermP2 (tiName tabInfo) permDef permInfo

runReplaceMetadata
  :: ( QErrM m, UserInfoM m, CacheRWM m, MonadTx m
     , MonadIO m, HasHttpManager m, HasSQLGenCtx m
     )
  => ReplaceMetadata -> m EncJSON
runReplaceMetadata q = do
  applyQP1 q
  applyQP2 q

data ExportMetadata
  = ExportMetadata
  deriving (Show, Eq, Lift)

instance FromJSON ExportMetadata where
  parseJSON _ = return ExportMetadata

$(deriveToJSON defaultOptions ''ExportMetadata)

fetchMetadata :: Q.TxE QErr ReplaceMetadata
fetchMetadata = do
  tables <- Q.catchE defaultTxErrorHandler fetchTables
  let qts          = map (uncurry QualifiedObject) tables
      tableMetaMap = M.fromList $ zip qts $ map mkTableMeta qts

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

  let (_, postRelMap) = flip runState tableMetaMap $ do
        modMetaMap tmObjectRelationships objRelDefs
        modMetaMap tmArrayRelationships arrRelDefs
        modMetaMap tmInsertPermissions insPermDefs
        modMetaMap tmSelectPermissions selPermDefs
        modMetaMap tmUpdatePermissions updPermDefs
        modMetaMap tmDeletePermissions delPermDefs
        modMetaMap tmEventTriggers triggerMetaDefs

  -- fetch all functions
  functions <- map (uncurry QualifiedObject) <$>
    Q.catchE defaultTxErrorHandler fetchFunctions

  -- fetch all custom resolvers
  schemas <- DRS.fetchRemoteSchemas

  -- fetch all collections
  collections <- DQC.fetchAllCollections

  -- fetch allow list
  allowlist <- map DQC.CollectionReq <$> DQC.fetchAllowlist

  return $ ReplaceMetadata (M.elems postRelMap) (Just functions)
                           (Just schemas) (Just collections) (Just allowlist)

  where

    modMetaMap l xs = do
      st <- get
      put $ foldr (\(qt, dfn) b -> b & at qt._Just.l %~ (:) dfn) st xs

    mkPermDefs pt = mapM permRowToDef . filter (\pr -> pr ^. _4 == pt)

    permRowToDef (sn, tn, rn, _, Q.AltJ pDef, mComment) = do
      perm <- decodeValue pDef
      return (QualifiedObject sn tn,  DP.PermDef rn perm mComment)

    mkRelDefs rt = mapM relRowToDef . filter (\rr -> rr ^. _4 == rt)

    relRowToDef (sn, tn, rn, _, Q.AltJ rDef, mComment) = do
      using <- decodeValue rDef
      return (QualifiedObject sn tn, DR.RelDef rn using mComment)

    mkTriggerMetaDefs = mapM trigRowToDef

    trigRowToDef (sn, tn, Q.AltJ configuration) = do
      conf <- decodeValue configuration
      return (QualifiedObject sn tn, conf::EventTriggerConf)

    fetchTables =
      Q.listQ [Q.sql|
                SELECT table_schema, table_name from hdb_catalog.hdb_table
                 WHERE is_system_defined = 'false'
                    |] () False

    fetchRelationships =
      Q.listQ [Q.sql|
                SELECT table_schema, table_name, rel_name, rel_type, rel_def::json, comment
                  FROM hdb_catalog.hdb_relationship
                 WHERE is_system_defined = 'false'
                    |] () False

    fetchPermissions =
      Q.listQ [Q.sql|
                SELECT table_schema, table_name, role_name, perm_type, perm_def::json, comment
                  FROM hdb_catalog.hdb_permission
                 WHERE is_system_defined = 'false'
                    |] () False

    fetchEventTriggers =
     Q.listQ [Q.sql|
              SELECT e.schema_name, e.table_name, e.configuration::json
               FROM hdb_catalog.event_triggers e
              |] () False
    fetchFunctions =
      Q.listQ [Q.sql|
                SELECT function_schema, function_name
                FROM hdb_catalog.hdb_function
                WHERE is_system_defined = 'false'
                    |] () False

runExportMetadata
  :: (QErrM m, UserInfoM m, MonadTx m)
  => ExportMetadata -> m EncJSON
runExportMetadata _ = do
  adminOnly
  encJFromJValue <$> liftTx fetchMetadata

data ReloadMetadata
  = ReloadMetadata
  deriving (Show, Eq, Lift)

instance FromJSON ReloadMetadata where
  parseJSON _ = return ReloadMetadata

$(deriveToJSON defaultOptions ''ReloadMetadata)

runReloadMetadata
  :: ( QErrM m, UserInfoM m, CacheRWM m
     , MonadTx m, MonadIO m, HasHttpManager m, HasSQLGenCtx m
     )
  => ReloadMetadata -> m EncJSON
runReloadMetadata _ = do
  adminOnly
  DT.buildSchemaCache
  return successMsg

data DumpInternalState
  = DumpInternalState
  deriving (Show, Eq, Lift)

instance FromJSON DumpInternalState where
  parseJSON _ = return DumpInternalState

$(deriveToJSON defaultOptions ''DumpInternalState)

runDumpInternalState
  :: (QErrM m, UserInfoM m, CacheRM m)
  => DumpInternalState -> m EncJSON
runDumpInternalState _ = do
  adminOnly
  encJFromJValue <$> askSchemaCache


data GetInconsistentMetadata
  = GetInconsistentMetadata
  deriving (Show, Eq, Lift)

instance FromJSON GetInconsistentMetadata where
  parseJSON _ = return GetInconsistentMetadata

$(deriveToJSON defaultOptions ''GetInconsistentMetadata)

runGetInconsistentMetadata
  :: (QErrM m, UserInfoM m, CacheRM m)
  => GetInconsistentMetadata -> m EncJSON
runGetInconsistentMetadata _ = do
  adminOnly
  inconsObjs <- scInconsistentObjs <$> askSchemaCache
  return $ encJFromJValue $ object
                [ "is_consistent" .= null inconsObjs
                , "inconsistent_objects" .= inconsObjs
                ]

data DropInconsistentMetadata
 = DropInconsistentMetadata
 deriving(Show, Eq, Lift)

instance FromJSON DropInconsistentMetadata where
  parseJSON _ = return DropInconsistentMetadata

$(deriveToJSON defaultOptions ''DropInconsistentMetadata)

runDropInconsistentMetadata
  :: (QErrM m, UserInfoM m, CacheRWM m, MonadTx m)
  => DropInconsistentMetadata -> m EncJSON
runDropInconsistentMetadata _ = do
  adminOnly
  sc <- askSchemaCache
  let inconsSchObjs = map _moId $ scInconsistentObjs sc
  mapM_ purgeMetadataObj inconsSchObjs
  writeSchemaCache sc{scInconsistentObjs = []}
  return successMsg

purgeMetadataObj :: MonadTx m => MetadataObjId -> m ()
purgeMetadataObj = liftTx . \case
  (MOTable qt) ->
    Q.catchE defaultTxErrorHandler $ DT.delTableFromCatalog qt
  (MOFunction qf)                 -> DF.delFunctionFromCatalog qf
  (MORemoteSchema rsn)            -> DRS.removeRemoteSchemaFromCatalog rsn
  (MOTableObj qt (MTORel rn _))   -> DR.delRelFromCatalog qt rn
  (MOTableObj qt (MTOPerm rn pt)) -> DP.dropPermFromCatalog qt rn pt
  (MOTableObj _ (MTOTrigger trn)) -> DE.delEventTriggerFromCatalog trn
