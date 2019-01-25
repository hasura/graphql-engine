{-# LANGUAGE QuasiQuotes #-}
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
  ) where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Language.Haskell.TH.Syntax     (Lift)

import qualified Data.HashMap.Strict            as M
import qualified Data.HashSet                   as HS
import qualified Data.List                      as L
import qualified Data.Text                      as T

import           Hasura.GraphQL.Utils
import           Hasura.Prelude
import           Hasura.RQL.DDL.Utils
import           Hasura.RQL.Types
import           Hasura.SQL.Types

import qualified Database.PG.Query              as Q
import qualified Hasura.RQL.DDL.Permission      as DP
import qualified Hasura.RQL.DDL.QueryTemplate   as DQ
import qualified Hasura.RQL.DDL.Relationship    as DR
import qualified Hasura.RQL.DDL.RemoteSchema    as DRS
import qualified Hasura.RQL.DDL.Schema.Function as DF
import qualified Hasura.RQL.DDL.Schema.Table    as DT
import qualified Hasura.RQL.DDL.Subscribe       as DS
import qualified Hasura.RQL.Types.RemoteSchema  as TRS
import qualified Hasura.RQL.Types.Subscribe     as DTS

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
  Q.unitQ "DELETE FROM hdb_catalog.hdb_query_template WHERE is_system_defined <> 'true'" () False
  Q.unitQ "DELETE FROM hdb_catalog.hdb_function WHERE is_system_defined <> 'true'" () False
  Q.unitQ "DELETE FROM hdb_catalog.hdb_permission WHERE is_system_defined <> 'true'" () False
  Q.unitQ "DELETE FROM hdb_catalog.hdb_relationship WHERE is_system_defined <> 'true'" () False
  Q.unitQ "DELETE FROM hdb_catalog.hdb_table WHERE is_system_defined <> 'true'" () False
  Q.unitQ "DELETE FROM hdb_catalog.event_triggers" () False
  Q.unitQ "DELETE FROM hdb_catalog.remote_schemas" () False
  clearHdbViews

runClearMetadata
  :: ( QErrM m, UserInfoM m, CacheRWM m, MonadTx m
     , MonadIO m, HasHttpManager m)
  => ClearMetadata -> m RespBody
runClearMetadata _ = do
  adminOnly
  liftTx clearMetadata
  DT.buildSchemaCache
  return successMsg

data ReplaceMetadata
  = ReplaceMetadata
  { aqTables         :: ![TableMeta]
  , aqQueryTemplates :: ![DQ.CreateQueryTemplate]
  , aqFunctions      :: !(Maybe [QualifiedFunction])
  , aqRemoteSchemas  :: !(Maybe [TRS.AddRemoteSchemaQuery])
  } deriving (Show, Eq, Lift)

$(deriveJSON (aesonDrop 2 snakeCase){omitNothingFields=True} ''ReplaceMetadata)

applyQP1
  :: (QErrM m, UserInfoM m)
  => ReplaceMetadata -> m ()
applyQP1 (ReplaceMetadata tables templates mFunctions mSchemas) = do

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

  withPathK "queryTemplates" $
    checkMultipleDecls "query templates" $ map DQ.cqtName templates

  withPathK "functions" $
    checkMultipleDecls "functions" functions

  onJust mSchemas $ \schemas ->
      withPathK "remote_schemas" $
        checkMultipleDecls "remote schemas" $ map TRS._arsqName schemas

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
     )
  => ReplaceMetadata
  -> m RespBody
applyQP2 (ReplaceMetadata tables templates mFunctions mSchemas) = do

  liftTx clearMetadata
  DT.buildSchemaCache

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
        DS.subTableP2 (table ^. tmTable) False etc

  -- query templates
  withPathK "queryTemplates" $
    indexedForM_ templates $ \template -> do
      qti <- DQ.createQueryTemplateP1 template
      void $ DQ.createQueryTemplateP2 template qti

  -- sql functions
  withPathK "functions" $
    indexedMapM_ (void . DF.trackFunctionP2) functions

  -- remote schemas
  onJust mSchemas $ \schemas ->
    withPathK "remote_schemas" $
      indexedForM_ schemas $ \conf ->
        void $ DRS.addRemoteSchemaP2 conf

  return successMsg

  where
    functions = fromMaybe [] mFunctions
    processPerms tabInfo perms =
      indexedForM_ perms $ \permDef -> do
        permInfo <- DP.addPermP1 tabInfo permDef
        DP.addPermP2 (tiName tabInfo) permDef permInfo

runReplaceMetadata
  :: (QErrM m, UserInfoM m, CacheRWM m, MonadTx m, MonadIO m, HasHttpManager m)
  => ReplaceMetadata -> m RespBody
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

  -- Fetch all the query templates
  qTmpltRows <- Q.catchE defaultTxErrorHandler fetchQTemplates

  qTmpltDefs <- forM qTmpltRows $ \(qtn, Q.AltJ qtDefVal, mComment) -> do
    qtDef <- decodeValue qtDefVal
    return $ DQ.CreateQueryTemplate qtn qtDef mComment

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

  return $ ReplaceMetadata (M.elems postRelMap) qTmpltDefs
                            (Just functions) (Just schemas)

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

    fetchQTemplates =
      Q.listQ [Q.sql|
                SELECT template_name, template_defn :: json, comment
                  FROM hdb_catalog.hdb_query_template
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
  => ExportMetadata -> m RespBody
runExportMetadata _ = do
  adminOnly
  encode <$> liftTx fetchMetadata

data ReloadMetadata
  = ReloadMetadata
  deriving (Show, Eq, Lift)

instance FromJSON ReloadMetadata where
  parseJSON _ = return ReloadMetadata

$(deriveToJSON defaultOptions ''ReloadMetadata)

runReloadMetadata
  :: ( QErrM m, UserInfoM m, CacheRWM m
     , MonadTx m, MonadIO m, HasHttpManager m)
  => ReloadMetadata -> m RespBody
runReloadMetadata _ = do
  adminOnly
  liftTx $ Q.catchE defaultTxErrorHandler clearHdbViews
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
  => DumpInternalState -> m RespBody
runDumpInternalState _ = do
  adminOnly
  encode <$> askSchemaCache
