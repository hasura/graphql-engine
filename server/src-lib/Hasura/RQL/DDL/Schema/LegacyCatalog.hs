-- | Funtions for fetching/saving @'Metadata' from catalog schema (@hdb_catalog@) prior to
-- metadata separation. These functions are useful for upgrading/downgrading
-- the catalog schema.
module Hasura.RQL.DDL.Schema.LegacyCatalog
  ( saveMetadataToHdbTables
  , fetchMetadataFromHdbTables
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict.InsOrd          as OMap
import qualified Data.HashSet.InsOrd                 as HSIns
import qualified Data.Time.Clock                     as C
import qualified Database.PG.Query                   as Q

import           Control.Lens                        hiding ((.=))
import           Data.Aeson

import           Hasura.Backends.Postgres.Connection
import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.Eventing.ScheduledTrigger
import           Hasura.RQL.DDL.ComputedField
import           Hasura.RQL.DDL.Permission
import           Hasura.RQL.Types

saveMetadataToHdbTables :: (MonadTx m, HasSystemDefined m) => Metadata -> m ()
saveMetadataToHdbTables (Metadata tables functions schemas collections
                         allowlist customTypes actions cronTriggers) = do

  withPathK "tables" $ do
    indexedForM_ tables $ \TableMetadata{..} -> do
      -- Save table
      saveTableToCatalog _tmTable _tmIsEnum _tmConfiguration

      -- Relationships
      withPathK "object_relationships" $
        indexedForM_ _tmObjectRelationships $ \objRel ->
        insertRelationshipToCatalog _tmTable ObjRel objRel
      withPathK "array_relationships" $
        indexedForM_ _tmArrayRelationships $ \arrRel ->
        insertRelationshipToCatalog _tmTable ArrRel arrRel

      -- Computed Fields
      withPathK "computed_fields" $
        indexedForM_ _tmComputedFields $
          \(ComputedFieldMetadata name definition comment) ->
            addComputedFieldToCatalog $
              AddComputedField _tmTable name definition comment

      -- Remote Relationships
      withPathK "remote_relationships" $
        indexedForM_ _tmRemoteRelationships $
          \(RemoteRelationshipMetadata name def) -> do
             let RemoteRelationshipDef rs hf rf = def
             addRemoteRelationshipToCatalog $
               RemoteRelationship name _tmTable hf rs rf

      -- Permissions
      withPathK "insert_permissions" $ processPerms _tmTable _tmInsertPermissions
      withPathK "select_permissions" $ processPerms _tmTable _tmSelectPermissions
      withPathK "update_permissions" $ processPerms _tmTable _tmUpdatePermissions
      withPathK "delete_permissions" $ processPerms _tmTable _tmDeletePermissions

      -- Event triggers
      withPathK "event_triggers" $
        indexedForM_ _tmEventTriggers $ \etc -> addEventTriggerToCatalog _tmTable etc

  -- sql functions
  withPathK "functions" $ indexedForM_ functions $
    \(FunctionMetadata function config) -> addFunctionToCatalog function config

  -- query collections
  systemDefined <- askSystemDefined
  withPathK "query_collections" $
    indexedForM_ collections $ \c -> liftTx $ addCollectionToCatalog c systemDefined

  -- allow list
  withPathK "allowlist" $ do
    indexedForM_ allowlist $ \(CollectionReq name) ->
      liftTx $ addCollectionToAllowlistCatalog name

  -- remote schemas
  withPathK "remote_schemas" $
    indexedMapM_ (liftTx . addRemoteSchemaToCatalog) schemas

  -- custom types
  withPathK "custom_types" $ setCustomTypesInCatalog customTypes

  -- cron triggers
  withPathK "cron_triggers" $
    indexedForM_ cronTriggers $ \ct -> liftTx $ do
    addCronTriggerToCatalog ct

  -- actions
  withPathK "actions" $
    indexedForM_ actions $ \action -> do
      let createAction =
            CreateAction (_amName action) (_amDefinition action) (_amComment action)
      addActionToCatalog createAction
      withPathK "permissions" $
        indexedForM_ (_amPermissions action) $ \permission -> do
          let createActionPermission = CreateActionPermission (_amName action)
                                       (_apmRole permission) Nothing (_apmComment permission)
          addActionPermissionToCatalog createActionPermission

  where
    processPerms tableName perms = indexedForM_ perms $ \perm -> do
      let pt = permAccToType $ getPermAcc1 perm
      systemDefined <- askSystemDefined
      liftTx $ addPermissionToCatalog pt tableName perm systemDefined

saveTableToCatalog
  :: (MonadTx m, HasSystemDefined m) => QualifiedTable -> Bool -> TableConfig -> m ()
saveTableToCatalog (QualifiedObject sn tn) isEnum config = do
  systemDefined <- askSystemDefined
  liftTx $ Q.unitQE defaultTxErrorHandler [Q.sql|
    INSERT INTO "hdb_catalog"."hdb_table"
      (table_schema, table_name, is_system_defined, is_enum, configuration)
    VALUES ($1, $2, $3, $4, $5)
  |] (sn, tn, systemDefined, isEnum, configVal) False
  where
    configVal = Q.AltJ $ toJSON config

insertRelationshipToCatalog
  :: (MonadTx m, HasSystemDefined m, ToJSON a)
  => QualifiedTable
  -> RelType
  -> RelDef a
  -> m ()
insertRelationshipToCatalog (QualifiedObject schema table) relType (RelDef name using comment) = do
  systemDefined <- askSystemDefined
  let args = (schema, table, name, relTypeToTxt relType, Q.AltJ using, comment, systemDefined)
  liftTx $ Q.unitQE defaultTxErrorHandler query args True
  where
    query = [Q.sql|
      INSERT INTO
        hdb_catalog.hdb_relationship
        (table_schema, table_name, rel_name, rel_type, rel_def, comment, is_system_defined)
      VALUES ($1, $2, $3, $4, $5 :: jsonb, $6, $7) |]

addEventTriggerToCatalog
  :: MonadTx m
  => QualifiedTable
  -> EventTriggerConf
  -> m ()
addEventTriggerToCatalog qt etc = liftTx do
  Q.unitQE defaultTxErrorHandler
         [Q.sql|
           INSERT into hdb_catalog.event_triggers
                       (name, type, schema_name, table_name, configuration)
           VALUES ($1, 'table', $2, $3, $4)
         |] (name, sn, tn, Q.AltJ $ toJSON etc) False
  where
    QualifiedObject sn tn = qt
    (EventTriggerConf name _ _ _ _ _) = etc

addComputedFieldToCatalog
  :: MonadTx m
  => AddComputedField -> m ()
addComputedFieldToCatalog q =
  liftTx $ Q.withQE defaultTxErrorHandler
    [Q.sql|
     INSERT INTO hdb_catalog.hdb_computed_field
       (table_schema, table_name, computed_field_name, definition, comment)
     VALUES ($1, $2, $3, $4, $5)
    |] (schemaName, tableName, computedField, Q.AltJ definition, comment) True
  where
    QualifiedObject schemaName tableName = table
    AddComputedField table computedField definition comment = q

addRemoteRelationshipToCatalog :: MonadTx m => RemoteRelationship -> m ()
addRemoteRelationshipToCatalog remoteRelationship = liftTx $
  Q.unitQE defaultTxErrorHandler [Q.sql|
       INSERT INTO hdb_catalog.hdb_remote_relationship
       (remote_relationship_name, table_schema, table_name, definition)
       VALUES ($1, $2, $3, $4::jsonb)
  |] (rtrName remoteRelationship, schemaName, tableName, Q.AltJ definition) True
  where
    QualifiedObject schemaName tableName = rtrTable remoteRelationship
    definition = mkRemoteRelationshipDef remoteRelationship
    mkRemoteRelationshipDef RemoteRelationship {..} =
      RemoteRelationshipDef rtrRemoteSchema rtrHasuraFields rtrRemoteField

addFunctionToCatalog
  :: (MonadTx m, HasSystemDefined m)
  => QualifiedFunction -> FunctionConfig -> m ()
addFunctionToCatalog (QualifiedObject sn fn) config = do
  systemDefined <- askSystemDefined
  liftTx $ Q.unitQE defaultTxErrorHandler [Q.sql|
         INSERT INTO "hdb_catalog"."hdb_function"
           (function_schema, function_name, configuration, is_system_defined)
         VALUES ($1, $2, $3, $4)
                 |] (sn, fn, Q.AltJ config, systemDefined) False

addRemoteSchemaToCatalog
  :: AddRemoteSchemaQuery
  -> Q.TxE QErr ()
addRemoteSchemaToCatalog (AddRemoteSchemaQuery name def comment) =
  Q.unitQE defaultTxErrorHandler [Q.sql|
    INSERT into hdb_catalog.remote_schemas
      (name, definition, comment)
      VALUES ($1, $2, $3)
  |] (name, Q.AltJ $ toJSON def, comment) True

addCollectionToCatalog
  :: MonadTx m => CreateCollection -> SystemDefined -> m ()
addCollectionToCatalog (CreateCollection name defn mComment) systemDefined = liftTx $
  Q.unitQE defaultTxErrorHandler [Q.sql|
    INSERT INTO hdb_catalog.hdb_query_collection
      (collection_name, collection_defn, comment, is_system_defined)
    VALUES ($1, $2, $3, $4)
  |] (name, Q.AltJ defn, mComment, systemDefined) True

addCollectionToAllowlistCatalog :: MonadTx m => CollectionName -> m ()
addCollectionToAllowlistCatalog collName = liftTx $
  Q.unitQE defaultTxErrorHandler [Q.sql|
      INSERT INTO hdb_catalog.hdb_allowlist
                   (collection_name)
            VALUES ($1)
      |] (Identity collName) True

setCustomTypesInCatalog :: MonadTx m => CustomTypes -> m ()
setCustomTypesInCatalog customTypes = liftTx do
  clearCustomTypes
  Q.unitQE defaultTxErrorHandler [Q.sql|
    INSERT into hdb_catalog.hdb_custom_types
      (custom_types)
      VALUES ($1)
  |] (Identity $ Q.AltJ customTypes) False
  where
    clearCustomTypes = do
      Q.unitQE defaultTxErrorHandler [Q.sql|
        DELETE FROM hdb_catalog.hdb_custom_types
      |] () False

addActionToCatalog :: (MonadTx m) =>  CreateAction -> m ()
addActionToCatalog (CreateAction actionName actionDefinition comment) = do
  liftTx $ Q.unitQE defaultTxErrorHandler [Q.sql|
    INSERT into hdb_catalog.hdb_action
      (action_name, action_defn, comment)
      VALUES ($1, $2, $3)
  |] (actionName, Q.AltJ actionDefinition, comment) True

addActionPermissionToCatalog :: (MonadTx m) => CreateActionPermission -> m ()
addActionPermissionToCatalog CreateActionPermission{..}= do
  liftTx $ Q.unitQE defaultTxErrorHandler [Q.sql|
    INSERT into hdb_catalog.hdb_action_permission
      (action_name, role_name, comment)
      VALUES ($1, $2, $3)
  |] (_capAction, _capRole, _capComment) True

addPermissionToCatalog
  :: (ToJSON a, MonadTx m)
  => PermType
  -> QualifiedTable
  -> PermDef a
  -> SystemDefined
  -> m ()
addPermissionToCatalog pt (QualifiedObject sn tn) (PermDef  rn qdef mComment) systemDefined = liftTx $
  Q.unitQE defaultTxErrorHandler [Q.sql|
           INSERT INTO
               hdb_catalog.hdb_permission
               (table_schema, table_name, role_name, perm_type, perm_def, comment, is_system_defined)
           VALUES ($1, $2, $3, $4, $5 :: jsonb, $6, $7)
                |] (sn, tn, rn, permTypeToCode pt, Q.AltJ qdef, mComment, systemDefined) True

addCronTriggerToCatalog :: (MonadTx m) => CronTriggerMetadata ->  m ()
addCronTriggerToCatalog CronTriggerMetadata {..} = liftTx $ do
  Q.unitQE defaultTxErrorHandler
    [Q.sql|
      INSERT into hdb_catalog.hdb_cron_triggers
        (name, webhook_conf, cron_schedule, payload, retry_conf, header_conf, include_in_metadata, comment)
      VALUES ($1, $2, $3, $4, $5, $6, $7, $8)
    |] (ctName, Q.AltJ ctWebhook, ctSchedule, Q.AltJ <$> ctPayload, Q.AltJ ctRetryConf
       ,Q.AltJ ctHeaders, ctIncludeInMetadata, ctComment) False
  currentTime <- liftIO C.getCurrentTime
  let scheduleTimes = generateScheduleTimes currentTime 100 ctSchedule -- generate next 100 events
  insertCronEvents $ map (CronEventSeed ctName) scheduleTimes

fetchMetadataFromHdbTables :: MonadTx m => m Metadata
fetchMetadataFromHdbTables = liftTx do
  tables <- Q.catchE defaultTxErrorHandler fetchTables
  let tableMetaMap = OMap.fromList . flip map tables $
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

  -- Fetch all remote relationships
  remoteRelationships <- Q.catchE defaultTxErrorHandler fetchRemoteRelationships

  let (_, fullTableMetaMap) = flip runState tableMetaMap $ do
        modMetaMap tmObjectRelationships _rdName objRelDefs
        modMetaMap tmArrayRelationships _rdName arrRelDefs
        modMetaMap tmInsertPermissions _pdRole insPermDefs
        modMetaMap tmSelectPermissions _pdRole selPermDefs
        modMetaMap tmUpdatePermissions _pdRole updPermDefs
        modMetaMap tmDeletePermissions _pdRole delPermDefs
        modMetaMap tmEventTriggers etcName triggerMetaDefs
        modMetaMap tmComputedFields _cfmName computedFields
        modMetaMap tmRemoteRelationships _rrmName remoteRelationships

  -- fetch all functions
  functions <- Q.catchE defaultTxErrorHandler fetchFunctions

  -- fetch all remote schemas
  remoteSchemas <- oMapFromL _arsqName <$> fetchRemoteSchemas

  -- fetch all collections
  collections <- oMapFromL _ccName <$> fetchCollections

  -- fetch allow list
  allowlist <- HSIns.fromList . map CollectionReq <$> fetchAllowlists

  customTypes <- fetchCustomTypes

  -- fetch actions
  actions <- oMapFromL _amName <$> fetchActions

  cronTriggers <- fetchCronTriggers

  pure $ Metadata fullTableMetaMap functions remoteSchemas collections
                  allowlist customTypes actions cronTriggers

  where
    modMetaMap l f xs = do
      st <- get
      put $ foldl' (\b (qt, dfn) -> b & at qt._Just.l %~ OMap.insert (f dfn) dfn) st xs

    mkPermDefs pt = mapM permRowToDef . filter (\pr -> pr ^. _4 == pt)

    permRowToDef (sn, tn, rn, _, Q.AltJ pDef, mComment) = do
      perm <- decodeValue pDef
      return (QualifiedObject sn tn,  PermDef rn perm mComment)

    mkRelDefs rt = mapM relRowToDef . filter (\rr -> rr ^. _4 == rt)

    relRowToDef (sn, tn, rn, _, Q.AltJ rDef, mComment) = do
      using <- decodeValue rDef
      return (QualifiedObject sn tn, RelDef rn using mComment)

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
      pure $ oMapFromL _fmFunction $
        flip map l $ \(sn, fn, Q.AltJ config) ->
                       FunctionMetadata (QualifiedObject sn fn) config

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


    fetchCollections =
      map fromRow <$> Q.listQE defaultTxErrorHandler [Q.sql|
               SELECT collection_name, collection_defn::json, comment
                 FROM hdb_catalog.hdb_query_collection
                 WHERE is_system_defined = 'false'
               ORDER BY collection_name ASC
              |] () False
      where
        fromRow (name, Q.AltJ defn, mComment) =
          CreateCollection name defn mComment

    fetchAllowlists = map runIdentity <$>
      Q.listQE defaultTxErrorHandler [Q.sql|
          SELECT collection_name
            FROM hdb_catalog.hdb_allowlist
          ORDER BY collection_name ASC
         |] () False

    fetchComputedFields = do
      r <- Q.listQE defaultTxErrorHandler [Q.sql|
              SELECT table_schema, table_name, computed_field_name,
                     definition::json, comment
                FROM hdb_catalog.hdb_computed_field
             |] () False
      pure $ flip map r $ \(schema, table, name, Q.AltJ definition, comment) ->
                          ( QualifiedObject schema table
                          , ComputedFieldMetadata name definition comment
                          )

    fetchCronTriggers =
      (oMapFromL ctName . map uncurryCronTrigger)
              <$> Q.listQE defaultTxErrorHandler
      [Q.sql|
       SELECT ct.name, ct.webhook_conf, ct.cron_schedule, ct.payload,
             ct.retry_conf, ct.header_conf, ct.include_in_metadata, ct.comment
        FROM hdb_catalog.hdb_cron_triggers ct
        WHERE include_in_metadata
      |] () False
      where
        uncurryCronTrigger
          (name, webhook, schedule, payload, retryConfig, headerConfig, includeMetadata, comment) =
          CronTriggerMetadata
          { ctName = name,
            ctWebhook = Q.getAltJ webhook,
            ctSchedule = schedule,
            ctPayload = Q.getAltJ <$> payload,
            ctRetryConf = Q.getAltJ retryConfig,
            ctHeaders = Q.getAltJ headerConfig,
            ctIncludeInMetadata = includeMetadata,
            ctComment = comment
          }

    fetchCustomTypes :: Q.TxE QErr CustomTypes
    fetchCustomTypes =
      Q.getAltJ . runIdentity . Q.getRow <$>
      Q.rawQE defaultTxErrorHandler [Q.sql|
         select coalesce((select custom_types::json from hdb_catalog.hdb_custom_types), '{}'::json)
         |] [] False

    fetchActions =
      Q.getAltJ . runIdentity . Q.getRow <$> Q.rawQE defaultTxErrorHandler [Q.sql|
        select
          coalesce(
            json_agg(
              json_build_object(
                'name', a.action_name,
                'definition', a.action_defn,
                'comment', a.comment,
                'permissions', ap.permissions
              ) order by a.action_name asc
            ),
            '[]'
          )
        from
          hdb_catalog.hdb_action as a
          left outer join lateral (
            select
              coalesce(
                json_agg(
                  json_build_object(
                    'role', ap.role_name,
                    'comment', ap.comment
                  ) order by ap.role_name asc
                ),
                '[]'
              ) as permissions
            from
              hdb_catalog.hdb_action_permission ap
            where
              ap.action_name = a.action_name
          ) ap on true;
                            |] [] False

    fetchRemoteRelationships = do
      r <- Q.listQ [Q.sql|
                SELECT table_schema, table_name,
                       remote_relationship_name, definition::json
                FROM hdb_catalog.hdb_remote_relationship
             |] () False
      pure $ flip map r $ \(schema, table, name, Q.AltJ definition) ->
                          ( QualifiedObject schema table
                          , RemoteRelationshipMetadata name definition
                          )
