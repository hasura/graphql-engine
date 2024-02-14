{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Funtions related to @hdb_catalog@ schema prior to metadata separation (catalog version < 43).
module Hasura.RQL.DDL.Schema.LegacyCatalog
  ( saveMetadataToHdbTables,
    fetchMetadataFromHdbTables,
    recreateSystemMetadata,
    addCronTriggerForeignKeyConstraint,

    -- * export for testing
    parseLegacyRemoteRelationshipDefinition,
  )
where

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.FileEmbed (makeRelativeToProject)
import Data.HashMap.Strict qualified as HashMap
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.Text.Extended ((<<>))
import Data.Text.NonEmpty
import Data.Time.Clock qualified as C
import Database.PG.Query qualified as PG
import Hasura.Backends.Postgres.Connection
import Hasura.Backends.Postgres.SQL.Types as Postgres
import Hasura.Base.Error
import Hasura.Eventing.ScheduledTrigger
import Hasura.Function.Cache
import Hasura.Function.Metadata (FunctionMetadata (..))
import Hasura.Prelude
import Hasura.RQL.DDL.Action
import Hasura.RQL.DDL.ComputedField
import Hasura.RQL.DDL.Permission
import Hasura.RQL.DDL.RemoteRelationship
import Hasura.RQL.Types.Action
import Hasura.RQL.Types.Allowlist
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.CustomTypes
import Hasura.RQL.Types.EventTrigger
import Hasura.RQL.Types.Metadata
import Hasura.RQL.Types.Permission
import Hasura.RQL.Types.QueryCollection
import Hasura.RQL.Types.Relationships.Local
import Hasura.RQL.Types.Relationships.Remote
import Hasura.RQL.Types.ScheduledTrigger
import Hasura.RQL.Types.SchemaCache
import Hasura.RemoteSchema.Metadata
import Hasura.Table.Metadata
  ( TableMetadata (..),
    mkTableMeta,
    tmArrayRelationships,
    tmComputedFields,
    tmDeletePermissions,
    tmEventTriggers,
    tmInsertPermissions,
    tmObjectRelationships,
    tmRemoteRelationships,
    tmSelectPermissions,
    tmUpdatePermissions,
  )

saveMetadataToHdbTables ::
  (MonadTx m, MonadReader SystemDefined m) => MetadataNoSources -> m ()
saveMetadataToHdbTables
  ( MetadataNoSources
      tables
      functions
      schemas
      collections
      allowlist
      customTypes
      actions
      cronTriggers
    ) = do
    withPathK "tables" $ do
      indexedForM_ tables $ \TableMetadata {..} -> do
        -- Save table
        saveTableToCatalog _tmTable _tmIsEnum _tmConfiguration

        -- Relationships
        withPathK "object_relationships"
          $ indexedForM_ _tmObjectRelationships
          $ \objRel ->
            insertRelationshipToCatalog _tmTable ObjRel objRel
        withPathK "array_relationships"
          $ indexedForM_ _tmArrayRelationships
          $ \arrRel ->
            insertRelationshipToCatalog _tmTable ArrRel arrRel

        -- Computed Fields
        withPathK "computed_fields"
          $ indexedForM_ _tmComputedFields
          $ \(ComputedFieldMetadata name definition comment) ->
            addComputedFieldToCatalog
              $ AddComputedField defaultSource _tmTable name definition comment

        -- Remote Relationships
        withPathK "remote_relationships"
          $ indexedForM_ _tmRemoteRelationships
          $ \RemoteRelationship {..} -> do
            addRemoteRelationshipToCatalog
              $ CreateFromSourceRelationship defaultSource _tmTable _rrName _rrDefinition

        -- Permissions
        withPathK "insert_permissions" $ processPerms _tmTable _tmInsertPermissions
        withPathK "select_permissions" $ processPerms _tmTable _tmSelectPermissions
        withPathK "update_permissions" $ processPerms _tmTable _tmUpdatePermissions
        withPathK "delete_permissions" $ processPerms _tmTable _tmDeletePermissions

        -- Event triggers
        withPathK "event_triggers"
          $ indexedForM_ _tmEventTriggers
          $ \etc -> addEventTriggerToCatalog _tmTable etc

    -- sql functions
    withPathK "functions"
      $ indexedForM_ functions
      $ \(FunctionMetadata function config _ _) -> addFunctionToCatalog function config

    -- query collections
    systemDefined <- ask
    withPathK "query_collections"
      $ indexedForM_ collections
      $ \c -> liftTx $ addCollectionToCatalog c systemDefined

    -- allow list
    withPathK "allowlist" $ do
      indexedForM_ allowlist $ \(AllowlistEntry collectionName scope) -> do
        unless (scope == AllowlistScopeGlobal)
          $ throw400 NotSupported
          $ "cannot downgrade to v1 because the "
          <> collectionName
          <<> " added to the allowlist is a role based allowlist"
        liftTx $ addCollectionToAllowlistCatalog collectionName

    -- remote schemas
    withPathK "remote_schemas"
      $ indexedMapM_ (liftTx . addRemoteSchemaToCatalog) schemas

    -- custom types
    withPathK "custom_types" $ setCustomTypesInCatalog customTypes

    -- cron triggers
    withPathK "cron_triggers"
      $ indexedForM_ cronTriggers
      $ \ct -> liftTx $ do
        addCronTriggerToCatalog ct

    -- actions
    withPathK "actions"
      $ indexedForM_ actions
      $ \action -> do
        let createAction =
              CreateAction (_amName action) (_amDefinition action) (_amComment action)
        addActionToCatalog createAction
        withPathK "permissions"
          $ indexedForM_ (_amPermissions action)
          $ \permission -> do
            let createActionPermission =
                  CreateActionPermission
                    (_amName action)
                    (_apmRole permission)
                    Nothing
                    (_apmComment permission)
            addActionPermissionToCatalog createActionPermission
    where
      processPerms tableName perms = indexedForM_ perms $ \perm -> do
        systemDefined <- ask
        liftTx $ addPermissionToCatalog tableName perm systemDefined

saveTableToCatalog ::
  (MonadTx m, MonadReader SystemDefined m) => QualifiedTable -> Bool -> TableConfig ('Postgres 'Vanilla) -> m ()
saveTableToCatalog (QualifiedObject sn tn) isEnum config = do
  systemDefined <- ask
  liftTx
    $ PG.unitQE
      defaultTxErrorHandler
      [PG.sql|
    INSERT INTO "hdb_catalog"."hdb_table"
      (table_schema, table_name, is_system_defined, is_enum, configuration)
    VALUES ($1, $2, $3, $4, $5)
  |]
      (sn, tn, systemDefined, isEnum, configVal)
      False
  where
    configVal = PG.ViaJSON $ toJSON config

insertRelationshipToCatalog ::
  (MonadTx m, MonadReader SystemDefined m, ToJSON a) =>
  QualifiedTable ->
  RelType ->
  RelDef a ->
  m ()
insertRelationshipToCatalog (QualifiedObject schema table) relType (RelDef name using comment) = do
  systemDefined <- ask
  let args = (schema, table, name, relTypeToTxt relType, PG.ViaJSON using, comment, systemDefined)
  liftTx $ PG.unitQE defaultTxErrorHandler query args True
  where
    query =
      [PG.sql|
      INSERT INTO
        hdb_catalog.hdb_relationship
        (table_schema, table_name, rel_name, rel_type, rel_def, comment, is_system_defined)
      VALUES ($1, $2, $3, $4, $5 :: jsonb, $6, $7) |]

addEventTriggerToCatalog ::
  (MonadTx m, Backend ('Postgres pgKind)) =>
  QualifiedTable ->
  EventTriggerConf ('Postgres pgKind) ->
  m ()
addEventTriggerToCatalog qt etc = liftTx do
  PG.unitQE
    defaultTxErrorHandler
    [PG.sql|
           INSERT into hdb_catalog.event_triggers
                       (name, type, schema_name, table_name, configuration)
           VALUES ($1, 'table', $2, $3, $4)
         |]
    (name, sn, tn, PG.ViaJSON $ toJSON etc)
    False
  where
    QualifiedObject sn tn = qt
    (EventTriggerConf name _ _ _ _ _ _ _ _ _) = etc

addComputedFieldToCatalog ::
  (MonadTx m) =>
  AddComputedField ('Postgres 'Vanilla) ->
  m ()
addComputedFieldToCatalog q =
  liftTx
    $ PG.withQE
      defaultTxErrorHandler
      [PG.sql|
     INSERT INTO hdb_catalog.hdb_computed_field
       (table_schema, table_name, computed_field_name, definition, commentText)
     VALUES ($1, $2, $3, $4, $5)
    |]
      (schemaName, tableName, computedField, PG.ViaJSON definition, commentText)
      True
  where
    commentText = commentToMaybeText comment
    QualifiedObject schemaName tableName = table
    AddComputedField _ table computedField definition comment = q

addRemoteRelationshipToCatalog :: (MonadTx m) => CreateFromSourceRelationship ('Postgres 'Vanilla) -> m ()
addRemoteRelationshipToCatalog CreateFromSourceRelationship {..} =
  liftTx
    $ PG.unitQE
      defaultTxErrorHandler
      [PG.sql|
       INSERT INTO hdb_catalog.hdb_remote_relationship
       (remote_relationship_name, table_schema, table_name, definition)
       VALUES ($1, $2, $3, $4::jsonb)
  |]
      (_crrName, schemaName, tableName, PG.ViaJSON _crrDefinition)
      True
  where
    QualifiedObject schemaName tableName = _crrTable

addFunctionToCatalog ::
  (MonadTx m, MonadReader SystemDefined m) =>
  QualifiedFunction ->
  FunctionConfig ('Postgres 'Vanilla) ->
  m ()
addFunctionToCatalog (QualifiedObject sn fn) config = do
  systemDefined <- ask
  liftTx
    $ PG.unitQE
      defaultTxErrorHandler
      [PG.sql|
         INSERT INTO "hdb_catalog"."hdb_function"
           (function_schema, function_name, configuration, is_system_defined)
         VALUES ($1, $2, $3, $4)
                 |]
      (sn, fn, PG.ViaJSON config, systemDefined)
      False

addRemoteSchemaToCatalog ::
  RemoteSchemaMetadata ->
  PG.TxE QErr ()
addRemoteSchemaToCatalog (RemoteSchemaMetadata name def comment _ _) =
  PG.unitQE
    defaultTxErrorHandler
    [PG.sql|
    INSERT into hdb_catalog.remote_schemas
      (name, definition, comment)
      VALUES ($1, $2, $3)
  |]
    (name, PG.ViaJSON $ toJSON def, comment)
    True

addCollectionToCatalog ::
  (MonadTx m) => CreateCollection -> SystemDefined -> m ()
addCollectionToCatalog (CreateCollection name defn mComment) systemDefined =
  liftTx
    $ PG.unitQE
      defaultTxErrorHandler
      [PG.sql|
    INSERT INTO hdb_catalog.hdb_query_collection
      (collection_name, collection_defn, comment, is_system_defined)
    VALUES ($1, $2, $3, $4)
  |]
      (name, PG.ViaJSON defn, mComment, systemDefined)
      True

addCollectionToAllowlistCatalog :: (MonadTx m) => CollectionName -> m ()
addCollectionToAllowlistCatalog collName =
  liftTx
    $ PG.unitQE
      defaultTxErrorHandler
      [PG.sql|
      INSERT INTO hdb_catalog.hdb_allowlist
                   (collection_name)
            VALUES ($1)
      |]
      (Identity collName)
      True

setCustomTypesInCatalog :: (MonadTx m) => CustomTypes -> m ()
setCustomTypesInCatalog customTypes = liftTx do
  clearCustomTypes
  PG.unitQE
    defaultTxErrorHandler
    [PG.sql|
    INSERT into hdb_catalog.hdb_custom_types
      (custom_types)
      VALUES ($1)
  |]
    (Identity $ PG.ViaJSON customTypes)
    False
  where
    clearCustomTypes = do
      PG.unitQE
        defaultTxErrorHandler
        [PG.sql|
        DELETE FROM hdb_catalog.hdb_custom_types
      |]
        ()
        False

addActionToCatalog :: (MonadTx m) => CreateAction -> m ()
addActionToCatalog (CreateAction actionName actionDefinition comment) = do
  liftTx
    $ PG.unitQE
      defaultTxErrorHandler
      [PG.sql|
    INSERT into hdb_catalog.hdb_action
      (action_name, action_defn, comment)
      VALUES ($1, $2, $3)
  |]
      (actionName, PG.ViaJSON actionDefinition, comment)
      True

addActionPermissionToCatalog :: (MonadTx m) => CreateActionPermission -> m ()
addActionPermissionToCatalog CreateActionPermission {..} = do
  liftTx
    $ PG.unitQE
      defaultTxErrorHandler
      [PG.sql|
    INSERT into hdb_catalog.hdb_action_permission
      (action_name, role_name, comment)
      VALUES ($1, $2, $3)
  |]
      (_capAction, _capRole, _capComment)
      True

addPermissionToCatalog ::
  (MonadTx m, Backend b) =>
  QualifiedTable ->
  PermDef b a ->
  SystemDefined ->
  m ()
addPermissionToCatalog (QualifiedObject sn tn) (PermDef rn qdef mComment) systemDefined =
  liftTx
    $ PG.unitQE
      defaultTxErrorHandler
      [PG.sql|
           INSERT INTO
               hdb_catalog.hdb_permission
               (table_schema, table_name, role_name, perm_type, perm_def, comment, is_system_defined)
           VALUES ($1, $2, $3, $4, $5 :: jsonb, $6, $7)
                |]
      (sn, tn, rn, permTypeToCode (reflectPermDefPermission qdef), PG.ViaJSON qdef, mComment, systemDefined)
      True

addCronTriggerToCatalog :: (MonadTx m) => CronTriggerMetadata -> m ()
addCronTriggerToCatalog CronTriggerMetadata {..} = liftTx $ do
  PG.unitQE
    defaultTxErrorHandler
    [PG.sql|
      INSERT into hdb_catalog.hdb_cron_triggers
        (name, webhook_conf, cron_schedule, payload, retry_conf, header_conf, include_in_metadata, comment)
      VALUES ($1, $2, $3, $4, $5, $6, $7, $8)
    |]
    ( ctName,
      PG.ViaJSON ctWebhook,
      ctSchedule,
      PG.ViaJSON <$> ctPayload,
      PG.ViaJSON ctRetryConf,
      PG.ViaJSON ctHeaders,
      ctIncludeInMetadata,
      ctComment
    )
    False
  currentTime <- liftIO C.getCurrentTime
  let scheduleTimes = generateScheduleTimes currentTime 100 ctSchedule -- generate next 100 events
  insertCronEventsTx $ map (CronEventSeed ctName) scheduleTimes

parseLegacyRemoteRelationshipDefinition ::
  (MonadError QErr m) =>
  Value ->
  m RemoteRelationshipDefinition
parseLegacyRemoteRelationshipDefinition =
  runAesonParser (parseRemoteRelationshipDefinition RRPLegacy)

fetchMetadataFromHdbTables :: (MonadTx m) => m MetadataNoSources
fetchMetadataFromHdbTables = liftTx do
  tables <- fetchTables
  let tableMetaMap = InsOrdHashMap.fromList
        . flip map tables
        $ \(schema, name, isEnum, maybeConfig) ->
          let qualifiedName = QualifiedObject schema name
              configuration = maybe emptyTableConfig PG.getViaJSON maybeConfig
           in (qualifiedName, mkTableMeta qualifiedName isEnum configuration)

  -- Fetch all the relationships
  relationships <- fetchRelationships

  objRelDefs <- mkRelDefs ObjRel relationships
  arrRelDefs <- mkRelDefs ArrRel relationships

  -- Fetch all the permissions
  permissions <- fetchPermissions

  -- Parse all the permissions
  insPermDefs <- mkPermDefs PTInsert permissions
  selPermDefs <- mkPermDefs PTSelect permissions
  updPermDefs <- mkPermDefs PTUpdate permissions
  delPermDefs <- mkPermDefs PTDelete permissions

  -- Fetch all event triggers
  eventTriggers :: [(SchemaName, Postgres.TableName, PG.ViaJSON Value)] <- fetchEventTriggers
  triggerMetaDefs <- mkTriggerMetaDefs eventTriggers

  -- Fetch all computed fields
  computedFields <- fetchComputedFields

  -- Fetch all remote relationships
  remoteRelationshipsRaw <- fetchRemoteRelationships
  remoteRelationships <- for remoteRelationshipsRaw $ \(table, relationshipName, definitionValue) -> do
    definition <- parseLegacyRemoteRelationshipDefinition definitionValue
    pure $ (table, RemoteRelationship relationshipName definition)

  let (_, fullTableMetaMap) = flip runState tableMetaMap $ do
        modMetaMap tmObjectRelationships _rdName objRelDefs
        modMetaMap tmArrayRelationships _rdName arrRelDefs
        modMetaMap tmInsertPermissions _pdRole insPermDefs
        modMetaMap tmSelectPermissions _pdRole selPermDefs
        modMetaMap tmUpdatePermissions _pdRole updPermDefs
        modMetaMap tmDeletePermissions _pdRole delPermDefs
        modMetaMap tmEventTriggers etcName triggerMetaDefs
        modMetaMap tmComputedFields _cfmName computedFields
        modMetaMap tmRemoteRelationships _rrName remoteRelationships

  functions <- fetchFunctions
  remoteSchemas <- oMapFromL _rsmName <$> fetchRemoteSchemas
  collections <- oMapFromL _ccName <$> fetchCollections
  allowlist <- oMapFromL aeCollection <$> fetchAllowlist
  customTypes <- fetchCustomTypes
  actions <- oMapFromL _amName <$> fetchActions
  cronTriggers <- fetchCronTriggers

  pure
    $ MetadataNoSources
      fullTableMetaMap
      functions
      remoteSchemas
      collections
      allowlist
      customTypes
      actions
      cronTriggers
  where
    modMetaMap l f xs = do
      st <- get
      put $ foldl' (\b (qt, dfn) -> b & at qt . _Just . l %~ InsOrdHashMap.insert (f dfn) dfn) st xs

    mkPermDefs pt = mapM permRowToDef . filter (\pr -> pr ^. _4 == pt)

    permRowToDef (sn, tn, rn, _, PG.ViaJSON pDef, mComment) = do
      perm <- decodeValue pDef
      return (QualifiedObject sn tn, PermDef rn perm mComment)

    mkRelDefs rt = mapM relRowToDef . filter (\rr -> rr ^. _4 == rt)

    relRowToDef (sn, tn, rn, _, PG.ViaJSON rDef, mComment) = do
      using <- decodeValue rDef
      return (QualifiedObject sn tn, RelDef rn using mComment)

    mkTriggerMetaDefs = mapM trigRowToDef

    trigRowToDef (sn, tn, PG.ViaJSON configuration) = do
      conf :: EventTriggerConf ('Postgres pgKind) <- decodeValue configuration
      return (QualifiedObject sn tn, conf)

    fetchTables =
      PG.withQE
        defaultTxErrorHandler
        [PG.sql|
                SELECT table_schema, table_name, is_enum, configuration::json
                FROM hdb_catalog.hdb_table
                 WHERE is_system_defined = 'false'
                ORDER BY table_schema ASC, table_name ASC
                    |]
        ()
        False

    fetchRelationships =
      PG.withQE
        defaultTxErrorHandler
        [PG.sql|
                SELECT table_schema, table_name, rel_name, rel_type, rel_def::json, comment
                  FROM hdb_catalog.hdb_relationship
                 WHERE is_system_defined = 'false'
                ORDER BY table_schema ASC, table_name ASC, rel_name ASC
                    |]
        ()
        False

    fetchPermissions =
      PG.withQE
        defaultTxErrorHandler
        [PG.sql|
                SELECT table_schema, table_name, role_name, perm_type, perm_def::json, comment
                  FROM hdb_catalog.hdb_permission
                 WHERE is_system_defined = 'false'
                ORDER BY table_schema ASC, table_name ASC, role_name ASC, perm_type ASC
                    |]
        ()
        False

    fetchEventTriggers =
      PG.withQE
        defaultTxErrorHandler
        [PG.sql|
              SELECT e.schema_name, e.table_name, e.configuration::json
               FROM hdb_catalog.event_triggers e
              ORDER BY e.schema_name ASC, e.table_name ASC, e.name ASC
              |]
        ()
        False

    fetchFunctions = do
      l <-
        PG.withQE
          defaultTxErrorHandler
          [PG.sql|
                SELECT function_schema, function_name, configuration::json
                FROM hdb_catalog.hdb_function
                WHERE is_system_defined = 'false'
                ORDER BY function_schema ASC, function_name ASC
                    |]
          ()
          False
      pure
        $ oMapFromL _fmFunction
        $ flip map l
        $ \(sn, fn, PG.ViaJSON config) ->
          -- function permissions were only introduced post 43rd
          -- migration, so it's impossible we get any permissions
          -- here
          FunctionMetadata (QualifiedObject sn fn) config [] Nothing

    fetchRemoteSchemas =
      map fromRow
        <$> PG.withQE
          defaultTxErrorHandler
          [PG.sql|
         SELECT name, definition, comment
           FROM hdb_catalog.remote_schemas
         ORDER BY name ASC
         |]
          ()
          True
      where
        fromRow (name, PG.ViaJSON def, comment) =
          RemoteSchemaMetadata name def comment mempty mempty

    fetchCollections =
      map fromRow
        <$> PG.withQE
          defaultTxErrorHandler
          [PG.sql|
               SELECT collection_name, collection_defn::json, comment
                 FROM hdb_catalog.hdb_query_collection
                 WHERE is_system_defined = 'false'
               ORDER BY collection_name ASC
              |]
          ()
          False
      where
        fromRow (name, PG.ViaJSON defn, mComment) =
          CreateCollection name defn mComment

    fetchAllowlist =
      map fromRow
        <$> PG.withQE
          defaultTxErrorHandler
          [PG.sql|
          SELECT collection_name
            FROM hdb_catalog.hdb_allowlist
          ORDER BY collection_name ASC
         |]
          ()
          False
      where
        fromRow (Identity name) = AllowlistEntry name AllowlistScopeGlobal

    fetchComputedFields = do
      r <-
        PG.withQE
          defaultTxErrorHandler
          [PG.sql|
              SELECT table_schema, table_name, computed_field_name,
                     definition::json, comment
                FROM hdb_catalog.hdb_computed_field
             |]
          ()
          False
      pure
        $ flip map r
        $ \(schema, table, name, PG.ViaJSON definition, comment) ->
          ( QualifiedObject schema table,
            ComputedFieldMetadata name definition (commentFromMaybeText comment)
          )

    fetchCronTriggers =
      oMapFromL ctName
        . map uncurryCronTrigger
        <$> PG.withQE
          defaultTxErrorHandler
          [PG.sql|
       SELECT ct.name, ct.webhook_conf, ct.cron_schedule, ct.payload,
             ct.retry_conf, ct.header_conf, ct.include_in_metadata, ct.comment
        FROM hdb_catalog.hdb_cron_triggers ct
        WHERE include_in_metadata
      |]
          ()
          False
      where
        uncurryCronTrigger
          (name, webhook, schedule, payload, retryConfig, headerConfig, includeMetadata, comment) =
            CronTriggerMetadata
              { ctName = name,
                ctWebhook = PG.getViaJSON webhook,
                ctSchedule = schedule,
                ctPayload = PG.getViaJSON <$> payload,
                ctRetryConf = PG.getViaJSON retryConfig,
                ctHeaders = PG.getViaJSON headerConfig,
                ctIncludeInMetadata = includeMetadata,
                ctComment = comment,
                ctRequestTransform = Nothing,
                ctResponseTransform = Nothing
              }

    fetchCustomTypes :: PG.TxE QErr CustomTypes
    fetchCustomTypes =
      PG.getViaJSON
        . runIdentity
        . PG.getRow
        <$> PG.rawQE
          defaultTxErrorHandler
          [PG.sql|
         select coalesce((select custom_types::json from hdb_catalog.hdb_custom_types), '{}'::json)
         |]
          []
          False

    fetchActions =
      PG.getViaJSON
        . runIdentity
        . PG.getRow
        <$> PG.rawQE
          defaultTxErrorHandler
          [PG.sql|
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
                            |]
          []
          False

    fetchRemoteRelationships = do
      r <-
        PG.withQE
          defaultTxErrorHandler
          [PG.sql|
                SELECT table_schema, table_name,
                       remote_relationship_name, definition::json
                FROM hdb_catalog.hdb_remote_relationship
             |]
          ()
          False
      pure
        $ flip map r
        $ \(schema, table, name, PG.ViaJSON definition) ->
          ( QualifiedObject schema table,
            name,
            definition
          )

addCronTriggerForeignKeyConstraint :: (MonadTx m) => m ()
addCronTriggerForeignKeyConstraint =
  liftTx
    $ PG.unitQE
      defaultTxErrorHandler
      [PG.sql|
      ALTER TABLE hdb_catalog.hdb_cron_events ADD CONSTRAINT
     hdb_cron_events_trigger_name_fkey FOREIGN KEY (trigger_name)
     REFERENCES hdb_catalog.hdb_cron_triggers(name)
     ON UPDATE CASCADE ON DELETE CASCADE;
     |]
      ()
      False

-- | Drops and recreates all “system-defined” metadata, aka metadata for tables and views in the
-- @information_schema@ and @hdb_catalog@ schemas. These tables and views are tracked to expose them
-- to the console, which allows us to reuse the same functionality we use to implement user-defined
-- APIs to expose the catalog.
--
-- This process has a long and storied history.
--
-- In the past, we reused the same machinery we use for CLI migrations to define our own internal
-- metadata migrations. This caused trouble, however, as we’d have to run those migrations in
-- lockstep with our SQL migrations to ensure the two didn’t get out of sync. This in turn caused
-- trouble because those migrations would hit code paths inside @graphql-engine@ to add or remove
-- things from the @pg_catalog@ tables, and /that/ in turn would fail because we hadn’t finished
-- running the SQL migrations, so we were running a new version of the code against an old version
-- of the schema! That caused #2826.
--
-- To fix that, #2379 switched to the approach of just dropping and recreating all system metadata
-- every time we run any SQL migrations. But /that/ in turn caused trouble due to the way we were
-- constantly rebuilding the schema cache (#3354), causing us to switch to incremental schema cache
-- construction (#3394). However, although that mostly resolved the problem, we still weren’t
-- totally out of the woods, as the incremental construction was still too slow on slow Postgres
-- instances (#3654).
--
-- To sidestep the whole issue, as of #3686 we now just create all the system metadata in code here,
-- and we only rebuild the schema cache once, at the very end. This is a little unsatisfying, since
-- it means our internal migrations are “blessed” compared to user-defined CLI migrations. If we
-- improve CLI migrations further in the future, maybe we can switch back to using that approach,
-- instead.
recreateSystemMetadata :: (MonadTx m) => m ()
recreateSystemMetadata = do
  () <- liftTx $ PG.multiQE defaultTxErrorHandler $(makeRelativeToProject "src-rsr/clear_system_metadata.sql" >>= PG.sqlFromFile)
  flip runReaderT (SystemDefined True) $ for_ systemMetadata \(tableName, tableRels) -> do
    saveTableToCatalog tableName False emptyTableConfig
    for_ tableRels \case
      Left relDef -> insertRelationshipToCatalog tableName ObjRel relDef
      Right relDef -> insertRelationshipToCatalog tableName ArrRel relDef
  where
    systemMetadata :: [(QualifiedTable, [Either (ObjRelDef ('Postgres 'Vanilla)) (ArrRelDef ('Postgres 'Vanilla))])]
    systemMetadata =
      [ table "information_schema" "tables" [],
        table "information_schema" "schemata" [],
        table "information_schema" "views" [],
        table "information_schema" "columns" [],
        table
          "hdb_catalog"
          "hdb_table"
          [ objectRel $$(nonEmptyText "detail")
              $ manualConfig "information_schema" "tables" tableNameMapping,
            objectRel $$(nonEmptyText "primary_key")
              $ manualConfig "hdb_catalog" "hdb_primary_key" tableNameMapping,
            arrayRel $$(nonEmptyText "columns")
              $ manualConfig "information_schema" "columns" tableNameMapping,
            arrayRel $$(nonEmptyText "foreign_key_constraints")
              $ manualConfig "hdb_catalog" "hdb_foreign_key_constraint" tableNameMapping,
            arrayRel $$(nonEmptyText "relationships")
              $ manualConfig "hdb_catalog" "hdb_relationship" tableNameMapping,
            arrayRel $$(nonEmptyText "permissions")
              $ manualConfig "hdb_catalog" "hdb_permission_agg" tableNameMapping,
            arrayRel $$(nonEmptyText "computed_fields")
              $ manualConfig "hdb_catalog" "hdb_computed_field" tableNameMapping,
            arrayRel $$(nonEmptyText "check_constraints")
              $ manualConfig "hdb_catalog" "hdb_check_constraint" tableNameMapping,
            arrayRel $$(nonEmptyText "unique_constraints")
              $ manualConfig "hdb_catalog" "hdb_unique_constraint" tableNameMapping
          ],
        table "hdb_catalog" "hdb_primary_key" [],
        table "hdb_catalog" "hdb_foreign_key_constraint" [],
        table "hdb_catalog" "hdb_relationship" [],
        table "hdb_catalog" "hdb_permission_agg" [],
        table "hdb_catalog" "hdb_computed_field" [],
        table "hdb_catalog" "hdb_check_constraint" [],
        table "hdb_catalog" "hdb_unique_constraint" [],
        table "hdb_catalog" "hdb_remote_relationship" [],
        table
          "hdb_catalog"
          "event_triggers"
          [ arrayRel $$(nonEmptyText "events")
              $ manualConfig "hdb_catalog" "event_log" [("name", "trigger_name")]
          ],
        table
          "hdb_catalog"
          "event_log"
          [ objectRel $$(nonEmptyText "trigger")
              $ manualConfig "hdb_catalog" "event_triggers" [("trigger_name", "name")],
            arrayRel $$(nonEmptyText "logs")
              $ RUFKeyOn
              $ ArrRelUsingFKeyOn (QualifiedObject "hdb_catalog" "event_invocation_logs") (pure "event_id")
          ],
        table
          "hdb_catalog"
          "event_invocation_logs"
          [objectRel $$(nonEmptyText "event") $ RUFKeyOn $ SameTable (pure "event_id")],
        table "hdb_catalog" "hdb_function" [],
        table
          "hdb_catalog"
          "hdb_function_agg"
          [ objectRel $$(nonEmptyText "return_table_info")
              $ manualConfig
                "hdb_catalog"
                "hdb_table"
                [ ("return_type_schema", "table_schema"),
                  ("return_type_name", "table_name")
                ]
          ],
        table "hdb_catalog" "remote_schemas" [],
        table "hdb_catalog" "hdb_version" [],
        table "hdb_catalog" "hdb_query_collection" [],
        table "hdb_catalog" "hdb_allowlist" [],
        table "hdb_catalog" "hdb_custom_types" [],
        table "hdb_catalog" "hdb_action_permission" [],
        table
          "hdb_catalog"
          "hdb_action"
          [ arrayRel $$(nonEmptyText "permissions")
              $ manualConfig
                "hdb_catalog"
                "hdb_action_permission"
                [("action_name", "action_name")]
          ],
        table "hdb_catalog" "hdb_action_log" [],
        table
          "hdb_catalog"
          "hdb_role"
          [ arrayRel $$(nonEmptyText "action_permissions")
              $ manualConfig
                "hdb_catalog"
                "hdb_action_permission"
                [("role_name", "role_name")],
            arrayRel $$(nonEmptyText "permissions")
              $ manualConfig
                "hdb_catalog"
                "hdb_permission_agg"
                [("role_name", "role_name")]
          ],
        table
          "hdb_catalog"
          "hdb_cron_triggers"
          [ arrayRel $$(nonEmptyText "cron_events")
              $ RUFKeyOn
              $ ArrRelUsingFKeyOn (QualifiedObject "hdb_catalog" "hdb_cron_events") (pure "trigger_name")
          ],
        table
          "hdb_catalog"
          "hdb_cron_events"
          [ objectRel $$(nonEmptyText "cron_trigger") $ RUFKeyOn $ SameTable (pure "trigger_name"),
            arrayRel $$(nonEmptyText "cron_event_logs")
              $ RUFKeyOn
              $ ArrRelUsingFKeyOn (QualifiedObject "hdb_catalog" "hdb_cron_event_invocation_logs") (pure "event_id")
          ],
        table
          "hdb_catalog"
          "hdb_cron_event_invocation_logs"
          [ objectRel $$(nonEmptyText "cron_event") $ RUFKeyOn $ SameTable (pure "event_id")
          ],
        table
          "hdb_catalog"
          "hdb_scheduled_events"
          [ arrayRel $$(nonEmptyText "scheduled_event_logs")
              $ RUFKeyOn
              $ ArrRelUsingFKeyOn (QualifiedObject "hdb_catalog" "hdb_scheduled_event_invocation_logs") (pure "event_id")
          ],
        table
          "hdb_catalog"
          "hdb_scheduled_event_invocation_logs"
          [ objectRel $$(nonEmptyText "scheduled_event") $ RUFKeyOn $ SameTable (pure "event_id")
          ]
      ]

    tableNameMapping =
      [ ("table_schema", "table_schema"),
        ("table_name", "table_name")
      ]

    table schemaName tableName relationships = (QualifiedObject schemaName tableName, relationships)
    objectRel name using = Left $ RelDef (RelName name) using Nothing
    arrayRel name using = Right $ RelDef (RelName name) using Nothing
    manualConfig schemaName tableName columns =
      RUManual
        $ RelManualTableConfig
        $ RelManualTableConfigC
          (QualifiedObject schemaName tableName)
          (RelManualCommon (RelMapping $ HashMap.fromList columns) Nothing)
