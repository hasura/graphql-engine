module Ops
  ( initCatalogSafe
  , cleanCatalog
  , migrateCatalog
  , execQuery
  ) where

import           Data.Time.Clock              (UTCTime)
import           Language.Haskell.TH.Syntax   (Q, TExp, unTypeQ)

import           Hasura.Prelude
import           Hasura.RQL.DDL.Schema.Table
import           Hasura.RQL.Types
import           Hasura.Server.Query
import           Hasura.SQL.Types

import qualified Data.Aeson                   as A
import qualified Data.ByteString.Lazy         as BL
import qualified Data.Text                    as T
import qualified Data.Yaml.TH                 as Y

import qualified Database.PG.Query            as Q
import qualified Database.PG.Query.Connection as Q

curCatalogVer :: T.Text
curCatalogVer = "9"

initCatalogSafe
  :: (QErrM m, UserInfoM m, CacheRWM m, MonadTx m, MonadIO m, HasHttpManager m)
  => UTCTime -> m String
initCatalogSafe initTime =  do
  hdbCatalogExists <- liftTx $ Q.catchE defaultTxErrorHandler $
                      doesSchemaExist $ SchemaName "hdb_catalog"
  bool (initCatalogStrict True initTime) onCatalogExists hdbCatalogExists
  where
    onCatalogExists = do
      versionExists <- liftTx $ Q.catchE defaultTxErrorHandler $
                       doesVersionTblExist
                       (SchemaName "hdb_catalog") (TableName "hdb_version")
      bool (initCatalogStrict False initTime) (return initialisedMsg) versionExists

    initialisedMsg = "the state is already initialised"

    doesVersionTblExist sn tblN =
      (runIdentity . Q.getRow) <$> Q.withQ [Q.sql|
           SELECT EXISTS (
               SELECT 1
                 FROM pg_tables
                WHERE schemaname = $1 AND tablename = $2)
               |] (sn, tblN) False

    doesSchemaExist sn =
      (runIdentity . Q.getRow) <$> Q.withQ [Q.sql|
           SELECT EXISTS (
               SELECT 1
                 FROM information_schema.schemata
                WHERE schema_name = $1
           )
                    |] (Identity sn) False

initCatalogStrict
  :: (QErrM m, UserInfoM m, CacheRWM m, MonadTx m, MonadIO m, HasHttpManager m)
  => Bool -> UTCTime -> m String
initCatalogStrict createSchema initTime =  do
  liftTx $ Q.catchE defaultTxErrorHandler $
    when createSchema $ do
      Q.unitQ "CREATE SCHEMA hdb_catalog" () False
      -- This is where the generated views and triggers are stored
      Q.unitQ "CREATE SCHEMA hdb_views" () False

  pgcryptoExtExists <- liftTx $
    Q.catchE defaultTxErrorHandler $ isExtAvailable "pgcrypto"

  if pgcryptoExtExists
    -- only if we created the schema, create the extension
    then when createSchema $ liftTx $ Q.unitQE needsPgCryptoExt
         "CREATE EXTENSION IF NOT EXISTS pgcrypto SCHEMA public" () False
    else throw500 pgcryptoNotAvlMsg

  liftTx $ Q.catchE defaultTxErrorHandler $ do
    Q.Discard () <- Q.multiQ $(Q.sqlFromFile "src-rsr/initialise.sql")
    return ()

  -- add default metadata
  void $ runQueryM metadataQuery

  setAllAsSystemDefined >> addVersion initTime
  return "successfully initialised"

  where
    metadataQuery =
      $(unTypeQ (Y.decodeFile "src-rsr/hdb_metadata.yaml" :: Q (TExp RQLQuery)))
    needsPgCryptoExt :: Q.PGTxErr -> QErr
    needsPgCryptoExt e@(Q.PGTxErr _ _ _ err) =
      case err of
        Q.PGIUnexpected _ -> (err500 PostgresError pgcryptoReqdMsg) { qeInternal = Just $ A.toJSON e }
        Q.PGIStatement pgErr ->
          case Q.edStatusCode pgErr of
            Just "42501" -> err500 PostgresError pgcryptoPermsMsg
            _ -> (err500 PostgresError pgcryptoReqdMsg) { qeInternal = Just $ A.toJSON e }

    addVersion modTime = liftTx $ Q.catchE defaultTxErrorHandler $
      Q.unitQ [Q.sql|
                INSERT INTO "hdb_catalog"."hdb_version"
                (version, upgraded_on) VALUES ($1, $2)
                |] (curCatalogVer, modTime) False

    isExtAvailable :: T.Text -> Q.Tx Bool
    isExtAvailable sn =
      (runIdentity . Q.getRow) <$> Q.withQ [Q.sql|
           SELECT EXISTS (
               SELECT 1
                 FROM pg_catalog.pg_available_extensions
                WHERE name = $1
           )
                    |] (Identity sn) False


migrateMetadata
  :: (MonadTx m, HasHttpManager m, CacheRWM m, UserInfoM m, MonadIO m)
  => RQLQuery -> m ()
migrateMetadata rqlQuery = do
  -- build schema cache
  buildSchemaCache
  -- run the RQL query to migrate metadata
  void $ runQueryM rqlQuery

setAllAsSystemDefined :: (MonadTx m) => m ()
setAllAsSystemDefined = liftTx $ Q.catchE defaultTxErrorHandler $ do
  Q.unitQ "UPDATE hdb_catalog.hdb_table SET is_system_defined = 'true'" () False
  Q.unitQ "UPDATE hdb_catalog.hdb_relationship SET is_system_defined = 'true'" () False
  Q.unitQ "UPDATE hdb_catalog.hdb_permission SET is_system_defined = 'true'" () False
  Q.unitQ "UPDATE hdb_catalog.hdb_query_template SET is_system_defined = 'true'" () False

setAsSystemDefinedFor2 :: (MonadTx m) => m ()
setAsSystemDefinedFor2 =
  liftTx $ Q.catchE defaultTxErrorHandler $
  Q.multiQ [Q.sql|
            UPDATE hdb_catalog.hdb_table
            SET is_system_defined = 'true'
            WHERE table_schema = 'hdb_catalog'
             AND (  table_name = 'event_triggers'
                 OR table_name = 'event_log'
                 OR table_name = 'event_invocation_logs'
                 );
            UPDATE hdb_catalog.hdb_relationship
            SET is_system_defined = 'true'
            WHERE table_schema = 'hdb_catalog'
             AND (  table_name = 'event_triggers'
                 OR table_name = 'event_log'
                 OR table_name = 'event_invocation_logs'
                 );
           |]

setAsSystemDefinedFor5 :: (MonadTx m) => m ()
setAsSystemDefinedFor5 =
  liftTx $ Q.catchE defaultTxErrorHandler $
  Q.multiQ [Q.sql|
            UPDATE hdb_catalog.hdb_table
            SET is_system_defined = 'true'
            WHERE table_schema = 'hdb_catalog'
             AND table_name = 'remote_schemas';
           |]

setAsSystemDefinedFor8 :: (MonadTx m) => m ()
setAsSystemDefinedFor8 =
  liftTx $ Q.catchE defaultTxErrorHandler $
  Q.multiQ [Q.sql|
            UPDATE hdb_catalog.hdb_table
            SET is_system_defined = 'true'
            WHERE table_schema = 'hdb_catalog'
             AND (  table_name = 'hdb_function_agg'
                 OR table_name = 'hdb_function'
                 );
            UPDATE hdb_catalog.hdb_relationship
            SET is_system_defined = 'true'
            WHERE table_schema = 'hdb_catalog'
             AND  table_name = 'hdb_function_agg';
           |]

setAsSystemDefinedFor9 :: (MonadTx m) => m ()
setAsSystemDefinedFor9 =
  liftTx $ Q.catchE defaultTxErrorHandler $
  Q.multiQ [Q.sql|
            UPDATE hdb_catalog.hdb_table
            SET is_system_defined = 'true'
            WHERE table_schema = 'hdb_catalog'
             AND  table_name = 'hdb_version';
           |]


cleanCatalog :: (MonadTx m) => m ()
cleanCatalog = liftTx $ Q.catchE defaultTxErrorHandler $ do
  -- This is where the generated views and triggers are stored
  Q.unitQ "DROP SCHEMA IF EXISTS hdb_views CASCADE" () False
  Q.unitQ "DROP SCHEMA hdb_catalog CASCADE" () False

getCatalogVersion
  :: (MonadTx m)
  => m T.Text
getCatalogVersion = do
  res <- liftTx $ Q.withQE defaultTxErrorHandler [Q.sql|
                SELECT version FROM hdb_catalog.hdb_version
                    |] () False
  return $ runIdentity $ Q.getRow res

from08To1 :: (MonadTx m) => m ()
from08To1 = liftTx $ Q.catchE defaultTxErrorHandler $ do
  Q.unitQ "ALTER TABLE hdb_catalog.hdb_relationship ADD COLUMN comment TEXT NULL" () False
  Q.unitQ "ALTER TABLE hdb_catalog.hdb_permission ADD COLUMN comment TEXT NULL" () False
  Q.unitQ "ALTER TABLE hdb_catalog.hdb_query_template ADD COLUMN comment TEXT NULL" () False
  Q.unitQ [Q.sql|
          UPDATE hdb_catalog.hdb_query_template
             SET template_defn =
                 json_build_object('type', 'select', 'args', template_defn->'select');
                |] () False

from1To2
  :: (MonadTx m, HasHttpManager m, CacheRWM m, UserInfoM m, MonadIO m)
  => m ()
from1To2 = do
  -- migrate database
  Q.Discard () <- liftTx $ Q.multiQE defaultTxErrorHandler
    $(Q.sqlFromFile "src-rsr/migrate_from_1.sql")
  migrateMetadata migrateMetadataFrom1
  -- set as system defined
  setAsSystemDefinedFor2
  where
    migrateMetadataFrom1 =
      $(unTypeQ (Y.decodeFile "src-rsr/migrate_metadata_from_1.yaml" :: Q (TExp RQLQuery)))

from2To3 :: (MonadTx m) => m ()
from2To3 = liftTx $ Q.catchE defaultTxErrorHandler $ do
  Q.unitQ "ALTER TABLE hdb_catalog.event_triggers ADD COLUMN headers JSON" () False
  Q.unitQ "ALTER TABLE hdb_catalog.event_log ADD COLUMN next_retry_at TIMESTAMP" () False
  Q.unitQ "CREATE INDEX ON hdb_catalog.event_log (trigger_id)" () False
  Q.unitQ "CREATE INDEX ON hdb_catalog.event_invocation_logs (event_id)" () False

-- custom resolver
from4To5
  :: (MonadTx m, HasHttpManager m, CacheRWM m, UserInfoM m, MonadIO m)
  => m ()
from4To5 = do
  Q.Discard () <- liftTx $ Q.multiQE defaultTxErrorHandler
    $(Q.sqlFromFile "src-rsr/migrate_from_4_to_5.sql")
  migrateMetadata migrateMetadataFrom4
  -- set as system defined
  setAsSystemDefinedFor5
  where
    migrateMetadataFrom4 =
      $(unTypeQ (Y.decodeFile "src-rsr/migrate_metadata_from_4_to_5.yaml" :: Q (TExp RQLQuery)))

from3To4 :: (MonadTx m) => m ()
from3To4 = liftTx $ Q.catchE defaultTxErrorHandler $ do
  Q.unitQ "ALTER TABLE hdb_catalog.event_triggers ADD COLUMN configuration JSON" () False
  eventTriggers <- map uncurryEventTrigger <$> Q.listQ [Q.sql|
           SELECT e.name, e.definition::json, e.webhook, e.num_retries, e.retry_interval, e.headers::json
           FROM hdb_catalog.event_triggers e
           |] () False
  forM_ eventTriggers updateEventTrigger3To4
  Q.unitQ "ALTER TABLE hdb_catalog.event_triggers\
          \  DROP COLUMN definition\
          \, DROP COLUMN query\
          \, DROP COLUMN webhook\
          \, DROP COLUMN num_retries\
          \, DROP COLUMN retry_interval\
          \, DROP COLUMN headers" () False
  where
    uncurryEventTrigger (trn, Q.AltJ tDef, w, nr, rint, Q.AltJ headers) =
      EventTriggerConf trn tDef (Just w) Nothing (RetryConf nr rint Nothing) headers
    updateEventTrigger3To4 etc@(EventTriggerConf name _ _ _ _ _) = Q.unitQ [Q.sql|
                                         UPDATE hdb_catalog.event_triggers
                                         SET
                                         configuration = $1
                                         WHERE name = $2
                                         |] (Q.AltJ $ A.toJSON etc, name) True

from5To6 :: (MonadTx m) => m ()
from5To6 = liftTx $ do
  -- migrate database
  Q.Discard () <- Q.multiQE defaultTxErrorHandler
    $(Q.sqlFromFile "src-rsr/migrate_from_5_to_6.sql")
  return ()

from6To7 :: (MonadTx m) => m ()
from6To7 = liftTx $ do
  -- migrate database
  Q.Discard () <- Q.multiQE defaultTxErrorHandler
    $(Q.sqlFromFile "src-rsr/migrate_from_6_to_7.sql")
  return ()

from7To8
  :: (MonadTx m, HasHttpManager m, CacheRWM m, UserInfoM m, MonadIO m)
  => m ()
from7To8 = do
  -- migrate database
  Q.Discard () <- liftTx $ Q.multiQE defaultTxErrorHandler
    $(Q.sqlFromFile "src-rsr/migrate_from_7_to_8.sql")
  -- migrate metadata
  migrateMetadata migrateMetadataFrom7
  setAsSystemDefinedFor8
  where
    migrateMetadataFrom7 =
      $(unTypeQ (Y.decodeFile "src-rsr/migrate_metadata_from_7_to_8.yaml" :: Q (TExp RQLQuery)))

-- alter hdb_version table and track it (telemetry changes)
from8To9
  :: (MonadTx m, HasHttpManager m, CacheRWM m, UserInfoM m, MonadIO m)
  => m ()
from8To9 = do
  Q.Discard () <- liftTx $ Q.multiQE defaultTxErrorHandler
    $(Q.sqlFromFile "src-rsr/migrate_from_8_to_9.sql")
  void $ runQueryM migrateMetadataFrom8
  -- set as system defined
  setAsSystemDefinedFor9
  where
    migrateMetadataFrom8 =
      $(unTypeQ (Y.decodeFile "src-rsr/migrate_metadata_from_8_to_9.yaml" :: Q (TExp RQLQuery)))


migrateCatalog
  :: (MonadTx m, CacheRWM m, MonadIO m, UserInfoM m, HasHttpManager m)
  => UTCTime -> m String
migrateCatalog migrationTime = do
  preVer <- getCatalogVersion
  if | preVer == curCatalogVer ->
         return "already at the latest version"
     | preVer == "0.8" -> from08ToCurrent
     | preVer == "1"   -> from1ToCurrent
     | preVer == "2"   -> from2ToCurrent
     | preVer == "3"   -> from3ToCurrent
     | preVer == "4"   -> from4ToCurrent
     | preVer == "5"   -> from5ToCurrent
     | preVer == "6"   -> from6ToCurrent
     | preVer == "7"   -> from7ToCurrent
     | preVer == "8"   -> from8ToCurrent
     | otherwise -> throw400 NotSupported $
                    "unsupported version : " <> preVer
  where
    from8ToCurrent = do
      from8To9
      postMigrate

    from7ToCurrent = do
      from7To8
      from8ToCurrent

    from6ToCurrent = do
      from6To7
      from7ToCurrent

    from5ToCurrent = do
      from5To6
      from6ToCurrent

    from4ToCurrent = do
      from4To5
      from5ToCurrent

    from3ToCurrent = do
      from3To4
      from4ToCurrent

    from2ToCurrent = do
      from2To3
      from3ToCurrent

    from1ToCurrent = do
      from1To2
      from2ToCurrent

    from08ToCurrent = do
      from08To1
      from1ToCurrent

    postMigrate = do
       -- update the catalog version
       updateVersion
       -- try building the schema cache
       buildSchemaCache
       return $ "successfully migrated to " ++ show curCatalogVer

    updateVersion =
      liftTx $ Q.unitQE defaultTxErrorHandler [Q.sql|
                UPDATE "hdb_catalog"."hdb_version"
                   SET "version" = $1,
                       "upgraded_on" = $2
                    |] (curCatalogVer, migrationTime) False

execQuery
  :: (MonadTx m, CacheRWM m, MonadIO m, UserInfoM m, HasHttpManager m)
  => BL.ByteString -> m BL.ByteString
execQuery queryBs = do
  query <- case A.decode queryBs of
    Just jVal -> decodeValue jVal
    Nothing   -> throw400 InvalidJSON "invalid json"
  buildSchemaCache
  runQueryM query


-- error messages
pgcryptoReqdMsg :: T.Text
pgcryptoReqdMsg =
  "pgcrypto extension is required, but could not install; encountered unknown postgres error"

pgcryptoPermsMsg :: T.Text
pgcryptoPermsMsg =
  "pgcrypto extension is required, but current user doesn't have permission to create it. "
  <> "Please grant superuser permission or setup initial schema via "
  <> "https://docs.hasura.io/1.0/graphql/manual/deployment/postgres-permissions.html"

pgcryptoNotAvlMsg :: T.Text
pgcryptoNotAvlMsg =
  "pgcrypto extension is required, but could not find the extension in the "
  <> "PostgreSQL server. Please make sure this extension is available."
