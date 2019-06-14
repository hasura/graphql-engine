{- |
Description: Migrations for Hasura catalog.
-}

module Migrate
  ( curCatalogVer
  , migrateCatalog
  )
where

import           Data.Time.Clock            (UTCTime)
import           Language.Haskell.TH.Syntax (Q, TExp, unTypeQ)

import           Hasura.Prelude
import           Hasura.RQL.DDL.Schema
import           Hasura.RQL.Types
import           Hasura.Server.Query

import qualified Data.Aeson                 as A
import qualified Data.Text                  as T
import qualified Data.Yaml.TH               as Y

import qualified Database.PG.Query          as Q


type Migration m =
  ( MonadTx m
  , HasHttpManager m
  , HasSystemDefined m
  , CacheRWM m
  , UserInfoM m
  , MonadIO m
  , HasSQLGenCtx m
  )

curCatalogVer :: T.Text
curCatalogVer = "24"

getCatalogVersion :: MonadTx m => m T.Text
getCatalogVersion = do
  res <- liftTx $ Q.withQE defaultTxErrorHandler [Q.sql|
    SELECT version FROM hdb_catalog.hdb_version |] () False
  return $ runIdentity $ Q.getRow res

migrateCatalog :: Migration m => UTCTime -> m String
migrateCatalog migrationTime = migrateFrom =<< getCatalogVersion
  where
    migrateFrom previousVersion
      | previousVersion == curCatalogVer =
          return $ "already at the latest version. current version: " <> show curCatalogVer
      | [] <- neededMigrations =
          throw400 NotSupported $ "unsupported version : " <> previousVersion
      | otherwise =
          traverse_ snd neededMigrations *> postMigrate
      where
        neededMigrations = dropWhile ((/= previousVersion) . fst) migrations
        migrations =
          [ ("0.8", from08To1)
          , ("1", from1To2)
          , ("2", from2To3)
          , ("3", from3To4)
          , ("4", from4To5)
          , ("5", from5To6)
          , ("6", from6To7)
          , ("7", from7To8)
          , ("8", from8To9)
          , ("9", from9To10)
          , ("10", from10To11)
          , ("11", from11To12)
          , ("12", from12To13)
          , ("13", from13To14)
          , ("14", from14To15)
          , ("15", from15To16)
          , ("16", from16To17)
          , ("17", from17To18)
          , ("18", from18To19)
          , ("19", from19To20)
          , ("20", from20To21)
          , ("21", from21To22)
          , ("22", from22To23)
          , ("23", from23To24)
          ]

    postMigrate = do
      -- update the catalog version
      updateVersion
      -- replace system metadata
      clearSystemMetadata
      createSystemMetadata
      -- try building the schema cache
      buildSchemaCacheStrict
      return $ "successfully migrated to " ++ show curCatalogVer

    updateVersion =
      liftTx $ Q.unitQE defaultTxErrorHandler [Q.sql|
                UPDATE "hdb_catalog"."hdb_version"
                   SET "version" = $1,
                       "upgraded_on" = $2
                    |] (curCatalogVer, migrationTime) False

runTx :: MonadTx m => Q.Query -> m ()
runTx = liftTx . Q.multiQE defaultTxErrorHandler

clearSystemMetadata :: MonadTx m => m ()
clearSystemMetadata = runTx $(Q.sqlFromFile "src-rsr/clear_system_metadata.sql")

createSystemMetadata :: Migration m => m ()
createSystemMetadata = void $ runQueryM rqlQuery
  where
    rqlQuery = $(unTypeQ (Y.decodeFile "src-rsr/hdb_metadata.yaml" :: Q (TExp RQLQuery)))

from08To1 :: MonadTx m => m ()
from08To1 = runTx $(Q.sqlFromFile "src-rsr/migrate_from_08_to_1.sql")

from1To2 :: MonadTx m => m ()
from1To2 = runTx $(Q.sqlFromFile "src-rsr/migrate_from_1_to_2.sql")

from2To3 :: MonadTx m => m ()
from2To3 = runTx $(Q.sqlFromFile "src-rsr/migrate_from_2_to_3.sql")

from3To4 :: MonadTx m => m ()
from3To4 = liftTx $ Q.catchE defaultTxErrorHandler $ do
  Q.unitQ [Q.sql|
    ALTER TABLE hdb_catalog.event_triggers
    ADD COLUMN configuration JSON |] () False
  eventTriggers <- map uncurryEventTrigger <$> Q.listQ [Q.sql|
    SELECT e.name, e.definition::json, e.webhook, e.num_retries, e.retry_interval, e.headers::json
    FROM hdb_catalog.event_triggers e |] () False
  forM_ eventTriggers updateEventTrigger3To4
  Q.unitQ [Q.sql|
    ALTER TABLE hdb_catalog.event_triggers
    DROP COLUMN definition,
    DROP COLUMN query,
    DROP COLUMN webhook,
    DROP COLUMN num_retries,
    DROP COLUMN retry_interval,
    DROP COLUMN headers |] () False
  where
    uncurryEventTrigger (trn, Q.AltJ tDef, w, nr, rint, Q.AltJ headers) =
      EventTriggerConf trn tDef (Just w) Nothing (RetryConf nr rint Nothing) headers
    updateEventTrigger3To4 etc@(EventTriggerConf name _ _ _ _ _) = Q.unitQ [Q.sql|
                                         UPDATE hdb_catalog.event_triggers
                                         SET
                                         configuration = $1
                                         WHERE name = $2
                                         |] (Q.AltJ $ A.toJSON etc, name) True

from4To5 :: MonadTx m => m ()
from4To5 = runTx $(Q.sqlFromFile "src-rsr/migrate_from_4_to_5.sql")

from5To6 :: MonadTx m => m ()
from5To6 = runTx $(Q.sqlFromFile "src-rsr/migrate_from_5_to_6.sql")

from6To7 :: MonadTx m => m ()
from6To7 = runTx $(Q.sqlFromFile "src-rsr/migrate_from_6_to_7.sql")

from7To8 :: MonadTx m => m ()
from7To8 = runTx $(Q.sqlFromFile "src-rsr/migrate_from_7_to_8.sql")

from8To9 :: MonadTx m => m ()
from8To9 = runTx $(Q.sqlFromFile "src-rsr/migrate_from_8_to_9.sql")

from9To10 :: MonadTx m => m ()
from9To10 = runTx $(Q.sqlFromFile "src-rsr/migrate_from_9_to_10.sql")

from10To11 :: MonadTx m => m ()
from10To11 = runTx $(Q.sqlFromFile "src-rsr/migrate_from_10_to_11.sql")

from11To12 :: MonadTx m => m ()
from11To12 = runTx $(Q.sqlFromFile "src-rsr/migrate_from_11_to_12.sql")

from12To13 :: MonadTx m => m ()
from12To13 = runTx $(Q.sqlFromFile "src-rsr/migrate_from_12_to_13.sql")

from13To14 :: MonadTx m => m ()
from13To14 = runTx $(Q.sqlFromFile "src-rsr/migrate_from_13_to_14.sql")

from14To15 :: MonadTx m => m ()
from14To15 = runTx $(Q.sqlFromFile "src-rsr/migrate_from_14_to_15.sql")

from15To16 :: MonadTx m => m ()
from15To16 = runTx $(Q.sqlFromFile "src-rsr/migrate_from_15_to_16.sql")

from16To17 :: MonadTx m => m ()
from16To17 = runTx $(Q.sqlFromFile "src-rsr/migrate_from_16_to_17.sql")

from17To18 :: MonadTx m => m ()
from17To18 =
  liftTx $ Q.catchE defaultTxErrorHandler $
  Q.multiQ [Q.sql|
            DELETE FROM hdb_catalog.hdb_table
            WHERE table_schema = 'hdb_catalog'
              AND table_name = 'hdb_query_template';
            DROP table hdb_catalog.hdb_query_template
           |]

from18To19 :: MonadTx m => m ()
from18To19 = do
  -- Migrate database
  Q.Discard () <- liftTx $ Q.multiQE defaultTxErrorHandler
    $(Q.sqlFromFile "src-rsr/migrate_from_18_to_19.sql")
  return ()

from19To20 :: (MonadTx m) => m ()
from19To20 = do
  Q.Discard () <- liftTx $ Q.multiQE defaultTxErrorHandler
    $(Q.sqlFromFile "src-rsr/migrate_from_19_to_20.sql")
  pure ()

from20To21 :: (MonadTx m) => m ()
from20To21 = liftTx $ Q.catchE defaultTxErrorHandler $ do
  Q.unitQ "CREATE INDEX ON hdb_catalog.event_log (locked)" () False

from21To22 :: (MonadTx m) => m ()
from21To22 = do
  Q.Discard () <- liftTx $ Q.multiQE defaultTxErrorHandler
    $(Q.sqlFromFile "src-rsr/migrate_from_21_to_22.sql")
  pure ()

from22To23 :: (MonadTx m) => m ()
from22To23 = do
  Q.Discard () <- liftTx $ Q.multiQE defaultTxErrorHandler
    $(Q.sqlFromFile "src-rsr/migrate_from_22_to_23.sql")
  pure ()

from23To24 :: MonadTx m => m ()
from23To24 =
  liftTx $ Q.catchE defaultTxErrorHandler $
  Q.multiQ [Q.sql|
            ALTER TABLE hdb_catalog.hdb_table
            ADD COLUMN configuration JSONB NOT NULL DEFAULT '{}'::jsonb;
           |]
