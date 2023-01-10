{-# LANGUAGE QuasiQuotes #-}

module Hasura.Server.Migrate.Internal
  ( getCatalogVersion,
    from3To4,
    setCatalogVersion,
  )
where

import Data.Aeson qualified as A
import Data.Text qualified as T
import Data.Time.Clock (UTCTime)
import Database.PG.Query qualified as PG
import Hasura.Backends.Postgres.Connection
import Hasura.Base.Error
import Hasura.Prelude
import Hasura.RQL.Types.Backend (Backend)
import Hasura.RQL.Types.Common (InputWebhook, TriggerOnReplication (..))
import Hasura.RQL.Types.EventTrigger
import Hasura.SQL.Backend
import Hasura.Server.Migrate.Version

-- | The old 0.8 catalog version is non-integral, so the version has always been
-- stored as a string.
getCatalogVersion :: PG.TxE QErr MetadataCatalogVersion
getCatalogVersion = do
  versionText <-
    runIdentity . PG.getRow
      <$> PG.withQE
        defaultTxErrorHandler
        [PG.sql| SELECT version FROM hdb_catalog.hdb_version |]
        ()
        False
  onLeft (readEither $ T.unpack versionText) $
    \err -> throw500 $ "Unexpected: couldn't convert read catalog version " <> versionText <> ", err:" <> tshow err

from3To4 :: forall m. (Backend ('Postgres 'Vanilla), MonadTx m) => m ()
from3To4 = liftTx $
  PG.catchE defaultTxErrorHandler $ do
    PG.unitQ
      [PG.sql|
      ALTER TABLE hdb_catalog.event_triggers
      ADD COLUMN configuration JSON |]
      ()
      False
    eventTriggers <-
      map uncurryEventTrigger
        <$> PG.withQ
          [PG.sql|
      SELECT e.name, e.definition::json, e.webhook, e.num_retries, e.retry_interval, e.headers::json
      FROM hdb_catalog.event_triggers e |]
          ()
          False
    forM_ eventTriggers updateEventTrigger3To4
    PG.unitQ
      [PG.sql|
      ALTER TABLE hdb_catalog.event_triggers
      DROP COLUMN definition,
      DROP COLUMN query,
      DROP COLUMN webhook,
      DROP COLUMN num_retries,
      DROP COLUMN retry_interval,
      DROP COLUMN headers,
      DROP COLUMN metadataTransform|]
      ()
      False
  where
    uncurryEventTrigger ::
      ( TriggerName,
        PG.ViaJSON (TriggerOpsDef ('Postgres 'Vanilla)),
        InputWebhook,
        Int,
        Int,
        PG.ViaJSON (Maybe [HeaderConf])
      ) ->
      EventTriggerConf ('Postgres 'Vanilla)
    uncurryEventTrigger (trn, PG.ViaJSON tDef, w, nr, rint, PG.ViaJSON headers) =
      EventTriggerConf trn tDef (Just w) Nothing (RetryConf nr rint Nothing) headers Nothing Nothing Nothing TORDisableTrigger
    updateEventTrigger3To4 etc@(EventTriggerConf name _ _ _ _ _ _ _ _ _) =
      PG.unitQ
        [PG.sql|
                                            UPDATE hdb_catalog.event_triggers
                                            SET
                                            configuration = $1
                                            WHERE name = $2
                                            |]
        (PG.ViaJSON $ A.toJSON etc, name)
        True

setCatalogVersion :: MonadTx m => Text -> UTCTime -> m ()
setCatalogVersion ver time =
  liftTx $
    PG.unitQE
      defaultTxErrorHandler
      [PG.sql|
    INSERT INTO hdb_catalog.hdb_version (version, upgraded_on) VALUES ($1, $2)
    ON CONFLICT ((version IS NOT NULL))
    DO UPDATE SET version = $1, upgraded_on = $2
  |]
      (ver, time)
      False
