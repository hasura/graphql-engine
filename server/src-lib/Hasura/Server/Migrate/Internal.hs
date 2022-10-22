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
import Database.PG.Query qualified as Q
import Hasura.Backends.Postgres.Connection
import Hasura.Base.Error
import Hasura.Prelude
import Hasura.RQL.Types.Backend (Backend)
import Hasura.RQL.Types.Common (InputWebhook)
import Hasura.RQL.Types.EventTrigger
import Hasura.SQL.Backend

-- | The old 0.8 catalog version is non-integral, so the version has always been stored
--   as a string.
--   TODO: Change the version column to Float?
getCatalogVersion :: Q.TxE QErr Float
getCatalogVersion = do
  versionText <-
    runIdentity . Q.getRow
      <$> Q.withQE
        defaultTxErrorHandler
        [Q.sql| SELECT version FROM hdb_catalog.hdb_version |]
        ()
        False
  onLeft (readEither $ T.unpack versionText) $
    \err -> throw500 $ "Unexpected: couldn't convert " <> versionText <> " to float, err:" <> tshow err

from3To4 :: forall m. (Backend ('Postgres 'Vanilla), MonadTx m) => m ()
from3To4 = liftTx $
  Q.catchE defaultTxErrorHandler $ do
    Q.unitQ
      [Q.sql|
      ALTER TABLE hdb_catalog.event_triggers
      ADD COLUMN configuration JSON |]
      ()
      False
    eventTriggers <-
      map uncurryEventTrigger
        <$> Q.listQ
          [Q.sql|
      SELECT e.name, e.definition::json, e.webhook, e.num_retries, e.retry_interval, e.headers::json
      FROM hdb_catalog.event_triggers e |]
          ()
          False
    forM_ eventTriggers updateEventTrigger3To4
    Q.unitQ
      [Q.sql|
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
        Q.AltJ (TriggerOpsDef ('Postgres 'Vanilla)),
        InputWebhook,
        Int,
        Int,
        Q.AltJ (Maybe [HeaderConf])
      ) ->
      EventTriggerConf ('Postgres 'Vanilla)
    uncurryEventTrigger (trn, Q.AltJ tDef, w, nr, rint, Q.AltJ headers) =
      EventTriggerConf trn tDef (Just w) Nothing (RetryConf nr rint Nothing) headers Nothing Nothing
    updateEventTrigger3To4 etc@(EventTriggerConf name _ _ _ _ _ _ _) =
      Q.unitQ
        [Q.sql|
                                            UPDATE hdb_catalog.event_triggers
                                            SET
                                            configuration = $1
                                            WHERE name = $2
                                            |]
        (Q.AltJ $ A.toJSON etc, name)
        True

setCatalogVersion :: MonadTx m => Text -> UTCTime -> m ()
setCatalogVersion ver time =
  liftTx $
    Q.unitQE
      defaultTxErrorHandler
      [Q.sql|
    INSERT INTO hdb_catalog.hdb_version (version, upgraded_on) VALUES ($1, $2)
    ON CONFLICT ((version IS NOT NULL))
    DO UPDATE SET version = $1, upgraded_on = $2
  |]
      (ver, time)
      False
