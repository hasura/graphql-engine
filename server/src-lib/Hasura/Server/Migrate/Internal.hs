module Hasura.Server.Migrate.Internal
  ( runTx
  , getCatalogVersion
  , from3To4
  )
where
import           Hasura.Backends.Postgres.Connection
import           Hasura.Prelude
import           Hasura.RQL.Types.EventTrigger

import qualified Data.Aeson                          as A
import qualified Database.PG.Query                   as Q

runTx :: (MonadTx m) => Q.Query -> m ()
runTx = liftTx . Q.multiQE defaultTxErrorHandler

-- | The old 0.8 catalog version is non-integral, so we store it in the database as a
-- string.
getCatalogVersion :: MonadTx m => m Text
getCatalogVersion = liftTx $ runIdentity . Q.getRow <$> Q.withQE defaultTxErrorHandler
  [Q.sql| SELECT version FROM hdb_catalog.hdb_version |] () False

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
