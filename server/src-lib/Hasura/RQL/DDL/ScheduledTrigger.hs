module Hasura.RQL.DDL.ScheduledTrigger
  ( runCreateScheduledTrigger
  ) where

import           Hasura.Db
import           Hasura.EncJSON
import           Hasura.Prelude
import           Hasura.RQL.DDL.Schema.Cache       (CacheBuildM)
import           Hasura.RQL.Types.Helpers
import           Hasura.RQL.Types.ScheduledTrigger

import qualified Database.PG.Query                 as Q

runCreateScheduledTrigger :: CacheBuildM m => ScheduledTrigger ->  m EncJSON
runCreateScheduledTrigger ScheduledTrigger {..} = do
  liftTx $  Q.unitQE defaultTxErrorHandler
         [Q.sql|
           INSERT into hdb_catalog.hdb_scheduled_trigger
                       (name, webhook, schedule, payload, retry_conf)
           VALUES ($1, $2, $3, $4, $5)
         |] (stName, stWebhook, Q.AltJ stSchedule, Q.AltJ <$> stPayload, Q.AltJ stRetryConf) False
  return successMsg
