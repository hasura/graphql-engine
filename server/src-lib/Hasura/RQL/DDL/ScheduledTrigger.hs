{-# LANGUAGE DeriveLift      #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.RQL.DDL.ScheduledTrigger
  ( ScheduledTriggerQuery(..)
  , runCreateScheduledTrigger
  , ScheduleType(..)
  ) where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.Time.Clock
import           Hasura.EncJSON
import           Hasura.Prelude
import           Hasura.RQL.DDL.Schema.Cache (CacheBuildM)
import           Hasura.RQL.Types
import           Hasura.RQL.Types.Common     (NonEmptyText)
import           Instances.TH.Lift           ()
import           Language.Haskell.TH.Syntax  as TH
import           System.Cron.Parser
import           System.Cron.Types

import qualified Data.Aeson                  as J
import qualified Data.Text                   as T
import qualified Database.PG.Query           as Q

instance Lift UTCTime

instance Lift CronSchedule where
  lift = cronScheduleExp

cronScheduleExp :: CronSchedule -> Q Exp
cronScheduleExp c = [| c |]

data ScheduleType = OneOff UTCTime | Cron CronSchedule
  deriving (Show, Eq, Lift)

$(deriveJSON (defaultOptions){sumEncoding=TaggedObject "type" "value"} ''ScheduleType)

data ScheduledTriggerQuery
  = ScheduledTriggerQuery
  { stqName     :: !NonEmptyText
  , stqWebhook  :: !NonEmptyText
  , stqSchedule :: !ScheduleType
  }
  deriving (Show, Eq, Lift)

instance FromJSON CronSchedule where
  parseJSON = withText "CronSchedule" $ \t ->
    either fail pure $ parseCronSchedule t

instance ToJSON CronSchedule where
  toJSON = J.String . serializeCronSchedule

$(deriveJSON (aesonDrop 3 snakeCase){omitNothingFields=True} ''ScheduledTriggerQuery)

runCreateScheduledTrigger :: CacheBuildM m => ScheduledTriggerQuery ->  m EncJSON
runCreateScheduledTrigger ScheduledTriggerQuery{..} = do
  liftTx $  Q.unitQE defaultTxErrorHandler
         [Q.sql|
           INSERT into hdb_catalog.hdb_scheduled_trigger
                       (name, webhook, schedule)
           VALUES ($1, $2, $3)
         |] (stqName, stqWebhook, toTxt stqSchedule) False
  return successMsg
  where
    toTxt = \case
      OneOff utcTime -> T.pack $ show utcTime
      Cron cron -> serializeCronSchedule cron
