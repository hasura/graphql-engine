{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.RQL.DDL.ScheduledTrigger
  ( runCreateScheduledTrigger
  , ScheduleType(..)
  , ScheduledTrigger(..)
  , formatTime'
  ) where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.Time.Clock
import           Data.Time.Format
import           Hasura.Db
import           Hasura.EncJSON
import           Hasura.Prelude
import           Hasura.RQL.DDL.Schema.Cache (CacheBuildM)
import           Hasura.RQL.Types            (successMsg)
import           Language.Haskell.TH.Syntax  as TH
import           System.Cron.Parser
import           System.Cron.Types

import qualified Data.Aeson                  as J
import qualified Data.Text                   as T
import qualified Database.PG.Query           as Q

-- aeson doesn't decode 'UTC' identifier so explicitly provide 'Z'
-- TODO: take proper timezone
formatTime' :: UTCTime -> T.Text
formatTime'= T.pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S Z"

instance TH.Lift DiffTime where
  lift x = [|picosecondsToDiffTime x'|]
    where
      x' = diffTimeToPicoseconds x

data RetryConf
  = RetryConf
  { rcNumRetries  :: !Int
  , rcIntervalSec :: !Int
  , rcTimeoutSec  :: !Int
  , rcTolerance   :: !DiffTime
  } deriving (Show, Eq, Lift)

$(deriveJSON (aesonDrop 2 snakeCase){omitNothingFields=True} ''RetryConf)

defaultRetryConf :: RetryConf
defaultRetryConf =
  RetryConf
  { rcNumRetries = 1
  , rcIntervalSec = 10
  , rcTimeoutSec = 60
  , rcTolerance = fromInteger 21600 -- 6 hours
  }

instance TH.Lift UTCTime

data ScheduleType = OneOff UTCTime | Cron CronSchedule
  deriving (Show, Eq, Lift)

$(deriveJSON (defaultOptions){sumEncoding=TaggedObject "type" "value"} ''ScheduleType)

data ScheduledTrigger
  = ScheduledTrigger
  { stName      :: !T.Text
  , stWebhook   :: !T.Text
  , stSchedule  :: !ScheduleType
  , stPayload   :: !(Maybe J.Value)
  , stRetryConf :: !RetryConf
  }
  deriving (Show, Eq, Lift)

instance FromJSON CronSchedule where
  parseJSON = withText "CronSchedule" $ \t ->
    either fail pure $ parseCronSchedule t

instance ToJSON CronSchedule where
  toJSON = J.String . serializeCronSchedule

instance FromJSON ScheduledTrigger where
  parseJSON =
    withObject "ScheduledTriggerQuery " $ \o -> do
      stName <- o .: "name"
      stWebhook <- o .: "webhook"
      stPayload <- o .:? "payload"
      stSchedule <- o .: "schedule"
      stRetryConf <- o .:? "retry_conf" .!= defaultRetryConf
      pure ScheduledTrigger {..}

$(deriveToJSON (aesonDrop 3 snakeCase){omitNothingFields=True} ''ScheduledTrigger)

runCreateScheduledTrigger :: CacheBuildM m => ScheduledTrigger ->  m EncJSON
runCreateScheduledTrigger ScheduledTrigger {..} = do
  liftTx $  Q.unitQE defaultTxErrorHandler
         [Q.sql|
           INSERT into hdb_catalog.hdb_scheduled_trigger
                       (name, webhook, schedule, payload, retry_conf)
           VALUES ($1, $2, $3, $4, $5)
         |] (stName, stWebhook, Q.AltJ stSchedule, Q.AltJ <$> stPayload, Q.AltJ stRetryConf) False
  return successMsg
