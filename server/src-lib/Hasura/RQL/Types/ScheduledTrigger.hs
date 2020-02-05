{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.RQL.Types.ScheduledTrigger
  ( ScheduleType(..)
  , CreateScheduledTrigger(..)
  , RetryConfST(..)
  , formatTime'
  ) where

import           Data.Time.Clock
import           Data.Time.Clock.Units
import           Data.Time.Format
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Hasura.Prelude
import           System.Cron.Types
import           System.Cron.Parser
import           Hasura.Incremental

import qualified Data.Text                     as T
import qualified Data.Aeson                    as J
import qualified Hasura.RQL.Types.EventTrigger as ET

data RetryConfST
  = RetryConfST
  { rcstNumRetries  :: !Int
  , rcstIntervalSec :: !Seconds
  , rcstTimeoutSec  :: !Seconds
  , rcstTolerance   :: !NominalDiffTime
  } deriving (Show, Eq, Generic)

instance NFData RetryConfST
instance Cacheable RetryConfST

$(deriveJSON (aesonDrop 2 snakeCase){omitNothingFields=True} ''RetryConfST)

defaultRetryConf :: RetryConfST
defaultRetryConf =
  RetryConfST
  { rcstNumRetries = 0
  , rcstIntervalSec = seconds 10
  , rcstTimeoutSec = seconds 60
  , rcstTolerance = 21600 -- 6 hours
  }

data ScheduleType = OneOff UTCTime | Cron CronSchedule
  deriving (Show, Eq, Generic)

instance NFData ScheduleType
instance Cacheable ScheduleType

$(deriveJSON defaultOptions{sumEncoding=TaggedObject "type" "value"} ''ScheduleType)

data CreateScheduledTrigger
  = CreateScheduledTrigger
  { stName           :: !ET.TriggerName
  , stWebhookConf    :: !ET.WebhookConf
  , stSchedule       :: !ScheduleType
  , stPayload        :: !(Maybe J.Value)
  , stRetryConf      :: !RetryConfST
  , stHeaders        :: ![ET.HeaderConf]
  } deriving (Show, Eq, Generic)

instance NFData CreateScheduledTrigger
instance Cacheable CreateScheduledTrigger

instance FromJSON CronSchedule where
  parseJSON = withText "CronSchedule" $ \t ->
    either fail pure $ parseCronSchedule t

instance ToJSON CronSchedule where
  toJSON = J.String . serializeCronSchedule

instance FromJSON CreateScheduledTrigger where
  parseJSON =
    withObject "CreateScheduledTrigger" $ \o -> do
      stName <- o .: "name"
      stWebhook <- o .:? "webhook"
      stWebhookFromEnv <- o .:? "webhook_from_env"
      stPayload <- o .:? "payload"
      stSchedule <- o .: "schedule"
      stRetryConf <- o .:? "retry_conf" .!= defaultRetryConf
      stHeaders <- o .:? "headers" .!= []
      stWebhookConf <- case (stWebhook, stWebhookFromEnv) of
        (Just value, Nothing) -> pure $ ET.WCValue value
        (Nothing, Just env) -> pure $ ET.WCEnv env
        (Just _, Just _)  -> fail "only one of webhook or webhook_from_env should be given"
        (Nothing, Nothing) ->   fail "must provide webhook or webhook_from_env"
      pure CreateScheduledTrigger {..}

$(deriveToJSON (aesonDrop 2 snakeCase){omitNothingFields=True} ''CreateScheduledTrigger)

-- Supported time string formats for the API:
-- (see FromJSON for ZonedTime: https://hackage.haskell.org/package/aeson-1.4.6.0/docs/src/Data.Aeson.Types.FromJSON.html#line-2050)

-- YYYY-MM-DD HH:MM Z YYYY-MM-DD HH:MM:SS Z YYYY-MM-DD HH:MM:SS.SSS Z

-- The first space may instead be a T, and the second space is optional. The Z represents UTC.
-- The Z may be replaced with a time zone offset of the form +0000 or -08:00,
-- where the first two digits are hours, the : is optional and the second two digits (also optional) are minutes.
formatTime' :: UTCTime -> T.Text
formatTime'= T.pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S Z"
