{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.RQL.Types.ScheduledTrigger
  ( ScheduleType(..)
  , CreateScheduledTrigger(..)
  , RetryConfST(..)
  , formatTime'
  ) where

import           Data.Time.Clock
import           Data.Time.Format
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.Fixed
import           Hasura.Prelude
import           Language.Haskell.TH.Syntax as TH
import           System.Cron.Types
import           System.Cron.Parser

import qualified Data.Text                     as T
import qualified Data.Aeson                    as J
import qualified Hasura.RQL.Types.EventTrigger as ET

instance TH.Lift (Fixed E12) where
  lift x = [| MkFixed x' |]
    where
      x' = resolution x

instance TH.Lift NominalDiffTime where
  lift x = [| secondsToNominalDiffTime x'|]
    where
      x' =  nominalDiffTimeToSeconds x

instance TH.Lift UTCTime

data RetryConfST
  = RetryConfST
  { rcstNumRetries  :: !Int
  , rcstIntervalSec :: !Int
  , rcstTimeoutSec  :: !Int
  , rcstTolerance   :: !NominalDiffTime
  } deriving (Show, Eq, Lift)

$(deriveJSON (aesonDrop 2 snakeCase){omitNothingFields=True} ''RetryConfST)

defaultRetryConf :: RetryConfST
defaultRetryConf =
  RetryConfST
  { rcstNumRetries = 1
  , rcstIntervalSec = 10
  , rcstTimeoutSec = 60
  , rcstTolerance = 21600 -- 6 hours
  }

data ScheduleType = OneOff UTCTime | Cron CronSchedule
  deriving (Show, Eq, Lift)

$(deriveJSON defaultOptions{sumEncoding=TaggedObject "type" "value"} ''ScheduleType)

data CreateScheduledTrigger
  = CreateScheduledTrigger
  { stName           :: !ET.TriggerName
  , stWebhookConf    :: !ET.WebhookConf
  , stSchedule       :: !ScheduleType
  , stPayload        :: !(Maybe J.Value)
  , stRetryConf      :: !RetryConfST
  , stHeaders        :: !(Maybe [ET.HeaderConf])
  } deriving (Show, Eq, Lift)

instance FromJSON CronSchedule where
  parseJSON = withText "CronSchedule" $ \t ->
    either fail pure $ parseCronSchedule t

instance ToJSON CronSchedule where
  toJSON = J.String . serializeCronSchedule

instance FromJSON CreateScheduledTrigger where
  parseJSON =
    withObject "CreateScheduledTrigger" $ \o -> do
      stName <- o .: "name"
      stWebhook <- o .: "webhook"
      stWebhookFromEnv <- o .: "webhook_from_env"
      stPayload <- o .:? "payload"
      stSchedule <- o .: "schedule"
      stRetryConf <- o .:? "retry_conf" .!= defaultRetryConf
      stHeaders <- o .:? "headers"
      stWebhookConf <- case (stWebhook, stWebhookFromEnv) of
        (Just value, Nothing) -> pure $ ET.WCValue value
        (Nothing, Just env) -> pure $ ET.WCEnv env
        (Just _, Just _)  -> fail "only one of webhook or webhook_from_env should be given"
        (Nothing, Nothing) ->   fail "must provide webhook or webhook_from_env"
      pure CreateScheduledTrigger {..}

$(deriveToJSON (aesonDrop 3 snakeCase){omitNothingFields=True} ''CreateScheduledTrigger)

-- aeson doesn't decode 'UTC' identifier so explicitly provide 'Z'
-- TODO: take proper timezone
formatTime' :: UTCTime -> T.Text
formatTime'= T.pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S Z"
