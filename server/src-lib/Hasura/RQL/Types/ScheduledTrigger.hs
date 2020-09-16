-- | These are types for Scheduled Trigger definition; see "Hasura.Eventing.ScheduledTrigger"
module Hasura.RQL.Types.ScheduledTrigger
  ( ScheduledTriggerName(..)
  , CronTriggerMetadata(..)
  , CreateCronTrigger(..)
  , STRetryConf(..)
  , CreateScheduledEvent(..)
  , CronEventId
  , OneOffScheduledEventId
  , formatTime'
  , defaultSTRetryConf
  ) where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.Time.Clock
import           Data.Time.Clock.Units
import           Data.Time.Format.ISO8601
import           Hasura.Incremental
import           Hasura.RQL.Types.Common     (NonNegativeDiffTime, unsafeNonNegativeDiffTime)
import           Hasura.RQL.Types.Action     (InputWebhook(..))
import           Hasura.Prelude
import           System.Cron.Types

import qualified Data.Aeson                    as J
import qualified Data.Text                     as T
import qualified Hasura.RQL.Types.EventTrigger as ET

type CronEventId = Text

type OneOffScheduledEventId = Text

data STRetryConf
  = STRetryConf
  { strcNumRetries           :: !Int
  , strcRetryIntervalSeconds :: !NonNegativeDiffTime
  , strcTimeoutSeconds       :: !NonNegativeDiffTime
  , strcToleranceSeconds     :: !NonNegativeDiffTime
  -- ^ The tolerance configuration is used to determine whether a scheduled
  --   event is not too old to process. The age of the scheduled event is the
  --   difference between the current timestamp and the scheduled event's
  --   timestamp, if the age is than the tolerance then the scheduled event
  --   is marked as dead.
  } deriving (Show, Eq, Generic)

instance NFData STRetryConf
instance Cacheable STRetryConf

instance FromJSON STRetryConf where
  parseJSON = withObject "STRetryConf" \o -> do
    numRetries' <- o .:? "num_retries" .!= 0
    retryInterval <-
      o .:? "retry_interval_seconds" .!= (unsafeNonNegativeDiffTime $ seconds 10)
    timeout <-
      o .:? "timeout_seconds" .!= (unsafeNonNegativeDiffTime $ seconds 60)
    tolerance <-
      o .:? "tolerance_seconds" .!= (unsafeNonNegativeDiffTime $ hours 6)
    if numRetries' < 0
    then fail "num_retries cannot be a negative value"
    else pure $ STRetryConf numRetries' retryInterval timeout tolerance

$(deriveToJSON (aesonDrop 4 snakeCase){omitNothingFields=True} ''STRetryConf)

defaultSTRetryConf :: STRetryConf
defaultSTRetryConf =
  STRetryConf
  { strcNumRetries = 0
  , strcRetryIntervalSeconds = unsafeNonNegativeDiffTime $ seconds 10
  , strcTimeoutSeconds = unsafeNonNegativeDiffTime $ seconds 60
  , strcToleranceSeconds = unsafeNonNegativeDiffTime $ hours 6
  }

data CronTriggerMetadata
  = CronTriggerMetadata
  { ctName              :: !ET.TriggerName
  , ctWebhook           :: !InputWebhook
  , ctSchedule          :: !CronSchedule
  , ctPayload           :: !(Maybe J.Value)
  , ctRetryConf         :: !STRetryConf
  , ctHeaders           :: ![ET.HeaderConf]
  , ctIncludeInMetadata :: !Bool
  , ctComment           :: !(Maybe Text)
  } deriving (Show, Eq, Generic)

instance NFData CronTriggerMetadata
instance Cacheable CronTriggerMetadata

instance FromJSON CronTriggerMetadata where
  parseJSON =
    withObject "CronTriggerMetadata" $ \o -> do
      ctName <- o .: "name"
      ctWebhook <- o .: "webhook"
      ctPayload <- o .:? "payload"
      ctSchedule <- o .: "schedule"
      ctRetryConf <- o .:? "retry_conf" .!= defaultSTRetryConf
      ctHeaders <- o .:? "headers" .!= []
      ctIncludeInMetadata <- o .: "include_in_metadata"
      ctComment <- o .:? "comment"
      pure CronTriggerMetadata {..}

$(deriveToJSON (aesonDrop 2 snakeCase){omitNothingFields=True} ''CronTriggerMetadata)

data CreateCronTrigger
  = CreateCronTrigger
  { cctName              :: !ET.TriggerName
  , cctWebhook           :: !InputWebhook
  , cctCronSchedule      :: !CronSchedule
  , cctPayload           :: !(Maybe J.Value)
  , cctRetryConf         :: !STRetryConf
  , cctHeaders           :: ![ET.HeaderConf]
  , cctIncludeInMetadata :: !Bool
  , cctComment           :: !(Maybe Text)
  , cctReplace           :: !Bool
  } deriving (Show, Eq, Generic)

instance NFData CreateCronTrigger
instance Cacheable CreateCronTrigger

instance FromJSON CreateCronTrigger where
  parseJSON =
    withObject "CreateCronTrigger" $ \o -> do
      cctName <- o .: "name"
      cctWebhook <- o .: "webhook"
      cctPayload <- o .:? "payload"
      cctCronSchedule <- o .: "schedule"
      cctRetryConf <- o .:? "retry_conf" .!= defaultSTRetryConf
      cctHeaders <- o .:? "headers" .!= []
      cctIncludeInMetadata <- o .: "include_in_metadata"
      cctComment <- o .:? "comment"
      cctReplace <- o .:? "replace" .!= False
      pure CreateCronTrigger {..}

$(deriveToJSON (aesonDrop 3 snakeCase){omitNothingFields=True} ''CreateCronTrigger)

newtype ScheduledTriggerName
  = ScheduledTriggerName { unName :: ET.TriggerName }
  deriving (Show, Eq)

$(deriveJSON (aesonDrop 2 snakeCase) ''ScheduledTriggerName)

formatTime' :: UTCTime -> T.Text
formatTime'= T.pack . iso8601Show

data CreateScheduledEvent
  = CreateScheduledEvent
  { cseWebhook           :: !InputWebhook
  , cseScheduleAt        :: !UTCTime
    -- ^ The timestamp should be in the
    -- <ISO 8601 https://en.wikipedia.org/wiki/ISO_8601>
    -- format (which is what @aeson@ expects by default for 'UTCTime').
  , csePayload           :: !(Maybe J.Value)
  , cseHeaders           :: ![ET.HeaderConf]
  , cseRetryConf         :: !STRetryConf
  , cseComment           :: !(Maybe Text)
  } deriving (Show, Eq, Generic)

instance FromJSON CreateScheduledEvent where
  parseJSON =
    withObject "CreateScheduledEvent" $ \o ->
      CreateScheduledEvent <$> o .: "webhook"
                                   <*> o .: "schedule_at"
                                   <*> o .:? "payload"
                                   <*> o .:? "headers" .!= []
                                   <*> o .:? "retry_conf" .!= defaultSTRetryConf
                                   <*> o .:? "comment"

$(deriveToJSON (aesonDrop 3 snakeCase) ''CreateScheduledEvent)
