module Hasura.Eventing.ScheduledTrigger.Types where

import           Hasura.Prelude

import qualified Data.Aeson           as J
import qualified Data.Aeson.TH        as J

import           Data.Time.Clock

import qualified Hasura.Logging       as L

import           Hasura.Base.Error
import           Hasura.Eventing.HTTP
import           Hasura.RQL.Types


newtype ScheduledTriggerInternalErr
  = ScheduledTriggerInternalErr QErr
  deriving (Show, Eq)

instance L.ToEngineLog ScheduledTriggerInternalErr L.Hasura where
  toEngineLog (ScheduledTriggerInternalErr qerr) =
    (L.LevelError, L.scheduledTriggerLogType, J.toJSON qerr)

data CronTriggerStats
  = CronTriggerStats
  { ctsName                :: !TriggerName
  , ctsUpcomingEventsCount :: !Int
  , ctsMaxScheduledTime    :: !UTCTime
  } deriving (Show, Eq)

data RetryContext
  = RetryContext
  { _rctxTries :: !Int
  , _rctxConf  :: !STRetryConf
  } deriving (Show, Eq)

data ScheduledEventWebhookPayload
  = ScheduledEventWebhookPayload
  { sewpId            :: !EventId
  , sewpName          :: !(Maybe TriggerName)
  , sewpScheduledTime :: !UTCTime
  , sewpPayload       :: !J.Value
  , sewpComment       :: !(Maybe Text)
  , sewpCreatedAt     :: !(Maybe UTCTime)
  -- ^ sewpCreatedAt is the time at which the event was created,
  -- In case of one-off scheduled events, it's the time at which
  -- the user created the event and in case of cron triggers, the
  -- graphql-engine generator, generates the cron events, the
  -- `created_at` is just an implementation detail, so we
  -- don't send it
  } deriving (Show, Eq)

$(J.deriveToJSON hasuraJSON {J.omitNothingFields = True} ''ScheduledEventWebhookPayload)

data ScheduledEventOp
  = SEOpRetry !UTCTime
  | SEOpStatus !ScheduledEventStatus
  deriving (Show, Eq)
