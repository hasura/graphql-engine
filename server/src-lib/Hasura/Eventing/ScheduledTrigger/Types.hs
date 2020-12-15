module Hasura.Eventing.ScheduledTrigger.Types where

import           Data.Time.Clock
import           Hasura.Eventing.HTTP
import           Hasura.Prelude
import           Hasura.RQL.Types

import qualified Data.Aeson                 as J
import qualified Data.Aeson.Casing          as J
import qualified Data.Aeson.TH              as J
import qualified Database.PG.Query          as Q
import qualified Database.PG.Query.PTI      as PTI
import qualified Hasura.Logging             as L
import qualified PostgreSQL.Binary.Encoding as PE

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

$(J.deriveToJSON (J.aesonDrop 4 J.snakeCase) {J.omitNothingFields = True} ''ScheduledEventWebhookPayload)

newtype ScheduledEventIdArray =
  ScheduledEventIdArray { unScheduledEventIdArray :: [ScheduledEventId]}
  deriving (Show, Eq)

instance Q.ToPrepArg ScheduledEventIdArray where
  toPrepVal (ScheduledEventIdArray l) =
    Q.toPrepValHelper PTI.unknown encoder $ map unEventId l
    where
      -- 25 is the OID value of TEXT, https://jdbc.postgresql.org/development/privateapi/constant-values.html
      encoder = PE.array 25 . PE.dimensionArray foldl' (PE.encodingArray . PE.text_strict)

data ScheduledEventOp
  = SEOpRetry !UTCTime
  | SEOpStatus !ScheduledEventStatus
  deriving (Show, Eq)
