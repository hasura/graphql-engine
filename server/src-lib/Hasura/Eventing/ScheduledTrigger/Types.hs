module Hasura.Eventing.ScheduledTrigger.Types where

import           Control.Arrow.Extended      (dup)
import           Control.Concurrent.Extended (sleep)
import           Control.Concurrent.STM.TVar
import           Data.Has
import           Data.Int                    (Int64)
import           Data.List                   (unfoldr)
import           Data.Time.Clock
import           Hasura.Eventing.Common
import           Hasura.Eventing.HTTP
import           Hasura.HTTP
import           Hasura.Prelude
import           Hasura.RQL.DDL.EventTrigger (getHeaderInfosFromConf)
import           Hasura.RQL.DDL.Headers
import           Hasura.RQL.Types
import           Hasura.Server.Version       (HasVersion)
import           Hasura.SQL.DML
import           Hasura.SQL.Types
import           System.Cron

import qualified Data.Aeson                  as J
import qualified Data.Aeson.Casing           as J
import qualified Data.Aeson.TH               as J
import qualified Data.ByteString.Lazy        as BL
import qualified Data.Environment            as Env
import qualified Data.HashMap.Strict         as Map
import qualified Data.Set                    as Set
import qualified Data.TByteString            as TBS
import qualified Data.Text                   as T
import qualified Database.PG.Query           as Q
import qualified Database.PG.Query.PTI       as PTI
import qualified Hasura.Logging              as L
import qualified Hasura.Tracing              as Tracing
import qualified Network.HTTP.Client         as HTTP
import qualified PostgreSQL.Binary.Decoding  as PD
import qualified PostgreSQL.Binary.Encoding  as PE
import qualified Text.Builder                as TB (run)
newtype ScheduledTriggerInternalErr
  = ScheduledTriggerInternalErr QErr
  deriving (Show, Eq)

instance L.ToEngineLog ScheduledTriggerInternalErr L.Hasura where
  toEngineLog (ScheduledTriggerInternalErr qerr) =
    (L.LevelError, L.scheduledTriggerLogType, J.toJSON qerr)

cronEventsTable :: QualifiedTable
cronEventsTable =
  QualifiedObject
    hdbCatalogSchema
    (TableName $ T.pack "hdb_cron_events")

data ScheduledEventStatus
  = SESScheduled
  | SESLocked
  | SESDelivered
  | SESError
  | SESDead
  deriving (Show, Eq)

scheduledEventStatusToText :: ScheduledEventStatus -> Text
scheduledEventStatusToText SESScheduled = "scheduled"
scheduledEventStatusToText SESLocked    = "locked"
scheduledEventStatusToText SESDelivered = "delivered"
scheduledEventStatusToText SESError     = "error"
scheduledEventStatusToText SESDead      = "dead"

instance Q.ToPrepArg ScheduledEventStatus where
  toPrepVal = Q.toPrepVal . scheduledEventStatusToText

instance Q.FromCol ScheduledEventStatus where
  fromCol bs = flip Q.fromColHelper bs $ PD.enum $ \case
    "scheduled" -> Just SESScheduled
    "locked"    -> Just SESLocked
    "delivered" -> Just SESDelivered
    "error"     -> Just SESError
    "dead"      -> Just SESDead
    _           -> Nothing

instance J.ToJSON ScheduledEventStatus where
  toJSON = J.String . scheduledEventStatusToText

type ScheduledEventId = Text

data CronTriggerStats
  = CronTriggerStats
  { ctsName                :: !TriggerName
  , ctsUpcomingEventsCount :: !Int
  , ctsMaxScheduledTime    :: !UTCTime
  } deriving (Show, Eq)

data CronEventSeed
  = CronEventSeed
  { cesName          :: !TriggerName
  , cesScheduledTime :: !UTCTime
  } deriving (Show, Eq)

data CronEventPartial
  = CronEventPartial
  { cepId            :: !CronEventId
  , cepName          :: !TriggerName
  , cepScheduledTime :: !UTCTime
  , cepTries         :: !Int
  , cepCreatedAt     :: !UTCTime
  -- ^ cepCreatedAt is the time at which the cron event generator
  -- created the cron event
  } deriving (Show, Eq)

data ScheduledEventFull
  = ScheduledEventFull
  { sefId            :: !ScheduledEventId
  , sefName          :: !(Maybe TriggerName)
  -- ^ sefName is the name of the cron trigger.
  -- A one-off scheduled event is not associated with a name, so in that
  -- case, 'sefName' will be @Nothing@
  , sefScheduledTime :: !UTCTime
  , sefTries         :: !Int
  , sefWebhook       :: !Text
  , sefPayload       :: !J.Value
  , sefRetryConf     :: !STRetryConf
  , sefHeaders       :: ![EventHeaderInfo]
  , sefComment       :: !(Maybe Text)
  , sefCreatedAt     :: !UTCTime
  } deriving (Show, Eq)
$(J.deriveToJSON (J.aesonDrop 3 J.snakeCase) {J.omitNothingFields = True} ''ScheduledEventFull)

data OneOffScheduledEvent
  = OneOffScheduledEvent
  { ooseId            :: !OneOffScheduledEventId
  , ooseScheduledTime :: !UTCTime
  , ooseTries         :: !Int
  , ooseWebhook       :: !InputWebhook
  , oosePayload       :: !(Maybe J.Value)
  , ooseRetryConf     :: !STRetryConf
  , ooseHeaderConf    :: ![HeaderConf]
  , ooseComment       :: !(Maybe Text)
  , ooseCreatedAt     :: !UTCTime
  } deriving (Show, Eq)
$(J.deriveToJSON (J.aesonDrop 4 J.snakeCase) {J.omitNothingFields = True} ''OneOffScheduledEvent)

-- | The 'ScheduledEventType' data type is needed to differentiate
--   between a 'CronScheduledEvent' and 'OneOffScheduledEvent' scheduled
--   event because they both have different configurations
--   and they live in different tables.
data ScheduledEventType =
    Cron
  -- ^ A Cron scheduled event has a template defined which will
  -- contain the webhook, header configuration, retry
  -- configuration and a payload. Every cron event created
  -- uses the above mentioned configurations defined in the template.
  -- The configuration defined with the cron trigger is cached
  -- and hence it's not fetched along the cron scheduled events.
  | OneOff
  -- ^ A One-off scheduled event doesn't have any template defined
  -- so all the configuration is fetched along the scheduled events.
    deriving (Eq, Show)

data ScheduledEventWebhookPayload
  = ScheduledEventWebhookPayload
  { sewpId            :: !Text
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
  toPrepVal (ScheduledEventIdArray l) = Q.toPrepValHelper PTI.unknown encoder l
    where
      -- 25 is the OID value of TEXT, https://jdbc.postgresql.org/development/privateapi/constant-values.html
      encoder = PE.array 25 . PE.dimensionArray foldl' (PE.encodingArray . PE.text_strict)
