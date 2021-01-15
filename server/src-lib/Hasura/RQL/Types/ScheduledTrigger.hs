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
  , ScheduledEventId
  , InvocationId
  , CronEventSeed(..)
  , ScheduledEventSeed(..)
  , ScheduledEventStatus(..)
  , scheduledEventStatusToText
  , ScheduledEventType(..)
  , ScheduledEvent(..)
  , ScheduledEventInvocation(..)
  , OneOffScheduledEvent(..)
  , CronEvent(..)
  , ScheduledEventPagination(..)
  , GetScheduledEvents(..)
  , WithTotalCount(..)
  , DeleteScheduledEvent(..)
  , GetInvocationsBy(..)
  , GetEventInvocations(..)
  ) where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.Aeson.Types
import           Data.Time.Clock
import           Data.Time.Clock.Units
import           Data.Time.Format.ISO8601
import           Hasura.Incremental
import           Hasura.Prelude
import           Hasura.RQL.Types.Action       (InputWebhook (..))
import           Hasura.RQL.Types.Common       (NonNegativeDiffTime, unsafeNonNegativeDiffTime)
import           Hasura.RQL.Types.EventTrigger
import           System.Cron.Types

import qualified Data.Aeson                    as J
import qualified Data.Text                     as T
import qualified Database.PG.Query             as Q
import qualified PostgreSQL.Binary.Decoding    as PD

type CronEventId = EventId

type OneOffScheduledEventId = EventId

type ScheduledEventId = EventId

type InvocationId = Text

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
      o .:? "retry_interval_seconds" .!= unsafeNonNegativeDiffTime (seconds 10)
    timeout <-
      o .:? "timeout_seconds" .!= unsafeNonNegativeDiffTime (seconds 60)
    tolerance <-
      o .:? "tolerance_seconds" .!= unsafeNonNegativeDiffTime (hours 6)
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
  { ctName              :: !TriggerName
  , ctWebhook           :: !InputWebhook
  , ctSchedule          :: !CronSchedule
  , ctPayload           :: !(Maybe J.Value)
  , ctRetryConf         :: !STRetryConf
  , ctHeaders           :: ![HeaderConf]
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
  { cctName              :: !TriggerName
  , cctWebhook           :: !InputWebhook
  , cctCronSchedule      :: !CronSchedule
  , cctPayload           :: !(Maybe J.Value)
  , cctRetryConf         :: !STRetryConf
  , cctHeaders           :: ![HeaderConf]
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
  = ScheduledTriggerName { unName :: TriggerName }
  deriving (Show, Eq)

$(deriveJSON (aesonDrop 2 snakeCase) ''ScheduledTriggerName)

formatTime' :: UTCTime -> Text
formatTime'= T.pack . iso8601Show

data CreateScheduledEvent
  = CreateScheduledEvent
  { cseWebhook    :: !InputWebhook
  , cseScheduleAt :: !UTCTime
    -- ^ The timestamp should be in the
    -- <ISO 8601 https://en.wikipedia.org/wiki/ISO_8601>
    -- format (which is what @aeson@ expects by default for 'UTCTime').
  , csePayload    :: !(Maybe J.Value)
  , cseHeaders    :: ![HeaderConf]
  , cseRetryConf  :: !STRetryConf
  , cseComment    :: !(Maybe Text)
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
$(deriveJSON defaultOptions{constructorTagModifier = snakeCase} ''ScheduledEventType)

data ScheduledEventInvocation
  = ScheduledEventInvocation
  { _seiId        :: !InvocationId
  , _seiEventId   :: !EventId
  , _seiStatus    :: !(Maybe Int)
  , _seiRequest   :: !(Maybe Value)
  , _seiResponse  :: !(Maybe Value)
  , _seiCreatedAt :: !UTCTime
  } deriving (Show, Eq)
$(deriveJSON (aesonDrop 4 snakeCase) ''ScheduledEventInvocation)

data ScheduledEvent
  = SEOneOff
  | SECron !TriggerName
  deriving (Show, Eq)

parseScheduledEvent :: Object -> Parser ScheduledEvent
parseScheduledEvent o = do
  ty <- o .: "type"
  case ty of
    Cron   -> SECron <$> o .: "trigger_name"
    OneOff -> pure SEOneOff

scheduledEventToPairs :: ScheduledEvent -> [Pair]
scheduledEventToPairs = \case
  SEOneOff    -> ["type" .= OneOff]
  SECron name -> ["type" .= Cron, "trigger_name" .= name]

data CronEventSeed
  = CronEventSeed
  { cesName          :: !TriggerName
  , cesScheduledTime :: !UTCTime
  } deriving (Show, Eq)

data ScheduledEventSeed
  = SESCron ![CronEventSeed]
  | SESOneOff !CreateScheduledEvent
  deriving (Show, Eq)

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

textToScheduledEventStatus :: Text -> Maybe ScheduledEventStatus
textToScheduledEventStatus = \case
  "scheduled" -> Just SESScheduled
  "locked"    -> Just SESLocked
  "delivered" -> Just SESDelivered
  "error"     -> Just SESError
  "dead"      -> Just SESDead
  _           -> Nothing

instance Q.ToPrepArg ScheduledEventStatus where
  toPrepVal = Q.toPrepVal . scheduledEventStatusToText

instance Q.FromCol ScheduledEventStatus where
  fromCol bs =
    flip Q.fromColHelper bs $ PD.enum textToScheduledEventStatus

instance ToJSON ScheduledEventStatus where
  toJSON = String . scheduledEventStatusToText

instance FromJSON ScheduledEventStatus where
  parseJSON = withText "String" $ \s -> onNothing (textToScheduledEventStatus s) $
    fail $ T.unpack $ "unexpected status: " <> s

data OneOffScheduledEvent
  = OneOffScheduledEvent
  { _ooseId            :: !OneOffScheduledEventId
  , _ooseWebhookConf   :: !InputWebhook
  , _ooseScheduledTime :: !UTCTime
  , _ooseRetryConf     :: !STRetryConf
  , _oosePayload       :: !(Maybe Value)
  , _ooseHeaderConf    :: ![HeaderConf]
  , _ooseStatus        :: !Text
  , _ooseTries         :: !Int
  , _ooseCreatedAt     :: !UTCTime
  , _ooseNextRetryAt   :: !(Maybe UTCTime)
  , _ooseComment       :: !(Maybe Text)
  } deriving (Show, Eq)
$(deriveJSON (aesonDrop 5 snakeCase) ''OneOffScheduledEvent)

data CronEvent
  = CronEvent
  { _ceId            :: !CronEventId
  , _ceTriggerName   :: !TriggerName
  , _ceScheduledTime :: !UTCTime
  , _ceStatus        :: !Text
  , _ceTries         :: !Int
  , _ceCreatedAt     :: !UTCTime
  -- ^ it is the time at which the cron event generator created the event
  , _ceNextRetryAt   :: !(Maybe UTCTime)
  } deriving (Show, Eq)
$(deriveJSON (aesonDrop 3 snakeCase) ''CronEvent)

data ScheduledEventPagination
  = ScheduledEventPagination
  { _sepLimit  :: !(Maybe Int)
  , _sepOffset :: !(Maybe Int)
  } deriving (Show, Eq)

parseScheduledEventPagination :: Object -> Parser ScheduledEventPagination
parseScheduledEventPagination o =
  ScheduledEventPagination
    <$> o .:? "limit"
    <*> o .:? "offset"

scheduledEventPaginationToPairs :: ScheduledEventPagination -> [Pair]
scheduledEventPaginationToPairs ScheduledEventPagination{..} =
  ["limit" .= _sepLimit, "offset" .= _sepOffset]

-- | Query type to fetch all one-off/cron scheduled events
data GetScheduledEvents
  = GetScheduledEvents
  { _gseScheduledEvent :: !ScheduledEvent
  , _gsePagination     :: !ScheduledEventPagination
  , _gseStatus         :: ![ScheduledEventStatus]
  } deriving (Show, Eq)
instance ToJSON GetScheduledEvents where
  toJSON GetScheduledEvents{..} =
    object $ scheduledEventToPairs _gseScheduledEvent
          <> scheduledEventPaginationToPairs _gsePagination
          <> ["status" .= _gseStatus]

instance FromJSON GetScheduledEvents where
  parseJSON = withObject "Object" $ \o ->
    GetScheduledEvents
      <$> parseScheduledEvent o
      <*> parseScheduledEventPagination o
      <*> o .:? "status" .!= []

data WithTotalCount a
  = WithTotalCount
  { _wtcCount :: !Int
  , _wtcData  :: !a
  } deriving (Show, Eq)

-- | Query type to delete cron/one-off events.
data DeleteScheduledEvent
  = DeleteScheduledEvent
  { _dseType    :: !ScheduledEventType
  , _dseEventId :: !ScheduledEventId
  } deriving (Show, Eq)
$(deriveJSON (aesonDrop 4 snakeCase) ''DeleteScheduledEvent)

data GetInvocationsBy
  = GIBEventId !EventId !ScheduledEventType
  | GIBEvent !ScheduledEvent
  deriving (Show, Eq)

data GetEventInvocations
  = GetEventInvocations
  { _geiInvocationsBy :: !GetInvocationsBy
  , _geiPagination    :: !ScheduledEventPagination
  } deriving (Eq, Show)

instance FromJSON GetEventInvocations where
  parseJSON = withObject "Object" $ \o ->
    GetEventInvocations
      <$> (parseEventId o <|> (GIBEvent <$> parseScheduledEvent o))
      <*> parseScheduledEventPagination o
    where
      parseEventId o =
        GIBEventId <$> o .: "event_id" <*> o .: "type"

instance ToJSON GetEventInvocations where
  toJSON GetEventInvocations{..} =
    object $ case _geiInvocationsBy of
             GIBEventId eventId eventType -> ["event_id" .= eventId, "type" .= eventType]
             GIBEvent event               -> scheduledEventToPairs event
          <> scheduledEventPaginationToPairs _geiPagination
