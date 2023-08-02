{-# LANGUAGE TemplateHaskell #-}

-- | These are types for Scheduled Trigger definition; see "Hasura.Eventing.ScheduledTrigger"
module Hasura.RQL.Types.ScheduledTrigger
  ( ScheduledTriggerName (..),
    CronTriggerMetadata (..),
    CreateCronTrigger (..),
    STRetryConf (..),
    CreateScheduledEvent (..),
    CronEventId,
    OneOffScheduledEventId,
    formatTime',
    defaultSTRetryConf,
    ScheduledEventId,
    InvocationId,
    CronEventSeed (..),
    OneOffEvent,
    ScheduledEventStatus (..),
    scheduledEventStatusToText,
    ScheduledEventType (..),
    ScheduledEvent (..),
    ScheduledEventInvocation (..),
    OneOffScheduledEvent (..),
    CronEvent (..),
    RowsCountOption (..),
    ScheduledEventPagination (..),
    GetScheduledEvents (..),
    WithOptionalTotalCount (..),
    DeleteScheduledEvent (..),
    GetScheduledEventInvocationsBy (..),
    GetScheduledEventInvocations (..),
    ClearCronEvents (..),
    cctName,
    cctWebhook,
    cctCronSchedule,
    cctPayload,
    cctRetryConf,
    cctHeaders,
    cctIncludeInMetadata,
    cctComment,
    cctReplace,
    cctRequestTransform,
    cctResponseTransform,
  )
where

import Autodocodec (HasCodec (codec), bimapCodec, optionalField', optionalFieldWithDefaultWith', optionalFieldWithOmittedDefault', requiredField')
import Autodocodec qualified as AC
import Autodocodec.Extended (refinedCodec)
import Control.Lens (makeLenses)
import Data.Aeson
import Data.Aeson qualified as J
import Data.Aeson.Casing
import Data.Aeson.Types
import Data.Text qualified as T
import Data.Time.Clock
import Data.Time.Clock.Units
import Data.Time.Format.ISO8601
import Database.PG.Query qualified as PG
import Hasura.Prelude
import Hasura.RQL.Types.Common (InputWebhook (..))
import Hasura.RQL.Types.EventTrigger
import Hasura.RQL.Types.Eventing
import Hasura.RQL.Types.Webhook.Transform (MetadataResponseTransform, RequestTransform)
import PostgreSQL.Binary.Decoding qualified as PD
import Refined (NonNegative, Refined, refineTH)
import System.Cron.Types

type CronEventId = EventId

type OneOffScheduledEventId = EventId

type ScheduledEventId = EventId

type InvocationId = Text

data STRetryConf = STRetryConf
  { strcNumRetries :: Int,
    strcRetryIntervalSeconds :: Refined NonNegative DiffTime,
    strcTimeoutSeconds :: Refined NonNegative DiffTime,
    -- | The tolerance configuration is used to determine whether a scheduled
    --   event is not too old to process. The age of the scheduled event is the
    --   difference between the current timestamp and the scheduled event's
    --   timestamp, if the age is than the tolerance then the scheduled event
    --   is marked as dead.
    strcToleranceSeconds :: Refined NonNegative DiffTime
  }
  deriving (Show, Eq, Generic)

defaultSTRetryConf :: STRetryConf
defaultSTRetryConf =
  STRetryConf
    { strcNumRetries = 0,
      strcRetryIntervalSeconds = $$(refineTH (seconds 10)),
      strcTimeoutSeconds = $$(refineTH (seconds 60)),
      strcToleranceSeconds = $$(refineTH (hours 6))
    }

instance NFData STRetryConf

instance HasCodec STRetryConf where
  codec =
    AC.object "STRetryConf"
      $ STRetryConf
      <$> optionalFieldWithDefaultWith' "num_retries" nonNegativeCodec (strcNumRetries defaultSTRetryConf)
      AC..= strcNumRetries
        <*> optionalFieldWithDefaultWith' "retry_interval_seconds" refinedCodec (strcRetryIntervalSeconds defaultSTRetryConf)
      AC..= strcRetryIntervalSeconds
        <*> optionalFieldWithDefaultWith' "timeout_seconds" refinedCodec (strcTimeoutSeconds defaultSTRetryConf)
      AC..= strcTimeoutSeconds
        <*> optionalFieldWithDefaultWith' "tolerance_seconds" refinedCodec (strcToleranceSeconds defaultSTRetryConf)
      AC..= strcToleranceSeconds
    where
      nonNegativeCodec = bimapCodec validateNonNegative id codec
      validateNonNegative n =
        if n < 0
          then Left "cannot be a negative value"
          else Right n

instance FromJSON STRetryConf where
  parseJSON = withObject "STRetryConf" \o -> do
    numRetries' <- o .:? "num_retries" .!= 0
    retryInterval <-
      o .:? "retry_interval_seconds" .!= $$(refineTH @NonNegative @DiffTime (seconds 10))
    timeout <-
      o .:? "timeout_seconds" .!= $$(refineTH @NonNegative @DiffTime (seconds 60))
    tolerance <-
      o .:? "tolerance_seconds" .!= $$(refineTH @NonNegative @DiffTime (hours 6))
    if numRetries' < 0
      then fail "num_retries cannot be a negative value"
      else pure $ STRetryConf numRetries' retryInterval timeout tolerance

instance ToJSON STRetryConf where
  toJSON = genericToJSON hasuraJSON {omitNothingFields = True}
  toEncoding = genericToEncoding hasuraJSON {omitNothingFields = True}

data CronTriggerMetadata = CronTriggerMetadata
  { ctName :: TriggerName,
    ctWebhook :: InputWebhook,
    ctSchedule :: CronSchedule,
    ctPayload :: Maybe J.Value,
    ctRetryConf :: STRetryConf,
    ctHeaders :: [HeaderConf],
    ctIncludeInMetadata :: Bool,
    ctComment :: Maybe Text,
    ctRequestTransform :: Maybe RequestTransform,
    ctResponseTransform :: Maybe MetadataResponseTransform
  }
  deriving (Show, Eq, Generic)

instance NFData CronTriggerMetadata

instance HasCodec CronTriggerMetadata where
  codec =
    AC.object "CronTriggerMetadata"
      $ CronTriggerMetadata
      <$> requiredField' "name"
      AC..= ctName
        <*> requiredField' "webhook"
      AC..= ctWebhook
        <*> requiredField' "schedule"
      AC..= ctSchedule
        <*> optionalField' "payload"
      AC..= ctPayload
        <*> optionalFieldWithOmittedDefault' "retry_conf" defaultSTRetryConf
      AC..= ctRetryConf
        <*> optionalFieldWithOmittedDefault' "headers" []
      AC..= ctHeaders
        <*> requiredField' "include_in_metadata"
      AC..= ctIncludeInMetadata
        <*> optionalField' "comment"
      AC..= ctComment
        <*> optionalField' "request_transform"
      AC..= ctRequestTransform
        <*> optionalField' "response_transform"
      AC..= ctResponseTransform

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
      ctRequestTransform <- o .:? "request_transform"
      ctResponseTransform <- o .:? "response_transform"
      pure CronTriggerMetadata {..}

instance ToJSON CronTriggerMetadata where
  toJSON = genericToJSON hasuraJSON {omitNothingFields = True}
  toEncoding = genericToEncoding hasuraJSON {omitNothingFields = True}

data CreateCronTrigger = CreateCronTrigger
  { _cctName :: TriggerName,
    _cctWebhook :: InputWebhook,
    _cctCronSchedule :: CronSchedule,
    _cctPayload :: Maybe J.Value,
    _cctRetryConf :: STRetryConf,
    _cctHeaders :: [HeaderConf],
    _cctIncludeInMetadata :: Bool,
    _cctComment :: Maybe Text,
    _cctReplace :: Bool,
    _cctRequestTransform :: Maybe RequestTransform,
    _cctResponseTransform :: Maybe MetadataResponseTransform
  }
  deriving (Show, Eq, Generic)

$(makeLenses ''CreateCronTrigger)

instance NFData CreateCronTrigger

instance FromJSON CreateCronTrigger where
  parseJSON =
    withObject "CreateCronTrigger" $ \o -> do
      _cctName <- o .: "name"
      _cctWebhook <- o .: "webhook"
      _cctPayload <- o .:? "payload"
      _cctCronSchedule <- o .: "schedule"
      _cctRetryConf <- o .:? "retry_conf" .!= defaultSTRetryConf
      _cctHeaders <- o .:? "headers" .!= []
      _cctIncludeInMetadata <- o .: "include_in_metadata"
      _cctComment <- o .:? "comment"
      _cctReplace <- o .:? "replace" .!= False
      _cctRequestTransform <- o .:? "request_transform"
      _cctResponseTransform <- o .:? "response_transform"
      pure CreateCronTrigger {..}

instance ToJSON CreateCronTrigger where
  toJSON = genericToJSON hasuraJSON {omitNothingFields = True}
  toEncoding = genericToEncoding hasuraJSON {omitNothingFields = True}

newtype ScheduledTriggerName = ScheduledTriggerName {unName :: TriggerName}
  deriving stock (Show, Eq, Generic)

instance FromJSON ScheduledTriggerName where
  parseJSON = genericParseJSON hasuraJSON {omitNothingFields = True}

instance ToJSON ScheduledTriggerName where
  toJSON = genericToJSON hasuraJSON {omitNothingFields = True}
  toEncoding = genericToEncoding hasuraJSON {omitNothingFields = True}

formatTime' :: UTCTime -> Text
formatTime' = T.pack . iso8601Show

data CreateScheduledEvent = CreateScheduledEvent
  { cseWebhook :: InputWebhook,
    -- | The timestamp should be in the
    -- <ISO 8601 https://en.wikipedia.org/wiki/ISO_8601>
    -- format (which is what @aeson@ expects by default for 'UTCTime').
    cseScheduleAt :: UTCTime,
    csePayload :: Maybe J.Value,
    cseHeaders :: [HeaderConf],
    cseRetryConf :: STRetryConf,
    cseComment :: Maybe Text,
    cseRequestTransform :: Maybe RequestTransform,
    cseResponseTransform :: Maybe MetadataResponseTransform
  }
  deriving (Show, Eq, Generic)

instance FromJSON CreateScheduledEvent where
  parseJSON =
    withObject "CreateScheduledEvent" $ \o ->
      CreateScheduledEvent
        <$> o
        .: "webhook"
        <*> o
        .: "schedule_at"
        <*> o
        .:? "payload"
        <*> o
        .:? "headers"
        .!= []
        <*> o
        .:? "retry_conf"
        .!= defaultSTRetryConf
        <*> o
        .:? "comment"
        <*> o
        .:? "request_transform"
        <*> o
        .:? "response_transform"

instance ToJSON CreateScheduledEvent where
  toJSON = genericToJSON hasuraJSON
  toEncoding = genericToEncoding hasuraJSON

-- | The 'ScheduledEventType' data type is needed to differentiate
--   between a 'CronScheduledEvent' and 'OneOffScheduledEvent' scheduled
--   event because they both have different configurations
--   and they live in different tables.
data ScheduledEventType
  = -- | A Cron scheduled event has a template defined which will
    -- contain the webhook, header configuration, retry
    -- configuration and a payload. Every cron event created
    -- uses the above mentioned configurations defined in the template.
    -- The configuration defined with the cron trigger is cached
    -- and hence it's not fetched along the cron scheduled events.
    Cron
  | -- | A One-off scheduled event doesn't have any template defined
    -- so all the configuration is fetched along the scheduled events.
    OneOff
  deriving stock (Eq, Show, Generic)

instance FromJSON ScheduledEventType where
  parseJSON = genericParseJSON hasuraJSON {constructorTagModifier = snakeCase}

instance ToJSON ScheduledEventType where
  toJSON = genericToJSON hasuraJSON {constructorTagModifier = snakeCase}
  toEncoding = genericToEncoding hasuraJSON {constructorTagModifier = snakeCase}

data ScheduledEventInvocation = ScheduledEventInvocation
  { _seiId :: InvocationId,
    _seiEventId :: EventId,
    _seiStatus :: Maybe Int,
    _seiRequest :: Maybe Value,
    _seiResponse :: Maybe Value,
    _seiCreatedAt :: UTCTime
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ScheduledEventInvocation where
  parseJSON = genericParseJSON hasuraJSON

instance ToJSON ScheduledEventInvocation where
  toJSON = genericToJSON hasuraJSON
  toEncoding = genericToEncoding hasuraJSON

data ScheduledEvent
  = SEOneOff
  | SECron TriggerName
  deriving (Show, Eq)

parseScheduledEvent :: Object -> Parser ScheduledEvent
parseScheduledEvent o = do
  ty <- o .: "type"
  case ty of
    Cron -> SECron <$> o .: "trigger_name"
    OneOff -> pure SEOneOff

scheduledEventToPairs :: ScheduledEvent -> [Pair]
scheduledEventToPairs = \case
  SEOneOff -> ["type" .= OneOff]
  SECron name -> ["type" .= Cron, "trigger_name" .= name]

data CronEventSeed = CronEventSeed
  { cesName :: TriggerName,
    cesScheduledTime :: UTCTime
  }
  deriving (Show, Eq)

type OneOffEvent = CreateScheduledEvent

data ScheduledEventStatus
  = SESScheduled
  | SESLocked
  | SESDelivered
  | SESError
  | SESDead
  deriving (Show, Eq)

scheduledEventStatusToText :: ScheduledEventStatus -> Text
scheduledEventStatusToText SESScheduled = "scheduled"
scheduledEventStatusToText SESLocked = "locked"
scheduledEventStatusToText SESDelivered = "delivered"
scheduledEventStatusToText SESError = "error"
scheduledEventStatusToText SESDead = "dead"

textToScheduledEventStatus :: Text -> Maybe ScheduledEventStatus
textToScheduledEventStatus = \case
  "scheduled" -> Just SESScheduled
  "locked" -> Just SESLocked
  "delivered" -> Just SESDelivered
  "error" -> Just SESError
  "dead" -> Just SESDead
  _ -> Nothing

instance PG.ToPrepArg ScheduledEventStatus where
  toPrepVal = PG.toPrepVal . scheduledEventStatusToText

instance PG.FromCol ScheduledEventStatus where
  fromCol bs =
    flip PG.fromColHelper bs $ PD.enum textToScheduledEventStatus

instance ToJSON ScheduledEventStatus where
  toJSON = String . scheduledEventStatusToText

instance FromJSON ScheduledEventStatus where
  parseJSON = withText "String" $ \s ->
    onNothing (textToScheduledEventStatus s)
      $ fail
      $ T.unpack
      $ "unexpected status: "
      <> s

data OneOffScheduledEvent = OneOffScheduledEvent
  { _ooseId :: OneOffScheduledEventId,
    _ooseWebhookConf :: InputWebhook,
    _ooseScheduledTime :: UTCTime,
    _ooseRetryConf :: STRetryConf,
    _oosePayload :: Maybe Value,
    _ooseHeaderConf :: [HeaderConf],
    _ooseStatus :: Text,
    _ooseTries :: Int,
    _ooseCreatedAt :: UTCTime,
    _ooseNextRetryAt :: Maybe UTCTime,
    _ooseComment :: Maybe Text,
    _ooseRequestTransform :: Maybe RequestTransform,
    _ooseResponseTransform :: Maybe MetadataResponseTransform
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON OneOffScheduledEvent where
  parseJSON = genericParseJSON hasuraJSON

instance ToJSON OneOffScheduledEvent where
  toJSON = genericToJSON hasuraJSON
  toEncoding = genericToEncoding hasuraJSON

data CronEvent = CronEvent
  { _ceId :: CronEventId,
    _ceTriggerName :: TriggerName,
    -- | We expect this to always be at second zero, since cron events have
    -- minute resolution. Note that a OneOffScheduledEvent has full timestamp
    -- precision.
    _ceScheduledTime :: UTCTime,
    _ceStatus :: Text,
    _ceTries :: Int,
    -- | it is the time at which the cron event generator created the event
    _ceCreatedAt :: UTCTime,
    _ceNextRetryAt :: Maybe UTCTime
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON CronEvent where
  parseJSON = genericParseJSON hasuraJSON

instance ToJSON CronEvent where
  toJSON = genericToJSON hasuraJSON
  toEncoding = genericToEncoding hasuraJSON

data ScheduledEventPagination = ScheduledEventPagination
  { _sepLimit :: Maybe Int,
    _sepOffset :: Maybe Int
  }
  deriving (Show, Eq)

parseScheduledEventPagination :: Object -> Parser ScheduledEventPagination
parseScheduledEventPagination o =
  ScheduledEventPagination
    <$> o
    .:? "limit"
    <*> o
    .:? "offset"

scheduledEventPaginationToPairs :: ScheduledEventPagination -> [Pair]
scheduledEventPaginationToPairs ScheduledEventPagination {..} =
  ["limit" .= _sepLimit, "offset" .= _sepOffset]

data RowsCountOption = IncludeRowsCount | DontIncludeRowsCount
  deriving (Show, Eq)

instance FromJSON RowsCountOption where
  parseJSON = withBool "RowsCountOption" $ pure . bool DontIncludeRowsCount IncludeRowsCount

instance ToJSON RowsCountOption where
  toJSON = Bool . (== IncludeRowsCount)

-- | Query type to fetch all one-off/cron scheduled events
data GetScheduledEvents = GetScheduledEvents
  { _gseScheduledEvent :: ScheduledEvent,
    _gsePagination :: ScheduledEventPagination,
    _gseStatus :: [ScheduledEventStatus],
    _gseGetRowsCount :: RowsCountOption
  }
  deriving (Show, Eq)

instance ToJSON GetScheduledEvents where
  toJSON GetScheduledEvents {..} =
    object
      $ scheduledEventToPairs _gseScheduledEvent
      <> scheduledEventPaginationToPairs _gsePagination
      <> [ "status" .= _gseStatus,
           "get_rows_count" .= _gseGetRowsCount
         ]

instance FromJSON GetScheduledEvents where
  parseJSON = withObject "GetScheduledEvents" $ \o ->
    GetScheduledEvents
      <$> parseScheduledEvent o
      <*> parseScheduledEventPagination o
      <*> o
      .:? "status"
      .!= []
      <*> o
      .:? "get_rows_count"
      .!= DontIncludeRowsCount

data WithOptionalTotalCount a = WithOptionalTotalCount
  { _wtcCount :: Maybe Int,
    _wtcData :: a
  }
  deriving (Show, Eq)

-- | Query type to delete cron/one-off events.
data DeleteScheduledEvent = DeleteScheduledEvent
  { _dseType :: ScheduledEventType,
    _dseEventId :: ScheduledEventId
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON DeleteScheduledEvent where
  parseJSON = genericParseJSON hasuraJSON

instance ToJSON DeleteScheduledEvent where
  toJSON = genericToJSON hasuraJSON
  toEncoding = genericToEncoding hasuraJSON

data GetScheduledEventInvocationsBy
  = GIBEventId EventId ScheduledEventType
  | GIBEvent ScheduledEvent
  deriving (Show, Eq)

data GetScheduledEventInvocations = GetScheduledEventInvocations
  { _geiInvocationsBy :: GetScheduledEventInvocationsBy,
    _geiPagination :: ScheduledEventPagination,
    -- | Option to include the total rows corresponding in
    --   response.
    _geiGetRowsCount :: RowsCountOption
  }
  deriving (Eq, Show)

instance FromJSON GetScheduledEventInvocations where
  parseJSON = withObject "GetScheduledEventInvocations" $ \o ->
    GetScheduledEventInvocations
      <$> (parseEventId o <|> (GIBEvent <$> parseScheduledEvent o))
      <*> parseScheduledEventPagination o
      <*> o
      .:? "get_rows_count"
      .!= DontIncludeRowsCount
    where
      parseEventId o =
        GIBEventId <$> o .: "event_id" <*> o .: "type"

instance ToJSON GetScheduledEventInvocations where
  toJSON GetScheduledEventInvocations {..} =
    object
      $ case _geiInvocationsBy of
        GIBEventId eventId eventType -> ["event_id" .= eventId, "type" .= eventType]
        GIBEvent event ->
          scheduledEventToPairs event
            <> scheduledEventPaginationToPairs _geiPagination
            <> bool mempty ["get_rows_count" .= True] (_geiGetRowsCount == IncludeRowsCount)

data ClearCronEvents
  = -- | Used to delete the cron events only of the specified cron trigger
    SingleCronTrigger TriggerName
  | -- | Used to delete all the cron events of the cron triggers with `include_in_metadata: true`
    -- It is used in the case of the `replace_metadata` API
    MetadataCronTriggers [TriggerName]
  deriving (Show, Eq)
