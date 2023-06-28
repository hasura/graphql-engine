{-# LANGUAGE UndecidableInstances #-}

module Hasura.RQL.Types.EventTrigger
  ( SubscribeOpSpec (..),
    SubscribeColumns (..),
    TriggerName (..),
    triggerNameToTxt,
    Ops (..),
    TriggerOpsDef (..),
    EventTriggerConf (..),
    RetryConf (..),
    EventHeaderInfo (..),
    WebhookConf (..),
    WebhookConfInfo (..),
    HeaderConf (..),
    defaultRetryConf,
    defaultTimeoutSeconds,
    RecreateEventTriggers (..),
    EventWithSource (..),
    TriggerMetadata (..),
    Event (..),
    TriggerTypes (..),
    Invocation (..),
    ProcessEventError (..),
    EventTriggerInfoMap,
    EventTriggerInfo (..),
    FetchBatchSize (..),
    AutoTriggerLogCleanupConfig (..),
    TriggerLogCleanupConfig (..),
    EventTriggerCleanupStatus (..),
    DeletedEventLogStats (..),
    EventTriggerQualifier (..),
    TriggerLogCleanupSources (..),
    TriggerLogCleanupToggleConfig (..),
    updateCleanupConfig,
    isIllegalTriggerName,
    EventLogStatus (..),
    GetEventLogs (..),
    EventLog (..),
    GetEventInvocations (..),
    EventInvocationLog (..),
    GetEventById (..),
    EventLogWithInvocations (..),
  )
where

import Autodocodec (HasCodec, codec, dimapCodec, disjointEitherCodec, listCodec, literalTextCodec, optionalField', optionalFieldWithDefault', optionalFieldWithOmittedDefault', requiredField')
import Autodocodec qualified as AC
import Autodocodec.Extended (boolConstCodec)
import Data.Aeson
import Data.Aeson.Extended ((.=?))
import Data.ByteString.Lazy qualified as LBS
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Text.Extended
import Data.Text.NonEmpty
import Data.Time.Clock qualified as Time
import Data.Time.LocalTime (LocalTime)
import Database.PG.Query qualified as PG
import Hasura.Prelude
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendTag (backendPrefix)
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Common (EnvRecord, InputWebhook, ResolvedWebhook, SourceName (..), TriggerOnReplication (..))
import Hasura.RQL.Types.Eventing
import Hasura.RQL.Types.Headers (HeaderConf (..))
import Hasura.RQL.Types.Webhook.Transform (MetadataResponseTransform, RequestTransform)
import System.Cron (CronSchedule)
import Text.Regex.TDFA qualified as TDFA

-- | Unique name for event trigger.
newtype TriggerName = TriggerName {unTriggerName :: NonEmptyText}
  deriving
    ( Show,
      Eq,
      Ord,
      Hashable,
      ToTxt,
      FromJSON,
      ToJSON,
      ToJSONKey,
      PG.ToPrepArg,
      Generic,
      NFData,
      PG.FromCol
    )

instance HasCodec TriggerName where
  codec = dimapCodec TriggerName unTriggerName codec

triggerNameToTxt :: TriggerName -> Text
triggerNameToTxt = unNonEmptyText . unTriggerName

isIllegalTriggerName :: TriggerName -> Bool
isIllegalTriggerName (TriggerName name) =
  -- TODO (paritosh): Should we allow `.`? (See issue 9429)
  let regex = "^[A-Za-z]+[A-Za-z0-9_\\-]*$" :: LBS.ByteString
      compiledRegex = TDFA.makeRegex regex :: TDFA.Regex
   in not $ TDFA.match compiledRegex . T.unpack $ unNonEmptyText name

data Ops = INSERT | UPDATE | DELETE | MANUAL deriving (Show, Eq, Generic)

instance Hashable Ops

data SubscribeColumns (b :: BackendType) = SubCStar | SubCArray [Column b]
  deriving (Generic)

deriving instance (Backend b) => Show (SubscribeColumns b)

deriving instance (Backend b) => Eq (SubscribeColumns b)

instance (Backend b) => NFData (SubscribeColumns b)

instance (Backend b) => HasCodec (SubscribeColumns b) where
  codec =
    dimapCodec
      (either (const SubCStar) SubCArray)
      (\case SubCStar -> Left "*"; SubCArray cols -> Right cols)
      $ disjointEitherCodec (literalTextCodec "*") (listCodec codec)

instance (Backend b) => FromJSON (SubscribeColumns b) where
  parseJSON (String s) = case s of
    "*" -> return SubCStar
    _ -> fail "only * or [] allowed"
  parseJSON v@(Array _) = SubCArray <$> parseJSON v
  parseJSON _ = fail "unexpected columns"

instance (Backend b) => ToJSON (SubscribeColumns b) where
  toJSON SubCStar = "*"
  toJSON (SubCArray cols) = toJSON cols

data SubscribeOpSpec (b :: BackendType) = SubscribeOpSpec
  { -- | Columns of the table that user can subscribe to listen for changes.
    sosColumns :: SubscribeColumns b,
    -- | Columns that the event trigger payload should consists. If set, only those columns will be
    -- visible in the payload. By default, the payload consists of all the columns of the table.
    sosPayload :: Maybe (SubscribeColumns b)
  }
  deriving (Show, Eq, Generic)

instance (Backend b) => NFData (SubscribeOpSpec b)

instance (Backend b) => HasCodec (SubscribeOpSpec b) where
  codec =
    AC.object (backendPrefix @b <> "SubscribeOpSpec")
      $ SubscribeOpSpec
      <$> requiredField' "columns"
      AC..= sosColumns
        <*> optionalField' "payload"
      AC..= sosPayload

instance (Backend b) => FromJSON (SubscribeOpSpec b) where
  parseJSON = genericParseJSON hasuraJSON {omitNothingFields = True}

instance (Backend b) => ToJSON (SubscribeOpSpec b) where
  toJSON = genericToJSON hasuraJSON {omitNothingFields = True}

defaultNumRetries :: Int
defaultNumRetries = 0

defaultRetryInterval :: Int
defaultRetryInterval = 10

defaultTimeoutSeconds :: Int
defaultTimeoutSeconds = 60

defaultRetryConf :: RetryConf
defaultRetryConf = RetryConf defaultNumRetries defaultRetryInterval (Just defaultTimeoutSeconds)

data RetryConf = RetryConf
  { rcNumRetries :: Int,
    rcIntervalSec :: Int,
    rcTimeoutSec :: Maybe Int
  }
  deriving (Show, Eq, Generic)

instance NFData RetryConf

instance HasCodec RetryConf where
  codec =
    AC.object "RetryConf"
      $ RetryConf
      <$> requiredField' "num_retries"
      AC..= rcNumRetries
        <*> requiredField' "interval_sec"
      AC..= rcIntervalSec
        <*> optionalField' "timeout_sec"
      AC..= rcTimeoutSec

instance FromJSON RetryConf where
  parseJSON = genericParseJSON hasuraJSON {omitNothingFields = True}

instance ToJSON RetryConf where
  toJSON = genericToJSON hasuraJSON {omitNothingFields = True}
  toEncoding = genericToEncoding hasuraJSON {omitNothingFields = True}

data EventHeaderInfo = EventHeaderInfo
  { ehiHeaderConf :: HeaderConf,
    ehiCachedValue :: Text
  }
  deriving (Show, Eq, Generic)

instance NFData EventHeaderInfo

instance ToJSON EventHeaderInfo where
  toJSON = genericToJSON hasuraJSON {omitNothingFields = True}
  toEncoding = genericToEncoding hasuraJSON {omitNothingFields = True}

data WebhookConf = WCValue InputWebhook | WCEnv Text
  deriving (Show, Eq, Generic)

instance NFData WebhookConf

instance ToJSON WebhookConf where
  toJSON (WCValue w) = toJSON w
  toJSON (WCEnv wEnv) = object ["from_env" .= wEnv]

instance FromJSON WebhookConf where
  parseJSON (Object o) = WCEnv <$> o .: "from_env"
  parseJSON t@(String _) =
    case fromJSON t of
      Error s -> fail s
      Success a -> pure $ WCValue a
  parseJSON _ = fail "one of string or object must be provided for webhook"

data WebhookConfInfo = WebhookConfInfo
  { wciWebhookConf :: WebhookConf,
    wciCachedValue :: EnvRecord ResolvedWebhook
  }
  deriving (Show, Eq, Generic)

instance NFData WebhookConfInfo

instance ToJSON WebhookConfInfo where
  toJSON = genericToJSON hasuraJSON {omitNothingFields = True}
  toEncoding = genericToEncoding hasuraJSON {omitNothingFields = True}

-- | The table operations on which the event trigger will be invoked.
data TriggerOpsDef (b :: BackendType) = TriggerOpsDef
  { tdInsert :: Maybe (SubscribeOpSpec b),
    tdUpdate :: Maybe (SubscribeOpSpec b),
    tdDelete :: Maybe (SubscribeOpSpec b),
    tdEnableManual :: Maybe Bool
  }
  deriving (Show, Eq, Generic)

instance (Backend b) => NFData (TriggerOpsDef b)

instance (Backend b) => HasCodec (TriggerOpsDef b) where
  codec =
    AC.object (backendPrefix @b <> "TriggerOpsDef")
      $ TriggerOpsDef
      <$> optionalField' "insert"
      AC..= tdInsert
        <*> optionalField' "update"
      AC..= tdUpdate
        <*> optionalField' "delete"
      AC..= tdDelete
        <*> optionalField' "enable_manual"
      AC..= tdEnableManual

instance (Backend b) => FromJSON (TriggerOpsDef b) where
  parseJSON = genericParseJSON hasuraJSON {omitNothingFields = True}

instance (Backend b) => ToJSON (TriggerOpsDef b) where
  toJSON = genericToJSON hasuraJSON {omitNothingFields = True}

data EventTriggerCleanupStatus = ETCSPaused | ETCSUnpaused deriving (Show, Eq, Generic)

instance NFData EventTriggerCleanupStatus

instance HasCodec EventTriggerCleanupStatus where
  codec = boolConstCodec ETCSPaused ETCSUnpaused

instance ToJSON EventTriggerCleanupStatus where
  toJSON = Bool . (ETCSPaused ==)

instance FromJSON EventTriggerCleanupStatus where
  parseJSON =
    withBool "EventTriggerCleanupStatus" $ \isPaused -> do
      pure $ if isPaused then ETCSPaused else ETCSUnpaused

-- | Automatic event trigger log cleanup configuration
data AutoTriggerLogCleanupConfig = AutoTriggerLogCleanupConfig
  { -- | cron schedule for the automatic cleanup
    _atlccSchedule :: CronSchedule,
    -- | maximum number of events to be deleted in a single cleanup action
    _atlccBatchSize :: Int,
    -- | retention period (in hours) for the event trigger logs
    _atlccClearOlderThan :: Int,
    -- | SQL query timeout (in seconds)
    _atlccTimeout :: Int,
    -- | should we clean the invocation logs as well
    _atlccCleanInvocationLogs :: Bool,
    -- | is the cleanup action paused
    _atlccPaused :: EventTriggerCleanupStatus
  }
  deriving (Show, Eq, Generic)

instance NFData AutoTriggerLogCleanupConfig

instance HasCodec AutoTriggerLogCleanupConfig where
  codec =
    AC.object "AutoTriggerLogCleanupConfig"
      $ AutoTriggerLogCleanupConfig
      <$> requiredField' "schedule"
      AC..= _atlccSchedule
        <*> optionalFieldWithDefault' "batch_size" 10000
      AC..= _atlccBatchSize
        <*> requiredField' "clear_older_than"
      AC..= _atlccClearOlderThan
        <*> optionalFieldWithDefault' "timeout" 60
      AC..= _atlccTimeout
        <*> optionalFieldWithDefault' "clean_invocation_logs" False
      AC..= _atlccCleanInvocationLogs
        <*> optionalFieldWithDefault' "paused" ETCSUnpaused
      AC..= _atlccPaused

instance FromJSON AutoTriggerLogCleanupConfig where
  parseJSON =
    withObject "AutoTriggerLogCleanupConfig" $ \o -> do
      _atlccSchedule <- o .: "schedule"
      _atlccBatchSize <- o .:? "batch_size" .!= 10000
      _atlccClearOlderThan <- o .: "clear_older_than"
      _atlccTimeout <- o .:? "timeout" .!= 60
      _atlccCleanInvocationLogs <- o .:? "clean_invocation_logs" .!= False
      _atlccPaused <- o .:? "paused" .!= ETCSUnpaused
      pure AutoTriggerLogCleanupConfig {..}

instance ToJSON AutoTriggerLogCleanupConfig where
  toJSON = genericToJSON hasuraJSON

-- | Manual event trigger log cleanup configuration
data TriggerLogCleanupConfig = TriggerLogCleanupConfig
  { -- | name of the event trigger
    tlccEventTriggerName :: TriggerName,
    -- | source of the event trigger
    tlccSourceName :: SourceName,
    -- | batch size of for the cleanup action
    tlccBatchSize :: Int,
    -- | retention period (in hours) for the event trigger logs
    tlccClearOlderThan :: Int,
    -- | SQL query timeout (in seconds)
    tlccTimeout :: Int,
    -- | should we clean the invocation logs as well
    tlccCleanInvocationLogs :: Bool
  }
  deriving (Show, Eq, Generic)

instance NFData TriggerLogCleanupConfig

instance FromJSON TriggerLogCleanupConfig where
  parseJSON =
    withObject "TriggerLogCleanupConfig" $ \o -> do
      tlccEventTriggerName <- o .: "event_trigger_name"
      tlccSourceName <- o .:? "source" .!= SNDefault
      tlccBatchSize <- o .:? "batch_size" .!= 10000
      tlccClearOlderThan <- o .: "clear_older_than"
      tlccTimeout <- o .:? "timeout" .!= 60
      tlccCleanInvocationLogs <- o .:? "clean_invocation_logs" .!= False
      pure TriggerLogCleanupConfig {..}

instance ToJSON TriggerLogCleanupConfig where
  toJSON = genericToJSON hasuraJSON

data EventTriggerQualifier = EventTriggerQualifier
  { _etqSourceName :: SourceName,
    _etqEventTriggers :: NE.NonEmpty TriggerName
  }
  deriving (Show, Eq, Generic)

instance NFData EventTriggerQualifier

instance FromJSON EventTriggerQualifier where
  parseJSON =
    withObject "EventTriggerQualifier" $ \o -> do
      _etqEventTriggers <- o .: "event_triggers"
      _etqSourceName <- o .:? "source_name" .!= SNDefault
      pure EventTriggerQualifier {..}

instance ToJSON EventTriggerQualifier where
  toJSON = genericToJSON hasuraJSON

data TriggerLogCleanupSources = TriggerAllSource | TriggerSource (NE.NonEmpty SourceName)
  deriving (Show, Eq, Generic)

instance NFData TriggerLogCleanupSources

instance ToJSON TriggerLogCleanupSources where
  toJSON = genericToJSON hasuraJSON {omitNothingFields = True}

instance FromJSON TriggerLogCleanupSources where
  parseJSON (String "*") = pure TriggerAllSource
  parseJSON (Array arr) = do
    case NE.nonEmpty (toList arr) of
      Just lst -> TriggerSource <$> traverse parseJSON lst
      Nothing -> fail "source name list should have atleast one value"
  parseJSON _ = fail "source can be * or a list of source names"

data TriggerLogCleanupToggleConfig = TriggerLogCleanupSources TriggerLogCleanupSources | TriggerQualifier (NE.NonEmpty EventTriggerQualifier)
  deriving (Show, Eq, Generic)

instance NFData TriggerLogCleanupToggleConfig

instance ToJSON TriggerLogCleanupToggleConfig where
  toJSON = genericToJSON hasuraJSON {omitNothingFields = True}

instance FromJSON TriggerLogCleanupToggleConfig where
  parseJSON = withObject "TriggerLogCleanupToggleConfig" $ \o -> do
    eventTriggers <- o .: "event_triggers"
    case eventTriggers of
      (Object obj) -> do
        sourceInfo <- obj .: "sources"
        TriggerLogCleanupSources <$> parseJSON sourceInfo
      (Array arr) -> do
        qualifiers <- parseJSON (Array arr)
        case NE.nonEmpty qualifiers of
          Just lst -> pure $ TriggerQualifier lst
          Nothing -> fail "qualifier list should have atleast one value"
      _ -> fail "The event trigger cleanup argument should either be \"*\", list of sources or list of event trigger qualifiers"

data EventTriggerConf (b :: BackendType) = EventTriggerConf
  { etcName :: TriggerName,
    etcDefinition :: TriggerOpsDef b,
    etcWebhook :: Maybe InputWebhook,
    etcWebhookFromEnv :: Maybe Text,
    etcRetryConf :: RetryConf,
    etcHeaders :: Maybe [HeaderConf],
    etcRequestTransform :: Maybe RequestTransform,
    etcResponseTransform :: Maybe MetadataResponseTransform,
    etcCleanupConfig :: Maybe AutoTriggerLogCleanupConfig,
    etcTriggerOnReplication :: TriggerOnReplication
  }
  deriving (Show, Eq, Generic)

instance (Backend b) => HasCodec (EventTriggerConf b) where
  codec =
    AC.object (backendPrefix @b <> "EventTriggerConfEventTriggerConf")
      $ EventTriggerConf
      <$> requiredField' "name"
      AC..= etcName
        <*> requiredField' "definition"
      AC..= etcDefinition
        <*> optionalField' "webhook"
      AC..= etcWebhook
        <*> optionalField' "webhook_from_env"
      AC..= etcWebhookFromEnv
        <*> requiredField' "retry_conf"
      AC..= etcRetryConf
        <*> optionalField' "headers"
      AC..= etcHeaders
        <*> optionalField' "request_transform"
      AC..= etcRequestTransform
        <*> optionalField' "response_transform"
      AC..= etcResponseTransform
        <*> optionalField' "cleanup_config"
      AC..= etcCleanupConfig
        <*> triggerOnReplication
    where
      triggerOnReplication = case defaultTriggerOnReplication @b of
        Just (_, defTOR) -> optionalFieldWithOmittedDefault' "trigger_on_replication" defTOR AC..= etcTriggerOnReplication
        Nothing -> error "No default setting for trigger_on_replication is defined for backend type."

instance (Backend b) => FromJSON (EventTriggerConf b) where
  parseJSON = withObject "EventTriggerConf" \o -> do
    name <- o .: "name"
    definition <- o .: "definition"
    webhook <- o .:? "webhook"
    webhookFromEnv <- o .:? "webhook_from_env"
    retryConf <- o .: "retry_conf"
    headers <- o .:? "headers"
    requestTransform <- o .:? "request_transform"
    responseTransform <- o .:? "response_transform"
    cleanupConfig <- o .:? "cleanup_config"
    defTOR <- case defaultTriggerOnReplication @b of
      Just (_, dt) -> pure dt
      Nothing -> fail "No default setting for trigger_on_replication is defined for backend type."
    triggerOnReplication <- o .:? "trigger_on_replication" .!= defTOR
    return $ EventTriggerConf name definition webhook webhookFromEnv retryConf headers requestTransform responseTransform cleanupConfig triggerOnReplication

instance (Backend b) => ToJSON (EventTriggerConf b) where
  toJSON (EventTriggerConf name definition webhook webhookFromEnv retryConf headers requestTransform responseTransform cleanupConfig triggerOnReplication) =
    object
      $ [ "name" .= name,
          "definition" .= definition,
          "retry_conf" .= retryConf
        ]
      <> catMaybes
        [ "webhook" .=? webhook,
          "webhook_from_env" .=? webhookFromEnv,
          "headers" .=? headers,
          "request_transform" .=? requestTransform,
          "response_transform" .=? responseTransform,
          "cleanup_config" .=? cleanupConfig,
          "trigger_on_replication"
            .=? case defaultTriggerOnReplication @b of
              Just (_, defTOR) -> if triggerOnReplication == defTOR then Nothing else Just triggerOnReplication
              Nothing -> Just triggerOnReplication
        ]

updateCleanupConfig :: Maybe AutoTriggerLogCleanupConfig -> EventTriggerConf b -> EventTriggerConf b
updateCleanupConfig cleanupConfig etConf = etConf {etcCleanupConfig = cleanupConfig}

data RecreateEventTriggers
  = RETRecreate
  | RETDoNothing
  deriving (Show, Eq, Generic)

instance Semigroup RecreateEventTriggers where
  RETRecreate <> RETRecreate = RETRecreate
  RETRecreate <> RETDoNothing = RETRecreate
  RETDoNothing <> RETRecreate = RETRecreate
  RETDoNothing <> RETDoNothing = RETDoNothing

data TriggerMetadata = TriggerMetadata {tmName :: TriggerName}
  deriving (Show, Eq, Generic)

instance FromJSON TriggerMetadata where
  parseJSON = genericParseJSON hasuraJSON {omitNothingFields = True}

instance ToJSON TriggerMetadata where
  toJSON = genericToJSON hasuraJSON {omitNothingFields = True}
  toEncoding = genericToEncoding hasuraJSON {omitNothingFields = True}

-- | Change data for a particular row
--
-- https://docs.hasura.io/1.0/graphql/manual/event-triggers/payload.html
data Event (b :: BackendType) = Event
  { eId :: EventId,
    eSource :: SourceName,
    eTable :: TableName b,
    eTrigger :: TriggerMetadata,
    eEvent :: Value,
    eTries :: Int,
    -- Ideally 'eCreatedAt' should have been a Time.UTCTime, but while intializing the
    -- hdb_catalog.event_log tables for Postgres, we incorrectly created `created_at :: TIMESTAMP`
    -- column as a Timestamp type. This means the `created_at` column for Postgres stores
    -- the local time in which the Postgres DB is in. Hence to avoid confusion and
    -- other time related problems, we use the `LocalTime` type for `created_at`.
    --
    -- Note, this problem only exists for PG sources since for MSSQL the `created_at`
    -- stores  UTCTime. Since the `Event` type is common for all sources, we have to do
    -- a redundant conversion of UTCTime to LocalTime for MSSQL sources while fetching
    -- events for MSSQL source.
    eCreatedAt :: LocalTime,
    eRetryAt :: Maybe Time.UTCTime,
    -- | The values `eCreatedAtUTC` and `eRetryAtUTC` are only used for
    --   calculating the `event_processing_time` metric.
    eCreatedAtUTC :: Time.UTCTime,
    eRetryAtUTC :: Maybe Time.UTCTime
  }
  deriving (Generic)

deriving instance (Backend b) => Show (Event b)

deriving instance (Backend b) => Eq (Event b)

instance (Backend b) => FromJSON (Event b) where
  parseJSON = genericParseJSON hasuraJSON {omitNothingFields = True}

-- | The event payload processed by 'processEvent'
data EventWithSource (b :: BackendType) = EventWithSource
  { _ewsEvent :: Event b,
    _ewsSourceConfig :: SourceConfig b,
    _ewsSourceName :: SourceName,
    -- | The 'Time.UTCTime' represents the time when the event was fetched from DB.
    -- ^ Used to calculate Event Lock time
    _ewsFetchTime :: Time.UTCTime
  }

data ProcessEventError
  = PESetRetry Time.UTCTime
  | PESetError
  deriving (Show, Eq)

data EventTriggerInfo (b :: BackendType) = EventTriggerInfo
  { etiName :: TriggerName,
    etiOpsDef :: TriggerOpsDef b,
    etiRetryConf :: RetryConf,
    -- | The HTTP(s) URL which will be called with the event payload on configured operation.
    -- Must be a POST handler. This URL can be entered manually or can be picked up from an
    -- environment variable (the environment variable needs to be set before using it for
    -- this configuration).
    etiWebhookInfo :: WebhookConfInfo,
    -- | Custom headers can be added to an event trigger. Each webhook request will have these
    -- headers added.
    etiHeaders :: [EventHeaderInfo],
    etiRequestTransform :: Maybe RequestTransform,
    etiResponseTransform :: Maybe MetadataResponseTransform,
    etiCleanupConfig :: Maybe AutoTriggerLogCleanupConfig,
    etiTriggerOnReplication :: TriggerOnReplication
  }
  deriving (Generic, Eq)

instance (Backend b) => NFData (EventTriggerInfo b)

instance (Backend b) => ToJSON (EventTriggerInfo b) where
  toJSON = genericToJSON hasuraJSON

type EventTriggerInfoMap b = HashMap.HashMap TriggerName (EventTriggerInfo b)

newtype FetchBatchSize = FetchBatchSize {_unFetchBatchSize :: Int}
  deriving (Show, Eq)

-- | Statistics of deleted event logs and invocation logs
data DeletedEventLogStats = DeletedEventLogStats
  { deletedEventLogs :: Int,
    deletedInvocationLogs :: Int
  }
  deriving (Show, Eq)

data EventLogStatus
  = Processed
  | Pending
  | All
  deriving (Show, Eq)

instance ToJSON EventLogStatus where
  toJSON Processed = String "processed"
  toJSON Pending = String "pending"
  toJSON All = String "all"

instance FromJSON EventLogStatus where
  parseJSON (String "processed") = pure Processed
  parseJSON (String "pending") = pure Pending
  parseJSON _ = fail "event logs status can only be one of the following: processed or pending"

data GetEventLogs (b :: BackendType) = GetEventLogs
  { _gelName :: TriggerName,
    _gelSourceName :: SourceName,
    _gelLimit :: Int,
    _gelOffset :: Int,
    _gelStatus :: EventLogStatus
  }
  deriving (Show)

instance ToJSON (GetEventLogs b) where
  toJSON GetEventLogs {..} =
    object
      $ [ "name" .= _gelName,
          "source" .= _gelSourceName,
          "limit" .= _gelLimit,
          "offset" .= _gelOffset,
          "status" .= _gelStatus
        ]

instance FromJSON (GetEventLogs b) where
  parseJSON = withObject "GetEventLogs" $ \o ->
    GetEventLogs
      <$> o
      .: "name"
      <*> o
      .:? "source"
      .!= SNDefault
      <*> o
      .:? "limit"
      .!= 100
      <*> o
      .:? "offset"
      .!= 0
      <*> o
      .:? "status"
      .!= All

data EventLog = EventLog
  { elId :: EventId,
    elSchemaName :: Text,
    elTableName :: Text,
    elTriggerName :: TriggerName,
    elPayload :: Value,
    elDelivered :: Bool,
    elError :: Bool,
    elTries :: Int,
    elCreatedAt :: Time.UTCTime,
    elLocked :: Maybe Time.UTCTime,
    elNextRetryAt :: Maybe Time.UTCTime,
    elArchived :: Bool
  }
  deriving (Eq, Generic)

instance ToJSON EventLog where
  toJSON = genericToJSON hasuraJSON
  toEncoding = genericToEncoding hasuraJSON

data GetEventInvocations (b :: BackendType) = GetEventInvocations
  { _geiName :: TriggerName,
    _geiSourceName :: SourceName,
    _geiLimit :: Int,
    _geiOffset :: Int
  }
  deriving (Show)

instance ToJSON (GetEventInvocations b) where
  toJSON GetEventInvocations {..} =
    object
      $ [ "name" .= _geiName,
          "source" .= _geiSourceName,
          "limit" .= _geiLimit,
          "offset" .= _geiOffset
        ]

instance FromJSON (GetEventInvocations b) where
  parseJSON = withObject "GetEventInvocations" $ \o ->
    GetEventInvocations
      <$> o
      .: "name"
      <*> o
      .:? "source"
      .!= SNDefault
      <*> o
      .:? "limit"
      .!= 100
      <*> o
      .:? "offset"
      .!= 0

data EventInvocationLog = EventInvocationLog
  { eilId :: Text,
    eilTriggerName :: TriggerName,
    eilEventId :: EventId,
    eilHttpStatus :: Maybe Int,
    eilRequest :: Value,
    eilResponse :: Value,
    eilCreatedAt :: Time.UTCTime
  }
  deriving (Generic)

instance ToJSON EventInvocationLog where
  toJSON = genericToJSON hasuraJSON
  toEncoding = genericToEncoding hasuraJSON

data GetEventById (b :: BackendType) = GetEventById
  { _gebiSourceName :: SourceName,
    _gebiEventId :: EventId,
    _gebiInvocationLogLimit :: Int,
    _gebiInvocationLogOffset :: Int
  }
  deriving (Show)

instance ToJSON (GetEventById b) where
  toJSON GetEventById {..} =
    object
      $ [ "source" .= _gebiSourceName,
          "event_id" .= _gebiEventId,
          "invocation_log_limit" .= _gebiInvocationLogLimit,
          "invocation_log_offset" .= _gebiInvocationLogOffset
        ]

instance FromJSON (GetEventById b) where
  parseJSON = withObject "GetEventById" $ \o ->
    GetEventById
      <$> o
      .:? "source"
      .!= SNDefault
      <*> o
      .: "event_id"
      <*> o
      .:? "invocation_log_limit"
      .!= 100
      <*> o
      .:? "invocation_log_offset"
      .!= 0

data EventLogWithInvocations = EventLogWithInvocations
  { elwiEvent :: Maybe EventLog,
    elwiInvocations :: [EventInvocationLog]
  }
  deriving (Generic)

instance ToJSON EventLogWithInvocations where
  toJSON = genericToJSON hasuraJSON
  toEncoding = genericToEncoding hasuraJSON
