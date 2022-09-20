{-# LANGUAGE TemplateHaskell #-}

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
  )
where

import Data.Aeson
import Data.Aeson.TH
import Data.HashMap.Strict qualified as M
import Data.List.NonEmpty qualified as NE
import Data.Text.Extended
import Data.Text.NonEmpty
import Data.Time.Clock qualified as Time
import Database.PG.Query qualified as Q
import Hasura.Incremental (Cacheable)
import Hasura.Prelude
import Hasura.RQL.DDL.Headers
import Hasura.RQL.DDL.Webhook.Transform (MetadataResponseTransform, RequestTransform)
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Common (EnvRecord, InputWebhook, ResolvedWebhook, SourceName (..))
import Hasura.RQL.Types.Eventing
import Hasura.SQL.Backend
import System.Cron (CronSchedule)

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
      Q.ToPrepArg,
      Generic,
      NFData,
      Cacheable,
      Q.FromCol
    )

triggerNameToTxt :: TriggerName -> Text
triggerNameToTxt = unNonEmptyText . unTriggerName

data Ops = INSERT | UPDATE | DELETE | MANUAL deriving (Show, Eq, Generic)

instance Hashable Ops

data SubscribeColumns (b :: BackendType) = SubCStar | SubCArray [Column b]
  deriving (Generic)

deriving instance Backend b => Show (SubscribeColumns b)

deriving instance Backend b => Eq (SubscribeColumns b)

instance Backend b => NFData (SubscribeColumns b)

instance Backend b => Cacheable (SubscribeColumns b)

instance Backend b => FromJSON (SubscribeColumns b) where
  parseJSON (String s) = case s of
    "*" -> return SubCStar
    _ -> fail "only * or [] allowed"
  parseJSON v@(Array _) = SubCArray <$> parseJSON v
  parseJSON _ = fail "unexpected columns"

instance Backend b => ToJSON (SubscribeColumns b) where
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

instance (Backend b) => Cacheable (SubscribeOpSpec b)

instance Backend b => FromJSON (SubscribeOpSpec b) where
  parseJSON = genericParseJSON hasuraJSON {omitNothingFields = True}

instance Backend b => ToJSON (SubscribeOpSpec b) where
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

instance Cacheable RetryConf

$(deriveJSON hasuraJSON {omitNothingFields = True} ''RetryConf)

data EventHeaderInfo = EventHeaderInfo
  { ehiHeaderConf :: HeaderConf,
    ehiCachedValue :: Text
  }
  deriving (Show, Eq, Generic)

instance NFData EventHeaderInfo

$(deriveToJSON hasuraJSON {omitNothingFields = True} ''EventHeaderInfo)

data WebhookConf = WCValue InputWebhook | WCEnv Text
  deriving (Show, Eq, Generic)

instance NFData WebhookConf

instance Cacheable WebhookConf

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

instance Cacheable WebhookConfInfo

$(deriveToJSON hasuraJSON {omitNothingFields = True} ''WebhookConfInfo)

-- | The table operations on which the event trigger will be invoked.
data TriggerOpsDef (b :: BackendType) = TriggerOpsDef
  { tdInsert :: Maybe (SubscribeOpSpec b),
    tdUpdate :: Maybe (SubscribeOpSpec b),
    tdDelete :: Maybe (SubscribeOpSpec b),
    tdEnableManual :: Maybe Bool
  }
  deriving (Show, Eq, Generic)

instance Backend b => NFData (TriggerOpsDef b)

instance Backend b => Cacheable (TriggerOpsDef b)

instance Backend b => FromJSON (TriggerOpsDef b) where
  parseJSON = genericParseJSON hasuraJSON {omitNothingFields = True}

instance Backend b => ToJSON (TriggerOpsDef b) where
  toJSON = genericToJSON hasuraJSON {omitNothingFields = True}

data EventTriggerCleanupStatus = ETCSPaused | ETCSUnpaused deriving (Show, Eq, Generic)

instance NFData EventTriggerCleanupStatus

instance Cacheable EventTriggerCleanupStatus

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
    _atlccRetentionPeriod :: Int,
    -- | SQL query timeout (in seconds)
    _atlccTimeout :: Int,
    -- | should we clean the invocation logs as well
    _atlccCleanInvocationLogs :: Bool,
    -- | is the cleanup action paused
    _atlccPaused :: EventTriggerCleanupStatus
  }
  deriving (Show, Eq, Generic)

instance NFData AutoTriggerLogCleanupConfig

instance Cacheable AutoTriggerLogCleanupConfig

instance FromJSON AutoTriggerLogCleanupConfig where
  parseJSON =
    withObject "AutoTriggerLogCleanupConfig" $ \o -> do
      _atlccSchedule <- o .: "schedule"
      _atlccBatchSize <- o .:? "batch_size" .!= 10000
      _atlccRetentionPeriod <- o .:? "retention_period" .!= 168 -- 7 Days = 168 hours
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
    tlccRetentionPeriod :: Int,
    -- | SQL query timeout (in seconds)
    tlccQueryTimeout :: Int,
    -- | should we clean the invocation logs as well
    tlccCleanInvocationLogs :: Bool
  }
  deriving (Show, Eq, Generic)

instance NFData TriggerLogCleanupConfig

instance Cacheable TriggerLogCleanupConfig

instance FromJSON TriggerLogCleanupConfig where
  parseJSON =
    withObject "TriggerLogCleanupConfig" $ \o -> do
      tlccEventTriggerName <- o .: "event_trigger_name"
      tlccSourceName <- o .:? "source" .!= SNDefault
      tlccBatchSize <- o .:? "batch_size" .!= 10000
      tlccRetentionPeriod <- o .:? "retention_period" .!= 168 -- 7 Days = 168 hours
      tlccQueryTimeout <- o .:? "timeout" .!= 60
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

instance Cacheable EventTriggerQualifier

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

instance Cacheable TriggerLogCleanupSources

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

instance Cacheable TriggerLogCleanupToggleConfig

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
    etcCleanupConfig :: Maybe AutoTriggerLogCleanupConfig
  }
  deriving (Show, Eq, Generic)

instance Backend b => Cacheable (EventTriggerConf b)

instance Backend b => FromJSON (EventTriggerConf b) where
  parseJSON = genericParseJSON hasuraJSON {omitNothingFields = True}

instance Backend b => ToJSON (EventTriggerConf b) where
  toJSON = genericToJSON hasuraJSON {omitNothingFields = True}

updateCleanupConfig :: Maybe AutoTriggerLogCleanupConfig -> EventTriggerConf b -> EventTriggerConf b
updateCleanupConfig cleanupConfig etConf = etConf {etcCleanupConfig = cleanupConfig}

data RecreateEventTriggers
  = RETRecreate
  | RETDoNothing
  deriving (Show, Eq, Generic)

instance Cacheable RecreateEventTriggers

instance Semigroup RecreateEventTriggers where
  RETRecreate <> RETRecreate = RETRecreate
  RETRecreate <> RETDoNothing = RETRecreate
  RETDoNothing <> RETRecreate = RETRecreate
  RETDoNothing <> RETDoNothing = RETDoNothing

data TriggerMetadata = TriggerMetadata {tmName :: TriggerName}
  deriving (Show, Eq)

$(deriveJSON hasuraJSON {omitNothingFields = True} ''TriggerMetadata)

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
    eCreatedAt :: Time.UTCTime
  }
  deriving (Generic)

deriving instance Backend b => Show (Event b)

deriving instance Backend b => Eq (Event b)

instance Backend b => FromJSON (Event b) where
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
    etiCleanupConfig :: Maybe AutoTriggerLogCleanupConfig
  }
  deriving (Generic, Eq)

instance Backend b => NFData (EventTriggerInfo b)

instance Backend b => ToJSON (EventTriggerInfo b) where
  toJSON = genericToJSON hasuraJSON

type EventTriggerInfoMap b = M.HashMap TriggerName (EventTriggerInfo b)

newtype FetchBatchSize = FetchBatchSize {_unFetchBatchSize :: Int}
  deriving (Show, Eq)

-- | Statistics of deleted event logs and invocation logs
data DeletedEventLogStats = DeletedEventLogStats
  { deletedEventLogs :: Int,
    deletedInvocationLogs :: Int
  }
  deriving (Show, Eq)
