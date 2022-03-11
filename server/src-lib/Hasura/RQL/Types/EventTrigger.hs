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
  )
where

import Data.Aeson
import Data.Aeson.TH
import Data.HashMap.Strict qualified as M
import Data.Text.Extended
import Data.Text.NonEmpty
import Data.Time.Clock qualified as Time
import Database.PG.Query qualified as Q
import Hasura.Incremental (Cacheable)
import Hasura.Prelude
import Hasura.RQL.DDL.Headers
import Hasura.RQL.DDL.Webhook.Transform (MetadataResponseTransform, RequestTransform)
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Common (InputWebhook, ResolvedWebhook, SourceName)
import Hasura.RQL.Types.Eventing
import Hasura.SQL.Backend

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

data Ops = INSERT | UPDATE | DELETE | MANUAL deriving (Show)

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
    sosColumns :: !(SubscribeColumns b),
    -- | Columns that the event trigger payload should consists. If set, only those columns will be
    -- visible in the payload. By default, the payload consists of all the columns of the table.
    sosPayload :: !(Maybe (SubscribeColumns b))
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
  { rcNumRetries :: !Int,
    rcIntervalSec :: !Int,
    rcTimeoutSec :: !(Maybe Int)
  }
  deriving (Show, Eq, Generic)

instance NFData RetryConf

instance Cacheable RetryConf

$(deriveJSON hasuraJSON {omitNothingFields = True} ''RetryConf)

data EventHeaderInfo = EventHeaderInfo
  { ehiHeaderConf :: !HeaderConf,
    ehiCachedValue :: !Text
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
  { wciWebhookConf :: !WebhookConf,
    wciCachedValue :: !ResolvedWebhook
  }
  deriving (Show, Eq, Generic)

instance NFData WebhookConfInfo

instance Cacheable WebhookConfInfo

$(deriveToJSON hasuraJSON {omitNothingFields = True} ''WebhookConfInfo)

-- | The table operations on which the event trigger will be invoked.
data TriggerOpsDef (b :: BackendType) = TriggerOpsDef
  { tdInsert :: !(Maybe (SubscribeOpSpec b)),
    tdUpdate :: !(Maybe (SubscribeOpSpec b)),
    tdDelete :: !(Maybe (SubscribeOpSpec b)),
    tdEnableManual :: !(Maybe Bool)
  }
  deriving (Show, Eq, Generic)

instance Backend b => NFData (TriggerOpsDef b)

instance Backend b => Cacheable (TriggerOpsDef b)

instance Backend b => FromJSON (TriggerOpsDef b) where
  parseJSON = genericParseJSON hasuraJSON {omitNothingFields = True}

instance Backend b => ToJSON (TriggerOpsDef b) where
  toJSON = genericToJSON hasuraJSON {omitNothingFields = True}

data EventTriggerConf (b :: BackendType) = EventTriggerConf
  { etcName :: !TriggerName,
    etcDefinition :: !(TriggerOpsDef b),
    etcWebhook :: !(Maybe InputWebhook),
    etcWebhookFromEnv :: !(Maybe Text),
    etcRetryConf :: !RetryConf,
    etcHeaders :: !(Maybe [HeaderConf]),
    etcRequestTransform :: !(Maybe RequestTransform),
    etcResponseTransform :: !(Maybe MetadataResponseTransform)
  }
  deriving (Show, Eq, Generic)

instance Backend b => Cacheable (EventTriggerConf b)

instance Backend b => FromJSON (EventTriggerConf b) where
  parseJSON = genericParseJSON hasuraJSON {omitNothingFields = True}

instance Backend b => ToJSON (EventTriggerConf b) where
  toJSON = genericToJSON hasuraJSON {omitNothingFields = True}

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
  { eId :: !EventId,
    eSource :: !SourceName,
    eTable :: !(TableName b),
    eTrigger :: !TriggerMetadata,
    eEvent :: !Value,
    eTries :: !Int,
    eCreatedAt :: !Time.UTCTime
  }
  deriving (Generic)

deriving instance Backend b => Show (Event b)

deriving instance Backend b => Eq (Event b)

instance Backend b => FromJSON (Event b) where
  parseJSON = genericParseJSON hasuraJSON {omitNothingFields = True}

-- | The event payload processed by 'processEvent'
data EventWithSource (b :: BackendType) = EventWithSource
  { _ewsEvent :: !(Event b),
    _ewsSourceConfig :: !(SourceConfig b),
    _ewsSourceName :: !SourceName,
    -- | The 'Time.UTCTime' represents the time when the event was fetched from DB.
    -- ^ Used to calculate Event Lock time
    _ewsFetchTime :: !Time.UTCTime
  }

data ProcessEventError
  = PESetRetry !Time.UTCTime
  | PESetError
  deriving (Show, Eq)

data EventTriggerInfo (b :: BackendType) = EventTriggerInfo
  { etiName :: !TriggerName,
    etiOpsDef :: !(TriggerOpsDef b),
    etiRetryConf :: !RetryConf,
    -- | The HTTP(s) URL which will be called with the event payload on configured operation.
    -- Must be a POST handler. This URL can be entered manually or can be picked up from an
    -- environment variable (the environment variable needs to be set before using it for
    -- this configuration).
    etiWebhookInfo :: !WebhookConfInfo,
    -- | Custom headers can be added to an event trigger. Each webhook request will have these
    -- headers added.
    etiHeaders :: ![EventHeaderInfo],
    etiRequestTransform :: !(Maybe RequestTransform),
    etiResponseTransform :: !(Maybe MetadataResponseTransform)
  }
  deriving (Generic, Eq)

instance Backend b => NFData (EventTriggerInfo b)

instance Backend b => ToJSON (EventTriggerInfo b) where
  toJSON = genericToJSON hasuraJSON

type EventTriggerInfoMap b = M.HashMap TriggerName (EventTriggerInfo b)

newtype FetchBatchSize = FetchBatchSize {_unFetchBatchSize :: Int}
  deriving (Show, Eq)
