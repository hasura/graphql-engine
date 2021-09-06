module Hasura.RQL.Types.EventTrigger
  ( SubscribeOpSpec(..)
  , SubscribeColumns(..)
  , TriggerName(..)
  , triggerNameToTxt
  , Ops(..)
  , EventId(..)
  , TriggerOpsDef(..)
  , EventTriggerConf(..)
  , RetryConf(..)
  , EventHeaderInfo(..)
  , WebhookConf(..)
  , WebhookConfInfo(..)
  , HeaderConf(..)

  , defaultRetryConf
  , defaultTimeoutSeconds

  , RecreateEventTriggers (..)
  , EventTriggerInfo (..)
  , EventTriggerInfoMap
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict      as M
import qualified Database.PG.Query        as Q

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Text.Extended
import           Data.Text.NonEmpty

import           Hasura.Incremental       (Cacheable)
import           Hasura.RQL.DDL.Headers
import           Hasura.RQL.Types.Backend
import           Hasura.RQL.Types.Common  (InputWebhook)
import           Hasura.SQL.Backend


-- | Unique name for event trigger.
newtype TriggerName = TriggerName { unTriggerName :: NonEmptyText }
  deriving (Show, Eq, Ord, Hashable, ToTxt, FromJSON, ToJSON, ToJSONKey
           , Q.ToPrepArg, Generic, NFData, Cacheable, Q.FromCol)

triggerNameToTxt :: TriggerName -> Text
triggerNameToTxt = unNonEmptyText . unTriggerName

newtype EventId = EventId {unEventId :: Text}
  deriving (Show, Eq, Ord, Hashable, ToTxt, FromJSON, ToJSON, ToJSONKey, Q.FromCol, Q.ToPrepArg, Generic, NFData, Cacheable)

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
                          _   -> fail "only * or [] allowed"
  parseJSON v@(Array _) = SubCArray <$> parseJSON v
  parseJSON _ = fail "unexpected columns"

instance Backend b => ToJSON (SubscribeColumns b) where
  toJSON SubCStar         = "*"
  toJSON (SubCArray cols) = toJSON cols

data SubscribeOpSpec (b :: BackendType)
  = SubscribeOpSpec
  { sosColumns :: !(SubscribeColumns b)
  -- ^ Columns of the table that user can subscribe to listen for changes.
  , sosPayload :: !(Maybe (SubscribeColumns b))
  -- ^ Columns that the event trigger payload should consists. If set, only those columns will be
  -- visible in the payload. By default, the payload consists of all the columns of the table.
  } deriving (Show, Eq, Generic)
instance (Backend b) => NFData (SubscribeOpSpec b)
instance (Backend b) => Cacheable (SubscribeOpSpec b)

instance Backend b => FromJSON (SubscribeOpSpec b) where
  parseJSON = genericParseJSON hasuraJSON {omitNothingFields=True}

instance Backend b => ToJSON (SubscribeOpSpec b) where
  toJSON = genericToJSON hasuraJSON {omitNothingFields=True}

defaultNumRetries :: Int
defaultNumRetries = 0

defaultRetryInterval :: Int
defaultRetryInterval = 10

defaultTimeoutSeconds:: Int
defaultTimeoutSeconds = 60

defaultRetryConf :: RetryConf
defaultRetryConf = RetryConf defaultNumRetries defaultRetryInterval (Just defaultTimeoutSeconds)

data RetryConf
  = RetryConf
  { rcNumRetries  :: !Int
  , rcIntervalSec :: !Int
  , rcTimeoutSec  :: !(Maybe Int)
  } deriving (Show, Eq, Generic)
instance NFData RetryConf
instance Cacheable RetryConf
$(deriveJSON hasuraJSON{omitNothingFields=True} ''RetryConf)

data EventHeaderInfo
  = EventHeaderInfo
  { ehiHeaderConf  :: !HeaderConf
  , ehiCachedValue :: !Text
  } deriving (Show, Eq, Generic)
instance NFData EventHeaderInfo
$(deriveToJSON hasuraJSON{omitNothingFields=True} ''EventHeaderInfo)

data WebhookConf = WCValue InputWebhook | WCEnv Text
  deriving (Show, Eq, Generic)
instance NFData WebhookConf
instance Cacheable WebhookConf

instance ToJSON WebhookConf where
  toJSON (WCValue w)  = toJSON w
  toJSON (WCEnv wEnv) = object ["from_env" .= wEnv ]

instance FromJSON WebhookConf where
  parseJSON (Object o) = WCEnv <$> o .: "from_env"
  parseJSON t@(String _) =
    case fromJSON t of
      Error s   -> fail s
      Success a -> pure $ WCValue a
  parseJSON _          = fail "one of string or object must be provided for webhook"

data WebhookConfInfo
  = WebhookConfInfo
  { wciWebhookConf :: !WebhookConf
  , wciCachedValue :: !Text
  } deriving (Show, Eq, Generic)
instance NFData WebhookConfInfo
$(deriveToJSON hasuraJSON{omitNothingFields=True} ''WebhookConfInfo)

-- | The table operations on which the event trigger will be invoked.
data TriggerOpsDef (b :: BackendType)
  = TriggerOpsDef
  { tdInsert       :: !(Maybe (SubscribeOpSpec b))
  , tdUpdate       :: !(Maybe (SubscribeOpSpec b))
  , tdDelete       :: !(Maybe (SubscribeOpSpec b))
  , tdEnableManual :: !(Maybe Bool)
  } deriving (Show, Eq, Generic)
instance Backend b => NFData (TriggerOpsDef b)
instance Backend b => Cacheable (TriggerOpsDef b)

instance Backend b => FromJSON (TriggerOpsDef b) where
  parseJSON = genericParseJSON hasuraJSON  {omitNothingFields=True}

instance Backend b => ToJSON (TriggerOpsDef b) where
  toJSON = genericToJSON hasuraJSON {omitNothingFields=True}

data EventTriggerConf (b :: BackendType)
  = EventTriggerConf
  { etcName           :: !TriggerName
  , etcDefinition     :: !(TriggerOpsDef b)
  , etcWebhook        :: !(Maybe InputWebhook)
  , etcWebhookFromEnv :: !(Maybe Text)
  , etcRetryConf      :: !RetryConf
  , etcHeaders        :: !(Maybe [HeaderConf])
  } deriving (Show, Eq, Generic)
instance Backend b => Cacheable (EventTriggerConf b)

instance Backend b => FromJSON (EventTriggerConf b) where
  parseJSON = genericParseJSON hasuraJSON {omitNothingFields=True}

instance Backend b => ToJSON (EventTriggerConf b) where
  toJSON = genericToJSON hasuraJSON {omitNothingFields=True}

data RecreateEventTriggers
  = RETRecreate
  | RETDoNothing
  deriving (Show, Eq, Generic)
instance Cacheable RecreateEventTriggers

data EventTriggerInfo (b :: BackendType)
 = EventTriggerInfo
   { etiName        :: !TriggerName
   , etiOpsDef      :: !(TriggerOpsDef b)
   , etiRetryConf   :: !RetryConf
   , etiWebhookInfo :: !WebhookConfInfo
   -- ^ The HTTP(s) URL which will be called with the event payload on configured operation.
   -- Must be a POST handler. This URL can be entered manually or can be picked up from an
   -- environment variable (the environment variable needs to be set before using it for
   -- this configuration).
   , etiHeaders     :: ![EventHeaderInfo]
   -- ^ Custom headers can be added to an event trigger. Each webhook request will have these
   -- headers added.
   } deriving (Generic, Eq)
instance Backend b => NFData (EventTriggerInfo b)

instance Backend b => ToJSON (EventTriggerInfo b) where
  toJSON = genericToJSON hasuraJSON

type EventTriggerInfoMap b = M.HashMap TriggerName (EventTriggerInfo b)
