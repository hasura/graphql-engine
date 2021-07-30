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
  ) where

import           Hasura.Prelude

import qualified Database.PG.Query                  as Q

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Text.Extended
import           Data.Text.NonEmpty

import qualified Hasura.Backends.Postgres.SQL.Types as PG

import           Hasura.Incremental                 (Cacheable)
import           Hasura.RQL.DDL.Headers
import           Hasura.RQL.Types.Common            (InputWebhook)


-- | Unique name for event trigger.
newtype TriggerName = TriggerName { unTriggerName :: NonEmptyText }
  deriving (Show, Eq, Ord, Hashable, ToTxt, FromJSON, ToJSON, ToJSONKey
           , Q.ToPrepArg, Generic, NFData, Cacheable, Q.FromCol)

triggerNameToTxt :: TriggerName -> Text
triggerNameToTxt = unNonEmptyText . unTriggerName

newtype EventId = EventId {unEventId :: Text}
  deriving (Show, Eq, Ord, Hashable, ToTxt, FromJSON, ToJSON, ToJSONKey, Q.FromCol, Q.ToPrepArg, Generic, NFData, Cacheable)

data Ops = INSERT | UPDATE | DELETE | MANUAL deriving (Show)

data SubscribeColumns = SubCStar | SubCArray [PG.PGCol]
  deriving (Show, Eq, Generic)
instance NFData SubscribeColumns
instance Cacheable SubscribeColumns

instance FromJSON SubscribeColumns where
  parseJSON (String s) = case s of
                          "*" -> return SubCStar
                          _   -> fail "only * or [] allowed"
  parseJSON v@(Array _) = SubCArray <$> parseJSON v
  parseJSON _ = fail "unexpected columns"

instance ToJSON SubscribeColumns where
  toJSON SubCStar         = "*"
  toJSON (SubCArray cols) = toJSON cols

data SubscribeOpSpec
  = SubscribeOpSpec
  { sosColumns :: !SubscribeColumns
  -- ^ Columns of the table that user can subscribe to listen for changes.
  , sosPayload :: !(Maybe SubscribeColumns)
  -- ^ Columns that the event trigger payload should consists. If set, only those columns will be
  -- visible in the payload. By default, the payload consists of all the columns of the table.
  } deriving (Show, Eq, Generic)
instance NFData SubscribeOpSpec
instance Cacheable SubscribeOpSpec
$(deriveJSON hasuraJSON{omitNothingFields=True} ''SubscribeOpSpec)

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
data TriggerOpsDef
  = TriggerOpsDef
  { tdInsert       :: !(Maybe SubscribeOpSpec)
  , tdUpdate       :: !(Maybe SubscribeOpSpec)
  , tdDelete       :: !(Maybe SubscribeOpSpec)
  , tdEnableManual :: !(Maybe Bool)
  } deriving (Show, Eq, Generic)
instance NFData TriggerOpsDef
instance Cacheable TriggerOpsDef
$(deriveJSON hasuraJSON{omitNothingFields=True} ''TriggerOpsDef)

data EventTriggerConf
  = EventTriggerConf
  { etcName           :: !TriggerName
  , etcDefinition     :: !TriggerOpsDef
  , etcWebhook        :: !(Maybe InputWebhook)
  , etcWebhookFromEnv :: !(Maybe Text)
  , etcRetryConf      :: !RetryConf
  , etcHeaders        :: !(Maybe [HeaderConf])
  } deriving (Show, Eq, Generic)
instance Cacheable EventTriggerConf

$(deriveJSON hasuraJSON{omitNothingFields=True} ''EventTriggerConf)

data RecreateEventTriggers
  = RETRecreate
  | RETDoNothing
  deriving (Show, Eq, Generic)
instance Cacheable RecreateEventTriggers
