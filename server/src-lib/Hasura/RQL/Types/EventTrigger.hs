module Hasura.RQL.Types.EventTrigger
  ( CreateEventTriggerQuery(..)
  , SubscribeOpSpec(..)
  , SubscribeColumns(..)
  , TriggerName(..)
  , triggerNameToTxt
  , Ops(..)
  , EventId
  , TriggerOpsDef(..)
  , EventTriggerConf(..)
  , RetryConf(..)
  , DeleteEventTriggerQuery(..)
  , RedeliverEventQuery(..)
  , InvokeEventTriggerQuery(..)
  -- , HeaderConf(..)
  -- , HeaderValue(..)
  -- , HeaderName
  , EventHeaderInfo(..)
  , WebhookConf(..)
  , WebhookConfInfo(..)

  , defaultRetryConf
  , defaultTimeoutSeconds
  ) where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Hasura.Prelude
import           Hasura.RQL.DDL.Headers
import           Hasura.RQL.Types.Common    (NonEmptyText (..))
import           Hasura.SQL.Types
import           Language.Haskell.TH.Syntax (Lift)

import qualified Data.ByteString.Lazy       as LBS
import qualified Data.Text                  as T
import qualified Database.PG.Query          as Q
import qualified Text.Regex.TDFA            as TDFA

newtype TriggerName = TriggerName { unTriggerName :: NonEmptyText }
  deriving (Show, Eq, Hashable, Lift, FromJSON, ToJSON, ToJSONKey, Q.FromCol, Q.ToPrepArg)

triggerNameToTxt :: TriggerName -> Text
triggerNameToTxt = unNonEmptyText . unTriggerName

type EventId = T.Text

data Ops = INSERT | UPDATE | DELETE | MANUAL deriving (Show)

data SubscribeColumns = SubCStar | SubCArray [PGCol] deriving (Show, Eq, Lift)

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
  , sosPayload :: !(Maybe SubscribeColumns)
  } deriving (Show, Eq, Lift)

$(deriveJSON (aesonDrop 3 snakeCase){omitNothingFields=True} ''SubscribeOpSpec)

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
  } deriving (Show, Eq, Lift)

$(deriveJSON (aesonDrop 2 snakeCase){omitNothingFields=True} ''RetryConf)

data EventHeaderInfo
  = EventHeaderInfo
  { ehiHeaderConf  :: !HeaderConf
  , ehiCachedValue :: !T.Text
  } deriving (Show, Eq, Lift)

$(deriveToJSON (aesonDrop 3 snakeCase){omitNothingFields=True} ''EventHeaderInfo)

data WebhookConf = WCValue T.Text | WCEnv T.Text
  deriving (Show, Eq, Lift)

instance ToJSON WebhookConf where
  toJSON (WCValue w)  = String w
  toJSON (WCEnv wEnv) = String wEnv

data WebhookConfInfo
  = WebhookConfInfo
  { wciWebhookConf :: !WebhookConf
  , wciCachedValue :: !T.Text
  } deriving (Show, Eq, Lift)

$(deriveToJSON (aesonDrop 3 snakeCase){omitNothingFields=True} ''WebhookConfInfo)

data CreateEventTriggerQuery
  = CreateEventTriggerQuery
  { cetqName           :: !TriggerName
  , cetqTable          :: !QualifiedTable
  , cetqInsert         :: !(Maybe SubscribeOpSpec)
  , cetqUpdate         :: !(Maybe SubscribeOpSpec)
  , cetqDelete         :: !(Maybe SubscribeOpSpec)
  , cetqEnableManual   :: !(Maybe Bool)
  , cetqRetryConf      :: !(Maybe RetryConf)
  , cetqWebhook        :: !(Maybe T.Text)
  , cetqWebhookFromEnv :: !(Maybe T.Text)
  , cetqHeaders        :: !(Maybe [HeaderConf])
  , cetqReplace        :: !Bool
  } deriving (Show, Eq, Lift)

instance FromJSON CreateEventTriggerQuery where
  parseJSON (Object o) = do
    name           <- o .: "name"
    table          <- o .: "table"
    insert         <- o .:? "insert"
    update         <- o .:? "update"
    delete         <- o .:? "delete"
    enableManual   <- o .:? "enable_manual" .!= False
    retryConf      <- o .:? "retry_conf"
    webhook        <- o .:? "webhook"
    webhookFromEnv <- o .:? "webhook_from_env"
    headers        <- o .:? "headers"
    replace        <- o .:? "replace" .!= False
    let regex = "^[A-Za-z]+[A-Za-z0-9_\\-]*$" :: LBS.ByteString
        compiledRegex = TDFA.makeRegex regex :: TDFA.Regex
        isMatch = TDFA.match compiledRegex . T.unpack $ triggerNameToTxt name
    if isMatch then return ()
      else fail "only alphanumeric and underscore and hyphens allowed for name"
    if any isJust [insert, update, delete] || enableManual then
      return ()
      else
      fail "atleast one amongst insert/update/delete/enable_manual spec must be provided"
    case (webhook, webhookFromEnv) of
      (Just _, Nothing) -> return ()
      (Nothing, Just _) -> return ()
      (Just _, Just _)  -> fail "only one of webhook or webhook_from_env should be given"
      _ ->   fail "must provide webhook or webhook_from_env"
    mapM_ checkEmptyCols [insert, update, delete]
    return $ CreateEventTriggerQuery name table insert update delete (Just enableManual) retryConf webhook webhookFromEnv headers replace
    where
      checkEmptyCols spec
        = case spec of
        Just (SubscribeOpSpec (SubCArray cols) _) -> when (null cols) (fail "found empty column specification")
        Just (SubscribeOpSpec _ (Just (SubCArray cols)) ) -> when (null cols) (fail "found empty payload specification")
        _ -> return ()
  parseJSON _ = fail "expecting an object"

$(deriveToJSON (aesonDrop 4 snakeCase){omitNothingFields=True} ''CreateEventTriggerQuery)

data TriggerOpsDef
  = TriggerOpsDef
  { tdInsert       :: !(Maybe SubscribeOpSpec)
  , tdUpdate       :: !(Maybe SubscribeOpSpec)
  , tdDelete       :: !(Maybe SubscribeOpSpec)
  , tdEnableManual :: !(Maybe Bool)
  } deriving (Show, Eq, Lift)

$(deriveJSON (aesonDrop 2 snakeCase){omitNothingFields=True} ''TriggerOpsDef)

data DeleteEventTriggerQuery
  = DeleteEventTriggerQuery
  { detqName :: !TriggerName
  } deriving (Show, Eq, Lift)

$(deriveJSON (aesonDrop 4 snakeCase){omitNothingFields=True} ''DeleteEventTriggerQuery)

data EventTriggerConf
  = EventTriggerConf
  { etcName           :: !TriggerName
  , etcDefinition     :: !TriggerOpsDef
  , etcWebhook        :: !(Maybe T.Text)
  , etcWebhookFromEnv :: !(Maybe T.Text)
  , etcRetryConf      :: !RetryConf
  , etcHeaders        :: !(Maybe [HeaderConf])
  } deriving (Show, Eq, Lift)

$(deriveJSON (aesonDrop 3 snakeCase){omitNothingFields=True} ''EventTriggerConf)

newtype RedeliverEventQuery
  = RedeliverEventQuery
  { rdeqEventId :: EventId
  } deriving (Show, Eq, Lift)

$(deriveJSON (aesonDrop 4 snakeCase){omitNothingFields=True} ''RedeliverEventQuery)

data InvokeEventTriggerQuery
  = InvokeEventTriggerQuery
  { ietqName    :: !TriggerName
  , ietqPayload :: !Value
  } deriving (Show, Eq, Lift)

$(deriveJSON (aesonDrop 4 snakeCase){omitNothingFields=True} ''InvokeEventTriggerQuery)
