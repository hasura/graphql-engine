module Hasura.RQL.Types.EventTrigger
  ( CreateEventTriggerQuery(..)
  , SubscribeOpSpec(..)
  , SubscribeColumns(..)
  , TriggerName
  , Ops(..)
  , EventId
  , TriggerOpsDef(..)
  , EventTriggerConf(..)
  , RetryConf(..)
  , DeleteEventTriggerQuery(..)
  , RedeliverEventQuery(..)
  , InvokeEventTriggerQuery(..)
  , ListenColumns(..)
  , PayloadColumns(..)
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
import           Hasura.SQL.Types
import           Language.Haskell.TH.Syntax (Lift)

import qualified Data.ByteString.Lazy       as LBS
import qualified Data.Text                  as T
import qualified Text.Regex.TDFA            as TDFA

type TriggerName = T.Text
type EventId     = T.Text

data Ops = INSERT | UPDATE | DELETE | MANUAL deriving (Show, Eq)

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

newtype ListenColumns
  = ListenColumns {
      getListenCols :: SubscribeColumns
    }
  deriving (Show, Eq, Lift)

newtype PayloadColumns
  = PayloadColumns {
      getPayloadCols :: SubscribeColumns
    }
  deriving (Show, Eq, Lift)

data SubscribeOpSpecOpt
  = SubscribeOpSpecOpt
  { sosoColumns :: !(Maybe SubscribeColumns)
  , sosoPayload :: !(Maybe SubscribeColumns)
  }
  deriving (Show, Eq, Lift)

$(deriveFromJSON (aesonDrop 4 snakeCase){omitNothingFields=True} ''SubscribeOpSpecOpt)

data SubscribeOpSpec = InsDelSpec PayloadColumns | UpdSpec PayloadColumns ListenColumns
  deriving (Show, Eq, Lift)

instance ToJSON SubscribeOpSpec where
  toJSON (InsDelSpec payload)    = object
    [ "payload" .= (toJSON $ getPayloadCols payload) ]
  toJSON (UpdSpec payload listen) = object
    [ "payload" .= (toJSON $ getPayloadCols payload)
    , "columns" .= (toJSON $ getListenCols listen)
    ]

instance FromJSON SubscribeOpSpec where
  parseJSON (Object v) = parseUpdSpec v <|> parseInsDelSpec v
    where
    parseUpdSpec obj = UpdSpec
                       <$> (PayloadColumns <$> obj .: "payload")
                       <*> (ListenColumns <$> obj .: "columns")
    parseInsDelSpec obj = InsDelSpec
                          <$> (PayloadColumns <$> obj .: "payload")
  parseJSON _          = fail "expected object for operation spec"

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
  { cetqName           :: !T.Text
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
        isMatch = TDFA.match compiledRegex (T.unpack name)
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
    let insert' = fmap mergeInsDefaults insert
        update' = fmap mergeUpdDefaults update
        delete' = fmap mergeDelDefaults delete
    return $ CreateEventTriggerQuery
      name
      table
      insert'
      update'
      delete'
      (Just enableManual)
      retryConf
      webhook
      webhookFromEnv
      headers
      replace
    where
      checkEmptyCols spec
        = case (sosoPayload <$> spec) of
        Just (Just (SubCArray cols)) -> when (null cols)
                                        (fail "found empty payload specification")
        _ -> return ()
      mergeInsDefaults specOpt = InsDelSpec
        (PayloadColumns $ fromMaybe SubCStar $ sosoPayload specOpt )
      mergeUpdDefaults specOpt = UpdSpec
        (PayloadColumns $ fromMaybe SubCStar $ sosoPayload specOpt)
        (ListenColumns $ fromMaybe (SubCArray []) $ sosoColumns specOpt)
      mergeDelDefaults specOpt = InsDelSpec
        (PayloadColumns $ fromMaybe SubCStar $ sosoPayload specOpt)

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
  { detqName :: !T.Text
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

data RedeliverEventQuery
  = RedeliverEventQuery
  { rdeqEventId :: !EventId
  } deriving (Show, Eq, Lift)

$(deriveJSON (aesonDrop 4 snakeCase){omitNothingFields=True} ''RedeliverEventQuery)

data InvokeEventTriggerQuery
  = InvokeEventTriggerQuery
  { ietqName    :: !T.Text
  , ietqPayload :: !Value
  } deriving (Show, Eq, Lift)

$(deriveJSON (aesonDrop 4 snakeCase){omitNothingFields=True} ''InvokeEventTriggerQuery)
