{-# LANGUAGE DeriveLift        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Hasura.RQL.Types.Subscribe
  ( CreateEventTriggerQuery(..)
  , SubscribeOpSpec(..)
  , SubscribeColumns(..)
  , TriggerName
  , TriggerId
  , Ops(..)
  , EventId
  , TriggerOpsDef(..)
  , EventTrigger(..)
  , EventTriggerConf(..)
  , RetryConf(..)
  , DeleteEventTriggerQuery(..)
  , DeliverEventQuery(..)
  , HeaderConf(..)
  , HeaderValue(..)
  , HeaderName
  , EventHeaderInfo(..)
  , WebhookConf(..)
  , WebhookConfInfo(..)
  ) where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Hasura.Prelude
import           Hasura.SQL.Types
import           Language.Haskell.TH.Syntax (Lift)
import           Text.Regex                 (matchRegex, mkRegex)

import qualified Data.Text                  as T

type TriggerName = T.Text
type TriggerId   = T.Text
type EventId     = T.Text
type HeaderName  = T.Text

data Ops = INSERT | UPDATE | DELETE deriving (Show)

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

data RetryConf
  = RetryConf
  { rcNumRetries  :: !Int
  , rcIntervalSec :: !Int
  } deriving (Show, Eq, Lift)

$(deriveJSON (aesonDrop 2 snakeCase){omitNothingFields=True} ''RetryConf)

data HeaderConf = HeaderConf HeaderName HeaderValue
   deriving (Show, Eq, Lift)

data HeaderValue = HVValue T.Text | HVEnv T.Text
   deriving (Show, Eq, Lift)

instance FromJSON HeaderConf where
  parseJSON (Object o) = do
    name <- o .: "name"
    value <- o .:? "value"
    valueFromEnv <- o .:? "value_from_env"
    case (value, valueFromEnv ) of
      (Nothing, Nothing)  -> fail "expecting value or value_from_env keys"
      (Just val, Nothing) -> return $ HeaderConf name (HVValue val)
      (Nothing, Just val) -> return $ HeaderConf name (HVEnv val)
      (Just _, Just _)    -> fail "expecting only one of value or value_from_env keys"
  parseJSON _ = fail "expecting object for headers"

instance ToJSON HeaderConf where
  toJSON (HeaderConf name (HVValue val)) = object ["name" .= name, "value" .= val]
  toJSON (HeaderConf name (HVEnv val)) = object ["name" .= name, "value_from_env" .= val]

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
    retryConf      <- o .:? "retry_conf"
    webhook        <- o .:? "webhook"
    webhookFromEnv <- o .:? "webhook_from_env"
    headers        <- o .:? "headers"
    replace        <- o .:? "replace" .!= False
    let regex = mkRegex "^\\w+$"
        mName = matchRegex regex (T.unpack name)
    case mName of
      Just _  -> return ()
      Nothing -> fail "only alphanumeric and underscore allowed for name"
    case insert <|> update <|> delete of
      Just _  -> return ()
      Nothing -> fail "must provide operation spec(s)"
    case (webhook, webhookFromEnv) of
      (Just _, Nothing)  -> return ()
      (Nothing, Just _) -> return ()
      (Just _, Just _) -> fail "only one of webhook or webhook_from_env should be given"
      _ ->   fail "must provide webhook or webhook_from_env"
    mapM_ checkEmptyCols [insert, update, delete]
    return $ CreateEventTriggerQuery name table insert update delete retryConf webhook webhookFromEnv headers replace
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
  { tdInsert :: !(Maybe SubscribeOpSpec)
  , tdUpdate :: !(Maybe SubscribeOpSpec)
  , tdDelete :: !(Maybe SubscribeOpSpec)
  } deriving (Show, Eq, Lift)

$(deriveJSON (aesonDrop 2 snakeCase){omitNothingFields=True} ''TriggerOpsDef)

data DeleteEventTriggerQuery
  = DeleteEventTriggerQuery
  { detqName :: !T.Text
  } deriving (Show, Eq, Lift)

$(deriveJSON (aesonDrop 4 snakeCase){omitNothingFields=True} ''DeleteEventTriggerQuery)

data EventTrigger
  = EventTrigger
  { etTable      :: !QualifiedTable
  , etName       :: !TriggerName
  , etDefinition :: !TriggerOpsDef
  , etWebhook    :: !T.Text
  , etRetryConf  :: !RetryConf
  }

$(deriveJSON (aesonDrop 2 snakeCase){omitNothingFields=True} ''EventTrigger)

data EventTriggerConf
  = EventTriggerConf
  { etcName          :: !TriggerName
  , etcOpsDefinition :: !TriggerOpsDef
  , etcWebhookConf   :: !WebhookConf
  , etcRetryConf     :: !RetryConf
  , etcHeaders       :: !(Maybe [HeaderConf])
  } deriving (Show, Eq, Lift)

instance FromJSON EventTriggerConf where
  parseJSON (Object o) = do
    name <- o .: "name"
    opsDef <- o .: "definition"
    webhook <- o .:? "webhook"
    webhookFromEnv <- o .:? "webhook_from_env"
    retryConf <- o .: "retry_conf"
    headers <- o .:? "headers"
    webhookConf <- case (webhook, webhookFromEnv) of
                        (Just w, Nothing)  -> return $ WCValue w
                        (Nothing, Just wEnv) -> return $ WCEnv wEnv
                        (Just _, Just _) -> fail "only one of webhook or webhook_from_env should be given"
                        _ ->   fail "must provide webhook or webhook_from_env"
    return $ EventTriggerConf name opsDef webhookConf retryConf headers
  parseJSON _ = fail "expecting object for event_trigger_def"

instance ToJSON EventTriggerConf where
  toJSON (EventTriggerConf name def (WCValue w) rc headers) = object ["name" .= name
                                                                   , "definition" .= def
                                                                   , "webhook" .= w
                                                                   , "retry_conf" .= rc
                                                                   , "headers" .= headers
                                                                    ]
  toJSON (EventTriggerConf name def (WCEnv wEnv) rc headers) = object ["name" .= name
                                                                   , "definition" .= def
                                                                   , "webhook_from_env" .= wEnv
                                                                   , "retry_conf" .= rc
                                                                   , "headers" .= headers
                                                                   ]

data DeliverEventQuery
  = DeliverEventQuery
  { deqEventId :: !EventId
  } deriving (Show, Eq, Lift)

$(deriveJSON (aesonDrop 3 snakeCase){omitNothingFields=True} ''DeliverEventQuery)
