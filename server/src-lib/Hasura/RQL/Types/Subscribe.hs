{-# LANGUAGE DeriveLift        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Hasura.RQL.Types.Subscribe
  ( SubscribeTableQuery(..)
  , SubscribeOpSpec(..)
  , SubscribeColumns(..)
  , TriggerName
  , TriggerId
  , TriggerOpsDef(..)
  , EventTrigger(..)
  , EventTriggerDef(..)
  , RetryConf(..)
  , UnsubscribeTableQuery(..)
  ) where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.Int                   (Int64)
import           Hasura.Prelude
import           Hasura.SQL.Types
import           Language.Haskell.TH.Syntax (Lift)

import qualified Data.Text                  as T

type TriggerName = T.Text
type TriggerId = T.Text

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
  } deriving (Show, Eq, Lift)

$(deriveJSON (aesonDrop 3 snakeCase){omitNothingFields=True} ''SubscribeOpSpec)

data RetryConf
  = RetryConf
  { rcNumRetries  :: !Int64
  , rcIntervalSec :: !Int64
  } deriving (Show, Eq, Lift)

$(deriveJSON (aesonDrop 2 snakeCase){omitNothingFields=True} ''RetryConf)

data SubscribeTableQuery
  = SubscribeTableQuery
  { stqName      :: !T.Text
  , stqTable     :: !QualifiedTable
  , stqInsert    :: !(Maybe SubscribeOpSpec)
  , stqUpdate    :: !(Maybe SubscribeOpSpec)
  , stqDelete    :: !(Maybe SubscribeOpSpec)
  , stqRetryConf :: !(Maybe RetryConf)
  , stgWebhook   :: !T.Text
  } deriving (Show, Eq, Lift)

instance FromJSON SubscribeTableQuery where
  parseJSON (Object o) = do
    name      <- o .: "name"
    table     <- o .: "table"
    insert    <- o .:? "insert"
    update    <- o .:? "update"
    delete    <- o .:? "delete"
    retryConf <- o .:? "retry_conf"
    webhook   <- o .: "webhook"
    case insert <|> update <|> delete of
      Just _  -> return ()
      Nothing -> fail "must provide operation spec(s)"
    return $ SubscribeTableQuery name table insert update delete retryConf webhook
  parseJSON _ = fail "expecting an object"

$(deriveToJSON (aesonDrop 3 snakeCase){omitNothingFields=True} ''SubscribeTableQuery)

data TriggerOpsDef
  = TriggerOpsDef
  { tdInsert :: !(Maybe SubscribeOpSpec)
  , tdUpdate :: !(Maybe SubscribeOpSpec)
  , tdDelete :: !(Maybe SubscribeOpSpec)
  } deriving (Show, Eq, Lift)

$(deriveJSON (aesonDrop 2 snakeCase){omitNothingFields=True} ''TriggerOpsDef)

data UnsubscribeTableQuery
  = UnsubscribeTableQuery
  { utqName :: !T.Text
  } deriving (Show, Eq, Lift)

$(deriveJSON (aesonDrop 3 snakeCase){omitNothingFields=True} ''UnsubscribeTableQuery)

data EventTrigger
  = EventTrigger
  { etTable      :: !QualifiedTable
  , etName       :: !TriggerName
  , etDefinition :: !TriggerOpsDef
  , etWebhook    :: !T.Text
  , etRetryConf  :: !RetryConf
  }

$(deriveJSON (aesonDrop 2 snakeCase){omitNothingFields=True} ''EventTrigger)

data EventTriggerDef
  = EventTriggerDef
  { etdName       :: !TriggerName
  , etdDefinition :: !TriggerOpsDef
  , etdWebhook    :: !T.Text
  , etdRetryConf  :: !RetryConf
  } deriving (Show, Eq, Lift)

$(deriveJSON (aesonDrop 3 snakeCase){omitNothingFields=True} ''EventTriggerDef)
