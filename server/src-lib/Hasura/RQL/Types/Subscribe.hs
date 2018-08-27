{-# LANGUAGE DeriveLift        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Hasura.RQL.Types.Subscribe
  ( SubscribeTableQuery(..)
  , SubscribeOpSpec(..)
  , SubscribeColumns(..)
  , TriggerName
  , TriggerDefinition(..)
  , EventTrigger(..)
  , RetryConf(..)
  , toRetryConf
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
  { rcNumRetries  :: !(Maybe Int64)
  , rcIntervalSec :: !(Maybe Int64)
  } deriving (Show, Eq, Lift)

$(deriveJSON (aesonDrop 2 snakeCase){omitNothingFields=True} ''RetryConf)

toRetryConf :: Int64 -> Int64 -> RetryConf
toRetryConf nr rint= RetryConf (Just nr) (Just rint)

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

data TriggerDefinition
  = TriggerDefinition
  { tdInsert :: !(Maybe SubscribeOpSpec)
  , tdUpdate :: !(Maybe SubscribeOpSpec)
  , tdDelete :: !(Maybe SubscribeOpSpec)
  }

$(deriveJSON (aesonDrop 2 snakeCase){omitNothingFields=True} ''TriggerDefinition)

data UnsubscribeTableQuery
  = UnsubscribeTableQuery
  { utqName :: !T.Text
  } deriving (Show, Eq, Lift)

$(deriveJSON (aesonDrop 3 snakeCase){omitNothingFields=True} ''UnsubscribeTableQuery)

data EventTrigger
  = EventTrigger
  { etName       :: TriggerName
  , etType       :: T.Text
  , etTable      :: QualifiedTable
  , etDefinition :: TriggerDefinition
  }

$(deriveJSON (aesonDrop 2 snakeCase){omitNothingFields=True} ''EventTrigger)
