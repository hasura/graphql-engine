{-# LANGUAGE DeriveLift        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Hasura.RQL.Types.SubscribeTable
  ( SubscribeTableQuery(..)
  , SubscribeOpSpec(..)
  , SubscribeColumns(..)
  , TriggerDefinition(..)
  , TriggerName
  ) where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Hasura.Prelude
import           Hasura.SQL.Types
import           Language.Haskell.TH.Syntax (Lift)

import qualified Data.Text                  as T

type TriggerName = T.Text

data SubscribeTableQuery
  = SubscribeTableQuery
  { stqName    :: !T.Text
  , stqTable   :: !QualifiedTable
  , stqInsert  :: !(Maybe SubscribeOpSpec)
  , stqUpdate  :: !(Maybe SubscribeOpSpec)
  , stqDelete  :: !(Maybe SubscribeOpSpec)
  , stgWebhook :: !T.Text
  } deriving (Show, Eq, Lift)

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

data TriggerDefinition
  = TriggerDefinition
  { tdInsert :: !(Maybe SubscribeOpSpec)
  , tdUpdate :: !(Maybe SubscribeOpSpec)
  , tdDelete :: !(Maybe SubscribeOpSpec)
  }

$(deriveJSON (aesonDrop 2 snakeCase){omitNothingFields=True} ''TriggerDefinition)

instance FromJSON SubscribeTableQuery where
  parseJSON (Object o) = do
    name    <- o .: "name"
    table   <- o .: "table"
    insert  <- o .:? "insert"
    update  <- o .:? "update"
    delete  <- o .:? "delete"
    webhook <- o .: "webhook"
    case insert <|> update <|> delete of
      Just _  -> return ()
      Nothing -> fail "must provide operation spec(s)"
    return $ SubscribeTableQuery name table insert update delete webhook
  parseJSON _ = fail "expecting an object"

$(deriveToJSON (aesonDrop 3 snakeCase){omitNothingFields=True} ''SubscribeTableQuery)

