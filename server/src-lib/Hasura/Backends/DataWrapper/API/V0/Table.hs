{-# LANGUAGE DeriveAnyClass #-}

--
module Hasura.Backends.DataWrapper.API.V0.Table
  ( TableInfo (..),
    TableName (..),
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as J
import Data.Aeson.Casing (snakeCase)
import Hasura.Backends.DataWrapper.API.V0.Column qualified as API.V0
import Hasura.Incremental (Cacheable)
import Hasura.Prelude

--------------------------------------------------------------------------------

newtype TableName = TableName Text
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving newtype (FromJSON, ToJSON)
  deriving anyclass (NFData, Cacheable, Hashable)

--------------------------------------------------------------------------------

-- | Table schema data from the 'SchemaResponse'.
data TableInfo = TableInfo
  { dtiName :: TableName,
    dtiColumns :: [API.V0.ColumnInfo],
    dtiPrimaryKey :: Maybe Text,
    dtiDescription :: Maybe Text
  }
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving anyclass (NFData, Cacheable, Hashable)

instance ToJSON TableInfo where
  toJSON =
    J.genericToJSON $
      J.defaultOptions
        { J.fieldLabelModifier = snakeCase . drop 3
        }

instance FromJSON TableInfo where
  parseJSON =
    J.genericParseJSON $
      J.defaultOptions
        { J.fieldLabelModifier = snakeCase . drop 3
        }
