{-# LANGUAGE DeriveAnyClass #-}

--
module Hasura.Backends.DataWrapper.API.V0.Column
  ( ColumnInfo (..),
    ColumnName (..),
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as J
import Data.Aeson.Casing (snakeCase)
import Hasura.Backends.DataWrapper.API.V0.Scalar.Type qualified as API.V0.Scalar
import Hasura.Incremental (Cacheable)
import Hasura.Prelude

--------------------------------------------------------------------------------

newtype ColumnName = ColumnName Text
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving newtype (FromJSON, ToJSON)
  deriving anyclass (NFData, Cacheable, Hashable)

--------------------------------------------------------------------------------

data ColumnInfo = ColumnInfo
  { dciName :: ColumnName,
    dciType :: API.V0.Scalar.Type,
    dciNullable :: Bool,
    dciDescription :: Maybe Text
  }
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving anyclass (NFData, Cacheable, Hashable)

instance ToJSON ColumnInfo where
  toJSON =
    J.genericToJSON $
      J.defaultOptions
        { J.fieldLabelModifier = snakeCase . drop 3
        }

instance FromJSON ColumnInfo where
  parseJSON =
    J.genericParseJSON $
      J.defaultOptions
        { J.fieldLabelModifier = snakeCase . drop 3
        }
