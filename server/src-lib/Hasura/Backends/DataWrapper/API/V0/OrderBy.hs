{-# LANGUAGE DeriveAnyClass #-}

--
module Hasura.Backends.DataWrapper.API.V0.OrderBy
  ( OrderBy (..),
    OrderType (..),
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

data OrderBy = OrderBy
  { column :: API.V0.ColumnName,
    ordering :: OrderType
  }
  deriving stock (Data, Eq, Generic, Ord, Show)
  deriving anyclass (Cacheable, Hashable, NFData)

instance ToJSON OrderBy where
  toJSON = J.genericToJSON J.defaultOptions

instance FromJSON OrderBy where
  parseJSON = J.genericParseJSON J.defaultOptions

--------------------------------------------------------------------------------

data OrderType
  = Ascending
  | Descending
  deriving stock (Data, Eq, Generic, Ord, Show)
  deriving anyclass (Cacheable, Hashable, NFData)

instance ToJSON OrderType where
  toJSON =
    J.genericToJSON $
      J.defaultOptions
        { J.constructorTagModifier = snakeCase
        }

instance FromJSON OrderType where
  parseJSON =
    J.genericParseJSON $
      J.defaultOptions
        { J.constructorTagModifier = snakeCase
        }
