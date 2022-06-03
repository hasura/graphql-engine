{-# LANGUAGE DeriveAnyClass #-}

module Hasura.Backends.DataConnector.API.V0.Schema
  ( SchemaResponse (..),
  )
where

import Autodocodec
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import Data.Hashable (Hashable)
import Data.OpenApi (ToSchema)
import GHC.Generics (Generic)
import Hasura.Backends.DataConnector.API.V0.Table qualified as API.V0
import Prelude

--------------------------------------------------------------------------------
-- Schema Response

-- | The Schema Response provides the schemas for tracked tables and
-- 'Capabilities' supported by the service.
newtype SchemaResponse = SchemaResponse
  { srTables :: [API.V0.TableInfo]
  }
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec SchemaResponse

instance HasCodec SchemaResponse where
  codec =
    object "SchemaResponse" $
      SchemaResponse <$> requiredField "tables" "Available tables" .= srTables
