{-# LANGUAGE DeriveAnyClass #-}

module Hasura.Backends.DataWrapper.API.V0.Schema
  ( SchemaResponse (..),
    QueryResponse (..),
    Capabilities (..),
  )
where

import Autodocodec
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, Object, ToJSON)
import Data.Data (Data)
import Data.Hashable (Hashable)
import Data.OpenApi (ToSchema)
import GHC.Generics (Generic)
import Hasura.Backends.DataWrapper.API.V0.Table qualified as API.V0
import Prelude

--------------------------------------------------------------------------------
-- Schema Response

-- | The Schema Response provides the schemas for tracked tables and
-- 'Capabilities' supported by the service.
data SchemaResponse = SchemaResponse
  { srCapabilities :: Capabilities,
    srTables :: [API.V0.TableInfo]
  }
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec SchemaResponse

instance HasCodec SchemaResponse where
  codec =
    object "SchemaResponse" $
      SchemaResponse
        <$> requiredField "capabilities" "Capabilities of the agent" .= srCapabilities
        <*> requiredField "tables" "Available tables" .= srTables

-- | The 'Capabilities' describes the _capabilities_ of the
-- service. Specifically, the service is capable of serving queries
-- which involve relationships.
data Capabilities = Capabilities
  { dcRelationships :: Bool
  }
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec Capabilities

instance HasCodec Capabilities where
  codec =
    object "Capabilities" $
      Capabilities <$> requiredField "relationships" "Does the agent support relationships?" .= dcRelationships

--------------------------------------------------------------------------------
-- Query Response

-- | The resolved query response provided by the 'POST /query'
-- endpoint encoded as 'J.Value'.
newtype QueryResponse = QueryResponse {getQueryResponse :: [Object]}
  deriving newtype (Eq, Ord, Show, NFData)
  deriving (ToJSON, FromJSON, ToSchema) via Autodocodec [Object]
