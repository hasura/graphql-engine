{-# LANGUAGE DeriveAnyClass #-}

-- | TODO(Solomon): Add Haddocks
module Hasura.Backends.DataWrapper.Agent.Schema
  ( -- * Routes
    Routes (..),

    -- * Responses
    SchemaResponse (..),
    QueryResponse (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as J
import Data.Aeson.Casing (snakeCase)
import Hasura.Backends.DataWrapper.API qualified as API
import Hasura.Incremental (Cacheable)
import Hasura.Prelude
import Servant.API
import Servant.API.Generic

--------------------------------------------------------------------------------
-- Servant Routes

data Routes mode = Routes
  { -- | 'GET /schema'
    _schema ::
      mode :- "schema"
        :> Get '[JSON] SchemaResponse,
    -- | 'POST /query'
    _query ::
      mode :- "query"
        :> ReqBody '[JSON] API.Query
        :> Post '[JSON] QueryResponse
  }
  deriving (Generic)

--------------------------------------------------------------------------------
-- Schema Response

-- | The Schema Response provides the schemas for tracked tables and
-- 'Capabilities' supported by the service.
data SchemaResponse = SchemaResponse
  { srCapabilities :: Capabilities,
    srTables :: [API.TableInfo]
  }
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving anyclass (NFData, Cacheable, Hashable)

instance ToJSON SchemaResponse where
  toJSON =
    J.genericToJSON $
      J.defaultOptions
        { J.fieldLabelModifier = snakeCase . drop 2
        }

instance FromJSON SchemaResponse where
  parseJSON =
    J.genericParseJSON $
      J.defaultOptions
        { J.fieldLabelModifier = snakeCase . drop 2
        }

-- | The 'Capabilities' describes the _capabilities_ of the
-- service. Specifically, the service is capable of serving queries
-- which involve relationships.
data Capabilities = Capabilities
  { dcRelationships :: Bool
  }
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving anyclass (NFData, Cacheable, Hashable)

instance ToJSON Capabilities where
  toJSON =
    J.genericToJSON $
      J.defaultOptions
        { J.fieldLabelModifier = snakeCase . drop 2
        }

instance FromJSON Capabilities where
  parseJSON =
    J.genericParseJSON $
      J.defaultOptions
        { J.fieldLabelModifier = snakeCase . drop 2
        }

--------------------------------------------------------------------------------
-- Query Schema

-- | The resolved query response provided by the 'POST /query'
-- endpoint encoded as 'J.Value'.
newtype QueryResponse = QueryResponse {getQueryResponse :: [J.Value]}
  deriving newtype (Eq, Ord, Show, NFData, ToJSON, FromJSON)
