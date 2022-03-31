{-# LANGUAGE DeriveAnyClass #-}

-- | TODO(Solomon): Add Haddocks
module Hasura.Backends.DataWrapper.Agent.Schema
  ( -- * Routes
    Routes (..),

    -- * Responses

    -- TODO: I think the response DTOs should be part of API/V0?
    SchemaResponse (..),
    Capabilities (..),
    QueryResponse (..),
    openApiSchema,
  )
where

import Autodocodec
import Autodocodec.OpenAPI ()
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as J
import Data.OpenApi
import Data.Proxy
import Hasura.Backends.DataWrapper.API qualified as API
import Hasura.Incremental (Cacheable)
import Hasura.Prelude
import Servant.API
import Servant.API.Generic
import Servant.OpenApi

--------------------------------------------------------------------------------
-- Servant Routes

type SchemaApi =
  "schema"
    :> Get '[JSON] SchemaResponse

type QueryApi =
  "query"
    :> ReqBody '[JSON] API.Query
    :> Post '[JSON] QueryResponse

data Routes mode = Routes
  { -- | 'GET /schema'
    _schema :: mode :- SchemaApi,
    -- | 'POST /query'
    _query :: mode :- QueryApi
  }
  deriving stock (Generic)

-- | servant-openapi3 does not (yet) support NamedRoutes so we need to compose the
-- API the old-fashioned way using :<|> for use by @toOpenApi@
type Api = SchemaApi :<|> QueryApi

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
  deriving anyclass (NFData, Cacheable, Hashable)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec Capabilities

instance HasCodec Capabilities where
  codec =
    object "Capabilities" $
      Capabilities <$> requiredField "relationships" "Does the agent support relationships?" .= dcRelationships

--------------------------------------------------------------------------------
-- Query Schema

-- | The resolved query response provided by the 'POST /query'
-- endpoint encoded as 'J.Value'.
newtype QueryResponse = QueryResponse {getQueryResponse :: [J.Object]}
  deriving newtype (Eq, Ord, Show, NFData)
  deriving (ToJSON, FromJSON, ToSchema) via Autodocodec [J.Object]

-- | Provide an OpenApi 3.0 schema for the API
openApiSchema :: OpenApi
openApiSchema = toOpenApi (Proxy :: Proxy Api)
