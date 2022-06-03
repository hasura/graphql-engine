--
module Hasura.Backends.DataConnector.API
  ( module V0,
    Api,
    SchemaApi,
    QueryApi,
    ConfigHeader,
    SourceNameHeader,
    SourceName,
    openApiSchemaJson,
    Routes (..),
    apiClient,
  )
where

import Data.Aeson qualified as J
import Data.Data (Proxy (..))
import Data.OpenApi (OpenApi)
import Data.Text (Text)
import Hasura.Backends.DataConnector.API.V0.API as V0
import Servant.API
import Servant.API.Generic
import Servant.Client (Client, ClientM, client)
import Servant.OpenApi
import Prelude

--------------------------------------------------------------------------------
-- Servant Routes

type CapabilitiesApi =
  "capabilities"
    :> Get '[JSON] V0.CapabilitiesResponse

type SchemaApi =
  "schema"
    :> SourceNameHeader
    :> ConfigHeader
    :> Get '[JSON] V0.SchemaResponse

type QueryApi =
  "query"
    :> SourceNameHeader
    :> ConfigHeader
    :> ReqBody '[JSON] V0.Query
    :> Post '[JSON] V0.QueryResponse

type ConfigHeader = Header' '[Required, Strict] "X-Hasura-DataConnector-Config" V0.Config

type SourceNameHeader = Header' '[Required, Strict] "X-Hasura-DataConnector-SourceName" SourceName

type SourceName = Text

data Routes mode = Routes
  { -- | 'GET /capabilities'
    _capabilities :: mode :- CapabilitiesApi,
    -- | 'GET /schema'
    _schema :: mode :- SchemaApi,
    -- | 'POST /query'
    _query :: mode :- QueryApi
  }
  deriving stock (Generic)

-- | servant-openapi3 does not (yet) support NamedRoutes so we need to compose the
-- API the old-fashioned way using :<|> for use by @toOpenApi@
type Api = CapabilitiesApi :<|> SchemaApi :<|> QueryApi

-- | Provide an OpenApi 3.0 schema for the API
openApiSchema :: OpenApi
openApiSchema = toOpenApi (Proxy @Api)

-- | The OpenAPI 3.0 schema for the API
--
-- This is not exposed as the 'OpenApi' type because we need to do some hackery in
-- the serialized JSON to work around some limitations in the openapi3 library
openApiSchemaJson :: J.Value
openApiSchemaJson = V0.fixExternalSchemaRefsInComponentSchemas $ J.toJSON openApiSchema

apiClient :: Client ClientM (NamedRoutes Routes)
apiClient =
  client (Proxy @(NamedRoutes Routes))
