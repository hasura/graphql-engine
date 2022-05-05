--
module Hasura.Backends.DataConnector.API
  ( module V0,
    Api,
    SchemaApi,
    QueryApi,
    ConfigHeader,
    openApiSchemaJson,
    Routes (..),
    apiClient,
  )
where

import Data.Aeson qualified as J
import Data.Data (Proxy (..))
import Data.OpenApi (OpenApi)
import Hasura.Backends.DataConnector.API.V0.API as V0
import Servant.API
import Servant.API.Generic
import Servant.Client (Client, ClientM, client)
import Servant.OpenApi
import Prelude

--------------------------------------------------------------------------------
-- Servant Routes

type ConfigSchemaApi =
  "config-schema"
    :> Get '[JSON] V0.ConfigSchemaResponse

type SchemaApi =
  "schema"
    :> ConfigHeader
    :> Get '[JSON] V0.SchemaResponse

type QueryApi =
  "query"
    :> ConfigHeader
    :> ReqBody '[JSON] V0.Query
    :> Post '[JSON] V0.QueryResponse

type ConfigHeader = Header' '[Required, Strict] "X-Hasura-DataConnector-Config" V0.Config

data Routes mode = Routes
  { -- | 'GET /config-schema'
    _configSchema :: mode :- ConfigSchemaApi,
    -- | 'GET /schema'
    _schema :: mode :- SchemaApi,
    -- | 'POST /query'
    _query :: mode :- QueryApi
  }
  deriving stock (Generic)

-- | servant-openapi3 does not (yet) support NamedRoutes so we need to compose the
-- API the old-fashioned way using :<|> for use by @toOpenApi@
type Api = ConfigSchemaApi :<|> SchemaApi :<|> QueryApi

-- | Provide an OpenApi 3.0 schema for the API
openApiSchema :: OpenApi
openApiSchema = toOpenApi (Proxy :: Proxy Api)

-- | The OpenAPI 3.0 schema for the API
--
-- This is not exposed as the 'OpenApi' type because we need to do some hackery in
-- the serialized JSON to work around some limitations in the openapi3 library
openApiSchemaJson :: J.Value
openApiSchemaJson = V0.fixExternalSchemaRefsInComponentSchemas $ J.toJSON openApiSchema

apiClient :: Client ClientM (NamedRoutes Routes)
apiClient =
  client (Proxy @(NamedRoutes Routes))
