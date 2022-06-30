--
module Hasura.Backends.DataConnector.API
  ( module V0,
    Api,
    SchemaApi,
    QueryApi,
    ConfigHeader,
    SourceNameHeader,
    SourceName,
    openApiSchema,
    Routes (..),
    apiClient,
  )
where

import Data.Data (Proxy (..))
import Data.OpenApi (OpenApi)
import Data.Text (Text)
import Hasura.Backends.DataConnector.API.V0 as V0
import Servant.API
import Servant.API.Generic
import Servant.Client (Client, ClientM, client)
import Servant.OpenApi

--------------------------------------------------------------------------------
-- Servant Routes

type CapabilitiesApi =
  "capabilities"
    :> Get '[JSON] V0.CapabilitiesResponse

type SchemaApi =
  "schema"
    :> SourceNameHeader Required
    :> ConfigHeader Required
    :> Get '[JSON] V0.SchemaResponse

type QueryApi =
  "query"
    :> SourceNameHeader Required
    :> ConfigHeader Required
    :> ReqBody '[JSON] V0.QueryRequest
    :> Post '[JSON] V0.QueryResponse

type HealthApi =
  "health"
    :> SourceNameHeader Optional
    :> ConfigHeader Optional
    :> GetNoContent

type ConfigHeader optionality = Header' '[optionality, Strict] "X-Hasura-DataConnector-Config" V0.Config

type SourceNameHeader optionality = Header' '[optionality, Strict] "X-Hasura-DataConnector-SourceName" SourceName

type SourceName = Text

data Routes mode = Routes
  { -- | 'GET /capabilities'
    _capabilities :: mode :- CapabilitiesApi,
    -- | 'GET /schema'
    _schema :: mode :- SchemaApi,
    -- | 'POST /query'
    _query :: mode :- QueryApi,
    -- | 'GET /health'
    _health :: mode :- HealthApi
  }
  deriving stock (Generic)

-- | servant-openapi3 does not (yet) support NamedRoutes so we need to compose the
-- API the old-fashioned way using :<|> for use by @toOpenApi@
type Api = CapabilitiesApi :<|> SchemaApi :<|> QueryApi :<|> HealthApi

-- | Provide an OpenApi 3.0 schema for the API
openApiSchema :: OpenApi
openApiSchema = toOpenApi (Proxy @Api)

apiClient :: Client ClientM (NamedRoutes Routes)
apiClient =
  client (Proxy @(NamedRoutes Routes))
