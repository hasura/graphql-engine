--
module Hasura.Backends.DataWrapper.API
  ( module V0,
    Api,
    SchemaApi,
    QueryApi,
    openApiSchema,
    Routes (..),
    apiClient,
  )
where

import Data.Data (Proxy (..))
import Data.OpenApi (OpenApi)
import Hasura.Backends.DataWrapper.API.V0.API as V0
import Servant.API
import Servant.API.Generic
import Servant.Client (Client, ClientM, client)
import Servant.OpenApi

--------------------------------------------------------------------------------
-- Servant Routes

type SchemaApi =
  "schema"
    :> Get '[JSON] V0.SchemaResponse

type QueryApi =
  "query"
    :> ReqBody '[JSON] V0.Query
    :> Post '[JSON] V0.QueryResponse

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

-- | Provide an OpenApi 3.0 schema for the API
openApiSchema :: OpenApi
openApiSchema = toOpenApi (Proxy :: Proxy Api)

apiClient :: Client ClientM (NamedRoutes Routes)
apiClient =
  client (Proxy @(NamedRoutes Routes))
