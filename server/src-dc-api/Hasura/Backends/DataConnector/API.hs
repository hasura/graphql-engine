--
module Hasura.Backends.DataConnector.API
  ( module V0,
    Api,
    SchemaApi,
    QueryApi,
    ConfigHeader,
    Prometheus,
    SourceNameHeader,
    SourceName,
    openApiSchema,
    Routes (..),
    apiClient,
  )
where

import Control.Arrow (left)
import Data.ByteString.Lazy as BL
import Data.Data (Proxy (..))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.OpenApi (OpenApi)
import Data.Text (Text)
import Data.Text.Encoding as TE
import Hasura.Backends.DataConnector.API.V0 as V0
import Network.HTTP.Media ((//), (/:))
import Servant.API
import Servant.API.Generic
import Servant.Client (Client, ClientM, client)
import Servant.OpenApi
import Prelude (show)

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

data Prometheus

-- NOTE: This seems like quite a brittle definition and we may want to be
--       more permissive about the allowed content-types. However for now
--       we can just demand that agent authors pick one of the following.
instance Accept Prometheus where
  contentTypes _ =
    "text" // "plain" /: ("version", "0.0.4") /: ("charset", "utf-8")
      :| ["application" // "openmetrics-text" /: ("version", "0.0.1")]

instance MimeRender Prometheus Text where
  mimeRender _ t = BL.fromStrict (TE.encodeUtf8 t)

instance MimeUnrender Prometheus Text where
  mimeUnrender _ bs = left show (TE.decodeUtf8' (BL.toStrict bs))

type MetricsApi = "metrics" :> Get '[Prometheus] Text

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
    _health :: mode :- HealthApi,
    -- | 'GET /metrics'
    _metrics :: mode :- MetricsApi
  }
  deriving stock (Generic)

-- | servant-openapi3 does not (yet) support NamedRoutes so we need to compose the
-- API the old-fashioned way using :<|> for use by @toOpenApi@
type Api = CapabilitiesApi :<|> SchemaApi :<|> QueryApi :<|> HealthApi :<|> MetricsApi

-- | Provide an OpenApi 3.0 schema for the API
openApiSchema :: OpenApi
openApiSchema = toOpenApi (Proxy @Api)

apiClient :: Client ClientM (NamedRoutes Routes)
apiClient =
  client (Proxy @(NamedRoutes Routes))
