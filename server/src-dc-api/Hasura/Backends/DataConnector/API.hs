--
module Hasura.Backends.DataConnector.API
  ( module V0,
    Api,
    SchemaApi,
    QueryApi,
    Config (..),
    ConfigHeader,
    openApiSchema,
    Routes (..),
    apiClient,
  )
where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as J
import Data.ByteString.Lazy qualified as BSL
import Data.Data (Proxy (..))
import Data.Hashable (Hashable)
import Data.OpenApi (AdditionalProperties (..), OpenApi, OpenApiType (..), Schema (..), ToParamSchema (..))
import Data.Text.Encoding qualified as Text
import Hasura.Backends.DataConnector.API.V0.API as V0
import Servant.API
import Servant.API.Generic
import Servant.Client (Client, ClientM, client)
import Servant.OpenApi
import Prelude

--------------------------------------------------------------------------------
-- Servant Routes

type SchemaApi =
  "schema"
    :> ConfigHeader
    :> Get '[JSON] V0.SchemaResponse

type QueryApi =
  "query"
    :> ConfigHeader
    :> ReqBody '[JSON] V0.Query
    :> Post '[JSON] V0.QueryResponse

newtype Config = Config {unConfig :: J.Object}
  deriving stock (Eq, Show, Ord)
  deriving newtype (Hashable, NFData, ToJSON, FromJSON)

instance ToHttpApiData Config where
  toUrlPiece (Config val) = Text.decodeUtf8 . BSL.toStrict $ J.encode val
  toHeader (Config val) = BSL.toStrict $ J.encode val

instance ToParamSchema Config where
  toParamSchema _ =
    mempty
      { _schemaType = Just OpenApiObject,
        _schemaNullable = Just False,
        _schemaAdditionalProperties = Just (AdditionalPropertiesAllowed True)
      }

type ConfigHeader = Header' '[Required, Strict] "X-Hasura-DataConnector-Config" Config

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
