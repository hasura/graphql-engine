{-# LANGUAGE ScopedTypeVariables #-}

--
module Hasura.Backends.DataConnector.API
  ( module V0,
    Api,
    CapabilitiesResponses,
    QueryResponses,
    SchemaApi,
    SchemaResponses,
    QueryApi,
    ConfigHeader,
    Prometheus,
    SourceNameHeader,
    SourceName,
    capabilitiesCase,
    schemaCase,
    queryCase,
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
import Servant.Client (Client, ClientM, client, matchUnion)
import Servant.OpenApi
import Prelude (Maybe (Just, Nothing), Monad, show)

--------------------------------------------------------------------------------
-- Servant Routes

-- | This function defines a central place to ensure that all cases are covered for capabilities and error responses.
--   When additional responses are added to the Union, this should be updated to ensure that all responses have been considered.
--   A general function of this form doesn't seem easy to write currently as you need a type inequality with ErrorResponse.
capabilitiesCase :: a -> (CapabilitiesResponse -> a) -> (ErrorResponse -> a) -> Union CapabilitiesResponses -> a
capabilitiesCase defaultAction capabilitiesAction errorAction union = do
  let capabilitiesM = matchUnion @CapabilitiesResponse union
  let errorM = matchUnion @ErrorResponse union
  case (capabilitiesM, errorM) of
    (Just c, Nothing) -> capabilitiesAction c
    (Nothing, Just e) -> errorAction e
    _ -> defaultAction -- Note, this could technically include the (Just _, Just _) scenario which is not possible.

type CapabilitiesResponses = '[V0.CapabilitiesResponse, V0.ErrorResponse]

type CapabilitiesApi =
  "capabilities"
    :> UVerb 'GET '[JSON] CapabilitiesResponses

-- | This function defines a central place to ensure that all cases are covered for schema and error responses.
--   When additional responses are added to the Union, this should be updated to ensure that all responses have been considered.
schemaCase :: Monad m => m a -> (SchemaResponse -> m a) -> (ErrorResponse -> m a) -> Union SchemaResponses -> m a
schemaCase defaultAction schemaAction errorAction union = do
  let schemaM = matchUnion @SchemaResponse union
  let errorM = matchUnion @ErrorResponse union
  case (schemaM, errorM) of
    (Just c, Nothing) -> schemaAction c
    (Nothing, Just e) -> errorAction e
    _ -> defaultAction -- Note, this could technically include the (Just _, Just _) scenario which is not possible.

type SchemaResponses = '[V0.SchemaResponse, V0.ErrorResponse]

type SchemaApi =
  "schema"
    :> SourceNameHeader Required
    :> ConfigHeader Required
    :> UVerb 'GET '[JSON] SchemaResponses

-- | This function defines a central place to ensure that all cases are covered for query and error responses.
--   When additional responses are added to the Union, this should be updated to ensure that all responses have been considered.
queryCase :: Monad m => m a -> (QueryResponse -> m a) -> (ErrorResponse -> m a) -> Union QueryResponses -> m a
queryCase defaultAction queryAction errorAction union = do
  let queryM = matchUnion @QueryResponse union
  let errorM = matchUnion @ErrorResponse union
  case (queryM, errorM) of
    (Just c, Nothing) -> queryAction c
    (Nothing, Just e) -> errorAction e
    _ -> defaultAction -- Note, this could technically include the (Just _, Just _) scenario which is not possible.

type QueryResponses = '[V0.QueryResponse, V0.ErrorResponse]

type QueryApi =
  "query"
    :> SourceNameHeader Required
    :> ConfigHeader Required
    :> ReqBody '[JSON] V0.QueryRequest
    :> UVerb 'POST '[JSON] QueryResponses

type ExplainApi =
  "explain"
    :> SourceNameHeader Required
    :> ConfigHeader Required
    :> ReqBody '[JSON] V0.QueryRequest
    :> Post '[JSON] V0.ExplainResponse

type HealthApi =
  "health"
    :> SourceNameHeader Optional
    :> ConfigHeader Optional
    :> GetNoContent

type RawApi =
  "raw"
    :> SourceNameHeader Required
    :> ConfigHeader Required
    :> ReqBody '[JSON] V0.RawRequest
    :> Post '[JSON] V0.RawResponse

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
    -- | 'POST /explain'
    _explain :: mode :- ExplainApi,
    -- | 'GET /health'
    _health :: mode :- HealthApi,
    -- | 'GET /metrics'
    _metrics :: mode :- MetricsApi,
    -- | 'GET /metrics'
    _raw :: mode :- RawApi
  }
  deriving stock (Generic)

-- | servant-openapi3 does not (yet) support NamedRoutes so we need to compose the
-- API the old-fashioned way using :<|> for use by @toOpenApi@
type Api = CapabilitiesApi :<|> SchemaApi :<|> QueryApi :<|> ExplainApi :<|> HealthApi :<|> MetricsApi :<|> RawApi

-- | Provide an OpenApi 3.0 schema for the API
openApiSchema :: OpenApi
openApiSchema = toOpenApi (Proxy @Api)

apiClient :: Client ClientM (NamedRoutes Routes)
apiClient =
  client (Proxy @(NamedRoutes Routes))
