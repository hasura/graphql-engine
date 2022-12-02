{-# LANGUAGE ScopedTypeVariables #-}

--
module Hasura.Backends.DataConnector.API
  ( module V0,
    Api,
    CapabilitiesResponses,
    QueryResponses,
    MutationResponses,
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
import Data.Foldable (for_)
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
  let errorM400 = matchUnion @ErrorResponse400 union
  case (capabilitiesM, errorM, errorM400) of
    (Nothing, Nothing, Nothing) -> defaultAction
    (Just c, _, _) -> capabilitiesAction c
    (_, Just e, _) -> errorAction e
    (_, _, Just (WithStatus e)) -> errorAction e

type CapabilitiesResponses = '[V0.CapabilitiesResponse, V0.ErrorResponse, V0.ErrorResponse400]

type CapabilitiesApi =
  "capabilities"
    :> UVerb 'GET '[JSON] CapabilitiesResponses

-- | This function defines a central place to ensure that all cases are covered for schema and error responses.
--   When additional responses are added to the Union, this should be updated to ensure that all responses have been considered.
schemaCase :: a -> (SchemaResponse -> a) -> (ErrorResponse -> a) -> Union SchemaResponses -> a
schemaCase defaultAction schemaAction errorAction union = do
  let schemaM = matchUnion @SchemaResponse union
  let errorM = matchUnion @ErrorResponse union
  let errorM400 = matchUnion @ErrorResponse400 union
  case (schemaM, errorM, errorM400) of
    -- Note, this could technically include the ...Just _, Just _... scenario, but won't occurr due to matchUnion
    (Nothing, Nothing, Nothing) -> defaultAction
    (Just x, _, _) -> schemaAction x
    (_, Just x, _) -> errorAction x
    (_, _, Just (WithStatus x)) -> errorAction x

type SchemaResponses = '[V0.SchemaResponse, V0.ErrorResponse, V0.ErrorResponse400]

type SchemaApi =
  "schema"
    :> SourceNameHeader Required
    :> ConfigHeader Required
    :> UVerb 'GET '[JSON] SchemaResponses

-- | This function defines a central place to ensure that all cases are covered for query and error responses.
--   When additional responses are added to the Union, this should be updated to ensure that all responses have been considered.
queryCase :: a -> (QueryResponse -> a) -> (ErrorResponse -> a) -> Union QueryResponses -> a
queryCase defaultAction queryAction errorAction union = do
  let queryM = matchUnion @QueryResponse union
  let errorM = matchUnion @ErrorResponse union
  let errorM400 = matchUnion @ErrorResponse400 union
  case (queryM, errorM, errorM400) of
    (Nothing, Nothing, Nothing) -> defaultAction
    (Just c, _, _) -> queryAction c
    (_, Just e, _) -> errorAction e
    (_, _, Just (WithStatus e)) -> errorAction e

type QueryResponses = '[V0.QueryResponse, V0.ErrorResponse, V0.ErrorResponse400]

type MutationResponses = '[V0.MutationResponse, V0.ErrorResponse, V0.ErrorResponse400]

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

type MutationApi =
  "mutation"
    :> SourceNameHeader Required
    :> ConfigHeader Required
    :> ReqBody '[JSON] V0.MutationRequest
    :> UVerb 'POST '[JSON] MutationResponses

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
    ("text" // "plain" /: ("version", "0.0.4") /: ("charset", "utf-8"))
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
    -- | 'POST /mutation'
    _mutation :: mode :- MutationApi,
    -- | 'GET /health'
    _health :: mode :- HealthApi,
    -- | 'GET /metrics'
    _metrics :: mode :- MetricsApi,
    -- | 'GET /raw'
    _raw :: mode :- RawApi
  }
  deriving stock (Generic)

-- | servant-openapi3 does not (yet) support NamedRoutes so we need to compose the
-- API the old-fashioned way using :<|> for use by @toOpenApi@
type Api = CapabilitiesApi :<|> SchemaApi :<|> QueryApi :<|> ExplainApi :<|> MutationApi :<|> HealthApi :<|> MetricsApi :<|> RawApi

-- | Provide an OpenApi 3.0 schema for the API
openApiSchema :: OpenApi
openApiSchema = toOpenApi (Proxy @Api)

apiClient :: Client ClientM (NamedRoutes Routes)
apiClient =
  client (Proxy @(NamedRoutes Routes))
