{-# LANGUAGE ScopedTypeVariables #-}

--
module Hasura.Backends.DataConnector.API
  ( module V0,
    Api,
    CapabilitiesResponses,
    QueryResponses,
    MutationResponses,
    SchemaGetApi,
    SchemaPostApi,
    SchemaResponses,
    QueryApi,
    ConfigHeader,
    Prometheus,
    SourceNameHeader,
    SourceName,
    capabilitiesCase,
    schemaCase,
    queryCase,
    mutationCase,
    openApiSchema,
    RoutesG (..),
    Routes,
    DatasetRoutes (..),
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
import Prelude

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

type SchemaGetApi config =
  "schema"
    :> SourceNameHeader Required
    :> ConfigHeader config Required
    :> UVerb 'GET '[JSON] SchemaResponses

type SchemaPostApi config =
  "schema"
    :> SourceNameHeader Required
    :> ConfigHeader config Required
    :> ReqBody '[JSON] V0.SchemaRequest
    :> UVerb 'POST '[JSON] SchemaResponses

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

-- | This function defines a central place to ensure that all cases are covered for mutation and error responses.
--   When additional responses are added to the Union, this should be updated to ensure that all responses have been considered.
mutationCase :: a -> (MutationResponse -> a) -> (ErrorResponse -> a) -> Union MutationResponses -> a
mutationCase defaultAction mutationAction errorAction union = do
  let mutationM = matchUnion @MutationResponse union
  let errorM = matchUnion @ErrorResponse union
  let errorM400 = matchUnion @ErrorResponse400 union
  case (mutationM, errorM, errorM400) of
    (Nothing, Nothing, Nothing) -> defaultAction
    (Just c, _, _) -> mutationAction c
    (_, Just e, _) -> errorAction e
    (_, _, Just (WithStatus e)) -> errorAction e

type MutationResponses = '[V0.MutationResponse, V0.ErrorResponse, V0.ErrorResponse400]

type QueryApi config =
  "query"
    :> SourceNameHeader Required
    :> ConfigHeader config Required
    :> ReqBody '[JSON] V0.QueryRequest
    :> UVerb 'POST '[JSON] QueryResponses

type ExplainApi config =
  "explain"
    :> SourceNameHeader Required
    :> ConfigHeader config Required
    :> ReqBody '[JSON] V0.QueryRequest
    :> Post '[JSON] V0.ExplainResponse

type MutationApi config =
  "mutation"
    :> SourceNameHeader Required
    :> ConfigHeader config Required
    :> ReqBody '[JSON] V0.MutationRequest
    :> UVerb 'POST '[JSON] MutationResponses

type HealthApi config =
  "health"
    :> SourceNameHeader Optional
    :> ConfigHeader config Optional
    :> GetNoContent

type RawApi config =
  "raw"
    :> SourceNameHeader Required
    :> ConfigHeader config Required
    :> ReqBody '[JSON] V0.RawRequest
    :> Post '[JSON] V0.RawResponse

type DatasetGetTemplateApi =
  "datasets"
    :> "templates"
    :> Capture "template_name" DatasetTemplateName
    :> Get '[JSON] V0.DatasetGetTemplateResponse

type DatasetCreateCloneApi =
  "datasets"
    :> "clones"
    :> Capture "clone_name" DatasetCloneName
    :> ReqBody '[JSON] V0.DatasetCreateCloneRequest
    :> Post '[JSON] V0.DatasetCreateCloneResponse

type DatasetDeleteCloneApi =
  "datasets"
    :> "clones"
    :> Capture "clone_name" DatasetCloneName
    :> Delete '[JSON] V0.DatasetDeleteCloneResponse

type DatasetApi = DatasetGetTemplateApi :<|> DatasetCreateCloneApi :<|> DatasetDeleteCloneApi

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

type ConfigHeader config optionality = Header' '[optionality, Strict] "X-Hasura-DataConnector-Config" config

type SourceNameHeader optionality = Header' '[optionality, Strict] "X-Hasura-DataConnector-SourceName" SourceName

type SourceName = Text

data RoutesG config mode = Routes
  { -- | 'GET /capabilities'
    _capabilities :: mode :- CapabilitiesApi,
    -- | 'GET /schema'
    _schemaGet :: mode :- SchemaGetApi config,
    _schemaPost :: mode :- SchemaPostApi config,
    -- | 'POST /query'
    _query :: mode :- QueryApi config,
    -- | 'POST /explain'
    _explain :: mode :- ExplainApi config,
    -- | 'POST /mutation'
    _mutation :: mode :- MutationApi config,
    -- | 'GET /health'
    _health :: mode :- HealthApi config,
    -- | 'GET /metrics'
    _metrics :: mode :- MetricsApi,
    -- | 'GET /raw'
    _raw :: mode :- RawApi config,
    -- | '/datasets'
    _datasets :: mode :- NamedRoutes DatasetRoutes
  }
  deriving stock (Generic)

data DatasetRoutes mode = DatasetRoutes
  { -- | 'GET /datasets/templates/:template_name'
    _getTemplate :: mode :- DatasetGetTemplateApi,
    -- | 'POST /datasets/clones/:clone_name'
    _createClone :: mode :- DatasetCreateCloneApi,
    -- | 'DELETE /datasets/clones/:clone_name'
    _deleteClone :: mode :- DatasetDeleteCloneApi
  }
  deriving stock (Generic)

type Routes = RoutesG V0.Config

-- | servant-openapi3 does not (yet) support NamedRoutes so we need to compose the
-- API the old-fashioned way using :<|> for use by @toOpenApi@
type ApiG config =
  CapabilitiesApi
    :<|> SchemaGetApi config
    :<|> SchemaPostApi config
    :<|> QueryApi config
    :<|> ExplainApi config
    :<|> MutationApi config
    :<|> HealthApi config
    :<|> MetricsApi
    :<|> RawApi config
    :<|> DatasetApi

type Api = ApiG V0.Config

-- | Provide an OpenApi 3.0 schema for the API
openApiSchema :: OpenApi
openApiSchema = toOpenApi (Proxy @Api)

apiClient :: Client ClientM (NamedRoutes Routes)
apiClient =
  client (Proxy @(NamedRoutes Routes))
