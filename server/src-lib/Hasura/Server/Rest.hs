module Hasura.Server.Rest
  ( runCustomEndpoint,
    RestRequest (..),
  )
where

import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson hiding (json)
import Data.Aeson qualified as J
import Data.Align qualified as Align
import Data.Environment qualified as Env
import Data.HashMap.Strict.Extended qualified as HashMap
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Extended
import Data.These (These (..))
import Hasura.Backends.DataConnector.Agent.Client (AgentLicenseKey)
import Hasura.Base.Error
import Hasura.CredentialCache
import Hasura.EncJSON
import Hasura.GraphQL.Execute qualified as E
import Hasura.GraphQL.Logging (MonadExecutionLog, MonadQueryLog)
import Hasura.GraphQL.ParameterizedQueryHash
import Hasura.GraphQL.Parser.Name qualified as GName
import Hasura.GraphQL.Transport.HTTP qualified as GH
import Hasura.GraphQL.Transport.HTTP.Protocol
import Hasura.HTTP
import Hasura.Logging qualified as L
import Hasura.Metadata.Class
import Hasura.Prelude
import Hasura.QueryTags
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Endpoint
import Hasura.RQL.Types.QueryCollection
import Hasura.RQL.Types.SchemaCache
import Hasura.Server.Init qualified as Init
import Hasura.Server.Limits
import Hasura.Server.Logging
import Hasura.Server.Name qualified as Name
import Hasura.Server.Prometheus (PrometheusMetrics)
import Hasura.Server.Types
import Hasura.Services
import Hasura.Session
import Hasura.Tracing qualified as Tracing
import Language.GraphQL.Draft.Syntax qualified as G
import Network.HTTP.Types qualified as HTTP
import Network.Wai.Extended qualified as Wai

-- Note: There may be a better way of constructing this when building the Endpoint datastructure.
parseVariableNames :: EndpointMetadata GQLQueryWithText -> [Text]
parseVariableNames queryx =
  mapMaybe (T.stripPrefix ":") $ T.split (== '/') (toTxt $ _ceUrl queryx)

-- Given a list of expected variables and the parsed vars from the path,
-- return a map of variable names to `These expected parsed`.
alignVars :: [G.VariableDefinition] -> [(Text, Either Text Value)] -> HashMap G.Name (These G.VariableDefinition (Either Text Value))
alignVars defVars parseVars =
  Align.align
    (HashMap.fromList (map (\v -> (G._vdName v, v)) defVars))
    (HashMap.fromList (mapMaybe (\(k, v) -> (,v) <$> G.mkName k) parseVars))

-- | `resolveVar` is responsible for decoding variables sent via REST request.
-- These can either be via body (represented by Right) or via query-param or URL param (represented by Left).
-- A variable can be expected, unexpected, or missing (represented by These, This, and That).
resolveVar :: G.Name -> These G.VariableDefinition (Either Text J.Value) -> Either Text (Maybe Value)
resolveVar _ (This _expectedVar) = Right Nothing -- If a variable is expected but missing, assign a missing value `Nothing` to it for resolution in query execution. This allows Null defaulting.
resolveVar varName (That _providedVar) = Left $ "Unexpected variable " <> toTxt @G.Name varName -- If a variable is unexpected but present, throw an error.
resolveVar _varName (These _expectedVar (Right bodyVar)) = Right (Just bodyVar) -- Variables sent via body can be passed through to execution without parsing.
resolveVar varName (These expectedVar (Left l)) =
  case G._vdType expectedVar of
    G.TypeList _ _ -> Left $ "List variables are not currently supported in URL or Query parameters. (Variable " <> toTxt @G.Name varName <> ", with value " <> tshow l <> ")"
    G.TypeNamed (G.Nullability nullable) typeName
      | typeName == GName._Boolean && T.null l -> Right $ Just $ J.Bool True -- Booleans indicated true by a standalone key.
      | nullable && T.null l -> Right Nothing -- Missing value, but nullable variable sets value to null.
      | otherwise -> case J.decodeStrict (T.encodeUtf8 l) of -- We special case parsing of bools and numbers and pass the rest through as literal strings.
          Just v@(J.Bool _) | typeName `elem` [Name._Bool, GName._Boolean] -> Right $ Just v
          Just v@(J.Number _) | typeName `elem` [GName._Int, GName._Float, Name._Number, Name._Double, Name._float8, Name._numeric] -> Right $ Just v
          _ -> Right $ Just $ J.String l

mkPassthroughRequest :: EndpointMetadata GQLQueryWithText -> VariableValues -> GQLReq GQLQueryText
mkPassthroughRequest queryx resolvedVariables =
  GQLReq
    Nothing
    (GQLQueryText $ getGQLQueryText (_edQuery (_ceDefinition queryx)))
    (Just resolvedVariables)

data RestRequest method = RestRequest
  { -- | Remainder of the url path after `api/rest`
    reqPath :: Text,
    reqMethod :: method, -- EndpointMethod

    -- | URL Query/Request Body Arguments
    reqArgs :: [(Text, Either Text J.Value)]
  }
  deriving (Functor, Foldable, Traversable)

-- | Implements all the custom endpoints by looking up the
-- path/methods in the endpoint trie and delegating to the graphql
-- handler.
runCustomEndpoint ::
  forall m.
  ( MonadIO m,
    MonadError QErr m,
    Tracing.MonadTrace m,
    MonadBaseControl IO m,
    E.MonadGQLExecutionCheck m,
    MonadQueryLog m,
    MonadExecutionLog m,
    GH.MonadExecuteQuery m,
    MonadMetadataStorage m,
    MonadQueryTags m,
    HasResourceLimits m,
    ProvidesNetwork m,
    MonadGetPolicies m
  ) =>
  Env.Environment ->
  SQLGenCtx ->
  SchemaCache ->
  Init.AllowListStatus ->
  ReadOnlyMode ->
  PrometheusMetrics ->
  L.Logger L.Hasura ->
  Maybe (CredentialCache AgentLicenseKey) ->
  RequestId ->
  UserInfo ->
  [HTTP.Header] ->
  Wai.IpAddress ->
  RestRequest EndpointMethod ->
  EndpointTrie GQLQueryWithText ->
  m (HttpLogGraphQLInfo, HttpResponse EncJSON)
runCustomEndpoint env sqlGenCtx sc enableAL readOnlyMode prometheusMetrics logger agentLicenseKey requestId userInfo reqHeaders ipAddress RestRequest {..} endpoints = do
  -- First match the path to an endpoint.
  case matchPath reqMethod (T.split (== '/') reqPath) endpoints of
    MatchFound (queryx :: EndpointMetadata GQLQueryWithText) matches ->
      let definitions =
            queryx
              & _ceDefinition
              & _edQuery
              & getGQLQuery
              & unGQLQuery
              & G.getExecutableDefinitions
       in -- Next, pattern match on the query definition to extract the
          -- (hopefully single) ExecutableDefinitionOperation structure, so that
          -- we can get hold of the list of query variables.
          case definitions of
            [G.ExecutableDefinitionOperation (G.OperationDefinitionTyped typedDef)] -> do
              -- Perform a join between the expected variables and the provided variables.
              -- If there is a mismatch, throw an error. Also, check that the provided
              -- values are compatible with the expected types.
              let expectedVariables = G._todVariableDefinitions typedDef
              let joinedVars = HashMap.traverseWithKey resolveVar (alignVars expectedVariables (reqArgs ++ zip (parseVariableNames queryx) (map Left matches)))

              resolvedVariablesMaybe <- joinedVars `onLeft` throw400 BadRequest

              let resolvedVariables = catMaybes resolvedVariablesMaybe

              -- Construct a graphql query by pairing the resolved variables
              -- with the query string from the schema cache, and pass it
              -- through to the /v1/graphql endpoint.
              (httpLoggingMetadata, handlerResp) <- do
                (gqlOperationLog, resp) <- GH.runGQ env sqlGenCtx sc enableAL readOnlyMode prometheusMetrics logger agentLicenseKey requestId userInfo ipAddress reqHeaders E.QueryHasura (mkPassthroughRequest queryx resolvedVariables)
                let httpLoggingGQInfo = (CommonHttpLogMetadata RequestModeNonBatchable Nothing, (PQHSetSingleton (gqolParameterizedQueryHash gqlOperationLog)))
                return (httpLoggingGQInfo, fst <$> resp)
              case sequence handlerResp of
                Just resp -> pure (httpLoggingMetadata, fmap encodeHTTPResp resp)
                -- a Nothing value here indicates a failure to parse the cached request from redis.
                -- TODO: Do we need an additional log message here?
                Nothing -> throw500 "An unexpected error occurred while fetching the data from the cache"

            -- Note: This fallthrough is required for runtime scenarios where the endpoint is ambiguous, such as:
            --       Endpoints /:a/b + /a/:b = Request /a/b - Invalid, but checked at runtime.
            _ -> throw500 "A stored query should contain exactly one definition"
    MatchNotFound -> throw404 "Endpoint not found"
    MatchMissingKey allowedMethods -> throw405 $ "Allowed methods: " <> commaSeparated allowedMethods
    MatchAmbiguous -> throw500 "Multiple endpoints match request"
