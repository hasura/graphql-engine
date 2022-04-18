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
import Data.HashMap.Strict.Extended qualified as M
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Extended
import Data.These (These (..))
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.GraphQL.Execute qualified as E
import Hasura.GraphQL.Execute.Backend qualified as EB
import Hasura.GraphQL.Logging (MonadQueryLog)
import Hasura.GraphQL.ParameterizedQueryHash (ParameterizedQueryHashList (..))
import Hasura.GraphQL.Parser.Constants qualified as G
import Hasura.GraphQL.Transport.HTTP qualified as GH
import Hasura.GraphQL.Transport.HTTP.Protocol
import Hasura.HTTP
import Hasura.Metadata.Class
import Hasura.Prelude hiding (get, put)
import Hasura.RQL.Types
import Hasura.RQL.Types.Endpoint
import Hasura.Server.Limits
import Hasura.Server.Logging
import Hasura.Server.Types
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
    (M.fromList (map (\v -> (G._vdName v, v)) defVars))
    (M.fromList (mapMaybe (\(k, v) -> (,v) <$> G.mkName k) parseVars))

resolveVar :: G.Name -> These G.VariableDefinition (Either Text J.Value) -> Either Text (Maybe Value)
resolveVar _ (This _expectedVar) = Right Nothing
resolveVar varName (That _providedVar) = Left $ "Unexpected variable " <> toTxt @G.Name varName
resolveVar varName (These expectedVar providedVar) =
  -- TODO: See CustomTypes.hs for SCALAR types
  case G._vdType expectedVar of
    G.TypeNamed (G.Nullability nullable) typeName -> case providedVar of
      Right r -> Right (Just r)
      Left l
        | typeName == stringScalar -> Right $ Just $ J.String l -- "String" -- Note: Strings don't need to be decoded since the format already matches.
        | otherwise ->
          case (J.decodeStrict (T.encodeUtf8 l), nullable) of
            (Just J.Null, True) -> pure Nothing
            (decoded, _)
              | typeName == boolScalar && T.null l -> Right $ Just $ J.Bool True -- Key present but value missing for bools defaults to True.
              | typeName == G._UUID -> Right $ Just $ J.String l
              | typeName == G._uuid -> Right $ Just $ J.String l
              | typeName == idScalar -> Right $ Just $ J.String l -- "ID" -- Note: Console doesn't expose this as a column type.
              | otherwise -> case decoded of
                (Just J.Null) -> Left $ "Null or missing value for non-nullable variable: " <> G.unName varName
                (Just x@(J.Bool _))
                  | typeName == boolScalar -> pure $ Just x -- "Boolean"
                  | typeName == G._Bool -> pure $ Just x
                  | otherwise -> Left $ "Expected " <> toTxt typeName <> " for variable " <> G.unName varName <> " got Bool"
                (Just x@(J.Number _))
                  | typeName == intScalar -> pure $ Just x -- "Int"
                  | typeName == floatScalar -> pure $ Just x -- "Float"
                  | typeName == G._Number -> pure $ Just x
                  | typeName == G._Double -> pure $ Just x
                  | typeName == G._float8 -> pure $ Just x
                  | typeName == G._numeric -> pure $ Just x
                  | otherwise -> Left $ "Expected " <> toTxt typeName <> " for variable " <> G.unName varName <> " got Number"
                _ -> Left ("Type of URL parameter for variable " <> G.unName varName <> " not supported - Consider putting it in the request body: " <> tshow l)
    -- TODO: This is a fallthrough case and is still required
    --       but we can move checks for template variables being
    --       scalars into the schema-cache construction.
    G.TypeList _ _ -> case providedVar of
      Right r -> Right (Just r)
      Left l -> Left ("The variable type for the expected variable " <> toTxt @G.Name varName <> ", with value " <> tshow l <> " is not supported.")

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
    GH.MonadExecuteQuery m,
    MonadMetadataStorage (MetadataStorageT m),
    HttpLog m,
    EB.MonadQueryTags m,
    HasResourceLimits m
  ) =>
  Env.Environment ->
  E.ExecutionCtx ->
  RequestId ->
  UserInfo ->
  [HTTP.Header] ->
  Wai.IpAddress ->
  RestRequest EndpointMethod ->
  EndpointTrie GQLQueryWithText ->
  m (HttpLogMetadata m, HttpResponse EncJSON)
runCustomEndpoint env execCtx requestId userInfo reqHeaders ipAddress RestRequest {..} endpoints = do
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
              let joinedVars = M.traverseWithKey resolveVar (alignVars expectedVariables (reqArgs ++ zip (parseVariableNames queryx) (map Left matches)))

              resolvedVariablesMaybe <- joinedVars `onLeft` throw400 BadRequest

              let resolvedVariables = M.catMaybes resolvedVariablesMaybe

              -- Construct a graphql query by pairing the resolved variables
              -- with the query string from the schema cache, and pass it
              -- through to the /v1/graphql endpoint.
              (httpLoggingMetadata, handlerResp) <- flip runReaderT execCtx $ do
                (gqlOperationLog, resp) <- GH.runGQ env (E._ecxLogger execCtx) requestId userInfo ipAddress reqHeaders E.QueryHasura (mkPassthroughRequest queryx resolvedVariables)
                let httpLogMetadata =
                      buildHttpLogMetadata @m (PQHSetSingleton (gqolParameterizedQueryHash gqlOperationLog)) RequestModeNonBatchable Nothing
                return (httpLogMetadata, fst <$> resp)
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
