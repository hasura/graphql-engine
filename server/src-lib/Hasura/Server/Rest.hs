module Hasura.Server.Rest
  ( runCustomEndpoint
  , RestRequest(..)
  ) where

import           Hasura.Prelude                         hiding (get, put)

import qualified Data.Aeson                             as J
import qualified Data.Align                             as Align
import qualified Data.Environment                       as Env
import qualified Data.HashMap.Strict                    as M
import qualified Data.Text                              as T
import qualified Data.Text.Encoding                     as T
import qualified Network.HTTP.Types                     as HTTP
import qualified Network.Wai.Extended                   as Wai

import           Control.Monad.Trans.Control            (MonadBaseControl)
import           Data.Aeson                             hiding (json)
import           Data.Text.Extended

import qualified Hasura.GraphQL.Execute                 as E
import qualified Hasura.GraphQL.Transport.HTTP          as GH
import qualified Hasura.Tracing                         as Tracing
import qualified Language.GraphQL.Draft.Syntax          as G

import           Hasura.EncJSON
import           Hasura.GraphQL.Logging                 (MonadQueryLog)
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.HTTP
import           Hasura.Metadata.Class
import           Hasura.RQL.Types
import           Hasura.Server.Logging                  (HttpLog (..))
import           Hasura.Server.Types
import           Hasura.Server.Version
import           Hasura.Session


-- Note: There may be a better way of constructing this when building the Endpoint datastructure.
parseVariableNames :: EndpointMetadata GQLQueryWithText -> [Text]
parseVariableNames queryx =
    mapMaybe (T.stripPrefix ":") $ T.split (=='/') (toTxt $ _ceUrl queryx)

-- Given a list of expected variables and the parsed vars from the path,
-- return a map of variable names to `These expected parsed`.
alignVars :: [G.VariableDefinition] -> [(Text, Either Text Value)] -> HashMap G.Name (These G.VariableDefinition (Either Text Value))
alignVars defVars parseVars =
  Align.align
    (M.fromList (map (\v -> (G._vdName v, v)) defVars))
    (M.fromList (mapMaybe (\(k,v) -> (, v) <$> G.mkName k) parseVars))

resolveVar :: G.Name -> These G.VariableDefinition (Either Text J.Value) -> Either Text (Maybe Value)
resolveVar _ (This _expectedVar) = Right Nothing
resolveVar varName (That _providedVar) = Left $ "Unexpected variable " <> toTxt @G.Name varName
resolveVar varName (These expectedVar providedVar) =
  -- TODO: See CustomTypes.hs for SCALAR types
  case G._vdType expectedVar of
    G.TypeNamed (G.Nullability _) typeName -> case providedVar of
      Right r -> Right (Just r)
      Left l
        | typeName == boolScalar && T.null l -> Right $ Just $ J.Bool True -- Key present but value missing for bools defaults to True.
        | typeName == stringScalar           -> Right $ Just $ J.String l -- Note: Strings don't need to be decoded since the format already matches.
        | typeName == $$(G.litName "UUID")   -> Right $ Just $ J.String l -- TODO: Handle UUIDs - Is this litName correct?
        | typeName == idScalar               -> Right $ Just $ J.String l
        | otherwise -> case J.decodeStrict (T.encodeUtf8 l) of
          (Just    J.Null     ) | typeName == $$(G.litName "Null")   -> pure $ Just J.Null
                                | otherwise                          -> Left $ "Expected " <> toTxt typeName <> " got Null"
          (Just x@(J.Bool   _)) | typeName == boolScalar             -> pure $ Just x
                                | typeName == $$(G.litName "Bool")   -> pure $ Just x
                                | otherwise                          -> Left $ "Expected " <> toTxt typeName <> " got Bool"
          (Just x@(J.Number _)) | typeName == $$(G.litName "Number") -> pure $ Just x -- TODO: Check other numeric types
                                | typeName == intScalar              -> pure $ Just x
                                | typeName == floatScalar            -> pure $ Just x
                                | typeName == $$(G.litName "Double") -> pure $ Just x
                                | otherwise                          -> Left $ "Expected " <> toTxt typeName <> " got Number"
          _ -> Left ("Type of URL parameter not supported - Consider putting it in the request body: " <> tshow l)

    -- TODO: This is a fallthrough case and is still required
    --       but we can move checks for template variables being
    --       scalars into the schema-cache construction.
    _ -> Left ("The variable type for the expected variable " <> toTxt @G.Name varName <> " was not supported.")

mkPassthroughRequest :: EndpointMetadata GQLQueryWithText -> VariableValues -> GQLReq GQLQueryText
mkPassthroughRequest queryx resolvedVariables =
  GQLReq
    Nothing
    (GQLQueryText $ getGQLQueryText (_edQuery (_ceDefinition queryx)))
    (Just resolvedVariables)

data RestRequest method = RestRequest
  { reqPath   :: Text
    -- ^ Remainder of the url path after `api/rest`
  , reqMethod :: method -- EndpointMethod
  , reqArgs   :: [(Text, Either Text J.Value)]
    -- ^ URL Query/Request Body Arguments
  } deriving (Functor, Foldable, Traversable)

-- | Implements all the custom endpoints by looking up the
-- path/methods in the endpoint trie and delegating to the graphql
-- handler.
runCustomEndpoint
    :: forall m
     . ( HasVersion
       , MonadIO m
       , MonadError QErr m
       , Tracing.MonadTrace m
       , MonadBaseControl IO m
       , E.MonadGQLExecutionCheck m
       , MonadQueryLog m
       , GH.MonadExecuteQuery m
       , MonadMetadataStorage (MetadataStorageT m)
       , HttpLog m
       )
    => Env.Environment
    -> E.ExecutionCtx
    -> RequestId
    -> UserInfo
    -> [HTTP.Header]
    -> Wai.IpAddress
    -> RestRequest EndpointMethod
    -> EndpointTrie GQLQueryWithText
    -> m (HTTPLoggingMetadata m, HttpResponse EncJSON)
runCustomEndpoint env execCtx requestId userInfo reqHeaders ipAddress RestRequest{..} endpoints = do
  -- First match the path to an endpoint.
  case matchPath reqMethod (T.split (== '/') reqPath) endpoints of
    MatchFound (queryx :: EndpointMetadata GQLQueryWithText) matches ->
      let definitions = queryx
            & _ceDefinition
            & _edQuery
            & getGQLQuery
            & unGQLQuery
            & G.getExecutableDefinitions

      -- Next, pattern match on the query definition to extract the
      -- (hopefully single) ExecutableDefinitionOperation structure, so that
      -- we can get hold of the list of query variables.
      in case definitions of
        [G.ExecutableDefinitionOperation (G.OperationDefinitionTyped typedDef)] -> do
          -- Perform a join between the expected variables and the provided variables.
          -- If there is a mismatch, throw an error. Also, check that the provided
          -- values are compatible with the expected types.
          let expectedVariables = G._todVariableDefinitions typedDef
          let joinedVars = M.traverseWithKey resolveVar (alignVars expectedVariables (reqArgs ++ zip (parseVariableNames queryx) (map Left matches)))

          resolvedVariablesMaybe <- joinedVars `onLeft` throw400 BadRequest

          let resolvedVariables = M.mapMaybe id resolvedVariablesMaybe

          -- Construct a graphql query by pairing the resolved variables
          -- with the query string from the schema cache, and pass it
          -- through to the /v1/graphql endpoint.
          (httpLoggingMetadata, handlerResp) <- flip runReaderT execCtx $ do
              (normalizedSelectionSet, resp) <- GH.runGQ env (E._ecxLogger execCtx) requestId userInfo ipAddress reqHeaders E.QueryHasura (mkPassthroughRequest queryx resolvedVariables)
              let httpLoggingMetadata = buildHTTPLoggingMetadata @m [normalizedSelectionSet]
              return (httpLoggingMetadata, fst <$> resp)
          case sequence handlerResp of
            Just resp -> pure $ (httpLoggingMetadata, fmap encodeHTTPResp resp)
            -- a Nothing value here indicates a failure to parse the cached request from redis.
            -- TODO: Do we need an additional log message here?
            Nothing -> throw500 "An unexpected error occurred while fetching the data from the cache"

        -- Note: This fallthrough is required for runtime scenarios where the endpoint is ambiguous, such as:
        --       Endpoints /:a/b + /a/:b = Request /a/b - Invalid, but checked at runtime.
        _ -> throw500 "A stored query should contain exactly one definition"
    MatchNotFound -> throw404 "Endpoint not found"
    MatchMissingKey allowedMethods -> throw405 $ "Allowed methods: " <> commaSeparated allowedMethods
    MatchAmbiguous -> throw500 "Multiple endpoints match request"
