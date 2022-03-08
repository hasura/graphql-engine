{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Webhook Transformations are data transformations used to modify
-- HTTP Requests/Responses before requests are executed and after
-- responses are received.
--
-- Transformations are supplied by users as part of the Metadata for a
-- particular Action or EventTrigger as a 'RequestTransform'
-- record. Per-field Transformations are stored as data
-- (defunctionalized), often in the form of a Kriti template, and then
-- converted into actual functions (reified) at runtime by the
-- 'Transform' typeclass.
--
-- We take a Higher Kinded Data (HKD) approach to representing the
-- transformations. 'RequestFields' is an HKD which can represent the
-- actual request data as 'RequestFields Identity' or the
-- defunctionalized transforms as 'RequestFields (WithOptional
-- TransformFn)'.
--
-- We can then traverse over the entire 'RequestFields' HKD to reify
-- all the fields at once and apply them to our actual request
-- data.
--
-- NOTE: We don't literally use 'traverse' or the HKD equivalent
-- 'btraverse', but you can think of this operation morally as a
-- traversal. See 'applyRequestTransform' for implementation details.
module Hasura.RQL.DDL.Webhook.Transform
  ( -- * Request Transformation
    RequestFields (..),
    RequestTransform (..),
    RequestTransformFn,
    applyRequestTransform,

    -- * Request Transformation Context
    RequestTransformCtx (..),
    TransformErrorBundle (..),

    -- * Optional Functor
    WithOptional (..),

    -- * Old Style Response Transforms
    MetadataResponseTransform (..),
    ResponseTransform (..),
    ResponseTransformCtx (..),
    applyResponseTransform,
    buildRespTransformCtx,
    mkResponseTransform,
  )
where

-------------------------------------------------------------------------------

import Control.Lens (Lens', lens, set, view)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as J
import Data.Bifunctor (first)
import Data.ByteString.Lazy qualified as BL
import Data.CaseInsensitive qualified as CI
import Data.Functor.Barbie
import Data.HashMap.Strict qualified as M
import Data.Text.Encoding qualified as TE
import Data.Validation qualified as V
import Hasura.Incremental (Cacheable)
import Hasura.Prelude hiding (first)
import Hasura.RQL.DDL.Webhook.Transform.Body
import Hasura.RQL.DDL.Webhook.Transform.Class
import Hasura.RQL.DDL.Webhook.Transform.Headers
import Hasura.RQL.DDL.Webhook.Transform.Method
import Hasura.RQL.DDL.Webhook.Transform.QueryParams
import Hasura.RQL.DDL.Webhook.Transform.Url
import Hasura.Session (SessionVariables, getSessionVariableValue, mkSessionVariable)
import Kriti qualified (runKriti)
import Kriti.Error qualified as Kriti (CustomFunctionError (CustomFunctionError), serialize)
import Network.HTTP.Client.Transformable qualified as HTTP

-------------------------------------------------------------------------------

-- | 'RequestTransform' is the metadata representation of a request
-- transformation. It consists of a record of higher kinded data (HKD)
-- along with some regular data. We seperate the HKD data into its own
-- record field called 'requestFields' which we nest inside our
-- non-HKD record. The actual transformation operations are contained
-- in the HKD.
data RequestTransform = RequestTransform
  { version :: Version,
    requestFields :: RequestFields (WithOptional TransformFn),
    templateEngine :: TemplatingEngine
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData, Cacheable)

instance ToJSON RequestTransform where
  toJSON RequestTransform {..} =
    let RequestFields {..} = requestFields
        body' = case version of
          V1 -> case (getOptional body) of
            Just (BodyTransform (ModifyBody template)) -> Just ("body", J.toJSON template)
            _ -> Nothing
          V2 -> "body" .=? getOptional body
     in J.object $
          [ "version" J..= version,
            "template_engine" J..= templateEngine
          ]
            <> catMaybes
              [ "method" .=? getOptional method,
                "url" .=? getOptional url,
                "query_params" .=? getOptional queryParams,
                "request_headers" .=? getOptional requestHeaders,
                body'
              ]

instance FromJSON RequestTransform where
  parseJSON = J.withObject "RequestTransform" \o -> do
    version <- o J..:? "version" J..!= V1
    method <- o J..:? "method"
    url <- o J..:? "url"
    body <- case version of
      V1 -> do
        template :: Maybe Template <- o J..:? "body"
        pure $ fmap ModifyBody template
      V2 -> o J..:? "body"
    queryParams <- o J..:? "query_params"
    headers <- o J..:? "request_headers"
    let requestFields =
          RequestFields
            { method = WithOptional $ fmap MethodTransform method,
              url = WithOptional $ fmap UrlTransform url,
              body = WithOptional $ fmap BodyTransform body,
              queryParams = WithOptional $ fmap QueryParamsTransform queryParams,
              requestHeaders = WithOptional $ fmap HeadersTransform headers
            }
    templateEngine <- o J..:? "template_engine" J..!= Kriti
    pure $ RequestTransform {..}

-------------------------------------------------------------------------------

-- | Defunctionalized Webhook Transformation
--
-- We represent a defunctionalized request transformation by parameterizing
-- our HKD with 'WithOptional'@ @'TransformFn', which marks each of the fields
-- as optional and supplies the appropriate transformation function to them if
-- if they are provided. 'WithOptional _' is equivalent to 'Compose Maybe _'.
type RequestTransformFn = RequestFields (WithOptional TransformFn)

-- | Actual Request Data
--
-- We represent the actual request data by parameterizing our HKD with
-- 'Identity', which allows us to trivially unwrap the fields (which should
-- exist after any transformations have been applied).
type RequestData = RequestFields Identity

-- | This is our HKD type. It is a record with fields for each
-- component of an 'HTTP.Request' we wish to transform.
data RequestFields f = RequestFields
  { method :: f Method,
    url :: f Url,
    body :: f Body,
    queryParams :: f QueryParams,
    requestHeaders :: f Headers
  }
  deriving stock (Generic)
  deriving anyclass (FunctorB, TraversableB, ApplicativeB, ConstraintsB)

deriving instance AllBF Show f RequestFields => Show (RequestFields f)

deriving instance AllBF Eq f RequestFields => Eq (RequestFields f)

deriving instance AllBF NFData f RequestFields => NFData (RequestFields f)

deriving instance AllBF Cacheable f RequestFields => Cacheable (RequestFields f)

-- NOTE: It is likely that we can derive these instances. Possibly if
-- we move the aeson instances onto the *Transform types.
instance FromJSON RequestTransformFn where
  parseJSON = J.withObject "RequestTransformFn" $ \o -> do
    method <- o J..:? "method"
    url <- o J..:? "url"
    body <- o J..:? "body"
    queryParams <- o J..:? "query_params"
    headers <- o J..:? "request_headers"
    pure $
      RequestFields
        { method = WithOptional $ fmap MethodTransform method,
          url = WithOptional $ fmap UrlTransform url,
          body = WithOptional $ fmap BodyTransform body,
          queryParams = WithOptional $ fmap QueryParamsTransform queryParams,
          requestHeaders = WithOptional $ fmap HeadersTransform headers
        }

instance ToJSON RequestTransformFn where
  toJSON RequestFields {..} =
    J.object . catMaybes $
      [ "method" .=? getOptional method,
        "url" .=? getOptional url,
        "body" .=? getOptional body,
        "query_params" .=? getOptional queryParams,
        "request_headers" .=? getOptional requestHeaders
      ]

-------------------------------------------------------------------------------

-- TODO(SOLOMON): Add lens law unit tests

-- | A lens for mapping from an actual 'HTTP.Request' term to our HKD.
request :: Lens' HTTP.Request RequestData
request = lens getter setter
  where
    getter :: HTTP.Request -> RequestData
    getter req =
      RequestFields
        { method = coerce $ CI.mk $ TE.decodeUtf8 $ view HTTP.method req,
          url = coerce $ view HTTP.url req,
          body = coerce $ JSONBody $ J.decode =<< view HTTP.body req,
          queryParams = coerce $ view HTTP.queryParams req,
          requestHeaders = coerce $ view HTTP.headers req
        }

    serializeBody :: Body -> Maybe BL.ByteString
    serializeBody = \case
      JSONBody body -> fmap J.encode body
      RawBody "" -> Nothing
      RawBody bs -> Just bs

    setter :: HTTP.Request -> RequestData -> HTTP.Request
    setter req RequestFields {..} =
      req & set HTTP.method (TE.encodeUtf8 $ CI.original $ coerce method)
        & set HTTP.body (serializeBody $ coerce body)
        & set HTTP.url (coerce url)
        & set HTTP.queryParams (unQueryParams $ coerce queryParams)
        & set HTTP.headers (coerce requestHeaders)

-- | Transform an 'HTTP.Request' with a 'RequestTransform'.
--
-- Note: we pass in the request url explicitly for use in the
-- 'ReqTransformCtx'. We do this so that we can ensure that the url
-- is syntactically identical to what the use submits. If we use the
-- parsed request from the 'HTTP.Request' term then it is possible
-- that the url is semantically equivalent but syntactically
-- different. An example of this is the presence or lack of a trailing
-- slash on the URL path. This important when performing string
-- interpolation on the request url.
applyRequestTransform ::
  MonadError TransformErrorBundle m =>
  (HTTP.Request -> RequestTransformCtx) ->
  RequestTransformFn ->
  HTTP.Request ->
  m HTTP.Request
applyRequestTransform mkCtx mt reqData =
  let ctx = mkCtx reqData
      f ::
        (MonadError TransformErrorBundle m, Transform a) =>
        a ->
        TransformFn a ->
        m a
      f a x = transform x ctx a
   in request (bsequence' . bzipWithC @Transform (\(WithOptional mx) (Identity a) -> maybe (pure a) (f a) mx) mt) reqData

-------------------------------------------------------------------------------

infixr 8 .=?

(.=?) :: J.ToJSON v => k -> Maybe v -> Maybe (k, J.Value)
(.=?) k = fmap ((k,) . J.toJSON)

-- | 'Data.Functor.WithOptional' has 'Eq' and 'Show' instances with
-- difficult to satisfy constraints. 'WithOptional' is isomorphic to
-- 'Compose Maybe f'.
--
-- TODO(jkachmar): Add better documentation now that we've specialized this
-- type.
newtype WithOptional fn result = WithOptional
  { getOptional :: Maybe (fn result)
  }
  deriving stock (Eq, Functor, Generic, Show)
  deriving newtype (FromJSON, ToJSON)

deriving newtype instance
  (Cacheable (fn result)) =>
  Cacheable (WithOptional fn result)

deriving newtype instance
  (NFData (fn result)) =>
  NFData (WithOptional fn result)

-------------------------------------------------------------------------------
-- TODO(SOLOMON): Rewrite with HKD

-- | A set of data transformation functions generated from a
-- 'MetadataResponseTransform'. 'Nothing' means use the original
-- response value.
data ResponseTransform = ResponseTransform
  { respTransformBody :: Maybe (ResponseTransformCtx -> Either TransformErrorBundle J.Value),
    respTransformTemplateEngine :: TemplatingEngine
  }

data MetadataResponseTransform = MetadataResponseTransform
  { mrtVersion :: Version,
    mrtBodyTransform :: Maybe BodyTransformAction,
    mrtTemplatingEngine :: TemplatingEngine
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData, Cacheable)

instance J.ToJSON MetadataResponseTransform where
  toJSON MetadataResponseTransform {..} =
    let body = case mrtVersion of
          V1 -> case mrtBodyTransform of
            Just (ModifyBody template) -> Just ("body", J.toJSON template)
            _ -> Nothing
          V2 -> "body" .=? mrtBodyTransform
     in J.object $
          ["template_engine" J..= mrtTemplatingEngine, "version" J..= mrtVersion]
            <> catMaybes [body]

instance J.FromJSON MetadataResponseTransform where
  parseJSON = J.withObject "Object" $ \o -> do
    mrtVersion <- o J..:? "version" J..!= V1
    mrtBodyTransform <- case mrtVersion of
      V1 -> do
        template :: (Maybe Template) <- o J..:? "body"
        pure $ fmap ModifyBody template
      V2 -> o J..:? "body"
    templateEngine <- o J..:? "template_engine"
    let mrtTemplatingEngine = fromMaybe Kriti templateEngine
    pure $ MetadataResponseTransform {..}

-- | A helper function for constructing the 'RespTransformCtx'
buildRespTransformCtx :: Maybe RequestTransformCtx -> Maybe SessionVariables -> TemplatingEngine -> BL.ByteString -> ResponseTransformCtx
buildRespTransformCtx reqCtx sessionVars engine respBody =
  ResponseTransformCtx
    { responseTransformBody = fromMaybe J.Null $ J.decode @J.Value respBody,
      responseTransformReqCtx = J.toJSON reqCtx,
      responseTransformEngine = engine,
      responseTransformFunctions = M.singleton "getSessionVariable" getSessionVar
    }
  where
    getSessionVar :: J.Value -> Either Kriti.CustomFunctionError J.Value
    getSessionVar inp = case inp of
      J.String txt ->
        case sessionVarValue of
          Just x -> Right $ J.String x
          Nothing -> Left . Kriti.CustomFunctionError $ "Session variable \"" <> txt <> "\" not found"
        where
          sessionVarValue = sessionVars >>= getSessionVariableValue (mkSessionVariable txt)
      _ -> Left $ Kriti.CustomFunctionError "Session variable name should be a string"

-- | Construct a Template Transformation function for Responses
mkRespTemplateTransform :: TemplatingEngine -> BodyTransformAction -> ResponseTransformCtx -> Either TransformErrorBundle J.Value
mkRespTemplateTransform _ RemoveBody _ = pure J.Null
mkRespTemplateTransform engine (ModifyBody (Template template)) ResponseTransformCtx {..} =
  let context = [("$body", responseTransformBody), ("$request", responseTransformReqCtx)]
   in case engine of
        Kriti -> first (TransformErrorBundle . pure . J.toJSON . Kriti.serialize) $ Kriti.runKriti template context
mkRespTemplateTransform engine (FormUrlEncoded formTemplates) context =
  case engine of
    Kriti -> do
      result <- liftEither . V.toEither $ traverse (validateUnescapedResponseTemplateTransform context) formTemplates
      pure $ J.String $ TE.decodeUtf8 $ BL.toStrict $ foldFormEncoded result

mkResponseTransform :: MetadataResponseTransform -> ResponseTransform
mkResponseTransform MetadataResponseTransform {..} =
  let bodyTransform = mkRespTemplateTransform mrtTemplatingEngine <$> mrtBodyTransform
   in ResponseTransform bodyTransform mrtTemplatingEngine

-- | At the moment we only transform the body of
-- Responses. 'http-client' does not export the constructors for
-- 'Response'. If we want to transform then we will need additional
-- 'apply' functions.
applyResponseTransform :: ResponseTransform -> ResponseTransformCtx -> Either TransformErrorBundle BL.ByteString
applyResponseTransform ResponseTransform {..} ctx@ResponseTransformCtx {..} =
  let bodyFunc :: BL.ByteString -> Either TransformErrorBundle BL.ByteString
      bodyFunc body =
        case respTransformBody of
          Nothing -> pure body
          Just f -> J.encode <$> f ctx
   in bodyFunc (J.encode responseTransformBody)
