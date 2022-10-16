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
    RequestTransformFns,
    applyRequestTransform,

    -- * Request Transformation Context
    RequestTransformCtx (..),
    RequestContext,
    mkRequestContext,
    mkReqTransformCtx,
    TransformErrorBundle (..),

    -- * Optional Functor
    WithOptional (..),
    withOptional,

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

import Control.Lens (Lens', lens, set, traverseOf, view)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Extended ((.!=), (.:?), (.=), (.=?))
import Data.Aeson.Extended qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.CaseInsensitive qualified as CI
import Data.Functor.Barbie (AllBF, ApplicativeB, ConstraintsB, FunctorB, TraversableB)
import Data.Functor.Barbie qualified as B
import Data.Text.Encoding qualified as TE
import Data.Validation qualified as V
import Hasura.Incremental (Cacheable)
import Hasura.Prelude hiding (first)
import Hasura.RQL.DDL.Webhook.Transform.Body (Body (..), BodyTransformFn, TransformFn (BodyTransformFn_))
import Hasura.RQL.DDL.Webhook.Transform.Body qualified as Body
import Hasura.RQL.DDL.Webhook.Transform.Class
import Hasura.RQL.DDL.Webhook.Transform.Headers
import Hasura.RQL.DDL.Webhook.Transform.Method
import Hasura.RQL.DDL.Webhook.Transform.QueryParams
import Hasura.RQL.DDL.Webhook.Transform.Request
import Hasura.RQL.DDL.Webhook.Transform.Response
import Hasura.RQL.DDL.Webhook.Transform.Url
import Hasura.RQL.DDL.Webhook.Transform.WithOptional (WithOptional (..), withOptional)
import Hasura.Session (SessionVariables)
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

instance FromJSON RequestTransform where
  parseJSON = Aeson.withObject "RequestTransform" \o -> do
    version <- o .:? "version" .!= V1
    method <- o .:? "method"
    url <- o .:? "url"
    body <- case version of
      V1 -> do
        template :: Maybe Template <- o .:? "body"
        pure $ fmap Body.ModifyAsJSON template
      V2 -> o .:? "body"
    queryParams <- o .:? "query_params"
    headers <- o .:? "request_headers"
    let requestFields =
          RequestFields
            { method = withOptional @MethodTransformFn method,
              url = withOptional @UrlTransformFn url,
              body = withOptional @BodyTransformFn body,
              queryParams = withOptional @QueryParamsTransformFn queryParams,
              requestHeaders = withOptional @HeadersTransformFn headers
            }
    templateEngine <- o .:? "template_engine" .!= Kriti
    pure $ RequestTransform {..}

instance ToJSON RequestTransform where
  toJSON RequestTransform {..} =
    let RequestFields {..} = requestFields
        body' = case version of
          V1 -> case (getOptional body) of
            Just (BodyTransformFn_ (Body.ModifyAsJSON template)) ->
              Just ("body", Aeson.toJSON template)
            _ -> Nothing
          V2 -> "body" .=? getOptional body
     in Aeson.object $
          [ "version" .= version,
            "template_engine" .= templateEngine
          ]
            <> catMaybes
              [ "method" .=? getOptional method,
                "url" .=? getOptional url,
                "query_params" .=? getOptional queryParams,
                "request_headers" .=? getOptional requestHeaders,
                body'
              ]

-------------------------------------------------------------------------------

-- | Defunctionalized Webhook Request Transformation
--
-- We represent a defunctionalized request transformation by parameterizing
-- our HKD with 'WithOptional'@ @'TransformFn', which marks each of the fields
-- as optional and supplies the appropriate transformation function to them if
-- if they are provided.
type RequestTransformFns = RequestFields (WithOptional TransformFn)

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
  deriving anyclass (FunctorB, ApplicativeB, TraversableB, ConstraintsB)

deriving stock instance
  AllBF Show f RequestFields =>
  Show (RequestFields f)

deriving stock instance
  AllBF Eq f RequestFields =>
  Eq (RequestFields f)

deriving anyclass instance
  AllBF NFData f RequestFields =>
  NFData (RequestFields f)

deriving anyclass instance
  AllBF Cacheable f RequestFields =>
  Cacheable (RequestFields f)

-- NOTE: It is likely that we can derive these instances. Possibly if
-- we move the aeson instances onto the *Transform types.
instance FromJSON RequestTransformFns where
  parseJSON = Aeson.withObject "RequestTransformFns" $ \o -> do
    method <- o .:? "method"
    url <- o .:? "url"
    body <- o .:? "body"
    queryParams <- o .:? "query_params"
    headers <- o .:? "request_headers"
    pure $
      RequestFields
        { method = withOptional @MethodTransformFn method,
          url = withOptional @UrlTransformFn url,
          body = withOptional @BodyTransformFn body,
          queryParams = withOptional @QueryParamsTransformFn queryParams,
          requestHeaders = withOptional @HeadersTransformFn headers
        }

instance ToJSON RequestTransformFns where
  toJSON RequestFields {..} =
    Aeson.object . catMaybes $
      [ "method" .=? getOptional method,
        "url" .=? getOptional url,
        "body" .=? getOptional body,
        "query_params" .=? getOptional queryParams,
        "request_headers" .=? getOptional requestHeaders
      ]

type RequestContext = RequestFields TransformCtx

instance ToJSON RequestContext where
  toJSON RequestFields {..} =
    Aeson.object
      [ "method" .= coerce @_ @RequestTransformCtx method,
        "url" .= coerce @_ @RequestTransformCtx url,
        "body" .= coerce @_ @RequestTransformCtx body,
        "query_params" .= coerce @_ @RequestTransformCtx queryParams,
        "request_headers" .= coerce @_ @RequestTransformCtx requestHeaders
      ]

mkRequestContext :: RequestTransformCtx -> RequestContext
mkRequestContext ctx =
  -- NOTE: Type Applications are here for documentation purposes.
  RequestFields
    { method = coerce @RequestTransformCtx @(TransformCtx Method) ctx,
      url = coerce @RequestTransformCtx @(TransformCtx Url) ctx,
      body = coerce @RequestTransformCtx @(TransformCtx Body) ctx,
      queryParams = coerce @RequestTransformCtx @(TransformCtx QueryParams) ctx,
      requestHeaders = coerce @RequestTransformCtx @(TransformCtx Headers) ctx
    }

-------------------------------------------------------------------------------

-- TODO(SOLOMON): Add lens law unit tests

-- | A 'Lens\'' for viewing a 'HTTP.Request' as our 'RequestData' HKD; it does
-- so by wrapping each of the matching request fields in a corresponding
-- 'TransformFn'.
--
-- XXX: This function makes internal usage of 'TE.decodeUtf8', which throws an
-- impure exception when the supplied 'ByteString' cannot be decoded into valid
-- UTF8 text!
requestL :: Lens' HTTP.Request RequestData
requestL = lens getter setter
  where
    getter :: HTTP.Request -> RequestData
    getter req =
      RequestFields
        { method = coerce $ CI.mk $ TE.decodeUtf8 $ view HTTP.method req,
          url = coerce $ view HTTP.url req,
          body = coerce $ JSONBody $ Aeson.decode =<< view HTTP.body req,
          queryParams = coerce $ view HTTP.queryParams req,
          requestHeaders = coerce $ view HTTP.headers req
        }

    serializeBody :: Body -> Maybe BL.ByteString
    serializeBody = \case
      JSONBody body -> fmap Aeson.encode body
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
  forall m.
  MonadError TransformErrorBundle m =>
  (HTTP.Request -> RequestContext) ->
  RequestTransformFns ->
  HTTP.Request ->
  m HTTP.Request
applyRequestTransform mkCtx transformations request =
  traverseOf
    requestL
    (transformReqData (mkCtx request))
    request
  where
    -- Apply all of the provided request transformation functions to the
    -- request data extracted from the given 'HTTP.Request'.
    transformReqData transformCtx reqData =
      B.bsequence' $
        B.bzipWith3C @Transform
          transformField
          transformCtx
          transformations
          reqData
    -- Apply a transformation to some request data, if it exists; otherwise
    -- return the original request data.
    transformField ctx (WithOptional maybeFn) (Identity a) =
      case maybeFn of
        Nothing -> pure a
        Just fn -> transform fn ctx a

-------------------------------------------------------------------------------
-- TODO(SOLOMON): Rewrite with HKD

-- | A set of data transformation functions generated from a
-- 'MetadataResponseTransform'. 'Nothing' means use the original
-- response value.
data ResponseTransform = ResponseTransform
  { respTransformBody :: Maybe (ResponseTransformCtx -> Either TransformErrorBundle Aeson.Value),
    respTransformTemplateEngine :: TemplatingEngine
  }

data MetadataResponseTransform = MetadataResponseTransform
  { mrtVersion :: Version,
    mrtBodyTransform :: Maybe BodyTransformFn,
    mrtTemplatingEngine :: TemplatingEngine
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData, Cacheable)

instance FromJSON MetadataResponseTransform where
  parseJSON = Aeson.withObject "MetadataResponseTransform" $ \o -> do
    mrtVersion <- o .:? "version" .!= V1
    mrtBodyTransform <- case mrtVersion of
      V1 -> do
        template :: (Maybe Template) <- o .:? "body"
        pure $ fmap Body.ModifyAsJSON template
      V2 -> o .:? "body"
    templateEngine <- o .:? "template_engine"
    let mrtTemplatingEngine = fromMaybe Kriti templateEngine
    pure $ MetadataResponseTransform {..}

instance ToJSON MetadataResponseTransform where
  toJSON MetadataResponseTransform {..} =
    let body = case mrtVersion of
          V1 -> case mrtBodyTransform of
            Just (Body.ModifyAsJSON template) -> Just ("body", Aeson.toJSON template)
            _ -> Nothing
          V2 -> "body" .=? mrtBodyTransform
     in Aeson.object $
          [ "template_engine" .= mrtTemplatingEngine,
            "version" .= mrtVersion
          ]
            <> maybeToList body

-- | A helper function for constructing the 'ResponseTransformCtx'
buildRespTransformCtx :: Maybe RequestContext -> Maybe SessionVariables -> TemplatingEngine -> BL.ByteString -> ResponseTransformCtx
buildRespTransformCtx requestContext sessionVars engine respBody =
  ResponseTransformCtx
    { responseTransformBody = fromMaybe Aeson.Null $ Aeson.decode @Aeson.Value respBody,
      responseTransformReqCtx = Aeson.toJSON requestContext,
      responseSessionVariables = sessionVars,
      responseTransformEngine = engine
    }

-- | Construct a Template Transformation function for Responses
--
-- XXX: This function makes internal usage of 'TE.decodeUtf8', which throws an
-- impure exception when the supplied 'ByteString' cannot be decoded into valid
-- UTF8 text!
mkRespTemplateTransform ::
  BodyTransformFn ->
  ResponseTransformCtx ->
  Either TransformErrorBundle Aeson.Value
mkRespTemplateTransform Body.Remove _ = pure Aeson.Null
mkRespTemplateTransform (Body.ModifyAsJSON template) context =
  runResponseTemplateTransform template context
mkRespTemplateTransform (Body.ModifyAsFormURLEncoded formTemplates) context = do
  result <-
    liftEither . V.toEither . for formTemplates $
      runUnescapedResponseTemplateTransform' context
  pure . Aeson.String . TE.decodeUtf8 . BL.toStrict $ Body.foldFormEncoded result

mkResponseTransform :: MetadataResponseTransform -> ResponseTransform
mkResponseTransform MetadataResponseTransform {..} =
  let bodyTransform = mkRespTemplateTransform <$> mrtBodyTransform
   in ResponseTransform bodyTransform mrtTemplatingEngine

-- | At the moment we only transform the body of
-- Responses. 'http-client' does not export the constructors for
-- 'Response'. If we want to transform other fields then we will need
-- additional 'apply' functions.
applyResponseTransform ::
  ResponseTransform ->
  ResponseTransformCtx ->
  Either TransformErrorBundle BL.ByteString
applyResponseTransform ResponseTransform {..} ctx@ResponseTransformCtx {..} =
  let bodyFunc :: BL.ByteString -> Either TransformErrorBundle BL.ByteString
      bodyFunc body =
        case respTransformBody of
          Nothing -> pure body
          Just f -> Aeson.encode <$> f ctx
   in bodyFunc (Aeson.encode responseTransformBody)
