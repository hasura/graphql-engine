{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use maybe" #-}

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

import Control.Lens (Lens', lens, preview, set, traverseOf, view)
import Data.Aeson.Extended qualified as J
import Data.ByteString.Lazy qualified as BL
import Data.CaseInsensitive qualified as CI
import Data.Functor.Barbie qualified as B
import Data.Text.Encoding qualified as TE
import Data.Validation qualified as V
import Hasura.Prelude hiding (first)
import Hasura.RQL.DDL.Webhook.Transform.Body (Body (..), BodyTransformFn)
import Hasura.RQL.DDL.Webhook.Transform.Body qualified as Body
import Hasura.RQL.DDL.Webhook.Transform.Class
import Hasura.RQL.DDL.Webhook.Transform.Headers
import Hasura.RQL.DDL.Webhook.Transform.Method
import Hasura.RQL.DDL.Webhook.Transform.QueryParams
import Hasura.RQL.DDL.Webhook.Transform.Request
import Hasura.RQL.DDL.Webhook.Transform.Response
import Hasura.RQL.DDL.Webhook.Transform.Url
import Hasura.RQL.Types.Webhook.Transform (MetadataResponseTransform (..), RequestContext, RequestData, RequestFields (..), RequestTransform (..), RequestTransformFns)
import Hasura.RQL.Types.Webhook.Transform.WithOptional (WithOptional (..), withOptional)
import Hasura.Session (SessionVariables)
import Network.HTTP.Client.Transformable qualified as HTTP

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
          body = coerce $ JSONBody $ J.decode =<< preview (HTTP.body . HTTP._RequestBodyLBS) req,
          queryParams = coerce $ view HTTP.queryParams req,
          requestHeaders = coerce $ view HTTP.headers req
        }

    serializeBody :: Body -> HTTP.RequestBody
    serializeBody = \case
      JSONBody body -> HTTP.RequestBodyLBS $ fromMaybe mempty $ fmap J.encode body
      RawBody "" -> mempty
      RawBody bs -> HTTP.RequestBodyLBS bs

    setter :: HTTP.Request -> RequestData -> HTTP.Request
    setter req RequestFields {..} =
      req
        & set HTTP.method (TE.encodeUtf8 $ CI.original $ coerce method)
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
  (MonadError TransformErrorBundle m) =>
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
      B.bsequence'
        $ B.bzipWith3C @Transform
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
  { respTransformBody :: Maybe (ResponseTransformCtx -> Either TransformErrorBundle J.Value),
    respTransformTemplateEngine :: TemplatingEngine
  }

-- | A helper function for constructing the 'ResponseTransformCtx'
buildRespTransformCtx :: Maybe RequestContext -> Maybe SessionVariables -> TemplatingEngine -> BL.ByteString -> Int -> ResponseTransformCtx
buildRespTransformCtx requestContext sessionVars engine respBody respStatusCode =
  ResponseTransformCtx
    { responseTransformBody = fromMaybe J.Null $ J.decode @J.Value respBody,
      responseTransformReqCtx = J.toJSON requestContext,
      responseSessionVariables = sessionVars,
      responseTransformEngine = engine,
      responseStatusCode = respStatusCode
    }

-- | Construct a Template Transformation function for Responses
--
-- XXX: This function makes internal usage of 'TE.decodeUtf8', which throws an
-- impure exception when the supplied 'ByteString' cannot be decoded into valid
-- UTF8 text!
mkRespTemplateTransform ::
  BodyTransformFn ->
  ResponseTransformCtx ->
  Either TransformErrorBundle J.Value
mkRespTemplateTransform Body.Remove _ = pure J.Null
mkRespTemplateTransform (Body.ModifyAsJSON template) context =
  runResponseTemplateTransform template context
mkRespTemplateTransform (Body.ModifyAsFormURLEncoded formTemplates) context = do
  result <-
    liftEither
      . V.toEither
      . for formTemplates
      $ runUnescapedResponseTemplateTransform' context
  pure . J.String . TE.decodeUtf8 . BL.toStrict $ Body.foldFormEncoded result

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
          Just f -> J.encode <$> f ctx
   in bodyFunc (J.encode responseTransformBody)

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
