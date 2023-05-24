{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Hasura.RQL.Types.Webhook.Transform
  ( MetadataResponseTransform (..),
    RequestTransform (..),
    RequestTransformFns,
    RequestContext,
    RequestData,
    RequestFields (..),
  )
where

import Autodocodec (HasCodec, dimapCodec, disjointEitherCodec, optionalField', optionalFieldWithDefault')
import Autodocodec qualified as AC
import Autodocodec.Extended (optionalVersionField, versionField)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as J
import Data.Aeson.Extended ((.!=), (.:?), (.=), (.=?))
import Data.Functor.Barbie (AllBF, ApplicativeB, ConstraintsB, FunctorB, TraversableB)
import Data.Functor.Barbie qualified as B
import Hasura.Prelude
import Hasura.RQL.Types.Webhook.Transform.Body (Body, BodyTransformFn, TransformCtx (..), TransformFn (..))
import Hasura.RQL.Types.Webhook.Transform.Body qualified as Body
import Hasura.RQL.Types.Webhook.Transform.Class (Template, TemplatingEngine (..))
import Hasura.RQL.Types.Webhook.Transform.Headers (Headers, HeadersTransformFn, TransformCtx (..), TransformFn (..))
import Hasura.RQL.Types.Webhook.Transform.Method (Method, MethodTransformFn, TransformCtx (..), TransformFn (..))
import Hasura.RQL.Types.Webhook.Transform.QueryParams (QueryParams, QueryParamsTransformFn, TransformCtx (..), TransformFn (..))
import Hasura.RQL.Types.Webhook.Transform.Request (RequestTransformCtx, Version (..))
import Hasura.RQL.Types.Webhook.Transform.Url (TransformCtx (..), TransformFn (..), Url, UrlTransformFn (..))
import Hasura.RQL.Types.Webhook.Transform.WithOptional (WithOptional (..), withOptional, withOptionalField')

-------------------------------------------------------------------------------
-- TODO(SOLOMON): Rewrite with HKD

data MetadataResponseTransform = MetadataResponseTransform
  { mrtVersion :: Version,
    mrtBodyTransform :: Maybe BodyTransformFn,
    mrtTemplatingEngine :: TemplatingEngine
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

instance HasCodec MetadataResponseTransform where
  codec =
    dimapCodec
      (either id id)
      (\rt -> case mrtVersion rt of V1 -> Left rt; V2 -> Right rt)
      $ disjointEitherCodec transformV1 transformV2
    where
      transformV1 =
        AC.object "ResponseTransformV1"
          $ MetadataResponseTransform
          <$> (V1 <$ optionalVersionField 1)
          <*> bodyV1
          AC..= mrtBodyTransform
            <*> transformCommon

      transformV2 =
        AC.object "ResponseTransformV2"
          $ MetadataResponseTransform
          <$> (V2 <$ versionField 2)
          <*> bodyV2
          AC..= mrtBodyTransform
            <*> transformCommon

      transformCommon = optionalFieldWithDefault' "template_engine" Kriti AC..= mrtTemplatingEngine

      bodyV1 =
        dimapCodec
          (fmap Body.ModifyAsJSON)
          (\case Just (Body.ModifyAsJSON template) -> Just template; _ -> Nothing)
          $ optionalField' @Template "body"

      bodyV2 = optionalField' @BodyTransformFn "body"

instance FromJSON MetadataResponseTransform where
  parseJSON = J.withObject "MetadataResponseTransform" $ \o -> do
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
            Just (Body.ModifyAsJSON template) -> Just ("body", J.toJSON template)
            _ -> Nothing
          V2 -> "body" .=? mrtBodyTransform
     in J.object
          $ [ "template_engine" .= mrtTemplatingEngine,
              "version" .= mrtVersion
            ]
          <> maybeToList body

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
  deriving anyclass (NFData)

instance HasCodec RequestTransform where
  codec =
    dimapCodec
      (either id id)
      (\rt -> case version rt of V1 -> Left rt; V2 -> Right rt)
      $ disjointEitherCodec transformV1 transformV2
    where
      transformV1 =
        AC.object "RequestTransformV1"
          $ RequestTransform
          <$> (V1 <$ optionalVersionField 1)
          <*> requestFieldsCodec bodyV1
          AC..= requestFields
            <*> transformCommon

      transformV2 =
        AC.object "RequestTransformV2"
          $ RequestTransform
          <$> (V2 <$ versionField 2)
          <*> requestFieldsCodec bodyV2
          AC..= requestFields
            <*> transformCommon

      transformCommon = optionalFieldWithDefault' "template_engine" Kriti AC..= templateEngine

      requestFieldsCodec bodyCodec =
        RequestFields
          <$> withOptionalField' @MethodTransformFn "method"
          AC..= method
            <*> withOptionalField' @UrlTransformFn "url"
          AC..= url
            <*> bodyCodec
          AC..= body
            <*> withOptionalField' @QueryParamsTransformFn "query_params"
          AC..= queryParams
            <*> withOptionalField' @HeadersTransformFn "request_headers"
          AC..= requestHeaders

      bodyV1 = dimapCodec dec enc $ optionalField' @Template "body"
        where
          dec template = withOptional $ fmap Body.ModifyAsJSON template
          enc body = case getOptional body of
            Just (BodyTransformFn_ (Body.ModifyAsJSON template)) -> Just template
            _ -> Nothing

      bodyV2 = withOptionalField' @BodyTransformFn "body"

instance FromJSON RequestTransform where
  parseJSON = J.withObject "RequestTransform" \o -> do
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
              Just ("body", J.toJSON template)
            _ -> Nothing
          V2 -> "body" .=? getOptional body
     in J.object
          $ [ "version" .= version,
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
  (AllBF Show f RequestFields) =>
  Show (RequestFields f)

deriving stock instance
  (AllBF Eq f RequestFields) =>
  Eq (RequestFields f)

deriving anyclass instance
  (AllBF NFData f RequestFields) =>
  NFData (RequestFields f)

-- NOTE: It is likely that we can derive these instances. Possibly if
-- we move the aeson instances onto the *Transform types.
instance FromJSON RequestTransformFns where
  parseJSON = J.withObject "RequestTransformFns" $ \o -> do
    method <- o .:? "method"
    url <- o .:? "url"
    body <- o .:? "body"
    queryParams <- o .:? "query_params"
    headers <- o .:? "request_headers"
    pure
      $ RequestFields
        { method = withOptional @MethodTransformFn method,
          url = withOptional @UrlTransformFn url,
          body = withOptional @BodyTransformFn body,
          queryParams = withOptional @QueryParamsTransformFn queryParams,
          requestHeaders = withOptional @HeadersTransformFn headers
        }

instance ToJSON RequestTransformFns where
  toJSON RequestFields {..} =
    J.object
      . catMaybes
      $ [ "method" .=? getOptional method,
          "url" .=? getOptional url,
          "body" .=? getOptional body,
          "query_params" .=? getOptional queryParams,
          "request_headers" .=? getOptional requestHeaders
        ]

type RequestContext = RequestFields TransformCtx

instance ToJSON RequestContext where
  toJSON RequestFields {..} =
    J.object
      [ "method" .= coerce @_ @RequestTransformCtx method,
        "url" .= coerce @_ @RequestTransformCtx url,
        "body" .= coerce @_ @RequestTransformCtx body,
        "query_params" .= coerce @_ @RequestTransformCtx queryParams,
        "request_headers" .= coerce @_ @RequestTransformCtx requestHeaders
      ]
