module Hasura.RQL.WebhookTransformsSpec (spec) where

import Data.Aeson
import Data.CaseInsensitive qualified as CI
import Data.List (nubBy)
import Data.Set qualified as S
import Hasura.Prelude
import Hasura.RQL.DDL.Webhook.Transform
import Hasura.RQL.DDL.Webhook.Transform.Body
import Hasura.RQL.DDL.Webhook.Transform.Class
import Hasura.RQL.DDL.Webhook.Transform.Headers
import Hasura.RQL.DDL.Webhook.Transform.Method
import Hasura.RQL.DDL.Webhook.Transform.QueryParams
import Hasura.RQL.DDL.Webhook.Transform.Url
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog

spec :: Spec
spec = do
  it "Method RoundTrip" $
    hedgehog $ forAll genMethod >>= trippingJSON

  it "StringTemplateText RoundTrip" $
    hedgehog $ forAll genUnescapedTemplate >>= trippingJSON

  it "Url RoundTrip" $
    hedgehog $ forAll genUrl >>= trippingJSON

  it "Template RoundTrip" $
    hedgehog $ forAll genTemplate >>= trippingJSON

  it "TemplateEngine RoundTrip" $
    hedgehog $ forAll genTemplatingEngine >>= trippingJSON

  it "TransformHeaders" $
    hedgehog $ do
      headers <- forAll genTransformHeaders
      let sortH :: ReplaceHeaderFields -> ReplaceHeaderFields
          sortH (ReplaceHeaderFields {..}) =
            ReplaceHeaderFields (sort rhf_addHeaders) (sort rhf_removeHeaders)
      let headersMaybe = eitherDecode $ encode headers
      Right (sortH headers) === fmap sortH headersMaybe

  it "MetadataRequestTransform RoundTrip" $
    hedgehog $ do
      reqFields <- forAll genRequestTransformDefunc
      let sortH :: WithOptional TransformFn Headers -> WithOptional TransformFn Headers
          sortH (WithOptional Nothing) = WithOptional Nothing
          sortH
            (WithOptional (Just (HeadersTransform (ReplaceHeaders (ReplaceHeaderFields {..}))))) =
              WithOptional $ Just $ HeadersTransform $ ReplaceHeaders $ ReplaceHeaderFields (sort rhf_addHeaders) (sort rhf_removeHeaders)

      let sortQ :: WithOptional TransformFn QueryParams -> WithOptional TransformFn QueryParams
          sortQ (WithOptional Nothing) = WithOptional Nothing
          sortQ
            (WithOptional (Just (QueryParamsTransform (QueryParamsTransformAction qs)))) =
              WithOptional $ Just $ QueryParamsTransform $ QueryParamsTransformAction $ sortOn fst qs
      let sortRF rf@RequestFields {requestHeaders, queryParams} =
            rf {requestHeaders = sortH requestHeaders, queryParams = sortQ queryParams}
      let reqFieldsMaybe = eitherDecode $ encode reqFields
      Right (sortRF reqFields) === fmap sortRF reqFieldsMaybe

trippingJSON :: (Show a, Eq a, ToJSON a, FromJSON a, MonadTest m) => a -> m ()
trippingJSON val = tripping val (toJSON) (fromJSON)

genMethod :: Gen Method
genMethod = (Method . CI.mk) <$> Gen.text (Range.constant 3 20) Gen.alphaNum

genTemplatingEngine :: Gen TemplatingEngine
genTemplatingEngine = Gen.enumBounded @_ @TemplatingEngine

-- NOTE: This generator is strictly useful for roundtrip aeson testing
-- and does not produce valid template snippets.
genTemplate :: Gen Template
genTemplate = Template . wrap <$> Gen.text (Range.constant 3 20) Gen.alphaNum
  where
    wrap txt = "\"" <> txt <> "\""

genUnescapedTemplate :: Gen UnescapedTemplate
genUnescapedTemplate = UnescapedTemplate <$> Gen.text (Range.constant 3 20) Gen.alphaNum

genTransformHeaders :: Gen ReplaceHeaderFields
genTransformHeaders = do
  numHeaders <- Gen.integral $ Range.constant 1 20

  let genHeaderKey = CI.mk <$> Gen.text (Range.constant 1 20) Gen.alphaNum
      genHeaderValue = genUnescapedTemplate

      genKeys = S.toList <$> Gen.set (Range.singleton numHeaders) genHeaderKey
      genValues = S.toList <$> Gen.set (Range.singleton numHeaders) genHeaderValue

  removeHeaders <- Gen.list (Range.constant 1 10) genHeaderKey
  addHeaders <- liftA2 zip genKeys genValues
  pure $ ReplaceHeaderFields addHeaders removeHeaders

genQueryParams :: Gen [(UnescapedTemplate, Maybe UnescapedTemplate)]
genQueryParams = do
  numParams <- Gen.integral $ Range.constant 1 20
  let keyGen = genUnescapedTemplate
      valueGen = Gen.maybe $ genUnescapedTemplate
  keys <- Gen.list (Range.singleton numParams) keyGen
  values <- Gen.list (Range.singleton numParams) valueGen
  pure $ nubBy (\a b -> fst a == fst b) $ zip keys values

genUrl :: Gen UnescapedTemplate
genUrl = do
  host <- Gen.text (Range.constant 3 20) Gen.alphaNum

  pure $ UnescapedTemplate $ "http://www." <> host <> ".com"

genRequestTransformDefunc :: Gen RequestTransformFn
genRequestTransformDefunc = do
  method <- Gen.maybe genMethod
  -- NOTE: At the moment no need to generate valid urls or templates
  -- but such instances maybe useful in the future.
  url <- Gen.maybe $ genUrl
  body <- Gen.maybe $ genTemplate
  queryParams <- Gen.maybe $ genQueryParams
  headers <- Gen.maybe $ genTransformHeaders
  pure
    RequestFields
      { method = coerce $ fmap (MethodTransform . ReplaceMethod) method,
        url = coerce $ fmap (UrlTransform . ModifyUrl) url,
        body = coerce $ fmap (BodyTransform . ModifyBody) body,
        queryParams = coerce $ fmap (QueryParamsTransform . QueryParamsTransformAction) queryParams,
        requestHeaders = coerce $ fmap (HeadersTransform . ReplaceHeaders) headers -- HeaderTransform $ fmap (,engine) headers
      }
