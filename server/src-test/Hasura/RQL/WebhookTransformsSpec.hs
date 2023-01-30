module Hasura.RQL.WebhookTransformsSpec
  ( spec,
  )
where

-------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode, fromJSON, toJSON)
import Data.CaseInsensitive qualified as CI
import Data.List (nubBy)
import Data.Set qualified as S
import Hasura.Prelude
import Hasura.RQL.DDL.Webhook.Transform (RequestFields (..), RequestTransformFns, WithOptional (..), withOptional)
import Hasura.RQL.DDL.Webhook.Transform.Body (BodyTransformFn)
import Hasura.RQL.DDL.Webhook.Transform.Body qualified as Body
import Hasura.RQL.DDL.Webhook.Transform.Class (Template (..), TemplatingEngine (..), UnescapedTemplate (..))
import Hasura.RQL.DDL.Webhook.Transform.Headers (HeadersTransformFn, TransformFn (..))
import Hasura.RQL.DDL.Webhook.Transform.Headers qualified as Headers
import Hasura.RQL.DDL.Webhook.Transform.Method (Method (..), MethodTransformFn)
import Hasura.RQL.DDL.Webhook.Transform.Method qualified as Method
import Hasura.RQL.DDL.Webhook.Transform.QueryParams (QueryParamsTransformFn, TransformFn (..))
import Hasura.RQL.DDL.Webhook.Transform.QueryParams qualified as QueryParams
import Hasura.RQL.DDL.Webhook.Transform.Url (UrlTransformFn)
import Hasura.RQL.DDL.Webhook.Transform.Url qualified as Url
import Hedgehog (Gen, MonadTest, forAll, tripping, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

-------------------------------------------------------------------------------

spec :: Spec
spec = describe "WebhookTransform" do
  it "Method RoundTrip" . hedgehog $
    forAll genMethod >>= trippingJSON

  it "StringTemplateText RoundTrip" . hedgehog $
    forAll genUnescapedTemplate >>= trippingJSON

  it "Url RoundTrip" . hedgehog $
    forAll genUrl >>= trippingJSON

  it "Template RoundTrip" . hedgehog $
    forAll genTemplate >>= trippingJSON

  it "TemplateEngine RoundTrip" . hedgehog $
    forAll genTemplatingEngine >>= trippingJSON

  it "TransformHeaders" . hedgehog $ do
    headers <- forAll genTransformHeaders
    let sortH (Headers.AddReplaceOrRemoveFields {..}) =
          Headers.AddReplaceOrRemoveFields (sort addOrReplaceHeaders) (sort removeHeaders)
    let headersMaybe = eitherDecode $ encode headers
    Right (sortH headers) === fmap sortH headersMaybe

  it "MetadataRequestTransform RoundTrip" . hedgehog $ do
    reqFields <- forAll genRequestTransformDefunc
    let sortH (WithOptional Nothing) = WithOptional Nothing
        sortH
          (WithOptional (Just (HeadersTransformFn_ (Headers.AddReplaceOrRemove (Headers.AddReplaceOrRemoveFields {..}))))) =
            WithOptional . Just . HeadersTransformFn_ . Headers.AddReplaceOrRemove $
              Headers.AddReplaceOrRemoveFields (sort addOrReplaceHeaders) (sort removeHeaders)

    let sortQ (WithOptional Nothing) = WithOptional Nothing
        sortQ
          (WithOptional (Just (QueryParamsTransformFn_ (QueryParams.AddOrReplace qs)))) =
            WithOptional . Just . QueryParamsTransformFn_ . QueryParams.AddOrReplace $
              sortOn fst qs
        sortQ
          (WithOptional (Just (QueryParamsTransformFn_ (QueryParams.ParamTemplate qs)))) =
            WithOptional . Just . QueryParamsTransformFn_ . QueryParams.ParamTemplate $ qs
    let sortRF rf@RequestFields {requestHeaders, queryParams} =
          rf {requestHeaders = sortH requestHeaders, queryParams = sortQ queryParams}
    let reqFieldsMaybe = eitherDecode $ encode reqFields
    Right (sortRF reqFields) === fmap sortRF reqFieldsMaybe

-------------------------------------------------------------------------------
-- Generators

genMethod :: Gen Method
genMethod = (Method . CI.mk) <$> Gen.text (Range.constant 3 20) Gen.alphaNum

genTemplatingEngine :: Gen TemplatingEngine
genTemplatingEngine = Gen.enumBounded

-- NOTE: This generator is strictly useful for roundtrip aeson testing
-- and does not produce valid template snippets.
genTemplate :: Gen Template
genTemplate = Template . wrap <$> Gen.text (Range.constant 3 20) Gen.alphaNum
  where
    wrap txt = "\"" <> txt <> "\""

genUnescapedTemplate :: Gen UnescapedTemplate
genUnescapedTemplate = UnescapedTemplate <$> Gen.text (Range.constant 3 20) Gen.alphaNum

genTransformHeaders :: Gen Headers.AddReplaceOrRemoveFields
genTransformHeaders = do
  numHeaders <- Gen.integral $ Range.constant 1 20

  let genHeaderKey = CI.mk <$> Gen.text (Range.constant 1 20) Gen.alphaNum
      genHeaderValue = genUnescapedTemplate

      genKeys = S.toList <$> Gen.set (Range.singleton numHeaders) genHeaderKey
      genValues = S.toList <$> Gen.set (Range.singleton numHeaders) genHeaderValue

  removeHeaders <- Gen.list (Range.constant 1 10) genHeaderKey
  addOrReplaceHeaders <- liftA2 zip genKeys genValues
  pure $ Headers.AddReplaceOrRemoveFields {addOrReplaceHeaders, removeHeaders}

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

genRequestTransformDefunc :: Gen RequestTransformFns
genRequestTransformDefunc = do
  method <- Gen.maybe genMethod
  -- NOTE: At the moment no need to generate valid urls or templates
  -- but such instances maybe useful in the future.
  url <- Gen.maybe genUrl
  body <- Gen.maybe genTemplate
  queryParams <- Gen.maybe genQueryParams
  headers <- Gen.maybe genTransformHeaders
  pure
    RequestFields
      { method = withOptional @MethodTransformFn (fmap Method.Replace method),
        url = withOptional @UrlTransformFn (fmap Url.Modify url),
        body = withOptional @BodyTransformFn (fmap Body.ModifyAsJSON body),
        queryParams = withOptional @QueryParamsTransformFn (fmap QueryParams.AddOrReplace queryParams),
        requestHeaders = withOptional @HeadersTransformFn (fmap Headers.AddReplaceOrRemove headers)
      }

-------------------------------------------------------------------------------
-- Helpers

-- | TODO: Move this out to a common module!
trippingJSON ::
  forall a m.
  (Show a, Eq a, ToJSON a, FromJSON a, MonadTest m) =>
  a ->
  m ()
trippingJSON val = tripping val toJSON fromJSON
