module Hasura.RQL.WebhookTransformsSpec (spec) where

import Data.Aeson
import Data.CaseInsensitive qualified as CI
import Data.List (nubBy)
import Data.Set qualified as S
import Hasura.Prelude
import Hasura.RQL.DDL.WebhookTransforms
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog

spec :: Spec
spec = do
  it "StringTemplateText RoundTrip" $
    hedgehog $ forAll genStringTemplateText >>= trippingJSON

  it "TemplateEngine RoundTrip" $
    hedgehog $ forAll genTemplatingEngine >>= trippingJSON

  it "TemplateText RoundTrip" $
    hedgehog $ forAll genTemplateText >>= trippingJSON

  it "TransformHeaders" $
    hedgehog $ do
      headers <- forAll genTransformHeaders
      let sortH TransformHeaders {..} = TransformHeaders (sort addHeaders) (sort removeHeaders)
          headersMaybe = eitherDecode $ encode headers
      Right (sortH headers) === fmap sortH headersMaybe

  it "MetadataRequestTransform RoundTrip" $
    hedgehog $ do
      transform <- forAll genMetadataRequestTransform
      let sortH TransformHeaders {..} = TransformHeaders (sort addHeaders) (sort removeHeaders)
      let sortMT mt@MetadataRequestTransform {mtRequestHeaders, mtQueryParams} = mt {mtRequestHeaders = sortH <$> mtRequestHeaders, mtQueryParams = sort <$> mtQueryParams}
          transformMaybe = eitherDecode $ encode transform
      Right (sortMT transform) === fmap sortMT transformMaybe

trippingJSON :: (Show a, Eq a, ToJSON a, FromJSON a, MonadTest m) => a -> m ()
trippingJSON val = tripping val (toJSON) (fromJSON)

genTemplatingEngine :: Gen TemplatingEngine
genTemplatingEngine = Gen.enumBounded @_ @TemplatingEngine

-- NOTE: This generator is strictly useful for roundtrip aeson testing
-- and does not produce valid template snippets.
genTemplateText :: Gen TemplateText
genTemplateText = TemplateText . wrap <$> Gen.text (Range.constant 3 20) Gen.alphaNum
  where
    wrap txt = "\"" <> txt <> "\""

genStringTemplateText :: Gen StringTemplateText
genStringTemplateText = StringTemplateText <$> Gen.text (Range.constant 3 20) Gen.alphaNum

genTransformHeaders :: Gen TransformHeaders
genTransformHeaders = do
  numHeaders <- Gen.integral $ Range.constant 1 20

  let genHeaderKey = CI.mk <$> Gen.text (Range.constant 1 20) Gen.alphaNum
      genHeaderValue = genStringTemplateText

      genKeys = S.toList <$> Gen.set (Range.singleton numHeaders) genHeaderKey
      genValues = S.toList <$> Gen.set (Range.singleton numHeaders) genHeaderValue

  removeHeaders <- Gen.list (Range.constant 1 10) genHeaderKey
  addHeaders <- liftA2 zip genKeys genValues
  pure $ TransformHeaders addHeaders removeHeaders

genQueryParams :: Gen [(StringTemplateText, Maybe StringTemplateText)]
genQueryParams = do
  numParams <- Gen.integral $ Range.constant 1 20
  let keyGen = genStringTemplateText
      valueGen = Gen.maybe $ genStringTemplateText
  keys <- Gen.list (Range.singleton numParams) keyGen
  values <- Gen.list (Range.singleton numParams) valueGen
  pure $ nubBy (\a b -> fst a == fst b) $ zip keys values

genUrl :: Gen StringTemplateText
genUrl = do
  host <- Gen.text (Range.constant 3 20) Gen.alphaNum

  pure $ StringTemplateText $ "http://www." <> host <> ".com"

genMetadataRequestTransform :: Gen MetadataRequestTransform
genMetadataRequestTransform = do
  method <- Gen.maybe genStringTemplateText
  -- NOTE: At the moment no need to generate valid urls or templates
  -- but such instances maybe useful in the future.
  url <- Gen.maybe $ genUrl
  bodyTransform <- Gen.maybe $ fmap Transform $ genTemplateText
  queryParams <- Gen.maybe $ genQueryParams
  reqHeaders <- Gen.maybe $ genTransformHeaders
  MetadataRequestTransform
    V2
    method
    url
    bodyTransform
    queryParams
    reqHeaders
    <$> genTemplatingEngine
