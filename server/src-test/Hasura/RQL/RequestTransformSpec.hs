module Hasura.RQL.RequestTransformSpec (spec) where

import Data.Aeson
import Data.CaseInsensitive qualified as CI
import Data.HashMap.Strict qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import Hasura.Prelude
import Hasura.RQL.DDL.RequestTransform
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog

spec :: Spec
spec = do
  it "RequstMethod RoundTrip" $
    hedgehog $ forAll genRequestMethod >>= trippingJSON

  it "TemplateEngine RoundTrip" $
    hedgehog $ forAll genTemplatingEngine >>= trippingJSON

  it "TemplateText RoundTrip" $
    hedgehog $ forAll genTemplateText >>= trippingJSON

  it "ContentType RoundTrip" $
    hedgehog $ forAll genContentType >>= trippingJSON

  it "TransformHeaders" $
    hedgehog $ do
      headers <- forAll genTransformHeaders
      let sortH TransformHeaders {..} = TransformHeaders (sort addHeaders) (sort removeHeaders)
          headersMaybe = decode $ encode headers
      Just (sortH headers) === fmap sortH headersMaybe

  it "MetadataTransform RoundTrip" $
    hedgehog $ do
      transform <- forAll genMetadataTransform
      let sortH TransformHeaders {..} = TransformHeaders (sort addHeaders) (sort removeHeaders)
          sortMT mt@MetadataTransform {mtRequestHeaders} = mt {mtRequestHeaders = sortH <$> mtRequestHeaders}
          transformMaybe = decode $ encode transform
      Just (sortMT transform) === fmap sortMT transformMaybe

trippingJSON :: (Show a, Eq a, ToJSON a, FromJSON a, MonadTest m) => a -> m ()
trippingJSON val = tripping val (toJSON) (fromJSON)

genRequestMethod :: Gen RequestMethod
genRequestMethod = Gen.enumBounded @_ @RequestMethod

genTemplatingEngine :: Gen TemplatingEngine
genTemplatingEngine = Gen.enumBounded @_ @TemplatingEngine

-- NOTE: This generator is strictly useful for roundtrip aeson testing
-- and does not produce valid template snippets.
genTemplateText :: Gen TemplateText
genTemplateText = TemplateText <$> Gen.text (Range.constant 3 20) Gen.alphaNum

genContentType :: Gen ContentType
genContentType = Gen.enumBounded @_ @ContentType

genTransformHeaders :: Gen TransformHeaders
genTransformHeaders = do
  numHeaders <- Gen.integral $ Range.constant 1 20

  let genHeaderKey = CI.mk <$> Gen.text (Range.constant 1 20) Gen.alphaNum
      genHeaderValue = Gen.text (Range.constant 3 20) Gen.alphaNum

      genKeys = S.toList <$> Gen.set (Range.singleton numHeaders) genHeaderKey
      genValues = S.toList <$> Gen.set (Range.singleton numHeaders) genHeaderValue

  removeHeaders <- Gen.list (Range.constant 1 10) genHeaderKey
  addHeaders <- liftA2 zip genKeys genValues
  pure $ TransformHeaders addHeaders removeHeaders

genQueryParams :: Gen (M.HashMap T.Text (Maybe T.Text))
genQueryParams = do
  numParams <- Gen.integral $ Range.constant 1 20
  let keyGen = Gen.text (Range.constant 1 20) Gen.alphaNum
      valueGen = Gen.maybe $ Gen.text (Range.constant 1 20) Gen.alphaNum
  keys <- Gen.list (Range.singleton numParams) keyGen
  values <- Gen.list (Range.singleton numParams) valueGen
  pure $ M.fromList $ zip keys values

genMetadataTransform :: Gen MetadataTransform
genMetadataTransform = do
  method <- Gen.maybe genRequestMethod
  -- NOTE: At the moment no need to generate valid urls or templates
  -- but such instances maybe useful in the future.
  url <- Gen.maybe $ Gen.text (Range.constant 3 20) Gen.alphaNum
  bodyTransform <- Gen.maybe $ genTemplateText
  contentType <- Gen.maybe $ genContentType
  queryParams <- Gen.maybe $ genQueryParams
  reqHeaders <- Gen.maybe $ genTransformHeaders
  MetadataTransform
    method
    url
    bodyTransform
    contentType
    queryParams
    reqHeaders
    <$> genTemplatingEngine
