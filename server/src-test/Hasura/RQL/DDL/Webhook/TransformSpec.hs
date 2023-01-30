{-# LANGUAGE QuasiQuotes #-}

module Hasura.RQL.DDL.Webhook.TransformSpec (spec) where

import Autodocodec (HasCodec, parseJSONViaCodec, toJSONViaCodec)
import Data.Aeson (FromJSON, ToJSON, Value, fromJSON, toJSON)
import Data.Aeson.QQ (aesonQQ)
import Data.Aeson.Types (parse, parseEither)
import Data.CaseInsensitive qualified as CI
import Hasura.Prelude
import Hasura.RQL.DDL.Webhook.Transform (RequestFields (..), RequestTransform (..), WithOptional (getOptional))
import Hasura.RQL.DDL.Webhook.Transform.Body (BodyTransformFn (..), TransformFn (BodyTransformFn_))
import Hasura.RQL.DDL.Webhook.Transform.Class (Template (..), UnescapedTemplate (..))
import Hasura.RQL.DDL.Webhook.Transform.Headers
  ( AddReplaceOrRemoveFields (..),
    HeadersTransformFn (..),
    TransformFn (..),
  )
import Test.Hspec

v2Input :: Value
v2Input =
  [aesonQQ|{
      version: 2,
      method: "get",
      url: "https://test.com/webhook",
      query_params: {
        secret: "hunter2",
        limit: "4"
      },
      request_headers: {
        add_headers: {
          authorization: "Bearer hunter2",
          "x-power-level": "9000"
        },
        remove_headers: ["Cookie"]
      },
      body: {
        action: "transform",
        template: "{{cats}}"
      }
    }|]

v1Input :: Value
v1Input =
  [aesonQQ|{
      version: 1,
      method: "get",
      url: "https://test.com/webhook",
      query_params: {
        secret: "hunter2",
        limit: "4"
      },
      request_headers: {
        add_headers: {
          authorization: "Bearer hunter2",
          "x-power-level": "9000"
        },
        remove_headers: ["Cookie"]
      },
      body: "{{cats}}"
    }|]

v2Parsed :: Either String RequestTransform
v2Parsed = parseEither (parseJSONViaCodec @RequestTransform) v2Input

v1Parsed :: Either String RequestTransform
v1Parsed = parseEither (parseJSONViaCodec @RequestTransform) v1Input

spec :: Spec
spec = do
  describe "Webhook Transform" do
    it "should serialize v2 equivalently with codecs or with Aeson" do
      shouldRoundTripEquivalentlyToJSON @RequestTransform v2Input

    it "should serialize v1 equivalently with codecs or with Aeson" do
      shouldRoundTripEquivalentlyToJSON @RequestTransform v1Input

    it "parses request headers" do
      let expected =
            AddReplaceOrRemoveFields
              { addOrReplaceHeaders =
                  [ (CI.mk "Authorization", UnescapedTemplate "Bearer hunter2"),
                    (CI.mk "X-Power-Level", UnescapedTemplate "9000")
                  ],
                removeHeaders = ["Cookie"]
              }
      let headers = collapseOptional $ getOptional . requestHeaders . requestFields <$> v2Parsed
      (sortOn fst . addOrReplaceHeaders . coerce <$> headers) `shouldBe` (Right (addOrReplaceHeaders expected))
      (sort . removeHeaders . coerce <$> headers) `shouldBe` (Right (removeHeaders expected))

    it "parses v1 body" do
      let expected = BodyTransformFn_ $ ModifyAsJSON $ Template "{{cats}}"
      let actualBody = collapseOptional $ getOptional . body . requestFields <$> v1Parsed
      actualBody `shouldBe` Right expected

    it "parses v2 body" do
      let expected = BodyTransformFn_ $ ModifyAsJSON $ Template "{{cats}}"
      let actualBody = collapseOptional $ getOptional . body . requestFields <$> v2Parsed
      actualBody `shouldBe` Right expected

shouldRoundTripEquivalentlyToJSON :: forall a. (Eq a, HasCodec a, FromJSON a, ToJSON a, Show a) => Value -> Expectation
shouldRoundTripEquivalentlyToJSON input = do
  decodedViaCodec `shouldBe` decodedViaJSON
  encodedViaCodec `shouldBe` encodedViaJSON
  where
    decodedViaCodec = parse (parseJSONViaCodec @a) input
    decodedViaJSON = fromJSON @a input
    encodedViaCodec = toJSONViaCodec <$> decodedViaCodec
    encodedViaJSON = toJSON <$> decodedViaJSON

collapseOptional :: Either String (Maybe a) -> Either String a
collapseOptional (Right (Just val)) = Right val
collapseOptional (Right Nothing) = Left "Got @Nothing@"
collapseOptional (Left e) = Left e
