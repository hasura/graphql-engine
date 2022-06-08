-- | Some helper functions for testing Aeson instances
module Test.Aeson.Utils
  ( testToFromJSON,
    validateToJSONOpenApi,
    testToFromJSONToSchema,
    jsonRoundTrip,
    jsonProperties,
    validateAgainstOpenApiSchema,
    jsonOpenApiProperties,
    genObject,
    genValue,
  )
where

import Data.Aeson
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (parseEither)
import Data.OpenApi
import Data.Vector qualified as Vec
import Hasura.Prelude
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Internal.Range
import Test.Hspec
import Test.Hspec.Hedgehog

testToFromJSON :: (Eq a, Show a, FromJSON a, ToJSON a) => a -> Value -> Spec
testToFromJSON a v = do
  it "parses from JSON" $
    parseEither parseJSON v `shouldBe` Right a
  it "encodes to JSON" $
    toJSON a `shouldBe` v

validateToJSONOpenApi :: (ToJSON a, ToSchema a) => a -> Spec
validateToJSONOpenApi a = do
  it "value validates against OpenAPI schema" $
    validatePrettyToJSON a `shouldBe` Nothing

testToFromJSONToSchema :: (Eq a, Show a, FromJSON a, ToJSON a, ToSchema a) => a -> Value -> Spec
testToFromJSONToSchema a v = do
  testToFromJSON a v
  validateToJSONOpenApi a

jsonRoundTrip :: (Eq a, Show a, FromJSON a, ToJSON a) => Gen a -> Spec
jsonRoundTrip gen =
  it "JSON roundtrips" $
    hedgehog $ do
      a <- forAll gen
      tripping a toJSON (parseEither parseJSON)

jsonEncodingEqualsValue :: (Show a, ToJSON a) => Gen a -> Spec
jsonEncodingEqualsValue gen =
  it "JSON encoding equals value" $
    hedgehog $ do
      a <- forAll gen
      let encoded = encode a
          decoded = decode encoded :: Maybe Value
      decoded === Just (toJSON a)

jsonProperties :: (Eq a, Show a, FromJSON a, ToJSON a) => Gen a -> Spec
jsonProperties gen = do
  jsonRoundTrip gen
  jsonEncodingEqualsValue gen

validateAgainstOpenApiSchema :: (Show a, ToJSON a, ToSchema a) => Gen a -> Spec
validateAgainstOpenApiSchema gen = do
  it "ToJSON validates against OpenAPI schema" $
    hedgehog $ do
      a <- forAll gen
      validatePrettyToJSON a === Nothing

jsonOpenApiProperties :: (Eq a, Show a, FromJSON a, ToJSON a, ToSchema a) => Gen a -> Spec
jsonOpenApiProperties gen = do
  jsonProperties gen
  validateAgainstOpenApiSchema gen

genObject :: MonadGen m => m Object
genObject = KM.fromList . map (first K.fromText) <$> Gen.list (linear 0 5) ((,) <$> Gen.text (linear 0 5) Gen.unicode <*> genValue)

genValue :: MonadGen m => m Value
genValue =
  Gen.choice
    [ Object <$> genObject,
      Array . Vec.fromList <$> Gen.list (linear 0 5) genValue,
      String <$> Gen.text (linear 0 5) Gen.unicode,
      Number . realToFrac <$> Gen.realFrac_ @_ @Double (linearFrac 0 20),
      Bool <$> Gen.bool,
      pure Null
    ]
