-- | Some helper functions for testing Aeson instances
module Test.Aeson.Utils
  ( testFromJSON,
    testToFromJSON,
    validateToJSONOpenApi,
    testToFromJSONToSchema,
    jsonRoundTrip,
    jsonProperties,
    validateAgainstOpenApiSchema,
    jsonOpenApiProperties,
    genKeyMap,
    genObject,
    genValue,
  )
where

import Data.Aeson
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (parseEither)
import Data.OpenApi
import Data.Typeable (Proxy (..), Typeable, typeRep)
import Data.Vector qualified as Vec
import Hasura.Prelude
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Internal.Range
import Test.Hspec
import Test.Hspec.Hedgehog

showType :: forall a. (Typeable a) => String
showType = show $ typeRep (Proxy :: Proxy a)

testFromJSON :: (HasCallStack, Eq a, Show a, FromJSON a) => a -> Value -> Spec
testFromJSON a v = do
  it "parses from JSON"
    $ parseEither parseJSON v
    `shouldBe` Right a

testToFromJSON :: (HasCallStack, Eq a, Show a, FromJSON a, ToJSON a) => a -> Value -> Spec
testToFromJSON a v = do
  testFromJSON a v
  it "encodes to JSON"
    $ toJSON a
    `shouldBe` v

validateToJSONOpenApi :: (HasCallStack, ToJSON a, ToSchema a) => a -> Spec
validateToJSONOpenApi a = do
  it "value validates against OpenAPI schema"
    $ validatePrettyToJSON a
    `shouldBe` Nothing

testToFromJSONToSchema :: (HasCallStack, Eq a, Show a, FromJSON a, ToJSON a, ToSchema a) => a -> Value -> Spec
testToFromJSONToSchema a v = do
  testToFromJSON a v
  validateToJSONOpenApi a

jsonRoundTrip :: forall a. (HasCallStack, Typeable a, Eq a, Show a, FromJSON a, ToJSON a) => Gen a -> Spec
jsonRoundTrip gen =
  it ("JSON roundtrips " <> showType @a)
    $ hedgehog
    $ do
      a <- forAll gen
      tripping a toJSON (parseEither parseJSON)

jsonEncodingEqualsValue :: (HasCallStack, Show a, ToJSON a) => Gen a -> Spec
jsonEncodingEqualsValue gen =
  it "JSON encoding equals value"
    $ hedgehog
    $ do
      a <- forAll gen
      let encoded = encode a
          decoded = decode encoded :: Maybe Value
      decoded === Just (toJSON a)

jsonProperties :: (HasCallStack, Typeable a, Eq a, Show a, FromJSON a, ToJSON a) => Gen a -> Spec
jsonProperties gen = do
  jsonRoundTrip gen
  jsonEncodingEqualsValue gen

validateAgainstOpenApiSchema :: (HasCallStack, Show a, ToJSON a, ToSchema a) => Gen a -> Spec
validateAgainstOpenApiSchema gen = do
  it "ToJSON validates against OpenAPI schema"
    $ hedgehog
    $ do
      a <- forAll gen
      validatePrettyToJSON a === Nothing

jsonOpenApiProperties :: (HasCallStack, Eq a, Show a, FromJSON a, ToJSON a, ToSchema a) => Gen a -> Spec
jsonOpenApiProperties gen = do
  jsonProperties gen
  validateAgainstOpenApiSchema gen

genKeyMap :: (MonadGen m) => m value -> m (KM.KeyMap value)
genKeyMap genKMValue =
  KM.fromList . map (first K.fromText) <$> Gen.list (linear 0 5) ((,) <$> Gen.text (linear 0 5) Gen.unicode <*> genKMValue)

genObject :: (MonadGen m) => m Object
genObject = genKeyMap genValue

genValue :: (MonadGen m) => m Value
genValue =
  Gen.recursive
    Gen.choice
    [ Object <$> genObject,
      Array . Vec.fromList <$> Gen.list (linear 0 5) genValue
    ]
    [ String <$> Gen.text (linear 0 5) Gen.unicode,
      Number . realToFrac <$> Gen.realFrac_ @_ @Double (linearFrac 0 20),
      Bool <$> Gen.bool,
      pure Null
    ]
