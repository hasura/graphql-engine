{-# LANGUAGE QuasiQuotes #-}

module Hasura.Backends.DataWrapper.API.V0.Scalar.ValueSpec (spec, genValue) where

import Data.Aeson.QQ.Simple (aesonQQ)
import Hasura.Backends.DataWrapper.API.V0.Scalar.Value
import Hasura.Prelude
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range
import Test.Aeson.Utils (jsonOpenApiProperties, testToFromJSONToSchema)
import Test.Hspec

spec :: Spec
spec = do
  describe "Value" $ do
    describe "String" $
      testToFromJSONToSchema (String "foo") [aesonQQ|"foo"|]
    describe "Number" $
      testToFromJSONToSchema (Number 42) [aesonQQ|42|]
    describe "Bool" $
      testToFromJSONToSchema (Boolean True) [aesonQQ|true|]
    describe "Null" $
      testToFromJSONToSchema Null [aesonQQ|null|]
    jsonOpenApiProperties genValue

genValue :: MonadGen m => m Value
genValue =
  Gen.choice
    [ String <$> Gen.text (linear 0 20) Gen.unicode,
      Number . realToFrac <$> Gen.realFrac_ @_ @Double (linearFrac 0 20),
      Boolean <$> Gen.bool,
      pure Null
    ]
