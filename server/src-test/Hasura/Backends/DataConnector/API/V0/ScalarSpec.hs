{-# LANGUAGE QuasiQuotes #-}

module Hasura.Backends.DataConnector.API.V0.ScalarSpec (spec, genScalarType, genScalarValue) where

import Data.Aeson.QQ.Simple (aesonQQ)
import Hasura.Backends.DataConnector.API.V0.Scalar
import Hasura.Generator.Common (defaultRange, genArbitraryAlphaNumTextExcluding)
import Hasura.Prelude
import Hedgehog
import Test.Aeson.Utils
import Test.Hspec

spec :: Spec
spec = do
  describe "ScalarType" $ do
    testToFromJSONToSchema (ScalarType "foo") [aesonQQ|"foo"|]
    jsonOpenApiProperties genScalarType

genScalarType :: (MonadGen m, GenBase m ~ Identity) => m ScalarType
genScalarType =
  ScalarType
    <$> genArbitraryAlphaNumTextExcluding ["string", "number", "bool"] defaultRange

genScalarValue :: (MonadGen m, GenBase m ~ Identity) => m ScalarValue
genScalarValue = ScalarValue <$> genValue <*> genScalarType
