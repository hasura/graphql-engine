{-# LANGUAGE QuasiQuotes #-}

module Hasura.Backends.DataConnector.API.V0.ScalarSpec (spec, genScalarType) where

import Data.Aeson.QQ.Simple (aesonQQ)
import Hasura.Backends.DataConnector.API.V0.Scalar
import Hasura.Generator.Common (defaultRange, genArbitraryAlphaNumTextExcluding)
import Hasura.Prelude
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Test.Aeson.Utils
import Test.Hspec

spec :: Spec
spec = do
  describe "ScalarType" $ do
    describe "StringTy" $
      testToFromJSONToSchema StringTy [aesonQQ|"string"|]
    describe "NumberTy" $
      testToFromJSONToSchema NumberTy [aesonQQ|"number"|]
    describe "BoolTy" $
      testToFromJSONToSchema BoolTy [aesonQQ|"bool"|]
    describe "CustomTy" $
      testToFromJSONToSchema (CustomTy "foo") [aesonQQ|"foo"|]
    jsonOpenApiProperties genScalarType

genScalarType :: (MonadGen m, GenBase m ~ Identity) => m ScalarType
genScalarType =
  Gen.choice
    [ pure StringTy,
      pure NumberTy,
      pure BoolTy,
      CustomTy
        <$> genArbitraryAlphaNumTextExcluding ["string", "number", "bool"] defaultRange
    ]
