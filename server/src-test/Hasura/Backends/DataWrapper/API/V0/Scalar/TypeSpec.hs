{-# LANGUAGE QuasiQuotes #-}

module Hasura.Backends.DataWrapper.API.V0.Scalar.TypeSpec (spec, genType) where

import Data.Aeson.QQ.Simple (aesonQQ)
import Hasura.Backends.DataWrapper.API.V0.Scalar.Type
import Hasura.Prelude
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Test.Aeson.Utils
import Test.Hspec

spec :: Spec
spec = do
  describe "Type" $ do
    describe "StringTy" $
      testToFromJSONToSchema StringTy [aesonQQ|"string"|]
    describe "NumberTy" $
      testToFromJSONToSchema NumberTy [aesonQQ|"number"|]
    describe "BoolTy" $
      testToFromJSONToSchema BoolTy [aesonQQ|"bool"|]
    jsonOpenApiProperties genType

genType :: MonadGen m => m Type
genType = Gen.enumBounded
