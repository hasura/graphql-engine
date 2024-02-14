{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Hasura.Backends.DataConnector.API.V0.FunctionSpec
  ( spec,
    genFunctionName,
    genFunctionArgument,
  )
where

import Data.Aeson.QQ.Simple (aesonQQ)
import Hasura.Backends.DataConnector.API.V0
import Hasura.Backends.DataConnector.API.V0.ScalarSpec (genScalarType)
import Hasura.Generator.Common (defaultRange, genArbitraryAlphaNumText)
import Hasura.Prelude
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Internal.Range (linear)
import Test.Aeson.Utils (genValue, jsonOpenApiProperties, testToFromJSONToSchema)
import Test.Hspec

spec :: Spec
spec = do
  describe "FunctionName" $ do
    testToFromJSONToSchema (TableName ["my_function_name"]) [aesonQQ|["my_function_name"]|]
    jsonOpenApiProperties genFunctionName

genFunctionName :: (MonadGen m) => m FunctionName
genFunctionName = FunctionName <$> Gen.nonEmpty (linear 1 3) (genArbitraryAlphaNumText defaultRange)

genFunctionArgument :: Gen FunctionArgument
genFunctionArgument =
  NamedArgument
    <$> genArbitraryAlphaNumText defaultRange
    <*> genArgumentValue

genArgumentValue :: Gen ArgumentValue
genArgumentValue =
  fmap ScalarArgumentValue
    $ ScalarValue
    <$> genValue
    <*> genScalarType
