{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Hasura.Backends.DataConnector.API.V0.TargetSpec
  ( spec,
    genTargetName,
  )
where

import Data.Aeson.QQ.Simple (aesonQQ)
import Hasura.Backends.DataConnector.API.V0
import Hasura.Backends.DataConnector.API.V0.FunctionSpec (genFunctionName)
import Hasura.Backends.DataConnector.API.V0.TableSpec (genTableName)
import Hasura.Prelude
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Test.Aeson.Utils (jsonOpenApiProperties, testToFromJSONToSchema)
import Test.Hspec

spec :: Spec
spec = do
  describe "TargetName" $ do
    describe "TNTable"
      $ testToFromJSONToSchema
        (TNTable $ TableName ["my_table"])
        [aesonQQ|
          { "type": "table",
            "table": ["my_table"]
          }
        |]
    describe "TNFunction"
      $ testToFromJSONToSchema
        (TNFunction $ FunctionName ["my_function"])
        [aesonQQ|
          { "type": "function",
            "function": ["my_function"]
          }
        |]

    jsonOpenApiProperties genTargetName

genTargetName :: (MonadGen m) => m TargetName
genTargetName =
  Gen.choice
    [ TNTable <$> genTableName,
      TNFunction <$> genFunctionName
    ]
