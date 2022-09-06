{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Hasura.Backends.DataConnector.API.V0.SchemaSpec (spec) where

import Data.Aeson.QQ.Simple (aesonQQ)
import Hasura.Backends.DataConnector.API.V0.Schema
import Hasura.Backends.DataConnector.API.V0.TableSpec (genTableInfo)
import Hasura.Generator.Common (defaultRange)
import Hasura.Prelude
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Test.Aeson.Utils
import Test.Hspec

spec :: Spec
spec = do
  describe "SchemaResponse" $ do
    testToFromJSONToSchema (SchemaResponse []) [aesonQQ|{"tables": []}|]
    jsonOpenApiProperties genSchemaResponse

genSchemaResponse :: MonadGen m => m SchemaResponse
genSchemaResponse =
  SchemaResponse <$> Gen.list defaultRange genTableInfo
