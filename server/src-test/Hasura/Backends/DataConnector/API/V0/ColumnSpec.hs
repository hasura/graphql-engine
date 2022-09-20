{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Hasura.Backends.DataConnector.API.V0.ColumnSpec (spec, genColumnName, genColumnInfo) where

import Data.Aeson.QQ.Simple (aesonQQ)
import Hasura.Backends.DataConnector.API.V0
import Hasura.Backends.DataConnector.API.V0.ScalarSpec (genType)
import Hasura.Generator.Common (defaultRange, genArbitraryAlphaNumText)
import Hasura.Prelude
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Test.Aeson.Utils
import Test.Hspec

spec :: Spec
spec = do
  describe "ColumnName" $ do
    testToFromJSONToSchema (ColumnName "my_column_name") [aesonQQ|"my_column_name"|]
    jsonOpenApiProperties genColumnName
  describe "ColumnInfo" $ do
    describe "without description" $
      testToFromJSONToSchema
        (ColumnInfo (ColumnName "my_column_name") StringTy False Nothing)
        [aesonQQ|
          { "name": "my_column_name",
            "type": "string",
            "nullable": false
          }
        |]
    describe "with description" $
      testToFromJSONToSchema
        (ColumnInfo (ColumnName "my_column_name") NumberTy True (Just "My column description"))
        [aesonQQ|
          { "name": "my_column_name",
            "type": "number",
            "nullable": true,
            "description": "My column description"
          }
        |]
    jsonOpenApiProperties genColumnInfo

genColumnName :: MonadGen m => m ColumnName
genColumnName = ColumnName <$> genArbitraryAlphaNumText defaultRange

genColumnInfo :: MonadGen m => m ColumnInfo
genColumnInfo =
  ColumnInfo
    <$> genColumnName
    <*> genType
    <*> Gen.bool
    <*> Gen.maybe (genArbitraryAlphaNumText defaultRange)
