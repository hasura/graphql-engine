{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Hasura.Backends.DataConnector.API.V0.ColumnSpec (spec, genColumnName, genColumnInfo) where

import Data.Aeson.QQ.Simple (aesonQQ)
import Hasura.Backends.DataConnector.API.V0
import Hasura.Backends.DataConnector.API.V0.ScalarSpec (genScalarType)
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
    describe "minimal" $
      testFromJSON
        (ColumnInfo (ColumnName "my_column_name") StringTy False Nothing False False)
        [aesonQQ|
          { "name": "my_column_name",
            "type": "string",
            "nullable": false
          }
        |]
    describe "non-minimal" $
      testToFromJSONToSchema
        (ColumnInfo (ColumnName "my_column_name") NumberTy True (Just "My column description") True True)
        [aesonQQ|
          { "name": "my_column_name",
            "type": "number",
            "nullable": true,
            "description": "My column description",
            "insertable": true,
            "updatable": true
          }
        |]
    jsonOpenApiProperties genColumnInfo

genColumnName :: MonadGen m => m ColumnName
genColumnName = ColumnName <$> genArbitraryAlphaNumText defaultRange

genColumnInfo :: (MonadGen m, GenBase m ~ Identity) => m ColumnInfo
genColumnInfo =
  ColumnInfo
    <$> genColumnName
    <*> genScalarType
    <*> Gen.bool
    <*> Gen.maybe (genArbitraryAlphaNumText defaultRange)
    <*> Gen.bool
    <*> Gen.bool
