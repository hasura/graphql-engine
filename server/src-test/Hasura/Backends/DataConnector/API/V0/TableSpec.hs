{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Hasura.Backends.DataConnector.API.V0.TableSpec (spec, genTableName, genTableInfo) where

import Data.Aeson.QQ.Simple (aesonQQ)
import Hasura.Backends.DataConnector.API.V0
import Hasura.Backends.DataConnector.API.V0.ColumnSpec (genColumnInfo, genColumnName)
import Hasura.Prelude
import Hedgehog
import Hedgehog.Gen
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range
import Test.Aeson.Utils
import Test.Hspec

spec :: Spec
spec = do
  describe "TableName" $ do
    testToFromJSONToSchema (TableName ["my_table_name"]) [aesonQQ|["my_table_name"]|]
    jsonOpenApiProperties genTableName
  describe "TableInfo" $ do
    describe "minimal" $
      testToFromJSONToSchema
        (TableInfo (TableName ["my_table_name"]) [] Nothing Nothing)
        [aesonQQ|
          { "name": ["my_table_name"],
            "columns": []
          }
        |]
    describe "non-minimal" $
      testToFromJSONToSchema
        ( TableInfo
            (TableName ["my_table_name"])
            [ColumnInfo (ColumnName "id") StringTy False Nothing]
            (Just [ColumnName "id"])
            (Just "my description")
        )
        [aesonQQ|
          { "name": ["my_table_name"],
            "columns": [{"name": "id", "type": "string", "nullable": false}],
            "primary_key": ["id"],
            "description": "my description"
          }
        |]
    jsonOpenApiProperties genTableInfo

genTableName :: MonadGen m => m TableName
genTableName = TableName <$> Gen.nonEmpty (linear 1 3) (text (linear 0 10) unicode)

genTableInfo :: MonadGen m => m TableInfo
genTableInfo =
  TableInfo
    <$> genTableName
    <*> Gen.list (linear 0 5) genColumnInfo
    <*> Gen.maybe (Gen.list (linear 0 5) genColumnName)
    <*> Gen.maybe (text (linear 0 20) unicode)
