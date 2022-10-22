{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Hasura.Backends.DataConnector.API.V0.TableSpec (spec, genTableName, genTableInfo) where

import Data.Aeson.QQ.Simple (aesonQQ)
import Data.HashMap.Strict qualified as HashMap
import Hasura.Backends.DataConnector.API.V0
import Hasura.Backends.DataConnector.API.V0.ColumnSpec (genColumnInfo, genColumnName)
import Hasura.Generator.Common
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
        (TableInfo (TableName ["my_table_name"]) [] Nothing Nothing Nothing)
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
            Nothing
        )
        [aesonQQ|
          { "name": ["my_table_name"],
            "columns": [{"name": "id", "type": "string", "nullable": false}],
            "primary_key": ["id"],
            "description": "my description"
          }
        |]
    describe "foreign-key" $
      testToFromJSONToSchema
        ( TableInfo
            (TableName ["my_table_name"])
            [ColumnInfo (ColumnName "id") StringTy False Nothing]
            (Just [ColumnName "id"])
            (Just "my description")
            (Just $ ForeignKeys $ HashMap.singleton (ConstraintName "Artist") (Constraint "Artists" (HashMap.singleton "ArtistId" "ArtistId")))
        )
        [aesonQQ|
          { "name": ["my_table_name"],
            "columns": [{"name": "id", "type": "string", "nullable": false}],
            "primary_key": ["id"],
            "description": "my description",
            "foreign_keys": {
              "Artist": {
                "foreign_table": "Artists",
                "column_mapping": {
                  "ArtistId": "ArtistId"
                }
              }
            }
          }
        |]
    jsonOpenApiProperties genTableInfo

genTableName :: MonadGen m => m TableName
genTableName = TableName <$> Gen.nonEmpty (linear 1 3) (text (linear 0 10) unicode)

genForeignKeys :: MonadGen m => m ForeignKeys
genForeignKeys = ForeignKeys <$> genHashMap genConstraintName genConstraint defaultRange

genConstraintName :: MonadGen m => m ConstraintName
genConstraintName = ConstraintName <$> text (linear 0 10) unicode

genConstraint :: MonadGen m => m Constraint
genConstraint =
  let foreignTable = text (linear 0 10) unicode
      mapping = genHashMap (text (linear 0 10) unicode) (text (linear 0 10) unicode) defaultRange
   in Constraint <$> foreignTable <*> mapping

-- | Note: this generator is intended for serialization tests only and does not ensure valid Foreign Key Constraints.
genTableInfo :: MonadGen m => m TableInfo
genTableInfo =
  TableInfo
    <$> genTableName
    <*> Gen.list (linear 0 5) genColumnInfo
    <*> Gen.maybe (Gen.list (linear 0 5) genColumnName)
    <*> Gen.maybe (text (linear 0 20) unicode)
    <*> Gen.maybe genForeignKeys
