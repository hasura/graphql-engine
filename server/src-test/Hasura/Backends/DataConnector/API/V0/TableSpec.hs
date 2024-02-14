{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Hasura.Backends.DataConnector.API.V0.TableSpec (spec, genTableName, genTableTarget, genTableInfo) where

import Data.Aeson.QQ.Simple (aesonQQ)
import Data.HashMap.Strict qualified as HashMap
import Hasura.Backends.DataConnector.API.V0
import Hasura.Backends.DataConnector.API.V0.ColumnSpec (genColumnInfo, genColumnName, genColumnSelector)
import Hasura.Generator.Common
import Hasura.Prelude
import Hedgehog
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
    describe "minimal"
      $ testFromJSON
        (TableInfo (TableName ["my_table_name"]) Table [] Nothing (ForeignKeys mempty) Nothing False False False)
        [aesonQQ|
          { "name": ["my_table_name"],
            "columns": []
          }
        |]
    describe "non-minimal"
      $ testToFromJSONToSchema
        ( TableInfo
            (TableName ["my_table_name"])
            View
            [ColumnInfo (ColumnName "id") (ColumnTypeScalar $ ScalarType "string") False Nothing False False Nothing]
            (Just $ ColumnName "id" :| [])
            (ForeignKeys mempty)
            (Just "my description")
            True
            True
            True
        )
        [aesonQQ|
          { "name": ["my_table_name"],
            "type": "view",
            "columns": [{"name": "id", "type": "string", "nullable": false, "insertable": false, "updatable": false}],
            "primary_key": ["id"],
            "description": "my description",
            "insertable": true,
            "updatable": true,
            "deletable": true
          }
        |]
    describe "foreign-key"
      $ testToFromJSONToSchema
        ( TableInfo
            (TableName ["my_table_name"])
            Table
            [ColumnInfo (ColumnName "id") (ColumnTypeScalar $ ScalarType "string") False Nothing False False Nothing]
            (Just $ ColumnName "id" :| [])
            ( ForeignKeys
                $ HashMap.singleton
                  (ConstraintName "Artist")
                  ( Constraint
                      (TableName ["artist_table"])
                      (ColumnPathMapping $ HashMap.singleton (mkColumnSelector $ ColumnName "ArtistId") (mkColumnSelector $ ColumnName "ArtistId"))
                  )
            )
            (Just "my description")
            False
            False
            False
        )
        [aesonQQ|
          { "name": ["my_table_name"],
            "type": "table",
            "columns": [{"name": "id", "type": "string", "nullable": false, "insertable": false, "updatable": false}],
            "primary_key": ["id"],
            "description": "my description",
            "foreign_keys": {
              "Artist": {
                "foreign_table": ["artist_table"],
                "column_mapping": {
                  "ArtistId": "ArtistId"
                }
              }
            },
            "insertable": false,
            "updatable": false,
            "deletable": false
          }
        |]
    jsonOpenApiProperties genTableInfo

genTableName :: (MonadGen m) => m TableName
genTableName = TableName <$> Gen.nonEmpty (linear 1 3) (genArbitraryAlphaNumText defaultRange)

genTableTarget :: (MonadGen m) => m Target
genTableTarget = TTable . TargetTable <$> genTableName

genForeignKeys :: (MonadGen m) => m ForeignKeys
genForeignKeys = ForeignKeys <$> genHashMap genConstraintName genConstraint defaultRange

genConstraintName :: (MonadGen m) => m ConstraintName
genConstraintName = ConstraintName <$> genArbitraryAlphaNumText defaultRange

genConstraint :: (MonadGen m) => m Constraint
genConstraint =
  let mapping = ColumnPathMapping <$> genHashMap genColumnSelector genColumnSelector defaultRange
   in Constraint <$> genTableName <*> mapping

genTableType :: (MonadGen m) => m TableType
genTableType = Gen.enumBounded

-- | Note: this generator is intended for serialization tests only and does not ensure valid Foreign Key Constraints.
genTableInfo :: Gen TableInfo
genTableInfo =
  TableInfo
    <$> genTableName
    <*> genTableType
    <*> Gen.list defaultRange genColumnInfo
    <*> Gen.maybe (Gen.nonEmpty defaultRange genColumnName)
    <*> genForeignKeys
    <*> Gen.maybe (genArbitraryAlphaNumText defaultRange)
    <*> Gen.bool
    <*> Gen.bool
    <*> Gen.bool
