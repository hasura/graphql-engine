{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Hasura.Backends.DataConnector.API.V0.ColumnSpec (spec, genColumnName, genColumnSelector, genColumnType, genColumnInfo, genColumnValueGenerationStrategy) where

import Data.Aeson.QQ.Simple (aesonQQ)
import Hasura.Backends.DataConnector.API.V0
import Hasura.Backends.DataConnector.API.V0.ScalarSpec (genScalarType)
import Hasura.Generator.Common (defaultRange, genArbitraryAlphaNumText)
import Hasura.Prelude
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Language.GraphQL.Draft.Generator (genName)
import Test.Aeson.Utils
import Test.Hspec

spec :: Spec
spec = do
  describe "ColumnName" $ do
    testToFromJSONToSchema (ColumnName "my_column_name") [aesonQQ|"my_column_name"|]
    jsonOpenApiProperties genColumnName
  describe "ColumnSelector" $ do
    describe "single column selector"
      $ testToFromJSONToSchema (ColumnSelectorColumn $ ColumnName "foo") [aesonQQ|"foo"|]
    describe "nested path selector"
      $ testToFromJSONToSchema (ColumnSelectorPath [ColumnName "foo", ColumnName "bar"]) [aesonQQ|["foo","bar"]|]
    jsonOpenApiProperties genColumnSelector
  describe "ColumnInfo" $ do
    describe "minimal"
      $ testFromJSON
        (ColumnInfo (ColumnName "my_column_name") (ColumnTypeScalar $ ScalarType "string") False Nothing False False Nothing)
        [aesonQQ|
          { "name": "my_column_name",
            "type": "string",
            "nullable": false
          }
        |]
    describe "non-minimal"
      $ testToFromJSONToSchema
        (ColumnInfo (ColumnName "my_column_name") (ColumnTypeScalar $ ScalarType "number") True (Just "My column description") True True (Just AutoIncrement))
        [aesonQQ|
          { "name": "my_column_name",
            "type": "number",
            "nullable": true,
            "description": "My column description",
            "insertable": true,
            "updatable": true,
            "value_generated": { "type": "auto_increment" }
          }
        |]
    jsonOpenApiProperties genColumnInfo
  describe "ColumnValueGenerationStrategy" $ do
    describe "AutoIncrement"
      $ testToFromJSONToSchema AutoIncrement [aesonQQ|{"type": "auto_increment"}|]

    describe "UniqueIdentifier"
      $ testToFromJSONToSchema UniqueIdentifier [aesonQQ|{"type": "unique_identifier"}|]

    describe "DefaultValue"
      $ testToFromJSONToSchema DefaultValue [aesonQQ|{"type": "default_value"}|]

genColumnName :: (MonadGen m) => m ColumnName
genColumnName = ColumnName <$> genArbitraryAlphaNumText defaultRange

genColumnSelector :: (MonadGen m) => m ColumnSelector
genColumnSelector =
  Gen.choice
    [ ColumnSelectorPath <$> Gen.nonEmpty defaultRange genColumnName,
      ColumnSelectorColumn <$> genColumnName
    ]

genColumnType :: Gen ColumnType
genColumnType =
  Gen.choice
    [ ColumnTypeScalar <$> genScalarType,
      ColumnTypeObject <$> genName,
      ColumnTypeArray <$> genColumnType <*> Gen.bool
    ]

genColumnInfo :: Gen ColumnInfo
genColumnInfo =
  ColumnInfo
    <$> genColumnName
    <*> genColumnType
    <*> Gen.bool
    <*> Gen.maybe (genArbitraryAlphaNumText defaultRange)
    <*> Gen.bool
    <*> Gen.bool
    <*> Gen.maybe genColumnValueGenerationStrategy

genColumnValueGenerationStrategy :: (MonadGen m) => m ColumnValueGenerationStrategy
genColumnValueGenerationStrategy =
  Gen.element [AutoIncrement, UniqueIdentifier, DefaultValue]
