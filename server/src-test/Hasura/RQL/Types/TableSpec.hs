{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Hasura.RQL.Types.TableSpec (spec) where

import Data.Aeson (parseJSON, toJSON)
import Data.Aeson.QQ.Simple (aesonQQ)
import Data.Aeson.Types (parseEither)
import Data.HashMap.Strict qualified as HashMap
import Data.Text.NonEmpty (mkNonEmptyText)
import Hasura.Backends.Postgres.SQL.Types (unsafePGCol)
import Hasura.Prelude
import Hasura.QuickCheck.Instances ()
import Hasura.RQL.Types.BackendType (BackendType (Postgres), PostgresKind (Vanilla))
import Hasura.RQL.Types.Common (Comment (..))
import Hasura.Table.Cache (ColumnConfig (..), CustomRootField (..), TableConfig (..), TableCustomRootFields (..), emptyCustomRootFields, emptyTableConfig)
import Language.GraphQL.Draft.Syntax qualified as G
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  tableCustomRootFieldsSpec
  columnConfigSpec
  tableConfigSpec

tableCustomRootFieldsSpec :: Spec
tableCustomRootFieldsSpec = describe "TableCustomRootFields" $ do
  prop "JSON serialization roundtrips" $ \(tableCustomRootFields :: TableCustomRootFields) ->
    parseEither parseJSON (toJSON tableCustomRootFields) `shouldBe` Right tableCustomRootFields

  it "Duplicated field names fail to deserialize" $ do
    let emptyTrcf@TableCustomRootFields {..} = emptyCustomRootFields
    let trcfWithDupes =
          emptyTrcf
            { _tcrfSelect = _tcrfSelect {_crfName = G.mkName "DuplicateFieldName"},
              _tcrfInsert = _tcrfInsert {_crfName = G.mkName "DuplicateFieldName"}
            }
    parseEither parseJSON (toJSON trcfWithDupes) `shouldBe` (Left "Error in $: the following custom root field names are duplicated: DuplicateFieldName" :: Either String TableCustomRootFields)

{-# HLINT ignore columnConfigSpec "Monoid law, right identity" #-}
{-# HLINT ignore columnConfigSpec "Monoid law, left identity" #-}
{-# HLINT ignore columnConfigSpec "Use mconcat" #-}
columnConfigSpec :: Spec
columnConfigSpec = describe "ColumnConfig" $ do
  describe "Monoid laws" $ do
    prop "Right identity" $ \(x :: ColumnConfig) -> x <> mempty `shouldBe` x
    prop "Left identity" $ \(x :: ColumnConfig) -> mempty <> x `shouldBe` x
    prop "Associativity" $ \(x :: ColumnConfig) (y :: ColumnConfig) (z :: ColumnConfig) -> x <> (y <> z) `shouldBe` (x <> y) <> z

tableConfigSpec :: Spec
tableConfigSpec = describe "TableConfig" $ do
  describe "custom_column_names property migration to column_config in JSON deserialization" $ do
    it "custom_column_names are migrated to column_config" $ do
      let json =
            [aesonQQ|
        {
          "custom_column_names": {
            "id": "Id",
            "last_name": "Surname"
          }
        }
      |]
      let actual :: Either String (TableConfig ('Postgres 'Vanilla)) = parseEither parseJSON json
      let expectedColumns =
            HashMap.fromList
              [ (unsafePGCol "id", ColumnConfig (G.mkName "Id") Automatic),
                (unsafePGCol "last_name", ColumnConfig (G.mkName "Surname") Automatic)
              ]
      let expected = (emptyTableConfig @('Postgres 'Vanilla)) {_tcColumnConfig = expectedColumns}
      actual `shouldBe` Right expected

    it "column_config custom names take precedence over custom_column_names" $ do
      let json =
            [aesonQQ|
        {
          "custom_column_names": {
            "id": "Id",
            "last_name": "Surname"
          },
          "column_config": {
            "id": {
              "custom_name": "EyeDee"
            }
          }
        }
      |]
      let actual :: Either String (TableConfig ('Postgres 'Vanilla)) = parseEither parseJSON json
      let expectedColumns =
            HashMap.fromList
              [ (unsafePGCol "id", ColumnConfig (G.mkName "EyeDee") Automatic),
                (unsafePGCol "last_name", ColumnConfig (G.mkName "Surname") Automatic)
              ]
      let expected = (emptyTableConfig @('Postgres 'Vanilla)) {_tcColumnConfig = expectedColumns}
      actual `shouldBe` Right expected

    it "custom_column_names are merged into existing column_configs" $ do
      let json =
            [aesonQQ|
        {
          "custom_column_names": {
            "id": "Id",
            "last_name": "Surname"
          },
          "column_config": {
            "id": {
              "custom_name": "EyeDee",
              "comment": "This is poorly spelled"
            },
            "first_name": {
              "custom_name": "FirstName",
              "comment": "The first name"
            },
            "last_name": {
              "comment": "The surname"
            }
          }
        }
      |]
      let actual :: Either String (TableConfig ('Postgres 'Vanilla)) = parseEither parseJSON json
      let expectedColumns =
            HashMap.fromList
              [ (unsafePGCol "id", ColumnConfig (G.mkName "EyeDee") (Explicit $ mkNonEmptyText "This is poorly spelled")),
                (unsafePGCol "first_name", ColumnConfig (G.mkName "FirstName") (Explicit $ mkNonEmptyText "The first name")),
                (unsafePGCol "last_name", ColumnConfig (G.mkName "Surname") (Explicit $ mkNonEmptyText "The surname"))
              ]
      let expected = (emptyTableConfig @('Postgres 'Vanilla)) {_tcColumnConfig = expectedColumns}
      actual `shouldBe` Right expected

  describe "custom_column_names property backwards compatibility during JSON serialization" $ do
    it "custom_column_names is serialized as a copy of data in column_config" $ do
      let columns =
            HashMap.fromList
              [ (unsafePGCol "id", ColumnConfig Nothing (Explicit $ mkNonEmptyText "This is poorly spelled")),
                (unsafePGCol "first_name", ColumnConfig (G.mkName "FirstName") Automatic),
                (unsafePGCol "last_name", ColumnConfig (G.mkName "Surname") (Explicit $ mkNonEmptyText "The surname")),
                (unsafePGCol "age", ColumnConfig Nothing Automatic)
              ]
      let (tableConfig :: TableConfig ('Postgres 'Vanilla)) = emptyTableConfig {_tcColumnConfig = columns}
      let expectedJson =
            [aesonQQ|
        {
          "custom_root_fields": {},
          "custom_column_names": {
            "first_name": "FirstName",
            "last_name": "Surname"
          },
          "column_config": {
            "id": {
              "comment": "This is poorly spelled"
            },
            "first_name": {
              "custom_name": "FirstName"
            },
            "last_name": {
              "custom_name": "Surname",
              "comment": "The surname"
            }
          }
        }
      |]
      toJSON tableConfig `shouldBe` expectedJson
