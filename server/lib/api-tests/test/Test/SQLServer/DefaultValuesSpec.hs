{-# LANGUAGE QuasiQuotes #-}

-- | Test that Postgres databases correctly roundtrip timestamps.
--
-- https://hasura.io/docs/latest/mutations/ms-sql-server/insert/#insert-multiple-objects-of-the-same-type-in-the-same-mutation
module Test.SQLServer.DefaultValuesSpec (spec) where

import Data.Aeson (Value)
import Data.List.NonEmpty qualified as NE
import Harness.Backend.Sqlserver qualified as Sqlserver
import Harness.GraphqlEngine (postGraphql)
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (yaml)
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (TestEnvironment)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

spec :: SpecWith TestEnvironment
spec =
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Fixture.SQLServer)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Sqlserver.setupTablesAction schema testEnv
                ]
            }
        ]
    )
    tests

--------------------------------------------------------------------------------
-- Schema

schema :: [Schema.Table]
schema =
  [ (Schema.table "alldefaults")
      { Schema.tableColumns =
          [ Schema.column "id" Schema.defaultSerialType,
            Schema.column "dt" defaultDateTimeType
          ],
        Schema.tablePrimaryKey = ["id"]
      },
    (Schema.table "somedefaults")
      { Schema.tableColumns =
          [ Schema.column "id" Schema.defaultSerialType,
            Schema.column "dt" defaultDateTimeType,
            Schema.column "name" Schema.TStr
          ],
        Schema.tablePrimaryKey = ["name"]
      },
    (Schema.table "withrelationship")
      { Schema.tableColumns =
          [ Schema.column "id" Schema.defaultSerialType,
            Schema.column "nickname" Schema.TStr,
            Schema.column "time_id" Schema.TInt
          ],
        Schema.tablePrimaryKey = ["nickname"],
        Schema.tableReferences = [Schema.Reference "time_id" "alldefaults" "id"]
      }
  ]

defaultDateTimeType :: Schema.ScalarType
defaultDateTimeType =
  Schema.TCustomType $
    Schema.defaultBackendScalarType
      { Schema.bstMysql = Nothing,
        Schema.bstMssql = Just "DATETIME DEFAULT GETDATE()",
        Schema.bstCitus = Just "TIMESTAMP DEFAULT NOW()",
        Schema.bstPostgres = Just "TIMESTAMP DEFAULT NOW()",
        Schema.bstBigQuery = Nothing
      }

--------------------------------------------------------------------------------
-- Tests

tests :: Fixture.Options -> SpecWith TestEnvironment
tests opts = do
  let shouldBe :: IO Value -> Value -> IO ()
      shouldBe = shouldReturnYaml opts

  describe "Default values tests" do
    it "Upsert simple object with default values - check empty if_matched" \testEnvironment -> do
      let expected :: Value
          expected =
            [yaml|
              data:
                insert_hasura_somedefaults:
                  affected_rows: 1
                  returning:
                  - id: 1
            |]

          actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                mutation {
                  insert_hasura_somedefaults(
                    objects: [{ name: "a" }]
                    if_matched: {
                      match_columns: [],
                      update_columns: []
                    }
                  ) {
                    affected_rows
                    returning {
                      id
                    }
                  }
                }
              |]

      actual `shouldBe` expected

    it "Upsert simple object with default values - check conflict doesn't update" \testEnvironment -> do
      let expected :: Value
          expected =
            [yaml|
              data:
                insert_hasura_somedefaults:
                  affected_rows: 0
                  returning: []
            |]

          actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                mutation {
                  insert_hasura_somedefaults(
                    objects: [{ name: "a" }]
                    if_matched: {
                      match_columns: name,
                      update_columns: []
                    }
                  ) {
                    affected_rows
                    returning {
                      id
                    }
                  }
                }
              |]

      actual `shouldBe` expected
