{-# LANGUAGE QuasiQuotes #-}

-- |
-- Tests for the behaviour of columns with default values.
--
-- https://hasura.io/docs/latest/schema/postgres/default-values/postgres-defaults/
module Test.Postgres.DefaultValuesSpec (spec) where

import Data.Aeson (Value)
import Data.List.NonEmpty qualified as NE
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Postgres qualified as Postgres
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
        [ (Fixture.fixture $ Fixture.Backend Fixture.Postgres)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Postgres.setupTablesAction schema testEnv
                ]
            },
          (Fixture.fixture $ Fixture.Backend Fixture.Citus)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Citus.setupTablesAction schema testEnv
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
    it "Upsert simple object with default values - check empty constraints" \testEnvironment -> do
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
                    on_conflict: {
                      constraint: somedefaults_pkey,
                      update_columns: []
                    }
                  ){
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
                    on_conflict: {
                      constraint: somedefaults_pkey,
                      update_columns: []
                    }
                  ){
                    affected_rows
                    returning {
                      id
                    }
                  }
                }
              |]

      actual `shouldBe` expected

    it "Nested insert with empty object" \testEnvironment -> do
      let expected :: Value
          expected =
            [yaml|
              data:
                insert_hasura_withrelationship:
                  affected_rows: 2
                  returning:
                  - id: 1
                    nickname: "the a"
                    alldefaults_by_time_id_to_id:
                      id: 1
            |]

          actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                mutation {
                  insert_hasura_withrelationship(
                    objects: [{ nickname: "the a", alldefaults_by_time_id_to_id: {data: {} } }]
                    on_conflict: {
                      constraint: withrelationship_pkey,
                      update_columns: []
                    }
                  ) {
                    affected_rows
                    returning {
                      id
                      nickname
                      alldefaults_by_time_id_to_id {
                        id
                      }
                    }
                  }
                }
              |]

      actual `shouldBe` expected
