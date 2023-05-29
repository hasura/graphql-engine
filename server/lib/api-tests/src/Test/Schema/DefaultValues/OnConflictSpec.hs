{-# LANGUAGE QuasiQuotes #-}
-- For runWithLocalTestEnvironmentSingleSetup
{-# OPTIONS_GHC -Wno-deprecations #-}

-- |
-- Tests for the behaviour of columns with default values.
--
-- https://hasura.io/docs/latest/schema/postgres/default-values/postgres-defaults/
module Test.Schema.DefaultValues.OnConflictSpec (spec) where

import Data.Aeson (Value)
import Data.List.NonEmpty qualified as NE
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Cockroach qualified as Cockroach
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine (postGraphql)
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (yaml)
import Harness.Schema qualified as Schema
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

spec :: SpecWith GlobalTestEnvironment
spec =
  Fixture.runSingleSetup
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Postgres.setupTablesAction schema testEnv
                ]
            },
          (Fixture.fixture $ Fixture.Backend Citus.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Citus.setupTablesAction schema testEnv
                ]
            },
          (Fixture.fixture $ Fixture.Backend Cockroach.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Cockroach.setupTablesAction schema testEnv
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
        Schema.tableReferences = [Schema.reference "time_id" "alldefaults" "id"]
      }
  ]

defaultDateTimeType :: Schema.ScalarType
defaultDateTimeType =
  Schema.TCustomType
    $ Schema.defaultBackendScalarType
      { Schema.bstMssql = Just "DATETIME DEFAULT GETDATE()",
        Schema.bstCitus = Just "TIMESTAMP DEFAULT NOW()",
        Schema.bstPostgres = Just "TIMESTAMP DEFAULT NOW()",
        Schema.bstCockroach = Just "TIMESTAMP DEFAULT NOW()",
        Schema.bstBigQuery = Nothing
      }

--------------------------------------------------------------------------------
-- Tests

tests :: SpecWith TestEnvironment
tests =
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

      shouldReturnYaml testEnvironment actual expected

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

      shouldReturnYaml testEnvironment actual expected

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

      shouldReturnYaml testEnvironment actual expected
