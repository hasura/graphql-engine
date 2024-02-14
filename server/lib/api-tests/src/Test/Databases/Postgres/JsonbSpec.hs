{-# LANGUAGE QuasiQuotes #-}

-- |
-- Tests that we can decode JSONB values correctly
--
-- https://hasura.io/docs/latest/schema/postgres/default-values/postgres-defaults/
module Test.Databases.Postgres.JsonbSpec (spec) where

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
  Fixture.run
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

jsonType :: Schema.ScalarType
jsonType =
  Schema.TCustomType
    $ Schema.defaultBackendScalarType
      { Schema.bstPostgres = Just "JSON",
        Schema.bstCitus = Just "JSON",
        Schema.bstCockroach = Just "JSON"
      }

jsonbType :: Schema.ScalarType
jsonbType =
  Schema.TCustomType
    $ Schema.defaultBackendScalarType
      { Schema.bstPostgres = Just "JSONB",
        Schema.bstCitus = Just "JSONB",
        Schema.bstCockroach = Just "JSONB"
      }

mkJsonValue :: Text -> Schema.ScalarValue
mkJsonValue json =
  Schema.VCustomValue
    $ Schema.defaultBackendScalarValue
      { Schema.bsvPostgres = Just (Schema.Quoted json),
        Schema.bsvCitus = Just (Schema.Quoted json),
        Schema.bsvCockroach = Just (Schema.Quoted json)
      }

schema :: [Schema.Table]
schema =
  [ (Schema.table "test")
      { Schema.tableColumns =
          [ Schema.column "id" Schema.defaultSerialType,
            Schema.column "json" jsonType,
            Schema.column "jsonb" jsonbType
          ],
        Schema.tablePrimaryKey = ["id"],
        Schema.tableData =
          [ [Schema.VInt 1, mkJsonValue "{\"dog\": false, \"numbers\": [1,2,3]}", mkJsonValue "{\"dog\": false, \"numbers\": [1,2,3] }"],
            [Schema.VInt 2, mkJsonValue "{\"dog\": true}", mkJsonValue "{\"dog\":true}"]
          ]
      }
  ]

--------------------------------------------------------------------------------
-- Tests

tests :: SpecWith TestEnvironment
tests = do
  describe "Retrieves JSON and JSONB values" do
    it "Everything decodes as expected" \testEnvironment -> do
      let expected :: Value
          expected =
            [yaml|
              data:
                hasura_test:
                  - id: 1
                    json: {"dog": false, "numbers": [1,2,3] }
                    jsonb: {"dog": false, "numbers": [1,2,3] }
                  - id: 2
                    json: {"dog": true}
                    jsonb: {"dog": true}
            |]

          actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                query {
                  hasura_test {
                    id
                    json
                    jsonb
                  }
                }
              |]

      shouldReturnYaml testEnvironment actual expected

  describe "Fetches values from JSON and JSONB values with a path" do
    it "Everything decodes as expected" \testEnvironment -> do
      let expected :: Value
          expected =
            [yaml|
              data:
                hasura_test:
                  - id: 1
                    dog: false
                    dogb: false
                  - id: 2
                    dog: true
                    dogb: true
            |]

          actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                query {
                  hasura_test {
                    id
                    dog: json(path: "$.dog")
                    dogb: jsonb(path: "$.dog")
                  }
                }
              |]

      shouldReturnYaml testEnvironment actual expected
