{-# LANGUAGE QuasiQuotes #-}

-- |
-- Tests for interacting with enum fields.
--
-- https://hasura.io/docs/latest/schema/postgres/enums/
module Test.Schema.EnumsSpec (spec) where

import Data.Aeson (Value)
import Data.List.NonEmpty qualified as NE
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Cockroach qualified as Cockroach
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine (postGraphql)
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (yaml)
import Harness.Schema (Table (..), table)
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
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Fixture.SetupAction
                    { Fixture.setupAction =
                        Postgres.run_ testEnvironment setup,
                      Fixture.teardownAction = \_ -> pure ()
                    },
                  Postgres.setupTablesAction schema testEnvironment
                ]
            },
          (Fixture.fixture $ Fixture.Backend Cockroach.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Fixture.SetupAction
                    { Fixture.setupAction =
                        Cockroach.run_ testEnvironment setup,
                      Fixture.teardownAction = \_ -> pure ()
                    },
                  Cockroach.setupTablesAction schema testEnvironment
                ]
            },
          (Fixture.fixture $ Fixture.Backend Citus.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Fixture.SetupAction
                    { Fixture.setupAction =
                        Citus.run_ testEnvironment setup,
                      Fixture.teardownAction = \_ ->
                        -- Teardown is only run on Citus to avoid individual worker configuration
                        -- for easier local testing of distributed DBs like Azure CosmosDB.
                        -- In all other cases, we simply drop the DB.
                        Citus.run_ testEnvironment teardown
                    },
                  Citus.setupTablesAction schema testEnvironment
                ]
            }
        ]
    )
    tests

--------------------------------------------------------------------------------
-- Schema

schema :: [Schema.Table]
schema =
  [ (table "user")
      { tableColumns =
          [ Schema.column "id" Schema.TInt,
            Schema.column "name" Schema.TStr,
            Schema.column "role" do
              Schema.TCustomType
                Schema.defaultBackendScalarType
                  { Schema.bstCitus = Just "role",
                    Schema.bstPostgres = Just "role",
                    Schema.bstCockroach = Just "role"
                  }
          ],
        tablePrimaryKey = ["id"],
        tableData = []
      }
  ]

setup :: Text
setup = "create type \"role\" as enum ('admin', 'editor', 'moderator');"

teardown :: Text
teardown = "drop type \"role\" cascade;"

--------------------------------------------------------------------------------
-- Tests

tests :: SpecWith TestEnvironment
tests =
  describe "Enum values" do
    it "Allows valid values" \testEnvironment -> do
      let expected :: Value
          expected =
            [yaml|
              data:
                insert_hasura_user:
                  returning:
                  - role: admin
                    name: Alice
                    id: 1
                  - role: moderator
                    name: Bob
                    id: 2
                  affected_rows: 2
            |]

          actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                mutation {
                  insert_hasura_user(
                    objects: [
                      { id: 1, name: "Alice", role: "admin" },
                      { id: 2, name: "Bob", role: "moderator" }
                    ]
                  ) {
                    affected_rows
                    returning {
                      id
                      name
                      role
                    }
                  }
                }
              |]

      shouldReturnYaml testEnvironment actual expected

    it "Forbids invalid values" \testEnvironment -> do
      let expected :: Value
          expected =
            [yaml|
              errors:
              - extensions:
                  path: $.selectionSet.insert_hasura_user.args.objects
                  code: data-exception
                message: 'invalid input value for enum role: "invalid"'
            |]

          actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                mutation {
                  insert_hasura_user(
                    objects: [
                      { id: 3, name: "Eve", role: "invalid" }
                    ]
                  ) {
                    affected_rows
                    returning{
                      id
                      name
                      role
                    }
                  }
                }
              |]

      shouldReturnYaml testEnvironment actual expected
