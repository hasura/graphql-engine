{-# LANGUAGE QuasiQuotes #-}

-- |
-- Tests for interacting with enum fields.
--
-- https://hasura.io/docs/latest/schema/postgres/enums/
module Test.Schema.EnumSpec (spec) where

import Data.Aeson (Value)
import Data.List.NonEmpty qualified as NE
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine (postGraphql)
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (yaml)
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.Schema (Table (..), table)
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
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Fixture.SetupAction
                    { Fixture.setupAction =
                        Postgres.run_ setup,
                      Fixture.teardownAction = \_ ->
                        Postgres.run_ teardown
                    },
                  Postgres.setupTablesAction schema testEnvironment
                ]
            },
          (Fixture.fixture $ Fixture.Backend Fixture.Citus)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Fixture.SetupAction
                    { Fixture.setupAction =
                        Citus.run_ setup,
                      Fixture.teardownAction = \_ ->
                        Citus.run_ teardown
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
                    Schema.bstPostgres = Just "role"
                  }
          ],
        tablePrimaryKey = ["id"],
        tableData = []
      }
  ]

setup :: String
setup = "create type \"role\" as enum ('admin', 'editor', 'moderator')"

teardown :: String
teardown = "drop type \"role\""

--------------------------------------------------------------------------------
-- Tests

tests :: Fixture.Options -> SpecWith TestEnvironment
tests opts = do
  let shouldBe :: IO Value -> Value -> IO ()
      shouldBe = shouldReturnYaml opts

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

      actual `shouldBe` expected

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

      actual `shouldBe` expected
