{-# LANGUAGE QuasiQuotes #-}

module Test.Postgres.EnumSpec (spec) where

import Data.Aeson (Value)
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine (postGraphql)
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (shouldReturnYaml, yaml)
import Harness.Test.Context qualified as Context
import Harness.Test.Schema (Table (..), table)
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (TestEnvironment)
import Test.Hspec (SpecWith, describe, it)
import Prelude

spec :: SpecWith TestEnvironment
spec =
  Context.run
    [ Context.Context
        { name = Context.Backend Context.Postgres,
          mkLocalTestEnvironment = Context.noLocalTestEnvironment,
          setup = const (Postgres.run_ setup) <> Postgres.setup schema,
          teardown = Postgres.teardown schema <> const (Postgres.run_ teardown),
          customOptions = Nothing
        },
      Context.Context
        { name = Context.Backend Context.Citus,
          mkLocalTestEnvironment = Context.noLocalTestEnvironment,
          setup = const (Citus.run_ setup) <> Citus.setup schema,
          teardown = Citus.teardown schema <> const (Citus.run_ teardown),
          customOptions = Nothing
        }
    ]
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

tests :: Context.Options -> SpecWith TestEnvironment
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
