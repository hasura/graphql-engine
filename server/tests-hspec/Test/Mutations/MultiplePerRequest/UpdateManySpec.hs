{-# LANGUAGE QuasiQuotes #-}

-- | Test around performing multiple update requests at the same time.
--
-- https://hasura.io/docs/latest/mutations/postgres/multiple-mutations/
module Test.Mutations.MultiplePerRequest.UpdateManySpec (spec) where

import Data.Aeson (Value)
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
spec = do
  -- TODO: this test causes an internal server error for MySQL, even if we add
  -- "SERIAL" as the 'Schema.defaultSerialType' for MySQL.

  Fixture.run
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
    tests

--------------------------------------------------------------------------------
-- Schema

schema :: [Schema.Table]
schema =
  [ (table "author")
      { tableColumns =
          [ Schema.column "id" Schema.defaultSerialType,
            Schema.column "name" Schema.TStr
          ],
        tablePrimaryKey = ["id"],
        tableData =
          [ [Schema.VInt 1, Schema.VStr "Author 1"],
            [Schema.VInt 2, Schema.VStr "Author 2"],
            [Schema.VInt 3, Schema.VStr "Author 3"]
          ]
      }
  ]

--------------------------------------------------------------------------------
-- Tests

tests :: Fixture.Options -> SpecWith TestEnvironment
tests opts = do
  let shouldBe :: IO Value -> Value -> IO ()
      shouldBe = shouldReturnYaml opts

  describe "Update many objects" do
    it "When many equals zero" \testEnvironment -> do
      let expected :: Value
          expected =
            [yaml|
              data:
                update_hasura_author_many:
                - affected_rows: 0
            |]

          actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                mutation {
                  update_hasura_author_many(
                    updates: [
                      { where: { id: { _eq: 10 } }
                        _set: { name: "test" }
                      }
                    ]
                  ) {
                    affected_rows
                  }
                }
              |]

      actual `shouldBe` expected

    it "When many equals one" \testEnvironment -> do
      let expected :: Value
          expected =
            [yaml|
              data:
                update_hasura_author_many:
                - affected_rows: 0
                - affected_rows: 1
            |]

          actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                mutation {
                  update_hasura_author_many(
                    updates: [
                      { where: { id: { _eq: 10 } }
                        _set: { name: "test" }
                      }
                      { where: { id: { _gt: 2 } }
                        _set: { name: "test" }
                      }
                    ]
                  ){
                    affected_rows
                  }
                }
              |]

      actual `shouldBe` expected

    it "When many is more than one" \testEnvironment -> do
      let expected :: Value
          expected =
            [yaml|
              data:
                update_hasura_author_many:
                - affected_rows: 1
                  returning:
                  - name: "test"
                - affected_rows: 1
                  returning:
                  - name: "changed name"
            |]

          actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                mutation {
                  update_hasura_author_many(
                    updates: [
                      { where: { id: { _gt: 2 } }
                        _set: { name: "test" }
                      }
                      { where: { name: { _eq: "test" } }
                        _set: { name: "changed name" }
                      }
                    ]
                  ){
                    affected_rows
                    returning {
                      name
                    }
                  }
                }
              |]

      actual `shouldBe` expected

    it "Update record multiple times with overlapping conditions" \testEnvironment -> do
      let expected :: Value
          expected =
            [yaml|
              data:
                update_hasura_author_many:
                - affected_rows: 1
                  returning:
                  - name: "test"
                - affected_rows: 1
                  returning:
                  - name: "changed name"
            |]

          actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                mutation {
                  update_hasura_author_many(
                    updates: [
                      { where: { id: { _gt: 2 } }
                        _set: { name: "test" }
                      }
                      { where: { id: { _eq: 3 } }
                        _set: { name: "changed name" }
                      }
                    ]
                  ) {
                    affected_rows
                    returning {
                      name
                    }
                  }
                }
              |]

      actual `shouldBe` expected

    it "Reverts on error" \testEnvironment -> do
      let expected :: Value
          expected =
            [yaml|
              errors:
                - extensions:
                    code: validation-failed
                    path: $.selectionSet.update_hasura_author_many.selectionSet.returning.selectionSet.made_up_field
                  message: 'field ''made_up_field'' not found in type: ''hasura_author'''
            |]

          actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                mutation {
                  update_hasura_author_many(
                    updates: [
                      { where: { id: { _eq: 1 } }
                        _set: { name: "test" }
                      }
                      { where: { id: { _eq: 1 } }
                        _set: { name: "tested" }
                      }
                    ]
                  ) {
                    affected_rows
                    returning {
                      made_up_field
                    }
                  }
                }
              |]

      actual `shouldBe` expected
