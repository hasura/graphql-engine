{-# LANGUAGE QuasiQuotes #-}

-- | Test that each supported backend is able to do a simple live query /
-- subscription.
module Test.Subscriptions.LiveQueriesSpec (spec) where

import Data.Aeson
import Data.List.NonEmpty qualified as NE
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Cockroach qualified as Cockroach
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine
import Harness.Quoter.Graphql
import Harness.Quoter.Yaml (yaml)
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (TestEnvironment (..))
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, it)

spec :: SpecWith TestEnvironment
spec =
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Fixture.Postgres)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Postgres.setupTablesAction schema testEnvironment
                ]
            },
          (Fixture.fixture $ Fixture.Backend Fixture.Citus)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Citus.setupTablesAction schema testEnvironment
                ]
            },
          (Fixture.fixture $ Fixture.Backend Fixture.Cockroach)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Cockroach.setupTablesAction schema testEnvironment
                ]
            }
        ]
    )
    tests

--------------------------------------------------------------------------------
-- Schema

schema :: [Schema.Table]
schema =
  [ (Schema.table "example")
      { Schema.tableColumns =
          [ Schema.column "id" Schema.TInt,
            Schema.column "name" Schema.TStr
          ],
        Schema.tablePrimaryKey = ["id"],
        Schema.tableData = []
      }
  ]

--------------------------------------------------------------------------------
-- Tests

tests :: Fixture.Options -> SpecWith TestEnvironment
tests opts = do
  let shouldBe :: IO Value -> Value -> IO ()
      shouldBe = shouldReturnYaml opts

  withSubscriptions do
    it "Hasura sends updated query results after insert" \(mkSubscription, testEnvironment) -> do
      let schemaName :: Schema.SchemaName
          schemaName = Schema.getSchemaName testEnvironment
      query <-
        mkSubscription
          [graphql|
            subscription {
              #{schemaName}_example {
                id
                name
              }
            }
          |]
          []
      -- check initial query result
      do
        let expected :: Value
            expected =
              [yaml|
                data:
                  hasura_example: []
              |]
            actual :: IO Value
            actual = getNextResponse query

        actual `shouldBe` expected

      -- add some data
      do
        let expected :: Value
            expected =
              [yaml|
                data:
                  insert_hasura_example:
                    affected_rows: 2
              |]
            actual :: IO Value
            actual =
              postGraphql
                testEnvironment
                [graphql|
                  mutation {
                    insert_#{schemaName}_example(
                      objects: [{id: 1, name: "A"}, {id: 2, name: "B"}]
                    ) {
                      affected_rows
                    }
                  }
                |]
        actual `shouldBe` expected

      -- fetch the next response
      do
        let expected :: Value
            expected =
              [yaml|
                data:
                  hasura_example:
                    - id: 1
                      name: "A"
                    - id: 2
                      name: "B"
              |]

        getNextResponse query `shouldBe` expected

  withSubscriptions do
    it "Multiplexes" \(mkSubscription, testEnvironment) -> do
      let schemaName :: Schema.SchemaName
          schemaName = Schema.getSchemaName testEnvironment

      subIdEq3 <-
        mkSubscription
          [graphql|
            subscription {
              #{schemaName}_example(where: { id: { _eq: 3 } }) {
                id
                name
              }
            }
          |]
          []

      subIdEq4 <-
        mkSubscription
          [graphql|
            subscription {
              #{schemaName}_example(where: { id: { _eq: 4 } }) {
                id
                name
              }
            }
          |]
          []

      getNextResponse subIdEq3
        `shouldBe` [yaml|
        data:
          hasura_example: []
      |]

      getNextResponse subIdEq4
        `shouldBe` [yaml|
        data:
          hasura_example: []
      |]

      let expected :: Value
          expected =
            [yaml|
              data:
                insert_hasura_example:
                  affected_rows: 2
            |]

          actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                mutation {
                  insert_#{schemaName}_example(
                    objects:
                      [ {id: 3, name: "A"},
                        {id: 4, name: "B"}
                      ]
                  ) {
                    affected_rows
                  }
                }
              |]

      actual `shouldBe` expected

      getNextResponse subIdEq3
        `shouldBe` [yaml|
        data:
          hasura_example:
            - id: 3
              name: "A"
      |]

      getNextResponse subIdEq4
        `shouldBe` [yaml|
        data:
          hasura_example:
            - id: 4
              name: "B"
      |]

  withSubscriptions do
    it "Live query with variables" \(mkSubscription, testEnvironment) -> do
      let schemaName :: Schema.SchemaName
          schemaName = Schema.getSchemaName testEnvironment
      query <-
        mkSubscription
          [graphql|
            subscription getAlice($name: String!) {
              #{schemaName}_example(where: { name: { _eq: $name } }, order_by: {id: desc}) {
                id
                name
              }
            }
          |]
          [ "variables"
              .= object
                [ ("name", String "Alice")
                ]
          ]
      -- check initial query result
      do
        let expected :: Value
            expected =
              [yaml|
                data:
                  hasura_example: []
              |]
            actual :: IO Value
            actual = getNextResponse query

        actual `shouldBe` expected

      -- add some data
      do
        let expected :: Value
            expected =
              [yaml|
                data:
                  insert_hasura_example:
                    affected_rows: 2
              |]
            actual :: IO Value
            actual =
              postGraphql
                testEnvironment
                [graphql|
                  mutation {
                    insert_#{schemaName}_example(
                      objects: [{id: 101, name: "Alice"}, {id: 102, name: "Bob"}]
                    ) {
                      affected_rows
                    }
                  }
                |]
        actual `shouldBe` expected

      -- fetch the next response
      do
        let expected :: Value
            expected =
              [yaml|
                data:
                  hasura_example:
                    - id: 101
                      name: "Alice"
              |]

        getNextResponse query `shouldBe` expected

      -- add another alice
      do
        let expected :: Value
            expected =
              [yaml|
                data:
                  insert_hasura_example:
                    affected_rows: 2
              |]
            actual :: IO Value
            actual =
              postGraphql
                testEnvironment
                [graphql|
                  mutation {
                    insert_#{schemaName}_example(
                      objects: [{id: 103, name: "Alice"}, {id: 104, name: "Bart"}]
                    ) {
                      affected_rows
                    }
                  }
                |]
        actual `shouldBe` expected

      -- fetch the next response
      do
        let expected :: Value
            expected =
              [yaml|
                data:
                  hasura_example:
                    - id: 103
                      name: "Alice"
                    - id: 101
                      name: "Alice"
              |]

        getNextResponse query `shouldBe` expected
