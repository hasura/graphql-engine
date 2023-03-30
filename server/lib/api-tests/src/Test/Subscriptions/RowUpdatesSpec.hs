{-# LANGUAGE QuasiQuotes #-}

-- | Test that each supported backend is able to do a simple live query /
-- subscription.
--
-- https://hasura.io/docs/latest/subscriptions/postgres/livequery/use-cases/#pg-subscribe-table
module Test.Subscriptions.RowUpdatesSpec (spec) where

import Data.Aeson
import Data.List.NonEmpty qualified as NE
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Cockroach qualified as Cockroach
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine
import Harness.Quoter.Graphql
import Harness.Quoter.Yaml (yaml)
import Harness.Schema qualified as Schema
import Harness.Subscriptions
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment (..))
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, it)

spec :: SpecWith GlobalTestEnvironment
spec =
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Postgres.setupTablesAction schema testEnvironment
                ]
            },
          (Fixture.fixture $ Fixture.Backend Citus.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Citus.setupTablesAction schema testEnvironment
                ]
            },
          (Fixture.fixture $ Fixture.Backend Cockroach.backendTypeMetadata)
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

tests :: SpecWith TestEnvironment
tests = do
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

        shouldReturnYaml testEnvironment actual expected

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
        shouldReturnYaml testEnvironment actual expected

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

        shouldReturnYaml testEnvironment (getNextResponse query) expected

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

      shouldReturnYaml
        testEnvironment
        (getNextResponse subIdEq3)
        [yaml|
          data:
            hasura_example: []
        |]

      shouldReturnYaml
        testEnvironment
        (getNextResponse subIdEq4)
        [yaml|
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

      shouldReturnYaml testEnvironment actual expected

      shouldReturnYaml
        testEnvironment
        (getNextResponse subIdEq3)
        [yaml|
          data:
            hasura_example:
              - id: 3
                name: "A"
        |]

      shouldReturnYaml
        testEnvironment
        (getNextResponse subIdEq4)
        [yaml|
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

        shouldReturnYaml testEnvironment actual expected

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
        shouldReturnYaml testEnvironment actual expected

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

        shouldReturnYaml testEnvironment (getNextResponse query) expected

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
        shouldReturnYaml testEnvironment actual expected

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

        shouldReturnYaml testEnvironment (getNextResponse query) expected
