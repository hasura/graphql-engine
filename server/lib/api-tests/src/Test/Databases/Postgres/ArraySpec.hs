{-# LANGUAGE QuasiQuotes #-}

-- |
-- Tests that we can decode Arrays values correctly
module Test.Databases.Postgres.ArraySpec (spec) where

import Data.Aeson (Value)
import Data.List.NonEmpty qualified as NE
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Cockroach qualified as Cockroach
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine (postGraphql)
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (interpolateYaml)
import Harness.Schema qualified as Schema
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

spec :: SpecWith GlobalTestEnvironment
spec = do
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
    singleArrayTests

  -- CockroachDB does not support nested arrays
  -- https://www.cockroachlabs.com/docs/stable/array.html
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
            }
        ]
    )
    nestedArrayTests

--------------------------------------------------------------------------------
-- Schema

textArrayType :: Schema.ScalarType
textArrayType =
  Schema.TCustomType
    $ Schema.defaultBackendScalarType
      { Schema.bstPostgres = Just "text[]",
        Schema.bstCitus = Just "text[]",
        Schema.bstCockroach = Just "text[]"
      }

nestedTextArrayType :: Schema.ScalarType
nestedTextArrayType =
  Schema.TCustomType
    $ Schema.defaultBackendScalarType
      { Schema.bstPostgres = Just "text[][]",
        Schema.bstCitus = Just "text[][]",
        Schema.bstCockroach = Just "text[]" -- nested arrays aren't supported in Cockroach, so we'll skip this test anyway
      }

schema :: [Schema.Table]
schema =
  [ (Schema.table "author")
      { Schema.tableColumns =
          [ Schema.column "id" Schema.defaultSerialType,
            Schema.column "name" Schema.TStr,
            Schema.column "emails" textArrayType,
            Schema.column "grid" nestedTextArrayType
          ],
        Schema.tablePrimaryKey = ["id"]
      }
  ]

--------------------------------------------------------------------------------
-- Tests

singleArrayTests :: SpecWith TestEnvironment
singleArrayTests = do
  describe "Saves arrays" $ do
    it "Using native postgres array syntax" \testEnvironment -> do
      let expected :: Value
          expected =
            [interpolateYaml|
              data:
                insert_hasura_author:
                  affected_rows: 1
                  returning:
                    - name: "Ash"
                      emails: ["ash@ash.com", "ash123@ash.com"]
            |]

          actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                mutation {
                  insert_hasura_author (
                    objects: [
                      {
                        name: "Ash",
                        emails: "{ash@ash.com, ash123@ash.com}",
                        grid: "{}"
                      }
                    ]
                  ) {
                    affected_rows
                    returning {
                      name
                      emails
                    }
                  }
                }
              |]

      shouldReturnYaml testEnvironment actual expected

    it "Using native GraphQL array syntax" \testEnvironment -> do
      let expected :: Value
          expected =
            [interpolateYaml|
              data:
                insert_hasura_author:
                  affected_rows: 1
                  returning:
                    - name: "Ash"
                      emails: ["ash@ash.com", "ash123@ash.com"]
            |]

          actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                mutation {
                  insert_hasura_author (
                    objects: [
                      {
                        name: "Ash",
                        emails: ["ash@ash.com", "ash123@ash.com"],
                        grid: []
                      }
                    ]
                  ) {
                    affected_rows
                    returning {
                      name
                      emails
                    }
                  }
                }
              |]

      shouldReturnYaml testEnvironment actual expected

  describe "Filters with contains" $ do
    it "finds values using _contains" \testEnvironment -> do
      void
        $ postGraphql
          testEnvironment
          [graphql|
                mutation {
                  insert_hasura_author (
                    objects: [
                      {
                        name: "contains",
                        emails: ["horse@horse.com", "dog@dog.com"],
                        grid: []
                      }
                    ]
                  ) {
                    affected_rows
                    returning {
                      name
                      emails
                    }
                  }
                }
              |]

      let expected :: Value
          expected =
            [interpolateYaml|
              data:
                hasura_author:
                  - name: contains
            |]

          actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                query {
                  hasura_author (where: { emails: {_contains:["horse@horse.com"]}}) {
                    name
                  }
                }
              |]

      shouldReturnYaml testEnvironment actual expected

    it "finds values using _contained_in" \testEnvironment -> do
      void
        $ postGraphql
          testEnvironment
          [graphql|
                mutation {
                  insert_hasura_author (
                    objects: [
                      {
                        name: "contained_in",
                        emails: ["horse@horse2.com", "dog@dog2.com"],
                        grid: []
                      }
                    ]
                  ) {
                    affected_rows
                    returning {
                      name
                      emails
                    }
                  }
                }
              |]

      let expected :: Value
          expected =
            [interpolateYaml|
              data:
                hasura_author:
                  - name: contained_in
            |]

          actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                query {
                  hasura_author (where: { emails: {_contained_in:["cat@cat2.com","dog@dog2.com", "horse@horse2.com"]}}) {
                    name
                  }
                }
              |]

      shouldReturnYaml testEnvironment actual expected

nestedArrayTests :: SpecWith TestEnvironment
nestedArrayTests = do
  describe "Saves nested arrays" $ do
    -- Postgres introspection is able to tell us about thing[] but thing[][] always comes
    -- back as unknown, so the only way to operate on these values continues to
    -- be with Postgres native array syntax. This test is to ensure we have not
    -- broken that.
    it "Using native postgres array syntax" \testEnvironment -> do
      let expected :: Value
          expected =
            [interpolateYaml|
                data:
                  insert_hasura_author:
                    affected_rows: 1
                    returning:
                      - name: "Ash"
                        grid: [["one", "two", "three"],
                              ["four", "five", "six"]]
              |]

          actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                  mutation {
                    insert_hasura_author (
                      objects: [
                        {
                          name: "Ash",
                          emails: "{}",
                          grid: "{{one,two,three},{four,five,six}}"
                        }
                      ]
                    ) {
                      affected_rows
                      returning {
                        name
                        grid
                      }
                    }
                  }
                |]

      shouldReturnYaml testEnvironment actual expected
