{-# LANGUAGE QuasiQuotes #-}

-- |
-- Can we see @no_queries_available@ or not?
module Test.Queries.Errors.NoQueriesAvailableSpec (spec) where

import Data.Aeson (Value)
import Data.List.NonEmpty qualified as NE
import Harness.Backend.BigQuery qualified as BigQuery
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Cockroach qualified as Cockroach
import Harness.Backend.DataConnector.Sqlite qualified as Sqlite
import Harness.Backend.Postgres qualified as Postgres
import Harness.Backend.Sqlserver qualified as Sqlserver
import Harness.GraphqlEngine (postGraphql)
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (interpolateYaml)
import Harness.Schema (Table (..), table)
import Harness.Schema qualified as Schema
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

spec :: SpecWith GlobalTestEnvironment
spec = do
  let execute :: [Schema.Table] -> SpecWith TestEnvironment -> SpecWith GlobalTestEnvironment
      execute tables =
        Fixture.run
          ( NE.fromList
              [ (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
                  { Fixture.setupTeardown = \(testEnvironment, _) ->
                      [ Postgres.setupTablesAction tables testEnvironment
                      ]
                  },
                (Fixture.fixture $ Fixture.Backend Citus.backendTypeMetadata)
                  { Fixture.setupTeardown = \(testEnvironment, _) ->
                      [ Citus.setupTablesAction tables testEnvironment
                      ]
                  },
                (Fixture.fixture $ Fixture.Backend Cockroach.backendTypeMetadata)
                  { Fixture.setupTeardown = \(testEnv, _) ->
                      [ Cockroach.setupTablesAction tables testEnv
                      ]
                  },
                (Fixture.fixture $ Fixture.Backend Sqlserver.backendTypeMetadata)
                  { Fixture.setupTeardown = \(testEnvironment, _) ->
                      [ Sqlserver.setupTablesAction tables testEnvironment
                      ]
                  },
                (Fixture.fixture $ Fixture.Backend BigQuery.backendTypeMetadata)
                  { Fixture.setupTeardown = \(testEnvironment, _) ->
                      [ BigQuery.setupTablesAction tables testEnvironment
                      ],
                    Fixture.customOptions =
                      Just
                        $ Fixture.defaultOptions
                          { Fixture.stringifyNumbers = True
                          }
                  },
                (Fixture.fixture $ Fixture.Backend Sqlite.backendTypeMetadata)
                  { Fixture.setupTeardown = \(testEnvironment, _) ->
                      [ Sqlite.setupTablesAction tables testEnvironment
                      ]
                  }
              ]
          )

  describe "no_queries_available" do
    execute schema queriesAvailable
    execute mempty noQueriesAvailable

--------------------------------------------------------------------------------
-- Schema

schema :: [Schema.Table]
schema =
  [ (table "authors")
      { tableColumns = [Schema.column "id" Schema.TInt]
      }
  ]

--------------------------------------------------------------------------------
-- Tests

queriesAvailable :: SpecWith TestEnvironment
queriesAvailable = do
  it "Should disappear when queries are available" \testEnvironment -> do
    let actual :: IO Value
        actual =
          postGraphql
            testEnvironment
            [graphql|
              query {
                no_queries_available
              }
            |]

        expected :: Value
        expected =
          [interpolateYaml|
            errors:
            - extensions:
                code: validation-failed
                path: $.selectionSet.no_queries_available
              message: |-
               field 'no_queries_available' not found in type: 'query_root'
         |]

    shouldReturnYaml testEnvironment actual expected

noQueriesAvailable :: SpecWith TestEnvironment
noQueriesAvailable = do
  it "Should be present when queries are not" \testEnvironment -> do
    let actual :: IO Value
        actual =
          postGraphql
            testEnvironment
            [graphql|
              query {
                no_queries_available
              }
            |]

        expected :: Value
        expected =
          [interpolateYaml|
            data:
              no_queries_available: There are no queries available to the current role. Either
                there are no sources or remote schemas configured, or the current role doesn't
                have the required permissions.
         |]

    shouldReturnYaml testEnvironment actual expected
