{-# LANGUAGE QuasiQuotes #-}

-- |
-- Can we see @no_queries_available@ or not?
module Test.Queries.Simple.NoQueriesAvailableSpec (spec) where

import Data.Aeson (Value)
import Data.List.NonEmpty qualified as NE
import Harness.Backend.BigQuery qualified as BigQuery
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Cockroach qualified as Cockroach
import Harness.Backend.Mysql qualified as Mysql
import Harness.Backend.Postgres qualified as Postgres
import Harness.Backend.Sqlserver qualified as Sqlserver
import Harness.GraphqlEngine (postGraphql)
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (interpolateYaml)
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.Schema (Table (..), table)
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (TestEnvironment)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

spec :: SpecWith TestEnvironment
spec = do
  let execute :: [Schema.Table] -> (Fixture.Options -> SpecWith TestEnvironment) -> SpecWith TestEnvironment
      execute tables =
        Fixture.run
          ( NE.fromList
              [ (Fixture.fixture $ Fixture.Backend Fixture.MySQL)
                  { Fixture.setupTeardown = \(testEnvironment, _) ->
                      [ Mysql.setupTablesAction tables testEnvironment
                      ]
                  },
                (Fixture.fixture $ Fixture.Backend Fixture.Postgres)
                  { Fixture.setupTeardown = \(testEnvironment, _) ->
                      [ Postgres.setupTablesAction tables testEnvironment
                      ]
                  },
                (Fixture.fixture $ Fixture.Backend Fixture.Citus)
                  { Fixture.setupTeardown = \(testEnvironment, _) ->
                      [ Citus.setupTablesAction tables testEnvironment
                      ]
                  },
                (Fixture.fixture $ Fixture.Backend Fixture.Cockroach)
                  { Fixture.setupTeardown = \(testEnv, _) ->
                      [ Cockroach.setupTablesAction schema testEnv
                      ],
                    Fixture.customOptions =
                      Just $
                        Fixture.defaultOptions
                          { Fixture.stringifyNumbers = True,
                            Fixture.skipTests = Just "Cockroach disabled pending prepared args fix https://github.com/cockroachdb/cockroach/issues/86375"
                          }
                  },
                (Fixture.fixture $ Fixture.Backend Fixture.SQLServer)
                  { Fixture.setupTeardown = \(testEnvironment, _) ->
                      [ Sqlserver.setupTablesAction tables testEnvironment
                      ]
                  },
                (Fixture.fixture $ Fixture.Backend Fixture.BigQuery)
                  { Fixture.setupTeardown = \(testEnvironment, _) ->
                      [ BigQuery.setupTablesAction tables testEnvironment
                      ],
                    Fixture.customOptions =
                      Just $
                        Fixture.defaultOptions
                          { Fixture.stringifyNumbers = True
                          }
                  }
              ]
          )

  describe "`no_queries_available`" do
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

queriesAvailable :: Fixture.Options -> SpecWith TestEnvironment
queriesAvailable opts = do
  let shouldBe :: IO Value -> Value -> IO ()
      shouldBe = shouldReturnYaml opts

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

    actual `shouldBe` expected

noQueriesAvailable :: Fixture.Options -> SpecWith TestEnvironment
noQueriesAvailable opts = do
  let shouldBe :: IO Value -> Value -> IO ()
      shouldBe = shouldReturnYaml opts

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

    actual `shouldBe` expected
