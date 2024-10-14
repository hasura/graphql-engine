{-# LANGUAGE QuasiQuotes #-}

-- |
-- Tests for queries involving GraphQL variables.
--
-- https://hasura.io/docs/latest/queries/postgres/variables-aliases-fragments-directives/#using-variables
-- https://hasura.io/docs/latest/queries/ms-sql-server/variables-aliases-fragments-directives/#using-variables
-- https://hasura.io/docs/latest/queries/bigquery/variables-aliases-fragments-directives/#using-variables
module Test.Queries.VariablesAliasesFragments.VariablesSpec (spec) where

import Data.Aeson (Value (String), object, (.=))
import Data.List.NonEmpty qualified as NE
import Harness.Backend.BigQuery qualified as BigQuery
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Cockroach qualified as Cockroach
import Harness.Backend.Postgres qualified as Postgres
import Harness.Backend.Sqlserver qualified as Sqlserver
import Harness.GraphqlEngine (postGraphqlWithPair)
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
  Fixture.hgeWithEnv [("HASURA_GRAPHQL_EXPERIMENTAL_FEATURES", "no_null_unbound_variable_default")]
    $ Fixture.run
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
            (Fixture.fixture $ Fixture.Backend Sqlserver.backendTypeMetadata)
              { Fixture.setupTeardown = \(testEnv, _) ->
                  [ Sqlserver.setupTablesAction schema testEnv
                  ]
              },
            (Fixture.fixture $ Fixture.Backend Cockroach.backendTypeMetadata)
              { Fixture.setupTeardown = \(testEnv, _) ->
                  [ Cockroach.setupTablesAction schema testEnv
                  ]
              }
          ]
      )
      mutations

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
          (Fixture.fixture $ Fixture.Backend Sqlserver.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Sqlserver.setupTablesAction schema testEnv
                ]
            },
          (Fixture.fixture $ Fixture.Backend BigQuery.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ BigQuery.setupTablesAction schema testEnv
                ],
              Fixture.customOptions =
                Just
                  $ Fixture.defaultOptions
                    { Fixture.stringifyNumbers = True
                    }
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

schema :: [Schema.Table]
schema =
  [ (table "author")
      { tableColumns =
          [ Schema.column "id" Schema.TInt,
            (Schema.column "name" Schema.TStr)
              { Schema.columnNullable = True
              }
          ],
        tablePrimaryKey = ["id"],
        tableData =
          [ [Schema.VInt 1, Schema.VStr "Alice"],
            [Schema.VInt 2, Schema.VStr "Bob"]
          ]
      }
  ]

--------------------------------------------------------------------------------
-- Tests

tests :: SpecWith TestEnvironment
tests =
  describe "Using variables" do
    it "A query involving a single variable" \testEnvironment -> do
      let schemaName :: Schema.SchemaName
          schemaName = Schema.getSchemaName testEnvironment

          actual :: IO Value
          actual =
            postGraphqlWithPair
              testEnvironment
              [graphql|
                query($author: String!) {
                  #{schemaName}_author (where: { name: { _eq: $author } }) {
                    name
                    id
                  }
                }
              |]
              [ "variables"
                  .= object
                    [ ("author", String "Alice")
                    ]
              ]

          expected :: Value
          expected =
            [interpolateYaml|
              data:
                #{schemaName}_author:
                - name: Alice
                  id: 1
            |]

      shouldReturnYaml testEnvironment actual expected

mutations :: SpecWith TestEnvironment
mutations =
  describe "Using variables" do
    it "A query involving a nullable variable with no default" \testEnvironment -> do
      let schemaName :: Schema.SchemaName
          schemaName = Schema.getSchemaName testEnvironment

          actual :: IO Value
          actual =
            postGraphqlWithPair
              testEnvironment
              [graphql|
                mutation($name: String) {
                  update_#{schemaName}_author(
                    where: { id: { _eq: 2 } },
                    _set: { name: $name, id: 5 }
                  ) {
                    affected_rows

                    returning {
                      id
                      name
                    }
                  }
                }
              |]
              [ "variables" .= object []
              ]

          expected :: Value
          expected =
            [interpolateYaml|
              data:
                update_#{schemaName}_author:
                  affected_rows: 1
                  returning:
                  - id: 5
                    name: Bob
            |]

      shouldReturnYaml testEnvironment actual expected

    it "A query involving a nullable variable with non-null default" \testEnvironment -> do
      let schemaName :: Schema.SchemaName
          schemaName = Schema.getSchemaName testEnvironment

          actual :: IO Value
          actual =
            postGraphqlWithPair
              testEnvironment
              [graphql|
                mutation($name: String = "Tom") {
                  update_#{schemaName}_author(
                    where: { id: { _eq: 1 } },
                    _set: { name: $name }
                  ) {
                    affected_rows

                    returning {
                      id
                      name
                    }
                  }
                }
              |]
              [ "variables" .= object []
              ]

          expected :: Value
          expected =
            [interpolateYaml|
              data:
                update_#{schemaName}_author:
                  affected_rows: 1
                  returning:
                  - id: 1
                    name: Tom
            |]

      shouldReturnYaml testEnvironment actual expected

    it "A query involving a nullable variable with null default" \testEnvironment -> do
      let schemaName :: Schema.SchemaName
          schemaName = Schema.getSchemaName testEnvironment

      let actual :: IO Value
          actual =
            postGraphqlWithPair
              testEnvironment
              [graphql|
                mutation($name: String = null) {
                  update_#{schemaName}_author(
                    where: { id: { _eq: 1 } },
                    _set: { name: $name }
                  ) {
                    affected_rows

                    returning {
                      id
                      name
                    }
                  }
                }
              |]
              [ "variables" .= object []
              ]

          expected :: Value
          expected =
            [interpolateYaml|
              data:
                update_#{schemaName}_author:
                  affected_rows: 1
                  returning:
                  - id: 1
                    name: null
            |]

      shouldReturnYaml testEnvironment actual expected
