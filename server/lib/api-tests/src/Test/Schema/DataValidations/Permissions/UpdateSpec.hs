{-# LANGUAGE QuasiQuotes #-}

-- | Test update permissions
--
-- https://hasura.io/docs/latest/schema/postgres/data-validations/#using-hasura-permissions
module Test.Schema.DataValidations.Permissions.UpdateSpec (spec) where

import Data.Aeson (Value)
import Data.List.NonEmpty qualified as NE
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Cockroach qualified as Cockroach
import Harness.Backend.DataConnector.Sqlite qualified as Sqlite
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine (postGraphqlWithHeaders, postMetadata_)
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
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Postgres.setupTablesAction schema testEnvironment,
                  setupMetadata Postgres.backendTypeMetadata testEnvironment
                ]
            },
          (Fixture.fixture $ Fixture.Backend Citus.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Citus.setupTablesAction schema testEnvironment,
                  setupMetadata Citus.backendTypeMetadata testEnvironment
                ]
            },
          (Fixture.fixture $ Fixture.Backend Cockroach.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Cockroach.setupTablesAction schema testEnvironment,
                  setupMetadata Cockroach.backendTypeMetadata testEnvironment
                ]
            },
          (Fixture.fixture $ Fixture.Backend Sqlite.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Sqlite.setupTablesAction schema testEnvironment,
                  setupMetadata Sqlite.backendTypeMetadata testEnvironment
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
            Schema.column "name" Schema.TStr
          ],
        tablePrimaryKey = ["id"],
        tableData =
          [ [Schema.VInt 1, Schema.VStr "Phil"],
            [Schema.VInt 2, Schema.VStr "Will"]
          ]
      }
  ]

--------------------------------------------------------------------------------
-- Tests

tests :: SpecWith TestEnvironment
tests =
  describe "Permissions on mutations" do
    it "Author can update their name" \testEnvironment -> do
      let schemaName :: Schema.SchemaName
          schemaName = Schema.getSchemaName testEnvironment

          expected :: Value
          expected =
            [interpolateYaml|
              data:
                update_#{schemaName}_author:
                  returning:
                    - id: 1
                      name: "Bill"
            |]

          actual :: IO Value
          actual =
            postGraphqlWithHeaders
              testEnvironment
              [ ("X-Hasura-Role", "user"),
                ("X-Hasura-User-Id", "1")
              ]
              [graphql|
                mutation MyMutation {
                  update_#{schemaName}_author(where: {}, _set: {name: "Bill"}) {
                    returning {
                      id
                      name
                    }
                  }
                }
              |]

      shouldReturnYaml testEnvironment actual expected

    it "Author can update_many their name" \testEnvironment -> do
      let schemaName :: Schema.SchemaName
          schemaName = Schema.getSchemaName testEnvironment

          expected :: Value
          expected =
            [interpolateYaml|
              data:
                update_#{schemaName}_author_many:
                  - returning:
                    - id: 1
                      name: "Nill"
            |]

          actual :: IO Value
          actual =
            postGraphqlWithHeaders
              testEnvironment
              [ ("X-Hasura-Role", "user"),
                ("X-Hasura-User-Id", "1")
              ]
              [graphql|
                mutation MyMutation {
                  update_#{schemaName}_author_many(updates: {where: {}, _set: {name: "Nill"}}) {
                    returning {
                      id
                      name
                    }
                  }
                }
              |]

      shouldReturnYaml testEnvironment actual expected

    it "Author cannot update their id" \testEnvironment -> do
      let schemaName :: Schema.SchemaName
          schemaName = Schema.getSchemaName testEnvironment

          expected :: Value
          expected =
            [interpolateYaml|
            errors:
            - extensions:
                code: validation-failed
                path: "$.selectionSet.update_#{schemaName}_author.args._set.id"
              message: "field 'id' not found in type: '#{schemaName}_author_set_input'"
            |]

          actual :: IO Value
          actual =
            postGraphqlWithHeaders
              testEnvironment
              [ ("X-Hasura-Role", "user"),
                ("X-Hasura-User-Id", "1")
              ]
              [graphql|
                mutation MyMutation {
                  update_#{schemaName}_author(where: {}, _set: {id: 777}) {
                    returning {
                      id
                      name
                    }
                  }
                }
              |]

      shouldReturnYaml testEnvironment actual expected

    it "Author cannot update_many their id" \testEnvironment -> do
      let schemaName :: Schema.SchemaName
          schemaName = Schema.getSchemaName testEnvironment

          expected :: Value
          expected =
            [interpolateYaml|
            errors:
            - extensions:
                code: validation-failed
                path: "$.selectionSet.update_#{schemaName}_author_many.args.updates[0]._set.id"
              message: "field 'id' not found in type: '#{schemaName}_author_set_input'"
            |]

          actual :: IO Value
          actual =
            postGraphqlWithHeaders
              testEnvironment
              [ ("X-Hasura-Role", "user"),
                ("X-Hasura-User-Id", "1")
              ]
              [graphql|
                mutation MyMutation {
                  update_#{schemaName}_author_many(updates: {where: {}, _set: {id: 777}}) {
                    returning {
                      id
                      name
                    }
                  }
                }
              |]

      shouldReturnYaml testEnvironment actual expected

--------------------------------------------------------------------------------
-- Metadata

setupMetadata :: Fixture.BackendTypeConfig -> TestEnvironment -> Fixture.SetupAction
setupMetadata backendTypeMetadata testEnvironment = do
  let schemaName :: Schema.SchemaName
      schemaName = Schema.getSchemaName testEnvironment

      backendPrefix :: String
      backendPrefix = Fixture.backendTypeString backendTypeMetadata

      source :: String
      source = Fixture.backendSourceName backendTypeMetadata

      authorTable :: Value
      authorTable = Schema.mkTableField backendTypeMetadata schemaName "author"

      setup :: IO ()
      setup =
        postMetadata_
          testEnvironment
          [interpolateYaml|
            type: bulk
            args:
            - type: #{backendPrefix}_create_select_permission
              args:
                source: #{source}
                table: #{authorTable}
                role: user
                permission:
                  filter:
                    id:
                      _eq: X-Hasura-User-Id
                  columns:
                  - id
                  - name
            - type: #{backendPrefix}_create_update_permission
              args:
                source: #{source}
                table: #{authorTable}
                role: user
                permission:
                  filter:
                    id:
                      _eq: X-Hasura-User-Id
                  columns:
                  - name
          |]

      teardown :: IO ()
      teardown =
        postMetadata_
          testEnvironment
          [interpolateYaml|
            type: bulk
            args:
            - type: #{backendPrefix}_drop_select_permission
              args:
                source: #{source}
                table: #{authorTable}
                role: user
            - type: #{backendPrefix}_drop_update_permission
              args:
                source: #{source}
                table: #{authorTable}
                role: user
          |]

  Fixture.SetupAction setup \_ -> teardown
