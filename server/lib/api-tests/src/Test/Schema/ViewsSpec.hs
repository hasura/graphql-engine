{-# LANGUAGE QuasiQuotes #-}

module Test.Schema.ViewsSpec (spec) where

import Data.Aeson (Value)
import Data.List.NonEmpty qualified as NE
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Cockroach qualified as Cockroach
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine (postGraphql, postMetadata_)
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (yaml)
import Harness.Schema (Table (..), table)
import Harness.Schema qualified as Schema
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

spec :: SpecWith GlobalTestEnvironment
spec =
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Postgres.setupTablesAction schema testEnvironment,
                  setupPostgres testEnvironment,
                  setupMetadata Postgres.backendTypeMetadata testEnvironment
                ]
            },
          (Fixture.fixture $ Fixture.Backend Citus.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Citus.setupTablesAction schema testEnvironment,
                  setupCitus testEnvironment,
                  setupMetadata Citus.backendTypeMetadata testEnvironment
                ]
            },
          (Fixture.fixture $ Fixture.Backend Cockroach.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Cockroach.setupTablesAction schema testEnvironment,
                  setupCockroach testEnvironment,
                  setupMetadata Cockroach.backendTypeMetadata testEnvironment
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
          [ [Schema.VInt 1, Schema.VStr "Alice"],
            [Schema.VInt 2, Schema.VStr "Bob"]
          ]
      }
  ]

--------------------------------------------------------------------------------
-- Tests

tests :: SpecWith TestEnvironment
tests =
  describe "Queries involving views" do
    it "Queries views correctly" \testEnvironment -> do
      let expected :: Value
          expected =
            [yaml|
              data:
                hasura_author_view:
                - id: 1
                  name: Alice
            |]

          actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                query {
                  hasura_author_view(where: { id: { _eq: 1 } }) {
                    id
                    name
                  }
                }
              |]

      shouldReturnYaml testEnvironment actual expected

--------------------------------------------------------------------------------
-- Shared setup

createSQL :: Schema.SchemaName -> Text
createSQL schemaName =
  let schemaNameString = Schema.unSchemaName schemaName
   in "CREATE OR REPLACE VIEW "
        <> schemaNameString
        <> ".author_view AS SELECT id, name FROM "
        <> schemaNameString
        <> ".author"

dropSQL :: Schema.SchemaName -> Text
dropSQL schemaName =
  let schemaNameString = Schema.unSchemaName schemaName
   in "DROP VIEW IF EXISTS " <> schemaNameString <> ".author_view"

--------------------------------------------------------------------------------
-- Postgres setup

setupPostgres :: TestEnvironment -> Fixture.SetupAction
setupPostgres testEnvironment = do
  let schemaName = Schema.getSchemaName testEnvironment
  Fixture.SetupAction
    { Fixture.setupAction =
        Postgres.run_ testEnvironment (createSQL schemaName),
      Fixture.teardownAction = \_ ->
        Postgres.run_ testEnvironment (dropSQL schemaName)
    }

--------------------------------------------------------------------------------
-- Citus setup

setupCitus :: TestEnvironment -> Fixture.SetupAction
setupCitus testEnvironment = do
  let schemaName = Schema.getSchemaName testEnvironment
  Fixture.SetupAction
    { Fixture.setupAction =
        Citus.run_ testEnvironment (createSQL schemaName),
      Fixture.teardownAction = \_ ->
        Citus.run_ testEnvironment (dropSQL schemaName)
    }

--------------------------------------------------------------------------------
-- Cockroach setup

setupCockroach :: TestEnvironment -> Fixture.SetupAction
setupCockroach testEnvironment = do
  let schemaName = Schema.getSchemaName testEnvironment
  Fixture.SetupAction
    { Fixture.setupAction =
        Cockroach.run_ testEnvironment (createSQL schemaName),
      Fixture.teardownAction = \_ ->
        Cockroach.run_ testEnvironment (dropSQL schemaName)
    }

--------------------------------------------------------------------------------
-- Metadata

setupMetadata :: Fixture.BackendTypeConfig -> TestEnvironment -> Fixture.SetupAction
setupMetadata backendMetadata testEnvironment =
  let schemaName = Schema.getSchemaName testEnvironment
   in Fixture.SetupAction
        { Fixture.setupAction =
            postMetadata_
              testEnvironment
              [yaml|
            type: *track
            args:
              source: *source
              table:
                name: author_view
                schema: *schemaName
          |],
          Fixture.teardownAction = \_ ->
            postMetadata_
              testEnvironment
              [yaml|
            type: *untrack
            args:
              source: *source
              table:
                name: author_view
                schema: *schemaName
          |]
        }
  where
    label :: String
    label = Fixture.backendTypeString backendMetadata

    source :: String
    source = Fixture.backendSourceName backendMetadata

    track :: String
    track = label <> "_track_table"

    untrack :: String
    untrack = label <> "_untrack_table"
