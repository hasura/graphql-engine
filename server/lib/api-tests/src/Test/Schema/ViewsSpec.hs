{-# LANGUAGE QuasiQuotes #-}

module Test.Schema.ViewsSpec (spec) where

import Data.Aeson (Value)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Database.PG.Query.Pool (sql)
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Cockroach qualified as Cockroach
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine (postGraphql, postMetadata_)
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
spec =
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Fixture.Postgres)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Postgres.setupTablesAction schema testEnvironment,
                  setupPostgres testEnvironment,
                  setupMetadata Fixture.Postgres testEnvironment
                ]
            },
          (Fixture.fixture $ Fixture.Backend Fixture.Citus)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Citus.setupTablesAction schema testEnvironment,
                  setupCitus testEnvironment,
                  setupMetadata Fixture.Citus testEnvironment
                ]
            },
          (Fixture.fixture $ Fixture.Backend Fixture.Cockroach)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Cockroach.setupTablesAction schema testEnvironment,
                  setupCockroach testEnvironment,
                  setupMetadata Fixture.Cockroach testEnvironment
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

tests :: Fixture.Options -> SpecWith TestEnvironment
tests opts = do
  let shouldBe :: IO Value -> Value -> IO ()
      shouldBe = shouldReturnYaml opts

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

      actual `shouldBe` expected

--------------------------------------------------------------------------------
-- Shared setup

createSQL :: Schema.SchemaName -> String
createSQL schemaName =
  let schemaNameString = T.unpack (Schema.unSchemaName schemaName)
   in "CREATE OR REPLACE VIEW "
        <> schemaNameString
        <> ".author_view AS SELECT id, name FROM "
        <> schemaNameString
        <> ".author"

dropSQL :: Schema.SchemaName -> String
dropSQL schemaName =
  let schemaNameString = T.unpack (Schema.unSchemaName schemaName)
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

setupMetadata :: Fixture.BackendType -> TestEnvironment -> Fixture.SetupAction
setupMetadata backend testEnvironment = do
  let schemaName = Schema.getSchemaName testEnvironment
  Fixture.SetupAction
    { Fixture.setupAction =
        postMetadata_
          testEnvironment
          [yaml|
            type: *track
            args:
              source: *source
              table:
                name: author_view
                schema: hasura
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
                schema: hasura
          |]
    }
  where
    label :: String
    label = Fixture.defaultBackendTypeString backend

    source :: String
    source = Fixture.defaultSource backend

    track :: String
    track = label <> "_track_table"

    untrack :: String
    untrack = label <> "_untrack_table"
