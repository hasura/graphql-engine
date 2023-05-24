{-# LANGUAGE QuasiQuotes #-}

-- | Test relationships define using multiple columns.
module Test.Regression.MultiColumnObjectRelationshipsSpec (spec) where

import Data.Aeson (Value)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Harness.Backend.BigQuery qualified as BigQuery
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Cockroach qualified as Cockroach
import Harness.Backend.DataConnector.Sqlite qualified as Sqlite
import Harness.Backend.Postgres qualified as Postgres
import Harness.Backend.Sqlserver qualified as Sqlserver
import Harness.GraphqlEngine (postGraphql, postMetadata_)
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (interpolateYaml, yaml)
import Harness.Schema (Table (..), table)
import Harness.Schema qualified as Schema
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, it)

spec :: SpecWith GlobalTestEnvironment
spec = do
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Postgres.setupTablesAction schema testEnv,
                  setupRelationships Postgres.backendTypeMetadata testEnv
                ]
            },
          (Fixture.fixture $ Fixture.Backend Citus.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Citus.setupTablesAction schema testEnv,
                  setupRelationships Citus.backendTypeMetadata testEnv
                ]
            },
          (Fixture.fixture $ Fixture.Backend Sqlserver.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Sqlserver.setupTablesAction schema testEnv,
                  setupRelationships Sqlserver.backendTypeMetadata testEnv
                ]
            },
          (Fixture.fixture $ Fixture.Backend BigQuery.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ BigQuery.setupTablesAction schema testEnv,
                  setupRelationships BigQuery.backendTypeMetadata testEnv
                ],
              Fixture.customOptions =
                Just
                  $ Fixture.defaultOptions
                    { Fixture.stringifyNumbers = True
                    }
            },
          (Fixture.fixture $ Fixture.Backend Cockroach.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Cockroach.setupTablesAction schema testEnv,
                  setupRelationships Cockroach.backendTypeMetadata testEnv
                ]
            },
          (Fixture.fixture $ Fixture.Backend Sqlite.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Sqlite.setupTablesAction schema testEnv,
                  setupRelationships Sqlite.backendTypeMetadata testEnv
                ]
            }
        ]
    )
    tests

--------------------------------------------------------------------------------
-- Schema

schema :: [Schema.Table]
schema =
  [ (table "character")
      { tableColumns =
          [ Schema.column "first_name" Schema.TStr,
            Schema.column "last_name" Schema.TStr
          ],
        tableData =
          [ [Schema.VStr "Kaz", Schema.VStr "Brekker"],
            [Schema.VStr "Inej", Schema.VStr "Ghafa"]
          ]
      },
    (table "quote")
      { tableColumns =
          [ Schema.column "id" Schema.TInt,
            Schema.column "quote" Schema.TStr,
            Schema.column "character_first_name" Schema.TStr,
            Schema.column "character_last_name" Schema.TStr
          ],
        tableData =
          [ [ Schema.VInt 1,
              Schema.VStr "The heart is an arrow. It demands aim to land true.",
              Schema.VStr "Inej",
              Schema.VStr "Ghafa"
            ],
            [ Schema.VInt 2,
              -- skipping the ' in I'm because BigQuery escapes single quotes
              -- differently than any other sql database.
              Schema.VStr "Im a business man. No more, no less.",
              Schema.VStr "Kaz",
              Schema.VStr "Brekker"
            ]
          ]
      }
  ]

setupRelationships :: Fixture.BackendTypeConfig -> TestEnvironment -> Fixture.SetupAction
setupRelationships backendTypeConfig testEnv =
  let backend = T.pack $ Fixture.backendTypeString backendTypeConfig
      source = Fixture.backendSourceName backendTypeConfig
      schemaName = Schema.getSchemaName testEnv
      quoteTable = Schema.mkTableField backendTypeConfig schemaName "quote"
      characterTable = Schema.mkTableField backendTypeConfig schemaName "character"
      createRelationship = backend <> "_create_object_relationship"
      dropRelationship = backend <> "_drop_relationship"
   in Fixture.SetupAction
        { Fixture.setupAction =
            postMetadata_
              testEnv
              [yaml|
          type: *createRelationship
          args:
            source: *source
            name: by
            table: *quoteTable
            using:
              manual_configuration:
                remote_table: *characterTable
                column_mapping:
                  character_first_name: first_name
                  character_last_name: last_name
          |],
          Fixture.teardownAction = \_ ->
            postMetadata_
              testEnv
              [yaml|
          type: *dropRelationship
          args:
            source: *source
            table: *quoteTable
            relationship: by
          |]
        }

--------------------------------------------------------------------------------
-- Tests

tests :: SpecWith TestEnvironment
tests =
  it "Select quotes by characters" \testEnvironment -> do
    let schemaName = Schema.getSchemaName testEnvironment

    let expected :: Value
        expected =
          [interpolateYaml|
            data:
              #{schemaName}_quote:
              - quote: The heart is an arrow. It demands aim to land true.
                by:
                  first_name: Inej
                  last_name: Ghafa
              - quote: Im a business man. No more, no less.
                by:
                  first_name: Kaz
                  last_name: Brekker

          |]

        actual :: IO Value
        actual =
          postGraphql
            testEnvironment
            [graphql|
              query {
                #{schemaName}_quote(order_by: [{ id: asc }]) {
                  quote
                  by {
                    first_name
                    last_name
                  }
                }
              }
            |]

    shouldReturnYaml testEnvironment actual expected
