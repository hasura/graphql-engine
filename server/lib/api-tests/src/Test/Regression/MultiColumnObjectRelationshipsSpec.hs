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
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.Schema (Table (..), table)
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (TestEnvironment)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, it)

spec :: SpecWith TestEnvironment
spec = do
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Fixture.Postgres)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Postgres.setupTablesAction schema testEnv,
                  setupRelationships Fixture.Postgres testEnv
                ]
            },
          (Fixture.fixture $ Fixture.Backend Fixture.Citus)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Citus.setupTablesAction schema testEnv,
                  setupRelationships Fixture.Citus testEnv
                ]
            },
          (Fixture.fixture $ Fixture.Backend Fixture.SQLServer)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Sqlserver.setupTablesAction schema testEnv,
                  setupRelationships Fixture.SQLServer testEnv
                ]
            },
          (Fixture.fixture $ Fixture.Backend Fixture.BigQuery)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ BigQuery.setupTablesAction schema testEnv,
                  setupRelationships Fixture.BigQuery testEnv
                ],
              Fixture.customOptions =
                Just $
                  Fixture.defaultOptions
                    { Fixture.stringifyNumbers = True
                    }
            },
          (Fixture.fixture $ Fixture.Backend Fixture.Cockroach)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Cockroach.setupTablesAction schema testEnv,
                  setupRelationships Fixture.Cockroach testEnv
                ]
            },
          (Fixture.fixture $ Fixture.Backend Fixture.DataConnectorSqlite)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Sqlite.setupTablesAction schema testEnv,
                  setupRelationships Fixture.DataConnectorSqlite testEnv
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

setupRelationships :: Fixture.BackendType -> TestEnvironment -> Fixture.SetupAction
setupRelationships backendType testEnv =
  let backend = T.pack $ Fixture.defaultBackendTypeString backendType
      source = Fixture.defaultSource backendType
      schemaName = Schema.getSchemaName testEnv
      quoteTable = Schema.mkTableField backendType schemaName "quote"
      characterTable = Schema.mkTableField backendType schemaName "character"
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

tests :: Fixture.Options -> SpecWith TestEnvironment
tests opts = do
  let shouldBe :: IO Value -> Value -> IO ()
      shouldBe = shouldReturnYaml opts

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

    actual `shouldBe` expected
