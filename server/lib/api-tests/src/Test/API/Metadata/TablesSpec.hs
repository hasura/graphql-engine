{-# LANGUAGE QuasiQuotes #-}

module Test.API.Metadata.TablesSpec where

import Data.Aeson (Value (Null))
import Data.List.NonEmpty qualified as NE
import Harness.Backend.BigQuery qualified as BigQuery
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Cockroach qualified as Cockroach
import Harness.Backend.DataConnector.Sqlite qualified as Sqlite
import Harness.Backend.Postgres qualified as Postgres
import Harness.Backend.Sqlserver qualified as Sqlserver
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml.InterpolateYaml
import Harness.Schema qualified as Schema
import Harness.Test.BackendType qualified as BackendType
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment, getBackendTypeConfig)
import Harness.Yaml (shouldAtLeastBe, shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, it)

spec :: SpecWith GlobalTestEnvironment
spec = do
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
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Cockroach.setupTablesAction schema testEnv
                ]
            },
          (Fixture.fixture $ Fixture.Backend Sqlserver.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Sqlserver.setupTablesAction schema testEnvironment
                ]
            },
          (Fixture.fixture $ Fixture.Backend BigQuery.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ BigQuery.setupTablesAction schema testEnvironment
                ],
              Fixture.customOptions =
                Just
                  $ Fixture.defaultOptions
                    { Fixture.stringifyNumbers = True
                    }
            },
          (Fixture.fixture $ Fixture.Backend Sqlite.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Sqlite.setupTablesAction schema testEnvironment
                ]
            }
        ]
    )
    tests

schema :: [Schema.Table]
schema =
  [ (Schema.table "articles")
      { Schema.tableColumns =
          [ Schema.column "id" Schema.TInt,
            Schema.column "author" Schema.TInt,
            Schema.column "title" Schema.TStr
          ],
        Schema.tablePrimaryKey = ["id"]
      },
    (Schema.table "authors")
      { Schema.tableColumns =
          [ Schema.column "id" Schema.TInt,
            Schema.column "name" Schema.TStr
          ],
        Schema.tablePrimaryKey = ["id"]
      }
  ]

untracked :: Schema.Table
untracked =
  (Schema.table "untracked")
    { Schema.tableColumns =
        [ Schema.column "anything" Schema.TStr
        ]
    }

tests :: SpecWith TestEnvironment
tests = do
  it "Returns the source tables" \testEnvironment -> do
    let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
        backendType = BackendType.backendTypeString backendTypeMetadata
        schemaName = Schema.getSchemaName testEnvironment
        sourceName = BackendType.backendSourceName backendTypeMetadata

        actual :: IO Value
        actual =
          GraphqlEngine.postMetadata
            testEnvironment
            [interpolateYaml|
              type: #{backendType}_get_source_tables
              args:
                source: #{sourceName}
            |]

        expected :: Value
        expected = case backendType of
          "bigquery" ->
            [interpolateYaml|
              - dataset: #{schemaName}
                name: articles
              - dataset: #{schemaName}
                name: authors
              - dataset: #{schemaName}
                name: untracked
            |]
          "sqlite" ->
            [interpolateYaml|
              - [#{schemaName}, articles]
              - [#{schemaName}, authors]
              - [#{schemaName}, untracked]
            |]
          _ ->
            [interpolateYaml|
              - schema: #{schemaName}
                name: articles
              - schema: #{schemaName}
                name: authors
              - schema: #{schemaName}
                name: untracked
            |]

    -- We now need a table that isn't tracked to make sure it's still returned
    -- by the command.
    case backendType of
      "bigquery" -> BigQuery.createTable schemaName untracked
      "pg" -> Postgres.createTable testEnvironment untracked
      "citus" -> Citus.createTable testEnvironment untracked
      "cockroach" -> Cockroach.createTable testEnvironment untracked
      "mssql" -> Sqlserver.createTable testEnvironment untracked
      "sqlite" -> Sqlite.createTable (BackendType.backendSourceName backendTypeMetadata) testEnvironment untracked
      b -> error ("Unknown backend: " <> b)

    shouldReturnYaml testEnvironment actual expected

  it "Gets the table info" \testEnvironment -> do
    let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
        backendType = BackendType.backendTypeString backendTypeMetadata
        schemaName = Schema.getSchemaName testEnvironment
        sourceName = BackendType.backendSourceName backendTypeMetadata

        actual :: IO Value
        actual =
          GraphqlEngine.postMetadata
            testEnvironment
            case backendType of
              x
                | x `elem` ["pg", "citus"] ->
                    [interpolateYaml|
                      type: #{ backendType }_get_table_info
                      args:
                        source: #{ sourceName }
                        table:
                          schema: #{ schemaName }
                          name: articles
                    |]
              _ ->
                [interpolateYaml|
                  type: #{ backendType }_get_table_info
                  args:
                    source: #{ sourceName }
                    table:
                      - #{ schemaName }
                      - articles
                |]

        expected :: Value
        expected =
          case backendType of
            "sqlite" ->
              [interpolateYaml|
                columns:
                - insertable: true
                  name: id
                  nullable: false
                  type: number
                  updatable: true
                - insertable: true
                  name: author
                  nullable: false
                  type: number
                  updatable: true
                - insertable: true
                  name: title
                  nullable: false
                  type: string
                  updatable: true
                deletable: true
                insertable: true
                name:
                - main
                - articles
                primary_key:
                - id
                type: table
                updatable: true
              |]
            _ ->
              [interpolateYaml|
                columns:
                - insertable: true
                  name: id
                  nullable: false
                  type: integer
                  updatable: true
                - insertable: true
                  name: author
                  nullable: false
                  type: integer
                  updatable: true
                - insertable: true
                  name: title
                  nullable: false
                  type: text
                  updatable: true
                deletable: true
                insertable: true
                name:
                  schema: #{schemaName}
                  name: articles
                primary_key:
                - id
                type: table
                updatable: true
              |]

    unless (backendType `elem` ["cockroach", "mssql", "bigquery"])
      $ actual
      >>= \result -> result `shouldAtLeastBe` expected

  it "Returns null for an invalid table" \testEnvironment -> do
    let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
        backendType = BackendType.backendTypeString backendTypeMetadata
        schemaName = Schema.getSchemaName testEnvironment
        sourceName = BackendType.backendSourceName backendTypeMetadata

        actual :: IO Value
        actual =
          case backendType of
            "sqlite" ->
              GraphqlEngine.postMetadata
                testEnvironment
                [interpolateYaml|
                  type: #{backendType}_get_table_info
                  args:
                    source: #{sourceName}
                    table:
                      - #{schemaName}
                      - made_up_table
                |]
            _ ->
              GraphqlEngine.postMetadata
                testEnvironment
                [interpolateYaml|
                  type: #{backendType}_get_table_info
                  args:
                    source: #{sourceName}
                    table:
                      schema: #{schemaName}
                      name: made_up_table
                |]

    unless (backendType `elem` ["mssql", "bigquery"])
      $ shouldReturnYaml testEnvironment actual Null
