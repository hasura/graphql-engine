{-# LANGUAGE QuasiQuotes #-}

module Test.API.Metadata.TrackTablesSpec (spec) where

import Data.Aeson qualified as J
import Data.Aeson.Lens (key, _Array)
import Data.List.NonEmpty qualified as NE
import Data.Vector qualified as V
import Harness.Backend.BigQuery qualified as BigQuery
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Cockroach qualified as Cockroach
import Harness.Backend.DataConnector.Sqlite qualified as Sqlite
import Harness.Backend.Postgres qualified as Postgres
import Harness.Backend.Sqlserver qualified as Sqlserver
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (yaml)
import Harness.Quoter.Yaml.InterpolateYaml (interpolateYaml)
import Harness.Schema qualified as Schema
import Harness.Test.BackendType qualified as BackendType
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment, getBackendTypeConfig)
import Harness.Yaml (shouldBeYaml, shouldReturnYaml)
import Hasura.Prelude
import Hasura.RQL.DDL.Warnings (AllowWarnings (..))
import Test.Hspec (SpecWith, it)

spec :: SpecWith GlobalTestEnvironment
spec =
  Fixture.runClean
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Postgres.setupTablesAction trackedTables testEnvironment,
                  Postgres.createUntrackedTablesAction untrackedTables testEnvironment
                ]
            },
          (Fixture.fixture $ Fixture.Backend Citus.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Citus.setupTablesAction trackedTables testEnvironment,
                  Citus.createUntrackedTablesAction untrackedTables testEnvironment
                ]
            },
          (Fixture.fixture $ Fixture.Backend Cockroach.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Cockroach.setupTablesAction trackedTables testEnvironment,
                  Cockroach.createUntrackedTablesAction untrackedTables testEnvironment
                ]
            },
          (Fixture.fixture $ Fixture.Backend Sqlserver.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Sqlserver.setupTablesAction trackedTables testEnvironment,
                  Sqlserver.createUntrackedTablesAction untrackedTables testEnvironment
                ]
            },
          (Fixture.fixture $ Fixture.Backend BigQuery.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ BigQuery.setupTablesAction trackedTables testEnvironment,
                  BigQuery.createUntrackedTablesAction untrackedTables testEnvironment
                ]
            },
          (Fixture.fixture $ Fixture.Backend Sqlite.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Sqlite.setupTablesAction trackedTables testEnvironment,
                  Sqlite.createUntrackedTablesAction untrackedTables testEnvironment
                ]
            }
        ]
    )
    tests

trackedTables :: [Schema.Table]
trackedTables =
  [ (Schema.table "articles")
      { Schema.tableColumns =
          [ Schema.column "id" Schema.TInt,
            Schema.column "author" Schema.TInt,
            Schema.column "publisher" Schema.TInt,
            Schema.column "title" Schema.TStr
          ],
        Schema.tablePrimaryKey = ["id"]
      }
  ]

publishersTable :: Schema.Table
publishersTable =
  (Schema.table "publishers")
    { Schema.tableColumns =
        [ Schema.column "id" Schema.TInt,
          Schema.column "name" Schema.TInt
        ],
      Schema.tablePrimaryKey = ["id"]
    }

authorsTable :: Schema.Table
authorsTable =
  (Schema.table "authors")
    { Schema.tableColumns =
        [ Schema.column "id" Schema.TInt,
          Schema.column "name" Schema.TStr
        ],
      Schema.tablePrimaryKey = ["id"]
    }

untrackedTables :: [Schema.Table]
untrackedTables = [publishersTable, authorsTable]

tests :: SpecWith TestEnvironment
tests = do
  it "returns success with no warnings if all succeed" \testEnvironment -> do
    let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
        sourceName = BackendType.backendSourceName backendTypeMetadata

    actual <- Schema.trackTables sourceName untrackedTables testEnvironment
    actual
      `shouldBeYaml` [yaml|
        message: success
      |]

    -- Check if the tables actually tracked by introspecting the GraphQL schema
    shouldReturnYaml
      testEnvironment
      ( GraphqlEngine.postGraphql
          testEnvironment
          rootQueryFieldsIntrospectionQuery
      )
      (expectedRootQueryFieldsIntrospectionQuery testEnvironment (trackedTables <> untrackedTables))

  it "returns success with warnings if some tables fail to track, and warnings are allowed" \testEnvironment -> do
    let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
        sourceName = BackendType.backendSourceName backendTypeMetadata
        schemaName = Schema.getSchemaName testEnvironment

    -- Include the already-tracked table so that that table fails to track
    actual <- Schema.trackTables sourceName (trackedTables <> untrackedTables) testEnvironment
    actual
      `shouldBeYaml` [interpolateYaml|
        message: success
        warnings:
          - code: track-table-failed
            message: 'view/table already tracked: "#{schemaName}.articles"'
            name: table #{schemaName}.articles in source #{sourceName}
            type: table
      |]

    -- Check if the tables actually tracked by introspecting the GraphQL schema
    shouldReturnYaml
      testEnvironment
      ( GraphqlEngine.postGraphql
          testEnvironment
          rootQueryFieldsIntrospectionQuery
      )
      (expectedRootQueryFieldsIntrospectionQuery testEnvironment (trackedTables <> untrackedTables))

  it "returns success with warnings if some tables fail to track due to metadata inconsistency errors, and warnings are allowed" \testEnvironment -> do
    let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
        sourceName = BackendType.backendSourceName backendTypeMetadata
        schemaName = Schema.getSchemaName testEnvironment

    let nonExistentTables = [Schema.table "nope"]
    -- Include the non-existent table so that that table fails to track
    actual <- Schema.trackTables sourceName (nonExistentTables <> untrackedTables) testEnvironment
    actual
      `shouldBeYaml` [interpolateYaml|
        message: success
        warnings:
          - code: track-table-failed
            message: 'Inconsistent object: no such table/view exists in source: "#{schemaName}.nope"'
            name: table #{schemaName}.nope in source #{sourceName}
            type: table
      |]

    -- Check if the tables actually tracked by introspecting the GraphQL schema
    shouldReturnYaml
      testEnvironment
      ( GraphqlEngine.postGraphql
          testEnvironment
          rootQueryFieldsIntrospectionQuery
      )
      (expectedRootQueryFieldsIntrospectionQuery testEnvironment (trackedTables <> untrackedTables))

  it "fails if some tables fail to track, and warnings are disallowed" \testEnvironment -> do
    let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
        sourceName = BackendType.backendSourceName backendTypeMetadata
        schemaName = Schema.getSchemaName testEnvironment

    -- Include the already-tracked table so that that table fails to track
    actual <- Schema.trackTablesWithStatus sourceName (trackedTables <> untrackedTables) DisallowWarnings 400 testEnvironment
    actual
      `shouldBeYaml` [interpolateYaml|
        code: metadata-warnings
        error: failed due to metadata warnings
        internal:
        - code: track-table-failed
          message: 'view/table already tracked: "#{schemaName}.articles"'
          name: table #{schemaName}.articles in source #{sourceName}
          type: table
        path: $.args
      |]

    -- Check that none of the untracked tables actually tracked by introspecting the GraphQL schema
    shouldReturnYaml
      testEnvironment
      ( GraphqlEngine.postGraphql
          testEnvironment
          rootQueryFieldsIntrospectionQuery
      )
      (expectedRootQueryFieldsIntrospectionQuery testEnvironment trackedTables)

  it "fails if all tables fail to track, and warnings are allowed" \testEnvironment -> do
    let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
        sourceName = BackendType.backendSourceName backendTypeMetadata
        schemaName = Schema.getSchemaName testEnvironment

    let nonExistentTables = [Schema.table "nope", Schema.table "nah"]
    actual <- Schema.trackTablesWithStatus sourceName nonExistentTables AllowWarnings 400 testEnvironment

    let sortInternalErrors json =
          json & (key "internal" . _Array %~ (V.toList >>> sort >>> V.fromList))

    sortInternalErrors actual
      `shouldBeYaml` [interpolateYaml|
        code: invalid-configuration
        error: all tables failed to track
        internal:
        - code: track-table-failed
          message: 'Inconsistent object: no such table/view exists in source: "#{schemaName}.nah"'
          name: table #{schemaName}.nah in source #{sourceName}
          type: table
        - code: track-table-failed
          message: 'Inconsistent object: no such table/view exists in source: "#{schemaName}.nope"'
          name: table #{schemaName}.nope in source #{sourceName}
          type: table
        path: $.args
      |]

rootQueryFieldsIntrospectionQuery :: J.Value
rootQueryFieldsIntrospectionQuery =
  [graphql|
    query {
      __schema {
        queryType {
          fields {
            name
          }
        }
      }
    }
  |]

expectedRootQueryFieldsIntrospectionQuery :: TestEnvironment -> [Schema.Table] -> J.Value
expectedRootQueryFieldsIntrospectionQuery testEnvironment tables =
  let schemaName = Schema.getSchemaName testEnvironment
      backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
      backendType = BackendType.backendType backendTypeMetadata
      rootFieldObjs =
        tables
          & sortOn Schema.tableName
          & concatMap
            ( \Schema.Table {..} ->
                [ [interpolateYaml| name: #{schemaName}_#{tableName} |],
                  [interpolateYaml| name: #{schemaName}_#{tableName}_aggregate |]
                ]
                  <> case backendType of
                    Fixture.BigQuery ->
                      []
                    _ ->
                      [[interpolateYaml| name: #{schemaName}_#{tableName}_by_pk |]]
            )
      fields = J.toJSON rootFieldObjs
   in [yaml|
        data:
          __schema:
            queryType:
              fields: *fields
      |]
