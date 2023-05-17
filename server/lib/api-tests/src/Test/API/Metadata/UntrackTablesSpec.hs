{-# LANGUAGE QuasiQuotes #-}

module Test.API.Metadata.UntrackTablesSpec (spec) where

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
                [ Postgres.setupTablesAction trackedTables testEnvironment
                ]
            },
          (Fixture.fixture $ Fixture.Backend Citus.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Citus.setupTablesAction trackedTables testEnvironment
                ]
            },
          (Fixture.fixture $ Fixture.Backend Cockroach.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Cockroach.setupTablesAction trackedTables testEnvironment
                ]
            },
          (Fixture.fixture $ Fixture.Backend Sqlserver.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Sqlserver.setupTablesAction trackedTables testEnvironment
                ]
            },
          (Fixture.fixture $ Fixture.Backend BigQuery.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ BigQuery.setupTablesAction trackedTables testEnvironment
                ]
            },
          (Fixture.fixture $ Fixture.Backend Sqlite.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Sqlite.setupTablesAction trackedTables testEnvironment
                ]
            }
        ]
    )
    tests

trackedTables :: [Schema.Table]
trackedTables =
  [ publishersTable,
    authorsTable,
    articlesTable,
    usersTable
  ]

articlesTable :: Schema.Table
articlesTable =
  (Schema.table "articles")
    { Schema.tableColumns =
        [ Schema.column "id" Schema.TInt,
          Schema.column "author" Schema.TInt,
          Schema.column "publisher" Schema.TInt,
          Schema.column "title" Schema.TStr
        ],
      Schema.tablePrimaryKey = ["id"],
      Schema.tableReferences =
        [ Schema.reference "author" "authors" "id",
          Schema.reference "publisher" "publishers" "id"
        ]
    }

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

usersTable :: Schema.Table
usersTable =
  (Schema.table "users")
    { Schema.tableColumns =
        [ Schema.column "id" Schema.TInt,
          Schema.column "username" Schema.TStr
        ],
      Schema.tablePrimaryKey = ["id"]
    }

tests :: SpecWith TestEnvironment
tests = do
  it "returns success with no warnings if all succeed" \testEnvironment -> do
    let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
        sourceName = BackendType.backendSourceName backendTypeMetadata
        noCascade = False

    actual <- Schema.untrackTables sourceName (trackedTables <&> (,noCascade)) testEnvironment
    actual
      `shouldBeYaml` [yaml|
        message: success
      |]

    -- Check if the tables actually untracked by introspecting the GraphQL schema
    shouldReturnYaml
      testEnvironment
      ( GraphqlEngine.postGraphql
          testEnvironment
          rootQueryFieldsIntrospectionQuery
      )
      (expectedRootQueryFieldsIntrospectionQuery testEnvironment [])

  it "returns success with no warnings if all succeed, when using cascade to remove dependents" \testEnvironment -> do
    let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
        sourceName = BackendType.backendSourceName backendTypeMetadata
        schemaName = Schema.getSchemaName testEnvironment
        cascade = True

    -- Untrack a table that is unrelated to anything (users) and one that is has relations pointing to it (authors)
    let untrackTables = [authorsTable, usersTable]
    actual <- Schema.untrackTables sourceName (untrackTables <&> (,cascade)) testEnvironment
    actual
      `shouldBeYaml` [interpolateYaml|
        message: success
      |]

    -- Check if the tables actually untracked by introspecting the GraphQL schema
    shouldReturnYaml
      testEnvironment
      ( GraphqlEngine.postGraphql
          testEnvironment
          rootQueryFieldsIntrospectionQuery
      )
      (expectedRootQueryFieldsIntrospectionQuery testEnvironment [publishersTable, articlesTable])

    -- Check if the relationships pointing at author from articles have been removed
    shouldReturnYaml
      testEnvironment
      ( GraphqlEngine.postGraphql
          testEnvironment
          [graphql|
            query {
              __type(name: "#{schemaName}_articles") {
                fields {
                  name
                }
              }
            }
          |]
      )
      [interpolateYaml|
        data:
          __type:
            fields:
              - name: author
              - name: id
              - name: publisher
              - name: publishers_by_publisher_to_id
              - name: title
      |]

  it "returns success with warnings if some tables fail to untrack, and warnings are allowed" \testEnvironment -> do
    let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
        sourceName = BackendType.backendSourceName backendTypeMetadata
        schemaName = Schema.getSchemaName testEnvironment
        noCascade = False

    -- Untrack a table that is unrelated to anything (users) and one that will fail to untrack (authors) because has relations pointing to it
    let untrackTables = [authorsTable, usersTable]
    actual <- Schema.untrackTables sourceName (untrackTables <&> (,noCascade)) testEnvironment
    actual
      `shouldBeYaml` [interpolateYaml|
        message: success
        warnings:
          - code: untrack-table-failed
            message: 'cannot drop due to the following dependent objects: relationship #{schemaName}.articles.authors_by_author_to_id in source "#{sourceName}"'
            name: table #{schemaName}.authors in source #{sourceName}
            type: table
      |]

    -- Check if the tables actually untracked by introspecting the GraphQL schema
    shouldReturnYaml
      testEnvironment
      ( GraphqlEngine.postGraphql
          testEnvironment
          rootQueryFieldsIntrospectionQuery
      )
      (expectedRootQueryFieldsIntrospectionQuery testEnvironment [authorsTable, publishersTable, articlesTable])

  it "fails if some tables fail to untrack, and warnings are disallowed" \testEnvironment -> do
    let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
        sourceName = BackendType.backendSourceName backendTypeMetadata
        schemaName = Schema.getSchemaName testEnvironment
        noCascade = False

    -- Untrack a table that is unrelated to anything (users) and one that will fail to untrack (authors) because has relations pointing to it
    let untrackTables = [authorsTable, usersTable]
    actual <- Schema.untrackTablesWithStatus sourceName (untrackTables <&> (,noCascade)) DisallowWarnings 400 testEnvironment
    actual
      `shouldBeYaml` [interpolateYaml|
        code: metadata-warnings
        error: failed due to metadata warnings
        internal:
          - code: untrack-table-failed
            message: 'cannot drop due to the following dependent objects: relationship #{schemaName}.articles.authors_by_author_to_id in source "#{sourceName}"'
            name: table #{schemaName}.authors in source #{sourceName}
            type: table
        path: $.args
      |]

    -- Check that none of the tracked tables actually untracked by introspecting the GraphQL schema
    shouldReturnYaml
      testEnvironment
      ( GraphqlEngine.postGraphql
          testEnvironment
          rootQueryFieldsIntrospectionQuery
      )
      (expectedRootQueryFieldsIntrospectionQuery testEnvironment [authorsTable, publishersTable, articlesTable, usersTable])

  it "fails if all tables fail to untrack, and warnings are allowed" \testEnvironment -> do
    let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
        sourceName = BackendType.backendSourceName backendTypeMetadata
        schemaName = Schema.getSchemaName testEnvironment
        noCascade = False

    -- Untrack tables that have relations pointing to them so they will fail to untrack
    let untrackTables = [authorsTable, publishersTable]
    actual <- Schema.untrackTablesWithStatus sourceName (untrackTables <&> (,noCascade)) DisallowWarnings 400 testEnvironment

    let sortInternalErrors json =
          json & (key "internal" . _Array %~ (V.toList >>> sort >>> V.fromList))

    sortInternalErrors actual
      `shouldBeYaml` [interpolateYaml|
        code: invalid-configuration
        error: all tables failed to untrack
        internal:
          - code: untrack-table-failed
            message: 'cannot drop due to the following dependent objects: relationship #{schemaName}.articles.authors_by_author_to_id in source "#{sourceName}"'
            name: table #{schemaName}.authors in source #{sourceName}
            type: table
          - code: untrack-table-failed
            message: 'cannot drop due to the following dependent objects: relationship #{schemaName}.articles.publishers_by_publisher_to_id in source "#{sourceName}"'
            name: table #{schemaName}.publishers in source #{sourceName}
            type: table
        path: $.args
      |]

    -- Check that none of the tracked tables actually untracked by introspecting the GraphQL schema
    shouldReturnYaml
      testEnvironment
      ( GraphqlEngine.postGraphql
          testEnvironment
          rootQueryFieldsIntrospectionQuery
      )
      (expectedRootQueryFieldsIntrospectionQuery testEnvironment [authorsTable, publishersTable, articlesTable, usersTable])

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
      noQueriesAvailable = [yaml| name: no_queries_available |]
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
          & \case
            [] -> [noQueriesAvailable]
            objs -> objs
      fields = J.toJSON rootFieldObjs
   in [yaml|
        data:
          __schema:
            queryType:
              fields: *fields
      |]
