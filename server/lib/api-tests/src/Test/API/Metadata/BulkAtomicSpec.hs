module Test.API.Metadata.BulkAtomicSpec where

import Data.Aeson (Value)
import Data.List.NonEmpty qualified as NE
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (interpolateYaml)
import Harness.Schema qualified as Schema
import Harness.Test.BackendType qualified as BackendType
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment, getBackendTypeConfig)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, it)

-- ** Preamble

spec :: SpecWith GlobalTestEnvironment
spec =
  Fixture.runClean
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Postgres.setupTablesAction schema testEnvironment
                ]
            }
        ]
    )
    tests

-- ** Schema

schema :: [Schema.Table]
schema =
  [ (Schema.table "authors")
      { Schema.tableColumns =
          [ Schema.column "id" Schema.TInt,
            Schema.column "name" Schema.TStr
          ],
        Schema.tablePrimaryKey = ["id"],
        Schema.tableData =
          [ [ Schema.VInt 1,
              Schema.VStr "Justin"
            ],
            [ Schema.VInt 2,
              Schema.VStr "Beltran"
            ],
            [ Schema.VInt 3,
              Schema.VStr "Sidney"
            ],
            [ Schema.VInt 4,
              Schema.VStr "Anjela"
            ]
          ]
      },
    (Schema.table "articles")
      { Schema.tableColumns =
          [ Schema.column "id" Schema.TInt,
            Schema.column "name" Schema.TStr,
            Schema.column "author_id" Schema.TInt
          ],
        Schema.tablePrimaryKey = ["id"],
        Schema.tableData =
          [ [ Schema.VInt 1,
              Schema.VStr "Article",
              Schema.VInt 1
            ],
            [ Schema.VInt 2,
              Schema.VStr "Brticle",
              Schema.VInt 2
            ],
            [ Schema.VInt 3,
              Schema.VStr "Crticle",
              Schema.VInt 3
            ]
          ]
      }
  ]

tests :: SpecWith TestEnvironment
tests = do
  it "Supports table relationships being added" \testEnvironment -> do
    let schemaName = Schema.getSchemaName testEnvironment
        backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
        sourceName = BackendType.backendSourceName backendTypeMetadata

    GraphqlEngine.postMetadata_
      testEnvironment
      [interpolateYaml|
        type: bulk_atomic
        args:
        - type: pg_create_array_relationship
          args:
            name: articles_by_id_to_author_id
            source: #{sourceName}
            table:
              name: authors
              schema: #{schemaName}
            using:
              manual_configuration:
                remote_table:
                  schema: #{schemaName}
                  name: articles
                column_mapping:
                  id: author_id
        - type: pg_create_object_relationship
          args:
            name: authors_by_author_id_to_id
            source: #{sourceName}
            table:
              name: articles
              schema: #{schemaName}
            using:
              manual_configuration:
                remote_table:
                  schema: #{schemaName}
                  name: authors
                column_mapping:
                  author_id: id
      |]

    let actual :: IO Value
        actual =
          GraphqlEngine.postGraphql
            testEnvironment
            [graphql|
              query {
                #{schemaName}_authors {
                  id
                  name
                  articles_by_id_to_author_id {
                    name
                    authors_by_author_id_to_id { name }
                  }
                }
              }
            |]

    let expected :: Value
        expected =
          [interpolateYaml|
            data:
              #{schemaName}_authors:
              - id: 1
                name: Justin
                articles_by_id_to_author_id:
                  - name: "Article"
                    authors_by_author_id_to_id:
                      name: Justin
              - id: 2
                name: Beltran
                articles_by_id_to_author_id:
                  - name: "Brticle"
                    authors_by_author_id_to_id:
                      name: Beltran
              - id: 3
                name: Sidney
                articles_by_id_to_author_id:
                  - name: "Crticle"
                    authors_by_author_id_to_id:
                      name: Sidney
              - id: 4
                name: Anjela
                articles_by_id_to_author_id: []
          |]

    shouldReturnYaml testEnvironment actual expected

  it "Supports table relationships being added and removed" \testEnvironment -> do
    let schemaName = Schema.getSchemaName testEnvironment
        backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
        sourceName = BackendType.backendSourceName backendTypeMetadata

    GraphqlEngine.postMetadata_
      testEnvironment
      [interpolateYaml|
        type: pg_create_array_relationship
        args:
          name: articles_by_id_to_author_id
          source: #{sourceName}
          table:
            name: authors
            schema: #{schemaName}
          using:
            manual_configuration:
              remote_table:
                schema: #{schemaName}
                name: articles
              column_mapping:
                id: author_id
      |]

    GraphqlEngine.postMetadata_
      testEnvironment
      [interpolateYaml|
        type: bulk_atomic
        args:
        - type: pg_drop_relationship
          args:
            relationship: articles_by_id_to_author_id
            source: #{sourceName}
            table:
              name: authors
              schema: #{schemaName}
      |]

    let actual :: IO Value
        actual =
          GraphqlEngine.postGraphql
            testEnvironment
            [graphql|
              query {
                #{schemaName}_authors {
                  id
                  name
                  articles_by_id_to_author_id {
                    name
                    authors_by_author_id_to_id { name }
                  }
                }
              }
            |]

    let expected :: Value
        expected =
          [interpolateYaml|
            errors:
            - extensions:
                code: validation-failed
                path: $.selectionSet.hasura_authors.selectionSet.articles_by_id_to_author_id
              message: 'field ''articles_by_id_to_author_id'' not found in type: ''hasura_authors'''
          |]

    shouldReturnYaml testEnvironment actual expected

  it "Supports table relationships being added and removed" \testEnvironment -> do
    let schemaName = Schema.getSchemaName testEnvironment
        backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
        sourceName = BackendType.backendSourceName backendTypeMetadata

    GraphqlEngine.postMetadata_
      testEnvironment
      [interpolateYaml|
        type: pg_create_remote_relationship
        args:
          name: articles_far_away
          source: #{sourceName}
          table:
            schema: #{schemaName}
            name: authors
          definition:
            to_source:
              relationship_type: array
              source: #{sourceName}
              table:
                schema: #{schemaName}
                name: articles
              field_mapping:
                id: author_id
      |]

    GraphqlEngine.postMetadata_
      testEnvironment
      [interpolateYaml|
        type: bulk_atomic
        args:
        - type: pg_delete_remote_relationship
          args:
            name: articles_far_away
            source: #{sourceName}
            table:
              name: authors
              schema: #{schemaName}
      |]

    let actual :: IO Value
        actual =
          GraphqlEngine.postGraphql
            testEnvironment
            [graphql|
              query {
                #{schemaName}_authors {
                  id
                  name
                  articles_far_away {
                    name
                  }
                }
              }
            |]

    let expected :: Value
        expected =
          [interpolateYaml|
            errors:
            - extensions:
                code: validation-failed
                path: $.selectionSet.hasura_authors.selectionSet.articles_far_away
              message: 'field ''articles_far_away'' not found in type: ''hasura_authors'''
          |]

    shouldReturnYaml testEnvironment actual expected

  it "Fails on invalid relationships" \testEnvironment -> do
    let schemaName = Schema.getSchemaName testEnvironment
        backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
        sourceName = BackendType.backendSourceName backendTypeMetadata

    let actual :: IO Value
        actual =
          GraphqlEngine.postMetadataWithStatus
            400
            testEnvironment
            [interpolateYaml|
              type: bulk_atomic
              args:
              - type: pg_create_array_relationship
                args:
                  name: articles_by_id_to_author_id
                  source: #{sourceName}
                  table:
                    name: authors
                    schema: #{schemaName}
                  using:
                    manual_configuration:
                      remote_table:
                        schema: #{schemaName}
                        name: this_does_not_exist
                      column_mapping:
                        id: author_id
          |]

    let expected :: Value
        expected =
          [interpolateYaml|
            error: Schema inconsistency
            path: "$.args"
            code: bad-request
            internal:
            - - definition:
                  comment: null
                  name: articles_by_id_to_author_id
                  source: postgres
                  table:
                    name: authors
                    schema: hasura
                  using:
                    manual_configuration:
                      column_mapping:
                        id: author_id
                      insertion_order: null
                      remote_table:
                        name: this_does_not_exist
                        schema: hasura
                name: "array_relation articles_by_id_to_author_id in table hasura.authors in source postgres"
                reason: "Inconsistent object: table \"hasura.this_does_not_exist\" is not tracked"
                type: "array_relation"
          |]

    shouldReturnYaml testEnvironment actual expected
