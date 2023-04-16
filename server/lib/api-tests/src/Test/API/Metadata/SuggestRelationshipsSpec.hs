{-# LANGUAGE QuasiQuotes #-}

-- | All tests related to metadata API for relationship suggestion API
module Test.API.Metadata.SuggestRelationshipsSpec (spec) where

import Data.List.NonEmpty qualified as NE
import Data.Maybe qualified as Maybe
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml (yaml)
import Harness.Schema qualified as Schema
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment, getBackendTypeConfig)
import Harness.Yaml (mapObject, shouldReturnYamlF, sortArray)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

-- ** Preamble

spec :: SpecWith GlobalTestEnvironment
spec =
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Postgres.setupTablesAction schema testEnv,
                  setupMetadata testEnv
                ]
            }
        ]
    )
    tests

-- ** Schema

schema :: [Schema.Table]
schema =
  [ (Schema.table "author")
      { Schema.tableColumns =
          [ Schema.column "id" Schema.TInt,
            Schema.column "name" Schema.TStr
          ],
        Schema.tablePrimaryKey = ["id"]
      },
    (Schema.table "publication")
      { Schema.tableColumns =
          [ Schema.column "id" Schema.TInt,
            Schema.column "name" Schema.TStr
          ],
        Schema.tablePrimaryKey = ["id"]
      },
    (Schema.table "article")
      { Schema.tableColumns =
          [ Schema.column "id" Schema.TInt,
            Schema.column "title" Schema.TStr,
            Schema.column "content" Schema.TStr,
            Schema.column "author_id" Schema.TInt,
            Schema.column "publication_id" Schema.TInt
          ],
        Schema.tablePrimaryKey = ["id"],
        Schema.tableReferences =
          [ Schema.reference "publication_id" "publication" "id",
            Schema.reference "author_id" "author" "id"
          ],
        Schema.tableUniqueIndexes =
          [ Schema.UniqueIndexColumns ["publication_id"]
          ]
      }
  ]

setupMetadata :: TestEnvironment -> Fixture.SetupAction
setupMetadata testEnv = do
  let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnv
      sourceName = Fixture.backendSourceName backendTypeMetadata
      schemaName = Schema.getSchemaName testEnv
      sourceConfiguration = Postgres.defaultSourceConfiguration testEnv

      setup :: IO ()
      setup =
        GraphqlEngine.postMetadata_
          testEnv
          [yaml|
            type: replace_metadata
            args:
              metadata:
                version: 3
                sources:
                  - name: *sourceName
                    kind: postgres
                    configuration: *sourceConfiguration
                    tables:
                      - table:
                          name: author
                          schema: *schemaName
                      - table:
                          name: article
                          schema: *schemaName
                        object_relationships:
                          - name: publication
                            using:
                              foreign_key_constraint_on: publication_id
                      - table:
                          name: publication
                          schema: *schemaName
          |]

      teardown :: IO ()
      teardown =
        GraphqlEngine.postMetadata_
          testEnv
          [yaml|
            type: replace_metadata
            args:
              metadata:
                version: 3
                sources:
                  - name: *sourceName
                    kind: postgres
                    configuration: *sourceConfiguration
                    tables: []
          |]

  Fixture.SetupAction setup (const teardown)

-- * Tests

tests :: SpecWith TestEnvironment
tests = do
  describe "Suggest Relationships" do
    it "Uses reciprocal object relations if there is a unique constraint on the FK column" $ \testEnv -> do
      let backendTypeMetadata = Maybe.fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnv
          sourceName = Fixture.backendSourceName backendTypeMetadata
          schemaName = Schema.unSchemaName $ Schema.getSchemaName testEnv

      shouldReturnYamlF
        testEnv
        (pure . mapObject sortArray)
        ( GraphqlEngine.postMetadataWithStatus
            200
            testEnv
            [yaml|
              type: pg_suggest_relationships
              version: 1
              args:
                source: *sourceName
                tables:
                  - name: article
                    schema: *schemaName
                  - name: publication
                    schema: *schemaName
            |]
        )
        [yaml|
          relationships:
            - from:
                columns:
                  - author_id
                constraint_name: article_author_id_fkey
                table:
                  name: article
                  schema: hasura
              to:
                columns:
                  - id
                table:
                  name: author
                  schema: hasura
              type: object
            - from:
                columns:
                  - id
                table:
                  name: author
                  schema: hasura
              to:
                columns:
                  - author_id
                constraint_name: article_author_id_fkey
                table:
                  name: article
                  schema: hasura
              type: array
            - from:
                columns:
                  - id
                table:
                  name: publication
                  schema: hasura
              to:
                columns:
                  - publication_id
                constraint_name: article_publication_id_fkey
                table:
                  name: article
                  schema: hasura
              type: object
            - from:
                columns:
                  - publication_id
                constraint_name: article_publication_id_fkey
                table:
                  name: article
                  schema: hasura
              to:
                columns:
                  - id
                table:
                  name: publication
                  schema: hasura
              type: object
        |]

    it "Omits tracked relationships if that is requested" $ \testEnv -> do
      let backendTypeMetadata = Maybe.fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnv
          sourceName = Fixture.backendSourceName backendTypeMetadata

      shouldReturnYamlF
        testEnv
        (pure . mapObject sortArray)
        ( GraphqlEngine.postMetadataWithStatus
            200
            testEnv
            [yaml|
              type: pg_suggest_relationships
              version: 1
              args:
                source: *sourceName
                omit_tracked: true
            |]
        )
        [yaml|
          relationships:
            - from:
                columns:
                  - author_id
                constraint_name: article_author_id_fkey
                table:
                  name: article
                  schema: hasura
              to:
                columns:
                  - id
                table:
                  name: author
                  schema: hasura
              type: object
            - from:
                columns:
                  - id
                table:
                  name: author
                  schema: hasura
              to:
                columns:
                  - author_id
                constraint_name: article_author_id_fkey
                table:
                  name: article
                  schema: hasura
              type: array
            - from:
                columns:
                  - id
                table:
                  name: publication
                  schema: hasura
              to:
                columns:
                  - publication_id
                constraint_name: article_publication_id_fkey
                table:
                  name: article
                  schema: hasura
              type: object
        |]

    it "Recommendations should only include listed tables if included" $ \testEnv -> do
      let backendTypeMetadata = Maybe.fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnv
          sourceName = Fixture.backendSourceName backendTypeMetadata

      shouldReturnYamlF
        testEnv
        (pure . mapObject sortArray)
        ( GraphqlEngine.postMetadataWithStatus
            200
            testEnv
            [yaml|
              type: pg_suggest_relationships
              version: 1
              args:
                source: *sourceName
                tables: []
            |]
        )
        [yaml| relationships: [] |]

    it "All recommendations are made by default" $ \testEnv -> do
      let backendTypeMetadata = Maybe.fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnv
          sourceName = Fixture.backendSourceName backendTypeMetadata

      shouldReturnYamlF
        testEnv
        (pure . mapObject sortArray)
        ( GraphqlEngine.postMetadataWithStatus
            200
            testEnv
            [yaml|
              type: pg_suggest_relationships
              version: 1
              args:
                source: *sourceName
            |]
        )
        [yaml|
          relationships:
            - from:
                columns:
                  - author_id
                constraint_name: article_author_id_fkey
                table:
                  name: article
                  schema: hasura
              to:
                columns:
                  - id
                table:
                  name: author
                  schema: hasura
              type: object
            - from:
                columns:
                  - id
                table:
                  name: author
                  schema: hasura
              to:
                columns:
                  - author_id
                constraint_name: article_author_id_fkey
                table:
                  name: article
                  schema: hasura
              type: array
            - from:
                columns:
                  - id
                table:
                  name: publication
                  schema: hasura
              to:
                columns:
                  - publication_id
                constraint_name: article_publication_id_fkey
                table:
                  name: article
                  schema: hasura
              type: object
            - from:
                columns:
                  - publication_id
                constraint_name: article_publication_id_fkey
                table:
                  name: article
                  schema: hasura
              to:
                columns:
                  - id
                table:
                  name: publication
                  schema: hasura
              type: object
        |]
