{-# LANGUAGE QuasiQuotes #-}

-- | Test insert permissions
--
-- https://hasura.io/docs/latest/schema/bigquery/data-validations/#using-hasura-permissions
-- https://hasura.io/docs/latest/schema/postgres/data-validations/#using-hasura-permissions
module Test.Schema.DataValidation.Permissions.InsertSpec (spec) where

import Data.Aeson (Value)
import Data.List.NonEmpty qualified as NE
import Harness.Backend.Sqlserver qualified as Sqlserver
import Harness.GraphqlEngine (postGraphqlWithHeaders, postMetadata_)
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (interpolateYaml)
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.Schema (Table (..), table)
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (TestEnvironment)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

-- BigQuery should also work if we make a second version of the metadata with
-- "dataset" instead of "schema" as our key name.
spec :: SpecWith TestEnvironment
spec = do
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Fixture.SQLServer)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Sqlserver.setupTablesAction schema testEnvironment,
                  setupMetadata Fixture.SQLServer testEnvironment
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
          [ [Schema.VInt 1, Schema.VStr "Author 1"],
            [Schema.VInt 2, Schema.VStr "Author 2"]
          ]
      },
    (table "article")
      { tableColumns =
          [ Schema.column "id" Schema.TInt,
            Schema.column "title" Schema.TStr,
            Schema.columnNull "content" Schema.TStr,
            Schema.column "author_id" Schema.TInt
          ],
        tablePrimaryKey = ["id"],
        tableReferences = [Schema.Reference "author_id" "author" "id"]
      }
  ]

--------------------------------------------------------------------------------
-- Tests

tests :: Fixture.Options -> SpecWith TestEnvironment
tests opts = do
  let shouldBe :: IO Value -> Value -> IO ()
      shouldBe = shouldReturnYaml opts

  describe "Permissions on mutations" do
    it "Rejects insertions by authors on behalf of others" \testEnvironment -> do
      let schemaName :: Schema.SchemaName
          schemaName = Schema.getSchemaName testEnvironment

      let expected :: Value
          expected =
            [interpolateYaml|
              errors:
              - extensions:
                  path: "$"
                  code: permission-error
                message: check constraint of an insert permission has failed
            |]

          actual :: IO Value
          actual =
            postGraphqlWithHeaders
              testEnvironment
              [ ("X-Hasura-Role", "user"),
                ("X-Hasura-User-Id", "2")
              ]
              [graphql|
                mutation {
                  insert_#{schemaName}_article(objects: [
                    { id: 1, title: "Author 1 article", author_id: 1 }
                  ]) {
                    affected_rows
                  }
                }
              |]

      actual `shouldBe` expected

  it "Allows authors to insert their own articles" \testEnvironment -> do
    let schemaName :: Schema.SchemaName
        schemaName = Schema.getSchemaName testEnvironment

        expected :: Value
        expected =
          [interpolateYaml|
            data:
              insert_#{schemaName}_article:
                returning:
                - author_id: 2
                  content: null
                  id: 1
                  title: Author 2 article
                affected_rows: 1
          |]

        actual :: IO Value
        actual =
          postGraphqlWithHeaders
            testEnvironment
            [ ("X-Hasura-Role", "user"),
              ("X-Hasura-User-Id", "2")
            ]
            [graphql|
              mutation {
                insert_#{schemaName}_article(objects: [
                  { id: 1, title: "Author 2 article", author_id: 2 }
                ]) {
                  affected_rows
                  returning {
                    id
                    title
                    content
                    author_id
                  }
                }
              }
            |]

    actual `shouldBe` expected

  it "Authors can't add other authors" $ \testEnvironment -> do
    let schemaName :: Schema.SchemaName
        schemaName = Schema.getSchemaName testEnvironment

        expected :: Value
        expected =
          [interpolateYaml|
            errors:
            - extensions:
                path: "$"
                code: permission-error
              message: check constraint of an insert permission has failed
          |]

        actual :: IO Value
        actual =
          postGraphqlWithHeaders
            testEnvironment
            [ ("X-Hasura-Role", "user"),
              ("X-Hasura-User-Id", "2")
            ]
            [graphql|
              mutation {
                insert_#{schemaName}_author(objects: [
                  { id: 3, name: "Author 3" }
                ]) {
                  affected_rows
                }
              }
            |]

    actual `shouldBe` expected

--------------------------------------------------------------------------------
-- Metadata

setupMetadata :: Fixture.BackendType -> TestEnvironment -> Fixture.SetupAction
setupMetadata backendType testEnvironment = do
  let schemaName :: Schema.SchemaName
      schemaName = Schema.getSchemaName testEnvironment

      backend :: String
      backend = Fixture.defaultSource backendType

      setup :: IO ()
      setup =
        postMetadata_
          testEnvironment
          [interpolateYaml|
            type: bulk
            args:
            - type: #{backend}_create_insert_permission
              args:
                source: #{backend}
                table:
                  schema: #{schemaName}
                  name: article
                role: user
                permission:
                  check:
                    author_by_author_id_to_id:
                      id: X-Hasura-User-Id
                  columns:
                  - id
                  - title
                  - content
                  - author_id
            - type: #{backend}_create_select_permission
              args:
                source: #{backend}
                table:
                  schema: #{schemaName}
                  name: article
                role: user
                permission:
                  filter:
                    author_by_author_id_to_id:
                      id: X-Hasura-User-Id
                  columns:
                  - id
                  - title
                  - content
                  - author_id
            - type: #{backend}_create_insert_permission
              args:
                source: #{backend}
                table:
                  schema: #{schemaName}
                  name: author
                role: user
                permission:
                  check:
                    id: X-Hasura-User-Id
                  columns:
                  - id
                  - name
          |]

      teardown :: IO ()
      teardown =
        postMetadata_
          testEnvironment
          [interpolateYaml|
            type: bulk
            args:
            - type: #{backend}_drop_insert_permission
              args:
                source: #{backend}
                table:
                  schema: #{schemaName}
                  name: article
                role: user
            - type: #{backend}_drop_select_permission
              args:
                source: #{backend}
                table:
                  schema: #{schemaName}
                  name: article
                role: user
            - type: #{backend}_drop_insert_permission
              args:
                source: #{backend}
                table:
                  schema: #{schemaName}
                  name: author
                role: user
          |]

  Fixture.SetupAction setup \_ -> teardown
