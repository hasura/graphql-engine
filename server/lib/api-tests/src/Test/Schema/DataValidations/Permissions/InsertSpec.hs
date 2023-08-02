{-# LANGUAGE QuasiQuotes #-}

-- | Test insert permissions
--
-- https://hasura.io/docs/latest/schema/postgres/data-validations/#using-hasura-permissions
module Test.Schema.DataValidations.Permissions.InsertSpec (spec) where

import Control.Lens ((.~))
import Data.Aeson (Value)
import Data.Aeson.Lens (atKey, key, values)
import Data.List.NonEmpty qualified as NE
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Cockroach qualified as Cockroach
import Harness.Backend.DataConnector.Sqlite qualified as Sqlite
import Harness.Backend.Postgres qualified as Postgres
import Harness.Backend.Sqlserver qualified as Sqlserver
import Harness.GraphqlEngine (postGraphqlWithHeaders, postMetadata_)
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (interpolateYaml)
import Harness.Schema (Table (..), table)
import Harness.Schema qualified as Schema
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

spec :: SpecWith GlobalTestEnvironment
spec = do
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Postgres.setupTablesAction schema testEnvironment,
                  setupMetadata Postgres.backendTypeMetadata testEnvironment
                ]
            },
          (Fixture.fixture $ Fixture.Backend Citus.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Citus.setupTablesAction schema testEnvironment,
                  setupMetadata Citus.backendTypeMetadata testEnvironment
                ]
            },
          (Fixture.fixture $ Fixture.Backend Cockroach.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Cockroach.setupTablesAction schema testEnvironment,
                  setupMetadata Cockroach.backendTypeMetadata testEnvironment
                ]
            },
          (Fixture.fixture $ Fixture.Backend Sqlserver.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Sqlserver.setupTablesAction schema testEnvironment,
                  setupMetadata Sqlserver.backendTypeMetadata testEnvironment
                ]
            },
          (Fixture.fixture $ Fixture.Backend Sqlite.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Sqlite.setupTablesAction schema testEnvironment,
                  setupMetadata Sqlite.backendTypeMetadata testEnvironment
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
        tableReferences = [Schema.reference "author_id" "author" "id"]
      }
  ]

--------------------------------------------------------------------------------
-- Tests

tests :: SpecWith TestEnvironment
tests = describe "Permissions on mutations" do
  let -- The error path and internal property differs across backends. Since it's immaterial for the tests we want to make we simply ignore it.
      removeErrorPath :: Value -> Value
      removeErrorPath = key "errors" . values . key "extensions" . atKey "path" .~ Nothing
      removeErrorInternal :: Value -> Value
      removeErrorInternal = key "errors" . values . key "extensions" . atKey "internal" .~ Nothing

  it "Rejects insertions by authors on behalf of others" \testEnvironment -> do
    let schemaName :: Schema.SchemaName
        schemaName = Schema.getSchemaName testEnvironment

    let expected :: Value
        expected =
          [interpolateYaml|
            errors:
            - extensions:
                code: permission-error
              message: check constraint of an insert/update permission has failed
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

    shouldReturnYaml testEnvironment (fmap (removeErrorPath . removeErrorInternal) actual) expected

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

    shouldReturnYaml testEnvironment (fmap (removeErrorPath . removeErrorInternal) actual) expected

  it "Authors can't add other authors" $ \testEnvironment -> do
    let schemaName :: Schema.SchemaName
        schemaName = Schema.getSchemaName testEnvironment

        expected :: Value
        expected =
          [interpolateYaml|
            errors:
            - extensions:
                code: permission-error
              message: check constraint of an insert/update permission has failed
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

    shouldReturnYaml testEnvironment (fmap (removeErrorPath . removeErrorInternal) actual) expected

--------------------------------------------------------------------------------
-- Metadata

setupMetadata :: Fixture.BackendTypeConfig -> TestEnvironment -> Fixture.SetupAction
setupMetadata backendTypeMetadata testEnvironment = do
  let schemaName :: Schema.SchemaName
      schemaName = Schema.getSchemaName testEnvironment

      backendPrefix :: String
      backendPrefix = Fixture.backendTypeString backendTypeMetadata

      source :: String
      source = Fixture.backendSourceName backendTypeMetadata

      articleTable :: Value
      articleTable = Schema.mkTableField backendTypeMetadata schemaName "article"

      authorTable :: Value
      authorTable = Schema.mkTableField backendTypeMetadata schemaName "author"

      setup :: IO ()
      setup =
        postMetadata_
          testEnvironment
          [interpolateYaml|
            type: bulk
            args:
            - type: #{backendPrefix}_create_insert_permission
              args:
                source: #{source}
                table: #{articleTable}
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
            - type: #{backendPrefix}_create_select_permission
              args:
                source: #{source}
                table: #{articleTable}
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
            - type: #{backendPrefix}_create_insert_permission
              args:
                source: #{source}
                table: #{authorTable}
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
            - type: #{backendPrefix}_drop_insert_permission
              args:
                source: #{source}
                table: #{articleTable}
                role: user
            - type: #{backendPrefix}_drop_select_permission
              args:
                source: #{source}
                table: #{articleTable}
                role: user
            - type: #{backendPrefix}_drop_insert_permission
              args:
                source: #{source}
                table: #{authorTable}
                role: user
          |]

  Fixture.SetupAction setup \_ -> teardown
