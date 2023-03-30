{-# LANGUAGE QuasiQuotes #-}

-- |
-- Tests for backend-only permissions.
--
-- https://hasura.io/docs/latest/schema/postgres/data-validations/#using-hasura-permissions
module Test.Databases.Postgres.BackendOnlyPermissionsSpec (spec) where

import Data.Aeson (Value)
import Data.ByteString (ByteString)
import Data.List.NonEmpty qualified as NE
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine (postGraphqlWithHeaders, postMetadata_)
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (yaml)
import Harness.Schema (Table (..), table)
import Harness.Schema qualified as Schema
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Network.HTTP.Types.Header (HeaderName)
import Test.Hspec (SpecWith, describe, it)

spec :: SpecWith GlobalTestEnvironment
spec =
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Postgres.setupTablesAction schema testEnvironment,
                  setupMetadata testEnvironment
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

-------------------------------------------------------------------------------
-- Tests

tests :: SpecWith TestEnvironment
tests = do
  describe "Backend-only fields should be exposed to a role with `backend_only` set to `true`" do
    let headers =
          [ ("X-Hasura-Role", "backend_only_role"),
            ("X-Hasura-User-Id", "1"),
            ("X-Hasura-use-backend-only-permissions", "true")
          ]

    it "Update root field should be accesible" \testEnvironment -> do
      let expected :: Value
          expected =
            [yaml|
              data:
                update_hasura_author:
                  affected_rows: 1
            |]

          actual :: IO Value
          actual =
            postGraphqlWithHeaders
              testEnvironment
              headers
              [graphql|
                mutation {
                  update_hasura_author (_set: { name: "Author 1 modified" }, where: {}) {
                    affected_rows
                  }
                }
              |]

      shouldReturnYaml testEnvironment actual expected

    it "Delete root field should be accesible" \testEnvironment -> do
      let expected :: Value
          expected =
            [yaml|
              data:
                delete_hasura_author:
                  affected_rows: 1
            |]

          actual :: IO Value
          actual =
            postGraphqlWithHeaders
              testEnvironment
              headers
              [graphql|
                mutation {
                  delete_hasura_author (where: {}) {
                    affected_rows
                  }
                }
              |]

      shouldReturnYaml testEnvironment actual expected

  describe "Backend-only fields should not be exposed to a role with `backend_only` set to `true` but `X-Hasura-use-backend-only-permissions` set to `false`" do
    let headers :: [(HeaderName, ByteString)]
        headers =
          [ ("X-Hasura-Role", "backend_only_role"),
            ("X-Hasura-User-Id", ""),
            ("X-Hasura-use-backend-only-permissions", "false")
          ]

    it "Update root field should not be accesible" \testEnvironment -> do
      let expected :: Value
          expected =
            [yaml|
              errors:
              - extensions:
                  path: $
                  code: validation-failed
                message: no mutations exist
            |]

          actual :: IO Value
          actual =
            postGraphqlWithHeaders
              testEnvironment
              headers
              [graphql|
                mutation {
                  update_hasura_author (
                    _set: { name: "Author 1 modified" },
                    where: {}
                  ) {
                    affected_rows
                  }
                }
              |]

      shouldReturnYaml testEnvironment actual expected

    it "Delete root field should not be accesible" \testEnvironment -> do
      let expected :: Value
          expected =
            [yaml|
              errors:
              - extensions:
                  path: $
                  code: validation-failed
                message: no mutations exist
            |]

          actual :: IO Value
          actual =
            postGraphqlWithHeaders
              testEnvironment
              headers
              [graphql|
                mutation {
                  delete_hasura_author (where: {}) {
                    affected_rows
                  }
                }
              |]

      shouldReturnYaml testEnvironment actual expected

  describe "Backend-only fields should be exposed to a role with `backend_only` set to `false`" do
    let headers =
          [ ("X-Hasura-Role", "frontend_only_role"),
            ("X-Hasura-User-Id", "2"),
            ("X-Hasura-use-backend-only-permissions", "true")
          ]

    it "Update root field should be accesible" \testEnvironment -> do
      let expected :: Value
          expected =
            [yaml|
              data:
                update_hasura_author:
                  affected_rows: 1
            |]

          actual :: IO Value
          actual =
            postGraphqlWithHeaders
              testEnvironment
              headers
              [graphql|
                mutation {
                  update_hasura_author (
                    _set: { name: "Author 2 modified" },
                    where: {}
                  ) {
                    affected_rows
                  }
                }
              |]

      shouldReturnYaml testEnvironment actual expected

    it "Delete root field should be accesible" \testEnvironment -> do
      let expected :: Value
          expected =
            [yaml|
              data:
                delete_hasura_author:
                  affected_rows: 1
            |]

          actual :: IO Value
          actual =
            postGraphqlWithHeaders
              testEnvironment
              headers
              [graphql|
                mutation {
                  delete_hasura_author (where: {}) {
                    affected_rows
                  }
                }
              |]

      shouldReturnYaml testEnvironment actual expected

--------------------------------------------------------------------------------
-- Metadata

setupMetadata :: TestEnvironment -> Fixture.SetupAction
setupMetadata testEnvironment = do
  let setup :: IO ()
      setup =
        postMetadata_
          testEnvironment
          [yaml|
            type: bulk
            args:
            - type: pg_create_update_permission
              args:
                table:
                  schema: hasura
                  name: author
                source: postgres
                role: backend_only_role
                permission:
                  columns:
                    - name
                  filter:
                    id: x-hasura-user-id
                  check:
                    name:
                      _ne: ""
                  backend_only: true
            - type: pg_create_update_permission
              args:
                table:
                  schema: hasura
                  name: author
                source: postgres
                role: frontend_only_role
                permission:
                  columns:
                    - name
                  filter:
                    id: x-hasura-user-id
                  check:
                    name:
                      _ne: ""
            - type: pg_create_delete_permission
              args:
                table:
                  schema: hasura
                  name: author
                source: postgres
                role: backend_only_role
                permission:
                  filter:
                    id: x-hasura-user-id
                  backend_only: true
            - type: pg_create_delete_permission
              args:
                table:
                  schema: hasura
                  name: author
                source: postgres
                role: frontend_only_role
                permission:
                  filter:
                    id: x-hasura-user-id
          |]

      teardown :: IO ()
      teardown =
        postMetadata_
          testEnvironment
          [yaml|
            type: bulk
            args:
            - type: pg_drop_delete_permission
              args:
                table:
                  schema: hasura
                  name: author
                source: postgres
                role: frontend_only_role
            - type: pg_drop_delete_permission
              args:
                table:
                  schema: hasura
                  name: author
                source: postgres
                role: backend_only_role
            - type: pg_drop_update_permission
              args:
                table:
                  schema: hasura
                  name: author
                source: postgres
                role: frontend_only_role
            - type: pg_drop_update_permission
              args:
                table:
                  schema: hasura
                  name: author
                source: postgres
                role: backend_only_role
          |]

  Fixture.SetupAction setup \_ -> teardown
