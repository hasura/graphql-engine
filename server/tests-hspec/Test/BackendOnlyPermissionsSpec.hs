{-# LANGUAGE QuasiQuotes #-}

-- | Test backend only permissions
module Test.BackendOnlyPermissionsSpec (spec) where

import Data.List.NonEmpty qualified as NE
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (yaml)
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.Schema (Table (..), table)
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (TestEnvironment)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

--------------------------------------------------------------------------------

-- ** Preamble

spec :: SpecWith TestEnvironment
spec =
  -- Postgres
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Fixture.Postgres)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Postgres.setupTablesAction schema testEnv
                ]
                  <> postgresPermissionsSetup testEnv
            }
        ]
    )
    tests

--------------------------------------------------------------------------------

-- ** Schema

schema :: [Schema.Table]
schema = [author, article]

author :: Schema.Table
author =
  (table "author")
    { tableColumns =
        [ Schema.column "id" Schema.TInt,
          Schema.column "name" Schema.TStr
        ],
      tablePrimaryKey = ["id"],
      tableData =
        [ [Schema.VInt 1, Schema.VStr "Author 1"],
          [Schema.VInt 2, Schema.VStr "Author 2"]
        ]
    }

article :: Schema.Table
article =
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

--------------------------------------------------------------------------------

-- ** Postgres Setup and teardown

postgresPermissionsSetup :: TestEnvironment -> [Fixture.SetupAction]
postgresPermissionsSetup testEnvironment =
  [ Fixture.SetupAction
      { setupAction =
          GraphqlEngine.postMetadata_
            testEnvironment
            [yaml|
          type: pg_create_update_permission
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
          |],
        teardownAction = \_ ->
          GraphqlEngine.postMetadata_
            testEnvironment
            [yaml|
          type: pg_drop_update_permission
          args:
            table:
              schema: hasura
              name: author
            source: postgres
            role: backend_only_role
          |]
      },
    Fixture.SetupAction
      { setupAction =
          GraphqlEngine.postMetadata_
            testEnvironment
            [yaml|
            type: pg_create_update_permission
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
            |],
        teardownAction = \_ ->
          GraphqlEngine.postMetadata_
            testEnvironment
            [yaml|
            type: pg_drop_update_permission
            args:
              table:
                schema: hasura
                name: author
              source: postgres
              role: frontend_only_role
            |]
      },
    Fixture.SetupAction
      { setupAction =
          GraphqlEngine.postMetadata_
            testEnvironment
            [yaml|
            type: pg_create_delete_permission
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
            |],
        teardownAction = \_ ->
          GraphqlEngine.postMetadata_
            testEnvironment
            [yaml|
            type: pg_drop_delete_permission
            args:
              table:
                schema: hasura
                name: author
              source: postgres
              role: backend_only_role
            |]
      },
    Fixture.SetupAction
      { setupAction =
          GraphqlEngine.postMetadata_
            testEnvironment
            [yaml|
            type: pg_create_delete_permission
            args:
              table:
                schema: hasura
                name: author
              source: postgres
              role: frontend_only_role
              permission:
                filter:
                  id: x-hasura-user-id
            |],
        teardownAction = \_ ->
          GraphqlEngine.postMetadata_
            testEnvironment
            [yaml|
            type: pg_drop_delete_permission
            args:
              table:
                schema: hasura
                name: author
              source: postgres
              role: frontend_only_role
            |]
      }
  ]

-------------------------------------------------------------------------------

tests :: Fixture.Options -> SpecWith TestEnvironment
tests opts = do
  describe "backend-only fields should be exposed to a role with `backend_only` set to `true`" $ do
    let userHeaders = [("X-Hasura-Role", "backend_only_role"), ("X-Hasura-User-Id", "1"), ("X-Hasura-use-backend-only-permissions", "true")]
    it "update root field should be accesible" $ \testEnvironment -> do
      let query =
            [graphql|
            mutation {
              update_hasura_author (_set: {name: "Author 1 modified"}, where: {}) {
                affected_rows
              }
            }
          |]
          expectedResponse =
            [yaml|
            data:
              update_hasura_author:
                affected_rows: 1
          |]

      shouldReturnYaml
        opts
        (GraphqlEngine.postGraphqlWithHeaders testEnvironment userHeaders query)
        expectedResponse

    it "delete root field should be accesible" $ \testEnvironment -> do
      let query =
            [graphql|
            mutation {
              delete_hasura_author (where: {}) {
                affected_rows
              }
            }
          |]
          expectedResponse =
            [yaml|
            data:
              delete_hasura_author:
                affected_rows: 1
          |]

      shouldReturnYaml
        opts
        (GraphqlEngine.postGraphqlWithHeaders testEnvironment userHeaders query)
        expectedResponse

  describe "backend-only fields should not be exposed to a role with `backend_only` set to `true` but `X-Hasura-use-backend-only-permissions` set to `false`" $ do
    let userHeaders = [("X-Hasura-Role", "backend_only_role"), ("X-Hasura-User-Id", ""), ("X-Hasura-use-backend-only-permissions", "false")]
    it "update root field should not be accesible" $ \testEnvironment -> do
      let query =
            [graphql|
            mutation {
              update_hasura_author (_set: {name: "Author 1 modified"}, where: {}) {
                affected_rows
              }
            }
          |]
          expectedResponse =
            [yaml|
            errors:
            - extensions:
                path: $
                code: validation-failed
              message: no mutations exist
            |]

      shouldReturnYaml
        opts
        (GraphqlEngine.postGraphqlWithHeaders testEnvironment userHeaders query)
        expectedResponse

    it "delete root field should not be accesible" $ \testEnvironment -> do
      let query =
            [graphql|
            mutation {
              delete_hasura_author (where: {}) {
                affected_rows
              }
            }
          |]
          expectedResponse =
            [yaml|
            errors:
            - extensions:
                path: $
                code: validation-failed
              message: no mutations exist
          |]

      shouldReturnYaml
        opts
        (GraphqlEngine.postGraphqlWithHeaders testEnvironment userHeaders query)
        expectedResponse

  describe "backend-only fields should be exposed to a role with `backend_only` set to `false`" $ do
    let userHeaders = [("X-Hasura-Role", "frontend_only_role"), ("X-Hasura-User-Id", "2"), ("X-Hasura-use-backend-only-permissions", "true")]
    it "update root field should be accesible" $ \testEnvironment -> do
      let query =
            [graphql|
            mutation {
              update_hasura_author (_set: {name: "Author 2 modified"}, where: {}) {
                affected_rows
              }
            }
          |]
          expectedResponse =
            [yaml|
            data:
              update_hasura_author:
                affected_rows: 1
          |]

      shouldReturnYaml
        opts
        (GraphqlEngine.postGraphqlWithHeaders testEnvironment userHeaders query)
        expectedResponse

    it "delete root field should be accesible" $ \testEnvironment -> do
      let query =
            [graphql|
            mutation {
              delete_hasura_author (where: {}) {
                affected_rows
              }
            }
          |]
          expectedResponse =
            [yaml|
            data:
              delete_hasura_author:
                affected_rows: 1
          |]

      shouldReturnYaml
        opts
        (GraphqlEngine.postGraphqlWithHeaders testEnvironment userHeaders query)
        expectedResponse
