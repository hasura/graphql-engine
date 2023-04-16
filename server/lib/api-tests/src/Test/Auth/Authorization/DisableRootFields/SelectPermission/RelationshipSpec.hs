{-# LANGUAGE QuasiQuotes #-}

-- | There are often situations, when you want to make the columns of a table accessible
-- via a relationship, but not allow having any root fields for a particular table.
-- This tests tests such a situation and does the following:
--    * the root fields of 'article' table are disabled, but you can access the
--      columns of the 'articles' table via the the relationship from the article
--      table to the author table. This mean, you cannot query 'articles' table
--      directly.
module Test.Auth.Authorization.DisableRootFields.SelectPermission.RelationshipSpec (spec) where

import Data.List.NonEmpty qualified as NE
import Harness.Backend.Postgres qualified as Postgres
import Harness.Backend.Sqlserver qualified as SQLServer
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (yaml)
import Harness.Schema (Table (..), table)
import Harness.Schema qualified as Schema
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

--------------------------------------------------------------------------------
-- Preamble

spec :: SpecWith GlobalTestEnvironment
spec =
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Postgres.setupTablesAction schema testEnv
                ]
                  <> postgresSetupPermissions testEnv
            },
          (Fixture.fixture $ Fixture.Backend SQLServer.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ SQLServer.setupTablesAction schema testEnv
                ]
                  <> sqlserverSetupPermissions testEnv
            }
        ]
    )
    tests

--------------------------------------------------------------------------------

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
          Schema.columnNull "author_id" Schema.TInt
        ],
      tablePrimaryKey = ["id"],
      tableReferences =
        [ Schema.reference "author_id" "author" "id"
        ],
      tableData =
        [ [ Schema.VInt 1,
            Schema.VStr "Article 1",
            Schema.VInt 1
          ],
          [ Schema.VInt 2,
            Schema.VStr "Article 2",
            Schema.VInt 2
          ],
          [ Schema.VInt 3,
            Schema.VStr "Article 3",
            Schema.VInt 1
          ]
        ]
    }

--------------------------------------------------------------------------------
-- Setting up Postgres

-- No 'article' root fields will be exposed.
-- This scenario tests, when we want to disable querying a specific table but allow
-- it's row's and columns to be accessible when used from within a relationship.
postgresSetupPermissions :: TestEnvironment -> [Fixture.SetupAction]
postgresSetupPermissions testEnv =
  [ Fixture.SetupAction
      { setupAction =
          GraphqlEngine.postMetadata_
            testEnv
            [yaml|
          type: pg_create_select_permission
          args:
            source: postgres
            table:
              schema: hasura
              name: article
            role: user
            permission:
              filter: {}
              columns: '*'
              query_root_fields: []
              subscription_root_fields: []
          |],
        teardownAction = \_ ->
          GraphqlEngine.postMetadata_
            testEnv
            [yaml|
          type: pg_drop_select_permission
          args:
            source: postgres
            table:
              schema: hasura
              name: article
            role: user
          |]
      },
    Fixture.SetupAction
      { setupAction =
          GraphqlEngine.postMetadata_
            testEnv
            [yaml|
          type: pg_create_select_permission
          args:
            source: postgres
            table:
              schema: hasura
              name: author
            role: user
            permission:
              filter:
                articles_by_id_to_author_id:
                  author_id:
                    _eq:  X-Hasura-User-Id
              columns: '*'
              allow_aggregations: true
              query_root_fields: ["select", "select_by_pk", "select_aggregate"]
              subscription_root_fields: ["select_stream"]
          |],
        teardownAction = \_ ->
          GraphqlEngine.postMetadata_
            testEnv
            [yaml|
          type: pg_drop_select_permission
          args:
            source: postgres
            table:
              schema: hasura
              name: author
            role: user
          |]
      }
  ]

--------------------------------------------------------------------------------
-- Setting up SQL server

-- No 'article' root fields will be exposed.
-- This scenario tests, when we want to disable querying a specific table but allow
-- it's row's and columns to be accessible when used from within a relationship.
sqlserverSetupPermissions :: TestEnvironment -> [Fixture.SetupAction]
sqlserverSetupPermissions testEnv =
  [ Fixture.SetupAction
      { setupAction =
          GraphqlEngine.postMetadata_
            testEnv
            [yaml|
          type: mssql_create_select_permission
          args:
            source: mssql
            table:
              schema: hasura
              name: article
            role: user
            permission:
              filter: {}
              columns: '*'
              query_root_fields: []
              subscription_root_fields: []
          |],
        teardownAction = \_ ->
          GraphqlEngine.postMetadata_
            testEnv
            [yaml|
          type: mssql_drop_select_permission
          args:
            source: mssql
            table:
              schema: hasura
              name: article
            role: user
          |]
      },
    Fixture.SetupAction
      { setupAction =
          GraphqlEngine.postMetadata_
            testEnv
            [yaml|
          type: mssql_create_select_permission
          args:
            source: mssql
            table:
              schema: hasura
              name: author
            role: user
            permission:
              filter:
                articles_by_id_to_author_id:
                  author_id:
                    _eq:  X-Hasura-User-Id
              columns: '*'
              allow_aggregations: true
              query_root_fields: ["select", "select_by_pk", "select_aggregate"]
              subscription_root_fields: ["select_stream"]
          |],
        teardownAction = \_ ->
          GraphqlEngine.postMetadata_
            testEnv
            [yaml|
          type: mssql_drop_select_permission
          args:
            source: mssql
            table:
              schema: hasura
              name: author
            role: user
          |]
      }
  ]

--------------------------------------------------------------------------------
-- Tests

tests :: SpecWith TestEnvironment
tests = describe "RelationshipSpec" $ do
  let userHeaders = [("X-Hasura-Role", "user"), ("X-Hasura-User-Id", "1")]
  it "query root: 'list' root field for 'author' is accessible" $ \testEnvironment -> do
    let authorArticlesQuery =
          [graphql|
          query {
            # we put id=1 restrictions here because we don't assume ordering support
            hasura_author {
              id
              # the _by_id_to_author_id part is necessary to distinguish between multiple foreign key relationships between the same two tables
              articles_by_id_to_author_id {
                title
              }
            }
          }
          |]

        expectedResponse =
          [yaml|
          data:
            hasura_author:
            - id: 1
              articles_by_id_to_author_id:
                - title: Article 1
                - title: Article 3
          |]

    shouldReturnYaml
      testEnvironment
      (GraphqlEngine.postGraphqlWithHeaders testEnvironment userHeaders authorArticlesQuery)
      expectedResponse

  it "query root: 'list' root field for 'article' is disabled and not accessible" $ \testEnvironment -> do
    let articleQuery =
          [graphql|
          query {
            hasura_article{
              id
            }
          }
          |]

        expectedResponse =
          [yaml|
          errors:
            - extensions:
                path: $.selectionSet.hasura_article
                code: validation-failed
              message: |-
                field 'hasura_article' not found in type: 'query_root'
          |]

    shouldReturnYaml
      testEnvironment
      (GraphqlEngine.postGraphqlWithHeaders testEnvironment userHeaders articleQuery)
      expectedResponse
