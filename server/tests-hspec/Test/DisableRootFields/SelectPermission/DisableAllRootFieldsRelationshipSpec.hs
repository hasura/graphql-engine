{-# LANGUAGE QuasiQuotes #-}

-- | There are often situations, when you want to make the columns of a table accessible
-- via a relationship, but not allow having any root fields for a particular table.
-- This tests tests such a situation and does the following:
--    * the root fields of 'article' table are disabled, but you can access the
--      columns of the 'articles' table via the the relationship from the article
--      table to the author table. This mean, you cannot query 'articles' table
--      directly.
module Test.DisableRootFields.SelectPermission.DisableAllRootFieldsRelationshipSpec (spec) where

import Harness.Backend.Postgres qualified as Postgres
import Harness.Backend.Sqlserver qualified as SQLServer
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (shouldReturnYaml, yaml)
import Harness.Test.Context qualified as Context
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (TestEnvironment)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

--------------------------------------------------------------------------------
-- Preamble

spec :: SpecWith TestEnvironment
spec =
  Context.run
    [ Context.Context
        { name = Context.Backend Context.Postgres,
          mkLocalTestEnvironment = Context.noLocalTestEnvironment,
          setup = postgresSetup,
          teardown = postgresTeardown,
          customOptions = Nothing
        },
      Context.Context
        { name = Context.Backend Context.SQLServer,
          mkLocalTestEnvironment = Context.noLocalTestEnvironment,
          setup = sqlServerSetup,
          teardown = sqlServerTeardown,
          customOptions = Nothing
        }
    ]
    tests

--------------------------------------------------------------------------------

schema :: [Schema.Table]
schema = [author, article]

author :: Schema.Table
author =
  Schema.Table
    "author"
    [ Schema.column "id" Schema.TInt,
      Schema.column "name" Schema.TStr
    ]
    ["id"]
    []
    [ [Schema.VInt 1, Schema.VStr "Author 1"],
      [Schema.VInt 2, Schema.VStr "Author 2"]
    ]

article :: Schema.Table
article =
  Schema.Table
    "article"
    [ Schema.column "id" Schema.TInt,
      Schema.column "title" Schema.TStr,
      Schema.columnNull "author_id" Schema.TInt
    ]
    ["id"]
    [ Schema.Reference "author_id" "author" "id"
    ]
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

--------------------------------------------------------------------------------
-- Setting up Postgres

postgresSetup :: (TestEnvironment, ()) -> IO ()
postgresSetup (testEnvironment, localTestEnvironment) = do
  Postgres.setup schema (testEnvironment, localTestEnvironment)
  postgresCreatePermissions testEnvironment

postgresTeardown :: (TestEnvironment, ()) -> IO ()
postgresTeardown (testEnvironment, localTestEnvironment) = do
  postgresDropPermissions testEnvironment
  Postgres.teardown schema (testEnvironment, localTestEnvironment)

-- No 'article' root fields will be exposed.
-- This scenario tests, when we want to disable querying a specific table but allow
-- it's row's and columns to be accessible when used from within a relationship.
postgresCreatePermissions :: TestEnvironment -> IO ()
postgresCreatePermissions testEnvironment = do
  GraphqlEngine.postMetadata_
    testEnvironment
    [yaml|
type: bulk
args:
- type: pg_create_select_permission
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
- type: pg_create_select_permission
  args:
    source: postgres
    table:
      schema: hasura
      name: author
    role: user
    permission:
      filter:
        articles_by_author_id:
          author_id:
            _eq:  X-Hasura-User-Id
      columns: '*'
      allow_aggregations: true
      query_root_fields: ["select", "select_by_pk", "select_aggregate"]
      subscription_root_fields: ["select_stream"]
|]

postgresDropPermissions :: TestEnvironment -> IO ()
postgresDropPermissions testEnvironment = do
  GraphqlEngine.postMetadata_
    testEnvironment
    [yaml|
type: bulk
args:
- type: pg_drop_select_permission
  args:
    source: postgres
    table:
      schema: hasura
      name: article
    role: user
- type: pg_drop_select_permission
  args:
    source: postgres
    table:
      schema: hasura
      name: author
    role: user
|]

--------------------------------------------------------------------------------
-- Setting up SQL server

sqlServerSetup :: (TestEnvironment, ()) -> IO ()
sqlServerSetup (testEnvironment, localTestEnvironment) = do
  SQLServer.setup schema (testEnvironment, localTestEnvironment)
  mssqlCreatePermissions testEnvironment

sqlServerTeardown :: (TestEnvironment, ()) -> IO ()
sqlServerTeardown (testEnvironment, localTestEnvironment) = do
  mssqlDropPermissions testEnvironment
  SQLServer.teardown schema (testEnvironment, localTestEnvironment)

-- No 'article' root fields will be exposed.
-- This scenario tests, when we want to disable querying a specific table but allow
-- it's row's and columns to be accessible when used from within a relationship.
mssqlCreatePermissions :: TestEnvironment -> IO ()
mssqlCreatePermissions testEnvironment = do
  GraphqlEngine.postMetadata_
    testEnvironment
    [yaml|
type: bulk
args:
- type: mssql_create_select_permission
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
- type: mssql_create_select_permission
  args:
    source: mssql
    table:
      schema: hasura
      name: author
    role: user
    permission:
      filter:
        articles_by_author_id:
          author_id:
            _eq:  X-Hasura-User-Id
      columns: '*'
      allow_aggregations: true
      query_root_fields: ["select", "select_by_pk", "select_aggregate"]
      subscription_root_fields: ["select_stream"]
|]

mssqlDropPermissions :: TestEnvironment -> IO ()
mssqlDropPermissions testEnvironment = do
  GraphqlEngine.postMetadata_
    testEnvironment
    [yaml|
type: bulk
args:
- type: mssql_drop_select_permission
  args:
    source: mssql
    table:
      schema: hasura
      name: article
    role: user
- type: mssql_drop_select_permission
  args:
    source: mssql
    table:
      schema: hasura
      name: author
    role: user
|]

--------------------------------------------------------------------------------
-- Tests

tests :: Context.Options -> SpecWith TestEnvironment
tests opts = describe "DisableAllRootFieldsRelationshipSpec" $ do
  let userHeaders = [("X-Hasura-Role", "user"), ("X-Hasura-User-Id", "1")]
  it "query root: 'list' root field for 'author' is accessible" $ \testEnvironment -> do
    let authorArticlesQuery =
          [graphql|
          query {
            # we put id=1 restrictions here because we don't assume ordering support
            hasura_author {
              id
              # the _by_author_id part is necessary to distinguish between multiple foreign key relationships between the same two tables
              articles_by_author_id{
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
              articles_by_author_id:
                - title: Article 1
                - title: Article 3
          |]

    shouldReturnYaml
      opts
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
              message: 'field "hasura_article" not found in type: ''query_root'''
          |]

    shouldReturnYaml
      opts
      (GraphqlEngine.postGraphqlWithHeaders testEnvironment userHeaders articleQuery)
      expectedResponse
