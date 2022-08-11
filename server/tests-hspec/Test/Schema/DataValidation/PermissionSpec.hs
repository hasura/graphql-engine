{-# LANGUAGE QuasiQuotes #-}

-- | Test select permissions
module Test.Schema.DataValidation.PermissionSpec (spec) where

import Data.List.NonEmpty qualified as NE
import Harness.Backend.BigQuery qualified as BigQuery
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (interpolateYaml, yaml)
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.Schema (Table (..), table)
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (TestEnvironment)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, it)

--------------------------------------------------------------------------------

-- ** Preamble

spec :: SpecWith TestEnvironment
spec =
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Fixture.BigQuery)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ BigQuery.setupTablesAction schema testEnv
                ]
                  <> bigqueryPermissionsSetup testEnv
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
          Schema.column "is_published" Schema.TBool,
          Schema.column "author_id" Schema.TInt
        ],
      tablePrimaryKey = ["id"],
      tableReferences = [Schema.Reference "author_id" "author" "id"],
      tableData =
        [ [ Schema.VInt 1,
            Schema.VStr "Article 1",
            Schema.VStr "Sample article content 1",
            Schema.VBool False,
            Schema.VInt 1
          ],
          [ Schema.VInt 2,
            Schema.VStr "Article 2",
            Schema.VStr "Sample article content 2",
            Schema.VBool True,
            Schema.VInt 2
          ]
        ]
    }

--------------------------------------------------------------------------------

-- ** Setup and teardown

bigqueryPermissionsSetup :: TestEnvironment -> [Fixture.SetupAction]
bigqueryPermissionsSetup testEnvironment =
  let schemaName = Schema.getSchemaName testEnvironment
   in [ Fixture.SetupAction
          { setupAction =
              GraphqlEngine.postMetadata_
                testEnvironment
                [yaml|
            type: bigquery_create_select_permission
            args:
              source: bigquery
              table:
                dataset: *schemaName
                name: article
              role: author
              permission:
                filter:
                  author_id:
                    _eq: X-Hasura-User-Id
                columns: '*'
          |],
            teardownAction = \_ ->
              GraphqlEngine.postMetadata_
                testEnvironment
                [yaml|
            type: bigquery_drop_select_permission
            args:
              table:
                name: article
                dataset: *schemaName
              role: author
              source: bigquery
          |]
          },
        Fixture.SetupAction
          { setupAction =
              GraphqlEngine.postMetadata_
                testEnvironment
                [yaml|
            type: bigquery_create_select_permission
            args:
              source: bigquery
              table:
                dataset: *schemaName
                name: article
              role: user
              permission:
                filter:
                  is_published:
                    _eq: true
                columns: '*'
            |],
            teardownAction = \_ ->
              GraphqlEngine.postMetadata_
                testEnvironment
                [yaml|
            type: bigquery_drop_select_permission
            args:
              table:
                name: article
                dataset: *schemaName
              role: user
              source: bigquery
            |]
          }
      ]

--------------------------------------------------------------------------------

-- * Tests

tests :: Fixture.Options -> SpecWith TestEnvironment
tests opts = do
  it "Author role cannot select articles with mismatching author_id and X-Hasura-User-Id" $ \testEnvironment -> do
    let userHeaders = [("X-Hasura-Role", "author"), ("X-Hasura-User-Id", "0")]
        schemaName = Schema.getSchemaName testEnvironment

    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphqlWithHeaders
          testEnvironment
          userHeaders
          [graphql|
            query {
              #{schemaName}_article {
                id
                author_id
              }
            }
          |]
      )
      [interpolateYaml|
        data:
          #{schemaName}_article: []
      |]

  it "Author role can select articles with matching author_id and X-Hasura-User-Id" $ \testEnvironment -> do
    let userHeaders = [("X-Hasura-Role", "author"), ("X-Hasura-User-Id", "1")]
        schemaName = Schema.getSchemaName testEnvironment

    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphqlWithHeaders
          testEnvironment
          userHeaders
          [graphql|
            query {
              #{schemaName}_article {
                id
                author_id
              }
            }
          |]
      )
      [interpolateYaml|
        data:
          #{schemaName}_article:
          - id: '1'
            author_id: '1'
      |]

  it "User role can select published articles only" $ \testEnvironment -> do
    let userHeaders = [("X-Hasura-Role", "user"), ("X-Hasura-User-Id", "2")]
        schemaName = Schema.getSchemaName testEnvironment

    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphqlWithHeaders
          testEnvironment
          userHeaders
          [graphql|
              query {
                #{schemaName}_article {
                  title
                  content
                  author_id
                }
              }
            |]
      )
      [interpolateYaml|
        data:
          #{schemaName}_article:
          - author_id: '2'
            content: Sample article content 2
            title: Article 2
      |]
