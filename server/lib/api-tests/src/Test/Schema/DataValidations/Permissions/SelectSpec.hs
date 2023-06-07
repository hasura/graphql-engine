{-# LANGUAGE QuasiQuotes #-}

-- | Test select permissions
--
-- https://hasura.io/docs/latest/schema/bigquery/data-validations/#using-hasura-permissions
-- https://hasura.io/docs/latest/schema/postgres/data-validations/#using-hasura-permissions
module Test.Schema.DataValidations.Permissions.SelectSpec (spec) where

import Data.Aeson (Value)
import Data.List.NonEmpty qualified as NE
import Harness.Backend.BigQuery qualified as BigQuery
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Cockroach qualified as Cockroach
import Harness.Backend.DataConnector.Sqlite qualified as Sqlite
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine (postGraphqlWithHeaders, postMetadata_)
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (interpolateYaml)
import Harness.Schema (Table (..), table)
import Harness.Schema qualified as Schema
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it, pendingWith)

spec :: SpecWith GlobalTestEnvironment
spec = do
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Postgres.setupTablesAction schema testEnvironment,
                  setupMetadata SupportsArrayTypes Postgres.backendTypeMetadata testEnvironment
                ]
            },
          (Fixture.fixture $ Fixture.Backend Citus.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Citus.setupTablesAction schema testEnvironment,
                  setupMetadata SupportsArrayTypes Citus.backendTypeMetadata testEnvironment
                ]
            },
          (Fixture.fixture $ Fixture.Backend Cockroach.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Cockroach.setupTablesAction schema testEnvironment,
                  setupMetadata SupportsArrayTypes Cockroach.backendTypeMetadata testEnvironment
                ]
            },
          (Fixture.fixture $ Fixture.Backend Sqlite.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Sqlite.setupTablesAction schema testEnvironment,
                  setupMetadata SupportsArrayTypes Sqlite.backendTypeMetadata testEnvironment
                ]
            }
        ]
    )
    (tests SupportsArrayTypes)
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend BigQuery.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ BigQuery.setupTablesAction schema testEnvironment,
                  setupMetadata DoesNotSupportArrayTypes BigQuery.backendTypeMetadata testEnvironment
                ],
              Fixture.customOptions =
                Just
                  $ Fixture.defaultOptions
                    { Fixture.stringifyNumbers = True
                    }
            }
        ]
    )
    (tests DoesNotSupportArrayTypes)

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
            [Schema.VInt 2, Schema.VStr "Author 2"],
            [Schema.VInt 3, Schema.VStr "Author 3"]
          ]
      },
    (table "article")
      { tableColumns =
          [ Schema.column "id" Schema.TInt,
            Schema.column "title" Schema.TStr,
            Schema.columnNull "content" Schema.TStr,
            Schema.column "state" Schema.TStr, -- 'Draft', 'InReview', 'Published'
            Schema.column "author_id" Schema.TInt
          ],
        tablePrimaryKey = ["id"],
        tableReferences = [Schema.reference "author_id" "author" "id"],
        tableData =
          [ [ Schema.VInt 1,
              Schema.VStr "Article 1",
              Schema.VStr "Sample article content 1",
              Schema.VStr "InReview",
              Schema.VInt 1
            ],
            [ Schema.VInt 2,
              Schema.VStr "Article 2",
              Schema.VStr "Sample article content 2",
              Schema.VStr "Published",
              Schema.VInt 2
            ],
            [ Schema.VInt 3,
              Schema.VStr "Article 3",
              Schema.VStr "Sample article content 3",
              Schema.VStr "Draft",
              Schema.VInt 3
            ]
          ]
      }
  ]

--------------------------------------------------------------------------------
-- Tests

data ArrayTypeSupport
  = SupportsArrayTypes
  | DoesNotSupportArrayTypes
  deriving stock (Eq, Show)

tests :: ArrayTypeSupport -> SpecWith TestEnvironment
tests arrayTypeSupport = describe "Permissions on queries" do
  it "Authors can't select another author's articles" \testEnvironment -> do
    let schemaName :: Schema.SchemaName
        schemaName = Schema.getSchemaName testEnvironment

        expected :: Value
        expected =
          [interpolateYaml|
            data:
              #{schemaName}_article: []
          |]

        actual :: IO Value
        actual =
          postGraphqlWithHeaders
            testEnvironment
            [ ("X-Hasura-Role", "author"),
              ("X-Hasura-User-Id", "0")
            ]
            [graphql|
              query {
                #{schemaName}_article {
                  id
                  author_id
                }
              }
            |]

    shouldReturnYaml testEnvironment actual expected

  it "Authors can select their own articles" \testEnvironment -> do
    let schemaName :: Schema.SchemaName
        schemaName = Schema.getSchemaName testEnvironment

        expected :: Value
        expected =
          [interpolateYaml|
            data:
              #{schemaName}_article:
              - id: 1
                author_id: 1
          |]

        actual :: IO Value
        actual =
          postGraphqlWithHeaders
            testEnvironment
            [ ("X-Hasura-Role", "author"),
              ("X-Hasura-User-Id", "1")
            ]
            [graphql|
              query {
                #{schemaName}_article {
                  id
                  author_id
                }
              }
            |]

    shouldReturnYaml testEnvironment actual expected

  it "User role can select published articles only" \testEnvironment -> do
    let schemaName :: Schema.SchemaName
        schemaName = Schema.getSchemaName testEnvironment

        expected :: Value
        expected =
          [interpolateYaml|
            data:
              #{schemaName}_article:
              - author_id: 2
                content: Sample article content 2
                title: Article 2
          |]

        actual :: IO Value
        actual =
          postGraphqlWithHeaders
            testEnvironment
            [ ("X-Hasura-Role", "user"),
              ("X-Hasura-User-Id", "2")
            ]
            [graphql|
              query {
                #{schemaName}_article {
                  title
                  content
                  author_id
                }
              }
            |]

    shouldReturnYaml testEnvironment actual expected

  it "Editor role can select in review and published articles only" \testEnvironment -> do
    when (arrayTypeSupport == DoesNotSupportArrayTypes)
      $ pendingWith "Backend does not support array types"

    let schemaName :: Schema.SchemaName
        schemaName = Schema.getSchemaName testEnvironment

        expected :: Value
        expected =
          [interpolateYaml|
            data:
              #{schemaName}_article:
              - author_id: 1
                content: Sample article content 1
                title: Article 1
              - author_id: 2
                content: Sample article content 2
                title: Article 2
          |]

        actual :: IO Value
        actual =
          postGraphqlWithHeaders
            testEnvironment
            [ ("X-Hasura-Role", "editor")
            ]
            [graphql|
              query {
                #{schemaName}_article {
                  title
                  content
                  author_id
                }
              }
            |]

    shouldReturnYaml testEnvironment actual expected

--------------------------------------------------------------------------------
-- Metadata

setupMetadata :: ArrayTypeSupport -> Fixture.BackendTypeConfig -> TestEnvironment -> Fixture.SetupAction
setupMetadata arrayTypeSupport backendTypeMetadata testEnvironment = do
  let schemaName :: Schema.SchemaName
      schemaName = Schema.getSchemaName testEnvironment

      backendPrefix :: String
      backendPrefix = Fixture.backendTypeString backendTypeMetadata

      source :: String
      source = Fixture.backendSourceName backendTypeMetadata

      articleTable :: Value
      articleTable = Schema.mkTableField backendTypeMetadata schemaName "article"

      articleEditorFilter :: Value
      articleEditorFilter =
        case arrayTypeSupport of
          SupportsArrayTypes ->
            [interpolateYaml|
              state:
                _in: [InReview, Published]
            |]
          DoesNotSupportArrayTypes ->
            [interpolateYaml| {} |]

      setup :: IO ()
      setup =
        postMetadata_
          testEnvironment
          [interpolateYaml|
            type: bulk
            args:
            - type: #{backendPrefix}_create_select_permission
              args:
                source: #{source}
                table: #{articleTable}
                role: author
                permission:
                  filter:
                    author_id:
                      _eq: X-Hasura-User-Id
                  columns: "*"
            - type: #{backendPrefix}_create_select_permission
              args:
                source: #{source}
                table: #{articleTable}
                role: user
                permission:
                  filter:
                    state:
                      _eq: Published
                  columns: "*"
            - type: #{backendPrefix}_create_select_permission
              args:
                source: #{source}
                table: #{articleTable}
                role: editor
                permission:
                  filter: #{articleEditorFilter}
                  columns: "*"
          |]

      teardown :: IO ()
      teardown =
        postMetadata_
          testEnvironment
          [interpolateYaml|
            type: bulk
            args:
            - type: #{backendPrefix}_drop_select_permission
              args:
                source: #{source}
                table: #{articleTable}
                role: author
            - type: #{backendPrefix}_drop_select_permission
              args:
                source: #{source}
                table: #{articleTable}
                role: user
            - type: #{backendPrefix}_drop_select_permission
              args:
                source: #{source}
                table: #{articleTable}
                role: editor
          |]

  Fixture.SetupAction setup \_ -> teardown
