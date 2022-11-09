{-# LANGUAGE QuasiQuotes #-}

-- | Test select permissions
--
-- https://hasura.io/docs/latest/schema/bigquery/data-validations/#using-hasura-permissions
-- https://hasura.io/docs/latest/schema/postgres/data-validations/#using-hasura-permissions
module Test.Schema.DataValidation.Permissions.SelectSpec (spec) where

import Data.Aeson (Value)
import Data.Aeson.Key qualified as Key (toString)
import Data.List.NonEmpty qualified as NE
import Harness.Backend.BigQuery qualified as BigQuery
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Cockroach qualified as Cockroach
import Harness.Backend.Postgres qualified as Postgres
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

spec :: SpecWith TestEnvironment
spec = do
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Fixture.Postgres)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Postgres.setupTablesAction schema testEnvironment,
                  setupMetadata Fixture.Postgres testEnvironment
                ]
            },
          (Fixture.fixture $ Fixture.Backend Fixture.Citus)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Citus.setupTablesAction schema testEnvironment,
                  setupMetadata Fixture.Citus testEnvironment
                ]
            },
          (Fixture.fixture $ Fixture.Backend Fixture.Cockroach)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Cockroach.setupTablesAction schema testEnvironment,
                  setupMetadata Fixture.Cockroach testEnvironment
                ]
            },
          (Fixture.fixture $ Fixture.Backend Fixture.BigQuery)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ BigQuery.setupTablesAction schema testEnvironment,
                  setupMetadata Fixture.BigQuery testEnvironment
                ],
              Fixture.customOptions =
                Just $
                  Fixture.defaultOptions
                    { Fixture.stringifyNumbers = True
                    }
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
  ]

--------------------------------------------------------------------------------
-- Tests

tests :: Fixture.Options -> SpecWith TestEnvironment
tests opts = do
  let shouldBe :: IO Value -> Value -> IO ()
      shouldBe = shouldReturnYaml opts

  describe "Permissions on queries" do
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

      actual `shouldBe` expected

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

    actual `shouldBe` expected

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

    actual `shouldBe` expected

--------------------------------------------------------------------------------
-- Metadata

setupMetadata :: Fixture.BackendType -> TestEnvironment -> Fixture.SetupAction
setupMetadata backendType testEnvironment = do
  let schemaName :: Schema.SchemaName
      schemaName = Schema.getSchemaName testEnvironment

      schemaKeyword :: String
      schemaKeyword = Key.toString $ Fixture.schemaKeyword backendType

      backendPrefix :: String
      backendPrefix = Fixture.defaultBackendTypeString backendType

      source :: String
      source = Fixture.defaultSource backendType

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
                table:
                  #{schemaKeyword}: #{schemaName}
                  name: article
                role: author
                permission:
                  filter:
                    author_id:
                      _eq: X-Hasura-User-Id
                  columns: "*"
            - type: #{backendPrefix}_create_select_permission
              args:
                source: #{source}
                table:
                  #{schemaKeyword}: #{schemaName}
                  name: article
                role: user
                permission:
                  filter:
                    is_published:
                      _eq: true
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
                table:
                  name: article
                  #{schemaKeyword}: #{schemaName}
                role: author
            - type: #{backendPrefix}_drop_select_permission
              args:
                source: #{source}
                table:
                  name: article
                  #{schemaKeyword}: #{schemaName}
                role: user
          |]

  Fixture.SetupAction setup \_ -> teardown
