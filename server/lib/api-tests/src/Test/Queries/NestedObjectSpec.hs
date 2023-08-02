{-# LANGUAGE QuasiQuotes #-}

-- |
-- Tests for nested GraphQL queries.
--
-- https://hasura.io/docs/latest/queries/postgres/nested-object-queries/
-- https://hasura.io/docs/latest/queries/ms-sql-server/nested-object-queries/
-- https://hasura.io/docs/latest/queries/bigquery/nested-object-queries/
module Test.Queries.NestedObjectSpec (spec) where

import Data.Aeson (Value)
import Data.List.NonEmpty qualified as NE
import Harness.Backend.BigQuery qualified as BigQuery
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Cockroach qualified as Cockroach
import Harness.Backend.Postgres qualified as Postgres
import Harness.Backend.Sqlserver qualified as Sqlserver
import Harness.GraphqlEngine (postGraphql)
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (interpolateYaml)
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
spec = do
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Postgres.setupTablesAction schema testEnv
                ]
            },
          (Fixture.fixture $ Fixture.Backend Citus.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Citus.setupTablesAction schema testEnv
                ]
            },
          (Fixture.fixture $ Fixture.Backend Sqlserver.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Sqlserver.setupTablesAction schema testEnv
                ]
            },
          (Fixture.fixture $ Fixture.Backend BigQuery.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ BigQuery.setupTablesAction schema testEnv
                ],
              Fixture.customOptions =
                Just
                  $ Fixture.defaultOptions
                    { Fixture.stringifyNumbers = True
                    }
            },
          (Fixture.fixture $ Fixture.Backend Cockroach.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Cockroach.setupTablesAction schema testEnv
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
            Schema.column "published" Schema.TBool,
            Schema.column "author_id" Schema.TInt
          ],
        tablePrimaryKey = ["id"],
        tableReferences = [Schema.reference "author_id" "author" "id"],
        tableData =
          [ [Schema.VInt 1, Schema.VStr "Article 1", Schema.VBool False, Schema.VInt 1],
            [Schema.VInt 2, Schema.VStr "Article 2", Schema.VBool True, Schema.VInt 1],
            [Schema.VInt 3, Schema.VStr "Article 3", Schema.VBool True, Schema.VInt 2]
          ]
      }
  ]

--------------------------------------------------------------------------------
-- Tests

tests :: SpecWith TestEnvironment
tests =
  describe "Nested relationship queries" do
    it "Nests with 'where' clauses" \testEnvironment -> do
      let schemaName = Schema.getSchemaName testEnvironment

      let expected :: Value
          expected =
            [interpolateYaml|
              data:
                #{schemaName}_author:
                - name: "Author 1"
                  articles_by_id_to_author_id:
                  - id: 2
            |]

          actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                query {
                  #{schemaName}_author(
                    order_by: [{ id: asc }],
                    where: { id: { _eq: 1 } }
                  ) {
                    name

                    articles_by_id_to_author_id (
                      order_by: [{ id: asc }],
                      where: { published: { _eq: true } }
                    ) {
                      id
                    }
                  }
                }
              |]

      shouldReturnYaml testEnvironment actual expected

    it "Nesting in the 'where' clause" \testEnvironment -> do
      let schemaName = Schema.getSchemaName testEnvironment

      let expected :: Value
          expected =
            [interpolateYaml|
              data:
                #{schemaName}_article:
                - id: 1
                  author_id: 1
                - id: 2
                  author_id: 1
            |]

          actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                query {
                  #{schemaName}_article (
                    order_by: [{ id: asc }],
                    where: { author_by_author_id_to_id: { name: { _eq: "Author 1" } } }
                  ) {
                    id
                    author_id
                  }
                }
              |]

      shouldReturnYaml testEnvironment actual expected

    it "Deep nesting" \testEnvironment -> do
      let schemaName = Schema.getSchemaName testEnvironment

      let expected :: Value
          expected =
            [interpolateYaml|
              data:
                #{schemaName}_article:
                - id: 1
                  author_by_author_id_to_id:
                    id: 1
                    articles_by_id_to_author_id:
                    - id: 1
                      author_by_author_id_to_id:
                        id: 1
                        articles_by_id_to_author_id:
                        - id: 1
                          author_by_author_id_to_id:
                            id: 1
            |]

          actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                query {
                  #{schemaName}_article(where: {id: {_eq: 1}}) {
                    id
                    author_by_author_id_to_id {
                      id
                      articles_by_id_to_author_id(where: {id: {_eq: 1}}) {
                         id
                         author_by_author_id_to_id {
                           id
                           articles_by_id_to_author_id(where: {id: {_eq: 1}}) {
                             id
                             author_by_author_id_to_id {
                               id
                            }
                          }
                        }
                      }
                    }
                  }
                }
              |]

      shouldReturnYaml testEnvironment actual expected
