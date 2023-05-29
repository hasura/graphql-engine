{-# LANGUAGE QuasiQuotes #-}

-- | Postgres-like upsert mutations.
--
-- https://hasura.io/docs/latest/mutations/postgres/upsert/
module Test.Mutations.Upsert.OnConflictSpec (spec) where

import Data.Aeson (Value)
import Data.List.NonEmpty qualified as NE
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Cockroach qualified as Cockroach
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine (postGraphql)
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (interpolateYaml)
import Harness.Schema (Table (..), table)
import Harness.Schema qualified as Schema
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, it)

spec :: SpecWith GlobalTestEnvironment
spec = do
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Postgres.setupTablesAction schema testEnvironment
                ]
            },
          (Fixture.fixture $ Fixture.Backend Citus.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Citus.setupTablesAction schema testEnvironment
                ]
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
          [ Schema.column "id" Schema.defaultSerialType,
            Schema.column "name" Schema.TStr
          ],
        tablePrimaryKey = ["id"],
        tableConstraints = [Schema.UniqueConstraintColumns ["name"]],
        tableData =
          [ [ Schema.VInt 1,
              Schema.VStr "Justin"
            ],
            [ Schema.VInt 2,
              Schema.VStr "Beltran"
            ],
            [ Schema.VInt 3,
              Schema.VStr "Sidney"
            ],
            [ Schema.VInt 4,
              Schema.VStr "Anjela"
            ],
            [ Schema.VInt 5,
              Schema.VStr "John"
            ]
          ]
      },
    (table "article")
      { tableColumns =
          [ Schema.column "id" Schema.defaultSerialType,
            Schema.column "title" Schema.TStr,
            Schema.column "content" Schema.TStr,
            Schema.column "published_on" Schema.TStr,
            Schema.column "author_id" serialInt
          ],
        tableData =
          [ [ Schema.VInt 1,
              Schema.VStr "Article 1",
              Schema.VStr "article 1 content",
              Schema.VStr "2018-06-15",
              Schema.VInt 1
            ],
            [ Schema.VInt 2,
              Schema.VStr "Article 2",
              Schema.VStr "article 2 content",
              Schema.VStr "2018-06-15",
              Schema.VInt 2
            ]
          ],
        tablePrimaryKey = ["id"],
        tableConstraints = [Schema.UniqueConstraintColumns ["title"]],
        tableReferences =
          [ Schema.reference "author_id" "author" "id"
          ]
      }
  ]

serialInt :: Schema.ScalarType
serialInt =
  Schema.TCustomType
    $ Schema.defaultBackendScalarType
      { Schema.bstCitus = Just "INT",
        Schema.bstPostgres = Just "INT",
        Schema.bstCockroach = Just "INT4"
      }

--------------------------------------------------------------------------------
-- Tests

tests :: SpecWith TestEnvironment
tests = do
  it "Update selected columns on conflict" \testEnvironment -> do
    let schemaName :: Schema.SchemaName
        schemaName = Schema.getSchemaName testEnvironment

        actual :: IO Value
        actual =
          postGraphql
            testEnvironment
            [graphql|
              mutation upsert_article {
                insert_#{schemaName}_article (
                  objects: [
                    {
                      title: "Article 1",
                      content: "Updated article 1 content",
                      published_on: "2018-10-12",
                      author_id: 1,
                    }
                  ],
                  on_conflict: {
                    constraint: article_title_key,
                    update_columns: [content]
                  }
                ) {
                  returning {
                    id
                    title
                    content
                    published_on
                  }
                }
              }
            |]

        expected :: Value
        expected =
          [interpolateYaml|
            data:
              insert_#{schemaName}_article:
                returning:
                - id: 1
                  title: Article 1
                  content: Updated article 1 content
                  published_on: '2018-06-15'
          |]

    shouldReturnYaml testEnvironment actual expected

  it "Update selected columns on conflict using a filter" \testEnvironment -> do
    let schemaName :: Schema.SchemaName
        schemaName = Schema.getSchemaName testEnvironment

        actual :: IO Value
        actual =
          postGraphql
            testEnvironment
            [graphql|
              mutation upsert_article {
                insert_#{schemaName}_article (
                  objects: [
                    {
                      title: "Article 2",
                      content: "Updated article 2 content",
                      published_on: "2018-10-12",
                      author_id: 1,
                    }
                  ],
                  on_conflict: {
                    constraint: article_title_key,
                    update_columns: [published_on],
                    where: {
                      published_on: {_lt: "2018-10-12"}
                    }
                  }
                ) {
                  returning {
                    id
                    title
                    content
                    published_on
                    author_id
                  }
                }
              }
            |]

        expected :: Value
        expected =
          [interpolateYaml|
            data:
              insert_#{schemaName}_article:
                returning:
                - id: 2
                  title: Article 2
                  content: article 2 content
                  published_on: '2018-10-12'
                  author_id: 2
          |]

    shouldReturnYaml testEnvironment actual expected

  it "Ignore request on conflict" \testEnvironment -> do
    let schemaName :: Schema.SchemaName
        schemaName = Schema.getSchemaName testEnvironment

        actual :: IO Value
        actual =
          postGraphql
            testEnvironment
            [graphql|
              mutation upsert_author {
                insert_#{schemaName}_author(
                  objects: [
                    { name: "John" }
                  ],
                  on_conflict: {
                    constraint: author_name_key,
                    update_columns: []
                  }
                ) {
                  affected_rows
                }
              }
            |]

        expected :: Value
        expected =
          [interpolateYaml|
            data:
              insert_#{schemaName}_author:
                affected_rows: 0
          |]

    shouldReturnYaml testEnvironment actual expected

  it "Upsert in nested mutations" \testEnvironment -> do
    let schemaName :: Schema.SchemaName
        schemaName = Schema.getSchemaName testEnvironment

        actual :: IO Value
        actual =
          postGraphql
            testEnvironment
            [graphql|
              mutation upsert_author_article {
                insert_#{schemaName}_author(
                  objects: [
                    {
                      id: 6,
                      name: "Martin",
                      articles_by_id_to_author_id: {
                        data: [
                          {
                            id: 3,
                            title: "Article 3",
                            content: "Article 3 content",
                            published_on: "2018-10-12",
                          }
                        ],
                        on_conflict: {
                          constraint: article_title_key,
                          update_columns: [content]
                        }
                      }
                    }
                  ]
                ) {
                  affected_rows
                }
              }
            |]

        expected :: Value
        expected =
          [interpolateYaml|
            data:
              insert_#{schemaName}_author:
                affected_rows: 2
          |]

    shouldReturnYaml testEnvironment actual expected
