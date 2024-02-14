{-# LANGUAGE QuasiQuotes #-}

module Test.Schema.ComputedFields.AggregationSpec (spec) where

import Data.List.NonEmpty qualified as NE
import Data.String.Interpolate (i)
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (interpolateYaml)
import Harness.Schema (SchemaName (..), Table (..), table)
import Harness.Schema qualified as Schema
import Harness.Test.BackendType qualified as BackendType
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment (..), getBackendTypeConfig)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, it)

-- ** Preamble

spec :: SpecWith GlobalTestEnvironment
spec =
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Postgres.setupTablesAction schema testEnv
                ]
                  <> setupFunction testEnv
                  <> setupMetadata testEnv
            }
        ]
    )
    tests

-- ** Schema

schema :: [Table]
schema = [authorTable, articleTable]

authorTable :: Table
authorTable =
  (table "author")
    { tableColumns =
        [ Schema.column "id" Schema.TInt,
          Schema.column "first_name" Schema.TStr,
          Schema.column "last_name" Schema.TStr
        ],
      tablePrimaryKey = ["id"],
      tableData =
        [ [ Schema.VInt 1,
            Schema.VStr "Author 1",
            Schema.VStr "1"
          ],
          [ Schema.VInt 2,
            Schema.VStr "Author 2",
            Schema.VStr "2"
          ]
        ]
    }

articleTable :: Table
articleTable =
  (table "article")
    { tableColumns =
        [ Schema.column "id" Schema.TInt,
          Schema.column "author_id" Schema.TInt,
          Schema.column "title" Schema.TStr
        ],
      tablePrimaryKey = ["id"],
      tableData =
        [ [ Schema.VInt 1,
            Schema.VInt 1,
            Schema.VStr "Article One"
          ],
          [ Schema.VInt 2,
            Schema.VInt 1,
            Schema.VStr "Article Two"
          ],
          [ Schema.VInt 3,
            Schema.VInt 2,
            Schema.VStr "Article Three"
          ]
        ]
    }

-- ** Setup and teardown

-- * SQL

numberOfArticlesForAuthorSQL :: SchemaName -> Text
numberOfArticlesForAuthorSQL schemaName =
  [i|
    CREATE FUNCTION #{ unSchemaName schemaName }.count_articles(author_row author)
    RETURNS bigint AS $$
      SELECT COUNT(*)
      FROM article
      WHERE author_id = author_row.id
    $$ LANGUAGE sql STABLE;
  |]

numberOfArticlesForAuthorSQLPlus :: SchemaName -> Text
numberOfArticlesForAuthorSQLPlus schemaName =
  [i|
    CREATE FUNCTION #{ unSchemaName schemaName }.count_articles_plus(author_row author, plus int)
    RETURNS bigint AS $$
      SELECT (COUNT(*) + plus)
      FROM article
      WHERE author_id = author_row.id
    $$ LANGUAGE sql STABLE;
  |]

articleTitleLengthSQL :: SchemaName -> Text
articleTitleLengthSQL schemaName =
  [i|
    CREATE FUNCTION #{ unSchemaName schemaName }.article_length(article_row article)
    RETURNS int AS $$
      SELECT LENGTH(article_row.title)
    $$ LANGUAGE sql STABLE;
  |]

-- * Setup

setupFunction :: TestEnvironment -> [Fixture.SetupAction]
setupFunction testEnv =
  let schemaName = Schema.getSchemaName testEnv
   in [ Fixture.SetupAction
          { Fixture.setupAction = do
              Postgres.run_ testEnv (numberOfArticlesForAuthorSQL schemaName)
              Postgres.run_ testEnv (articleTitleLengthSQL schemaName)
              Postgres.run_ testEnv (numberOfArticlesForAuthorSQLPlus schemaName),
            Fixture.teardownAction = \_ -> pure ()
          }
      ]

setupMetadata :: TestEnvironment -> [Fixture.SetupAction]
setupMetadata testEnvironment =
  let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment

      schemaName :: Schema.SchemaName
      schemaName = Schema.getSchemaName testEnvironment

      source :: String
      source = Fixture.backendSourceName backendTypeMetadata

      backendPrefix :: String
      backendPrefix = BackendType.backendTypeString backendTypeMetadata
   in [ Fixture.SetupAction
          { Fixture.setupAction = do
              GraphqlEngine.postMetadata_
                testEnvironment
                [interpolateYaml|
                  type: #{ backendPrefix }_add_computed_field
                  args:
                    source: #{ source }
                    name: count_articles
                    table:
                      schema: #{ schemaName }
                      name: author
                    definition:
                      function:
                        schema: #{ schemaName }
                        name: count_articles
              |]

              GraphqlEngine.postMetadata_
                testEnvironment
                [interpolateYaml|
                  type: #{ backendPrefix }_add_computed_field
                  args:
                    source: #{ source }
                    name: article_length
                    table:
                      schema: #{ schemaName }
                      name: article
                    definition:
                      function:
                        schema: #{ schemaName }
                        name: article_length
              |]

              GraphqlEngine.postMetadata_
                testEnvironment
                [interpolateYaml|
                  type: #{ backendPrefix }_add_computed_field
                  args:
                    source: #{ source }
                    name: count_articles_plus
                    table:
                      schema: #{ schemaName }
                      name: author
                    definition:
                      function:
                        schema: #{ schemaName }
                        name: count_articles_plus
              |],
            Fixture.teardownAction = \_ -> pure ()
          }
      ]

-- * Tests

tests :: SpecWith TestEnvironment
tests = do
  it "Aggregate computed field from the authors table, plus another non-computed aggregation field" $ \testEnv -> do
    let schemaName = Schema.getSchemaName testEnv

    shouldReturnYaml
      testEnv
      ( GraphqlEngine.postGraphql
          testEnv
          [graphql|
            query {
              #{schemaName}_author_aggregate {
                aggregate {
                  avg {
                    count_articles
                    id
                  }
                  max {
                    count_articles
                    id
                  }
                }
              }
            }
          |]
      )
      [interpolateYaml|
        data:
          #{schemaName}_author_aggregate:
            aggregate:
              avg:
                count_articles: 1.5
                id: 1.5
              max:
                count_articles: 2
                id: 2

      |]

  it "Aggregate computed field from the authors table" $ \testEnv -> do
    let schemaName = Schema.getSchemaName testEnv

    shouldReturnYaml
      testEnv
      ( GraphqlEngine.postGraphql
          testEnv
          [graphql|
            query {
              #{schemaName}_author_aggregate {
                aggregate {
                  avg {
                    count_articles
                  }
                  max {
                    count_articles
                  }
                }
              }
            }
          |]
      )
      [interpolateYaml|
        data:
          #{schemaName}_author_aggregate:
            aggregate:
              avg:
                count_articles: 1.5
              max:
                count_articles: 2
      |]

  it "Aggregate computed field from the authors table and also return rows" $ \testEnv -> do
    let schemaName = Schema.getSchemaName testEnv

    shouldReturnYaml
      testEnv
      ( GraphqlEngine.postGraphql
          testEnv
          [graphql|
            query {
              #{schemaName}_author_aggregate(order_by: {id: asc}) {
                aggregate {
                  avg {
                    count_articles
                  }
                  max {
                    count_articles
                  }
                }
                nodes {
                  id
                }
              }
            }
          |]
      )
      [interpolateYaml|
        data:
          #{schemaName}_author_aggregate:
            aggregate:
              avg:
                count_articles: 1.5
              max:
                count_articles: 2
            nodes:
              - id: 1
              - id: 2
      |]

  it "Aggregate computed field from the authors table with arguments" $ \testEnv -> do
    let schemaName = Schema.getSchemaName testEnv

    shouldReturnYaml
      testEnv
      ( GraphqlEngine.postGraphql
          testEnv
          [graphql|
            query {
              #{schemaName}_author_aggregate {
                aggregate {
                  avg {
                    count_articles_plus (args: { plus: 10 })
                  }
                  max {
                    count_articles_plus (args: { plus: 10 })
                  }
                }
              }
            }
          |]
      )
      [interpolateYaml|
        data:
          #{schemaName}_author_aggregate:
            aggregate:
              avg:
                count_articles_plus: 11.5
              max:
                count_articles_plus: 12
      |]

  it "Aggregate computed fields from the authors table with different arguments" $ \testEnv -> do
    let schemaName = Schema.getSchemaName testEnv

    -- This test abuses field name aliases to have different invocations of the same
    -- computed field (ie invoked with different arguments) share the same field name.
    -- but under different aggregate functions. This highlights that the field name
    -- of the computed field (eg count_articles1) is not good enough on its own to
    -- identify that field you need the aggregate field name too (eg avg, max).
    shouldReturnYaml
      testEnv
      ( GraphqlEngine.postGraphql
          testEnv
          [graphql|
            query {
              #{schemaName}_author_aggregate {
                aggregate {
                  avg {
                    count_articles1: count_articles_plus (args: { plus: 12 })
                    count_articles2: count_articles_plus (args: { plus: 10 })
                  }
                  max {
                    count_articles1: count_articles_plus (args: { plus: 10 })
                    count_articles2: count_articles_plus (args: { plus: 12 })
                  }
                }
              }
            }
          |]
      )
      [interpolateYaml|
        data:
          #{schemaName}_author_aggregate:
            aggregate:
              avg:
                count_articles1: 13.5
                count_articles2: 11.5
              max:
                count_articles1: 12
                count_articles2: 14
      |]

  it "Aggregate computed field, but use two of them" $ \testEnv -> do
    let schemaName = Schema.getSchemaName testEnv

    shouldReturnYaml
      testEnv
      ( GraphqlEngine.postGraphql
          testEnv
          [graphql|
            query {
              #{schemaName}_author_aggregate {
                aggregate {
                  avg {
                    count_articles
                    count_articles_plus (args: { plus: 10 })
                  }
                  max {
                    count_articles
                    count_articles_plus (args: { plus: 10 })
                  }

                }
              }
            }
          |]
      )
      [interpolateYaml|
        data:
          #{schemaName}_author_aggregate:
            aggregate:
              avg:
                count_articles: 1.5
                count_articles_plus: 11.5
              max:
                count_articles: 2
                count_articles_plus: 12

      |]

  it "Aggregate computed field, but use two of them from different tables" $ \testEnv -> do
    let schemaName = Schema.getSchemaName testEnv

    shouldReturnYaml
      testEnv
      ( GraphqlEngine.postGraphql
          testEnv
          [graphql|
            query {
              #{schemaName}_article_aggregate {
                aggregate {
                  avg {
                    article_length
                  }
                  max {
                    article_length
                  }
                }
              }
              #{schemaName}_author_aggregate {
                aggregate {
                  avg {
                    count_articles
                    count_articles_plus (args: { plus: 10 })
                  }
                  max {
                    count_articles
                    count_articles_plus (args: { plus: 10 })
                  }
                }
              }
            }
          |]
      )
      [interpolateYaml|
        data:
          #{schemaName}_article_aggregate:
            aggregate:
              avg:
                article_length: 11.6666666666666667
              max:
                article_length: 13

          #{schemaName}_author_aggregate:
            aggregate:
              avg:
                count_articles: 1.5
                count_articles_plus: 11.5
              max:
                count_articles: 2
                count_articles_plus: 12

      |]
