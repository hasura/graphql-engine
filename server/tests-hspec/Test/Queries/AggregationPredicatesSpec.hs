{-# LANGUAGE QuasiQuotes #-}
-- Ignore "incomplete record updates" error on `Permission`s as the type currently
-- models permissions for all operations, which may have different fields.
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

-- |
-- Test filtering and searching using aggregate fields in "where" clauses.
--
-- https://github.com/hasura/graphql-engine/issues/1498
-- https://hasura.io/docs/latest/queries/bigquery/query-filters/
-- https://hasura.io/docs/latest/queries/ms-sql-server/query-filters/
-- https://hasura.io/docs/latest/queries/postgres/query-filters/
module Test.Queries.AggregationPredicatesSpec (spec) where

import Data.Aeson (Value)
import Data.List.NonEmpty qualified as NE
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine (postGraphql, postGraphqlWithHeaders)
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (interpolateYaml, yaml)
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.Permissions (Permission (..), selectPermission)
import Harness.Test.Schema (Table (..), table)
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (TestEnvironment)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

--------------------------------------------------------------------------------
-- Preamble

spec :: SpecWith TestEnvironment
spec = do
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Fixture.Postgres)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Postgres.setupTablesAction schema testEnv,
                  Postgres.setupPermissionsAction permissions testEnv
                ],
              Fixture.customOptions =
                Just $
                  Fixture.defaultOptions
                    { Fixture.skipTests = Just "Tests disabled until aggregation predicates are enabled for users in https://github.com/hasura/graphql-engine-mono/issues/5511"
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
            Schema.column "published" Schema.TBool,
            Schema.column "author_id" Schema.TInt
          ],
        tablePrimaryKey = ["id"],
        tableReferences = [Schema.Reference "author_id" "author" "id"],
        tableData =
          [ [Schema.VInt 1, Schema.VStr "Article 1", Schema.VBool False, Schema.VInt 1],
            [Schema.VInt 2, Schema.VStr "Article 2", Schema.VBool True, Schema.VInt 2],
            [Schema.VInt 3, Schema.VStr "Article 3", Schema.VBool True, Schema.VInt 2],
            [Schema.VInt 4, Schema.VStr "Article 4", Schema.VBool True, Schema.VInt 1]
          ]
      }
  ]

permissions :: [Permission]
permissions =
  [ selectPermission
      { permissionTable = "author",
        permissionSource = "postgres",
        permissionRole = "role-select-author-name-only",
        permissionColumns = ["id", "name"]
      },
    selectPermission
      { permissionTable = "article",
        permissionSource = "postgres",
        permissionRole = "role-select-author-name-only",
        permissionColumns = ["id", "title", "author_id"],
        permissionRows =
          [yaml|
          published: true
        |],
        permissionAllowAggregations = True
      },
    selectPermission
      { permissionTable = "author",
        permissionSource = "postgres",
        permissionRole = "disallow-aggregation-queries",
        permissionColumns = ["id", "name"]
      },
    selectPermission
      { permissionTable = "article",
        permissionSource = "postgres",
        permissionRole = "disallow-aggregation-queries",
        permissionColumns = ["id", "title", "author_id"],
        permissionAllowAggregations = False
      }
  ]

--------------------------------------------------------------------------------
-- Tests

-- NOTE: `Schema.mkArrayRelationshipName`
-- the array relationship name differs in test due to the `Schema.mkArrayRelationshipName` helper
-- in production, the relationship name would be `#{schemaName}_article_articles_aggregate`

tests :: Fixture.Options -> SpecWith TestEnvironment
tests opts = do
  let shouldBe :: IO Value -> Value -> IO ()
      shouldBe = shouldReturnYaml opts

  it "Filters on aggregation predicate `count`" \testEnvironment -> do
    let schemaName = Schema.getSchemaName testEnvironment

    let expected :: Value
        expected =
          [interpolateYaml|
            data:
              #{schemaName}_author:
                - id: 1
                  name: Author 1
                - id: 2
                  name: Author 2
          |]

        actual :: IO Value
        actual =
          postGraphql
            testEnvironment
            [graphql|
              query {
                #{schemaName}_author(
                  where: {
                    # see NOTE: `Schema.mkArrayRelationshipName`
                    articles_by_id_to_author_id_aggregate: {
                      count: { predicate: { _gt: 1 } }
                    }
                  }
                ) {
                  id
                  name
                }
              }
            |]

    actual `shouldBe` expected

  it "Filters on aggregation predicate `bool_and` with one clause" \testEnvironment -> do
    let schemaName = Schema.getSchemaName testEnvironment

    let expected :: Value
        expected =
          [interpolateYaml|
            data:
              #{schemaName}_author:
              - name: Author 1
                id: 1
          |]

        actual :: IO Value
        actual =
          postGraphql
            testEnvironment
            [graphql|
              query {
                #{schemaName}_author(
                  where: {
                    articles_by_id_to_author_id_aggregate: {
                      bool_and: { arguments: { arg0: published }, predicate: { _eq: false } }
                    }
                  }
                ) {
                  id
                  name
                }
              }
            |]

    actual `shouldBe` expected

  it "Filters on aggregation predicate `bool_and` with more than one clause" \testEnvironment -> do
    let schemaName = Schema.getSchemaName testEnvironment

    let expected :: Value
        expected =
          [interpolateYaml|
            data:
              #{schemaName}_author: []
          |]

        actual :: IO Value
        actual =
          postGraphql
            testEnvironment
            [graphql|
              query {
                #{schemaName}_author(
                  where: {
                    articles_by_id_to_author_id_aggregate: {
                      bool_and: {
                        arguments: { arg0: published }
                        predicate: { _eq: true }
                        filter: { author_id: { _neq: 2 } }
                      }
                    }
                  }
                ) {
                  id
                  name
                }
              }
            |]

    actual `shouldBe` expected

  it "Filters on aggregation predicate and other filters" \testEnvironment -> do
    let schemaName = Schema.getSchemaName testEnvironment

    let expected :: Value
        expected =
          [interpolateYaml|
            data:
              #{schemaName}_author:
              - name: Author 2
                id: 2
          |]

        actual :: IO Value
        actual =
          postGraphql
            testEnvironment
            [graphql|
              query {
                #{schemaName}_author(
                  where: {
                    _and: {
                      # see NOTE: `Schema.mkArrayRelationshipName`
                      articles_by_id_to_author_id_aggregate: { count: { predicate: { _gt: 1 } } }
                      name: { _like: "%2" }
                    }
                  }
                ) {
                  id
                  name
                }
              }
            |]

    actual `shouldBe` expected

  describe "Exactly one predicate should be specified" do
    it "Fails when no predicates are specified" \testEnvironment -> do
      let schemaName = Schema.getSchemaName testEnvironment

      let expected :: Value
          expected =
            [interpolateYaml|
              errors:
                - extensions:
                    code: validation-failed
                    path: $.selectionSet.hasura_author.args.where.articles_by_id_to_author_id_aggregate
                  message: exactly one predicate should be specified
            |]

          actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                query {
                  #{schemaName}_author(
                    where: {
                      # see NOTE: `Schema.mkArrayRelationshipName`
                      articles_by_id_to_author_id_aggregate: {}
                    }
                  ) {
                    id
                    name
                  }
                }
              |]

      actual `shouldBe` expected

    it "Fails when more than one predicate is specified" \testEnvironment -> do
      let schemaName = Schema.getSchemaName testEnvironment

      let expected :: Value
          expected =
            [interpolateYaml|
              errors:
                - extensions:
                    code: validation-failed
                    path: $.selectionSet.hasura_author.args.where.articles_by_id_to_author_id_aggregate
                  message: exactly one predicate should be specified
            |]

          actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                query {
                  #{schemaName}_author(
                    where: {
                      articles_by_id_to_author_id_aggregate: {
                        bool_and: {
                          arguments: { arg0: published }
                          predicate: { _eq: true }
                          filter: { author_id: { _neq: 2 } }
                        }
                        count: { predicate: { _lte: 1 } }
                      }
                    }
                  ) {
                    id
                    name
                  }
                }
              |]

      actual `shouldBe` expected

  it "Respects select permissions on the column" \testEnvironment -> do
    let schemaName = Schema.getSchemaName testEnvironment

    let expected :: Value
        expected =
          [interpolateYaml|
            errors:
              - extensions:
                  code: validation-failed
                  path: $.selectionSet.#{schemaName}_author.args.where.articles_by_id_to_author_id_aggregate.count.arguments[0]
                message: expected one of the values ['id', 'author_id', 'title'] for type '#{schemaName}_article_select_column', but found 'published'
          |]

        actual :: IO Value
        actual =
          postGraphqlWithHeaders
            testEnvironment
            [("X-Hasura-Role", "role-select-author-name-only")]
            [graphql|
              query {
                #{schemaName}_author(
                  where: {
                    articles_by_id_to_author_id_aggregate: {
                      count: { predicate: { _gt: 1 }, arguments: published }
                    }
                  }
                ) {
                  id
                }
              }
            |]

    actual `shouldBe` expected

  it "Respects select permissions on the row" \testEnvironment -> do
    let schemaName = Schema.getSchemaName testEnvironment

    let expected :: Value
        expected =
          [interpolateYaml|
            data:
              #{schemaName}_author:
              - id: 1
                name: Author 1
          |]

        actual :: IO Value
        actual =
          postGraphqlWithHeaders
            testEnvironment
            [("X-Hasura-Role", "role-select-author-name-only")]
            [graphql|
              query {
                #{schemaName}_author(
                  where: {
                    articles_by_id_to_author_id_aggregate: {
                      count: { predicate: { _eq: 1 } }
                    }
                  }
                ) {
                  id
                  name
                }
              }
            |]

    actual `shouldBe` expected

  it "Respects 'aggregation queries' select permissions for a given role and table" \testEnvironment -> do
    let schemaName = Schema.getSchemaName testEnvironment

    let expected :: Value
        expected =
          [interpolateYaml|
            errors:
              - extensions:
                  code: validation-failed
                  path: $.selectionSet.hasura_author.args.where.articles_by_id_to_author_id_aggregate
                message: 'field ''articles_by_id_to_author_id_aggregate'' not found in type: ''hasura_author_bool_exp'''
          |]

        actual :: IO Value
        actual =
          postGraphqlWithHeaders
            testEnvironment
            [("X-Hasura-Role", "disallow-aggregation-queries")]
            [graphql|
              query {
                #{schemaName}_author(
                  where: {
                    articles_by_id_to_author_id_aggregate: {
                      count: { predicate: { _eq: 1 } }
                    }
                  }
                ) {
                  id
                }
              }
            |]

    actual `shouldBe` expected
