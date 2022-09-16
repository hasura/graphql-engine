{-# LANGUAGE QuasiQuotes #-}

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
import Harness.GraphqlEngine (postGraphql)
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (interpolateYaml)
import Harness.Test.Fixture qualified as Fixture
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
                [ Postgres.setupTablesAction schema testEnv
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
            [Schema.VInt 3, Schema.VStr "Article 3", Schema.VBool True, Schema.VInt 2]
          ]
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

  it "Filter on aggregation predicate `count`" \testEnvironment -> do
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

  it "Filter on aggregation predicate `bool_and` with one clause" \testEnvironment -> do
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

  it "Filter on aggregation predicate `bool_and` with more than one clause" \testEnvironment -> do
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

  it "Filter on aggregation predicate and other filters" \testEnvironment -> do
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
