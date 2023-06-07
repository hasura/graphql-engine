{-# LANGUAGE QuasiQuotes #-}

-- | Test BigQuery-specific function definitions related to computed fields.
-- For generic tests of computed fields, see `Test.Schema.ComputedFields`
--
-- https://hasura.io/docs/latest/schema/bigquery/computed-fields/#bigquery-create-table-function
module Test.Databases.BigQuery.Schema.ComputedFields.TableSpec (spec) where

import Data.Aeson as J
import Data.List.NonEmpty qualified as NE
import Data.String.Interpolate (i)
import Harness.Backend.BigQuery qualified as BigQuery
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (interpolateYaml, yaml)
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
        [ (Fixture.fixture $ Fixture.Backend BigQuery.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ BigQuery.setupTablesAction schema testEnv
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
          Schema.column "name" Schema.TStr
        ],
      tablePrimaryKey = ["id"],
      tableData =
        [ [ Schema.VInt 1,
            Schema.VStr "Author 1"
          ],
          [ Schema.VInt 2,
            Schema.VStr "Author 2"
          ]
        ]
    }

articleTable :: Table
articleTable =
  (table "article")
    { tableColumns =
        [ Schema.column "id" Schema.TInt,
          Schema.column "title" Schema.TStr,
          Schema.column "content" Schema.TStr,
          Schema.column "author_id" Schema.TInt
        ],
      tablePrimaryKey = ["id"],
      tableData =
        [ [ Schema.VInt 1,
            Schema.VStr "Article 1 Title",
            Schema.VStr "Article 1 by Author 1",
            Schema.VInt 1
          ],
          [ Schema.VInt 2,
            Schema.VStr "Article 2 Title",
            Schema.VStr "Article 2 by Author 2",
            Schema.VInt 2
          ],
          [ Schema.VInt 3,
            Schema.VStr "Article 3 Title",
            Schema.VStr "Article 3 by Author 2, has search keyword",
            Schema.VInt 2
          ]
        ]
    }

-- ** Setup and teardown

setupFunction :: TestEnvironment -> [Fixture.SetupAction]
setupFunction testEnv =
  let schemaName = Schema.getSchemaName testEnv
   in [ Fixture.SetupAction
          { Fixture.setupAction =
              BigQuery.run_
                $ [i|
                CREATE TABLE FUNCTION #{ unSchemaName schemaName }.fetch_articles_implicit_return(a_id INT64, search STRING)
                AS
                SELECT article_alias.*
                FROM #{ unSchemaName schemaName }.article AS article_alias
                WHERE article_alias.author_id = a_id
                  AND (article_alias.title LIKE `search` OR article_alias.content LIKE `search`)
              |],
            Fixture.teardownAction = \_ -> pure ()
          },
        Fixture.SetupAction
          { Fixture.setupAction =
              BigQuery.run_
                $ [i|
                CREATE TABLE FUNCTION #{ unSchemaName schemaName }.fetch_articles_explicit_return(a_id INT64, search STRING)
                RETURNS TABLE<id INT64, title STRING, content STRING, author_id INT64> AS
                SELECT article_alias.id, article_alias.title, article_alias.content, article_alias.author_id
                FROM #{ unSchemaName schemaName }.article AS article_alias
                WHERE article_alias.author_id = a_id
                  AND (article_alias.title LIKE `search` OR article_alias.content LIKE `search`)
              |],
            Fixture.teardownAction = \_ -> pure ()
          }
      ]

setupMetadata :: TestEnvironment -> [Fixture.SetupAction]
setupMetadata testEnv =
  let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnv
      schemaName :: Schema.SchemaName
      schemaName = Schema.getSchemaName testEnv

      source :: String
      source = BackendType.backendSourceName backendTypeMetadata
   in -- Add computed fields
      [ Fixture.SetupAction
          { Fixture.setupAction =
              Schema.trackComputedField
                source
                authorTable
                "fetch_articles_implicit_return"
                "search_articles_implicit_return"
                [yaml| a_id: id |]
                [yaml|
                  name: article
                  dataset: *schemaName
                |]
                testEnv,
            Fixture.teardownAction = \_ -> pure ()
          },
        Fixture.SetupAction
          { Fixture.setupAction =
              Schema.trackComputedField
                source
                authorTable
                "fetch_articles_explicit_return"
                "search_articles_explicit_return"
                [yaml| a_id: id |]
                J.Null
                testEnv,
            Fixture.teardownAction = \_ -> pure ()
          }
      ]

-- * Tests

tests :: SpecWith TestEnvironment
tests = do
  -- This is a duplicate of `Test.Schema.ComputedFields.TableSpec/"Query with computed fields"
  -- but I thought it was clearer next to the counterexample that follows
  it "respects the `return_table` value in metadata, if it is not provided in the SQL statement" $ \testEnv -> do
    let schemaName = Schema.getSchemaName testEnv

    shouldReturnYaml
      testEnv
      ( GraphqlEngine.postGraphql
          testEnv
          [graphql|
            query {
              #{schemaName}_author(order_by: {id: asc}){
                id
                name
                search_articles_implicit_return(args: {search: "%keyword%"}){
                  id
                  title
                  content
                  author_id
                }
              }
            }
          |]
      )
      [interpolateYaml|
        data:
          #{schemaName}_author:
          - id: '1'
            name: Author 1
            search_articles_implicit_return: []
          - id: '2'
            name: Author 2
            search_articles_implicit_return:
            - id: '3'
              title: Article 3 Title
              content: Article 3 by Author 2, has search keyword
              author_id: '2'
      |]

  it "respects the return table based on the RETURNS TABLE<..> syntax in the SQL statement" $ \testEnv -> do
    let schemaName = Schema.getSchemaName testEnv

    shouldReturnYaml
      testEnv
      ( GraphqlEngine.postGraphql
          testEnv
          [graphql|
            query {
              #{schemaName}_author(order_by: {id: asc}){
                id
                name
                search_articles_explicit_return(args: {search: "%keyword%"}){
                  id
                  title
                  content
                  author_id
                }
              }
            }
          |]
      )
      [interpolateYaml|
        data:
          #{schemaName}_author:
          - id: '1'
            name: Author 1
            search_articles_explicit_return: []
          - id: '2'
            name: Author 2
            search_articles_explicit_return:
            - id: '3'
              title: Article 3 Title
              content: Article 3 by Author 2, has search keyword
              author_id: '2'
      |]

  -- This is a duplicate of `Test.Schema.ComputedFields.TableSpec/"Query respects limit and order_by"
  -- but I thought it was clearer next to the counterexample that follows
  it "respects limit and order_by when `returning_table` is specified in metadata" $ \testEnv -> do
    let schemaName = Schema.getSchemaName testEnv

    shouldReturnYaml
      testEnv
      ( GraphqlEngine.postGraphql
          testEnv
          [graphql|
            query {
              #{schemaName}_author(order_by: {id: asc}){
                id
                name
                search_articles_implicit_return(args: {search: "%by%"} limit: 1 order_by: {id: asc}){
                  id
                  title
                  content
                  author_id
                }
              }
            }
          |]
      )
      [interpolateYaml|
        data:
          #{schemaName}_author:
          - id: '1'
            name: Author 1
            search_articles_implicit_return:
            - author_id: '1'
              content: Article 1 by Author 1
              id: '1'
              title: Article 1 Title
          - id: '2'
            name: Author 2
            search_articles_implicit_return:
            - author_id: '2'
              content: Article 2 by Author 2
              id: '2'
              title: Article 2 Title
      |]

  it "throws an `order_by` error when `returning_table` is not specified in metadata" $ \testEnv -> do
    let schemaName = Schema.getSchemaName testEnv

    shouldReturnYaml
      testEnv
      ( GraphqlEngine.postGraphql
          testEnv
          [graphql|
            query {
              #{schemaName}_author(order_by: {id: asc}){
                id
                name
                search_articles_explicit_return(args: {search: "%by%"} limit: 1 order_by: {id: asc}){
                  id
                  title
                  content
                  author_id
                }
              }
            }
          |]
      )
      [interpolateYaml|
        errors:
        - extensions:
            path: $.selectionSet.#{schemaName}_author.selectionSet.search_articles_explicit_return
            code: validation-failed
          message: |-
            'search_articles_explicit_return' has no argument named 'order_by'
      |]
