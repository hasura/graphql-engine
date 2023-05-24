{-# LANGUAGE QuasiQuotes #-}

-- | Tests table computed fields whose associated SQL function returns a table
--
-- https://hasura.io/docs/latest/schema/postgres/computed-fields/#2-table-computed-fields
-- https://hasura.io/docs/latest/schema/bigquery/computed-fields/
module Test.Schema.ComputedFields.TableSpec (spec) where

import Data.List.NonEmpty qualified as NE
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Harness.Backend.BigQuery qualified as BigQuery
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Permissions (Permission (SelectPermission), SelectPermissionDetails (..), selectPermission)
import Harness.Permissions qualified as Permission
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
        [ (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Postgres.setupTablesAction schema testEnv
                ]
                  <> postgresSetupFunctions testEnv
                  <> setupMetadata testEnv
            },
          (Fixture.fixture $ Fixture.Backend BigQuery.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ BigQuery.setupTablesAction schema testEnv
                ]
                  <> bigquerySetupFunctions testEnv
                  <> setupMetadata testEnv,
              Fixture.customOptions =
                Just
                  $ Fixture.defaultOptions
                    { Fixture.stringifyNumbers = True
                    }
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

postgresSetupFunctions :: TestEnvironment -> [Fixture.SetupAction]
postgresSetupFunctions testEnv =
  let schemaName = Schema.getSchemaName testEnv
      articleTableSQL = unSchemaName schemaName <> ".article"
   in [ Fixture.SetupAction
          { Fixture.setupAction =
              Postgres.run_ testEnv
                $ [i|
                  CREATE FUNCTION #{ fetch_articles schemaName }(author_row author, search TEXT)
                  RETURNS SETOF article AS $$
                    SELECT *
                    FROM #{ articleTableSQL }
                    WHERE
                      ( title ilike ('%' || search || '%')
                        OR content ilike ('%' || search || '%')
                      ) AND author_id = author_row.id
                  $$ LANGUAGE sql STABLE;
                |],
            Fixture.teardownAction = \_ -> pure ()
          },
        Fixture.SetupAction
          { Fixture.setupAction =
              Postgres.run_ testEnv
                $ [i|
                  CREATE FUNCTION #{ fetch_articles_no_user_args schemaName }(author_row author)
                  RETURNS SETOF article AS $$
                    SELECT *
                    FROM #{ articleTableSQL }
                    WHERE author_id = author_row.id
                  $$ LANGUAGE sql STABLE;
                |],
            Fixture.teardownAction = \_ -> pure ()
          }
      ]

bigquerySetupFunctions :: TestEnvironment -> [Fixture.SetupAction]
bigquerySetupFunctions testEnv =
  let schemaName = Schema.getSchemaName testEnv
      articleTableSQL = unSchemaName schemaName <> ".article"
   in [ Fixture.SetupAction
          { Fixture.setupAction =
              BigQuery.run_
                $ [i|
                  CREATE TABLE FUNCTION
                  #{ fetch_articles schemaName }(a_id INT64, search STRING)
                  AS
                  (
                    SELECT t.* FROM #{ articleTableSQL } AS t
                    WHERE t.author_id = a_id and (t.title LIKE `search` OR t.content LIKE `search`)
                  )
                |],
            Fixture.teardownAction = \_ -> pure ()
          },
        Fixture.SetupAction
          { Fixture.setupAction =
              BigQuery.run_
                $ [i|
                  CREATE TABLE FUNCTION
                  #{ fetch_articles_no_user_args schemaName }(a_id INT64)
                  AS
                  (
                    SELECT t.* FROM #{ articleTableSQL } AS t
                    WHERE t.author_id = a_id
                  )
                |],
            Fixture.teardownAction = \_ -> pure ()
          }
      ]

fetch_articles :: SchemaName -> T.Text
fetch_articles schemaName =
  unSchemaName schemaName <> ".fetch_articles"

fetch_articles_no_user_args :: SchemaName -> T.Text
fetch_articles_no_user_args schemaName =
  unSchemaName schemaName <> ".fetch_articles_no_user_args"

setupMetadata :: TestEnvironment -> [Fixture.SetupAction]
setupMetadata testEnvironment =
  let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
      schemaName :: Schema.SchemaName
      schemaName = Schema.getSchemaName testEnvironment

      source :: String
      source = BackendType.backendSourceName backendTypeMetadata
   in -- Add computed fields and define select permissions
      [ Fixture.SetupAction
          { Fixture.setupAction =
              Schema.trackComputedField
                source
                authorTable
                "fetch_articles"
                "search_articles"
                [yaml| a_id: id |]
                [yaml|
                name: article
                dataset: *schemaName
              |]
                testEnvironment,
            Fixture.teardownAction = \_ -> pure ()
          },
        Fixture.SetupAction
          { Fixture.setupAction =
              Schema.trackComputedField
                source
                authorTable
                "fetch_articles_no_user_args"
                "articles_no_search"
                [yaml| a_id: id |]
                [yaml|
                  name: article
                  dataset: *schemaName
                |]
                testEnvironment,
            Fixture.teardownAction = \_ -> pure ()
          },
        Fixture.SetupAction
          { Fixture.setupAction =
              GraphqlEngine.postMetadata_ testEnvironment do
                -- Role user_1 has select permissions on author and article tables.
                -- user_1 can query search_articles computed field.
                Permission.createPermissionMetadata
                  testEnvironment
                  $ SelectPermission
                    selectPermission
                      { selectPermissionTable = "author",
                        selectPermissionRole = "user_1",
                        selectPermissionColumns = (["id", "name"] :: [Text])
                      },
            Fixture.teardownAction = \_ -> pure ()
          },
        Fixture.SetupAction
          { Fixture.setupAction =
              GraphqlEngine.postMetadata_ testEnvironment do
                Permission.createPermissionMetadata
                  testEnvironment
                  $ SelectPermission
                    selectPermission
                      { selectPermissionTable = "article",
                        selectPermissionRole = "user_1",
                        selectPermissionColumns = (["id", "title", "content", "author_id"] :: [Text])
                      },
            Fixture.teardownAction = \_ -> pure ()
          },
        Fixture.SetupAction
          { Fixture.setupAction =
              GraphqlEngine.postMetadata_ testEnvironment do
                -- Role user_2 has select permissions only on author table.
                Permission.createPermissionMetadata
                  testEnvironment
                  $ SelectPermission
                    selectPermission
                      { selectPermissionTable = "author",
                        selectPermissionRole = "user_2",
                        selectPermissionColumns = (["id", "name"] :: [Text])
                      },
            Fixture.teardownAction = \_ -> pure ()
          }
      ]

-- * Tests

tests :: SpecWith TestEnvironment
tests = do
  it "Query with computed fields" $ \testEnv -> do
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
                search_articles(args: {search: "%1%"}){
                  id
                  title
                  content
                }
              }
            }
          |]
      )
      [interpolateYaml|
        data:
          #{schemaName}_author:
          - id: 1
            name: Author 1
            search_articles:
            - id: 1
              title: Article 1 Title
              content: Article 1 by Author 1
          - id: 2
            name: Author 2
            search_articles: []
      |]

  it "Query with computed fields as user_1 role" $ \testEnv -> do
    let schemaName = Schema.getSchemaName testEnv

    shouldReturnYaml
      testEnv
      ( GraphqlEngine.postGraphqlWithHeaders
          testEnv
          [("X-Hasura-Role", "user_1")]
          [graphql|
            query {
              #{schemaName}_author(order_by: {id: asc}){
                id
                name
                search_articles(args: {search: "%1%"}){
                  id
                  title
                  content
                }
              }
            }
          |]
      )
      [interpolateYaml|
        data:
          #{schemaName}_author:
          - id: 1
            name: Author 1
            search_articles:
            - id: 1
              title: Article 1 Title
              content: Article 1 by Author 1
          - id: 2
            name: Author 2
            search_articles: []
      |]

  it "Query with computed field search_articles as user_2 role" $ \testEnv -> do
    let schemaName = Schema.getSchemaName testEnv

    shouldReturnYaml
      testEnv
      ( GraphqlEngine.postGraphqlWithHeaders
          testEnv
          [("X-Hasura-Role", "user_2")]
          [graphql|
            query {
              #{schemaName}_author(order_by: {id: asc}){
                id
                name
                search_articles(args: {search: "%1%"}){
                  id
                  title
                  content
                }
              }
            }
          |]
      )
      [interpolateYaml|
        errors:
        - extensions:
            path: "$.selectionSet.#{schemaName}_author.selectionSet.search_articles"
            code: validation-failed
          message: |-
            field 'search_articles' not found in type: '#{schemaName}_author'
        |]

  it "Query with computed field search_articles as user_2 role" $ \testEnv -> do
    let schemaName = Schema.getSchemaName testEnv

    shouldReturnYaml
      testEnv
      ( GraphqlEngine.postGraphqlWithHeaders
          testEnv
          [("X-Hasura-Role", "user_2")]
          [graphql|
            query {
              #{schemaName}_author(order_by: {id: asc}){
                id
                name
                search_articles(args: {search: "%keyword%"}){
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
            path: "$.selectionSet.#{schemaName}_author.selectionSet.search_articles"
            code: validation-failed
          message: |-
            field 'search_articles' not found in type: '#{schemaName}_author'
      |]

  it "Query articles_no_search without user arguments" $ \testEnv -> do
    let schemaName = Schema.getSchemaName testEnv

    shouldReturnYaml
      testEnv
      ( GraphqlEngine.postGraphql
          testEnv
          [graphql|
            query {
              #{schemaName}_author(order_by: {id: asc}){
                id
                articles_no_search(order_by: {id: asc}){
                  id
                }
              }
            }
          |]
      )
      [interpolateYaml|
        data:
          #{schemaName}_author:
          - id: 1
            articles_no_search:
            - id: 1
          - id: 2
            articles_no_search:
            - id: 2
            - id: 3
      |]

  it "Query respects limit and order_by" $ \testEnv -> do
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
                search_articles(args: {search: "%by%"} limit: 1 order_by: {id: asc}){
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
          - id: 1
            name: Author 1
            search_articles:
            - author_id: 1
              content: Article 1 by Author 1
              id: 1
              title: Article 1 Title
          - id: 2
            name: Author 2
            search_articles:
            - author_id: 2
              content: Article 2 by Author 2
              id: 2
              title: Article 2 Title
      |]
