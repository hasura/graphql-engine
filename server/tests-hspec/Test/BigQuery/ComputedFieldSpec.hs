{-# LANGUAGE QuasiQuotes #-}

-- | All tests related to computed fields in a BigQuery source
module Test.BigQuery.ComputedFieldSpec (spec) where

import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Harness.Backend.BigQuery qualified as BigQuery
import Harness.Exceptions (finally)
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (interpolateYaml, yaml)
import Harness.Test.Context qualified as Context
import Harness.Test.Schema (Table (..), table)
import Harness.Test.Schema qualified as Schema
import Harness.Test.SchemaName
import Harness.TestEnvironment (TestEnvironment)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, it)

-- ** Preamble

spec :: SpecWith TestEnvironment
spec =
  Context.run
    ( NE.fromList
        [ Context.Context
            { name = Context.Backend Context.BigQuery,
              mkLocalTestEnvironment = Context.noLocalTestEnvironment,
              setup = bigquerySetup,
              teardown = bigqueryTeardown,
              customOptions = Nothing
            }
        ]
    )
    tests

-- ** Setup and teardown

bigquerySetup :: (TestEnvironment, ()) -> IO ()
bigquerySetup (testEnv, ()) = do
  BigQuery.setup [authorTable, articleTable] (testEnv, ())
  let schemaName = getSchemaName testEnv

  -- Create functions in BigQuery
  BigQuery.runSql_ (createFunctionsSQL schemaName)

  -- Add computed fields and define select permissions
  GraphqlEngine.postMetadata_
    testEnv
    [yaml|
type: bulk
args:
- type: bigquery_add_computed_field
  args:
    source: bigquery
    name: search_articles_1
    table:
      dataset: *schemaName
      name: author
    definition:
      function:
        dataset: *schemaName
        name: fetch_articles_returns_table
      argument_mapping:
        a_id: id

- type: bigquery_add_computed_field
  args:
    source: bigquery
    name: search_articles_2
    table:
      dataset: *schemaName
      name: author
    definition:
      function:
        dataset: *schemaName
        name: fetch_articles
      argument_mapping:
        a_id: id
      return_table:
        name: article
        dataset: *schemaName

# Role user_1 has select permissions on author and article tables.
# user_1 can query search_articles_1 computed field.
- type: bigquery_create_select_permission
  args:
    source: bigquery
    table:
      dataset: *schemaName
      name: author
    role: user_1
    permission:
      columns: '*'
      filter: {}
      computed_fields:
      - search_articles_1

- type: bigquery_create_select_permission
  args:
    source: bigquery
    table:
      dataset: *schemaName
      name: article
    role: user_1
    permission:
      columns: '*'
      filter: {}

# Role user_2 has select permissions only on author table.
- type: bigquery_create_select_permission
  args:
    source: bigquery
    table:
      dataset: *schemaName
      name: author
    role: user_2
    permission:
      columns: '*'
      filter: {}
|]

bigqueryTeardown :: (TestEnvironment, ()) -> IO ()
bigqueryTeardown (testEnv, ()) = do
  -- Drop permissions and computed fields metadata
  let schemaName = getSchemaName testEnv

  let dropComputedFieldsYaml =
        [yaml|
type: bulk
args:
- type: bigquery_drop_select_permission
  args:
    source: bigquery
    table:
      dataset: *schemaName
      name: author
    role: user_1

- type: bigquery_drop_select_permission
  args:
    source: bigquery
    table:
      dataset: *schemaName
      name: article
    role: user_1

- type: bigquery_drop_select_permission
  args:
    source: bigquery
    table:
      dataset: *schemaName
      name: author
    role: user_2

- type: bigquery_drop_computed_field
  args:
    source: bigquery
    name: search_articles_1
    table:
      dataset: *schemaName
      name: author

- type: bigquery_drop_computed_field
  args:
    source: bigquery
    name: search_articles_2
    table:
      dataset: *schemaName
      name: author
|]
  finally
    (GraphqlEngine.postMetadata_ testEnv dropComputedFieldsYaml)
    ( finally
        -- Drop functions in BigQuery database
        (BigQuery.runSql_ (dropFunctionsSQL schemaName))
        -- Teardown schema
        (BigQuery.teardown [authorTable, articleTable] (testEnv, ()))
    )

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

fetch_articles_returns_table :: SchemaName -> T.Text
fetch_articles_returns_table schemaName =
  unSchemaName schemaName <> ".fetch_articles_returns_table"

fetch_articles :: SchemaName -> T.Text
fetch_articles schemaName =
  unSchemaName schemaName <> ".fetch_articles"

createFunctionsSQL :: SchemaName -> String
createFunctionsSQL schemaName =
  T.unpack $
    T.unwords $
      [ "CREATE TABLE FUNCTION ",
        fetch_articles_returns_table schemaName,
        "(a_id INT64, search STRING)",
        "RETURNS TABLE<id INT64, title STRING, content STRING>",
        "AS (",
        "SELECT t.id, t.title, t.content FROM",
        articleTableSQL,
        "AS t WHERE t.author_id = a_id and (t.title LIKE `search` OR t.content LIKE `search`)",
        ");"
      ]
        <> [ "CREATE TABLE FUNCTION ",
             fetch_articles schemaName,
             "(a_id INT64, search STRING)",
             "AS (",
             "SELECT t.* FROM",
             articleTableSQL,
             "AS t WHERE t.author_id = a_id and (t.title LIKE `search` OR t.content LIKE `search`)",
             ");"
           ]
  where
    articleTableSQL = unSchemaName schemaName <> ".article"

dropFunctionsSQL :: SchemaName -> String
dropFunctionsSQL schemaName =
  T.unpack $
    T.unwords $
      [ "DROP TABLE FUNCTION " <> fetch_articles_returns_table schemaName <> ";",
        "DROP TABLE FUNCTION " <> fetch_articles schemaName <> ";"
      ]

-- * Tests

tests :: Context.Options -> SpecWith TestEnvironment
tests opts = do
  it "Query with computed fields" $ \testEnv -> do
    let schemaName = getSchemaName testEnv

    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          testEnv
          [graphql|
query {
  #{schemaName}_author(order_by: {id: asc}){
    id
    name
    search_articles_1(args: {search: "%1%"}){
      id
      title
      content
    }
    search_articles_2(args: {search: "%keyword%"}){
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
    search_articles_1:
    - id: '1'
      title: Article 1 Title
      content: Article 1 by Author 1
    search_articles_2: []
  - id: '2'
    name: Author 2
    search_articles_1: []
    search_articles_2:
    - id: '3'
      title: Article 3 Title
      content: Article 3 by Author 2, has search keyword
      author_id: '2'
|]

  it "Query with computed fields using limit and order_by" $ \testEnv -> do
    let schemaName = getSchemaName testEnv

    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          testEnv
          [graphql|
query {
  #{schemaName}_author(order_by: {id: asc}){
    id
    name
    search_articles_2(args: {search: "%by%"} limit: 1 order_by: {id: asc}){
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
    search_articles_2:
    - author_id: '1'
      content: Article 1 by Author 1
      id: '1'
      title: Article 1 Title
  - id: '2'
    name: Author 2
    search_articles_2:
    - author_id: '2'
      content: Article 2 by Author 2
      id: '2'
      title: Article 2 Title
|]

  it "Query with computed fields as user_1 role" $ \testEnv -> do
    let schemaName = getSchemaName testEnv

    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphqlWithHeaders
          testEnv
          [("X-Hasura-Role", "user_1")]
          [graphql|
query {
  #{schemaName}_author(order_by: {id: asc}){
    id
    name
    search_articles_1(args: {search: "%1%"}){
      id
      title
      content
    }
    search_articles_2(args: {search: "%keyword%"}){
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
    search_articles_1:
    - id: '1'
      title: Article 1 Title
      content: Article 1 by Author 1
    search_articles_2: []
  - id: '2'
    name: Author 2
    search_articles_1: []
    search_articles_2:
    - id: '3'
      title: Article 3 Title
      content: Article 3 by Author 2, has search keyword
      author_id: '2'
|]

  it "Query with computed field search_articles_1 as user_2 role" $ \testEnv -> do
    let schemaName = getSchemaName testEnv

    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphqlWithHeaders
          testEnv
          [("X-Hasura-Role", "user_2")]
          [graphql|
query {
  #{schemaName}_author(order_by: {id: asc}){
    id
    name
    search_articles_1(args: {search: "%1%"}){
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
    path: "$.selectionSet.#{schemaName}_author.selectionSet.search_articles_1"
    code: validation-failed
  message: |-
    field 'search_articles_1' not found in type: '#{schemaName}_author'
|]

  it "Query with computed field search_articles_2 as user_2 role" $ \testEnv -> do
    let schemaName = getSchemaName testEnv

    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphqlWithHeaders
          testEnv
          [("X-Hasura-Role", "user_2")]
          [graphql|
query {
  #{schemaName}_author(order_by: {id: asc}){
    id
    name
    search_articles_2(args: {search: "%keyword%"}){
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
    path: "$.selectionSet.#{schemaName}_author.selectionSet.search_articles_2"
    code: validation-failed
  message: |-
    field 'search_articles_2' not found in type: '#{schemaName}_author'
|]
