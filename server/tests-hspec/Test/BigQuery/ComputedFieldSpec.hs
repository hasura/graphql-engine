{-# LANGUAGE QuasiQuotes #-}

-- | All tests related to computed fields in a BigQuery source
module Test.BigQuery.ComputedFieldSpec (spec) where

import Data.Text qualified as T
import Harness.Backend.BigQuery qualified as BigQuery
import Harness.Constants qualified as Constants
import Harness.Exceptions (finally)
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (shouldReturnYaml, yaml)
import Harness.Test.Context qualified as Context
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (TestEnvironment)
import Test.Hspec (SpecWith, it)
import Prelude

-- ** Preamble

spec :: SpecWith TestEnvironment
spec =
  Context.run
    [ Context.Context
        { name = Context.Backend Context.BigQuery,
          mkLocalTestEnvironment = Context.noLocalTestEnvironment,
          setup = bigquerySetup,
          teardown = bigqueryTeardown,
          customOptions = Nothing
        }
    ]
    tests

-- ** Setup and teardown

bigquerySetup :: (TestEnvironment, ()) -> IO ()
bigquerySetup (testEnv, ()) = do
  BigQuery.setup [authorTable, articleTable] (testEnv, ())

  -- Create functions in BigQuery
  BigQuery.runSql_ createFunctionsSQL

  -- Add computed fields
  let dataset = Constants.bigqueryDataset
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
      dataset: *dataset
      name: author
    definition:
      function:
        dataset: *dataset
        name: fetch_articles_returns_table
      argument_mapping:
        a_id: id

- type: bigquery_add_computed_field
  args:
    source: bigquery
    name: search_articles_2
    table:
      dataset: *dataset
      name: author
    definition:
      function:
        dataset: *dataset
        name: fetch_articles
      argument_mapping:
        a_id: id
      return_table:
        name: article
        dataset: *dataset
|]

bigqueryTeardown :: (TestEnvironment, ()) -> IO ()
bigqueryTeardown (testEnv, ()) = do
  -- Drop computed fields metadata
  let dataset = Constants.bigqueryDataset
      dropComputedFieldsYaml =
        [yaml|
type: bulk
args:
- type: bigquery_drop_computed_field
  args:
    source: bigquery
    name: search_articles_1
    table:
      dataset: *dataset
      name: author

- type: bigquery_drop_computed_field
  args:
    source: bigquery
    name: search_articles_2
    table:
      dataset: *dataset
      name: author
|]
  finally
    (GraphqlEngine.postMetadata_ testEnv dropComputedFieldsYaml)
    ( finally
        -- Drop functions in BigQuery database
        (BigQuery.runSql_ dropFunctionsSQL)
        -- Teardown schema
        (BigQuery.teardown [authorTable, articleTable] (testEnv, ()))
    )

authorTable :: Schema.Table
authorTable =
  Schema.Table
    "author"
    [ Schema.column "id" Schema.TInt,
      Schema.column "name" Schema.TStr
    ]
    ["id"]
    []
    [ [ Schema.VInt 1,
        Schema.VStr "Author 1"
      ],
      [ Schema.VInt 2,
        Schema.VStr "Author 2"
      ]
    ]

articleTable :: Schema.Table
articleTable =
  Schema.Table
    "article"
    [ Schema.column "id" Schema.TInt,
      Schema.column "title" Schema.TStr,
      Schema.column "content" Schema.TStr,
      Schema.column "author_id" Schema.TInt
    ]
    ["id"]
    []
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

fetch_articles_returns_table :: T.Text
fetch_articles_returns_table =
  T.pack Constants.bigqueryDataset <> ".fetch_articles_returns_table"

fetch_articles :: T.Text
fetch_articles =
  T.pack Constants.bigqueryDataset <> ".fetch_articles"

createFunctionsSQL :: String
createFunctionsSQL =
  T.unpack $
    T.unwords $
      [ "CREATE TABLE FUNCTION ",
        fetch_articles_returns_table,
        "(a_id INT64, search STRING)",
        "RETURNS TABLE<id INT64, title STRING, content STRING>",
        "AS (",
        "SELECT t.id, t.title, t.content FROM",
        articleTableSQL,
        "AS t WHERE t.author_id = a_id and (t.title LIKE `search` OR t.content LIKE `search`)",
        ");"
      ]
        <> [ "CREATE TABLE FUNCTION ",
             fetch_articles,
             "(a_id INT64, search STRING)",
             "AS (",
             "SELECT t.* FROM",
             articleTableSQL,
             "AS t WHERE t.author_id = a_id and (t.title LIKE `search` OR t.content LIKE `search`)",
             ");"
           ]
  where
    articleTableSQL = T.pack Constants.bigqueryDataset <> ".article"

dropFunctionsSQL :: String
dropFunctionsSQL =
  T.unpack $
    T.unwords $
      [ "DROP TABLE FUNCTION " <> fetch_articles_returns_table <> ";",
        "DROP TABLE FUNCTION " <> fetch_articles <> ";"
      ]

-- * Tests

tests :: Context.Options -> SpecWith TestEnvironment
tests opts = do
  it "Query with computed fields" $ \testEnv ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          testEnv
          [graphql|
query {
  hasura_author(order_by: {id: asc}){
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
      [yaml|
data:
  hasura_author:
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
