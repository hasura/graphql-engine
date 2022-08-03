{-# LANGUAGE QuasiQuotes #-}

-- | All tests related to metadata API for computed fields in a BigQuery source
module Test.BigQuery.Metadata.ComputedFieldSpec (spec) where

import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Harness.Backend.BigQuery qualified as BigQuery
import Harness.Constants qualified as Constants
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml (shouldReturnYaml, yaml)
import Harness.Test.Context qualified as Context
import Harness.Test.Schema (Table (..), table)
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (TestEnvironment)
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

  -- Create functions in BigQuery
  BigQuery.runSql_ createFunctionsSQL

bigqueryTeardown :: (TestEnvironment, ()) -> IO ()
bigqueryTeardown (testEnv, ()) = do
  -- Drop functions in BigQuery database
  BigQuery.runSql_ dropFunctionsSQL

  -- Teardown schema
  BigQuery.teardown [authorTable, articleTable] (testEnv, ())

authorTable :: Schema.Table
authorTable =
  (table "author")
    { tableColumns =
        [ Schema.column "id" Schema.TInt,
          Schema.column "name" Schema.TStr
        ],
      tablePrimaryKey = ["id"]
    }

articleTable :: Schema.Table
articleTable =
  (table "article")
    { tableColumns =
        [ Schema.column "id" Schema.TInt,
          Schema.column "title" Schema.TStr,
          Schema.column "content" Schema.TStr,
          Schema.column "author_id" Schema.TInt
        ],
      tablePrimaryKey = ["id"]
    }

fetch_articles_returns_table :: T.Text
fetch_articles_returns_table =
  T.pack Constants.bigqueryDataset <> ".fetch_articles_returns_table"

fetch_articles :: T.Text
fetch_articles =
  T.pack Constants.bigqueryDataset <> ".fetch_articles"

function_no_args :: T.Text
function_no_args =
  T.pack Constants.bigqueryDataset <> ".function_no_args"

add_int :: T.Text
add_int =
  T.pack Constants.bigqueryDataset <> ".add_int"

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
        <> [ "CREATE TABLE FUNCTION ",
             function_no_args <> "()",
             "AS (",
             "SELECT t.* FROM",
             articleTableSQL,
             "AS t);"
           ]
        -- A scalar function
        <> [ "CREATE FUNCTION ",
             add_int <> "(a INT64, b INT64)",
             "RETURNS INT64 AS (a + b);"
           ]
  where
    articleTableSQL = T.pack Constants.bigqueryDataset <> ".article"

dropFunctionsSQL :: String
dropFunctionsSQL =
  T.unpack $
    T.unwords $
      [ "DROP TABLE FUNCTION " <> fetch_articles_returns_table <> ";",
        "DROP TABLE FUNCTION " <> fetch_articles <> ";",
        "DROP TABLE FUNCTION " <> function_no_args <> ";",
        "DROP FUNCTION " <> add_int <> ";"
      ]

-- * Tests

tests :: Context.Options -> SpecWith TestEnvironment
tests opts = do
  let dataset = Constants.bigqueryDataset
  it "Add computed field with non exist function - exception" $ \testEnv ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postMetadataWithStatus
          400
          testEnv
          [yaml|
type: bigquery_add_computed_field
args:
  source: bigquery
  name: search_articles_1
  table:
    dataset: *dataset
    name: author
  definition:
    function:
      dataset: *dataset
      name: non_exist_function_name
    argument_mapping:
      a_id: id
|]
      )
      [yaml|
internal:
- definition:
    definition:
      function:
        dataset: hasura
        name: non_exist_function_name
      argument_mapping:
        a_id: id
    name: search_articles_1
    source: bigquery
    comment:
    table:
      dataset: hasura
      name: author
  reason: 'Inconsistent object: in table "hasura.author": in computed field "search_articles_1":
    no such function exists: "hasura.non_exist_function_name"'
  name: computed_field search_articles_1 in table hasura.author in source bigquery
  type: computed_field
path: "$.args"
error: 'Inconsistent object: in table "hasura.author": in computed field "search_articles_1":
  no such function exists: "hasura.non_exist_function_name"'
code: invalid-configuration
|]

  it "Add computed field without returning table - exception" $ \testEnv ->
    -- The function 'fetch_articles' is not defined with 'RETURNS TABLE<>' clause,
    -- we need to provide `return_table` in the payload
    shouldReturnYaml
      opts
      ( GraphqlEngine.postMetadataWithStatus
          400
          testEnv
          [yaml|
type: bigquery_add_computed_field
args:
  source: bigquery
  name: search_articles
  table:
    dataset: *dataset
    name: author
  definition:
    function:
      dataset: *dataset
      name: fetch_articles
    argument_mapping:
      a_id: id
#    return_table:
#      name: article
#      dataset: *dataset
|]
      )
      [yaml|
internal:
- definition:
    definition:
      function:
        dataset: hasura
        name: fetch_articles
      argument_mapping:
        a_id: id
    name: search_articles
    source: bigquery
    comment:
    table:
      dataset: hasura
      name: author
  reason: 'Inconsistent object: in table "hasura.author": in computed field "search_articles":
    the computed field "search_articles" cannot be added to table "hasura.author"
    because function "hasura.fetch_articles" is not defined with ''RETURNS TABLE''.
    Expecting return table name.'
  name: computed_field search_articles in table hasura.author in source bigquery
  type: computed_field
path: "$.args"
error: 'Inconsistent object: in table "hasura.author": in computed field "search_articles":
  the computed field "search_articles" cannot be added to table "hasura.author" because
  function "hasura.fetch_articles" is not defined with ''RETURNS TABLE''. Expecting
  return table name.'
code: invalid-configuration
|]

  it "Add computed field with non exist returning table - exception" $ \testEnv ->
    -- The function 'fetch_articles' is not defined with 'RETURNS TABLE<>' clause,
    -- we need to provide `return_table` in the payload
    shouldReturnYaml
      opts
      ( GraphqlEngine.postMetadataWithStatus
          400
          testEnv
          [yaml|
type: bigquery_add_computed_field
args:
  source: bigquery
  name: search_articles
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
      name: non_exist_table
      dataset: *dataset
|]
      )
      [yaml|
internal:
- definition:
    definition:
      function:
        dataset: hasura
        name: fetch_articles
      return_table:
        dataset: hasura
        name: non_exist_table
      argument_mapping:
        a_id: id
    name: search_articles
    source: bigquery
    comment:
    table:
      dataset: hasura
      name: author
  reason: 'Inconsistent object: in table "hasura.author": in computed field "search_articles":
    the computed field "search_articles" cannot be added to table "hasura.author"
    because function "hasura.fetch_articles" returning set of table "hasura.non_exist_table"
    is not tracked'
  name: computed_field search_articles in table hasura.author in source bigquery
  type: computed_field
path: "$.args"
error: 'Inconsistent object: in table "hasura.author": in computed field "search_articles":
  the computed field "search_articles" cannot be added to table "hasura.author" because
  function "hasura.fetch_articles" returning set of table "hasura.non_exist_table"
  is not tracked'
code: invalid-configuration
|]

  it "Add computed field with returning table when it is not required - exception" $ \testEnv ->
    -- The function 'fetch_articles_returns_table' is defined with 'RETURNS TABLE<>' clause,
    -- we don't need to provide 'return_table' in the payload as the returning fields are inferred
    -- from the function definition
    shouldReturnYaml
      opts
      ( GraphqlEngine.postMetadataWithStatus
          400
          testEnv
          [yaml|
type: bigquery_add_computed_field
args:
  source: bigquery
  name: search_articles
  table:
    dataset: *dataset
    name: author
  definition:
    function:
      dataset: *dataset
      name: fetch_articles_returns_table
    argument_mapping:
      a_id: id
    return_table:
      name: article
      dataset: *dataset
|]
      )
      [yaml|
internal:
- definition:
    definition:
      function:
        dataset: hasura
        name: fetch_articles_returns_table
      return_table:
        dataset: hasura
        name: article
      argument_mapping:
        a_id: id
    name: search_articles
    source: bigquery
    comment:
    table:
      dataset: hasura
      name: author
  reason: 'Inconsistent object: in table "hasura.author": in computed field "search_articles":
    the computed field "search_articles" cannot be added to table "hasura.author"
    because return table "hasura.article" is not required as the function "hasura.fetch_articles_returns_table"
    returns arbitrary column fields'
  name: computed_field search_articles in table hasura.author in source bigquery
  type: computed_field
path: "$.args"
error: 'Inconsistent object: in table "hasura.author": in computed field "search_articles":
  the computed field "search_articles" cannot be added to table "hasura.author" because
  return table "hasura.article" is not required as the function "hasura.fetch_articles_returns_table"
  returns arbitrary column fields'
code: invalid-configuration
|]

  it "Add computed field with a function that has no input arguments - exception" $ \testEnv ->
    -- The function 'function_no_args' has no input arguments
    shouldReturnYaml
      opts
      ( GraphqlEngine.postMetadataWithStatus
          400
          testEnv
          [yaml|
type: bigquery_add_computed_field
args:
  source: bigquery
  name: search_articles
  table:
    dataset: *dataset
    name: author
  definition:
    function:
      dataset: *dataset
      name: function_no_args
    argument_mapping:
      a_id: id
    return_table:
      name: article
      dataset: *dataset
|]
      )
      [yaml|
internal:
- definition:
    definition:
      function:
        dataset: hasura
        name: function_no_args
      return_table:
        dataset: hasura
        name: article
      argument_mapping:
        a_id: id
    name: search_articles
    source: bigquery
    comment:
    table:
      dataset: hasura
      name: author
  reason: 'Inconsistent object: in table "hasura.author": in computed field "search_articles":
    the computed field "search_articles" cannot be added to table "hasura.author"
    because function "hasura.function_no_args" has no input arguments defined'
  name: computed_field search_articles in table hasura.author in source bigquery
  type: computed_field
path: "$.args"
error: 'Inconsistent object: in table "hasura.author": in computed field "search_articles":
  the computed field "search_articles" cannot be added to table "hasura.author" because
  function "hasura.function_no_args" has no input arguments defined'
code: invalid-configuration
|]

  it "Add computed field with a function that returns a scalar value - exception" $ \testEnv ->
    -- The function 'add_int' returns a scalar value of type 'INT64', as of now we do not support
    -- scalar computed fields.
    shouldReturnYaml
      opts
      ( GraphqlEngine.postMetadataWithStatus
          400
          testEnv
          [yaml|
type: bigquery_add_computed_field
args:
  source: bigquery
  name: add_int
  table:
    dataset: *dataset
    name: author
  definition:
    function:
      dataset: *dataset
      name: add_int
    argument_mapping: {}
|]
      )
      [yaml|
internal:
- definition:
    definition:
      function:
        dataset: hasura
        name: add_int
      argument_mapping: {}
    name: add_int
    source: bigquery
    comment:
    table:
      dataset: hasura
      name: author
  reason: |
    Inconsistent object: in table "hasura.author": in computed field "add_int": the computed field "add_int" cannot be added to table "hasura.author" for the following reasons:
      • function "hasura.add_int" is not a TABLE_VALUED_FUNCTION
      • function "hasura.add_int" is not defined with 'RETURNS TABLE'. Expecting return table name.
  name: computed_field add_int in table hasura.author in source bigquery
  type: computed_field
path: "$.args"
error: |
  Inconsistent object: in table "hasura.author": in computed field "add_int": the computed field "add_int" cannot be added to table "hasura.author" for the following reasons:
    • function "hasura.add_int" is not a TABLE_VALUED_FUNCTION
    • function "hasura.add_int" is not defined with 'RETURNS TABLE'. Expecting return table name.
code: invalid-configuration
|]

  it "Add computed field with invalid argument name in argument_mapping - exception" $ \testEnv ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postMetadataWithStatus
          400
          testEnv
          [yaml|
type: bigquery_add_computed_field
args:
  source: bigquery
  name: search_articles
  table:
    dataset: *dataset
    name: author
  definition:
    function:
      dataset: *dataset
      name: fetch_articles_returns_table
    argument_mapping:
      invalid_argument: id
|]
      )
      [yaml|
internal:
- definition:
    definition:
      function:
        dataset: hasura
        name: fetch_articles_returns_table
      argument_mapping:
        invalid_argument: id
    name: search_articles
    source: bigquery
    comment:
    table:
      dataset: hasura
      name: author
  reason: 'Inconsistent object: in table "hasura.author": in computed field "search_articles":
    the computed field "search_articles" cannot be added to table "hasura.author"
    because the argument "invalid_argument" is not one of function "hasura.fetch_articles_returns_table"
    input arguments'
  name: computed_field search_articles in table hasura.author in source bigquery
  type: computed_field
path: "$.args"
error: 'Inconsistent object: in table "hasura.author": in computed field "search_articles":
  the computed field "search_articles" cannot be added to table "hasura.author" because
  the argument "invalid_argument" is not one of function "hasura.fetch_articles_returns_table"
  input arguments'
code: invalid-configuration
|]
