{-# LANGUAGE QuasiQuotes #-}

-- | All tests related to metadata API for computed fields
--
-- https://hasura.io/docs/latest/api-reference/metadata-api/computed-field/
module Test.API.Metadata.ComputedFieldsSpec (spec) where

import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Harness.Backend.BigQuery qualified as BigQuery
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml (interpolateYaml, yaml)
import Harness.Schema (SchemaName (..), Table (..), table)
import Harness.Schema qualified as Schema
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.SetupAction qualified as SetupAction
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment)
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
                  <> setupFunctions testEnv
            }
        ]
    )
    tests

-- ** Schema

schema :: [Table]
schema = [authorTable, articleTable]

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

-- ** Setup and teardown

setupFunctions :: TestEnvironment -> [Fixture.SetupAction]
setupFunctions testEnv =
  let schemaName = Schema.getSchemaName testEnv
      articleTableSQL = unSchemaName schemaName <> ".article"
   in [ SetupAction.noTeardown
          $ BigQuery.run_
          $ T.unpack
          $ T.unwords
          $ [ "CREATE TABLE FUNCTION ",
              fetch_articles_returns_table schemaName,
              "(a_id INT64, search STRING)",
              "RETURNS TABLE<id INT64, title STRING, content STRING>",
              "AS (",
              "SELECT t.id, t.title, t.content FROM",
              articleTableSQL,
              "AS t WHERE t.author_id = a_id and (t.title LIKE `search` OR t.content LIKE `search`)",
              ");"
            ],
        SetupAction.noTeardown
          $ BigQuery.run_
          $ T.unpack
          $ T.unwords
          $ [ "CREATE TABLE FUNCTION ",
              fetch_articles schemaName,
              "(a_id INT64, search STRING)",
              "AS (",
              "SELECT t.* FROM",
              articleTableSQL,
              "AS t WHERE t.author_id = a_id and (t.title LIKE `search` OR t.content LIKE `search`)",
              ");"
            ],
        SetupAction.noTeardown
          $ BigQuery.run_
          $ T.unpack
          $ T.unwords
          $ [ "CREATE TABLE FUNCTION ",
              function_no_args schemaName <> "()",
              "AS (",
              "SELECT t.* FROM",
              articleTableSQL,
              "AS t);"
            ],
        SetupAction.noTeardown
          $ BigQuery.run_
          $ T.unpack
          $ T.unwords
          $ [ "CREATE FUNCTION ",
              add_int schemaName <> "(a INT64, b INT64)",
              "RETURNS INT64 AS (a + b);"
            ]
      ]

fetch_articles_returns_table :: SchemaName -> T.Text
fetch_articles_returns_table schemaName =
  unSchemaName schemaName <> ".fetch_articles_returns_table"

fetch_articles :: SchemaName -> T.Text
fetch_articles schemaName =
  unSchemaName schemaName <> ".fetch_articles"

function_no_args :: SchemaName -> T.Text
function_no_args schemaName =
  unSchemaName schemaName <> ".function_no_args"

add_int :: SchemaName -> T.Text
add_int schemaName =
  unSchemaName schemaName <> ".add_int"

-- * Tests

tests :: SpecWith TestEnvironment
tests = do
  it "Add computed field with non exist function - exception" $ \testEnv -> do
    let schemaName = Schema.getSchemaName testEnv

    shouldReturnYaml
      testEnv
      ( GraphqlEngine.postMetadataWithStatus
          400
          testEnv
          [yaml|
type: bigquery_add_computed_field
args:
  source: bigquery
  name: search_articles_1
  table:
    dataset: *schemaName
    name: author
  definition:
    function:
      dataset: *schemaName
      name: non_exist_function_name
    argument_mapping:
      a_id: id
|]
      )
      [interpolateYaml|
internal:
- definition:
    definition:
      function:
        dataset: #{schemaName}
        name: non_exist_function_name
      argument_mapping:
        a_id: id
    name: search_articles_1
    source: bigquery
    comment:
    table:
      dataset: #{schemaName}
      name: author
  reason: 'Inconsistent object: in table "#{schemaName}.author": in computed field "search_articles_1":
    no such function exists: "#{schemaName}.non_exist_function_name"'
  name: computed_field search_articles_1 in table #{schemaName}.author in source bigquery
  type: computed_field
path: "$.args"
error: 'Inconsistent object: in table "#{schemaName}.author": in computed field "search_articles_1":
  no such function exists: "#{schemaName}.non_exist_function_name"'
code: invalid-configuration
|]

  it "Add computed field without returning table - exception" $ \testEnv -> do
    let schemaName = Schema.getSchemaName testEnv

    -- The function 'fetch_articles' is not defined with 'RETURNS TABLE<>' clause,
    -- we need to provide `return_table` in the payload
    shouldReturnYaml
      testEnv
      ( GraphqlEngine.postMetadataWithStatus
          400
          testEnv
          [yaml|
            type: bigquery_add_computed_field
            args:
              source: bigquery
              name: search_articles
              table:
                dataset: *schemaName
                name: author
              definition:
                function:
                  dataset: *schemaName
                  name: fetch_articles
                argument_mapping:
                  a_id: id
            #    return_table:
            #      name: article
            #      dataset: *schemaName
            |]
      )
      [interpolateYaml|
internal:
- definition:
    definition:
      function:
        dataset: #{schemaName}
        name: fetch_articles
      argument_mapping:
        a_id: id
    name: search_articles
    source: bigquery
    comment:
    table:
      dataset: #{schemaName}
      name: author
  reason: 'Inconsistent object: in table "#{schemaName}.author": in computed field "search_articles":
    the computed field "search_articles" cannot be added to table "#{schemaName}.author"
    because function "#{schemaName}.fetch_articles" is not defined with ''RETURNS TABLE''.
    Expecting return table name.'
  name: computed_field search_articles in table #{schemaName}.author in source bigquery
  type: computed_field
path: "$.args"
error: 'Inconsistent object: in table "#{schemaName}.author": in computed field "search_articles":
  the computed field "search_articles" cannot be added to table "#{schemaName}.author" because
  function "#{schemaName}.fetch_articles" is not defined with ''RETURNS TABLE''. Expecting
  return table name.'
code: invalid-configuration
|]

  it "Add computed field with non exist returning table - exception" $ \testEnv -> do
    let schemaName = Schema.getSchemaName testEnv

    -- The function 'fetch_articles' is not defined with 'RETURNS TABLE<>' clause,
    -- we need to provide `return_table` in the payload
    shouldReturnYaml
      testEnv
      ( GraphqlEngine.postMetadataWithStatus
          400
          testEnv
          [yaml|
type: bigquery_add_computed_field
args:
  source: bigquery
  name: search_articles
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
      name: non_exist_table
      dataset: *schemaName
|]
      )
      [interpolateYaml|
internal:
- definition:
    definition:
      function:
        dataset: #{schemaName}
        name: fetch_articles
      return_table:
        dataset: #{schemaName}
        name: non_exist_table
      argument_mapping:
        a_id: id
    name: search_articles
    source: bigquery
    comment:
    table:
      dataset: #{schemaName}
      name: author
  reason: 'Inconsistent object: in table "#{schemaName}.author": in computed field "search_articles":
    the computed field "search_articles" cannot be added to table "#{schemaName}.author"
    because function "#{schemaName}.fetch_articles" returning set of table "#{schemaName}.non_exist_table"
    is not tracked'
  name: computed_field search_articles in table #{schemaName}.author in source bigquery
  type: computed_field
path: "$.args"
error: 'Inconsistent object: in table "#{schemaName}.author": in computed field "search_articles":
  the computed field "search_articles" cannot be added to table "#{schemaName}.author" because
  function "#{schemaName}.fetch_articles" returning set of table "#{schemaName}.non_exist_table"
  is not tracked'
code: invalid-configuration
|]

  it "Add computed field with returning table when it is not required - exception" $ \testEnv -> do
    let schemaName = Schema.getSchemaName testEnv

    -- The function 'fetch_articles_returns_table' is defined with 'RETURNS TABLE<>' clause,
    -- we don't need to provide 'return_table' in the payload as the returning fields are inferred
    -- from the function definition
    shouldReturnYaml
      testEnv
      ( GraphqlEngine.postMetadataWithStatus
          400
          testEnv
          [yaml|
type: bigquery_add_computed_field
args:
  source: bigquery
  name: search_articles
  table:
    dataset: *schemaName
    name: author
  definition:
    function:
      dataset: *schemaName
      name: fetch_articles_returns_table
    argument_mapping:
      a_id: id
    return_table:
      name: article
      dataset: *schemaName
|]
      )
      [interpolateYaml|
internal:
- definition:
    definition:
      function:
        dataset: #{schemaName}
        name: fetch_articles_returns_table
      return_table:
        dataset: #{schemaName}
        name: article
      argument_mapping:
        a_id: id
    name: search_articles
    source: bigquery
    comment:
    table:
      dataset: #{schemaName}
      name: author
  reason: 'Inconsistent object: in table "#{schemaName}.author": in computed field "search_articles":
    the computed field "search_articles" cannot be added to table "#{schemaName}.author"
    because return table "#{schemaName}.article" is not required as the function "#{schemaName}.fetch_articles_returns_table"
    returns arbitrary column fields'
  name: computed_field search_articles in table #{schemaName}.author in source bigquery
  type: computed_field
path: "$.args"
error: 'Inconsistent object: in table "#{schemaName}.author": in computed field "search_articles":
  the computed field "search_articles" cannot be added to table "#{schemaName}.author" because
  return table "#{schemaName}.article" is not required as the function "#{schemaName}.fetch_articles_returns_table"
  returns arbitrary column fields'
code: invalid-configuration
|]

  it "Add computed field with a function that has no input arguments - exception" $ \testEnv -> do
    let schemaName = Schema.getSchemaName testEnv

    -- The function 'function_no_args' has no input arguments
    shouldReturnYaml
      testEnv
      ( GraphqlEngine.postMetadataWithStatus
          400
          testEnv
          [yaml|
type: bigquery_add_computed_field
args:
  source: bigquery
  name: search_articles
  table:
    dataset: *schemaName
    name: author
  definition:
    function:
      dataset: *schemaName
      name: function_no_args
    argument_mapping:
      a_id: id
    return_table:
      name: article
      dataset: *schemaName
|]
      )
      [interpolateYaml|
internal:
- definition:
    definition:
      function:
        dataset: #{schemaName}
        name: function_no_args
      return_table:
        dataset: #{schemaName}
        name: article
      argument_mapping:
        a_id: id
    name: search_articles
    source: bigquery
    comment:
    table:
      dataset: #{schemaName}
      name: author
  reason: 'Inconsistent object: in table "#{schemaName}.author": in computed field "search_articles":
    the computed field "search_articles" cannot be added to table "#{schemaName}.author"
    because function "#{schemaName}.function_no_args" has no input arguments defined'
  name: computed_field search_articles in table #{schemaName}.author in source bigquery
  type: computed_field
path: "$.args"
error: 'Inconsistent object: in table "#{schemaName}.author": in computed field "search_articles":
  the computed field "search_articles" cannot be added to table "#{schemaName}.author" because
  function "#{schemaName}.function_no_args" has no input arguments defined'
code: invalid-configuration
|]

  it "Add computed field with a function that returns a scalar value - exception" $ \testEnv -> do
    let schemaName = Schema.getSchemaName testEnv

    -- The function 'add_int' returns a scalar value of type 'INT64', as of now we do not support
    -- scalar computed fields.
    shouldReturnYaml
      testEnv
      ( GraphqlEngine.postMetadataWithStatus
          400
          testEnv
          [yaml|
type: bigquery_add_computed_field
args:
  source: bigquery
  name: add_int
  table:
    dataset: *schemaName
    name: author
  definition:
    function:
      dataset: *schemaName
      name: add_int
    argument_mapping: {}
|]
      )
      [interpolateYaml|
internal:
- definition:
    definition:
      function:
        dataset: #{schemaName}
        name: add_int
      argument_mapping: {}
    name: add_int
    source: bigquery
    comment:
    table:
      dataset: #{schemaName}
      name: author
  reason: |
    Inconsistent object: in table "#{schemaName}.author": in computed field "add_int": the computed field "add_int" cannot be added to table "#{schemaName}.author" for the following reasons:
      • function "#{schemaName}.add_int" is not a TABLE_VALUED_FUNCTION
      • function "#{schemaName}.add_int" is not defined with 'RETURNS TABLE'. Expecting return table name.
  name: computed_field add_int in table #{schemaName}.author in source bigquery
  type: computed_field
path: "$.args"
error: |
  Inconsistent object: in table "#{schemaName}.author": in computed field "add_int": the computed field "add_int" cannot be added to table "#{schemaName}.author" for the following reasons:
    • function "#{schemaName}.add_int" is not a TABLE_VALUED_FUNCTION
    • function "#{schemaName}.add_int" is not defined with 'RETURNS TABLE'. Expecting return table name.
code: invalid-configuration
|]

  it "Add computed field with invalid argument name in argument_mapping - exception" $ \testEnv -> do
    let schemaName = Schema.getSchemaName testEnv

    shouldReturnYaml
      testEnv
      ( GraphqlEngine.postMetadataWithStatus
          400
          testEnv
          [yaml|
type: bigquery_add_computed_field
args:
  source: bigquery
  name: search_articles
  table:
    dataset: *schemaName
    name: author
  definition:
    function:
      dataset: *schemaName
      name: fetch_articles_returns_table
    argument_mapping:
      invalid_argument: id
|]
      )
      [interpolateYaml|
internal:
- definition:
    definition:
      function:
        dataset: #{schemaName}
        name: fetch_articles_returns_table
      argument_mapping:
        invalid_argument: id
    name: search_articles
    source: bigquery
    comment:
    table:
      dataset: #{schemaName}
      name: author
  reason: 'Inconsistent object: in table "#{schemaName}.author": in computed field "search_articles":
    the computed field "search_articles" cannot be added to table "#{schemaName}.author"
    because the argument "invalid_argument" is not one of function "#{schemaName}.fetch_articles_returns_table"
    input arguments'
  name: computed_field search_articles in table #{schemaName}.author in source bigquery
  type: computed_field
path: "$.args"
error: 'Inconsistent object: in table "#{schemaName}.author": in computed field "search_articles":
  the computed field "search_articles" cannot be added to table "#{schemaName}.author" because
  the argument "invalid_argument" is not one of function "#{schemaName}.fetch_articles_returns_table"
  input arguments'
code: invalid-configuration
|]
