{-# LANGUAGE QuasiQuotes #-}

-- | Test directives.
module Test.DirectivesSpec (spec) where

import Data.Aeson (Value (..), object, (.=))
import Harness.Backend.BigQuery qualified as Bigquery
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Mysql as Mysql
import Harness.Backend.Postgres qualified as Postgres
import Harness.Backend.Sqlserver qualified as Sqlserver
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml
import Harness.Test.Context qualified as Context
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (TestEnvironment)
import Harness.Yaml
import Test.Hspec
import Prelude

--------------------------------------------------------------------------------
-- Preamble

spec :: SpecWith TestEnvironment
spec =
  Context.run
    [ Context.Context
        { name = Context.Backend Context.MySQL,
          mkLocalTestEnvironment = Context.noLocalTestEnvironment,
          setup = Mysql.setup schema,
          teardown = Mysql.teardown schema,
          customOptions = Nothing
        },
      Context.Context
        { name = Context.Backend Context.Postgres,
          mkLocalTestEnvironment = Context.noLocalTestEnvironment,
          setup = Postgres.setup schema,
          teardown = Postgres.teardown schema,
          customOptions = Nothing
        },
      Context.Context
        { name = Context.Backend Context.Citus,
          mkLocalTestEnvironment = Context.noLocalTestEnvironment,
          setup = Citus.setup schema,
          teardown = Citus.teardown schema,
          customOptions = Nothing
        },
      Context.Context
        { name = Context.Backend Context.SQLServer,
          mkLocalTestEnvironment = Context.noLocalTestEnvironment,
          setup = Sqlserver.setup schema,
          teardown = Sqlserver.teardown schema,
          customOptions = Nothing
        },
      Context.Context
        { name = Context.Backend Context.BigQuery,
          mkLocalTestEnvironment = Context.noLocalTestEnvironment,
          setup = Bigquery.setup schema,
          teardown = Bigquery.teardown schema,
          customOptions =
            Just $
              Context.Options
                { stringifyNumbers = True
                }
        }
    ]
    tests

--------------------------------------------------------------------------------
-- Schema

schema :: [Schema.Table]
schema = [author]

author :: Schema.Table
author =
  Schema.Table
    "author"
    [ Schema.column "id" Schema.TInt,
      Schema.column "name" Schema.TStr
    ]
    ["id"]
    []
    [ [Schema.VInt 1, Schema.VStr "Author 1"],
      [Schema.VInt 2, Schema.VStr "Author 2"]
    ]

--------------------------------------------------------------------------------
-- Tests

data QueryParams = QueryParams
  { includeId :: Bool,
    skipId :: Bool
  }

query :: QueryParams -> Value
query QueryParams {includeId, skipId} =
  [graphql|
  query author_with_both {
    hasura_author {
      id @include(if: #{includeId}) @skip(if: #{skipId})
      name
    }
  }
|]

tests :: Context.Options -> SpecWith TestEnvironment
tests opts = do
  -- Equivalent python suite: test_select_query_author_with_skip_include_directive
  -- https://github.com/hasura/graphql-engine/blob/369d1ab2f119634b0e27e9ed353fa3d08c22d3fb/server/tests-py/test_graphql_queries.py#L268
  it "Skip id field conditionally" \testEnvironment ->
    shouldReturnOneOfYaml
      opts
      ( GraphqlEngine.postGraphql
          testEnvironment
          (query QueryParams {includeId = False, skipId = False})
      )
      (combinationsObjectUsingValue authorResponse authorNames)
  -- Equivalent python suite: test_select_query_author_with_skip_include_directive
  -- https://github.com/hasura/graphql-engine/blob/369d1ab2f119634b0e27e9ed353fa3d08c22d3fb/server/tests-py/test_graphql_queries.py#L268
  it "Skip id field conditionally, includeId=true" \testEnvironment ->
    shouldReturnOneOfYaml
      opts
      ( GraphqlEngine.postGraphql
          testEnvironment
          (query QueryParams {includeId = True, skipId = False})
      )
      (combinationsObjectUsingValue authorResponse authorNameIds)
  -- Equivalent python suite: test_select_query_author_with_skip_include_directive
  -- https://github.com/hasura/graphql-engine/blob/369d1ab2f119634b0e27e9ed353fa3d08c22d3fb/server/tests-py/test_graphql_queries.py#L268
  it "Skip id field conditionally, skipId=true" \testEnvironment ->
    shouldReturnOneOfYaml
      opts
      ( GraphqlEngine.postGraphql
          testEnvironment
          (query QueryParams {includeId = False, skipId = True})
      )
      (combinationsObjectUsingValue authorResponse authorNames)
  -- Equivalent python suite: test_select_query_author_with_skip_include_directive
  -- https://github.com/hasura/graphql-engine/blob/369d1ab2f119634b0e27e9ed353fa3d08c22d3fb/server/tests-py/test_graphql_queries.py#L268
  it "Skip id field conditionally, skipId=true, includeId=true" \testEnvironment ->
    shouldReturnOneOfYaml
      opts
      ( GraphqlEngine.postGraphql
          testEnvironment
          (query QueryParams {includeId = True, skipId = True})
      )
      (combinationsObjectUsingValue authorResponse authorNames)
  -- Equivalent python suite: test_select_query_author_with_skip_directive
  -- https://github.com/hasura/graphql-engine/blob/369d1ab2f119634b0e27e9ed353fa3d08c22d3fb/server/tests-py/test_graphql_queries.py#L262
  it "Author with skip id" \testEnvironment ->
    shouldReturnOneOfYaml
      opts
      ( GraphqlEngine.postGraphqlYaml
          testEnvironment
          [yaml|
query: |
  query author_with_skip($skipId: Boolean!, $skipName: Boolean!) {
    hasura_author {
      id @skip(if: $skipId)
      name @skip(if: $skipName)
    }
  }
variables:
  skipId: true
  skipName: false
|]
      )
      (combinationsObjectUsingValue authorResponse authorNames)

  -- Equivalent python suite: test_select_query_author_with_skip_directive
  -- https://github.com/hasura/graphql-engine/blob/369d1ab2f119634b0e27e9ed353fa3d08c22d3fb/server/tests-py/test_graphql_queries.py#L262
  it "Author with skip name" \testEnvironment ->
    shouldReturnOneOfYaml
      opts
      ( GraphqlEngine.postGraphqlYaml
          testEnvironment
          [yaml|
query: |
  query author_with_skip($skipId: Boolean!, $skipName: Boolean!) {
    hasura_author {
      id @skip(if: $skipId)
      name @skip(if: $skipName)
    }
  }
variables:
  skipId: false
  skipName: true
|]
      )
      (combinationsObjectUsingValue authorResponse authorIds)
  -- Equivalent python suite: test_select_query_author_with_wrong_directive_err
  -- https://github.com/hasura/graphql-engine/blob/369d1ab2f119634b0e27e9ed353fa3d08c22d3fb/server/tests-py/test_graphql_queries.py#L271
  it "Rejects unknown directives" \testEnvironment ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphqlYaml
          testEnvironment
          [yaml|
    query: |
      query {
        hasura_author {
          id @exclude(if: true)
          name
        }
      }
|]
      )
      [yaml|
errors:
- extensions:
    path: $.selectionSet.hasura_author.selectionSet
    code: validation-failed
  message: directive "exclude" is not defined in the schema
|]
  it "Rejects duplicate directives" \testEnvironment ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphqlYaml
          testEnvironment
          [yaml|
    query: |
      query {
        hasura_author {
          id @include(if: true) @include(if: true)
          name
        }
      }
|]
      )
      [yaml|
errors:
- extensions:
    path: $.selectionSet.hasura_author.selectionSet
    code: validation-failed
  message: 'the following directives are used more than once: include'
|]
  it "Rejects directives on wrong element" \testEnvironment ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphqlYaml
          testEnvironment
          [yaml|
    query: |
      query @include(if: true) {
        hasura_author {
          id
          name
        }
      }
|]
      )
      [yaml|
errors:
- extensions:
    path: $
    code: validation-failed
  message: directive "include" is not allowed on a query
|]
  -- Equivalent python suite: test_select_query_author_with_include_directive
  -- https://github.com/hasura/graphql-engine/blob/369d1ab2f119634b0e27e9ed353fa3d08c22d3fb/server/tests-py/test_graphql_queries.py#L265
  it "works with includeId as True includeName as False" \testEnvironment ->
    shouldReturnOneOfYaml
      opts
      ( GraphqlEngine.postGraphqlWithPair
          testEnvironment
          [graphql|
  query author_with_include($includeId: Boolean!, $includeName: Boolean!) {
    hasura_author {
      id @include(if: $includeId)
      name @include(if: $includeName)
     }
    }
|]
          ["variables" .= object ["includeId" .= True, "includeName" .= False]]
      )
      (combinationsObjectUsingValue authorResponse authorIds)
  it "works with includeId as False includeName as True" \testEnvironment ->
    shouldReturnOneOfYaml
      opts
      ( GraphqlEngine.postGraphqlWithPair
          testEnvironment
          [graphql|
  query author_with_include($includeId: Boolean!, $includeName: Boolean!) {
    hasura_author {
      id @include(if: $includeId)
      name @include(if: $includeName)
     }
    }
|]
          ["variables" .= object ["includeId" .= False, "includeName" .= True]]
      )
      (combinationsObjectUsingValue authorResponse authorNames)

authorNames :: [Value]
authorNames =
  [ object
      [ "name"
          .= String "Author 1"
      ],
    object
      [ "name"
          .= String "Author 2"
      ]
  ]

authorIds :: [Value]
authorIds =
  [ object
      [ "id"
          .= (1 :: Int)
      ],
    object
      [ "id"
          .= (2 :: Int)
      ]
  ]

authorNameIds :: [Value]
authorNameIds =
  [ object
      [ "name"
          .= String "Author 1",
        "id" .= (1 :: Int)
      ],
    object
      [ "name"
          .= String "Author 2",
        "id" .= (2 :: Int)
      ]
  ]

authorResponse :: Value -> Value
authorResponse authors =
  [yaml|
data:
  hasura_author: *authors
|]
