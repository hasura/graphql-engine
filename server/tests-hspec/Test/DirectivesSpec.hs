{-# LANGUAGE QuasiQuotes #-}

-- | Test directives.
module Test.DirectivesSpec (spec) where

import Data.Aeson (Value)
import Harness.Backend.Mysql as Mysql
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml
import Harness.Test.Context qualified as Context
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (TestEnvironment)
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
  it "Skip id field conditionally" \testEnvironment ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          testEnvironment
          (query QueryParams {includeId = False, skipId = False})
      )
      [yaml|
data:
  hasura_author:
  - name: Author 1
  - name: Author 2
|]

  it "Skip id field conditionally, includeId=true" \testEnvironment ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          testEnvironment
          (query QueryParams {includeId = True, skipId = False})
      )
      [yaml|
data:
  hasura_author:
  - id: 1
    name: Author 1
  - id: 2
    name: Author 2
|]

  it "Skip id field conditionally, skipId=true" \testEnvironment ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          testEnvironment
          (query QueryParams {includeId = False, skipId = True})
      )
      [yaml|
data:
  hasura_author:
  - name: Author 1
  - name: Author 2
|]

  it "Skip id field conditionally, skipId=true, includeId=true" \testEnvironment ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          testEnvironment
          (query QueryParams {includeId = True, skipId = True})
      )
      [yaml|
data:
  hasura_author:
  - name: Author 1
  - name: Author 2
|]

  -- These two come from <https://github.com/hasura/graphql-engine-mono/blob/ec3568c704c4c3f13ecff757c547f0d5a272307b/server/tests-py/queries/graphql_query/mysql/select_query_author_with_skip_directive.yaml#L1>

  it "Author with skip id" \testEnvironment ->
    shouldReturnYaml
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
      [yaml|
data:
  hasura_author:
  - name: Author 1
  - name: Author 2
|]
  it "Author with skip name" \testEnvironment ->
    shouldReturnYaml
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
      [yaml|
data:
  hasura_author:
  - id: 1
  - id: 2
|]

  -- These three come from <https://github.com/hasura/graphql-engine-mono/blob/5f6f862e5f6b67d82cfa59568edfc4f08b920375/server/tests-py/queries/graphql_query/mysql/select_query_author_with_wrong_directive_err.yaml#L1>
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
