{-# LANGUAGE QuasiQuotes #-}

-- | Tests for limit/offset.
module Test.LimitOffsetSpec (spec) where

import Harness.Backend.Mysql as Mysql
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql
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
      [Schema.VInt 2, Schema.VStr "Author 2"],
      [Schema.VInt 3, Schema.VStr "Author 3"],
      [Schema.VInt 4, Schema.VStr "Author 4"]
    ]

--------------------------------------------------------------------------------
-- Tests

tests :: Context.Options -> SpecWith TestEnvironment
tests opts = do
  it "limit 1" $ \testEnvironment ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          testEnvironment
          [graphql|
query {
  hasura_author(limit: 1) {
    name
    id
  }
}
|]
      )
      [yaml|
data:
  hasura_author:
  - name: Author 1
    id: 1
|]

  -- Originally from <https://github.com/hasura/graphql-engine-mono/blob/719d03bd970def443e25d04210a37556d350d84b/server/tests-py/queries/graphql_query/mysql/select_query_author_offset.yaml>
  --
  -- Technically without an ORDER, the results are UB-ish. Keep an eye
  -- on ordering with tests like this.
  it "Basic offset query" $ \testEnvironment ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          testEnvironment
          [graphql|
query {
  hasura_author(offset: 1) {
    name
    id
  }
}
|]
      )
      [yaml|
data:
  hasura_author:
  - name: Author 2
    id: 2
  - name: Author 3
    id: 3
  - name: Author 4
    id: 4
|]

  -- This is a more precise version of <https://github.com/hasura/graphql-engine-mono/blob/dbf32f15c25c12fba88afced311fd876111cb987/server/tests-py/queries/graphql_query/mysql/select_query_author_limit_offset.yaml#L1>
  --
  -- We use ordering here, which yields a stable result.
  it "order descending, offset 2, limit 1" $ \testEnvironment ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          testEnvironment
          [graphql|
query {
  hasura_author(limit: 1, offset: 2, order_by: {id: desc}) {
    id
    name
  }
}
|]
      )
      [yaml|
data:
  hasura_author:
  - id: 2
    name: Author 2
|]
