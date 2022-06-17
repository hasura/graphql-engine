{-# LANGUAGE QuasiQuotes #-}

-- | Test ordering by fields.
module Test.OrderingSpec (spec) where

import Harness.Backend.Mysql as Mysql
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql
import Harness.Quoter.Yaml
import Harness.Test.Context qualified as Context
import Harness.Test.Schema (Table (..), table)
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
  (table "author")
    { tableColumns =
        [ Schema.column "id" Schema.TInt,
          Schema.column "name" Schema.TStr
        ],
      tablePrimaryKey = ["id"],
      tableData =
        [ [Schema.VInt 1, Schema.VStr "Author 1"],
          [Schema.VInt 2, Schema.VStr "Author 2"]
        ]
    }

--------------------------------------------------------------------------------
-- Tests

-- Added equivalents from <https://github.com/hasura/graphql-engine-mono/blob/ee524e94caf6405ce0ae39edfe161dadd223d60f/server/tests-py/queries/graphql_query/mysql/select_query_author_order_by.yaml#L1>
-- That includes order by {text,id} {desc,asc}
--

tests :: Context.Options -> SpecWith TestEnvironment
tests opts = do
  it "Order by id ascending" $ \testEnvironment ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          testEnvironment
          [graphql|
query {
  hasura_author (order_by: {id: asc}) {
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
  - name: Author 2
    id: 2
|]

  it "Order by id descending" $ \testEnvironment ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          testEnvironment
          [graphql|
query {
  hasura_author (order_by: {id: desc}) {
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
  - name: Author 1
    id: 1
|]

  it "Order by name ascending" $ \testEnvironment ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          testEnvironment
          [graphql|
query {
  hasura_author (order_by: {name: asc}) {
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
  - name: Author 2
    id: 2
|]

  it "Order by name descending" $ \testEnvironment ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          testEnvironment
          [graphql|
query {
  hasura_author (order_by: {name: desc}) {
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
  - name: Author 1
    id: 1
|]
