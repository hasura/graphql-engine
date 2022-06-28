{-# LANGUAGE QuasiQuotes #-}

-- | Primary key related queries
module Test.PrimaryKeySpec (spec) where

import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Postgres qualified as Postgres
import Harness.Backend.Sqlserver qualified as Sqlserver
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (shouldReturnYaml, yaml)
import Harness.Test.Context qualified as Context
import Harness.Test.Schema (Table (..), table)
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (TestEnvironment)
import Test.Hspec (SpecWith, describe, it)
import Prelude

--------------------------------------------------------------------------------
-- Preamble

spec :: SpecWith TestEnvironment
spec =
  Context.run
    [ Context.Context
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
        }
    ]
    tests

schema :: [Schema.Table]
schema = [author]

author :: Schema.Table
author =
  (table "author")
    { tableColumns =
        [ Schema.column "id" Schema.TInt,
          Schema.column "name" Schema.TStr,
          Schema.column "createdAt" Schema.TUTCTime
        ],
      tablePrimaryKey = ["id"],
      tableData =
        [ [ Schema.VInt 1,
            Schema.VStr "Author 1",
            Schema.parseUTCTimeOrError "2017-09-21 09:39:44"
          ],
          [ Schema.VInt 2,
            Schema.VStr "Author 2",
            Schema.parseUTCTimeOrError "2017-09-21 09:50:44"
          ]
        ]
    }

tests :: Context.Options -> SpecWith TestEnvironment
tests opts = describe "PrimaryKeySpec" $ do
  -- Equivalent python suite: test_select_query_author_pk
  -- https://github.com/hasura/graphql-engine/blob/369d1ab2f119634b0e27e9ed353fa3d08c22d3fb/server/tests-py/test_graphql_queries.py#L301
  it "works with primary key" $ \testEnvironment ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          testEnvironment
          [graphql|
query {
  hasura_author_by_pk(id: 1) {
    id
    name
  }
}|]
      )
      [yaml|
data:
  hasura_author_by_pk:
    id: 1
    name: Author 1
|]
  -- Equivalent python suite: test_select_query_author_pk_null
  -- https://github.com/hasura/graphql-engine/blob/369d1ab2f119634b0e27e9ed353fa3d08c22d3fb/server/tests-py/test_graphql_queries.py#L304
  it "works with non existent primary key" $ \testEnvironment ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          testEnvironment
          [graphql|
query {
  hasura_author_by_pk(id: 4) {
    id
    name
  }
}|]
      )
      [yaml|
data:
  hasura_author_by_pk: null
|]
