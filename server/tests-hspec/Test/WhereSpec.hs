{-# LANGUAGE QuasiQuotes #-}

-- | Tests that `where' works.
module Test.WhereSpec (spec) where

import Harness.Backend.Mysql as Mysql
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql
import Harness.Quoter.Yaml
import Harness.State (State)
import Harness.Test.Context qualified as Context
import Harness.Test.Schema qualified as Schema
import Test.Hspec
import Prelude

--------------------------------------------------------------------------------
-- Preamble

spec :: SpecWith State
spec =
  Context.run
    [ Context.Context
        { name = Context.Backend Context.MySQL,
          mkLocalState = Context.noLocalState,
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

tests :: Context.Options -> SpecWith State
tests opts = do
  it "Where id=1" \state ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          state
          [graphql|
query {
  hasura_author(where: {id: {_eq: 1}}) {
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
