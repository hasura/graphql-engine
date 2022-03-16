{-# LANGUAGE QuasiQuotes #-}

-- | Test views.
module Test.ViewsSpec (spec) where

import Harness.Backend.Mysql as Mysql
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql
import Harness.Quoter.Sql
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
          setup = mysqlSetup,
          teardown = mysqlTeardown,
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
      Schema.column "name" Schema.TStr,
      Schema.column "createdAt" Schema.TUTCTime
    ]
    ["id"]
    []
    [ [Schema.VInt 1, Schema.VStr "Author 1", Schema.parseUTCTimeOrError "2017-09-21 09:39:44"],
      [Schema.VInt 2, Schema.VStr "Author 2", Schema.parseUTCTimeOrError "2017-09-21 09:50:44"]
    ]

--------------------------------------------------------------------------------
-- Setup and Teardown

mysqlSetup :: (State, ()) -> IO ()
mysqlSetup (state, _) = do
  Mysql.setup schema (state, ())
  -- Setup views
  Mysql.run_
    [sql|
CREATE OR REPLACE VIEW search_author_view AS
  SELECT * FROM author;
|]
  -- Track the views
  GraphqlEngine.postMetadata_
    state
    [yaml|
type: mysql_track_table
args:
  source: mysql
  table:
    name: search_author_view
    schema: hasura
|]

mysqlTeardown :: (State, ()) -> IO ()
mysqlTeardown (state, _) = do
  Mysql.teardown schema (state, ())
  -- unrack the views
  GraphqlEngine.postMetadata_
    state
    [yaml|
type: mysql_untrack_table
args:
  source: mysql
  table:
    name: search_author_view
    schema: hasura
|]
  Mysql.run_
    [sql|
DROP VIEW IF EXISTS search_author_view;
|]

--------------------------------------------------------------------------------
-- Tests

tests :: Context.Options -> SpecWith State
tests opts = do
  it "Query that a view works properly" \state ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          state
          [graphql|
query {
  hasura_search_author_view(where: {id: {_eq: 1}}) {
    id
    name
    createdAt
  }
}
|]
      )
      [yaml|
data:
  hasura_search_author_view:
  - id: 1
    name: Author 1
    createdAt: "2017-09-21 09:39:44"
|]
