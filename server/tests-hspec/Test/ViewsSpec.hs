{-# LANGUAGE QuasiQuotes #-}

-- | Test views.
module Test.ViewsSpec (spec) where

import Harness.Backend.Mysql as Mysql
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql
import Harness.Quoter.Sql
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

mysqlSetup :: (TestEnvironment, ()) -> IO ()
mysqlSetup (testEnvironment, _) = do
  Mysql.setup schema (testEnvironment, ())
  -- Setup views
  Mysql.run_
    [sql|
CREATE OR REPLACE VIEW search_author_view AS
  SELECT * FROM author;
|]
  -- Track the views
  GraphqlEngine.postMetadata_
    testEnvironment
    [yaml|
type: mysql_track_table
args:
  source: mysql
  table:
    name: search_author_view
    schema: hasura
|]

mysqlTeardown :: (TestEnvironment, ()) -> IO ()
mysqlTeardown (testEnvironment, _) = do
  Mysql.teardown schema (testEnvironment, ())
  -- unrack the views
  GraphqlEngine.postMetadata_
    testEnvironment
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

tests :: Context.Options -> SpecWith TestEnvironment
tests opts = do
  it "Query that a view works properly" \testEnvironment ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          testEnvironment
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
