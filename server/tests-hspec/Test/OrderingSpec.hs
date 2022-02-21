{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

-- | Test ordering by fields.
module Test.OrderingSpec (spec) where

import Harness.Backend.Mysql as Mysql
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql
import Harness.Quoter.Sql
import Harness.Quoter.Yaml
import Harness.State (State)
import Harness.Test.Context qualified as Context
import Test.Hspec
import Prelude

--------------------------------------------------------------------------------
-- Preamble

spec :: SpecWith State
spec =
  Context.run
    [ Context.Context
        { name = Context.MySQL,
          mkLocalState = Context.noLocalState,
          setup = mysqlSetup,
          teardown = mysqlTeardown,
          customOptions = Nothing
        }
    ]
    tests

--------------------------------------------------------------------------------
-- MySQL backend

mysqlSetup :: (State, ()) -> IO ()
mysqlSetup (state, _) = do
  -- Clear and reconfigure the metadata
  GraphqlEngine.setSource state Mysql.defaultSourceMetadata

  -- Setup tables
  Mysql.run_
    [sql|
CREATE TABLE author
(
    id INT UNSIGNED NOT NULL AUTO_INCREMENT PRIMARY KEY,
    name VARCHAR(45) UNIQUE KEY
);
|]
  Mysql.run_
    [sql|
INSERT INTO author
    (name)
VALUES
    ( 'Author 1'),
    ( 'Author 2');
|]

  -- Track the tables
  GraphqlEngine.postMetadata_
    state
    [yaml|
type: mysql_track_table
args:
  source: mysql
  table:
    schema: hasura
    name: author
|]

mysqlTeardown :: (State, ()) -> IO ()
mysqlTeardown _ = do
  Mysql.run_
    [sql|
DROP TABLE author;
|]

--------------------------------------------------------------------------------
-- Tests

-- Added equivalents from <https://github.com/hasura/graphql-engine-mono/blob/ee524e94caf6405ce0ae39edfe161dadd223d60f/server/tests-py/queries/graphql_query/mysql/select_query_author_order_by.yaml#L1>
-- That includes order by {text,id} {desc,asc}
--

tests :: Context.Options -> SpecWith State
tests opts = do
  it "Order by id ascending" $ \state ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          state
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

  it "Order by id descending" $ \state ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          state
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

  it "Order by name ascending" $ \state ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          state
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

  it "Order by name descending" $ \state ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          state
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
