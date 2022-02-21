{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

-- | Tests for limit/offset.
module Test.LimitOffsetSpec (spec) where

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
    ( 'Author 2'),
    ( 'Author 3'),
    ( 'Author 4');
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

tests :: Context.Options -> SpecWith State
tests opts = do
  it "limit 1" $ \state ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          state
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
  it "Basic offset query" $ \state ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          state
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
  it "order descending, offset 2, limit 1" $ \state ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          state
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
