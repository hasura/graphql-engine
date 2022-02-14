{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

-- | Test views.
module Test.ViewsSpec (spec) where

import Harness.Backend.Mysql as Mysql
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql
import Harness.Quoter.Sql
import Harness.Quoter.Yaml
import Harness.State (State)
import Harness.Test.Feature qualified as Feature
import Test.Hspec
import Prelude

--------------------------------------------------------------------------------
-- Preamble

spec :: SpecWith State
spec =
  Feature.run
    [ Feature.Context
        { name = "MySQL",
          setup = mysqlSetup,
          teardown = mysqlTeardown,
          options = Feature.defaultOptions
        }
    ]
    tests

--------------------------------------------------------------------------------
-- MySQL backend

mysqlSetup :: State -> IO ()
mysqlSetup state = do
  -- Clear and reconfigure the metadata
  GraphqlEngine.setSource state Mysql.defaultSourceMetadata

  -- Setup tables
  Mysql.run_
    [sql|
CREATE TABLE author
(
    id INT UNSIGNED NOT NULL AUTO_INCREMENT PRIMARY KEY,
    name VARCHAR(45) UNIQUE KEY,
    createdAt DATETIME
);
|]
  Mysql.run_
    [sql|
INSERT INTO author
    (name, createdAt)
VALUES
    ( 'Author 1', '2017-09-21 09:39:44' ),
    ( 'Author 2', '2017-09-21 09:50:44' );
|]

  -- Setup views
  Mysql.run_
    [sql|
CREATE OR REPLACE VIEW search_author_view AS
  SELECT * FROM author;
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
mysqlTeardown _ = do
  Mysql.run_
    [sql|
DROP VIEW IF EXISTS search_author_view;
|]
  Mysql.run_
    [sql|
DROP TABLE author;
|]

--------------------------------------------------------------------------------
-- Tests

tests :: Feature.Options -> SpecWith State
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
