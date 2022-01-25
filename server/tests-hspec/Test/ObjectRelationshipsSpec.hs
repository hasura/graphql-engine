{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

-- | Testing object relationships.
module Test.ObjectRelationshipsSpec (spec) where

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
  Feature.feature
    Feature.Feature
      { Feature.backends =
          [ Feature.Backend
              { name = "MySQL",
                setup = mysqlSetup,
                teardown = mysqlTeardown
              }
          ],
        Feature.tests = tests
      }

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

  -- Setup tables
  Mysql.run_
    [sql|
CREATE TABLE article (
    id INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
    author_id INT UNSIGNED,
    FOREIGN KEY (author_id) REFERENCES author(id)
);
|]
  Mysql.run_
    [sql|
INSERT INTO article
    (author_id)
VALUES
    ( 1 ),
    ( 1 ),
    ( 2 );
|]

  -- Track the tables
  GraphqlEngine.post_
    state
    "/v1/metadata"
    [yaml|
type: mysql_track_table
args:
  source: mysql
  table:
    schema: hasura
    name: author
|]
  GraphqlEngine.post_
    state
    "/v1/metadata"
    [yaml|
type: mysql_track_table
args:
  source: mysql
  table:
    schema: hasura
    name: article
|]

  -- Setup relationships
  GraphqlEngine.post_
    state
    "/v1/metadata"
    [yaml|
type: mysql_create_object_relationship
args:
  source: mysql
  table:
    name: article
    schema: hasura
  name: author
  using:
    foreign_key_constraint_on: author_id
|]

mysqlTeardown :: State -> IO ()
mysqlTeardown _ = do
  Mysql.run_
    [sql|
DROP TABLE article;
|]
  Mysql.run_
    [sql|
DROP TABLE author;
|]

--------------------------------------------------------------------------------
-- Tests

tests :: SpecWith State
tests = do
  it "Author of article where id=1" $ \state ->
    shouldReturnYaml
      ( GraphqlEngine.postGraphql
          state
          [graphql|
query {
  hasura_article(where: {id: {_eq: 1}}) {
    id
    author {
      id
    }
  }
}
|]
      )
      [yaml|
data:
  hasura_article:
  - id: 1
    author:
      id: 1
|]

  -- originally from <https://github.com/hasura/graphql-engine-mono/blob/cf64da26e818ca0e4ec39667296c67021bc03c2a/server/tests-py/queries/graphql_query/mysql/select_query_author_quoted_col.yaml>
  it "Simple GraphQL object query on author" $ \state ->
    shouldReturnYaml
      ( GraphqlEngine.postGraphql
          state
          [graphql|
query {
 hasura_author {
    createdAt
    name
    id
  }
}
|]
      )
      -- I believe that the order is not necessarily guaranteed, but
      -- the Python test was written like this. We can't use
      -- limit/order here without requiring that new backends
      -- implement those too. Approach: hope that ordering is never
      -- violated, and if it is, rework this test.
      [yaml|
data:
  hasura_author:
  - createdAt: '2017-09-21 09:39:44'
    name: Author 1
    id: 1
  - createdAt: '2017-09-21 09:50:44'
    name: Author 2
    id: 2
|]
