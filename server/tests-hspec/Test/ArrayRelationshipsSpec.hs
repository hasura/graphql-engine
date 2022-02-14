{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

-- | Testing array relationships.
module Test.ArrayRelationshipsSpec (spec) where

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

  -- Setup tables
  Mysql.run_
    [sql|
CREATE TABLE article (
    id INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
    title TEXT,
    content TEXT,
    is_published BIT,
    published_on TIMESTAMP,
    author_id INT UNSIGNED,
    co_author_id INT UNSIGNED,
    FOREIGN KEY (author_id) REFERENCES author(id),
    FOREIGN KEY (co_author_id) REFERENCES author(id)
);
|]
  Mysql.run_
    [sql|
INSERT INTO article
    (title, content, author_id, is_published)
VALUES
    ( 'Article 1', 'Sample article content 1', 1, 0 ),
    ( 'Article 2', 'Sample article content 2', 1, 1 ),
    ( 'Article 3', 'Sample article content 3', 2, 1 );
|]

  -- Track the tables
  GraphqlEngine.postMetadata_
    state
    [yaml|
type: bulk
args:
- type: mysql_track_table
  args:
    source: mysql
    table:
      schema: hasura
      name: author
- type: mysql_track_table
  args:
    source: mysql
    table:
      schema: hasura
      name: article
|]

  -- Setup relationships
  GraphqlEngine.postMetadata_
    state
    [yaml|
type: mysql_create_array_relationship
args:
  source: mysql
  table:
    name: author
    schema: hasura
  name: articles
  using:
    foreign_key_constraint_on:
      table:
        name: article
        schema: hasura
      column: author_id
|]

mysqlTeardown :: (State, ()) -> IO ()
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

tests :: Feature.Options -> SpecWith State
tests opts = do
  it "Select an author and one of their articles" $ \state ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          state
          [graphql|
query {
  # we put id=1 restrictions here because we don't assume ordering support
  hasura_author(where: {id: {_eq: 1}}) {
    id
    articles(where: {id: {_eq: 1}}) {
      id
    }
  }
}
|]
      )
      [yaml|
data:
  hasura_author:
  - id: 1
    articles:
      - id: 1
|]
