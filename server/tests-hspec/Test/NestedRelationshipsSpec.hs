{-# LANGUAGE QuasiQuotes #-}

-- | Testing nested relationships.
--
-- Original inspiration for this module Test.is <https://github.com/hasura/graphql-engine-mono/blob/08caf7df10cad0aea0916327736147a0a8f808d1/server/tests-py/queries/graphql_query/mysql/nested_select_query_deep.yaml>
module Test.NestedRelationshipsSpec (spec) where

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
        { name = Context.Backend Context.MySQL,
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
type: bulk
args:
- type: mysql_create_object_relationship
  args:
    source: mysql
    table:
      name: article
      schema: hasura
    name: author
    using:
      foreign_key_constraint_on: author_id
- type: mysql_create_array_relationship
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

tests :: Context.Options -> SpecWith State
tests opts = do
  it "Nested select on article" $ \state ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          state
          [graphql|
query {
  hasura_article(where: {id: {_eq: 1}}) {
    id
    author {
      id
      articles(where: {id: {_eq: 1}}) {
        id
        author {
          id
          articles(where: {id: {_eq: 1}}) {
            id
            author {
              id
            }
          }
        }
      }
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
        articles:
        - id: 1
          author:
            id: 1
            articles:
            - id: 1
              author:
                id: 1
|]
