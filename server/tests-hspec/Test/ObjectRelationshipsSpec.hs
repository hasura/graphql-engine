{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

-- | Testing object relationships.
module Test.ObjectRelationshipsSpec (spec) where

import Data.Text (Text)
import Harness.Backend.Mysql qualified as Mysql
import Harness.Backend.Sqlserver qualified as Sqlserver
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
spec = do
  Context.run
    [ Context.Context
        { name = Context.SQLServer,
          mkLocalState = Context.noLocalState,
          setup = mssqlSetup,
          teardown = \_ -> mssqlTeardown,
          customOptions = Nothing
        }
    ]
    mssqlTests

  Context.run
    [ Context.Context
        { name = Context.MySQL,
          mkLocalState = Context.noLocalState,
          setup = mysqlSetup,
          teardown = \_ -> mysqlTeardown,
          customOptions = Nothing
        }
    ]
    mysqlTests

-- Tests

mysqlTests :: Context.Options -> SpecWith State
mysqlTests opts = do
  usingWhereClause opts
  xdescribe
    "Pending: The MySQL backend currently fails with relationship fields that are null.\
    \ (https://github.com/hasura/graphql-engine-mono/issues/3650)"
    (nullField opts)

mssqlTests :: Context.Options -> SpecWith State
mssqlTests opts = do
  usingWhereClause opts
  nullField opts

usingWhereClause :: Context.Options -> SpecWith State
usingWhereClause opts = do
  it "Author of article where id=1" $ \state ->
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

nullField :: Context.Options -> SpecWith State
nullField opts = do
  it "Can realise a null relationship field" $ \state ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          state
          [graphql|
query {
  hasura_article(where: {id: {_eq: 4}}) {
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
  - author: null
    id: 4
|]

--------------------------------------------------------------------------------
-- Unified test setup/teardown definitions

trackTableAuthor :: Text -> State -> IO ()
trackTableAuthor backendSource state = do
  let requestType = backendSource <> "_track_table"
  GraphqlEngine.post_
    state
    "/v1/metadata"
    [yaml|
type: *requestType
args:
  source: *backendSource
  table:
    schema: hasura
    name: author
|]

trackTableArticle :: Text -> State -> IO ()
trackTableArticle backendSource state = do
  let requestType = backendSource <> "_track_table"
  GraphqlEngine.post_
    state
    "/v1/metadata"
    [yaml|
type: *requestType
args:
  source: *backendSource
  table:
    schema: hasura
    name: article
|]

trackRelationshipArticleAuthor :: Text -> State -> IO ()
trackRelationshipArticleAuthor backendSource state = do
  let requestType = backendSource <> "_create_object_relationship"
  GraphqlEngine.post_
    state
    "/v1/metadata"
    [yaml|
type: *requestType
args:
  source: *backendSource
  table:
    name: article
    schema: hasura
  name: author
  using:
    foreign_key_constraint_on: author_id
|]

--------------------------------------------------------------------------------
-- MySQL backend

mysqlSetup :: (State, ()) -> IO ()
mysqlSetup (state, _) = do
  -- Clear and reconfigure the metadata
  GraphqlEngine.setSource state Mysql.defaultSourceMetadata

  mysqlCreateTableAuthor
  mysqlCreateTableArticle
  trackTableAuthor "mysql" state
  trackTableArticle "mysql" state
  trackRelationshipArticleAuthor "mysql" state

mysqlCreateTableAuthor :: IO ()
mysqlCreateTableAuthor = do
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
    ( 'Author 1' ),
    ( 'Author 2' );
|]

mysqlCreateTableArticle :: IO ()
mysqlCreateTableArticle = do
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
    ( 2 ),
    ( null );
|]

mysqlTeardown :: IO ()
mysqlTeardown = do
  Mysql.run_ [sql| DROP TABLE article; |]
  Mysql.run_ [sql| DROP TABLE author; |]

--------------------------------------------------------------------------------
-- MSSQL backend

mssqlSetup :: (State, ()) -> IO ()
mssqlSetup (state, _) = do
  -- Clear and reconfigure the metadata
  GraphqlEngine.setSource state Sqlserver.defaultSourceMetadata

  mssqlCreateTableAuthor
  mssqlCreateTableArticle
  trackTableAuthor "mssql" state
  trackTableArticle "mssql" state
  trackRelationshipArticleAuthor "mssql" state

mssqlCreateTableAuthor :: IO ()
mssqlCreateTableAuthor = do
  Sqlserver.run_
    [sql|
CREATE TABLE author
(
    id INT  IDENTITY PRIMARY KEY,
    name VARCHAR(45) UNIQUE
);
|]
  Sqlserver.run_
    [sql|
INSERT INTO author
    (name)
VALUES
    ( 'Author 1' ),
    ( 'Author 2' );
|]

-- Setup tables
mssqlCreateTableArticle :: IO ()
mssqlCreateTableArticle = do
  Sqlserver.run_
    [sql|
CREATE TABLE article
(
    id INT IDENTITY PRIMARY KEY,
    author_id INT ,
    FOREIGN KEY (author_id) REFERENCES author(id)
);
|]
  Sqlserver.run_
    [sql|
INSERT INTO article
    (author_id)
VALUES
    ( 1 ),
    ( 1 ),
    ( 2 ),
    ( null );
|]

mssqlTeardown :: IO ()
mssqlTeardown = do
  Sqlserver.run_ [sql| DROP TABLE article; |]
  Sqlserver.run_ [sql| DROP TABLE author; |]
