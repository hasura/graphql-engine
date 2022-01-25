{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

-- | Test querying an entity for a couple fields.
module Test.BasicFieldsSpec (spec) where

import Harness.Backend.Citus as Citus
import Harness.Backend.Mysql as Mysql
import Harness.Backend.Postgres as Postgres
import Harness.Backend.Sqlserver as Sqlserver
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
              },
            Feature.Backend
              { name = "PostgreSQL",
                setup = postgresSetup,
                teardown = postgresTeardown
              },
            Feature.Backend
              { name = "Citus",
                setup = citusSetup,
                teardown = citusTeardown
              },
            Feature.Backend
              { name = "SQLServer",
                setup = sqlserverSetup,
                teardown = sqlserverTeardown
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
CREATE TABLE hasura.author
(
    id INT UNSIGNED NOT NULL AUTO_INCREMENT PRIMARY KEY,
    name VARCHAR(45) UNIQUE KEY
);
|]
  Mysql.run_
    [sql|
INSERT INTO hasura.author
    (name)
VALUES
    ( 'Author 1'),
    ( 'Author 2');
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

mysqlTeardown :: State -> IO ()
mysqlTeardown _ = do
  Mysql.run_
    [sql|
DROP TABLE hasura.author;
|]

--------------------------------------------------------------------------------
-- PostgreSQL backend

postgresSetup :: State -> IO ()
postgresSetup state = do
  -- Clear and reconfigure the metadata
  GraphqlEngine.setSource state Postgres.defaultSourceMetadata

  -- Setup tables
  Postgres.run_
    [sql|
CREATE TABLE hasura.author
(
    id SERIAL PRIMARY KEY,
    name VARCHAR(45) UNIQUE
);
|]
  Postgres.run_
    [sql|
INSERT INTO hasura.author
    (name)
VALUES
    ( 'Author 1'),
    ( 'Author 2');
|]

  -- Track the tables
  GraphqlEngine.post_
    state
    "/v1/metadata"
    [yaml|
type: postgres_track_table
args:
  source: postgres
  table:
    schema: hasura
    name: author
|]

postgresTeardown :: State -> IO ()
postgresTeardown _ = do
  Postgres.run_
    [sql|
DROP TABLE hasura.author;
|]

--------------------------------------------------------------------------------
-- Citus backend

citusSetup :: State -> IO ()
citusSetup state = do
  -- Clear and reconfigure the metadata
  GraphqlEngine.setSource state Citus.defaultSourceMetadata

  -- Setup tables
  Citus.run_
    [sql|
CREATE TABLE hasura.author
(
    id SERIAL PRIMARY KEY,
    name VARCHAR(45) UNIQUE
);
|]
  Citus.run_
    [sql|
INSERT INTO hasura.author
    (name)
VALUES
    ( 'Author 1'),
    ( 'Author 2');
|]

  -- Track the tables
  GraphqlEngine.post_
    state
    "/v1/metadata"
    [yaml|
type: citus_track_table
args:
  source: citus
  table:
    schema: hasura
    name: author
|]

citusTeardown :: State -> IO ()
citusTeardown _ = do
  Citus.run_
    [sql|
DROP TABLE IF EXISTS hasura.author;
|]

--------------------------------------------------------------------------------
-- SQL Server backend

sqlserverSetup :: State -> IO ()
sqlserverSetup state = do
  -- Clear and reconfigure the metadata
  GraphqlEngine.setSource state Sqlserver.defaultSourceMetadata

  -- Setup tables
  Sqlserver.run_
    [sql|
CREATE TABLE hasura.author
(
    id INT NOT NULL IDENTITY(1,1) PRIMARY KEY CLUSTERED,
    name NVARCHAR(45) NOT NULL UNIQUE NONCLUSTERED
);
|]
  Sqlserver.run_
    [sql|
INSERT INTO hasura.author
    (name)
VALUES
    ('Author 1'),
    ('Author 2');
|]

  -- Track the tables
  GraphqlEngine.post_
    state
    "/v1/metadata"
    [yaml|
type: mssql_track_table
args:
  source: mssql
  table:
    schema: hasura
    name: author
|]

sqlserverTeardown :: State -> IO ()
sqlserverTeardown _ = do
  Sqlserver.run_
    [sql|
DROP TABLE hasura.author;
|]

--------------------------------------------------------------------------------
-- Tests

tests :: SpecWith State
tests = do
  it "Author fields" $ \state ->
    shouldReturnYaml
      ( GraphqlEngine.postGraphql
          state
          [graphql|
query {
  hasura_author {
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
  it "Use operationName" $ \state ->
    shouldReturnYaml
      ( GraphqlEngine.postGraphqlYaml
          state
          [yaml|
operationName: chooseThisOne
query: |
  query ignoreThisOne {
    MyQuery {
      name
    }
  }
  query chooseThisOne {
    hasura_author {
      id
      name
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
  it "Missing field" $ \state -> do
    shouldReturnYaml
      ( GraphqlEngine.postGraphql
          state
          [graphql|
query {
  hasura_author {
    id
    name
    notPresentCol
  }
}
|]
      )
      [yaml|
errors:
- extensions:
    code: validation-failed
    path: $.selectionSet.hasura_author.selectionSet.notPresentCol
  message: |-
    field "notPresentCol" not found in type: 'hasura_author'
|]
  it "Missing table" $ \state ->
    shouldReturnYaml
      ( GraphqlEngine.postGraphql
          state
          [graphql|
query {
  random {
    id
    name
  }
}
|]
      )
      [yaml|
errors:
- extensions:
    code: validation-failed
    path: $.selectionSet.random
  message: |-
    field "random" not found in type: 'query_root'
|]
