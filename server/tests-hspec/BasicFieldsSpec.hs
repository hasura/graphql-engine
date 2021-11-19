{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

-- | Test querying an entity for a couple fields.
module BasicFieldsSpec (spec) where

import Harness.Constants
import Harness.Feature qualified as Feature
import Harness.Graphql
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Mysql as Mysql
import Harness.Postgres as Postgres
import Harness.Sql
import Harness.Yaml
import Test.Hspec
import Prelude

--------------------------------------------------------------------------------
-- Preamble

spec :: Spec
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
              }
          ],
        Feature.tests = tests
      }

--------------------------------------------------------------------------------
-- MySQL backend

mysqlSetup :: IO ()
mysqlSetup = do
  -- Clear and reconfigure the metadata
  GraphqlEngine.post_
    "/v1/metadata"
    [yaml|
type: replace_metadata
args:
  version: 3
  sources:
  - name: mysql
    kind: mysql
    tables: []
    configuration:
      database: *mysqlDatabase
      user: *mysqlUser
      password: *mysqlPassword
      host: *mysqlHost
      port: *mysqlPort
      pool_settings: {}
|]

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
    "/v1/metadata"
    [yaml|
type: mysql_track_table
args:
  source: mysql
  table:
    schema: hasura
    name: author
|]

mysqlTeardown :: IO ()
mysqlTeardown = do
  Mysql.run_
    [sql|
DROP TABLE hasura.author;
|]

--------------------------------------------------------------------------------
-- PostgreSQL backend

postgresSetup :: IO ()
postgresSetup = do
  -- Clear and reconfigure the metadata
  GraphqlEngine.post_
    "/v1/metadata"
    [yaml|
type: replace_metadata
args:
  version: 3
  sources:
  - name: postgres
    kind: postgres
    tables: []
    configuration:
      connection_info:
        database_url: *postgresqlConnectionString
        pool_settings: {}
|]

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
    "/v1/metadata"
    [yaml|
type: postgres_track_table
args:
  source: postgres
  table:
    schema: hasura
    name: author
|]

postgresTeardown :: IO ()
postgresTeardown = do
  Postgres.run_
    [sql|
DROP TABLE hasura.author;
|]

--------------------------------------------------------------------------------
-- Tests

tests :: Spec
tests = do
  it "Author fields" $
    shouldReturnYaml
      ( GraphqlEngine.postGraphql
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
  it "Use operationName" $
    shouldReturnYaml
      ( GraphqlEngine.postGraphqlYaml
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
  it "Missing field" $ do
    shouldReturnYaml
      ( GraphqlEngine.postGraphql
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
  it "Missing table" $ do
    shouldReturnYaml
      ( GraphqlEngine.postGraphql
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
