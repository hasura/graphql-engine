{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

-- | Test directives.
module DirectivesSpec (spec) where

import Harness.Constants
import Harness.Feature qualified as Feature
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Mysql as Mysql
import Harness.Sql
import Harness.State (State)
import Harness.Yaml
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
  GraphqlEngine.post_
    state
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
DROP TABLE author;
|]

--------------------------------------------------------------------------------
-- Tests

tests :: SpecWith State
tests = do
  it "Skip id field conditionally" \state ->
    shouldReturnYaml
      ( GraphqlEngine.postGraphqlYaml
          state
          [yaml|
query: |
  query author_with_both($includeId: Boolean!, $skipId: Boolean!) {
    hasura_author {
      id @include(if: $includeId) @skip(if: $skipId)
      name
    }
  }
variables:
  includeId: false
  skipId: false
|]
      )
      [yaml|
data:
  hasura_author:
  - name: Author 1
  - name: Author 2
|]

  it "Skip id field conditionally, includeId=true" \state ->
    shouldReturnYaml
      ( GraphqlEngine.postGraphqlYaml
          state
          [yaml|
query: |
  query author_with_both($includeId: Boolean!, $skipId: Boolean!) {
    hasura_author {
      id @include(if: $includeId) @skip(if: $skipId)
      name
    }
  }
variables:
  includeId: true
  skipId: false
|]
      )
      [yaml|
data:
  hasura_author:
  - id: 1
    name: Author 1
  - id: 2
    name: Author 2
|]

  it "Skip id field conditionally, skipId=true" \state ->
    shouldReturnYaml
      ( GraphqlEngine.postGraphqlYaml
          state
          [yaml|
query: |
  query author_with_both($includeId: Boolean!, $skipId: Boolean!) {
    hasura_author {
      id @include(if: $includeId) @skip(if: $skipId)
      name
    }
  }
variables:
  includeId: false
  skipId: true
|]
      )
      [yaml|
data:
  hasura_author:
  - name: Author 1
  - name: Author 2
|]

  it "Skip id field conditionally, skipId=true, includeId=true" \state ->
    shouldReturnYaml
      ( GraphqlEngine.postGraphqlYaml
          state
          [yaml|
query: |
  query author_with_both($includeId: Boolean!, $skipId: Boolean!) {
    hasura_author {
      id @include(if: $includeId) @skip(if: $skipId)
      name
    }
  }
variables:
  includeId: true
  skipId: true
|]
      )
      [yaml|
data:
  hasura_author:
  - name: Author 1
  - name: Author 2
|]
