-- | Test querying an entity for a couple fields.
module Test.BasicFieldsSpec (spec) where

import Harness.Backend.BigQuery qualified as BigQuery
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Mysql qualified as Mysql
import Harness.Backend.Postgres qualified as Postgres
import Harness.Backend.Sqlserver qualified as Sqlserver
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Sql (sql)
import Harness.Quoter.Yaml (shouldReturnYaml, yaml)
import Harness.State (State)
import Harness.Test.Context qualified as Context
import Test.Hspec (SpecWith, it)
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
        },
      Context.Context
        { name = Context.Postgres,
          mkLocalState = Context.noLocalState,
          setup = postgresSetup,
          teardown = postgresTeardown,
          customOptions = Nothing
        },
      Context.Context
        { name = Context.Citus,
          mkLocalState = Context.noLocalState,
          setup = citusSetup,
          teardown = citusTeardown,
          customOptions = Nothing
        },
      Context.Context
        { name = Context.SQLServer,
          mkLocalState = Context.noLocalState,
          setup = sqlserverSetup,
          teardown = sqlserverTeardown,
          customOptions = Nothing
        },
      Context.Context
        { name = Context.BigQuery,
          mkLocalState = Context.noLocalState,
          setup = bigquerySetup,
          teardown = bigqueryTeardown,
          customOptions =
            Just $
              Context.Options
                { stringifyNumbers = True
                }
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
DROP TABLE hasura.author;
|]

--------------------------------------------------------------------------------
-- PostgreSQL backend

postgresSetup :: (State, ()) -> IO ()
postgresSetup (state, _) = do
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
  GraphqlEngine.postMetadata_
    state
    [yaml|
type: postgres_track_table
args:
  source: postgres
  table:
    schema: hasura
    name: author
|]

postgresTeardown :: (State, ()) -> IO ()
postgresTeardown _ = do
  Postgres.run_
    [sql|
DROP TABLE hasura.author;
|]

--------------------------------------------------------------------------------
-- Citus backend

citusSetup :: (State, ()) -> IO ()
citusSetup (state, ()) = do
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
  GraphqlEngine.postMetadata_
    state
    [yaml|
type: citus_track_table
args:
  source: citus
  table:
    schema: hasura
    name: author
|]

citusTeardown :: (State, ()) -> IO ()
citusTeardown _ = do
  Citus.run_
    [sql|
DROP TABLE IF EXISTS hasura.author;
|]

--------------------------------------------------------------------------------
-- SQL Server backend

sqlserverSetup :: (State, ()) -> IO ()
sqlserverSetup (state, ()) = do
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
  GraphqlEngine.postMetadata_
    state
    [yaml|
type: mssql_track_table
args:
  source: mssql
  table:
    schema: hasura
    name: author
|]

sqlserverTeardown :: (State, ()) -> IO ()
sqlserverTeardown _ = do
  Sqlserver.run_
    [sql|
DROP TABLE hasura.author;
|]

--------------------------------------------------------------------------------
-- BigQuery backend

bigquerySetup :: (State, ()) -> IO ()
bigquerySetup (state, ()) = do
  -- Clear and reconfigure the metadata
  serviceAccount <- BigQuery.getServiceAccount
  projectId <- BigQuery.getProjectId
  GraphqlEngine.postMetadata_
    state
    [yaml|
type: replace_metadata
args:
  version: 3
  sources:
  - name: bigquery
    kind: bigquery
    tables: []
    configuration:
      service_account: *serviceAccount
      project_id: *projectId
      datasets: [hasura]
|]

  -- Setup tables
  BigQuery.run_
    serviceAccount
    projectId
    [sql|
CREATE TABLE hasura.author
(
    id INT64,
    name STRING
);
|]
  BigQuery.run_
    serviceAccount
    projectId
    [sql|
INSERT INTO hasura.author
    (id, name)
VALUES
    (1, 'Author 1'),
    (2, 'Author 2');
|]

  -- Track the tables
  GraphqlEngine.postMetadata_
    state
    [yaml|
type: bigquery_track_table
args:
  source: bigquery
  table:
    dataset: hasura
    name: author
|]

bigqueryTeardown :: (State, ()) -> IO ()
bigqueryTeardown _ = do
  serviceAccount <- BigQuery.getServiceAccount
  projectId <- BigQuery.getProjectId
  BigQuery.run_
    serviceAccount
    projectId
    [sql|
DROP TABLE hasura.author;
|]

--------------------------------------------------------------------------------
-- Tests

tests :: Context.Options -> SpecWith State
tests opts = do
  it "Author fields" $ \state ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          state
          [graphql|
query {
  hasura_author(order_by:[{id:asc}]) {
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
      opts
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
    hasura_author(order_by:[{id:asc}]) {
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
      opts
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
      opts
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
