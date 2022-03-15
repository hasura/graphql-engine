-- | Testing column presets.
module Test.ColumnPresetsSpec (spec) where

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

-- * Preamble

spec :: SpecWith State
spec =
  Context.run
    [ Context.Context
        { name = Context.SQLServer,
          mkLocalState = Context.noLocalState,
          setup = sqlserverSetup,
          teardown = sqlserverTeardown,
          customOptions = Nothing
        },
      Context.Context
        { name = Context.Postgres,
          mkLocalState = Context.noLocalState,
          setup = postgresSetup,
          teardown = postgresTeardown,
          customOptions = Nothing
        }
    ]
    tests

--------------------------------------------------------------------------------

-- * Tests

tests :: Context.Options -> SpecWith State
tests opts = do
  ----------------------------------------
  it "override column presets as admin" $ \state ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          state
          [graphql|
mutation author {
  insert_hasura_author(objects:
    { name: "Author 1"
      uuid: "36a6257b-1111-1111-2222-c1b7a7997087"
      company: "arusah"
    }) {
    returning {
      uuid
      name
      company
    }
  }
}
|]
      )
      [yaml|
data:
  insert_hasura_author:
    returning:
    - uuid: '36a6257b-1111-1111-2222-c1b7a7997087'
      name: 'Author 1'
      company: 'arusah'
|]

  ----------------------------------------
  it "insert automatic uuid and default company" $ \state ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphqlWithHeaders
          state
          [ ("X-Hasura-Role", "user"),
            ("X-Hasura-User-Id", "36a6257b-1111-1111-1111-c1b7a7997087")
          ]
          [graphql|
mutation author {
  insert_hasura_author(objects: { name: "Author 2" }) {
    returning {
      uuid
      name
      company
    }
  }
}
|]
      )
      [yaml|
data:
  insert_hasura_author:
    returning:
    - uuid: '36a6257b-1111-1111-1111-c1b7a7997087'
      name: 'Author 2'
      company: 'hasura'
|]

  ----------------------------------------
  it "Session variable column preset is not overridable by user" $ \state ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphqlWithHeaders
          state
          [ ("X-Hasura-Role", "user"),
            ("X-Hasura-User-Id", "36a6257b-1111-1111-1111-c1b7a7997087")
          ]
          [graphql|
mutation author {
  insert_hasura_author(objects:
    { name: "Author 3"
      uuid: "36a6257b-1111-1111-2222-c1b7a7997087"
    }) {
    returning {
      uuid
      name
      company
    }
  }
}
|]
      )
      [yaml|
errors:
- extensions:
    path: $.selectionSet.insert_hasura_author.args.objects[0].uuid
    code: validation-failed
  message: 'field "uuid" not found in type: ''hasura_author_insert_input'''
|]

  ----------------------------------------
  it "Static column preset is not overridable by user" $ \state ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphqlWithHeaders
          state
          [ ("X-Hasura-Role", "user"),
            ("X-Hasura-User-Id", "36a6257b-1111-1111-1111-c1b7a7997087")
          ]
          [graphql|
mutation author {
  insert_hasura_author(objects: { name: "Author 4" company: "arusah" }) {
    returning {
      uuid
      name
      company
    }
  }
}
|]
      )
      [yaml|
errors:
- extensions:
    path: $.selectionSet.insert_hasura_author.args.objects[0].company
    code: validation-failed
  message: 'field "company" not found in type: ''hasura_author_insert_input'''
|]

--------------------------------------------------------------------------------

-- * Postgrse backend

-- ** Setup

postgresSetup :: (State, ()) -> IO ()
postgresSetup (state, _) = do
  -- Clear and reconfigure the metadata
  GraphqlEngine.setSource state Postgres.defaultSourceMetadata
  -- Setup
  postgresSetupTables
  postgresInsertValues
  postgresTrackTables state
  postgresCreateRelationships state
  postgresCreatePermissions state

postgresSetupTables :: IO ()
postgresSetupTables = do
  -- Setup tables
  Postgres.run_
    [sql|
CREATE TABLE hasura.author
(
    uuid VARCHAR(50) NOT NULL PRIMARY KEY,
    name TEXT NOT NULL,
    company TEXT NOT NULL
);
|]

postgresInsertValues :: IO ()
postgresInsertValues = do
  pure ()

postgresTrackTables :: State -> IO ()
postgresTrackTables state = do
  -- Track the tables
  GraphqlEngine.postMetadata_
    state
    [yaml|
type: pg_track_table
args:
  source: postgres
  table:
    schema: hasura
    name: author
|]

postgresCreateRelationships :: State -> IO ()
postgresCreateRelationships _ = do
  pure ()

postgresCreatePermissions :: State -> IO ()
postgresCreatePermissions state = do
  GraphqlEngine.postMetadata_
    state
    [yaml|
type: pg_create_select_permission
args:
  source: postgres
  table:
    schema: hasura
    name: author
  role: user
  permission:
    filter:
      uuid: X-Hasura-User-Id
    columns: '*'
|]
  GraphqlEngine.postMetadata_
    state
    [yaml|
type: pg_create_insert_permission
args:
  source: postgres
  table:
    schema: hasura
    name: author
  role: user
  permission:
    check: {}
    set:
      uuid: X-Hasura-User-Id
      company: hasura
    columns: '*'
|]

-- ** Teardown

postgresTeardown :: (State, ()) -> IO ()
postgresTeardown _ = do
  Postgres.run_
    [sql|
DROP TABLE hasura.author;
|]

--------------------------------------------------------------------------------

-- * SQL Server backend

-- ** Setup

sqlserverSetup :: (State, ()) -> IO ()
sqlserverSetup (state, _) = do
  -- Clear and reconfigure the metadata
  GraphqlEngine.setSource state Sqlserver.defaultSourceMetadata
  -- Setup
  sqlserverSetupTables
  sqlserverInsertValues
  sqlserverTrackTables state
  sqlserverCreateRelationships state
  sqlserverCreatePermissions state

sqlserverSetupTables :: IO ()
sqlserverSetupTables = do
  -- Setup tables
  Sqlserver.run_
    [sql|
CREATE TABLE hasura.author
(
    uuid VARCHAR(50) NOT NULL PRIMARY KEY,
    name NVARCHAR(50) NOT NULL,
    company NVARCHAR(50) NOT NULL
);
|]

sqlserverInsertValues :: IO ()
sqlserverInsertValues = do
  pure ()

sqlserverTrackTables :: State -> IO ()
sqlserverTrackTables state = do
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

sqlserverCreateRelationships :: State -> IO ()
sqlserverCreateRelationships _ = do
  pure ()

sqlserverCreatePermissions :: State -> IO ()
sqlserverCreatePermissions state = do
  GraphqlEngine.postMetadata_
    state
    [yaml|
type: mssql_create_select_permission
args:
  source: mssql
  table:
    schema: hasura
    name: author
  role: user
  permission:
    filter:
      uuid: X-Hasura-User-Id
    columns: '*'
|]
  GraphqlEngine.postMetadata_
    state
    [yaml|
type: mssql_create_insert_permission
args:
  source: mssql
  table:
    schema: hasura
    name: author
  role: user
  permission:
    check: {}
    set:
      uuid: X-Hasura-User-Id
      company: hasura
    columns: '*'
|]

-- ** Teardown

sqlserverTeardown :: (State, ()) -> IO ()
sqlserverTeardown _ = do
  Sqlserver.run_
    [sql|
DROP TABLE hasura.author;
|]
