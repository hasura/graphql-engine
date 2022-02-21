-- | Tests related to request headers
module Test.RequestHeadersSpec (spec) where

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
        }
    ]
    tests

--------------------------------------------------------------------------------

-- * Tests

tests :: Context.Options -> SpecWith State
tests opts = do
  -- See https://github.com/hasura/graphql-engine/issues/8158
  it "session variable string values are not truncated to default (30) length" $ \state ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphqlWithHeaders
          state
          [ ("X-Hasura-Role", "user"),
            ("X-Hasura-User-Id", "36a6257b-08bb-45ef-a5cf-c1b7a7997087")
          ]
          [graphql|
query {
  hasura_author {
    name
    uuid
  }
}
|]
      )
      [yaml|
data:
  hasura_author:
  - name: 'Author 1'
    uuid: '36a6257b-08bb-45ef-a5cf-c1b7a7997087'
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
    name NVARCHAR(50) NOT NULL
);
|]

sqlserverInsertValues :: IO ()
sqlserverInsertValues = do
  Sqlserver.run_
    [sql|
INSERT INTO hasura.author
    (uuid, name)
VALUES
    ('36a6257b-08bb-45ef-a5cf-c1b7a7997087', 'Author 1'),
    ('36a6257b-08bb-45ef-a5cf-c1b7a7', 'Author 2');
|]

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

-- ** Teardown

sqlserverTeardown :: (State, ()) -> IO ()
sqlserverTeardown _ = do
  Sqlserver.run_
    [sql|
DROP TABLE hasura.author;
|]
