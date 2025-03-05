{-# LANGUAGE QuasiQuotes #-}

-- | Test remote schema marked as inconsistent when conflicting types are present
module Test.Regression.RemoteSchemaConflictingTypesSpec where

import Data.Aeson qualified as J
import Data.Aeson.KeyMap qualified as J
import Data.List.NonEmpty qualified as NE
import Data.Morpheus.Document (gqlDocument)
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml (yaml)
import Harness.RemoteServer qualified as RemoteServer
import Harness.Test.Fixture (Fixture (..))
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, Server, TestEnvironment (..), stopServer)
import Harness.Yaml (shouldReturnYamlF)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

--------------------------------------------------------------------------
-- Preamble

spec :: SpecWith GlobalTestEnvironment
spec = Fixture.runWithLocalTestEnvironment (NE.fromList [context]) tests
  where
    context =
      (Fixture.fixture $ Fixture.RemoteGraphQLServer)
        { Fixture.mkLocalTestEnvironment = \_testEnvironment ->
            RemoteServer.run
              $ RemoteServer.generateQueryInterpreter
              $ Query {user = getUserResolver},
          setupTeardown = \(testEnvironment, server) ->
            [ Fixture.SetupAction
                { Fixture.setupAction = GraphqlEngine.clearMetadata testEnvironment,
                  Fixture.teardownAction = \_ -> do
                    GraphqlEngine.clearMetadata testEnvironment
                    stopServer server
                },
              Fixture.SetupAction
                { Fixture.setupAction = postgresSetup testEnvironment,
                  Fixture.teardownAction = \_ -> postgresTeardown testEnvironment
                }
            ]
        }

--------------------------------------------------------------------------
-- Tests
tests :: SpecWith (TestEnvironment, Server)
tests = describe "Remote schema conflicting types" $ do
  it "should mark remote schema as inconsistent when conflicting types are present" $ \(testEnvironment, remoteServer) -> do
    let remoteSchemaEndpoint = RemoteServer.graphqlEndpoint remoteServer
        expectedResponse =
          [yaml|
            "Inconsistent object: Found conflicting definitions for GraphQL typeFound conflicting
                   definitions for GraphQL type 'User'.  The definition at user differs from the definitions
                   at [delete_User_by_pk, update_User_by_pk, insert_User_one, insert_User.returning,
                   User_aggregate.nodes, User_by_pk, User].\nFormer has definition:\ntype User { id:
                   Int!\n  name: String!\n}\nLatter has definition:\n\"columns and relationships of
                   \\\"User\\\"\" \ntype User { first_name: String\n  id: Int!\n  last_name: String\n}"
          |]
        addRemoteSchema =
          [yaml|
          type: add_remote_schema
          args:
            name: remote
            definition:
              url: *remoteSchemaEndpoint
          |]
        getError (J.Object o) = onNothing (J.lookup "error" o) (fail "Should return error field")
        getError _ = fail "Should return Object"
    shouldReturnYamlF
      testEnvironment
      getError
      (GraphqlEngine.postMetadataWithStatus 400 testEnvironment addRemoteSchema)
      expectedResponse

--------------------------------------------------------------------------
--- Postgres

postgresSetup :: TestEnvironment -> IO ()
postgresSetup testEnvironment = do
  Postgres.createDatabase testEnvironment
  let sourceName :: Text = "db"
      sourceConfig = Postgres.defaultSourceConfiguration testEnvironment
  GraphqlEngine.postMetadata_
    testEnvironment
    [yaml|
type: pg_add_source
args:
  name: *sourceName
  configuration: *sourceConfig
|]
  -- setup tables only
  GraphqlEngine.postV2Query_
    testEnvironment
    [yaml|
type: pg_run_sql
args:
  source: *sourceName
  sql: |
    CREATE TABLE "User" (
      id serial PRIMARY KEY,
      first_name text,
      last_name text
    );
|]
  GraphqlEngine.postMetadata_
    testEnvironment
    [yaml|
type: pg_track_table
args:
  source: *sourceName
  table: User
|]

postgresTeardown :: TestEnvironment -> IO ()
postgresTeardown testEnvironment = Postgres.dropDatabase testEnvironment

--------------------------------------------------------------------------
--- Remote schema
[gqlDocument|
type Query {
  user: User!
}

type User {
  id: Int!
  name: String!
}
|]

getUserResolver :: (Monad m) => m (User m)
getUserResolver = pure $ User (pure 1) (pure "Alice")
