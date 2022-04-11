{-# LANGUAGE QuasiQuotes #-}

-- | Testing inserting values into column with enum type
--
-- Test fix for https://github.com/hasura/graphql-engine/issues/4014
module Test.InsertEnumColumnSpec (spec) where

import Harness.Backend.Citus as Citus
import Harness.Backend.Postgres as Postgres
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
        { name = Context.Backend Context.Postgres,
          mkLocalState = Context.noLocalState,
          setup = pgSetup,
          teardown = pgTeardown,
          customOptions = Nothing
        },
      Context.Context
        { name = Context.Backend Context.Citus,
          mkLocalState = Context.noLocalState,
          setup = citusSetup,
          teardown = citusTeardown,
          customOptions = Nothing
        }
    ]
    tests

--------------------------------------------------------------------------------
-- Common SQL

commonSetupSQL :: String
commonSetupSQL =
  [sql|
CREATE TYPE "UserRole" AS ENUM ('Admin', 'Editor', 'Moderator');

CREATE TABLE "User" (
 "Id" SERIAL PRIMARY KEY,
 "Name" TEXT NOT NULL,
 "Role" "UserRole" NOT NULL
);
|]

commonTeardownSQL :: String
commonTeardownSQL =
  [sql|
DROP TABLE "User";
DROP TYPE "UserRole";
|]

--------------------------------------------------------------------------------

-- * Postgres backend

pgSetup :: (State, ()) -> IO ()
pgSetup (state, ()) = do
  -- Clear and reconfigure the metadata
  GraphqlEngine.setSource state Postgres.defaultSourceMetadata

  -- Setup schema
  Postgres.run_ commonSetupSQL

  -- Track tables
  GraphqlEngine.postMetadata_
    state
    [yaml|
type: pg_track_table
args:
  source: postgres
  table:
    schema: hasura
    name: User
|]

pgTeardown :: (State, ()) -> IO ()
pgTeardown (state, ()) = do
  -- Untrack tables
  GraphqlEngine.postMetadata_
    state
    [yaml|
type: pg_untrack_table
args:
  source: postgres
  table:
    schema: hasura
    name: User
|]

  -- Teardown schema
  Postgres.run_ commonTeardownSQL

  -- Clear metadata
  GraphqlEngine.clearMetadata state

--------------------------------------------------------------------------------

-- * Citus backend

citusSetup :: (State, ()) -> IO ()
citusSetup (state, ()) = do
  -- Clear and reconfigure the metadata
  GraphqlEngine.setSource state Citus.defaultSourceMetadata

  -- Setup schema
  Citus.run_ commonSetupSQL

  -- Track tables
  GraphqlEngine.postMetadata_
    state
    [yaml|
type: citus_track_table
args:
  source: citus
  table:
    schema: hasura
    name: User
|]

citusTeardown :: (State, ()) -> IO ()
citusTeardown (state, ()) = do
  -- Untrack tables
  GraphqlEngine.postMetadata_
    state
    [yaml|
type: citus_untrack_table
args:
  source: citus
  table:
    schema: hasura
    name: User
|]

  -- Teardown schema
  Citus.run_ commonTeardownSQL

  -- Clear metadata
  GraphqlEngine.clearMetadata state

--------------------------------------------------------------------------------

-- * Tests

tests :: Context.Options -> SpecWith State
tests opts = do
  it "Insert into enum column with valid enum values" $ \state ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          state
          [graphql|
mutation {
  insert_hasura_User(
    objects: [{Name: "Bob", Role: "Admin"}, {Name: "Rob", Role: "Moderator"}]
  ) {
    affected_rows
    returning{
      Id
      Name
      Role
    }
  }
}
|]
      )
      [yaml|
data:
  insert_hasura_User:
    returning:
    - Role: Admin
      Name: Bob
      Id: 1
    - Role: Moderator
      Name: Rob
      Id: 2
    affected_rows: 2
|]

  it "Insert into enum column with invalid enum values" $ \state ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          state
          [graphql|
mutation {
  insert_hasura_User(
    objects: [{Name: "Bob1", Role: "Admin"}, {Name: "Rob", Role: "InvalidRole"}]
  ) {
    affected_rows
    returning{
      Id
      Name
      Role
    }
  }
}
|]
      )
      [yaml|
errors:
- extensions:
    path: $.selectionSet.insert_hasura_User.args.objects
    code: data-exception
  message: 'invalid input value for enum "UserRole": "InvalidRole"'
|]
