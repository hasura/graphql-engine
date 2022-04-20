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
import Harness.Test.Context qualified as Context
import Harness.TestEnvironment (TestEnvironment)
import Test.Hspec
import Prelude

--------------------------------------------------------------------------------
-- Preamble

spec :: SpecWith TestEnvironment
spec =
  Context.run
    [ Context.Context
        { name = Context.Backend Context.Postgres,
          mkLocalTestEnvironment = Context.noLocalTestEnvironment,
          setup = pgSetup,
          teardown = pgTeardown,
          customOptions = Nothing
        },
      Context.Context
        { name = Context.Backend Context.Citus,
          mkLocalTestEnvironment = Context.noLocalTestEnvironment,
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

pgSetup :: (TestEnvironment, ()) -> IO ()
pgSetup (testEnvironment, ()) = do
  -- Clear and reconfigure the metadata
  GraphqlEngine.setSource testEnvironment Postgres.defaultSourceMetadata

  -- Setup schema
  Postgres.run_ commonSetupSQL

  -- Track tables
  GraphqlEngine.postMetadata_
    testEnvironment
    [yaml|
type: pg_track_table
args:
  source: postgres
  table:
    schema: hasura
    name: User
|]

pgTeardown :: (TestEnvironment, ()) -> IO ()
pgTeardown (testEnvironment, ()) = do
  -- Untrack tables
  GraphqlEngine.postMetadata_
    testEnvironment
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
  GraphqlEngine.clearMetadata testEnvironment

--------------------------------------------------------------------------------

-- * Citus backend

citusSetup :: (TestEnvironment, ()) -> IO ()
citusSetup (testEnvironment, ()) = do
  -- Clear and reconfigure the metadata
  GraphqlEngine.setSource testEnvironment Citus.defaultSourceMetadata

  -- Setup schema
  Citus.run_ commonSetupSQL

  -- Track tables
  GraphqlEngine.postMetadata_
    testEnvironment
    [yaml|
type: citus_track_table
args:
  source: citus
  table:
    schema: hasura
    name: User
|]

citusTeardown :: (TestEnvironment, ()) -> IO ()
citusTeardown (testEnvironment, ()) = do
  -- Untrack tables
  GraphqlEngine.postMetadata_
    testEnvironment
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
  GraphqlEngine.clearMetadata testEnvironment

--------------------------------------------------------------------------------

-- * Tests

tests :: Context.Options -> SpecWith TestEnvironment
tests opts = do
  it "Insert into enum column with valid enum values" $ \testEnvironment ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          testEnvironment
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

  it "Insert into enum column with invalid enum values" $ \testEnvironment ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          testEnvironment
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
