{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
-- For runWithLocalTestEnvironmentSingleSetup
{-# OPTIONS_GHC -Wno-deprecations #-}

-- | Test that drop_source metadata API executes successfully when any remote
-- relationships is present between a remote schema and a database.
-- Currently remote relationships are possible between:
--  1. Two Postgres Sources
--  2. (Postgres - Remote Schema), here a PG source has remote relationship with a
--     remote schema
--  3. (Remote Schema - Postgres), here a remote schema has remote relationship with a
--     PG source.
--  4. (Remote Schema - Remote Schema), here a remote schema has a remote relationship
--     with another remote schema
--
-- A Remote relationship has two entities: LHS (left hand side) and RHS (right hand
-- side). Think of them as a mathematical equation: LHS = RHS i.e a LHS entity
-- depends on RHS entity.
-- In terms of remote relationship:
--    A source present on LHS has a remote relationship with the source on RHS. That
--    means, the source on LHS depends on RHS. This is the reason why in the setup of
--    tests - we first setup the RHS and then setup the LHS. And we do the reverse in
--    teardown.
--
-- The RHS source in the below tests have the source name as "target"
-- The LHS source in the below tests have the source name as "source"
--
-- In the below test, we test that dropping the remote schema (RHS, source name: "target")
-- does not create any inconsistent metadata, and the remote relationship is also
-- dropped from the databse (LHS, source name: "source").
module Test.Schema.RemoteRelationships.MetadataAPI.DropSource.RSToDBRelationshipSpec (spec) where

import Control.Lens (findOf, has, only, (^?!))
import Data.Aeson.Lens (key, values, _String)
import Data.List.NonEmpty qualified as NE
import Data.Maybe qualified as Unsafe (fromJust)
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml (yaml)
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (TestEnvironment)
import Harness.Yaml (shouldBeYaml, shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)
import Test.Schema.RemoteRelationships.MetadataAPI.Common (LocalTestTestEnvironment (..), remoteSchemaToDBRemoteRelationshipFixture)

--------------------------------------------------------------------------------
-- Preamble

spec :: SpecWith TestEnvironment
spec = Fixture.runWithLocalTestEnvironmentSingleSetup contexts tests
  where
    contexts = NE.fromList [remoteSchemaToDBRemoteRelationshipFixture]

--------------------------------------------------------------------------------
-- Tests

tests :: Fixture.Options -> SpecWith (TestEnvironment, LocalTestTestEnvironment)
tests opts = describe "drop-source-metadata-tests" do
  dropMetadataTests opts

dropMetadataTests :: Fixture.Options -> SpecWith (TestEnvironment, LocalTestTestEnvironment)
dropMetadataTests opts = describe "drop_source on RHS source should remove remote relationship from LHS" do
  it "drops the RHS source 'target'" \(testEnvironment, _) -> do
    let query =
          [yaml|
            type: pg_drop_source
            args :
              name: target
              cascade: true
          |]

        expectedDropSourceResponse =
          [yaml|
            message: success
          |]
    shouldReturnYaml
      opts
      (GraphqlEngine.postMetadata testEnvironment query)
      expectedDropSourceResponse

  it "export metadata, check if remote relationship is removed from LHS ('source', remote schema)" \(testEnvironment, _) -> do
    -- No remote relationship should be present for table 'track' in 'source' remote schema
    let expectedRSRelationshipMetadata =
          [yaml|
              - relationships: []
                type_name: hasura_track
            |]

    metadata <- GraphqlEngine.exportMetadata testEnvironment

    let remoteSchemas = key "remote_schemas" . values
        -- Extract the 'source' remote schema and check if any remote relationships exists
        sourceRemoteSchema =
          Unsafe.fromJust $
            findOf
              remoteSchemas
              (has $ key "name" . _String . only "source")
              metadata
        sourceRemoteSchemaRemoteRelationships = sourceRemoteSchema ^?! key "remote_relationships"

    shouldBeYaml expectedRSRelationshipMetadata sourceRemoteSchemaRemoteRelationships
