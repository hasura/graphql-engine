{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Test that clear_metadata API  executes successfully when any remote
-- relationships are present between two entities. Currently remote relationships are
-- possible between:
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
module Test.Schema.RemoteRelationships.MetadataAPI.ClearMetadataSpec (spec) where

import Data.List.NonEmpty qualified as NE
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml (yaml)
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment)
import Harness.Yaml (shouldReturnYaml)
import Test.Hspec (SpecWith, describe, it)
import Test.Schema.RemoteRelationships.MetadataAPI.Common qualified as Common

--------------------------------------------------------------------------------
-- Preamble

spec :: SpecWith GlobalTestEnvironment
spec = Fixture.runWithLocalTestEnvironment contexts tests
  where
    contexts =
      NE.fromList
        [ Common.dbTodbRemoteRelationshipFixture,
          Common.dbToRemoteSchemaRemoteRelationshipFixture,
          Common.remoteSchemaToDBRemoteRelationshipFixture,
          Common.remoteSchemaToremoteSchemaRemoteRelationshipFixture
        ]

--------------------------------------------------------------------------------
-- Tests

tests :: SpecWith (TestEnvironment, Common.LocalTestTestEnvironment)
tests = describe "clear-metadata-metadata-tests" do
  describe "clear_metadata" do
    it "clears the metadata" \(testEnvironment, _) -> do
      let query =
            [yaml|
              type: clear_metadata
              args : {}
            |]

          expectedResponse =
            [yaml|
              message: success
            |]
      shouldReturnYaml
        testEnvironment
        (GraphqlEngine.postMetadata testEnvironment query)
        expectedResponse
