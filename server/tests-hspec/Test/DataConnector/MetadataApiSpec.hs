{-# LANGUAGE QuasiQuotes #-}

-- | Metadata API tests for Data Connector Backend
module Test.DataConnector.MetadataApiSpec
  ( spec,
  )
where

--------------------------------------------------------------------------------

import Data.List.NonEmpty qualified as NE
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml (yaml)
import Harness.Test.Fixture (emptySetupAction)
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (TestEnvironment)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

--------------------------------------------------------------------------------
-- Reference Agent Query Tests

spec :: SpecWith TestEnvironment
spec =
  Fixture.runWithLocalTestEnvironment
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Fixture.DataConnector)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [emptySetupAction testEnv]
            }
        ]
    )
    tests

--------------------------------------------------------------------------------

tests :: Fixture.Options -> SpecWith (TestEnvironment, a)
tests opts = describe "Metadata API: A series of actions to setup and teardown a source with tracked tables and relationships" $ do
  describe "dc_add_agent" $ do
    it "Success" $ \(testEnvironment, _) -> do
      shouldReturnYaml
        opts
        ( GraphqlEngine.postMetadata
            testEnvironment
            [yaml|
            type: dc_add_agent
            args:
              name: reference
              url: http://localhost:65005
          |]
        )
        [yaml|
          message: success
        |]

  describe "list_source_kinds" $ do
    it "success" $ \(testEnvironment, _) -> do
      shouldReturnYaml
        opts
        ( GraphqlEngine.postMetadata
            testEnvironment
            [yaml|
            type: list_source_kinds
            args: {}
          |]
        )
        [yaml|
          sources:
          - builtin: true
            kind: pg
          - builtin: true
            kind: citus
          - builtin: true
            kind: cockroach
          - builtin: true
            kind: mssql
          - builtin: true
            kind: bigquery
          - builtin: true
            kind: mysql
          - builtin: false
            kind: reference
        |]

  describe "<kind>_add_source" $ do
    it "success" $ \(testEnvironment, _) -> do
      shouldReturnYaml
        opts
        ( GraphqlEngine.postMetadata
            testEnvironment
            [yaml|
            type: reference_add_source
            args:
              name: chinook
              configuration: 
                value: {}
          |]
        )
        [yaml|
          message: success
        |]

  describe "<kind>_track_table" $ do
    it "success" $ \(testEnvironment, _) -> do
      shouldReturnYaml
        opts
        ( GraphqlEngine.postMetadata
            testEnvironment
            [yaml|
            type: reference_track_table
            args:
              source: chinook
              table: Album 
          |]
        )
        [yaml|
          message: success
        |]

  describe "<kind>_create_object_relationship" $ do
    it "success" $ \(testEnvironment, _) -> do
      GraphqlEngine.postMetadata_
        testEnvironment
        [yaml|
        type: reference_track_table
        args:
          source: chinook
          table: Artist 
      |]

      shouldReturnYaml
        opts
        ( GraphqlEngine.postMetadata
            testEnvironment
            [yaml|
            type: reference_create_object_relationship
            args:
              source: chinook
              table: Album
              name: Artist
              using:
                foreign_key_constraint_on:
                  - ArtistId
          |]
        )
        [yaml|
          message: success
        |]

  describe "<kind>_create_array_relationship" $ do
    it "success" $ \(testEnvironment, _) -> do
      shouldReturnYaml
        opts
        ( GraphqlEngine.postMetadata
            testEnvironment
            [yaml|
            type: reference_create_array_relationship
            args:
              source: chinook
              table: Artist
              name: Albums
              using:
                foreign_key_constraint_on:
                  table: Album
                  columns:
                    - ArtistId
          |]
        )
        [yaml|
          message: success
        |]

  describe "export_metadata" $ do
    it "produces the expected metadata structure" $ \(testEnvironment, _) -> do
      shouldReturnYaml
        opts
        ( GraphqlEngine.postMetadata
            testEnvironment
            [yaml|
            type: export_metadata
            args: {}
          |]
        )
        [yaml|
          backend_configs:
            dataconnector:
              reference:
                uri: http://localhost:65005
          sources:
          - configuration:
              template: null
              timeout: null
              value: {}
            kind: reference
            name: chinook
            tables:
            - object_relationships:
              - name: Artist
                using:
                  foreign_key_constraint_on: ArtistId
              table:
              - Album
            - array_relationships:
              - name: Albums
                using:
                  foreign_key_constraint_on:
                    column: ArtistId
                    table:
                    - Album
              table:
              - Artist
          version: 3
        |]

  describe "<kind>_drop_relationship" $ do
    it "success" $ \(testEnvironment, _) -> do
      shouldReturnYaml
        opts
        ( GraphqlEngine.postMetadata
            testEnvironment
            [yaml|
            type: reference_drop_relationship
            args:
              source: chinook
              table: Artist
              relationship: Albums
          |]
        )
        [yaml|
          message: success
        |]

  describe "<kind>_untrack_table" $ do
    it "success" $ \(testEnvironment, _) -> do
      shouldReturnYaml
        opts
        ( GraphqlEngine.postMetadata
            testEnvironment
            [yaml|
            type: reference_untrack_table
            args:
              source: chinook
              table: Artist
              cascade: true
          |]
        )
        [yaml|
          message: success
        |]

  describe "<kind>_drop_source" $ do
    it "success" $ \(testEnvironment, _) -> do
      shouldReturnYaml
        opts
        ( GraphqlEngine.postMetadata
            testEnvironment
            [yaml|
            type: reference_drop_source
            args:
              name: chinook
              cascade: true
          |]
        )
        [yaml|
          message: success
        |]

  describe "dc_delete_agent" $ do
    it "success" $ \(testEnvironment, _) -> do
      shouldReturnYaml
        opts
        ( GraphqlEngine.postMetadata
            testEnvironment
            [yaml|
            type: dc_delete_agent
            args:
              name: reference
          |]
        )
        [yaml|
          message: success
        |]
