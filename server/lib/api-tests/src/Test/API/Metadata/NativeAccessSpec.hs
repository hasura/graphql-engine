{-# LANGUAGE QuasiQuotes #-}

-- | Access to the SQL
module Test.API.Metadata.NativeAccessSpec (spec) where

import Data.List.NonEmpty qualified as NE
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml (yaml)
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

-- ** Preamble

--
-- We currently don't need the table to exist in order to set up a custom SQL
-- stanza.

spec :: SpecWith GlobalTestEnvironment
spec =
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Postgres.setupTablesAction [] testEnv
                ],
              Fixture.customOptions =
                Just $
                  Fixture.defaultOptions
                    { Fixture.skipTests =
                        Just "Disabled until we can dynamically switch on Native Access with a command line option in NDAT-452"
                    }
            }
        ]
    )
    tests

-- ** Setup and teardown

tests :: Fixture.Options -> SpecWith TestEnvironment
tests opts = do
  let query :: Text
      query = "SELECT thing / {{denominator}} AS divided FROM stuff WHERE date = {{target_date}}"

  describe "Permissions" $ do
    it "Fails to track custom SQL without admin access" $ \testEnv -> do
      shouldReturnYaml
        opts
        ( GraphqlEngine.postMetadataWithStatusAndHeaders
            400
            testEnv
            [ ("X-Hasura-Role", "not-admin")
            ]
            [yaml|
              type: pg_track_custom_sql
              args:
                type: query
                source: postgres
                root_field_name: divided_stuff
                sql: *query
                parameters:
                  - name: denominator
                    type: int
                  - name: target_date
                    type: date
                returns: already_tracked_return_type
            |]
        )
        [yaml|
          code: access-denied
          error: "restricted access : admin only"
          path: "$.args"
        |]

    it "Fails to untrack custom SQL without admin access" $ \testEnv -> do
      shouldReturnYaml
        opts
        ( GraphqlEngine.postMetadataWithStatusAndHeaders
            400
            testEnv
            [ ("X-Hasura-Role", "not-admin")
            ]
            [yaml|
              type: pg_untrack_custom_sql
              args:
                root_field_name: divided_stuff
                source: postgres
            |]
        )
        [yaml|
          code: access-denied
          error: "restricted access : admin only"
          path: "$.args"
        |]

    it "Fails to list custom SQL without admin access" $ \testEnv -> do
      shouldReturnYaml
        opts
        ( GraphqlEngine.postMetadataWithStatusAndHeaders
            400
            testEnv
            [ ("X-Hasura-Role", "not-admin")
            ]
            [yaml|
              type: pg_get_custom_sql
              args:
                source: postgres
            |]
        )
        [yaml|
          code: access-denied
          error: "restricted access : admin only"
          path: "$.args"
        |]

  describe "Implementation " $ do
    it "Adds a native access function and returns a 200" $ \testEnv -> do
      shouldReturnYaml
        opts
        ( GraphqlEngine.postMetadata
            testEnv
            [yaml|
              type: pg_track_custom_sql
              args:
                type: query
                source: postgres
                root_field_name: divided_stuff
                sql: *query
                parameters:
                  - name: denominator
                    type: int
                  - name: target_date
                    type: date
                returns: already_tracked_return_type
            |]
        )
        [yaml|
          message: success
        |]

    it "Checks for the native access function" $ \testEnv -> do
      shouldReturnYaml
        opts
        ( GraphqlEngine.postMetadata
            testEnv
            [yaml|
              type: pg_track_custom_sql
              args:
                type: query
                source: postgres
                root_field_name: divided_stuff
                sql: *query
                parameters:
                  - name: denominator
                    type: int
                  - name: target_date
                    type: date
                returns: already_tracked_return_type
            |]
        )
        [yaml|
            message: success
          |]

      shouldReturnYaml
        opts
        ( GraphqlEngine.postMetadata
            testEnv
            [yaml|
              type: pg_get_custom_sql
              args:
                source: postgres
            |]
        )
        [yaml|
          divided_stuff:
            type: query
            root_field_name: divided_stuff
            sql: *query
            parameters:
              - name: denominator
                type: int
              - name: target_date
                type: date
            returns:
              name: already_tracked_return_type
              schema: public

        |]

    it "Drops a native access function and returns a 200" $ \testEnv -> do
      _ <-
        GraphqlEngine.postMetadata
          testEnv
          [yaml|
            type: pg_track_custom_sql
            args:
              type: query
              source: postgres
              root_field_name: divided_stuff
              sql: *query
              parameters:
                - name: denominator
                  type: int
                - name: target_date
                  type: date
              returns: already_tracked_return_type
          |]

      shouldReturnYaml
        opts
        ( GraphqlEngine.postMetadata
            testEnv
            [yaml|
              type: pg_untrack_custom_sql
              args:
                source: postgres
                root_field_name: divided_stuff
            |]
        )
        [yaml|
          message: success
        |]

    it "Checks the native access function can be deleted" $ \testEnv -> do
      _ <-
        GraphqlEngine.postMetadata
          testEnv
          [yaml|
            type: pg_track_custom_sql
            args:
              type: query
              source: postgres
              root_field_name: divided_stuff
              sql: *query
              parameters:
                - name: denominator
                  type: int
                - name: target_date
                  type: date
              returns: already_tracked_return_type
          |]

      _ <-
        GraphqlEngine.postMetadata
          testEnv
          [yaml|
            type: pg_untrack_custom_sql
            args:
              root_field_name: divided_stuff
              source: postgres
          |]

      shouldReturnYaml
        opts
        ( GraphqlEngine.postMetadata
            testEnv
            [yaml|
              type: pg_get_custom_sql
              args:
                source: postgres
            |]
        )
        [yaml|
          {}
        |]
