{-# LANGUAGE QuasiQuotes #-}

-- | Access to the SQL
module Test.API.Metadata.NativeAccessSpec (spec) where

import Data.List.NonEmpty qualified as NE
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml (yaml)
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.Schema (Table (..), table)
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

-- ** Preamble

-- We currently don't need the table to exist in order to set up a custom SQL
-- stanza.

featureFlagForNativeQuery :: String
featureFlagForNativeQuery = "HASURA_FF_NATIVE_QUERY_INTERFACE"

spec :: SpecWith GlobalTestEnvironment
spec =
  Fixture.hgeWithEnv [(featureFlagForNativeQuery, "True")] $
    Fixture.run
      ( NE.fromList
          [ (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
              { Fixture.setupTeardown = \(testEnv, _) ->
                  [ Postgres.setupTablesAction schema testEnv
                  ]
              }
          ]
      )
      tests

-- ** Setup and teardown

-- we add and track a table here as it's the only way we can currently define a
-- return type
schema :: [Schema.Table]
schema =
  [ (table "already_tracked_return_type")
      { tableColumns =
          [ Schema.column "divided" Schema.TInt
          ]
      }
  ]

tests :: Fixture.Options -> SpecWith TestEnvironment
tests opts = do
  let query :: Text
      query = "SELECT thing / {{denominator}} AS divided FROM stuff WHERE date = {{target_date}}"

  describe "Permissions" $ do
    it "Fails to track custom SQL without admin access" $ \testEnv -> do
      let schemaName = Schema.getSchemaName testEnv

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
                    type: "\"int\""
                  - name: target_date
                    type: "\"date\""
                returns:
                  name: already_tracked_return_type
                  schema: *schemaName
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

  describe "Implementation" $ do
    it "Adds a native access function and returns a 200" $ \testEnv -> do
      let schemaName = Schema.getSchemaName testEnv

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
                    type: "\"int\""
                  - name: target_date
                    type: "\"date\""
                returns:
                  name: already_tracked_return_type
                  schema: *schemaName
            |]
        )
        [yaml|
          message: success
        |]

    it "Checks for the native access function" $ \testEnv -> do
      let schemaName = Schema.getSchemaName testEnv

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
                    type: "\"int\""
                  - name: target_date
                    type: "\"date\""
                returns:
                  name: already_tracked_return_type
                  schema: *schemaName
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
                type: "\"int\""
              - name: target_date
                type: "\"date\""
            returns:
              name: already_tracked_return_type
              schema: hasura

        |]

    it "Drops a native access function and returns a 200" $ \testEnv -> do
      let schemaName = Schema.getSchemaName testEnv

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
                  type: "\"int\""
                - name: target_date
                  type: "\"date\""
              returns:
                name: already_tracked_return_type
                schema: *schemaName
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
      let schemaName = Schema.getSchemaName testEnv

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
                  type: "\"int\""
                - name: target_date
                  type: "\"date\""
              returns:
                name: already_tracked_return_type
                schema: *schemaName
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
