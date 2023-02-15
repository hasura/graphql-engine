{-# LANGUAGE QuasiQuotes #-}

-- | Tests of the Native Queryies feature.
module Test.API.Metadata.NativeQuerySpec (spec) where

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

-- We currently don't need the table to exist in order to set up a Native Query
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
      },
    (table "stuff")
      { tableColumns =
          [ Schema.column "thing" Schema.TInt,
            Schema.column "date" Schema.TUTCTime
          ]
      }
  ]

tests :: Fixture.Options -> SpecWith TestEnvironment
tests opts = do
  let simpleQuery :: Text
      simpleQuery = "SELECT thing / 2 AS divided FROM stuff"

  let query :: Text
      query = "SELECT thing / {{denominator}} AS divided FROM stuff WHERE date = {{target_date}}"

  it "Fails to track a Native Query without admin access" $
    \testEnv -> do
      let schemaName = Schema.getSchemaName testEnv
      shouldReturnYaml
        opts
        ( GraphqlEngine.postMetadataWithStatusAndHeaders
            400
            testEnv
            [ ("X-Hasura-Role", "not-admin")
            ]
            [yaml|
              type: pg_track_native_query
              args:
                type: query
                source: postgres
                root_field_name: divided_stuff
                code: *query
                arguments:
                  denominator: int
                  target_date: date
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
  it
    "Fails to untrack a Native Query without admin access"
    $ \testEnv -> do
      shouldReturnYaml
        opts
        ( GraphqlEngine.postMetadataWithStatusAndHeaders
            400
            testEnv
            [ ("X-Hasura-Role", "not-admin")
            ]
            [yaml|
              type: pg_untrack_native_query
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
  it
    "Fails to list a Native Query without admin access"
    $ \testEnv -> do
      shouldReturnYaml
        opts
        ( GraphqlEngine.postMetadataWithStatusAndHeaders
            400
            testEnv
            [ ("X-Hasura-Role", "not-admin")
            ]
            [yaml|
              type: pg_get_native_query
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
    it "Adds a simple native access function with no arguments and returns a 200" $ \testEnv -> do
      let schemaName = Schema.getSchemaName testEnv

      shouldReturnYaml
        opts
        ( GraphqlEngine.postMetadata
            testEnv
            [yaml|
              type: pg_track_native_query
              args:
                type: query
                source: postgres
                root_field_name: divided_stuff
                code: *simpleQuery
                arguments:
                  unused: int
                returns:
                  name: already_tracked_return_type
                  schema: *schemaName
            |]
        )
        [yaml|
          message: success
        |]

    it "Adding a native access function with broken SQL returns a 400" $ \testEnv -> do
      let schemaName = Schema.getSchemaName testEnv

      shouldReturnYaml
        opts
        ( GraphqlEngine.postMetadataWithStatus
            400
            testEnv
            [yaml|
              type: pg_track_native_query
              args:
                type: query
                source: postgres
                root_field_name: divided_stuff
                code: "SELECT * FROM dogs WHERE name = {{name"
                arguments:
                  denominator: int
                  target_date: date
                returns:
                  name: already_tracked_return_type
                  schema: *schemaName
            |]
        )
        [yaml|
          code: parse-failed
          error: "Found '{{' without a matching closing '}}'"
          path: "$.args"
        |]

    it "Checks for the native access function" $ \testEnv -> do
      let schemaName = Schema.getSchemaName testEnv

      shouldReturnYaml
        opts
        ( GraphqlEngine.postMetadata
            testEnv
            [yaml|
              type: pg_track_native_query
              args:
                type: query
                source: postgres
                root_field_name: divided_stuff
                code: *query
                arguments:
                  denominator: int
                  target_date: date
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
              type: pg_get_native_query
              args:
                source: postgres
            |]
        )
        [yaml|
          - root_field_name: divided_stuff
            code: *query
            arguments:
              denominator: int
              target_date: date
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
            type: pg_track_native_query
            args:
              type: query
              source: postgres
              root_field_name: divided_stuff
              code: *query
              arguments:
                denominator: int
                target_date: date
              returns:
                name: already_tracked_return_type
                schema: *schemaName
          |]

      shouldReturnYaml
        opts
        ( GraphqlEngine.postMetadata
            testEnv
            [yaml|
              type: pg_untrack_native_query
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
            type: pg_track_native_query
            args:
              type: query
              source: postgres
              root_field_name: divided_stuff
              code: *query
              arguments:
                denominator: int
                target_date: date
              returns:
                name: already_tracked_return_type
                schema: *schemaName
          |]

      _ <-
        GraphqlEngine.postMetadata
          testEnv
          [yaml|
            type: pg_untrack_native_query
            args:
              root_field_name: divided_stuff
              source: postgres
          |]

      shouldReturnYaml
        opts
        ( GraphqlEngine.postMetadata
            testEnv
            [yaml|
              type: pg_get_native_query
              args:
                source: postgres
            |]
        )
        [yaml|
          []
        |]

  describe "Validation fails on untrack a native query" do
    it "when native query does not exist" $
      \testEnv -> do
        shouldReturnYaml
          opts
          ( GraphqlEngine.postMetadataWithStatus
              400
              testEnv
              [yaml|
              type: pg_untrack_native_query
              args:
                root_field_name: some_native_query
                source: postgres
            |]
          )
          [yaml|
          code: not-found
          error: "Native query 'some_native_query' not found in source 'postgres'."
          path: "$.args"
        |]

  describe "Validation fails on track a native query when query" do
    it "has a syntax error" $
      \testEnv -> do
        let schemaName = Schema.getSchemaName testEnv
        let spicyQuery :: Text
            spicyQuery = "query bad"
        shouldReturnYaml
          opts
          ( GraphqlEngine.postMetadataWithStatus
              400
              testEnv
              [yaml|
                type: pg_track_native_query
                args:
                  type: query
                  source: postgres
                  root_field_name: divided_stuff
                  code: *spicyQuery
                  arguments:
                    denominator: int
                    target_date: date
                  returns:
                    name: already_tracked_return_type
                    schema: *schemaName
              |]
          )
          [yaml|
              code: validation-failed
              error: Failed to validate query
              internal:
                arguments: []
                error:
                  description: null
                  exec_status: "FatalError"
                  hint: null
                  message: "syntax error at or near \"query\""
                  status_code: "42601"
                prepared: false
                statement: "PREPARE _naqi_vali_divided_stuff AS query bad"
              path: "$.args"
          |]

    it "refers to non existing table" $
      \testEnv -> do
        let schemaName = Schema.getSchemaName testEnv
        let spicyQuery :: Text
            spicyQuery = "SELECT thing / {{denominator}} AS divided FROM does_not_exist WHERE date = {{target_date}}"
        shouldReturnYaml
          opts
          ( GraphqlEngine.postMetadataWithStatus
              400
              testEnv
              [yaml|
                type: pg_track_native_query
                args:
                  type: query
                  source: postgres
                  root_field_name: divided_stuff
                  code: *spicyQuery
                  arguments:
                    denominator: int
                    target_date: date
                  returns:
                    name: already_tracked_return_type
                    schema: *schemaName
              |]
          )
          [yaml|
              code: validation-failed
              error: Failed to validate query
              internal:
                arguments: []
                error:
                  description: null
                  exec_status: "FatalError"
                  hint: null
                  message: "relation \"does_not_exist\" does not exist"
                  status_code: "42P01"
                prepared: false
                statement: "PREPARE _naqi_vali_divided_stuff AS SELECT thing / $1 AS divided FROM does_not_exist WHERE date = $2"
              path: "$.args"
          |]
