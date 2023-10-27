{-# LANGUAGE QuasiQuotes #-}

-- |
-- Postgres-specific test case for permissions using remote source relationship
module Test.Databases.Postgres.DataValidation.RemoteSourcePermissionPredicateSpec (spec) where

import Data.Aeson (Value)
import Data.ByteString (ByteString)
import Data.List.NonEmpty qualified as NE
import Harness.GlobalTestEnvironment (Protocol (WebSocket), requestProtocol)
import Harness.GraphqlEngine
  ( postGraphqlWithHeaders,
    postGraphqlYamlWithHeaders,
    postMetadata,
    postMetadataWithStatus,
    postMetadata_,
  )
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (interpolateYaml, yaml)
import Harness.Schema qualified as Schema
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment
  ( GlobalTestEnvironment,
    TestEnvironment (..),
  )
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Network.HTTP.Types.Header (HeaderName)
import Test.Hspec (SpecWith, describe, it)
import Test.Schema.RemoteRelationships.MetadataAPI.Common qualified as Common

spec :: SpecWith GlobalTestEnvironment
spec = Fixture.runWithLocalTestEnvironment contexts tests
  where
    contexts = NE.fromList [Common.dbTodbRemoteRelationshipFixture]

--------------------------------------------------------------------------------
-- Tests

tests :: SpecWith (TestEnvironment, Common.LocalTestTestEnvironment)
tests = do
  describe "Simple row level permissions on track table to filter based on artist_id" do
    it "able to setup permission" \(testEnvironment, _) -> do
      let schemaName = Schema.getSchemaName testEnvironment
          metadataQuery =
            postMetadata
              testEnvironment
              [yaml|
                type: pg_create_select_permission
                args:
                  source: source
                  table:
                    schema: *schemaName
                    name: track
                  role: user
                  permission:
                    filter:
                      albums:
                        id: 
                          _eq: x-hasura-album-id
                    columns: '*'
              |]
      shouldReturnYaml
        testEnvironment
        metadataQuery
        [yaml|
        message: success
      |]

    it "filters rows based on the remote source relationship predicate" \(testEnvironment, _) -> do
      let schemaName = Schema.getSchemaName testEnvironment
      postMetadata_
        testEnvironment
        [yaml|
          type: pg_create_select_permission
          args:
            source: source
            table:
              schema: *schemaName
              name: track
            role: user
            permission:
              filter:
                albums:
                  id: 
                    _eq: x-hasura-user-id
              columns: '*'
        |]

      let userRoleHeaders :: ByteString -> [(HeaderName, ByteString)]
          userRoleHeaders userId =
            [ ("X-Hasura-Role", "user"),
              ("x-hasura-user-id", userId)
            ]

          expectedForUser1 :: Value
          expectedForUser1 =
            [yaml|
              data:
                hasura_track: 
                - id: 1
            |]

          expectedForUser2 :: Value
          expectedForUser2 =
            [yaml|
              data:
                hasura_track: 
                - id: 1
            |]

          expectedForUser3 :: Value
          expectedForUser3 =
            [yaml|
              data:
                hasura_track: 
                - id: 2
            |]

          actual :: [(HeaderName, ByteString)] -> IO Value
          actual headers =
            postGraphqlWithHeaders
              testEnvironment
              headers
              [graphql|
                query {
                    hasura_track {
                        id
                    }
                }
              |]

      shouldReturnYaml testEnvironment (actual (userRoleHeaders "1")) expectedForUser1
      shouldReturnYaml testEnvironment (actual (userRoleHeaders "2")) expectedForUser2
      shouldReturnYaml testEnvironment (actual (userRoleHeaders "3")) expectedForUser3

  describe "Trying different allowed remote relationship predicates" do
    it "remote relationship predicates in _and and _or" \(testEnvironment, _) -> do
      let schemaName = Schema.getSchemaName testEnvironment
          metadataQuery =
            postMetadata
              testEnvironment
              [yaml|
                type: pg_create_select_permission
                args:
                  source: source
                  table:
                    schema: *schemaName
                    name: track
                  role: user
                  permission:
                    filter:
                      _and:
                      - albums:
                          id: 
                            _lt: x-hasura-album-id
                      - _or:
                        - albums:
                            artist_id: 
                              _gt: x-hasura-user-id
                        - albums:
                            id: 
                              _lte: x-hasura-album-id
                    columns: '*'
              |]
      shouldReturnYaml
        testEnvironment
        metadataQuery
        [yaml|
        message: success
      |]
    it "remote relationship predicates in _not" \(testEnvironment, _) -> do
      let schemaName = Schema.getSchemaName testEnvironment
          metadataQuery =
            postMetadata
              testEnvironment
              [yaml|
                type: pg_create_select_permission
                args:
                  source: source
                  table:
                    schema: *schemaName
                    name: track
                  role: user
                  permission:
                    filter:
                      _not:
                        albums:
                          id: 
                            _gte: x-hasura-album-id
                    columns: '*'
              |]
      shouldReturnYaml
        testEnvironment
        metadataQuery
        [yaml|
        message: success
      |]

  describe "Trying different not allowed remote relationship predicates" do
    it "nested remote relationship predicate" \(testEnvironment, _) -> do
      let schemaName = Schema.getSchemaName testEnvironment
      postMetadata_
        testEnvironment
        [yaml|
          type: bulk
          args:
          - type: pg_create_remote_relationship
            args:
              source: target
              table:
                schema: *schemaName
                name: album
              name: tracks
              definition:
                to_source:
                  source: source
                  table:
                    schema: hasura
                    name: track
                  relationship_type: object
                  field_mapping:
                    id: album_id
            |]
      let metadataQuery =
            postMetadataWithStatus
              400
              testEnvironment
              [yaml|
                type: pg_create_select_permission
                args:
                  source: source
                  table:
                    schema: *schemaName
                    name: track
                  role: user
                  permission:
                    filter:
                      albums:
                        tracks: 
                          albums:
                            id:
                              _eq: x-hasura-album-id
                    columns: '*'
              |]
      shouldReturnYaml
        testEnvironment
        metadataQuery
        [yaml|
        code: invalid-configuration
        error: 'Inconsistent object: in table "hasura.track": in permission for role "user":
          Expected one of the supported operators (_eq, _neq, _gt, _lt, _gte, _lte, _in, _nin,
          _like, _nlike and _is_null)'
        internal:
        - definition:
            comment: null
            permission:
              allow_aggregations: false
              columns: '*'
              computed_fields: []
              filter:
                albums:
                  tracks:
                    albums:
                      id:
                        _eq: x-hasura-album-id
            role: user
            source: source
            table:
              name: track
              schema: hasura
          name: select_permission user in table hasura.track in source source
          reason: 'Inconsistent object: in table "hasura.track": in permission for role "user":
            Expected one of the supported operators (_eq, _neq, _gt, _lt, _gte, _lte, _in,
            _nin, _like, _nlike and _is_null)'
          type: select_permission
        path: $.args
      |]

  describe "Subscriptions on fields with permission based on remote relationship" do
    it "shouldn't be allowed" \(testEnvironment, _) -> do
      let schemaName = Schema.getSchemaName testEnvironment
          userRoleHeaders :: ByteString -> [(HeaderName, ByteString)]
          userRoleHeaders userId =
            [ ("X-Hasura-Role", "user"),
              ("x-hasura-user-id", userId)
            ]
          metadataQuery =
            postMetadata
              testEnvironment
              [yaml|
                type: pg_create_select_permission
                args:
                  source: source
                  table:
                    schema: *schemaName
                    name: track
                  role: user
                  permission:
                    filter:
                      albums:
                        id: 
                          _eq: x-hasura-album-id
                    columns: '*'
              |]
      shouldReturnYaml
        testEnvironment
        metadataQuery
        [yaml|
        message: success
      |]

      let expected :: Value
          expected =
            [interpolateYaml|
              errors:
              - extensions:
                  code: not-supported
                  path: $
                message: subscriptions on this field is not supported
            |]
          websocketTestEnv =
            testEnvironment
              { globalEnvironment =
                  (globalEnvironment testEnvironment)
                    { requestProtocol = WebSocket
                    }
              }

          actual :: IO Value
          actual =
            postGraphqlYamlWithHeaders
              websocketTestEnv
              (userRoleHeaders "1")
              [interpolateYaml|
                query: |
                  subscription {
                      #{schemaName}_track {
                          id
                      }
                    }
              |]
      shouldReturnYaml websocketTestEnv actual expected
