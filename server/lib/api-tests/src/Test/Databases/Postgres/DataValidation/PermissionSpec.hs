{-# LANGUAGE QuasiQuotes #-}

-- |
-- Postgres-specific test case for permissions expressed as arrays.
--
-- https://github.com/hasura/graphql-engine-mono/pull/4651
module Test.Databases.Postgres.DataValidation.PermissionSpec (spec) where

import Data.Aeson (Value)
import Data.ByteString (ByteString)
import Data.List.NonEmpty qualified as NE
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine (postGraphqlWithHeaders, postMetadata_)
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (yaml)
import Harness.Schema (Table (..))
import Harness.Schema qualified as Schema
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Network.HTTP.Types.Header (HeaderName)
import Test.Hspec (SpecWith, describe, it)

spec :: SpecWith GlobalTestEnvironment
spec =
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Postgres.setupTablesAction schema testEnvironment,
                  setupMetadata testEnvironment
                ]
            }
        ]
    )
    tests

--------------------------------------------------------------------------------
-- Schema

schema :: [Schema.Table]
schema =
  [ (Schema.table "author")
      { tableColumns =
          [ Schema.column "id" Schema.TInt,
            Schema.column "name" Schema.TStr
          ],
        tablePrimaryKey = ["id"],
        tableData =
          [ [Schema.VInt 1, Schema.VStr "Author 1"],
            [Schema.VInt 2, Schema.VStr "Author 2"]
          ]
      }
  ]

--------------------------------------------------------------------------------
-- Tests

tests :: SpecWith TestEnvironment
tests = do
  describe "Permissions involving array types" do
    it "Non-matching X-Hasura-Allowed-Ids should return no data" \testEnvironment -> do
      let headers :: [(HeaderName, ByteString)]
          headers =
            [ ("X-Hasura-Role", "user"),
              ("X-Hasura-Allowed-Ids", "{}")
            ]

          expected :: Value
          expected =
            [yaml|
              data:
                hasura_author: []
            |]

          actual :: IO Value
          actual =
            postGraphqlWithHeaders
              testEnvironment
              headers
              [graphql|
                query {
                    hasura_author {
                        id
                        name
                    }
                }
              |]

      shouldReturnYaml testEnvironment actual expected

    it "Matching X-Hasura-Allowed-Ids should return data" \testEnvironment -> do
      let headers :: [(HeaderName, ByteString)]
          headers =
            [ ("X-Hasura-Role", "user"),
              ("X-Hasura-Allowed-Ids", "{1,2,3}")
            ]

          expected :: Value
          expected =
            [yaml|
              data:
                hasura_author:
                - name: Author 1
                  id: 1
                - name: Author 2
                  id: 2
            |]

          actual :: IO Value
          actual =
            postGraphqlWithHeaders
              testEnvironment
              headers
              [graphql|
                query {
                    hasura_author {
                        id
                        name
                    }
                }
              |]

      shouldReturnYaml testEnvironment actual expected

--------------------------------------------------------------------------------
-- Metadata

setupMetadata :: TestEnvironment -> Fixture.SetupAction
setupMetadata testEnvironment = do
  let setup :: IO ()
      setup =
        postMetadata_
          testEnvironment
          [yaml|
            type: pg_create_select_permission
            args:
              source: postgres
              table:
                schema: hasura
                name: author
              role: user
              permission:
                filter:
                  id:
                    _in: X-Hasura-Allowed-Ids
                columns: '*'
          |]

      teardown :: IO ()
      teardown =
        postMetadata_
          testEnvironment
          [yaml|
            type: bulk
            args:
            - type: pg_drop_select_permission
              args:
                source: postgres
                table:
                  schema: hasura
                  name: author
                role: user
            |]

  Fixture.SetupAction setup \_ -> teardown
