{-# LANGUAGE QuasiQuotes #-}

-- |
-- See https://github.com/hasura/graphql-engine/issues/8158
module Test.Regression.DoNotTruncateSessionVariables8158Spec (spec) where

import Data.Aeson (Value)
import Data.List.NonEmpty qualified as NE
import Harness.Backend.Sqlserver qualified as Sqlserver
import Harness.GraphqlEngine (postGraphqlWithHeaders, postMetadata_)
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (interpolateYaml, yaml)
import Harness.Schema (Table (..), table)
import Harness.Schema qualified as Schema
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

spec :: SpecWith GlobalTestEnvironment
spec = do
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Sqlserver.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Sqlserver.setupTablesAction schema testEnvironment,
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
  [ (table "author")
      { tableColumns =
          [ Schema.column "uuid" do
              Schema.TCustomType
                Schema.defaultBackendScalarType
                  { Schema.bstMssql = Just "VARCHAR(50)"
                  },
            Schema.column "name" Schema.TStr
          ],
        tablePrimaryKey = ["uuid"],
        tableData =
          [ [Schema.VStr "36a6257b-08bb-45ef-a5cf-c1b7a7997087", Schema.VStr "Author 1"],
            [Schema.VStr "36a6257b-08bb-45ef-a5cf-c1b7a7", Schema.VStr "Author 2"]
          ]
      }
  ]

--------------------------------------------------------------------------------
-- Tests

tests :: SpecWith TestEnvironment
tests = do
  describe "Regression test for #8158" do
    it "Session variable strings are not truncated to 30 characters" \testEnvironment -> do
      let schemaName :: Schema.SchemaName
          schemaName = Schema.getSchemaName testEnvironment

          expected :: Value
          expected =
            [interpolateYaml|
              data:
                #{schemaName}_author:
                - name: 'Author 1'
                  uuid: '36a6257b-08bb-45ef-a5cf-c1b7a7997087'
            |]

          actual :: IO Value
          actual =
            postGraphqlWithHeaders
              testEnvironment
              [ ("X-Hasura-Role", "user"),
                ("X-Hasura-User-Id", "36a6257b-08bb-45ef-a5cf-c1b7a7997087")
              ]
              [graphql|
                query {
                  #{schemaName}_author {
                    name
                    uuid
                  }
                }
              |]

      shouldReturnYaml testEnvironment actual expected

--------------------------------------------------------------------------------
-- Metadata

setupMetadata :: TestEnvironment -> Fixture.SetupAction
setupMetadata testEnvironment = Fixture.SetupAction setup \_ -> teardown
  where
    setup =
      postMetadata_
        testEnvironment
        [yaml|
          type: mssql_create_select_permission
          args:
            source: mssql
            table:
              schema: hasura
              name: author
            role: user
            permission:
              filter:
                uuid: X-Hasura-User-Id
              columns: '*'
        |]

    teardown =
      postMetadata_
        testEnvironment
        [yaml|
          type: mssql_drop_select_permission
          args:
            source: mssql
            table:
              schema: hasura
              name: author
            role: user
        |]
