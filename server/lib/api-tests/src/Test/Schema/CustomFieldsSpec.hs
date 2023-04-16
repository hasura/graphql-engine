{-# LANGUAGE QuasiQuotes #-}

-- | Testing custom root fields
--
-- https://hasura.io/docs/latest/graphql/core/databases/postgres/schema/custom-field-names/
module Test.Schema.CustomFieldsSpec (spec) where

import Data.Aeson (Value)
import Data.Aeson.Key qualified as Key (toString)
import Data.List.NonEmpty qualified as NE
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Cockroach qualified as Cockroach
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine (postGraphqlWithHeaders, postMetadata_)
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (interpolateYaml, yaml)
import Harness.Schema (Table (..), table)
import Harness.Schema qualified as Schema
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, it)

spec :: SpecWith GlobalTestEnvironment
spec =
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Postgres.setupTablesAction schema testEnvironment,
                  setupMetadata Postgres.backendTypeMetadata testEnvironment
                ]
            },
          (Fixture.fixture $ Fixture.Backend Citus.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Citus.setupTablesAction schema testEnvironment,
                  setupMetadata Citus.backendTypeMetadata testEnvironment
                ]
            },
          (Fixture.fixture $ Fixture.Backend Cockroach.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Cockroach.setupTablesAction schema testEnvironment,
                  setupMetadata Cockroach.backendTypeMetadata testEnvironment
                ]
            }
        ]
    )
    tests

--------------------------------------------------------------------------------
-- Schema

schema :: [Schema.Table]
schema =
  [ (table "logs")
      { tableColumns =
          [ Schema.column "Id" Schema.TInt,
            Schema.column "Log" Schema.TStr,
            Schema.column "Level" Schema.TStr
          ],
        tablePrimaryKey = ["Id"],
        tableData = []
      }
  ]

--------------------------------------------------------------------------------
-- Tests

tests :: SpecWith TestEnvironment
tests = do
  -- TODO: At the time of writing this, there's no easy way to make a
  -- subscription request in the tests-hspec. When we do support that, we can
  -- add a new test that tests the streaming subscription with the customized
  -- root field name.
  it "The introspection includes the customized streaming subscription root fields" \testEnvironment -> do
    let expected :: Value
        expected =
          [yaml|
            data:
              __type:
                fields:
                  - name: LogsStream
           |]

        actual :: IO Value
        actual =
          postGraphqlWithHeaders
            testEnvironment
            [ ("X-Hasura-Role", "user")
            ]
            [graphql|
              query {
                __type(name: "subscription_root") {
                  fields {
                    name
                  }
                }
              }
            |]

    shouldReturnYaml testEnvironment actual expected

--------------------------------------------------------------------------------
-- Metadata

setupMetadata :: Fixture.BackendTypeConfig -> TestEnvironment -> Fixture.SetupAction
setupMetadata backendTypeMetadata testEnvironment = do
  let schemaName :: Schema.SchemaName
      schemaName = Schema.getSchemaName testEnvironment

      schemaKeyword :: String
      schemaKeyword = Key.toString $ Fixture.backendSchemaKeyword backendTypeMetadata

      backendPrefix :: String
      backendPrefix = Fixture.backendTypeString backendTypeMetadata

      source :: String
      source = Fixture.backendSourceName backendTypeMetadata

      setup :: IO ()
      setup =
        postMetadata_
          testEnvironment
          [interpolateYaml|
            type: bulk
            args:
            - type: #{backendPrefix}_set_table_customization
              args:
                source: #{source}
                table:
                  #{schemaKeyword}: #{schemaName}
                  name: logs
                configuration:
                  custom_root_fields:
                     select_stream: LogsStream
            - type: #{backendPrefix}_create_select_permission
              args:
                source: #{source}
                table:
                  #{schemaKeyword}: #{schemaName}
                  name: logs
                role: user
                permission:
                  columns: "*"
                  filter: {}
                  subscription_root_fields: ["select_stream"]
          |]

      teardown :: IO ()
      teardown =
        postMetadata_
          testEnvironment
          [interpolateYaml|
            type: #{backendPrefix}_drop_select_permission
            args:
              source: #{source}
              table:
                #{schemaKeyword}: #{schemaName}
                name: logs
              role: user
          |]

  Fixture.SetupAction setup \_ -> teardown
