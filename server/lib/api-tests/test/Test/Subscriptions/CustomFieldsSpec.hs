{-# LANGUAGE QuasiQuotes #-}

-- | Testing custom root fields for subscriptions.
--
-- https://hasura.io/docs/latest/graphql/core/databases/postgres/schema/custom-field-names/#expose-table-root-fields-with-a-different-name-in-the-graphql-api
module Test.Subscriptions.CustomFieldsSpec (spec) where

import Data.Aeson (Value)
import Data.List.NonEmpty qualified as NE
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine (postGraphqlWithHeaders, postMetadata_)
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (interpolateYaml, yaml)
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.Schema (Table (..), table)
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (TestEnvironment)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, it)

spec :: SpecWith TestEnvironment
spec =
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Fixture.Postgres)
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

tests :: Fixture.Options -> SpecWith TestEnvironment
tests opts = do
  let shouldBe :: IO Value -> Value -> IO ()
      shouldBe = shouldReturnYaml opts

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

    actual `shouldBe` expected

--------------------------------------------------------------------------------
-- Metadata

setupMetadata :: TestEnvironment -> Fixture.SetupAction
setupMetadata testEnvironment = do
  let schemaName :: Schema.SchemaName
      schemaName = Schema.getSchemaName testEnvironment

      setup :: IO ()
      setup =
        postMetadata_
          testEnvironment
          [interpolateYaml|
            type: bulk
            args:
            - type: pg_set_table_customization
              args:
                source: postgres
                table:
                  schema: #{schemaName}
                  name: logs
                configuration:
                  custom_root_fields:
                     select_stream: LogsStream
            - type: pg_create_select_permission
              args:
                source: postgres
                table:
                  schema: #{schemaName}
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
            type: pg_drop_select_permission
            args:
              source: postgres
              table:
                schema: hasura
                name: logs
              role: user
          |]

  Fixture.SetupAction setup \_ -> teardown
