{-# LANGUAGE QuasiQuotes #-}

-- | Testing custom root fields.
--   See the main hasura documentation for more information.
--
--   - Postgres: https://hasura.io/docs/latest/graphql/core/databases/postgres/schema/custom-field-names/#expose-table-root-fields-with-a-different-name-in-the-graphql-api
module Test.CustomRootFieldsSpec (spec) where

import Data.List.NonEmpty qualified as NE
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (yaml)
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.Schema (Table (..), table)
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (TestEnvironment)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, it)

--------------------------------------------------------------------------------

-- * Preamble

spec :: SpecWith TestEnvironment
spec =
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Fixture.Postgres)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Postgres.setupTablesAction schema testEnv
                ]
                  <> postgresCreateCustomNames testEnv
            }
        ]
    )
    streamingSubscriptionCustomRootFieldTests

--------------------------------------------------------------------------------

-- * Tests

streamingSubscriptionCustomRootFieldTests :: Fixture.Options -> SpecWith TestEnvironment
streamingSubscriptionCustomRootFieldTests opts = do
  -- TODO: At the time of writing this, there's no easy way to make a subscription
  -- request in the tests-hspec. When we do support that, we can add a new test that tests
  -- the streaming subscription with the customized root field name.
  it "The introspection includes the customized streaming subscription root fields" $ \testEnvironment ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphqlWithHeaders
          testEnvironment
          [("X-Hasura-Role", "user")]
          [graphql|
query {
  __type(name: "subscription_root") {
    fields {
      name
    }

  }
}
          |]
      )
      [yaml|
data:
  __type:
    fields:
      - name: LogsStream
           |]

--------------------------------------------------------------------------------

-- * Backend

-- ** Schema

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

-- ** Postgres backend

postgresCreateCustomNames :: TestEnvironment -> [Fixture.SetupAction]
postgresCreateCustomNames testEnv = do
  let source = Fixture.defaultBackendTypeString Fixture.Postgres
   in [ Fixture.SetupAction
          { Fixture.setupAction =
              GraphqlEngine.postMetadata_
                testEnv
                [yaml|
          type: pg_set_table_customization
          args:
            source: *source
            table:
              schema: hasura
              name: logs
            configuration:
              custom_root_fields:
                 "select_stream": "LogsStream"
          |],
            Fixture.teardownAction = \_ -> pure ()
          },
        Fixture.SetupAction
          { Fixture.setupAction =
              GraphqlEngine.postMetadata_
                testEnv
                [yaml|
          type: pg_create_select_permission
          args:
            source: *source
            table:
              schema: hasura
              name: logs
            role: user
            permission:
              columns: "*"
              filter: {}
              subscription_root_fields: ["select_stream"]
          |],
            Fixture.teardownAction = \_ ->
              GraphqlEngine.postMetadata_
                testEnv
                [yaml|
          type: pg_drop_select_permission
          args:
            source: *source
            table:
              schema: hasura
              name: logs
            role: user
          |]
          }
      ]
