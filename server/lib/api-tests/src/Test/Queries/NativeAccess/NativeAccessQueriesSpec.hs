{-# LANGUAGE QuasiQuotes #-}

-- | Access to the SQL
module Test.Queries.NativeAccess.NativeAccessQueriesSpec (spec) where

import Data.Aeson (Value)
import Data.List.NonEmpty qualified as NE
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql
import Harness.Quoter.Yaml (yaml)
import Harness.Test.BackendType qualified as BackendType
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.Schema (Table (..), table)
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment, getBackendTypeConfig)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

-- ** Preamble

featureFlagForNativeQuery :: String
featureFlagForNativeQuery = "HASURA_FF_NATIVE_QUERY_INTERFACE"

spec :: SpecWith GlobalTestEnvironment
spec =
  Fixture.hgeWithEnv [(featureFlagForNativeQuery, "True")] $
    Fixture.run
      ( NE.fromList
          [ (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
              { Fixture.setupTeardown = \(testEnvironment, _) ->
                  [ Postgres.setupTablesAction schema testEnvironment
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
  [ (table "hello_world_table")
      { tableColumns =
          [ Schema.column "one" Schema.TStr,
            Schema.column "two" Schema.TStr
          ]
      }
  ]

tests :: Fixture.Options -> SpecWith TestEnvironment
tests opts = do
  let query :: Text
      query = "SELECT 'hello' as \"one\", 'world' as \"two\""

      shouldBe :: IO Value -> Value -> IO ()
      shouldBe = shouldReturnYaml opts

  describe "Testing Native Access" $ do
    it "Runs a simple query that takes no parameters" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          source = BackendType.backendSourceName backendTypeMetadata
          schemaName = Schema.getSchemaName testEnvironment

      shouldReturnYaml
        opts
        ( GraphqlEngine.postMetadata
            testEnvironment
            [yaml|
              type: pg_track_native_query
              args:
                type: query
                source: *source
                root_field_name: hello_world_function
                code: *query
                arguments:
                  ignored: varchar
                returns:
                  name: hello_world_table
                  schema: *schemaName
            |]
        )
        [yaml|
          message: success
        |]

      let expected =
            [yaml|
                data:
                  hello_world_function:
                    - one: "hello"
                      two: "world"
              |]

          actual :: IO Value
          actual =
            GraphqlEngine.postGraphql
              testEnvironment
              [graphql|
              query {
                hello_world_function(args: { ignored: "hi" }) {
                  one
                  two
                }
              }
           |]

      actual `shouldBe` expected
