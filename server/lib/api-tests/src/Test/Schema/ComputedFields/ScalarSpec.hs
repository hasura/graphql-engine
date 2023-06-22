{-# LANGUAGE QuasiQuotes #-}

-- | Tests scalar computed fields whose associated SQL function
-- returns a base type like Integer, Boolean, Geography etc.
--
-- https://hasura.io/docs/latest/schema/postgres/computed-fields/#1-scalar-computed-fields
module Test.Schema.ComputedFields.ScalarSpec (spec) where

import Data.List.NonEmpty qualified as NE
import Data.String.Interpolate (i)
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (interpolateYaml)
import Harness.Schema (SchemaName (..), Table (..), table)
import Harness.Schema qualified as Schema
import Harness.Test.BackendType qualified as BackendType
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment (..), getBackendTypeConfig)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, it)

-- ** Preamble

spec :: SpecWith GlobalTestEnvironment
spec =
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Postgres.setupTablesAction schema testEnv
                ]
                  <> setupFunction testEnv
                  <> setupMetadata testEnv
            }
        ]
    )
    tests

-- ** Schema

schema :: [Table]
schema = [authorTable]

authorTable :: Table
authorTable =
  (table "author")
    { tableColumns =
        [ Schema.column "id" Schema.TInt,
          Schema.column "first_name" Schema.TStr,
          Schema.column "last_name" Schema.TStr
        ],
      tablePrimaryKey = ["id"],
      tableData =
        [ [ Schema.VInt 1,
            Schema.VStr "Author",
            Schema.VStr "1"
          ],
          [ Schema.VInt 2,
            Schema.VStr "Author",
            Schema.VStr "2"
          ]
        ]
    }

-- ** Setup and teardown

-- * SQL

authorFullNameOptionalSQL :: SchemaName -> Text
authorFullNameOptionalSQL schemaName =
  [i|
    CREATE FUNCTION #{ unSchemaName schemaName }.author_full_name_optional(author_row author, title text = 'Dr')
      RETURNS TEXT AS $$
      SELECT title || ' ' || author_row.first_name || ' ' || author_row.last_name
      $$ LANGUAGE sql STABLE;
  |]

authorFullNameSQL :: SchemaName -> Text
authorFullNameSQL schemaName =
  [i|
      CREATE FUNCTION #{ unSchemaName schemaName }.author_full_name(author_row author)
      RETURNS TEXT AS $$
      SELECT author_row.first_name || ' ' || author_row.last_name
      $$ LANGUAGE sql STABLE;
  |]

-- * Setup

setupFunction :: TestEnvironment -> [Fixture.SetupAction]
setupFunction testEnv =
  let schemaName = Schema.getSchemaName testEnv
   in [ Fixture.SetupAction
          { Fixture.setupAction =
              Postgres.run_ testEnv (authorFullNameSQL schemaName)
                >> Postgres.run_ testEnv (authorFullNameOptionalSQL schemaName),
            Fixture.teardownAction = \_ -> pure ()
          }
      ]

setupMetadata :: TestEnvironment -> [Fixture.SetupAction]
setupMetadata testEnvironment =
  let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment

      schemaName :: Schema.SchemaName
      schemaName = Schema.getSchemaName testEnvironment

      source :: String
      source = Fixture.backendSourceName backendTypeMetadata

      backendPrefix :: String
      backendPrefix = BackendType.backendTypeString backendTypeMetadata
   in [ Fixture.SetupAction
          { Fixture.setupAction = do
              GraphqlEngine.postMetadata_
                testEnvironment
                [interpolateYaml|
              type: #{ backendPrefix }_add_computed_field
              args:
                source: #{ source }
                name: full_name
                table:
                  schema: #{ schemaName }
                  name: author
                definition:
                  function:
                    schema: #{ schemaName }
                    name: author_full_name
              |]
              GraphqlEngine.postMetadata_
                testEnvironment
                [interpolateYaml|
              type: #{ backendPrefix }_add_computed_field
              args:
                source: #{ source }
                name: full_name_optional
                table:
                  schema: #{ schemaName }
                  name: author
                definition:
                  function:
                    schema: #{ schemaName }
                    name: author_full_name_optional
              |],
            Fixture.teardownAction = \_ -> pure ()
          }
      ]

-- * Tests

tests :: SpecWith TestEnvironment
tests = do
  it "Query data from the authors table" $ \testEnv -> do
    let schemaName = Schema.getSchemaName testEnv

    shouldReturnYaml
      testEnv
      ( GraphqlEngine.postGraphql
          testEnv
          [graphql|
            query {
              #{schemaName}_author {
                id
                full_name
              }
            }
          |]
      )
      [interpolateYaml|
        data:
          #{schemaName}_author:
            - id: 1
              full_name: Author 1
            - id: 2
              full_name: Author 2
      |]

  it "Query data from the authors table using default value" $ \testEnv -> do
    let schemaName = Schema.getSchemaName testEnv

    shouldReturnYaml
      testEnv
      ( GraphqlEngine.postGraphql
          testEnv
          [graphql|
            query {
              #{schemaName}_author {
                id
                full_name_optional
              }
            }
          |]
      )
      [interpolateYaml|
        data:
          #{schemaName}_author:
            - id: 1
              full_name_optional: Dr Author 1
            - id: 2
              full_name_optional: Dr Author 2
      |]
