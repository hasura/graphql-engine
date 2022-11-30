{-# LANGUAGE QuasiQuotes #-}

-- | Tests scalar computed fields whose associated SQL function
-- returns a base type like Integer, Boolean, Geography etc.
--
-- https://hasura.io/docs/latest/schema/postgres/computed-fields/#1-scalar-computed-fields
module Test.Schema.ComputedFields.ScalarSpec (spec) where

import Data.List.NonEmpty qualified as NE
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (interpolateYaml, yaml)
import Harness.Test.BackendType (BackendType (..))
import Harness.Test.BackendType qualified as BackendType
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.Schema (SchemaName (..), Table (..), table)
import Harness.Test.Schema qualified as Schema
import Harness.Test.SchemaName (SchemaName (..))
import Harness.TestEnvironment (TestEnvironment)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, it)

-- ** Preamble

spec :: SpecWith TestEnvironment
spec =
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Fixture.Postgres)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Postgres.setupTablesAction schema testEnv
                ]
                  <> setupFunction testEnv
                  <> setupMetadata testEnv BackendType.Postgres
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

-- | SQL
authorFullNameSQL :: SchemaName -> String
authorFullNameSQL schemaName =
  [i|
      CREATE FUNCTION #{ unSchemaName schemaName }.author_full_name(author_row author)
      RETURNS TEXT AS $$
      SELECT author_row.first_name || ' ' || author_row.last_name
      $$ LANGUAGE sql STABLE;
  |]

setupFunction :: TestEnvironment -> [Fixture.SetupAction]
setupFunction testEnv =
  let schemaName = Schema.getSchemaName testEnv
   in [ Fixture.SetupAction
          { Fixture.setupAction =
              Postgres.run_ testEnv (authorFullNameSQL schemaName),
            Fixture.teardownAction = \_ -> pure ()
          }
      ]

setupMetadata :: TestEnvironment -> BackendType -> [Fixture.SetupAction]
setupMetadata testEnv backend =
  let schemaName :: Schema.SchemaName
      schemaName = Schema.getSchemaName testEnv

      source :: String
      source = Fixture.defaultSource backend

      backendPrefix :: String
      backendPrefix = BackendType.defaultBackendTypeString backend
   in [ Fixture.SetupAction
          { Fixture.setupAction =
              GraphqlEngine.postMetadata_
                testEnv
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
              |],
            Fixture.teardownAction = \_ -> pure ()
          }
      ]

-- * Tests

tests :: Fixture.Options -> SpecWith TestEnvironment
tests opts = do
  it "Query data from the authors table" $ \testEnv -> do
    let schemaName = Schema.getSchemaName testEnv

    shouldReturnYaml
      opts
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
