{-# LANGUAGE QuasiQuotes #-}

-- | See https://github.com/hasura/graphql-engine/issues/8780
module Test.Regression.NullsOrderParsing8780Spec (spec) where

import Data.Aeson (Value)
import Data.List.NonEmpty qualified as NE
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml (interpolateYaml, yaml)
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.Schema
import Harness.TestEnvironment (TestEnvironment)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec

--------------------------------------------------------------------------------
-- Preamble

spec :: SpecWith TestEnvironment
spec = do
  -- Postgres
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Fixture.Postgres)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Postgres.setupTablesAction schema testEnv
                ]
            }
        ]
    )
    postgresTests

--------------------------------------------------------------------------------
-- Schema

schema :: [Table]
schema = [testTable]

testTable :: Table
testTable =
  (table "test")
    { tableColumns =
        [ column "id" TInt,
          column "name" TStr,
          column "age" TInt
        ],
      tablePrimaryKey = ["id"],
      tableData =
        [ [VInt 1, VStr "Author 1", VInt 25],
          [VInt 2, VStr "Author 2", VInt 30]
        ]
    }

--------------------------------------------------------------------------------
-- Tests

postgresTests :: Fixture.Options -> SpecWith TestEnvironment
postgresTests opts = do
  it "Browse table rows using nulls_first" \testEnvironment -> do
    let schemaName = getSchemaName testEnvironment
        source = Fixture.defaultSource Fixture.Postgres

    let queryYaml =
          [interpolateYaml|
            type: bulk
            source: #{source}
            args:
            - type: select
              args:
                source: #{source}
                table:
                  schema: #{schemaName}
                  name: test
                columns:
                - id
                - name
                - age
                offset: 0
                limit: 10
                order_by:
                - column: name
                  type: asc
                  nulls: first
          |]
    let expectedYaml :: Value
        expectedYaml =
          [yaml|
                  -
                    - age: 25
                      id: 1
                      name: Author 1
                    - age: 30
                      id: 2
                      name: Author 2
                |]
    shouldReturnYaml
      opts
      (GraphqlEngine.postV2Query 200 testEnvironment queryYaml)
      expectedYaml

  it "Browse table rows using nulls_last" \testEnvironment -> do
    let schemaName = getSchemaName testEnvironment
        source = Fixture.defaultSource Fixture.Postgres

    let queryYaml :: Value
        queryYaml =
          [interpolateYaml|
            type: bulk
            source: #{source}
            args:
            - type: select
              args:
                source: #{source}
                table:
                  schema: #{schemaName}
                  name: test
                columns:
                - id
                - name
                - age
                offset: 0
                limit: 10
                order_by:
                - column: name
                  type: asc
                  nulls: last
          |]

    let expectedYaml :: Value
        expectedYaml =
          [yaml|
                  -
                    - age: 25
                      id: 1
                      name: Author 1
                    - age: 30
                      id: 2
                      name: Author 2
                |]

    shouldReturnYaml
      opts
      (GraphqlEngine.postV2Query 200 testEnvironment queryYaml)
      expectedYaml
