{-# LANGUAGE QuasiQuotes #-}

-- | Test long identifiers
--
--   https://github.com/hasura/graphql-engine/issues/3796
--   See "Hasura.Backends.Postgres.SQL.RenameIdentifiers" for more details.
module Test.Regression.LongIdentifiers3796Spec (spec) where

import Data.List.NonEmpty qualified as NE
import Harness.Backend.BigQuery qualified as BigQuery
import Harness.Backend.Cockroach qualified as Cockroach
import Harness.Backend.Postgres qualified as Postgres
import Harness.Backend.Sqlserver qualified as Sqlserver
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (interpolateYaml)
import Harness.Schema (Table (..), table)
import Harness.Schema qualified as Schema
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, it)

--------------------------------------------------------------------------------
-- Preamble

spec :: SpecWith GlobalTestEnvironment
spec = do
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Postgres.setupTablesAction schema testEnv
                ]
            },
          -- Create table fails currently on a weird error:
          -- > relation "i_need_a_table_with_a_long_na_i_need_a_column_with_a_long_n_seq" already exists
          --
          -- (Fixture.fixture $ Fixture.Backend Citus.backendTypeMetadata)
          --   { Fixture.setupTeardown = \(testEnv, _) ->
          --       [ Citus.setupTablesAction schema testEnv
          --       ]
          --   },
          (Fixture.fixture $ Fixture.Backend Cockroach.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Cockroach.setupTablesAction schema testEnv
                ]
            },
          (Fixture.fixture $ Fixture.Backend Sqlserver.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Sqlserver.setupTablesAction schema testEnv
                ]
            },
          (Fixture.fixture $ Fixture.Backend BigQuery.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ BigQuery.setupTablesAction schema testEnv
                ],
              Fixture.customOptions =
                Just
                  $ Fixture.defaultOptions
                    { Fixture.stringifyNumbers = True
                    }
            }
        ]
    )
    tests
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Postgres.setupTablesAction schema testEnv
                ]
            }
        ]
    )
    (testInsert "Int")
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Cockroach.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Cockroach.setupTablesAction schema testEnv
                ]
            }
        ]
    )
    (testInsert "bigint")

--------------------------------------------------------------------------------
-- Schema

schema :: [Schema.Table]
schema = [regular, longtable, longtable2, multitable]

regular :: Schema.Table
regular =
  (table "regular_table_with_a_long_name_to_test_rename_identifiers")
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

longtable :: Schema.Table
longtable =
  (table "i_need_a_table_with_a_long_name_to_test_rename_identifiers") -- 58 characters
    { tableColumns =
        [ Schema.column "id" Schema.TInt,
          Schema.column "regular_id" Schema.TInt,
          Schema.column "i_need_a_column_with_a_long_name_to_test_rename_identifiers" Schema.TInt, -- 59 characters
          Schema.column "i_need_a_column_with_a_long_name_but_is_different" Schema.TInt -- 49 characters
        ],
      tablePrimaryKey = ["id"],
      tableReferences =
        [ Schema.reference
            "regular_id"
            "regular_table_with_a_long_name_to_test_rename_identifiers"
            "id"
        ],
      tableData =
        [ [Schema.VInt 1, Schema.VInt 1, Schema.VInt 1, Schema.VInt 1],
          [Schema.VInt 2, Schema.VInt 2, Schema.VInt 2, Schema.VInt 2]
        ]
    }

longtable2 :: Schema.Table
longtable2 =
  (table "i_need_a_table_with_a_long_name_to_test_rename_identifiers2") -- 59 characters
    { tableColumns =
        [ Schema.column "id" Schema.TInt,
          Schema.column "i_need_a_column_with_a_long_name_to_test_rename_identifiers" Schema.TInt, -- 59 characters
          Schema.column "i_need_a_column_with_a_long_name_but_is_different" Schema.TInt -- 49 characters
        ],
      tablePrimaryKey = ["id"],
      tableData =
        [ [Schema.VInt 1, Schema.VInt 1, Schema.VInt 1],
          [Schema.VInt 2, Schema.VInt 2, Schema.VInt 2]
        ]
    }

multitable :: Schema.Table
multitable =
  (table "table_with_multiple_relationships_and_long_name_2_test_rewrite") -- 62 characters
    { tableColumns =
        [ Schema.column "id" Schema.TInt,
          Schema.column "reference_for_longtable_id" Schema.TInt,
          Schema.column "reference_for_longtable2_id" Schema.TInt
        ],
      tablePrimaryKey = ["id"],
      tableReferences =
        [ Schema.reference
            "reference_for_longtable_id"
            "i_need_a_table_with_a_long_name_to_test_rename_identifiers"
            "id",
          Schema.reference
            "reference_for_longtable2_id"
            "i_need_a_table_with_a_long_name_to_test_rename_identifiers2"
            "id"
        ],
      tableData =
        [ [Schema.VInt 1, Schema.VInt 1, Schema.VInt 1],
          [Schema.VInt 2, Schema.VInt 2, Schema.VInt 2]
        ]
    }

--------------------------------------------------------------------------------
-- Tests

tests :: SpecWith TestEnvironment
tests = do
  it "select long table" $ \testEnvironment -> do
    let schemaName = Schema.getSchemaName testEnvironment

    shouldReturnYaml
      testEnvironment
      ( GraphqlEngine.postGraphql
          testEnvironment
          [graphql|
query {
  #{schemaName}_i_need_a_table_with_a_long_name_to_test_rename_identifiers(order_by:[{id:asc}]) {
    id
  }
}
|]
      )
      [interpolateYaml|
data:
  #{schemaName}_i_need_a_table_with_a_long_name_to_test_rename_identifiers:
  - id: 1
  - id: 2
|]

  it "select long column" $ \testEnvironment -> do
    let schemaName = Schema.getSchemaName testEnvironment

    shouldReturnYaml
      testEnvironment
      ( GraphqlEngine.postGraphql
          testEnvironment
          [interpolateYaml|
query {
  #{schemaName}_i_need_a_table_with_a_long_name_to_test_rename_identifiers(order_by:[{i_need_a_column_with_a_long_name_to_test_rename_identifiers:asc, i_need_a_column_with_a_long_name_but_is_different:asc}]) {
    id
    regular_id
    i_need_a_column_with_a_long_name_to_test_rename_identifiers
  }
}
|]
      )
      [interpolateYaml|
data:
  #{schemaName}_i_need_a_table_with_a_long_name_to_test_rename_identifiers:
  - id: 1
    regular_id: 1
    i_need_a_column_with_a_long_name_to_test_rename_identifiers: 1
  - id: 2
    regular_id: 2
    i_need_a_column_with_a_long_name_to_test_rename_identifiers: 2
|]

  it "select long column via array relationship" $ \testEnvironment -> do
    let schemaName = Schema.getSchemaName testEnvironment

    shouldReturnYaml
      testEnvironment
      ( GraphqlEngine.postGraphql
          testEnvironment
          [interpolateYaml|
query {
  #{schemaName}_regular_table_with_a_long_name_to_test_rename_identifiers(order_by:[{id:asc}]) {
    id
    i_need_a_table_with_a_long_name_to_test_rename_identifierss_by_id_to_regular_id(order_by:[{i_need_a_column_with_a_long_name_to_test_rename_identifiers:asc, i_need_a_column_with_a_long_name_but_is_different:asc}]) {
      i_need_a_column_with_a_long_name_to_test_rename_identifiers
      i_need_a_column_with_a_long_name_but_is_different
    }
  }
}
|]
      )
      [interpolateYaml|
data:
  #{schemaName}_regular_table_with_a_long_name_to_test_rename_identifiers:
  - id: 1
    i_need_a_table_with_a_long_name_to_test_rename_identifierss_by_id_to_regular_id:
      - i_need_a_column_with_a_long_name_to_test_rename_identifiers: 1
        i_need_a_column_with_a_long_name_but_is_different: 1
  - id: 2
    i_need_a_table_with_a_long_name_to_test_rename_identifierss_by_id_to_regular_id:
      - i_need_a_column_with_a_long_name_to_test_rename_identifiers: 2
        i_need_a_column_with_a_long_name_but_is_different: 2
|]

testInsert :: String -> SpecWith TestEnvironment
testInsert typ = do
  it "insert to regular table" $ \testEnvironment -> do
    let schemaName = Schema.getSchemaName testEnvironment

    shouldReturnYaml
      testEnvironment
      ( GraphqlEngine.postGraphqlYaml
          testEnvironment
          [interpolateYaml|
          query: |
            mutation MyMutation(
                $x: #{typ}
              ) {
              insert_#{schemaName}_regular_table_with_a_long_name_to_test_rename_identifiers
                ( objects:
                  { name: "bbb",
                    id: $x
                  }
                ) {
                affected_rows
                returning {
                  id
                  name
                }
              }
            }
          variables:
            x: 3
          |]
      )
      [interpolateYaml|
      data:
        insert_#{schemaName}_regular_table_with_a_long_name_to_test_rename_identifiers:
          affected_rows: 1
          returning:
          - id: 3
            name: bbb
      |]

  it "nested insert to long table name" $ \testEnvironment -> do
    let schemaName = Schema.getSchemaName testEnvironment

    shouldReturnYaml
      testEnvironment
      ( GraphqlEngine.postGraphqlYaml
          testEnvironment
          [interpolateYaml|
          query: |
            mutation MyMutation(
                $x: #{typ}
              ) {
              insert_#{schemaName}_regular_table_with_a_long_name_to_test_rename_identifiers
                ( objects:
                  { name: "bbb",
                    id: 4,
                    i_need_a_table_with_a_long_name_to_test_rename_identifierss_by_id_to_regular_id:
                      { data:
                        { i_need_a_column_with_a_long_name_but_is_different: 4,
                          i_need_a_column_with_a_long_name_to_test_rename_identifiers: $x,
                          id: 4
                        }
                      }
                  }
                ) {
                affected_rows
                returning {
                  id
                  name
                }
              }
            }
          variables:
            x: 4
          |]
      )
      [interpolateYaml|
      data:
        insert_#{schemaName}_regular_table_with_a_long_name_to_test_rename_identifiers:
          affected_rows: 2
          returning:
          - id: 4
            name: bbb
      |]

  it "nested insert to table with multiple relationships" $ \testEnvironment -> do
    let schemaName = Schema.getSchemaName testEnvironment

    shouldReturnYaml
      testEnvironment
      ( GraphqlEngine.postGraphqlYaml
          testEnvironment
          [interpolateYaml|
          query: |
            mutation MyMutation(
                $x: #{typ}
              ) {
              insert_#{schemaName}_table_with_multiple_relationships_and_long_name_2_test_rewrite
                ( objects:
                  { id: 5,
                    i_need_a_table_with_a_long_name_to_test_rename_identifiers_by_reference_for_longtable_id_to_id:
                      { data:
                        { i_need_a_column_with_a_long_name_but_is_different: 5,
                          i_need_a_column_with_a_long_name_to_test_rename_identifiers: $x,
                          id: 5,
                          regular_id: 1,
                        }
                      },
                    i_need_a_table_with_a_long_name_to_test_rename_identifiers2_by_reference_for_longtable2_id_to_id:
                      { data:
                        { i_need_a_column_with_a_long_name_but_is_different: 5,
                          i_need_a_column_with_a_long_name_to_test_rename_identifiers: $x,
                          id: 5
                        }
                      }
                  }
                ) {
                affected_rows
                returning {
                  id
                }
              }
            }
          variables:
            x: 5
          |]
      )
      [interpolateYaml|
      data:
        insert_#{schemaName}_table_with_multiple_relationships_and_long_name_2_test_rewrite:
          affected_rows: 3
          returning:
          - id: 5
      |]
