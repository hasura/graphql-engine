{-# LANGUAGE QuasiQuotes #-}

-- | Test long identifiers
--
--   See "Hasura.Backend.Postgres.SQL.RenameIdentifiers" for more details.
module Test.LongIdentifiersSpec (spec) where

import Harness.Backend.BigQuery qualified as Bigquery
import Harness.Backend.Postgres qualified as Postgres
import Harness.Backend.Sqlserver qualified as Sqlserver
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (shouldReturnYaml, yaml)
import Harness.Test.Context qualified as Context
import Harness.Test.Schema (Table (..), table)
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (TestEnvironment)
import Test.Hspec (SpecWith, it)
import Prelude

--------------------------------------------------------------------------------
-- Preamble

spec :: SpecWith TestEnvironment
spec =
  Context.run
    [ -- Create table fails currently becasuse we postfix table names for some reason
      -- which makes the valid table name go over the limit
      --
      -- Context.Context
      --   { name = Context.Backend Context.MySQL,
      --     mkLocalTestEnvironment = Context.noLocalTestEnvironment,
      --     setup = Mysql.setup schema,
      --     teardown = Mysql.teardown schema,
      --     customOptions = Nothing
      --   },
      Context.Context
        { name = Context.Backend Context.Postgres,
          mkLocalTestEnvironment = Context.noLocalTestEnvironment,
          setup = Postgres.setup schema,
          teardown = Postgres.teardown schema,
          customOptions = Nothing
        },
      -- Create table fails currently on a weird error:
      -- > relation "i_need_a_table_with_a_long_na_i_need_a_column_with_a_long_n_seq" already exists
      --
      -- Context.Context
      --   { name = Context.Backend Context.Citus,
      --     mkLocalTestEnvironment = Context.noLocalTestEnvironment,
      --     setup = Citus.setup schema,
      --     teardown = Citus.teardown schema,
      --     customOptions = Nothing
      --   },
      Context.Context
        { name = Context.Backend Context.SQLServer,
          mkLocalTestEnvironment = Context.noLocalTestEnvironment,
          setup = Sqlserver.setup schema,
          teardown = Sqlserver.teardown schema,
          customOptions = Nothing
        },
      Context.Context
        { name = Context.Backend Context.BigQuery,
          mkLocalTestEnvironment = Context.noLocalTestEnvironment,
          setup = Bigquery.setup schema,
          teardown = Bigquery.teardown schema,
          customOptions =
            Just $
              Context.Options
                { stringifyNumbers = True
                }
        }
    ]
    tests

--------------------------------------------------------------------------------
-- Schema

schema :: [Schema.Table]
schema = [regular, longtable]

regular :: Schema.Table
regular =
  (table "regular")
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
        [ Schema.Reference
            { referenceLocalColumn = "regular_id",
              referenceTargetTable = "regular",
              referenceTargetColumn = "id"
            }
        ],
      tableData =
        [ [Schema.VInt 1, Schema.VInt 1, Schema.VInt 1, Schema.VInt 1],
          [Schema.VInt 2, Schema.VInt 2, Schema.VInt 2, Schema.VInt 2]
        ]
    }

--------------------------------------------------------------------------------
-- Tests

tests :: Context.Options -> SpecWith TestEnvironment
tests opts = do
  it "select long table" $ \testEnvironment ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          testEnvironment
          [graphql|
query {
  hasura_i_need_a_table_with_a_long_name_to_test_rename_identifiers(order_by:[{id:asc}]) {
    id
  }
}
|]
      )
      [yaml|
data:
  hasura_i_need_a_table_with_a_long_name_to_test_rename_identifiers:
  - id: 1
  - id: 2
|]

  it "select long column" $ \testEnvironment ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          testEnvironment
          [yaml|
query {
  hasura_i_need_a_table_with_a_long_name_to_test_rename_identifiers(order_by:[{i_need_a_column_with_a_long_name_to_test_rename_identifiers:asc, i_need_a_column_with_a_long_name_but_is_different:asc}]) {
    id
    regular_id
    i_need_a_column_with_a_long_name_to_test_rename_identifiers
  }
}
|]
      )
      [yaml|
data:
  hasura_i_need_a_table_with_a_long_name_to_test_rename_identifiers:
  - id: 1
    regular_id: 1
    i_need_a_column_with_a_long_name_to_test_rename_identifiers: 1
  - id: 2
    regular_id: 2
    i_need_a_column_with_a_long_name_to_test_rename_identifiers: 2
|]

  it "select long column via array relationship" $ \testEnvironment ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          testEnvironment
          [yaml|
query {
  hasura_regular(order_by:[{id:asc}]) {
    id
    i_need_a_table_with_a_long_name_to_test_rename_identifierss_by_id_to_regular_id(order_by:[{i_need_a_column_with_a_long_name_to_test_rename_identifiers:asc, i_need_a_column_with_a_long_name_but_is_different:asc}]) {
      i_need_a_column_with_a_long_name_to_test_rename_identifiers
      i_need_a_column_with_a_long_name_but_is_different
    }
  }
}
|]
      )
      [yaml|
data:
  hasura_regular:
  - id: 1
    i_need_a_table_with_a_long_name_to_test_rename_identifierss_by_id_to_regular_id:
      - i_need_a_column_with_a_long_name_to_test_rename_identifiers: 1
        i_need_a_column_with_a_long_name_but_is_different: 1
  - id: 2
    i_need_a_table_with_a_long_name_to_test_rename_identifierss_by_id_to_regular_id:
      - i_need_a_column_with_a_long_name_to_test_rename_identifiers: 2
        i_need_a_column_with_a_long_name_but_is_different: 2
|]
