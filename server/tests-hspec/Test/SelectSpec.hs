{-# LANGUAGE QuasiQuotes #-}

-- | Select test for various queries
module Test.SelectSpec (spec) where

import Data.Aeson
  ( Value (..),
    object,
    (.=),
  )
import Harness.Backend.BigQuery qualified as Bigquery
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Postgres qualified as Postgres
import Harness.Backend.Sqlserver qualified as Sqlserver
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml
  ( shouldReturnOneOfYaml,
    shouldReturnYaml,
    yaml,
  )
import Harness.Test.Context qualified as Context
import Harness.Test.Schema
  ( BackendScalarType (..),
    BackendScalarValue (..),
    ScalarType (..),
    Table (..),
    defaultBackendScalarType,
    defaultBackendScalarValue,
    table,
  )
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (TestEnvironment)
import Harness.Yaml
import Test.Hspec (SpecWith, describe, it)
import Prelude

--------------------------------------------------------------------------------
-- Preamble

spec :: SpecWith TestEnvironment
spec =
  Context.run
    [ Context.Context
        { name = Context.Backend Context.Postgres,
          mkLocalTestEnvironment = Context.noLocalTestEnvironment,
          setup = Postgres.setup schema,
          teardown = Postgres.teardown schema,
          customOptions = Nothing
        },
      Context.Context
        { name = Context.Backend Context.Citus,
          mkLocalTestEnvironment = Context.noLocalTestEnvironment,
          setup = Citus.setup schema,
          teardown = Citus.teardown schema,
          customOptions = Nothing
        },
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

schema :: [Schema.Table]
schema = [author]

author :: Schema.Table
author =
  (table "author")
    { tableColumns =
        [ Schema.column "id" Schema.TInt,
          Schema.column "name" Schema.TStr,
          Schema.column "createdAt" dateTimeType
        ],
      tablePrimaryKey = ["id"],
      tableData =
        [ [ Schema.VInt 1,
            Schema.VStr "Author 1",
            Schema.VCustomValue $
              defaultBackendScalarValue
                { bsvMysql = Schema.quotedValue "2017-09-21 09:39:44",
                  bsvCitus = Schema.quotedValue "2017-09-21T09:39:44",
                  bsvMssql = Schema.quotedValue "2017-09-21T09:39:44Z",
                  bsvPostgres = Schema.quotedValue "2017-09-21T09:39:44",
                  bsvBigQuery = Schema.quotedValue "2017-09-21T09:39:44"
                }
          ],
          [ Schema.VInt 2,
            Schema.VStr "Author 2",
            Schema.VCustomValue $
              defaultBackendScalarValue
                { bsvMysql = Schema.quotedValue "2017-09-21 09:50:44",
                  bsvCitus = Schema.quotedValue "2017-09-21T09:50:44",
                  bsvMssql = Schema.quotedValue "2017-09-21T09:50:44Z",
                  bsvPostgres = Schema.quotedValue "2017-09-21T09:50:44",
                  bsvBigQuery = Schema.quotedValue "2017-09-21T09:50:44"
                }
          ]
        ]
    }
  where
    dateTimeType :: ScalarType
    dateTimeType =
      TCustomType $
        defaultBackendScalarType
          { bstMysql = Just "DATETIME",
            bstMssql = Just "DATETIME",
            bstCitus = Just "TIMESTAMP",
            bstPostgres = Just "TIMESTAMP",
            bstBigQuery = Just "DATETIME"
          }

tests :: Context.Options -> SpecWith TestEnvironment
tests opts = describe "SelectSpec" $ do
  -- Equivalent python suite: test_select_query_author_quoted_col
  -- https://github.com/hasura/graphql-engine/blob/369d1ab2f119634b0e27e9ed353fa3d08c22d3fb/server/tests-py/test_graphql_queries.py#L259
  it "works with simple object query" $ \testEnvironment -> do
    let authorOne, authorTwo :: Value
        authorOne =
          object
            [ "id" .= (1 :: Int),
              "name" .= String "Author 1",
              "createdAt" .= String "2017-09-21T09:39:44"
            ]
        authorTwo =
          object
            [ "id" .= (2 :: Int),
              "name" .= String "Author 2",
              "createdAt" .= String "2017-09-21T09:50:44"
            ]
    shouldReturnOneOfYaml
      opts
      ( GraphqlEngine.postGraphql
          testEnvironment
          [graphql|
query {
  hasura_author {
    id
    name
    createdAt
  }
}|]
      )
      (combinationsObject responseAuthor (map fromObject [authorOne, authorTwo]))

  -- Equivalent python suite: test_select_placeholder_err
  -- https://github.com/hasura/graphql-engine/blob/369d1ab2f119634b0e27e9ed353fa3d08c22d3fb/server/tests-py/test_graphql_queries.py#L307
  it "fails when placehold query is used with schemas present" $ \testEnvironment ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          testEnvironment
          [graphql|
query {
  no_queries_available
}
|]
      )
      [yaml|
errors:
- extensions:
    code: validation-failed
    path: $.selectionSet.no_queries_available
  message: |-
    field "no_queries_available" not found in type: 'query_root'
|]

responseAuthor :: Value -> Value
responseAuthor authors =
  [yaml|
data:
 hasura_author: *authors
|]
