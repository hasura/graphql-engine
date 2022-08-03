{-# LANGUAGE QuasiQuotes #-}

-- | Tests related to request headers
module Test.RequestHeadersSpec (spec) where

import Data.List.NonEmpty qualified as NE
import Harness.Backend.Sqlserver qualified as Sqlserver
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (shouldReturnYaml, yaml)
import Harness.Test.Context qualified as Context
import Harness.Test.Schema
  ( BackendScalarType (..),
    Table (..),
    defaultBackendScalarType,
    table,
  )
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (TestEnvironment)
import Hasura.Prelude
import Test.Hspec (SpecWith, it)

--------------------------------------------------------------------------------

-- * Preamble

spec :: SpecWith TestEnvironment
spec =
  Context.run
    ( NE.fromList
        [ Context.Context
            { name = Context.Backend Context.SQLServer,
              mkLocalTestEnvironment = Context.noLocalTestEnvironment,
              setup = sqlserverSetup,
              teardown = sqlserverTeardown,
              customOptions = Nothing
            }
        ]
    )
    tests

--------------------------------------------------------------------------------
-- Schema

schema :: [Schema.Table]
schema = [author]

author :: Table
author =
  (table "author")
    { tableColumns =
        [ Schema.column
            "uuid"
            ( Schema.TCustomType $
                defaultBackendScalarType
                  { bstMssql = Just "VARCHAR(50)"
                  }
            ),
          Schema.column "name" Schema.TStr
        ],
      tablePrimaryKey = ["uuid"],
      tableData =
        [ [Schema.VStr "36a6257b-08bb-45ef-a5cf-c1b7a7997087", Schema.VStr "Author 1"],
          [Schema.VStr "36a6257b-08bb-45ef-a5cf-c1b7a7", Schema.VStr "Author 2"]
        ]
    }

--------------------------------------------------------------------------------
-- Setup and teardown override

sqlserverSetup :: (TestEnvironment, ()) -> IO ()
sqlserverSetup (testEnvironment, _) = do
  Sqlserver.setup schema (testEnvironment, ())
  -- create permissions
  GraphqlEngine.postMetadata_
    testEnvironment
    [yaml|
type: mssql_create_select_permission
args:
  source: mssql
  table:
    schema: hasura
    name: author
  role: user
  permission:
    filter:
      uuid: X-Hasura-User-Id
    columns: '*'
|]

sqlserverTeardown :: (TestEnvironment, ()) -> IO ()
sqlserverTeardown (testEnvironment, _) = do
  -- drop permissions
  GraphqlEngine.postMetadata_
    testEnvironment
    [yaml|
type: mssql_drop_select_permission
args:
  source: mssql
  table:
    schema: hasura
    name: author
  role: user
|]
  -- rest of the teardown
  Sqlserver.teardown schema (testEnvironment, ())

--------------------------------------------------------------------------------
-- Tests

tests :: Context.Options -> SpecWith TestEnvironment
tests opts = do
  -- See https://github.com/hasura/graphql-engine/issues/8158
  it "session variable string values are not truncated to default (30) length" $ \testEnvironment ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphqlWithHeaders
          testEnvironment
          [ ("X-Hasura-Role", "user"),
            ("X-Hasura-User-Id", "36a6257b-08bb-45ef-a5cf-c1b7a7997087")
          ]
          [graphql|
query {
  hasura_author {
    name
    uuid
  }
}
|]
      )
      [yaml|
data:
  hasura_author:
  - name: 'Author 1'
    uuid: '36a6257b-08bb-45ef-a5cf-c1b7a7997087'
|]
