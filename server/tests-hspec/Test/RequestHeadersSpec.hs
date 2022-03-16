{-# LANGUAGE QuasiQuotes #-}

-- | Tests related to request headers
module Test.RequestHeadersSpec (spec) where

import Harness.Backend.Sqlserver qualified as Sqlserver
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (shouldReturnYaml, yaml)
import Harness.State (State)
import Harness.Test.Context qualified as Context
import Harness.Test.Schema qualified as Schema
import Test.Hspec (SpecWith, it)
import Prelude

--------------------------------------------------------------------------------

-- * Preamble

spec :: SpecWith State
spec =
  Context.run
    [ Context.Context
        { name = Context.Backend Context.SQLServer,
          mkLocalState = Context.noLocalState,
          setup = sqlserverSetup,
          teardown = sqlserverTeardown,
          customOptions = Nothing
        }
    ]
    tests

--------------------------------------------------------------------------------
-- Schema

schema :: [Schema.Table]
schema = [author]

author :: Schema.Table
author =
  Schema.Table
    "author"
    [ Schema.column "uuid" Schema.TVarchar50,
      Schema.column "name" Schema.TStr
    ]
    ["uuid"]
    []
    [ [Schema.VStr "36a6257b-08bb-45ef-a5cf-c1b7a7997087", Schema.VStr "Author 1"],
      [Schema.VStr "36a6257b-08bb-45ef-a5cf-c1b7a7", Schema.VStr "Author 2"]
    ]

--------------------------------------------------------------------------------
-- Setup and teardown override

sqlserverSetup :: (State, ()) -> IO ()
sqlserverSetup (state, _) = do
  Sqlserver.setup schema (state, ())
  -- create permissions
  GraphqlEngine.postMetadata_
    state
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

sqlserverTeardown :: (State, ()) -> IO ()
sqlserverTeardown (state, _) = do
  -- drop permissions
  GraphqlEngine.postMetadata_
    state
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
  Sqlserver.teardown schema (state, ())

--------------------------------------------------------------------------------
-- Tests

tests :: Context.Options -> SpecWith State
tests opts = do
  -- See https://github.com/hasura/graphql-engine/issues/8158
  it "session variable string values are not truncated to default (30) length" $ \state ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphqlWithHeaders
          state
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
