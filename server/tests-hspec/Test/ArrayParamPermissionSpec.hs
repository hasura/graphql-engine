{-# LANGUAGE QuasiQuotes #-}

-- | Test case for permissions with array params
-- https://github.com/hasura/graphql-engine-mono/pull/4651
module Test.ArrayParamPermissionSpec (spec) where

import Data.List.NonEmpty qualified as NE
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (shouldReturnYaml, yaml)
import Harness.Test.Context qualified as Context
import Harness.Test.Schema (Table (..))
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (TestEnvironment)
import Hasura.Prelude
import Test.Hspec (SpecWith, it)

--------------------------------------------------------------------------------

-- ** Preamble

spec :: SpecWith TestEnvironment
spec =
  Context.run
    ( NE.fromList
        [ Context.Context
            { name = Context.Backend Context.Postgres,
              mkLocalTestEnvironment = Context.noLocalTestEnvironment,
              setup = postgresSetup,
              teardown = postgresTeardown,
              customOptions = Nothing
            }
        ]
    )
    tests

--------------------------------------------------------------------------------

-- ** Schema

schema :: [Schema.Table]
schema = [author]

author :: Schema.Table
author =
  (Schema.table "author")
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

--------------------------------------------------------------------------------

-- ** Setup and teardown

postgresSetup :: (TestEnvironment, ()) -> IO ()
postgresSetup (testEnvironment, localTestEnvironment) = do
  Postgres.setup schema (testEnvironment, localTestEnvironment)
  postgresCreatePermissions testEnvironment

postgresCreatePermissions :: TestEnvironment -> IO ()
postgresCreatePermissions testEnvironment = do
  GraphqlEngine.postMetadata_
    testEnvironment
    [yaml|
type: pg_create_select_permission
args:
  source: postgres
  table:
    schema: hasura
    name: author
  role: user
  permission:
    filter:
      id:
        _in: X-Hasura-Allowed-Ids
    columns: '*'
|]

postgresTeardown :: (TestEnvironment, ()) -> IO ()
postgresTeardown (testEnvironment, ()) = do
  -- teardown permissions
  GraphqlEngine.postMetadata_ testEnvironment $
    [yaml|
type: bulk
args:
- type: pg_drop_select_permission
  args:
    source: postgres
    table:
      schema: hasura
      name: author
    role: user
|]

  -- and then rest of the teardown
  Postgres.teardown schema (testEnvironment, ())

--------------------------------------------------------------------------------

-- * Tests

tests :: Context.Options -> SpecWith TestEnvironment
tests opts = do
  it "non-matching X-Hasura-Allowed-Ids should return no data" $ \testEnvironment -> do
    let userHeaders = [("X-Hasura-Role", "user"), ("X-Hasura-Allowed-Ids", "{}")]
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphqlWithHeaders
          testEnvironment
          userHeaders
          [graphql|
query {
    hasura_author {
        id
        name
    }
}
|]
      )
      [yaml|
data:
  hasura_author: []
|]

  it "matching X-Hasura-Allowed-Ids should return data" $ \testEnvironment -> do
    let userHeaders = [("X-Hasura-Role", "user"), ("X-Hasura-Allowed-Ids", "{1,2,3}")]
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphqlWithHeaders
          testEnvironment
          userHeaders
          [graphql|
query {
    hasura_author {
        id
        name
    }
}
|]
      )
      [yaml|
data:
  hasura_author:
  - name: Author 1
    id: 1
  - name: Author 2
    id: 2
|]
