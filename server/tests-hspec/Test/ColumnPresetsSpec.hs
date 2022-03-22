{-# LANGUAGE QuasiQuotes #-}

-- | Testing column presets.
--   See the main hasura documentation for more information.
--
--   - Postgres: https://hasura.io/docs/latest/graphql/core/databases/postgres/schema/default-values/column-presets.html
--   - MSSQL: https://hasura.io/docs/latest/graphql/core/databases/ms-sql-server/schema/default-values/mssql-column-presets.html
module Test.ColumnPresetsSpec (spec) where

import Harness.Backend.Postgres qualified as Postgres
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
          teardown = Sqlserver.teardown schema,
          customOptions = Nothing
        },
      Context.Context
        { name = Context.Backend Context.Postgres,
          mkLocalState = Context.noLocalState,
          setup = postgresSetup,
          teardown = Postgres.teardown schema,
          customOptions = Nothing
        }
    ]
    tests

--------------------------------------------------------------------------------

-- * Tests

tests :: Context.Options -> SpecWith State
tests opts = do
  ----------------------------------------
  it "admin role is unaffected by column presets" $ \state ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          state
          [graphql|
mutation author {
  insert_hasura_author(objects:
    { name: "Author 1"
      uuid: "36a6257b-1111-1111-2222-c1b7a7997087"
      company: "arusah"
    }) {
    returning {
      uuid
      name
      company
    }
  }
}
|]
      )
      [yaml|
data:
  insert_hasura_author:
    returning:
    - uuid: '36a6257b-1111-1111-2222-c1b7a7997087'
      name: 'Author 1'
      company: 'arusah'
|]

  ----------------------------------------
  it "applies the column presets to the 'author id' and 'company' columns" $ \state ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphqlWithHeaders
          state
          [ ("X-Hasura-Role", "user"),
            ("X-Hasura-User-Id", "36a6257b-1111-1111-1111-c1b7a7997087")
          ]
          [graphql|
mutation author {
  insert_hasura_author(objects: { name: "Author 2" }) {
    returning {
      uuid
      name
      company
    }
  }
}
|]
      )
      [yaml|
data:
  insert_hasura_author:
    returning:
    - uuid: '36a6257b-1111-1111-1111-c1b7a7997087'
      name: 'Author 2'
      company: 'hasura'
|]

  ----------------------------------------
  it "Columns with session variables presets defined are not part of the schema" $ \state ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphqlWithHeaders
          state
          [ ("X-Hasura-Role", "user"),
            ("X-Hasura-User-Id", "36a6257b-1111-1111-1111-c1b7a7997087")
          ]
          [graphql|
mutation author {
  insert_hasura_author(objects:
    { name: "Author 3"
      uuid: "36a6257b-1111-1111-2222-c1b7a7997087"
    }) {
    returning {
      uuid
      name
      company
    }
  }
}
|]
      )
      [yaml|
errors:
- extensions:
    path: $.selectionSet.insert_hasura_author.args.objects[0].uuid
    code: validation-failed
  message: 'field "uuid" not found in type: ''hasura_author_insert_input'''
|]

  ----------------------------------------
  it "Columns with static presets defined are not part of the schema" $ \state ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphqlWithHeaders
          state
          [ ("X-Hasura-Role", "user"),
            ("X-Hasura-User-Id", "36a6257b-1111-1111-1111-c1b7a7997087")
          ]
          [graphql|
mutation author {
  insert_hasura_author(objects: { name: "Author 4" company: "arusah" }) {
    returning {
      uuid
      name
      company
    }
  }
}
|]
      )
      [yaml|
errors:
- extensions:
    path: $.selectionSet.insert_hasura_author.args.objects[0].company
    code: validation-failed
  message: 'field "company" not found in type: ''hasura_author_insert_input'''
|]

--------------------------------------------------------------------------------

-- * Backend

-- ** Schema

schema :: [Schema.Table]
schema =
  [ Schema.Table
      { tableName = "author",
        tableColumns =
          [ Schema.column "uuid" Schema.TStr,
            Schema.column "name" Schema.TStr,
            Schema.column "company" Schema.TStr
          ],
        tablePrimaryKey = ["uuid"],
        tableReferences = [],
        tableData = []
      }
  ]

--------------------------------------------------------------------------------

-- ** Postgres backend

postgresSetup :: (State, ()) -> IO ()
postgresSetup (state, localState) = do
  Postgres.setup schema (state, localState)
  postgresCreatePermissions state

postgresCreatePermissions :: State -> IO ()
postgresCreatePermissions state = do
  GraphqlEngine.postMetadata_
    state
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
      uuid: X-Hasura-User-Id
    columns: '*'
|]
  GraphqlEngine.postMetadata_
    state
    [yaml|
type: pg_create_insert_permission
args:
  source: postgres
  table:
    schema: hasura
    name: author
  role: user
  permission:
    check: {}
    set:
      uuid: X-Hasura-User-Id
      company: hasura
    columns: '*'
|]

--------------------------------------------------------------------------------

-- ** SQL Server backend

sqlserverSetup :: (State, ()) -> IO ()
sqlserverSetup (state, localState) = do
  Sqlserver.setup schema (state, localState)
  sqlserverCreatePermissions state

sqlserverCreatePermissions :: State -> IO ()
sqlserverCreatePermissions state = do
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
  GraphqlEngine.postMetadata_
    state
    [yaml|
type: mssql_create_insert_permission
args:
  source: mssql
  table:
    schema: hasura
    name: author
  role: user
  permission:
    check: {}
    set:
      uuid: X-Hasura-User-Id
      company: hasura
    columns: '*'
|]
