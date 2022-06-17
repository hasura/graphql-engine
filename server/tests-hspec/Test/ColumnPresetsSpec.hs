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
import Harness.Test.Context qualified as Context
import Harness.Test.Schema (Table (..), table)
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (TestEnvironment)
import Test.Hspec (SpecWith, it)
import Prelude

--------------------------------------------------------------------------------

-- * Preamble

spec :: SpecWith TestEnvironment
spec =
  Context.run
    [ Context.Context
        { name = Context.Backend Context.SQLServer,
          mkLocalTestEnvironment = Context.noLocalTestEnvironment,
          setup = sqlserverSetup,
          teardown = Sqlserver.teardown schema,
          customOptions = Nothing
        },
      Context.Context
        { name = Context.Backend Context.Postgres,
          mkLocalTestEnvironment = Context.noLocalTestEnvironment,
          setup = postgresSetup,
          teardown = Postgres.teardown schema,
          customOptions = Nothing
        }
    ]
    tests

--------------------------------------------------------------------------------

-- * Tests

tests :: Context.Options -> SpecWith TestEnvironment
tests opts = do
  ----------------------------------------
  it "admin role is unaffected by column presets" $ \testEnvironment ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          testEnvironment
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
  it "applies the column presets to the 'author id' and 'company' columns" $ \testEnvironment ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphqlWithHeaders
          testEnvironment
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
  it "Columns with session variables presets defined are not part of the schema" $ \testEnvironment ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphqlWithHeaders
          testEnvironment
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
  it "Columns with static presets defined are not part of the schema" $ \testEnvironment ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphqlWithHeaders
          testEnvironment
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
  [ (table "author")
      { tableColumns =
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
      uuid: X-Hasura-User-Id
    columns: '*'
|]
  GraphqlEngine.postMetadata_
    testEnvironment
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

sqlserverSetup :: (TestEnvironment, ()) -> IO ()
sqlserverSetup (testEnvironment, localTestEnvironment) = do
  Sqlserver.setup schema (testEnvironment, localTestEnvironment)
  sqlserverCreatePermissions testEnvironment

sqlserverCreatePermissions :: TestEnvironment -> IO ()
sqlserverCreatePermissions testEnvironment = do
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
  GraphqlEngine.postMetadata_
    testEnvironment
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
