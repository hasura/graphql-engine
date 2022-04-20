{-# LANGUAGE QuasiQuotes #-}

-- | Testing custom field names.
--   See the main hasura documentation for more information.
--
--   - Postgres: https://hasura.io/docs/latest/graphql/core/databases/postgres/schema/custom-field-names.html
--   - MSSQL: https://hasura.io/docs/latest/graphql/core/databases/ms-sql-server/schema/custom-field-names.html
module Test.CustomFieldNamesSpec (spec) where

import Harness.Backend.Postgres qualified as Postgres
import Harness.Backend.Sqlserver qualified as Sqlserver
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (shouldReturnYaml, yaml)
import Harness.Test.Context qualified as Context
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
  it "Delete respects custom names" $ \testEnvironment ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          testEnvironment
          [graphql|
mutation author {
  delete_hasura_author(
    where: {Id: {_eq: 1}}
  ) {
    returning {
      Id
      Name
    }
  }
}
|]
      )
      [yaml|
data:
  delete_hasura_author:
    returning:
      - Id: 1
        Name: 'Mercer'
|]

  it "Update respects custom names" $ \testEnvironment ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          testEnvironment
          [graphql|
mutation author {
  update_hasura_author(
    where: {Id: {_eq: 2}}
    _set: { Name: "Johnson" }
  ) {
    returning {
      Id
      Name
    }
  }
}
|]
      )
      [yaml|
data:
  update_hasura_author:
    returning:
      - Id: 2
        Name: 'Johnson'
|]

  it "Insert respects custom names" $ \testEnvironment ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          testEnvironment
          [graphql|
mutation author {
  insert_hasura_author(objects:
    { Id: 3
      Name: "Jaffe"
    }) {
    returning {
      Id
      Name
    }
  }
}
|]
      )
      [yaml|
data:
  insert_hasura_author:
    returning:
      - Id: 3
        Name: 'Jaffe'
|]

--------------------------------------------------------------------------------

-- * Backend

-- ** Schema

schema :: [Schema.Table]
schema =
  [ Schema.Table
      { tableName = "author",
        tableColumns =
          [ Schema.column "AuthorId" Schema.TInt,
            Schema.column "AuthorName" Schema.TStr
          ],
        tablePrimaryKey = ["AuthorId"],
        tableReferences = [],
        tableData =
          [ [Schema.VInt 1, Schema.VStr "Mercer"],
            [Schema.VInt 2, Schema.VStr "Ray"]
          ]
      }
  ]

--------------------------------------------------------------------------------

-- ** Postgres backend

postgresSetup :: (TestEnvironment, ()) -> IO ()
postgresSetup (testEnvironment, localTestEnvironment) = do
  Postgres.setup schema (testEnvironment, localTestEnvironment)
  postgresCreateCustomNames testEnvironment

postgresCreateCustomNames :: TestEnvironment -> IO ()
postgresCreateCustomNames testEnvironment = do
  let source = Context.defaultBackendTypeString Context.Postgres
   in GraphqlEngine.postMetadata_
        testEnvironment
        [yaml|
type: pg_set_table_customization
args:
  source: *source
  table:
    schema: hasura
    name: author
  configuration:
    custom_column_names:
      "AuthorId": "Id"
      "AuthorName": "Name"
|]

--------------------------------------------------------------------------------

-- ** SQL Server backend

sqlserverSetup :: (TestEnvironment, ()) -> IO ()
sqlserverSetup (testEnvironment, localTestEnvironment) = do
  Sqlserver.setup schema (testEnvironment, localTestEnvironment)
  sqlserverCreateCustomNames testEnvironment

sqlserverCreateCustomNames :: TestEnvironment -> IO ()
sqlserverCreateCustomNames testEnvironment = do
  let source = Context.defaultBackendTypeString Context.SQLServer
   in GraphqlEngine.postMetadata_
        testEnvironment
        [yaml|
type: mssql_set_table_customization
args:
  source: *source
  table:
    schema: hasura
    name: author
  configuration:
    custom_column_names:
      "AuthorId": "Id"
      "AuthorName": "Name"
|]
