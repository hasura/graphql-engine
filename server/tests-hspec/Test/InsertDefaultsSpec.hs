{-# LANGUAGE QuasiQuotes #-}

-- | Test insert with default values
module Test.InsertDefaultsSpec (spec) where

import Harness.Backend.Citus qualified as Citus
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

-- ** Preamble

spec :: SpecWith TestEnvironment
spec = do
  Context.run
    [ postgresContext,
      citusContext,
      mssqlContext
    ]
    commonTests

  Context.run [postgresContext, citusContext] postgresTests
  Context.run [mssqlContext] mssqlTests
  where
    postgresContext =
      Context.Context
        { name = Context.Backend Context.Postgres,
          mkLocalTestEnvironment = Context.noLocalTestEnvironment,
          setup = Postgres.setup schema,
          teardown = Postgres.teardown schema,
          customOptions = Nothing
        }
    citusContext =
      Context.Context
        { name = Context.Backend Context.Citus,
          mkLocalTestEnvironment = Context.noLocalTestEnvironment,
          setup = Citus.setup schema,
          teardown = Citus.teardown schema,
          customOptions = Nothing
        }
    mssqlContext =
      Context.Context
        { name = Context.Backend Context.SQLServer,
          mkLocalTestEnvironment = Context.noLocalTestEnvironment,
          setup = Sqlserver.setup schema,
          teardown = Sqlserver.teardown schema,
          customOptions = Nothing
        }

--------------------------------------------------------------------------------

-- ** Schema

schema :: [Schema.Table]
schema =
  [ alldefaults,
    somedefaults,
    withrelationship
  ]

alldefaults :: Schema.Table
alldefaults =
  (table "alldefaults")
    { tableColumns =
        [ Schema.column "id" Schema.defaultSerialType,
          Schema.column "dt" defaultDateTimeType
        ],
      tablePrimaryKey = ["id"]
    }

somedefaults :: Schema.Table
somedefaults =
  (table "somedefaults")
    { tableColumns =
        [ Schema.column "id" Schema.defaultSerialType,
          Schema.column "dt" defaultDateTimeType,
          Schema.column "name" Schema.TStr
        ],
      tablePrimaryKey = ["name"]
    }

withrelationship :: Schema.Table
withrelationship =
  (table "withrelationship")
    { tableColumns =
        [ Schema.column "id" Schema.defaultSerialType,
          Schema.column "nickname" Schema.TStr,
          Schema.column "time_id" Schema.TInt
        ],
      tablePrimaryKey = ["nickname"],
      tableReferences = [Schema.Reference "time_id" "alldefaults" "id"]
    }

defaultDateTimeType :: Schema.ScalarType
defaultDateTimeType =
  Schema.TCustomType $
    Schema.defaultBackendScalarType
      { Schema.bstMysql = Nothing,
        Schema.bstMssql = Just "DATETIME DEFAULT GETDATE()",
        Schema.bstCitus = Just "TIMESTAMP DEFAULT NOW()",
        Schema.bstPostgres = Just "TIMESTAMP DEFAULT NOW()",
        Schema.bstBigQuery = Nothing
      }

--------------------------------------------------------------------------------

-- * Tests

commonTests :: Context.Options -> SpecWith TestEnvironment
commonTests opts = do
  it "Insert empty object with default values" $ \testEnvironment ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          testEnvironment
          [graphql|
mutation {
  insert_hasura_alldefaults(
    objects:[{}]
  ){
    affected_rows
  }
}
|]
      )
      [yaml|
data:
  insert_hasura_alldefaults:
    affected_rows: 1
|]

  it "Insert multiple empty objects with default values" $ \testEnvironment ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          testEnvironment
          [graphql|
mutation {
  insert_hasura_alldefaults(
    objects:[{}, {}, {}]
  ){
    affected_rows
  }
}
|]
      )
      [yaml|
data:
  insert_hasura_alldefaults:
    affected_rows: 3
|]

  it "Insert simple object with default values" $ \testEnvironment ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          testEnvironment
          [graphql|
mutation {
  insert_hasura_somedefaults(
    objects:[{ name: "a" }]
  ){
    affected_rows
    returning {
      id
      name
    }
  }
}
|]
      )
      [yaml|
data:
  insert_hasura_somedefaults:
    affected_rows: 1
    returning:
    - id: 1
      name: "a"
|]

postgresTests :: Context.Options -> SpecWith TestEnvironment
postgresTests opts = do
  it "Upsert simple object with default values - check empty constraints" $ \testEnvironment ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          testEnvironment
          [graphql|
mutation {
  insert_hasura_somedefaults(
    objects: [{ name: "a" }]
    on_conflict: {
      constraint: somedefaults_pkey,
      update_columns: []
    }
  ){
    affected_rows
    returning {
      id
    }
  }
}
|]
      )
      [yaml|
data:
  insert_hasura_somedefaults:
    affected_rows: 1
    returning:
    - id: 1
|]

  it "Upsert simple object with default values - check conflict doesn't update" $ \testEnvironment ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          testEnvironment
          [graphql|
mutation {
  insert_hasura_somedefaults(
    objects: [{ name: "a" }]
    on_conflict: {
      constraint: somedefaults_pkey,
      update_columns: []
    }
  ){
    affected_rows
    returning {
      id
    }
  }
}
|]
      )
      [yaml|
data:
  insert_hasura_somedefaults:
    affected_rows: 0
    returning: []
|]

  it "Nested insert with empty object" $ \testEnvironment ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          testEnvironment
          [graphql|
mutation {
  insert_hasura_withrelationship(
    objects: [{ nickname: "the a", alldefaults_by_time_id_to_id: {data: {} } }]
    on_conflict: {
      constraint: withrelationship_pkey,
      update_columns: []
    }
  ){
    affected_rows
    returning {
      id
      nickname
      alldefaults_by_time_id_to_id {
        id
      }
    }
  }
}
|]
      )
      [yaml|
data:
  insert_hasura_withrelationship:
    affected_rows: 2
    returning:
    - id: 1
      nickname: "the a"
      alldefaults_by_time_id_to_id:
        id: 1
|]

mssqlTests :: Context.Options -> SpecWith TestEnvironment
mssqlTests opts = do
  it "Upsert simple object with default values - check empty if_matched" $ \testEnvironment ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          testEnvironment
          [graphql|
mutation {
  insert_hasura_somedefaults(
    objects: [{ name: "a" }]
    if_matched: {
      match_columns: [],
      update_columns: []
    }
  ){
    affected_rows
    returning {
      id
    }
  }
}
|]
      )
      [yaml|
data:
  insert_hasura_somedefaults:
    affected_rows: 1
    returning:
    - id: 1
|]

  it "Upsert simple object with default values - check conflict doesn't update" $ \testEnvironment ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          testEnvironment
          [graphql|
mutation {
  insert_hasura_somedefaults(
    objects: [{ name: "a" }]
    if_matched: {
      match_columns: name,
      update_columns: []
    }
  ){
    affected_rows
    returning {
      id
    }
  }
}
|]
      )
      [yaml|
data:
  insert_hasura_somedefaults:
    affected_rows: 0
    returning: []
|]
