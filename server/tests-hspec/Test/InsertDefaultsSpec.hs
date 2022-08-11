{-# LANGUAGE QuasiQuotes #-}

-- | Test insert with default values
module Test.InsertDefaultsSpec (spec) where

import Data.List.NonEmpty qualified as NE
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Postgres qualified as Postgres
import Harness.Backend.Sqlserver qualified as Sqlserver
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (yaml)
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.Schema (Table (..), table)
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (TestEnvironment)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, it)

--------------------------------------------------------------------------------

-- ** Preamble

spec :: SpecWith TestEnvironment
spec = do
  Fixture.run (NE.fromList $ [postgresFixture, citusFixture, mssqlFixture]) commonTests
  Fixture.run (NE.fromList $ [postgresFixture, citusFixture]) postgresTests
  Fixture.run (NE.fromList $ [mssqlFixture]) mssqlTests
  where
    postgresFixture =
      (Fixture.fixture $ Fixture.Backend Fixture.Postgres)
        { Fixture.setupTeardown = \(testEnv, _) ->
            [Postgres.setupTablesAction schema testEnv]
        }
    citusFixture =
      (Fixture.fixture $ Fixture.Backend Fixture.Citus)
        { Fixture.setupTeardown = \(testEnv, _) ->
            [Citus.setupTablesAction schema testEnv]
        }
    mssqlFixture =
      (Fixture.fixture $ Fixture.Backend Fixture.SQLServer)
        { Fixture.setupTeardown = \(testEnv, _) ->
            [Sqlserver.setupTablesAction schema testEnv]
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

commonTests :: Fixture.Options -> SpecWith TestEnvironment
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

postgresTests :: Fixture.Options -> SpecWith TestEnvironment
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

mssqlTests :: Fixture.Options -> SpecWith TestEnvironment
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
