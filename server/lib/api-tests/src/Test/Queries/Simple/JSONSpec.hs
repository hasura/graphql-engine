{-# LANGUAGE QuasiQuotes #-}

-- |
-- Simple JSON queries on single objects.
--
-- https://hasura.io/docs/latest/queries/postgres/simple-object-queries/#fetch-list-of-objects
-- https://hasura.io/docs/latest/queries/ms-sql-server/simple-object-queries/#fetch-list-of-objects
-- https://hasura.io/docs/latest/queries/bigquery/simple-object-queries/#fetch-list-of-objects
module Test.Queries.Simple.JSONSpec (spec) where

import Data.Aeson (Value)
import Data.List.NonEmpty qualified as NE
import Harness.Backend.BigQuery qualified as BigQuery
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Cockroach qualified as Cockroach
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine (postGraphql)
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (interpolateYaml)
import Harness.Schema (Table (..), table)
import Harness.Schema qualified as Schema
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.Protocol (withEachProtocol)
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, it)

spec :: SpecWith GlobalTestEnvironment
spec = do
  withEachProtocol
    $ Fixture.run
      ( NE.fromList
          [ (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
              { Fixture.setupTeardown = \(testEnvironment, _) ->
                  [ Postgres.setupTablesAction schema testEnvironment
                  ]
              },
            (Fixture.fixture $ Fixture.Backend Citus.backendTypeMetadata)
              { Fixture.setupTeardown = \(testEnvironment, _) ->
                  [ Citus.setupTablesAction schema testEnvironment
                  ]
              },
            (Fixture.fixture $ Fixture.Backend Cockroach.backendTypeMetadata)
              { Fixture.setupTeardown = \(testEnv, _) ->
                  [ Cockroach.setupTablesAction schema testEnv
                  ]
              },
            (Fixture.fixture $ Fixture.Backend BigQuery.backendTypeMetadata)
              { Fixture.setupTeardown = \(testEnvironment, _) ->
                  [ BigQuery.setupTablesAction schema testEnvironment
                  ],
                Fixture.customOptions =
                  Just
                    $ Fixture.defaultOptions
                      { Fixture.stringifyNumbers = True
                      }
              }
          ]
      )
      tests

--------------------------------------------------------------------------------
-- Schema

schema :: [Schema.Table]
schema =
  [ (table "authors")
      { tableColumns =
          [ Schema.column "id" Schema.TInt,
            Schema.column "name" Schema.TStr,
            Schema.column "address"
              $ Schema.TCustomType
                Schema.defaultBackendScalarType
                  { Schema.bstCitus = Just "JSON",
                    Schema.bstCockroach = Just "JSON",
                    Schema.bstPostgres = Just "JSON",
                    Schema.bstBigQuery = Just "JSON"
                  }
          ],
        tablePrimaryKey = ["id"],
        tableData =
          [ [ Schema.VInt 1,
              Schema.VStr "Justin",
              Schema.VCustomValue
                $ Schema.defaultBackendScalarValue
                  { Schema.bsvCitus = Just (Schema.Quoted "{ \"city\": \"Bristol\" }"),
                    Schema.bsvCockroach = Just (Schema.Quoted "{ \"city\": \"Bristol\" }"),
                    Schema.bsvPostgres = Just (Schema.Quoted "{ \"city\": \"Bristol\" }"),
                    Schema.bsvBigQuery = Just (Schema.Unquoted "JSON '{ \"city\": \"Bristol\" }'")
                  }
            ]
          ]
      }
  ]

--------------------------------------------------------------------------------
-- Tests

tests :: SpecWith TestEnvironment
tests = do
  it "Fetch a list of objects with a JSON column" \testEnvironment -> do
    let schemaName :: Schema.SchemaName
        schemaName = Schema.getSchemaName testEnvironment

        actual :: IO Value
        actual =
          postGraphql
            testEnvironment
            [graphql|
              query {
                #{schemaName}_authors(order_by: { id: asc }) {
                  id
                  name
                  address
                }
              }
            |]

        expected :: Value
        expected =
          [interpolateYaml|
            data:
              #{schemaName}_authors:
              - id: 1
                name: Justin
                address:
                  city: Bristol
          |]

    shouldReturnYaml testEnvironment actual expected
