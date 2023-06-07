{-# LANGUAGE QuasiQuotes #-}

-- | Testing whether BigQuery types are understood and aggregated correctly.
--
-- TODO: extend this test when we understand @STRUCT@/@RECORD@.
module Test.Databases.BigQuery.Queries.TypeInterpretationSpec (spec) where

import Data.Aeson (Value)
import Data.List.NonEmpty qualified as NE
import Harness.Backend.BigQuery qualified as BigQuery
import Harness.GraphqlEngine (postGraphql)
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (interpolateYaml)
import Harness.Schema (Table (..), table)
import Harness.Schema qualified as Schema
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

spec :: SpecWith GlobalTestEnvironment
spec =
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend BigQuery.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ BigQuery.setupTablesAction schema testEnv
                ]
            }
        ]
    )
    tests

--------------------------------------------------------------------------------
-- Schema
--
-- We keep this very BigQuery-specific and explicit to ensure that we're
-- testing the specific distinct BigQuery types. For example, we want to be
-- sure that if someone changed the 'Schema.TInt' translation to "BIGNUMERIC",
-- we'd still test it with the "integer" column below.

column :: Text -> Text -> Schema.Column
column name custom = Schema.column name wrapped
  where
    wrapped :: Schema.ScalarType
    wrapped =
      Schema.TCustomType
        Schema.defaultBackendScalarType
          { Schema.bstBigQuery = Just custom
          }

value :: (Text -> Schema.BackendScalarValueType) -> Text -> Schema.ScalarValue
value present custom =
  Schema.VCustomValue
    Schema.defaultBackendScalarValue
      { Schema.bsvBigQuery = Just (present custom)
      }

schema :: [Schema.Table]
schema =
  [ (table "all_types")
      { tableColumns =
          [ column "string" "STRING",
            column "bytes" "BYTES",
            column "integer" "INT64",
            column "float" "FLOAT64",
            column "special_floats" "ARRAY<FLOAT64>",
            column "numeric" "NUMERIC",
            column "bignumeric" "BIGNUMERIC",
            column "boolean" "BOOL",
            column "timestamp" "TIMESTAMP",
            column "date" "DATE",
            column "time" "TIME",
            column "datetime" "DATETIME",
            column "geography" "GEOGRAPHY"
          ],
        tableData =
          [ [ value Schema.Quoted "STRING", -- STRING
              value Schema.Unquoted "CODE_POINTS_TO_BYTES([0,1,2,3,4,5])", -- BYTES
              value Schema.Unquoted "1", -- INTEGER
              value Schema.Unquoted "0.1", -- FLOAT
              value Schema.Unquoted "[1.23e23, IEEE_DIVIDE(-1, 0), IEEE_DIVIDE(1, 0) / IEEE_DIVIDE(1, 0)]", -- ARRAY<FLOAT64>
              value Schema.Unquoted "1000", -- NUMERIC
              value Schema.Unquoted "12345678987654321", -- BIGNUMERIC, over 32-bit
              value Schema.Unquoted "true", -- BOOL
              value Schema.Unquoted "PARSE_TIMESTAMP('%c', 'Thu Dec 25 07:30:00 2008')", -- TIMESTAMP
              value Schema.Unquoted "PARSE_DATE('%F', '2000-12-30')", -- DATE
              value Schema.Unquoted "PARSE_TIME('%T', '07:30:00')", -- TIME
              value Schema.Unquoted "PARSE_DATETIME('%Y-%m-%d %H:%M:%S', '1998-10-18 13:45:55')", -- DATETIME
              value Schema.Unquoted "ST_GEOGPOINT(1, 1)" -- GEOGRAPHY
            ],
            [ value Schema.Quoted "ANOTHER STRING", -- STRING
              value Schema.Unquoted "CODE_POINTS_TO_BYTES([5,4,3,2,1,0])", -- BYTES
              value Schema.Unquoted "3", -- INTEGER
              value Schema.Unquoted "0.5", -- FLOAT
              value Schema.Unquoted "[]", -- ARRAY<FLOAT64>
              value Schema.Unquoted "1234", -- NUMERIC
              value Schema.Unquoted "23456789098765432", -- BIGNUMERIC, over 32-bit
              value Schema.Unquoted "false", -- BOOL
              value Schema.Unquoted "PARSE_TIMESTAMP('%c', 'Thu Nov 25 07:30:00 2009')", -- TIMESTAMP
              value Schema.Unquoted "PARSE_DATE('%F', '2000-11-25')", -- DATE
              value Schema.Unquoted "PARSE_TIME('%T', '06:30:00')", -- TIME
              value Schema.Unquoted "PARSE_DATETIME('%Y-%m-%d %H:%M:%S', '1999-12-13 12:47:52')", -- DATETIME
              value Schema.Unquoted "ST_GEOGPOINT(2, 1)" -- GEOGRAPHY
            ]
          ]
      }
  ]

--------------------------------------------------------------------------------
-- Tests

tests :: SpecWith TestEnvironment
tests = do
  describe "Understanding BigQuery values via GraphQL" do
    it "Selects all types" \testEnvironment -> do
      let schemaName = Schema.getSchemaName testEnvironment

      let expected :: Value
          expected =
            [interpolateYaml|
              data:
                #{schemaName}_all_types:
                - string: 'ANOTHER STRING'
                  bytes: BQQDAgEA
                  integer: '3'
                  float: '0.5'
                  special_floats: []
                  numeric: '1234'
                  bignumeric: '23456789098765432'
                  boolean: false
                  timestamp: '2009-11-25T07:30:00Z'
                  date: '2000-11-25'
                  time: 06:30:00
                  datetime: '1999-12-13T12:47:52'
                  geography: POINT(2 1)

                - string: 'STRING'
                  bytes: AAECAwQF
                  integer: '1'
                  float: '0.1'
                  special_floats:
                    - '1.23E23'
                    - '-Infinity'
                    - NaN
                  numeric: '1000'
                  bignumeric: '12345678987654321'
                  boolean: true
                  timestamp: '2008-12-25T07:30:00Z'
                  date: '2000-12-30'
                  time: 07:30:00
                  datetime: '1998-10-18T13:45:55'
                  geography: POINT(1 1)
            |]

          actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                query {
                  #{schemaName}_all_types(order_by: [{ string: asc }]) {
                    string
                    bytes
                    integer
                    float
                    special_floats
                    numeric
                    bignumeric
                    boolean
                    timestamp
                    date
                    time
                    datetime
                    geography
                  }
                }
              |]

      shouldReturnYaml testEnvironment actual expected

    it "Accepts strings for numbers in input fields (experimental feature 'bigquery_string_numeric_input')" \testEnvironment -> do
      let schemaName = Schema.getSchemaName testEnvironment

      let expected :: Value
          expected =
            [interpolateYaml|
              data:
                #{schemaName}_all_types:
                - float: "0.5"
                  numeric: "1234"
                  bignumeric: "23456789098765432"
            |]

          actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                query {
                  #{schemaName}_all_types(where:
                    {_and: [
                        { float: { _eq: "0.5" }}
                        { integer: { _eq: "3" }}
                        { numeric: { _eq: "1234" }}
                        { bignumeric: { _eq: "23456789098765432" }}
                        ]
                    }
                ) {
                    float
                    numeric
                    bignumeric
                  }
                }
              |]

      shouldReturnYaml testEnvironment actual expected

    it "Aggregates all comparable types" \testEnvironment -> do
      let schemaName = Schema.getSchemaName testEnvironment

      let expected :: Value
          expected =
            [interpolateYaml|
              data:
                #{schemaName}_all_types_aggregate:
                  aggregate:
                    max:
                      bignumeric: '23456789098765432'
                      boolean: true
                      bytes: 'BQQDAgEA'
                      date: '2000-12-30'
                      datetime: '1999-12-13T12:47:52'
                      float: '0.5'
                      integer: '3'
                      numeric: '1234'
                      string: 'STRING'
                      time: '07:30:00'
                      timestamp: '2009-11-25T07:30:00Z'
                    min:
                      bignumeric: '12345678987654321'
                      boolean: false
                      bytes: 'AAECAwQF'
                      date: '2000-11-25'
                      datetime: '1998-10-18T13:45:55'
                      float: '0.1'
                      integer: '1'
                      numeric: '1000'
                      string: 'ANOTHER STRING'
                      time: '06:30:00'
                      timestamp: '2008-12-25T07:30:00Z'
            |]

          actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                query {
                  #{schemaName}_all_types_aggregate {
                    aggregate {
                      max {
                        string
                        bytes
                        integer
                        float
                        numeric
                        bignumeric
                        boolean
                        timestamp
                        date
                        time
                        datetime
                      }
                      min {
                        string
                        bytes
                        integer
                        float
                        numeric
                        bignumeric
                        boolean
                        timestamp
                        date
                        time
                        datetime
                      }
                    }
                  }
                }
              |]

      shouldReturnYaml testEnvironment actual expected
