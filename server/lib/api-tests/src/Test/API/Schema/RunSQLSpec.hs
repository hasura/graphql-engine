{-# LANGUAGE QuasiQuotes #-}

-- | Test *_run_sql query API
-- https://hasura.io/docs/latest/api-reference/schema-api/run-sql/
module Test.API.Schema.RunSQLSpec (spec) where

import Data.List.NonEmpty qualified as NE
import Harness.Backend.BigQuery qualified as BigQuery
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml (yaml)
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, it)

--------------------------------------------------------------------------------
-- Preamble

spec :: SpecWith GlobalTestEnvironment
spec = do
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend BigQuery.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ BigQuery.setupTablesAction [] testEnv
                ]
            }
        ]
    )
    tests

--------------------------------------------------------------------------------
-- Tests

tests :: SpecWith TestEnvironment
tests = do
  it "`bigquery_run_sql` with invalid SQL returns an `INVALID_ARGUMENT` error" \testEnvironment -> do
    let actual =
          GraphqlEngine.postV2Query
            400
            testEnvironment
            [yaml|
              type: bigquery_run_sql
              args:
                  source: bigquery
                  sql: 'some invalid SQL'
            |]

    let expected =
          [yaml|
            code: bigquery-error
            error: BigQuery HTTP request failed with status 400 "Bad Request"
            internal:
              rest_request_non_ok:
                error:
                  code: 400
                  errors:
                  - domain: global
                    location: q
                    locationType: parameter
                    message: 'Syntax error: Unexpected keyword SOME at [1:1]'
                    reason: invalidQuery
                  message: 'Syntax error: Unexpected keyword SOME at [1:1]'
                  status: INVALID_ARGUMENT
            path: $
          |]

    shouldReturnYaml testEnvironment actual expected
