{-# LANGUAGE QuasiQuotes #-}

-- | Test *_run_sql query API
module Test.RunSQLSpec (spec) where

import Harness.Backend.BigQuery qualified as BigQuery
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml
import Harness.State (State)
import Harness.Test.Context qualified as Context
import Test.Hspec
import Prelude

--------------------------------------------------------------------------------
-- Preamble
spec :: SpecWith State
spec =
  Context.run
    [ Context.Context
        { name = Context.Backend Context.BigQuery,
          mkLocalState = Context.noLocalState,
          setup = BigQuery.setup [],
          teardown = BigQuery.teardown [],
          customOptions = Nothing
        }
    ]
    tests

--------------------------------------------------------------------------------
-- Tests

tests :: Context.Options -> SpecWith State
tests opts = do
  it "BigQuery - running invalid SQL" \state ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postV2Query
          400
          state
          [yaml|
type: bigquery_run_sql
args:
  source: bigquery
  sql: 'some invalid SQL'
|]
      )
      [yaml|
internal:
  rest_request_non_ok:
    error:
      status: INVALID_ARGUMENT
      code: 400
      message: 'Syntax error: Expected end of input but got keyword SOME at [1:1]'
      errors:
      - location: q
        domain: global
        reason: invalidQuery
        locationType: parameter
        message: 'Syntax error: Expected end of input but got keyword SOME at [1:1]'
path: "$"
error: Bigquery HTTP request failed with status code 400 and status message "Bad Request"
code: bigquery-error
|]
