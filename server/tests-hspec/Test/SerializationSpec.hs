{-# LANGUAGE QuasiQuotes #-}

-- | Serialization test for specific data types
module Test.SerializationSpec (spec) where

import Harness.Backend.BigQuery qualified as Bigquery
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Sql
import Harness.Quoter.Yaml (shouldReturnYaml, yaml)
import Harness.State (State)
import Harness.Test.Context qualified as Context
import Harness.Test.Schema qualified as Schema
import Test.Hspec (SpecWith, describe, it)
import Prelude

--------------------------------------------------------------------------------
-- Preamble

spec :: SpecWith State
spec =
  Context.run
    [ Context.Context
        { name = Context.Backend Context.BigQuery,
          mkLocalState = Context.noLocalState,
          setup = bigQuerySetup,
          teardown = const bigQueryTeardown,
          customOptions =
            Just $
              Context.Options
                { stringifyNumbers = True
                }
        }
    ]
    tests

authorTable :: Schema.Table
authorTable =
  Schema.Table
    { tableName = "author",
      tableColumns = [],
      tablePrimaryKey = [],
      tableReferences = [],
      tableData = []
    }

-- todo: Remove this when this gets merged: https://github.com/hasura/graphql-engine-mono/pull/4246
bigQuerySetup :: (State, ()) -> IO ()
bigQuerySetup (state, _) = do
  sourceMetadata <- Bigquery.defaultSourceMetadata
  GraphqlEngine.postMetadata_ state sourceMetadata

  Bigquery.runSql_
    [sql|
        CREATE TABLE hasura.author (
          id INT,
          name STRING,
          tax_id DECIMAL,
        );
        |]
  Bigquery.runSql_
    [sql|
        INSERT hasura.author (id, name, tax_id)
        VALUES (1, "sibi", 5555555555555556666);
        |]

  Bigquery.trackTable state authorTable

bigQueryTeardown :: IO ()
bigQueryTeardown = do
  Bigquery.dropTable authorTable

tests :: Context.Options -> SpecWith State
tests opts = describe "SerializationSpec" $ do
  it "serde Decimal column" $ \state ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          state
          [graphql|
query MyQuery {
  hasura_author(where: {tax_id: {_eq: 5555555555555556666}}) {
    id
    tax_id
  }
}|]
      )
      [yaml|
data:
  hasura_author:
  - tax_id: "5555555555555556666"
    id: 1
|]
