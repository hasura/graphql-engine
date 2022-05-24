{-# LANGUAGE QuasiQuotes #-}

-- | Serialization test for specific data types
module Test.SerializationSpec (spec) where

import Harness.Backend.BigQuery qualified as Bigquery
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Sql
import Harness.Quoter.Yaml (shouldReturnYaml, yaml)
import Harness.Test.Context qualified as Context
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (TestEnvironment)
import Test.Hspec (SpecWith, describe, it)
import Prelude

--------------------------------------------------------------------------------
-- Preamble

spec :: SpecWith TestEnvironment
spec =
  Context.run
    [ Context.Context
        { name = Context.Backend Context.BigQuery,
          mkLocalTestEnvironment = Context.noLocalTestEnvironment,
          setup = bigquerySetup,
          teardown = Bigquery.teardown [author],
          customOptions = Nothing
        }
    ]
    tests

author :: Schema.Table
author =
  Schema.Table
    { tableName = "author",
      tableColumns = [],
      tablePrimaryKey = [],
      tableReferences = [],
      tableData = []
    }

bigquerySetup :: (TestEnvironment, ()) -> IO ()
bigquerySetup (testEnvironment, _) = do
  sourceMetadata <- Bigquery.defaultSourceMetadata
  GraphqlEngine.postMetadata_ testEnvironment sourceMetadata

  Bigquery.runSql_
    [sql|
        CREATE TABLE hasura.author (
          id INT,
          name STRING,
          tax_id DECIMAL,
          total_books BIGDECIMAL
        );
        |]
  Bigquery.runSql_
    [sql|
        INSERT hasura.author (id, name, tax_id, total_books)
        VALUES (1, "sibi", 5555555555555556666, 5555555555555556666);
        |]

  Bigquery.trackTable testEnvironment author

tests :: Context.Options -> SpecWith TestEnvironment
tests opts = describe "SerializationSpec" $ do
  it "serde Decimal column" $ \testEnvironment ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          testEnvironment
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
    id: "1"
|]
  it "serde BigDecimal column" $ \testEnvironment ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          testEnvironment
          [graphql|
query MyQuery {
  hasura_author(where: {total_books: {_eq: 5555555555555556666}}) {
    id
    tax_id
    total_books
  }
}|]
      )
      [yaml|
data:
  hasura_author:
  - tax_id: "5555555555555556666"
    total_books: "5555555555555556666"
    id: "1"
|]
