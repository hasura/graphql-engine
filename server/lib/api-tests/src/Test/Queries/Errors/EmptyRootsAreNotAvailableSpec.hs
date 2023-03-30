{-# LANGUAGE QuasiQuotes #-}

module Test.Queries.Errors.EmptyRootsAreNotAvailableSpec (spec) where

import Data.Aeson (Value)
import Data.List.NonEmpty qualified as NE
import Harness.Backend.BigQuery qualified as BigQuery
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Cockroach qualified as Cockroach
import Harness.Backend.DataConnector.Sqlite qualified as Sqlite
import Harness.Backend.Postgres qualified as Postgres
import Harness.Backend.Sqlserver qualified as Sqlserver
import Harness.GraphqlEngine (postGraphql)
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (interpolateYaml)
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

spec :: SpecWith GlobalTestEnvironment
spec = do
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata),
          (Fixture.fixture $ Fixture.Backend Citus.backendTypeMetadata),
          (Fixture.fixture $ Fixture.Backend Cockroach.backendTypeMetadata),
          (Fixture.fixture $ Fixture.Backend Sqlserver.backendTypeMetadata),
          (Fixture.fixture $ Fixture.Backend BigQuery.backendTypeMetadata),
          (Fixture.fixture $ Fixture.Backend Sqlite.backendTypeMetadata)
        ]
    )
    tests

tests :: SpecWith TestEnvironment
tests = describe "empty roots" do
  forM_ ["mutation_root", "subscription_root"] \name ->
    it ("should not have a " <> name) \testEnvironment -> do
      let actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                query {
                  __type(name: "#{name}") {
                    fields {
                      name
                    }
                  }
                }
              |]

          expected :: Value
          expected =
            [interpolateYaml|
              data:
                __type: null
          |]

      shouldReturnYaml testEnvironment actual expected
