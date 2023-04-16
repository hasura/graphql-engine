{-# LANGUAGE QuasiQuotes #-}

module Test.Regression.SqlServerIdentifierQuotingSpec (spec) where

import Data.List.NonEmpty qualified as NE
import Harness.Backend.Sqlserver qualified as Sqlserver
import Harness.GraphqlEngine
import Harness.Quoter.Graphql
import Harness.Quoter.Yaml
import Harness.Schema qualified as Schema
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment (..))
import Harness.Yaml
import Hasura.Prelude
import Test.Hspec (SpecWith, it)

spec :: SpecWith GlobalTestEnvironment
spec =
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Sqlserver.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Sqlserver.setupTablesAction schema testEnvironment
                ]
            }
        ]
    )
    tests

--------------------------------------------------------------------------------
-- Schema

schema :: [Schema.Table]
schema =
  [ (Schema.table "delimited_identifiers")
      { Schema.tableColumns =
          [(Schema.column "]colu[mn with] a quoted name." Schema.TInt) {Schema.columnGqlAlias = Just "c"}]
      }
  ]

tests :: SpecWith TestEnvironment
tests = do
  it "Can query a table with a delimited identifier" $ \testEnv -> do
    let schemaName = Schema.getSchemaName testEnv

    shouldReturnYaml
      testEnv
      ( postGraphql
          testEnv
          [graphql|
            query {
              #{schemaName}_delimited_identifiers {
                c
              }
            }
          |]
      )
      [interpolateYaml|
        data:
          #{schemaName}_delimited_identifiers: []
      |]
