{-# LANGUAGE QuasiQuotes #-}

-- | Test that backend which support it are able to explain queries.
module Test.Queries.ExplainSpec (spec) where

import Data.List.NonEmpty qualified as NE
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Cockroach qualified as Cockroach
import Harness.Backend.Mysql qualified as Mysql
import Harness.Backend.Postgres qualified as Postgres
import Harness.Backend.Sqlserver qualified as Sqlserver
import Harness.GraphqlEngine
import Harness.Quoter.Graphql
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (TestEnvironment (..))
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

spec :: SpecWith TestEnvironment
spec =
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Fixture.Postgres)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Postgres.setupTablesAction schema testEnvironment
                ]
            },
          (Fixture.fixture $ Fixture.Backend Fixture.Citus)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Citus.setupTablesAction schema testEnvironment
                ]
            },
          (Fixture.fixture $ Fixture.Backend Fixture.Cockroach)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Cockroach.setupTablesAction schema testEnvironment
                ]
            },
          (Fixture.fixture $ Fixture.Backend Fixture.SQLServer)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Sqlserver.setupTablesAction schema testEnvironment
                ]
            },
          (Fixture.fixture $ Fixture.Backend Fixture.MySQL)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Mysql.setupTablesAction schema testEnvironment
                ]
            }
        ]
    )
    tests

--------------------------------------------------------------------------------
-- Schema

schema :: [Schema.Table]
schema =
  [ (Schema.table "example")
      { Schema.tableColumns =
          [ Schema.column "id" Schema.TInt,
            Schema.column "name" Schema.TStr
          ],
        Schema.tablePrimaryKey = ["id"],
        Schema.tableData = []
      }
  ]

tests :: Fixture.Options -> SpecWith TestEnvironment
tests _opts = do
  describe "Explaining queries" do
    it "Works as expected" \testEnvironment -> do
      -- All that matters to this test is that the call completes without error.
      _ <- postExplain testEnvironment [graphql|query{hasura_example{id}}|]
      return ()
