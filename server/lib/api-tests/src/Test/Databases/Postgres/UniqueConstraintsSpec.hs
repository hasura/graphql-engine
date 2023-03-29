-- |
-- Tests to determine whether we can create tables with unique constraints.
--
-- https://hasura.io/docs/latest/guides/postgres/constraints/#unique-constraints
module Test.Databases.Postgres.UniqueConstraintsSpec (spec) where

import Data.List.NonEmpty qualified as NE
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Cockroach qualified as Cockroach
import Harness.Backend.Postgres qualified as Postgres
import Harness.Schema (Table (..), table)
import Harness.Schema qualified as Schema
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

spec :: SpecWith GlobalTestEnvironment
spec = do
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Postgres.setupTablesAction schema testEnv
                ]
            },
          (Fixture.fixture $ Fixture.Backend Citus.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Citus.setupTablesAction schema testEnv
                ]
            },
          (Fixture.fixture $ Fixture.Backend Cockroach.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Cockroach.setupTablesAction schema testEnv
                ]
            }
        ]
    )
    tests

--------------------------------------------------------------------------------
-- Schema

schema :: [Table]
schema =
  [ (table "test")
      { tableColumns =
          [ Schema.column "id" Schema.TInt,
            Schema.column "username" Schema.TStr,
            Schema.column "email" Schema.TStr
          ],
        tableConstraints =
          [ Schema.UniqueConstraintColumns ["username"]
          ],
        tableUniqueIndexes =
          [ Schema.UniqueIndexExpression "lower(email)"
          ]
      }
  ]

--------------------------------------------------------------------------------
-- Tests

tests :: SpecWith TestEnvironment
tests = describe "Unique constraints" do
  it "Creates a schema with unique constraints" \_ -> pure @IO ()
