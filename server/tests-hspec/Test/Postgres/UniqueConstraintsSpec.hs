-- |
-- Tests to determine whether we can create tables with unique constraints.
--
-- https://hasura.io/docs/latest/guides/postgres/constraints/#unique-constraints
module Test.Postgres.UniqueConstraintsSpec (spec) where

import Data.List.NonEmpty qualified as NE
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Postgres qualified as Postgres
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.Schema (Table (..), table)
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (TestEnvironment)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

spec :: SpecWith TestEnvironment
spec = do
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Fixture.Postgres)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Postgres.setupTablesAction schema testEnv
                ]
            },
          (Fixture.fixture $ Fixture.Backend Fixture.Citus)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Citus.setupTablesAction schema testEnv
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
            Schema.column "username" Schema.TStr
          ],
        tableUniqueConstraints =
          [ Schema.UniqueConstraintExpression "lower(username)"
          ]
      }
  ]

--------------------------------------------------------------------------------
-- Tests

tests :: Fixture.Options -> SpecWith TestEnvironment
tests _ = describe "Unique constraints" do
  it "Creates a schema with unique constraints" \_ -> pure @IO ()
