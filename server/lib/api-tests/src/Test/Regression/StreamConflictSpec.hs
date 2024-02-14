-- | Test the schema names conflict of <table_name>_stream
module Test.Regression.StreamConflictSpec (spec) where

import Data.List.NonEmpty qualified as NE
import Harness.Backend.Postgres qualified as Postgres
import Harness.Schema (Table (..), table)
import Harness.Schema qualified as Schema
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment)
import Hasura.Prelude
import Test.Hspec (SpecWith, it)

spec :: SpecWith GlobalTestEnvironment
spec =
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Postgres.setupTablesAction schema testEnv
                ],
              Fixture.customOptions =
                Just
                  $ Fixture.defaultOptions
                    { Fixture.skipTests =
                        Just "Disabled until we can dynamically change server settings per test. To test, add EFHideStreamFields to soSubscriptions in Harness.Constants -> serveOptions"
                    }
            }
        ]
    )
    tests

--------------------------------------------------------------------------------
-- Schema

schema :: [Schema.Table]
schema =
  [ (table "author")
      { tableColumns =
          [ Schema.column "id" Schema.defaultSerialType,
            Schema.column "name" Schema.TStr
          ],
        tablePrimaryKey = ["id"]
      },
    (table "author_stream")
      { tableColumns =
          [ Schema.column "id" Schema.defaultSerialType,
            Schema.column "name" Schema.TStr
          ],
        tablePrimaryKey = ["id"]
      }
  ]

--------------------------------------------------------------------------------
-- Tests

tests :: SpecWith TestEnvironment
tests =
  -- All of the testing is done during setup.
  -- If setup succeeds and we have no conflicts, and this test will pass.
  it "Creates a schema without conflicts" \_ -> pure @IO ()
