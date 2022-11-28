-- | Test the schema names conflict of <table_name>_stream
module Test.Regression.StreamConflictSpec (spec) where

import Data.List.NonEmpty qualified as NE
import Harness.Backend.Postgres qualified as Postgres
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.Schema (Table (..), table)
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (TestEnvironment)
import Hasura.Prelude
import Test.Hspec (SpecWith, it)

spec :: SpecWith TestEnvironment
spec =
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Fixture.Postgres)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Postgres.setupTablesAction schema testEnv
                ],
              Fixture.customOptions =
                Just $
                  Fixture.defaultOptions
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

tests :: Fixture.Options -> SpecWith TestEnvironment
tests _ =
  -- All of the testing is done during setup.
  -- If setup succeeds and we have no conflicts, and this test will pass.
  it "Creates a schema without conflicts" \_ -> pure @IO ()
