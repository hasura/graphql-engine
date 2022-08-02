-- | This module tests that the postgres table metadata collection correctly
-- handles unique constraints that are based on expressions rather than columns.
module Test.GatheringUniqueConstraintsSpec (spec) where

import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Postgres qualified as Postgres
import Harness.Test.BackendType qualified as BackendType
import Harness.Test.Fixture
import Harness.Test.Schema
import Harness.TestEnvironment (TestEnvironment)
import Hasura.Prelude
import Test.Hspec

--------------------------------------------------------------------------------
-- Preamble

spec :: SpecWith TestEnvironment
spec =
  run
    [postgresFixture, citusFixture]
    (\_ -> it "Tracks tables without failing" $ \_ -> return @IO ())

postgresFixture :: Fixture ()
postgresFixture =
  (fixture $ Backend BackendType.Postgres)
    { setupTeardown = \(t, _) ->
        [ Postgres.setupTablesAction tables t
        ]
    }

citusFixture :: Fixture ()
citusFixture =
  (fixture $ Backend BackendType.Citus)
    { setupTeardown = \(t, _) ->
        [ Citus.setupTablesAction tables t
        ]
    }

{-
CREATE TABLE test (id INT, username TEXT);
CREATE UNIQUE INDEX ON test ((lower(username)));
-}
tables :: [Table]
tables =
  [ (table "test")
      { tableColumns =
          [ column "id" TInt,
            column "username" TStr
          ],
        tableUniqueConstraints = [UniqueConstraintExpression "lower(username)"]
      }
  ]
