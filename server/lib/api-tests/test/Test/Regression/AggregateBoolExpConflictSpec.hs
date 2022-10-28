-- | Test the schema names conflict of bool exp aggregates and table selection aggregates
module Test.Regression.AggregateBoolExpConflictSpec (spec) where

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
                ]
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
    (table "user")
      { tableColumns =
          [ Schema.column "id" Schema.defaultSerialType,
            Schema.column "name" Schema.TStr
          ],
        tablePrimaryKey = ["id"]
      },
    (table "article")
      { tableColumns =
          [ Schema.column "id" Schema.defaultSerialType,
            Schema.column "title" Schema.TStr,
            Schema.column "content" Schema.TStr,
            Schema.column "published_on" Schema.TStr,
            Schema.column "author_id" serialInt,
            Schema.column "user_id" serialInt
          ],
        tablePrimaryKey = ["id"],
        tableReferences =
          [ Schema.Reference "author_id" "author" "id",
            Schema.Reference "user_id" "user" "id"
          ]
      },
    -- The regression specifically is that the `_aggregate` root field for this
    -- table will conflict with the aggregation predicate for the array relationship
    -- between author and article.
    (table $ Schema.mkArrayRelationshipName "author_article" "id" "author_id")
      { tableColumns =
          [ Schema.column "id" Schema.defaultSerialType
          ],
        tablePrimaryKey = ["id"]
      },
    (table $ Schema.mkArrayRelationshipName "article" "id" "author_id")
      { tableColumns =
          [ Schema.column "id" Schema.defaultSerialType
          ],
        tablePrimaryKey = ["id"]
      }
  ]

serialInt :: Schema.ScalarType
serialInt =
  Schema.TCustomType $
    Schema.defaultBackendScalarType
      { Schema.bstCitus = Just "INT",
        Schema.bstPostgres = Just "INT",
        Schema.bstCockroach = Just "INT4"
      }

--------------------------------------------------------------------------------
-- Tests

tests :: Fixture.Options -> SpecWith TestEnvironment
tests _ =
  -- All of the testing is done during setup.
  -- If setup succeeds and we have no conflicts, and this test will pass.
  it "Creates a schema without conflicts" \_ -> pure @IO ()
