-- | Testing regression reported at https://github.com/hasura/graphql-engine/issues/8643
module Test.Regression.UsingTheSameFunctionForRootFieldAndComputedField8643Spec
  ( spec,
  )
where

-- import Data.Aeson (Value (Null))
import Data.List.NonEmpty qualified as NE
import Harness.Backend.Postgres qualified as Postgres
import Harness.Constants qualified as Constants
-- import Harness.Quoter.Yaml (yaml)
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (TestEnvironment (..))
-- import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec

spec :: SpecWith TestEnvironment
spec =
  Fixture.run
    ( (Fixture.fixture $ Fixture.Backend Fixture.Postgres)
        { Fixture.setupTeardown = \(testEnvironment, _) ->
            [ Postgres.setupTablesAction schema testEnvironment,
              functionSetup,
              Postgres.setupFunctionRootFieldAction "authors" testEnvironment,
              Postgres.setupComputedFieldAction authorTable "authors" "author_comp" testEnvironment
            ]
        }
        NE.:| []
    )
    tests

--------------------------------------------------------------------------------
-- Schema

authorTable :: Schema.Table
authorTable =
  (Schema.table "author")
    { Schema.tableColumns =
        [ Schema.column "id" Schema.TInt
        ],
      Schema.tablePrimaryKey = ["id"]
    }

schema :: [Schema.Table]
schema =
  [ (Schema.table "book")
      { Schema.tableColumns =
          [ Schema.column "id" Schema.TInt,
            Schema.column "author_id" Schema.TInt
          ],
        Schema.tablePrimaryKey = ["id"]
      },
    authorTable
  ]

functionSetup :: Fixture.SetupAction
functionSetup =
  Fixture.SetupAction
    { setupAction =
        Postgres.run_ $
          "CREATE FUNCTION " ++ Constants.postgresDb
            ++ ".authors(author_row author) \
               \RETURNS SETOF author AS $$ \
               \  SELECT * \
               \  FROM "
            ++ Constants.postgresDb
            ++ ".author \
               \$$ LANGUAGE sql STABLE;",
      teardownAction = \_ -> Postgres.run_ $ "DROP FUNCTION " ++ Constants.postgresDb ++ ".authors;"
    }

--------------------------------------------------------------------------------
-- Tests

tests :: Fixture.Options -> SpecWith TestEnvironment
tests _opts = do
  describe "Tracking the same function as a root field and as a computed field" do
    it "Does not give rise to metadata inconsistencies" \testEnvironment -> do
      -- The error message we're looking out for if this fails is:
      --
      -- {
      --   "code": "not-supported",
      --   "error": "the following tracked function(s) cannot be overloaded: hasura.authors",
      --   "path": "$"
      -- }
      Postgres.runSQL
        ("alter table \"" ++ Constants.postgresDb ++ "\".\"author\" add column \"iae\" integer\n null;")
        testEnvironment
