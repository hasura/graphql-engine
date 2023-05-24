-- | Testing regression reported at https://github.com/hasura/graphql-engine/issues/8643
module Test.Regression.UsingTheSameFunctionForRootFieldAndComputedField8643Spec (spec) where

import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Harness.Backend.Postgres qualified as Postgres
import Harness.Schema
import Harness.Schema qualified as Schema
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.SetupAction qualified as SetupAction
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment (..))
import Hasura.Prelude
import Test.Hspec

spec :: SpecWith GlobalTestEnvironment
spec =
  Fixture.run
    ( (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
        { Fixture.setupTeardown = \(testEnvironment, _) ->
            [ Postgres.setupTablesAction schema testEnvironment,
              functionSetup testEnvironment,
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

functionSetup :: TestEnvironment -> Fixture.SetupAction
functionSetup testEnvironment =
  let schemaName = unSchemaName (getSchemaName testEnvironment)
   in SetupAction.noTeardown
        $ Postgres.run_ testEnvironment
        $ "CREATE FUNCTION "
        <> schemaName
        <> ".authors(author_row "
        <> schemaName
        <> ".author) \
           \RETURNS SETOF "
        <> schemaName
        <> ".author AS $$ \
           \  SELECT * \
           \  FROM "
        <> schemaName
        <> ".author \
           \$$ LANGUAGE sql STABLE;"

--------------------------------------------------------------------------------
-- Tests

tests :: SpecWith TestEnvironment
tests = do
  describe "Tracking the same function as a root field and as a computed field" do
    it "Does not give rise to metadata inconsistencies" \testEnvironment -> do
      let schemaName = T.unpack $ unSchemaName (getSchemaName testEnvironment)
      -- The error message we're looking out for if this fails is:
      --
      -- {
      --   "code": "not-supported",
      --   "error": "the following tracked function(s) cannot be overloaded: hasura.authors",
      --   "path": "$"
      -- }
      Postgres.runSQL
        ("alter table \"" <> schemaName <> "\".\"author\" add column \"iae\" integer\n null;")
        testEnvironment
