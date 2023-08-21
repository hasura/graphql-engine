{-# LANGUAGE QuasiQuotes #-}

-- | Test that backend which support it are able to explain queries.
-- https://hasura.io/docs/latest/api-reference/explain/
module Test.API.ExplainSpec (spec) where

import Data.Aeson qualified as J
import Data.ByteString.Lazy.Char8 qualified as ByteString
import Data.Char qualified as Char
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe qualified as Maybe
import Data.Text qualified as Text
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Cockroach qualified as Cockroach
import Harness.Backend.Postgres qualified as Postgres
import Harness.Backend.Sqlserver qualified as Sqlserver
import Harness.GraphqlEngine
import Harness.Quoter.Graphql
import Harness.Schema qualified as Schema
import Harness.Test.BackendType (backendType)
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment (..), getBackendTypeConfig)
import Hasura.Prelude
import Test.Hspec

-- A decoder type for the response JSON.
--
-- We don't have a `FromJSON` instance for the data type used for encoding.
data ExplainPlan = ExplainPlan
  { epField :: Text,
    epSql :: Maybe Text,
    epPlan :: Maybe [Text]
  }
  deriving (Show, Eq, Generic)

instance J.FromJSON ExplainPlan where
  parseJSON = J.genericParseJSON $ J.defaultOptions {J.fieldLabelModifier = map Char.toLower . drop 2}

spec :: SpecWith GlobalTestEnvironment
spec =
  Fixture.run
    ( NonEmpty.fromList
        [ (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Postgres.setupTablesAction schema testEnvironment
                ]
            },
          (Fixture.fixture $ Fixture.Backend Citus.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Citus.setupTablesAction schema testEnvironment
                ]
            },
          (Fixture.fixture $ Fixture.Backend Cockroach.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Cockroach.setupTablesAction schema testEnvironment
                ]
            },
          (Fixture.fixture $ Fixture.Backend Sqlserver.backendTypeMetadata)
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
  [ (Schema.table "example")
      { Schema.tableColumns =
          [ Schema.column "id" Schema.TInt,
            Schema.column "name" Schema.TStr
          ],
        Schema.tablePrimaryKey = ["id"],
        Schema.tableData = []
      }
  ]

tests :: SpecWith TestEnvironment
tests = do
  describe "Explaining queries" do
    it "Works as expected" \testEnvironment -> do
      -- Every database has its own query plan syntax, so we have to
      -- differentiate here. We can't assert on the whole query plan because it
      -- will change between runs, so we just check we get something reasonable
      -- back.
      expectedPlan <-
        case backendType <$> getBackendTypeConfig testEnvironment of
          Just Fixture.Postgres -> pure "Seq Scan"
          Just Fixture.Citus -> pure "Seq Scan"
          Just Fixture.Cockroach -> pure "scan"
          Just Fixture.SQLServer -> pure "Clustered Index Scan"
          Just _ -> fail "Please configure the test with a snippet of the expected query plan."
          Nothing -> fail "Unknown backend."

      response <- postExplain testEnvironment [graphql|query { hasura_example(limit: 5) { id } }|]
      [ExplainPlan {..}] <- case J.fromJSON response of
        J.Error message -> fail $ message <> "\nJSON:\n" <> ByteString.unpack (J.encode response)
        J.Success value -> pure value

      epField `shouldBe` "hasura_example"
      -- make sure the returned SQL looks plausible
      epSql `shouldSatisfy` (("example" `Text.isInfixOf`) . Maybe.fromJust)
      -- make sure the query plan looks plausible
      epPlan `shouldSatisfy` ((any (expectedPlan `Text.isInfixOf`)) . Maybe.fromJust)
