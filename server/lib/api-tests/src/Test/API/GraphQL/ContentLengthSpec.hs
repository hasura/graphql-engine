{-# LANGUAGE QuasiQuotes #-}

-- | Tests that the /graphql API correctly sets the Content-Length header.
--
-- Importantly, we DO NOT check that the length is correct! That's because the
-- library we use for http calls actually *does* check that the Content-Length
-- header is correct *if it's present*, meaning that all other tests already
-- ensure its correctness. However, that library doesn't enforce that the header
-- is present in the first place, which is what that this test is for.
module Test.API.GraphQL.ContentLengthSpec (spec) where

import Data.Aeson (Value, object, (.=))
import Data.List.NonEmpty qualified as NE
import Harness.Backend.Postgres qualified as Postgres
import Harness.Http qualified as Http
import Harness.Quoter.Graphql (graphql)
import Harness.Schema (Table (..), table)
import Harness.Schema qualified as Schema
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment (..), getServer, serverUrl)
import Hasura.Prelude
import Network.HTTP.Simple qualified as Http
import Test.Hspec (SpecWith, describe, it, shouldSatisfy)

--------------------------------------------------------------------------------
-- Preamble

spec :: SpecWith GlobalTestEnvironment
spec = do
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
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
          [ Schema.column "id" Schema.TInt,
            Schema.column "name" Schema.TStr
          ],
        tablePrimaryKey = ["id"],
        tableData = []
      }
  ]

--------------------------------------------------------------------------------
-- Tests

tests :: SpecWith TestEnvironment
tests = do
  describe "GraphQL query contains a Content-Length response header" do
    it "checks for the header in a valid GraphQL query" \testEnvironment -> do
      let schemaName :: Schema.SchemaName
          schemaName = Schema.getSchemaName testEnvironment
          queryGQL :: Value
          queryGQL =
            [graphql|
              query {
                #{schemaName}_author {
                  id
                  name
                }
              }
            |]
          url :: String
          url = serverUrl (getServer testEnvironment) ++ "/v1/graphql"
      response <- Http.post url mempty $ object ["query" .= queryGQL]
      let contentLengthHeaders = Http.getResponseHeader "Content-Length" response
      contentLengthHeaders `shouldSatisfy` ((== 1) . length)

  describe "GraphQL query contains a Content-Length response header" do
    it "checks for the header in an invalid GraphQL query" \testEnvironment -> do
      let schemaName :: Schema.SchemaName
          schemaName = Schema.getSchemaName testEnvironment
          queryGQL :: Value
          queryGQL =
            [graphql|
              query {
                #{schemaName}_artist {
                  favouriteVideoGame
                }
              }
            |]
          url :: String
          url = serverUrl (getServer testEnvironment) ++ "/v1/graphql"
      response <- Http.post url mempty $ object ["query" .= queryGQL]
      let contentLengthHeaders = Http.getResponseHeader "Content-Length" response
      contentLengthHeaders `shouldSatisfy` ((== 1) . length)
