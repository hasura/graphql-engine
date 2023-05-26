{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Tests for Error Conditions in Data Connector Backends
module Test.DataConnector.MockAgent.ErrorSpec (spec) where

--------------------------------------------------------------------------------

import Control.Lens ((?~))
import Data.Aeson qualified as J
import Data.List.NonEmpty qualified as NE
import Harness.Backend.DataConnector.Mock (AgentRequest (..), MockRequestResults (..), mockAgentGraphqlTest)
import Harness.Backend.DataConnector.Mock qualified as Mock
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (yaml)
import Harness.Test.BackendType qualified as BackendType
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment)
import Harness.Yaml (shouldBeYaml)
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Prelude
import Test.DataConnector.MockAgent.TestHelpers
import Test.Hspec (SpecWith, describe, shouldBe)

--------------------------------------------------------------------------------

spec :: SpecWith GlobalTestEnvironment
spec =
  Fixture.runWithLocalTestEnvironment
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Mock.backendTypeMetadata)
            { Fixture.mkLocalTestEnvironment = Mock.mkLocalTestEnvironment,
              Fixture.setupTeardown = \(testEnv, mockEnv) ->
                [Mock.setupAction sourceMetadata Mock.agentConfig (testEnv, mockEnv)]
            }
        ]
    )
    tests

sourceMetadata :: J.Value
sourceMetadata =
  let source = BackendType.backendSourceName Mock.backendTypeMetadata
      backendType = BackendType.backendTypeString Mock.backendTypeMetadata
   in [yaml|
        name : *source
        kind: *backendType
        tables:
          - table: [Album]
            configuration:
              custom_root_fields:
                select: albums
                select_by_pk: albums_by_pk
              column_config:
                AlbumId:
                  custom_name: id
                Title:
                  custom_name: title
        configuration: {}
      |]

--------------------------------------------------------------------------------

tests :: SpecWith (TestEnvironment, Mock.MockAgentEnvironment)
tests = describe "Error Protocol Tests" $ do
  mockAgentGraphqlTest "handles returned errors correctly" $ \_testEnv performGraphqlRequest -> do
    let headers = []
    let graphqlRequest =
          [graphql|
            query getAlbum {
              albums(limit: 1) {
                id
                title
              }
            }
          |]
    let errorResponse = API.ErrorResponse API.UncaughtError "Hello World!" [yaml| { foo: "bar" } |]
    let mockConfig = Mock.defaultMockRequestConfig {Mock._queryResponse = \_ -> Left errorResponse}

    MockRequestResults {..} <- performGraphqlRequest mockConfig headers graphqlRequest

    _mrrResponse
      `shouldBeYaml` [yaml|
        errors:
          -
            extensions:
              code: "data-connector-error"
              path: "$"
              internal:
                foo: "bar"
            message: "UncaughtError: Hello World!"
      |]

    _mrrRecordedRequest
      `shouldBe` Just
        ( Query $
            mkTableRequest
              (mkTableName "Album")
              ( emptyQuery
                  & API.qFields
                    ?~ mkFieldsMap
                      [ ("id", API.ColumnField (API.ColumnName "AlbumId") $ API.ScalarType "number"),
                        ("title", API.ColumnField (API.ColumnName "Title") $ API.ScalarType "string")
                      ]
                  & API.qLimit ?~ 1
              )
        )
