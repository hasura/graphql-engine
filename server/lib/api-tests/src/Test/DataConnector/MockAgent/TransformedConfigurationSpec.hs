{-# LANGUAGE QuasiQuotes #-}

-- | Configuration Transformation Tests for Data Connector Backend using a Mock Agent
module Test.DataConnector.MockAgent.TransformedConfigurationSpec (spec) where

--------------------------------------------------------------------------------

import Data.Aeson qualified as Aeson
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty qualified as NE
import Harness.Backend.DataConnector.Mock (AgentRequest (..), MockRequestResults (..), mockAgentTest, mockQueryResponse)
import Harness.Backend.DataConnector.Mock qualified as Mock
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (yaml)
import Harness.Test.BackendType qualified as BackendType
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment)
import Harness.Yaml (shouldBeYaml)
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, shouldBe)

--------------------------------------------------------------------------------

spec :: SpecWith GlobalTestEnvironment
spec =
  Fixture.runWithLocalTestEnvironment
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Mock.backendTypeMetadata)
            { Fixture.mkLocalTestEnvironment = Mock.mkLocalTestEnvironment,
              Fixture.setupTeardown = \(testEnv, mockEnv) ->
                [ Mock.setupAction
                    sourceMetadata
                    Mock.agentConfig
                    (testEnv, mockEnv)
                ]
            }
        ]
    )
    tests

--------------------------------------------------------------------------------

sourceMetadata :: Aeson.Value
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
                ArtistId:
                  custom_name: artist_id
            object_relationships:
              - name: artist
                using:
                  manual_configuration:
                    remote_table: [Artist]
                    column_mapping:
                      ArtistId: ArtistId
          - table: [Artist]
            configuration:
              custom_root_fields:
                select: artists
                select_by_pk: artists_by_pk
              column_config:
                ArtistId:
                  custom_name: id
                Name:
                  custom_name: name
            array_relationships:
              - name: albums
                using:
                  manual_configuration:
                    remote_table: [Album]
                    column_mapping:
                      ArtistId: ArtistId
        configuration:
          value: {}
          template: |
            {
              "DEBUG": {
                "session": {{ $session?.foo ?? "foo session default" }},
                "env":     {{ $env?.bar     ?? "bar env default"     }},
                "config":  {{ $config?.baz  ?? "baz config default"  }}
              }
            }
        |]

--------------------------------------------------------------------------------

tests :: Fixture.Options -> SpecWith (TestEnvironment, Mock.MockAgentEnvironment)
tests _opts = describe "Transformed Configuration Tests" $ do
  mockAgentTest "works with configuration transformation Kriti template" $ \performGraphqlRequest -> do
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
    let queryResponse =
          rowsResponse
            [ [ (API.FieldName "id", API.mkColumnFieldValue $ Aeson.Number 1),
                (API.FieldName "title", API.mkColumnFieldValue $ Aeson.String "For Those About To Rock We Salute You")
              ]
            ]
    let mockConfig = Mock.chinookMock & mockQueryResponse queryResponse

    MockRequestResults {..} <- performGraphqlRequest mockConfig headers graphqlRequest

    _mrrGraphqlResponse
      `shouldBeYaml` [yaml|
        data:
          albums:
            - id: 1
              title: For Those About To Rock We Salute You
      |]

    _mrrRecordedRequest
      `shouldBe` Just
        ( Query $
            API.QueryRequest
              { _qrTable = API.TableName ("Album" :| []),
                _qrTableRelationships = [],
                _qrQuery =
                  API.Query
                    { _qFields =
                        Just $
                          HashMap.fromList
                            [ (API.FieldName "id", API.ColumnField (API.ColumnName "AlbumId") $ API.ScalarType "number"),
                              (API.FieldName "title", API.ColumnField (API.ColumnName "Title") $ API.ScalarType "string")
                            ],
                      _qAggregates = Nothing,
                      _qLimit = Just 1,
                      _qOffset = Nothing,
                      _qWhere = Nothing,
                      _qOrderBy = Nothing
                    }
              }
        )

    Aeson.toJSON _mrrRecordedRequestConfig
      `shouldBeYaml` [yaml|
          DEBUG:
            config: "baz config default"
            env: "bar env default"
            session: "foo session default"
        |]

rowsResponse :: [[(API.FieldName, API.FieldValue)]] -> API.QueryResponse
rowsResponse rows = API.QueryResponse (Just $ HashMap.fromList <$> rows) Nothing
