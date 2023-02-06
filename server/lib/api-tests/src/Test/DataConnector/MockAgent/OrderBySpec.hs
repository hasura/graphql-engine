{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.DataConnector.MockAgent.OrderBySpec (spec) where

--------------------------------------------------------------------------------

import Data.Aeson qualified as Aeson
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty qualified as NE
import Harness.Backend.DataConnector.Mock (AgentRequest (..), MockRequestResults (..), mockAgentGraphqlTest, mockQueryResponse)
import Harness.Backend.DataConnector.Mock qualified as Mock
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (yaml)
import Harness.Test.BackendType qualified as BackendType
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment)
import Harness.Yaml (shouldBeYaml)
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Prelude
import Language.GraphQL.Draft.Syntax.QQ qualified as G
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
            object_relationships:
              - name: Artist
                using:
                  manual_configuration:
                    remote_table: [Artist]
                    column_mapping:
                      ArtistId: ArtistId
          - table: [Artist]
            array_relationships:
              - name: Albums
                using:
                  manual_configuration:
                    remote_table: [Album]
                    column_mapping:
                      ArtistId: ArtistId
        configuration: {}
      |]

--------------------------------------------------------------------------------

tests :: Fixture.Options -> SpecWith (TestEnvironment, Mock.MockAgentEnvironment)
tests _opts = describe "Order By Tests" $ do
  mockAgentGraphqlTest "can order by column" $ \performGraphqlRequest -> do
    let headers = []
    let graphqlRequest =
          [graphql|
            query getAlbum {
              Album(limit: 3, order_by: {AlbumId: asc}) {
                AlbumId
                Title
              }
            }
          |]
    let queryResponse =
          rowsResponse
            [ [ (API.FieldName "AlbumId", API.mkColumnFieldValue $ Aeson.Number 1),
                (API.FieldName "Title", API.mkColumnFieldValue $ Aeson.String "For Those About To Rock We Salute You")
              ],
              [ (API.FieldName "AlbumId", API.mkColumnFieldValue $ Aeson.Number 2),
                (API.FieldName "Title", API.mkColumnFieldValue $ Aeson.String "Balls to the Wall")
              ],
              [ (API.FieldName "AlbumId", API.mkColumnFieldValue $ Aeson.Number 3),
                (API.FieldName "Title", API.mkColumnFieldValue $ Aeson.String "Restless and Wild")
              ]
            ]
    let mockConfig = Mock.chinookMock & mockQueryResponse queryResponse

    MockRequestResults {..} <- performGraphqlRequest mockConfig headers graphqlRequest

    _mrrResponse
      `shouldBeYaml` [yaml|
        data:
          Album:
            - AlbumId: 1
              Title: For Those About To Rock We Salute You
            - AlbumId: 2
              Title: Balls to the Wall
            - AlbumId: 3
              Title: Restless and Wild
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
                            [ (API.FieldName "AlbumId", API.ColumnField (API.ColumnName "AlbumId") $ API.ScalarType "number"),
                              (API.FieldName "Title", API.ColumnField (API.ColumnName "Title") $ API.ScalarType "string")
                            ],
                      _qAggregates = Nothing,
                      _qLimit = Just 3,
                      _qOffset = Nothing,
                      _qWhere = Nothing,
                      _qOrderBy = Just (API.OrderBy mempty (API.OrderByElement [] (API.OrderByColumn (API.ColumnName "AlbumId")) API.Ascending :| []))
                    }
              }
        )

  mockAgentGraphqlTest "can order by aggregates" $ \performGraphqlRequest -> do
    let headers = []
    let graphqlRequest =
          [graphql|
            query getArtists {
              Artist(order_by: {Albums_aggregate: {count: asc, max: {AlbumId: asc}}}, limit: 2) {
                Name
              }
            }
          |]
    let queryResponse =
          rowsResponse
            [ [(API.FieldName "Name", API.mkColumnFieldValue $ Aeson.String "Milton Nascimento & Bebeto")],
              [(API.FieldName "Name", API.mkColumnFieldValue $ Aeson.String "Azymuth")]
            ]
    let mockConfig = Mock.chinookMock & mockQueryResponse queryResponse

    MockRequestResults {..} <- performGraphqlRequest mockConfig headers graphqlRequest

    _mrrResponse
      `shouldBeYaml` [yaml|
        data:
          Artist:
            - Name: Milton Nascimento & Bebeto
            - Name: Azymuth
      |]

    _mrrRecordedRequest
      `shouldBe` Just
        ( Query $
            API.QueryRequest
              { _qrTable = API.TableName ("Artist" :| []),
                _qrTableRelationships =
                  [ API.TableRelationships
                      { _trSourceTable = API.TableName ("Artist" :| []),
                        _trRelationships =
                          HashMap.fromList
                            [ ( API.RelationshipName "Albums",
                                API.Relationship
                                  { _rTargetTable = API.TableName ("Album" :| []),
                                    _rRelationshipType = API.ArrayRelationship,
                                    _rColumnMapping = HashMap.fromList [(API.ColumnName "ArtistId", API.ColumnName "ArtistId")]
                                  }
                              )
                            ]
                      }
                  ],
                _qrQuery =
                  API.Query
                    { _qFields =
                        Just $
                          HashMap.fromList
                            [ (API.FieldName "Name", API.ColumnField (API.ColumnName "Name") (API.ScalarType "string"))
                            ],
                      _qAggregates = Nothing,
                      _qLimit = Just 2,
                      _qOffset = Nothing,
                      _qWhere = Nothing,
                      _qOrderBy =
                        Just $
                          API.OrderBy
                            ( HashMap.fromList
                                [ ( API.RelationshipName "Albums",
                                    API.OrderByRelation Nothing mempty
                                  )
                                ]
                            )
                            ( NE.fromList
                                [ API.OrderByElement [API.RelationshipName "Albums"] API.OrderByStarCountAggregate API.Ascending,
                                  API.OrderByElement
                                    [API.RelationshipName "Albums"]
                                    ( API.OrderBySingleColumnAggregate $
                                        API.SingleColumnAggregate
                                          (API.SingleColumnAggregateFunction [G.name|max|])
                                          (API.ColumnName "AlbumId")
                                          (API.ScalarType "number")
                                    )
                                    API.Ascending
                                ]
                            )
                    }
              }
        )

rowsResponse :: [[(API.FieldName, API.FieldValue)]] -> API.QueryResponse
rowsResponse rows = API.QueryResponse (Just $ HashMap.fromList <$> rows) Nothing
