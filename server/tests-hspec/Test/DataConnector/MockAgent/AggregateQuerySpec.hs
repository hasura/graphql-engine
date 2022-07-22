{-# LANGUAGE QuasiQuotes #-}

module Test.DataConnector.MockAgent.AggregateQuerySpec
  ( spec,
  )
where

import Autodocodec.Extended (ValueWrapper (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.HashMap.Strict qualified as HashMap
import Harness.Backend.DataConnector (TestCase (..))
import Harness.Backend.DataConnector qualified as DataConnector
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (yaml)
import Harness.Test.BackendType (BackendType (..), defaultBackendTypeString, defaultSource)
import Harness.Test.Context qualified as Context
import Harness.TestEnvironment (TestEnvironment)
import Hasura.Backends.DataConnector.API qualified as API
import Test.Hspec (SpecWith, describe, it)
import Prelude

spec :: SpecWith TestEnvironment
spec =
  Context.runWithLocalTestEnvironment
    [ Context.Context
        { name = Context.Backend Context.DataConnector,
          mkLocalTestEnvironment = DataConnector.mkLocalTestEnvironmentMock,
          setup = DataConnector.setupMock sourceMetadata DataConnector.mockBackendConfig,
          teardown = DataConnector.teardownMock,
          customOptions = Nothing
        }
    ]
    tests

sourceMetadata :: Aeson.Value
sourceMetadata =
  let source = defaultSource DataConnector
      backendType = defaultBackendTypeString DataConnector
   in [yaml|
        name : *source
        kind: *backendType
        tables:
          - table: Album
            object_relationships:
              - name: Artist
                using:
                  manual_configuration:
                    remote_table: Artist
                    column_mapping:
                      ArtistId: ArtistId
          - table: Artist
            array_relationships:
              - name: Albums
                using:
                  manual_configuration:
                    remote_table: Album
                    column_mapping:
                      ArtistId: ArtistId
        configuration: {}
      |]

--------------------------------------------------------------------------------

tests :: Context.Options -> SpecWith (TestEnvironment, DataConnector.MockAgentEnvironment)
tests opts = describe "Nodes Tests" $ do
  it "works with multiple nodes fields and through array relations" $
    DataConnector.runMockedTest opts $
      let required =
            DataConnector.TestCaseRequired
              { _givenRequired =
                  let response =
                        [ [ ("Artists_ArtistId", API.ColumnFieldValue . ValueWrapper $ API.Number 1),
                            ( "nodes_Albums",
                              API.RelationshipFieldValue . ValueWrapper $
                                rowsResponse
                                  [ [("nodes_Title", API.ColumnFieldValue . ValueWrapper $ API.String "For Those About To Rock We Salute You")],
                                    [("nodes_Title", API.ColumnFieldValue . ValueWrapper $ API.String "Let There Be Rock")]
                                  ]
                            )
                          ]
                        ]
                   in DataConnector.chinookMock {DataConnector._queryResponse = \_ -> rowsResponse response},
                _whenRequestRequired =
                  [graphql|
                    query getArtist {
                      Artist_aggregate(limit: 1) {
                        Artists: nodes {
                          ArtistId
                        }
                        nodes {
                          Albums: Albums_aggregate {
                            nodes {
                              Title
                            }
                          }
                        }
                      }
                    }
                  |],
                _thenRequired =
                  [yaml|
                    data:
                      Artist_aggregate:
                        Artists:
                          - ArtistId: 1
                        nodes:
                          - Albums:
                              nodes:
                                - Title: For Those About To Rock We Salute You
                                - Title: Let There Be Rock
                    |]
              }
       in (DataConnector.defaultTestCase required)
            { _whenQuery =
                Just
                  ( API.QueryRequest
                      { _qrTable = API.TableName "Artist",
                        _qrTableRelationships =
                          [ API.TableRelationships
                              { _trSourceTable = API.TableName "Artist",
                                _trRelationships =
                                  HashMap.fromList
                                    [ ( API.RelationshipName "Albums",
                                        API.Relationship
                                          { _rTargetTable = API.TableName "Album",
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
                                  KM.fromList
                                    [ ("Artists_ArtistId", API.ColumnField (ValueWrapper (API.ColumnName "ArtistId"))),
                                      ( "nodes_Albums",
                                        API.RelField
                                          ( API.RelationshipField
                                              (API.RelationshipName "Albums")
                                              API.Query
                                                { _qFields =
                                                    Just $
                                                      KM.fromList
                                                        [ ("nodes_Title", API.ColumnField (ValueWrapper (API.ColumnName "Title")))
                                                        ],
                                                  _qAggregates = Nothing,
                                                  _qLimit = Nothing,
                                                  _qOffset = Nothing,
                                                  _qWhere = Just (API.And (ValueWrapper [])),
                                                  _qOrderBy = Nothing
                                                }
                                          )
                                      )
                                    ],
                              _qAggregates = Nothing,
                              _qLimit = Just 1,
                              _qOffset = Nothing,
                              _qWhere = Just (API.And (ValueWrapper [])),
                              _qOrderBy = Nothing
                            }
                      }
                  )
            }

rowsResponse :: [[(Aeson.Key, API.FieldValue)]] -> API.QueryResponse
rowsResponse rows = API.QueryResponse (Just $ KM.fromList <$> rows) Nothing
