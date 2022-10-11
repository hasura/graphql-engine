{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Query Tests for Data Connector Backend using a Mock Agent
module Test.DataConnector.MockAgent.BasicQuerySpec
  ( spec,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty qualified as NE
import Harness.Backend.DataConnector (TestCase (..))
import Harness.Backend.DataConnector qualified as DataConnector
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (yaml)
import Harness.Test.BackendType (BackendType (..), defaultBackendTypeString, defaultSource)
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (TestEnvironment)
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

--------------------------------------------------------------------------------

spec :: SpecWith TestEnvironment
spec =
  Fixture.runWithLocalTestEnvironment
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Fixture.DataConnectorMock)
            { Fixture.mkLocalTestEnvironment = DataConnector.mkLocalTestEnvironmentMock,
              Fixture.setupTeardown = \(testEnv, mockEnv) ->
                [DataConnector.setupMockAction sourceMetadata DataConnector.mockBackendConfig (testEnv, mockEnv)]
            }
        ]
    )
    tests

testRoleName :: ByteString
testRoleName = "test-role"

sourceMetadata :: Aeson.Value
sourceMetadata =
  let source = defaultSource DataConnectorMock
      backendType = defaultBackendTypeString DataConnectorMock
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
          - table: [Employee]
          - table: [Customer]
            select_permissions:
              - role: *testRoleName
                permission:
                  columns:
                    - CustomerId
                  filter:
                    _exists:
                      _table: [Employee]
                      _where:
                        EmployeeId:
                          _eq: X-Hasura-EmployeeId
        configuration: {}
      |]

--------------------------------------------------------------------------------

tests :: Fixture.Options -> SpecWith (TestEnvironment, DataConnector.MockAgentEnvironment)
tests opts = do
  describe "Basic Tests" $ do
    it "works with simple object query" $
      DataConnector.runMockedTest opts $
        let required =
              DataConnector.TestCaseRequired
                { _givenRequired =
                    let albums =
                          [ [ (API.FieldName "id", API.mkColumnFieldValue $ Aeson.Number 1),
                              (API.FieldName "title", API.mkColumnFieldValue $ Aeson.String "For Those About To Rock We Salute You")
                            ]
                          ]
                     in DataConnector.chinookMock {DataConnector._queryResponse = \_ -> Right (rowsResponse albums)},
                  _whenRequestRequired =
                    [graphql|
                      query getAlbum {
                        albums(limit: 1) {
                          id
                          title
                        }
                      }
                    |],
                  _thenRequired =
                    [yaml|
                      data:
                        albums:
                          - id: 1
                            title: For Those About To Rock We Salute You
                    |]
                }
         in (DataConnector.defaultTestCase required)
              { _whenQuery =
                  Just
                    ( API.QueryRequest
                        { _qrTable = API.TableName ("Album" :| []),
                          _qrTableRelationships = [],
                          _qrQuery =
                            API.Query
                              { _qFields =
                                  Just $
                                    HashMap.fromList
                                      [ (API.FieldName "id", API.ColumnField (API.ColumnName "AlbumId")),
                                        (API.FieldName "title", API.ColumnField (API.ColumnName "Title"))
                                      ],
                                _qAggregates = Nothing,
                                _qLimit = Just 1,
                                _qOffset = Nothing,
                                _qWhere = Nothing,
                                _qOrderBy = Nothing
                              }
                        }
                    )
              }

    it "works with order_by id" $
      DataConnector.runMockedTest opts $
        let required =
              DataConnector.TestCaseRequired
                { _givenRequired =
                    let albums =
                          [ [ (API.FieldName "id", API.mkColumnFieldValue $ Aeson.Number 1),
                              (API.FieldName "title", API.mkColumnFieldValue $ Aeson.String "For Those About To Rock We Salute You")
                            ],
                            [ (API.FieldName "id", API.mkColumnFieldValue $ Aeson.Number 2),
                              (API.FieldName "title", API.mkColumnFieldValue $ Aeson.String "Balls to the Wall")
                            ],
                            [ (API.FieldName "id", API.mkColumnFieldValue $ Aeson.Number 3),
                              (API.FieldName "title", API.mkColumnFieldValue $ Aeson.String "Restless and Wild")
                            ]
                          ]
                     in DataConnector.chinookMock {DataConnector._queryResponse = \_ -> Right (rowsResponse albums)},
                  _whenRequestRequired =
                    [graphql|
                      query getAlbum {
                        albums(limit: 3, order_by: {id: asc}) {
                          id
                          title
                        }
                      }
                    |],
                  _thenRequired =
                    [yaml|
                      data:
                        albums:
                          - id: 1
                            title: For Those About To Rock We Salute You
                          - id: 2
                            title: Balls to the Wall
                          - id: 3
                            title: Restless and Wild
                    |]
                }
         in (DataConnector.defaultTestCase required)
              { _whenQuery =
                  Just
                    ( API.QueryRequest
                        { _qrTable = API.TableName ("Album" :| []),
                          _qrTableRelationships = [],
                          _qrQuery =
                            API.Query
                              { _qFields =
                                  Just $
                                    HashMap.fromList
                                      [ (API.FieldName "id", API.ColumnField (API.ColumnName "AlbumId")),
                                        (API.FieldName "title", API.ColumnField (API.ColumnName "Title"))
                                      ],
                                _qAggregates = Nothing,
                                _qLimit = Just 3,
                                _qOffset = Nothing,
                                _qWhere = Nothing,
                                _qOrderBy = Just (API.OrderBy mempty (API.OrderByElement [] (API.OrderByColumn (API.ColumnName "AlbumId")) API.Ascending :| []))
                              }
                        }
                    )
              }

    it "works with an exists-based permissions filter" $
      DataConnector.runMockedTest opts $
        let required =
              DataConnector.TestCaseRequired
                { _givenRequired =
                    let albums =
                          [ [ (API.FieldName "CustomerId", API.mkColumnFieldValue $ Aeson.Number 1)
                            ],
                            [ (API.FieldName "CustomerId", API.mkColumnFieldValue $ Aeson.Number 2)
                            ],
                            [ (API.FieldName "CustomerId", API.mkColumnFieldValue $ Aeson.Number 3)
                            ]
                          ]
                     in DataConnector.chinookMock {DataConnector._queryResponse = \_ -> Right (rowsResponse albums)},
                  _whenRequestRequired =
                    [graphql|
                      query getCustomers {
                        Customer {
                          CustomerId
                        }
                      }
                    |],
                  _thenRequired =
                    [yaml|
                      data:
                        Customer:
                          - CustomerId: 1
                          - CustomerId: 2
                          - CustomerId: 3
                    |]
                }
         in (DataConnector.defaultTestCase required)
              { _whenRequestHeaders =
                  [ ("X-Hasura-Role", testRoleName),
                    ("X-Hasura-EmployeeId", "1")
                  ],
                _whenQuery =
                  Just
                    ( API.QueryRequest
                        { _qrTable = API.TableName ("Customer" :| []),
                          _qrTableRelationships = [],
                          _qrQuery =
                            API.Query
                              { _qFields =
                                  Just $
                                    HashMap.fromList
                                      [ (API.FieldName "CustomerId", API.ColumnField (API.ColumnName "CustomerId"))
                                      ],
                                _qAggregates = Nothing,
                                _qLimit = Nothing,
                                _qOffset = Nothing,
                                _qWhere =
                                  Just $
                                    API.Exists (API.UnrelatedTable $ API.TableName ("Employee" :| [])) $
                                      API.ApplyBinaryComparisonOperator
                                        API.Equal
                                        (API.ComparisonColumn API.CurrentTable (API.ColumnName "EmployeeId"))
                                        (API.ScalarValue (Aeson.Number 1)),
                                _qOrderBy = Nothing
                              }
                        }
                    )
              }

rowsResponse :: [[(API.FieldName, API.FieldValue)]] -> API.QueryResponse
rowsResponse rows = API.QueryResponse (Just $ HashMap.fromList <$> rows) Nothing
