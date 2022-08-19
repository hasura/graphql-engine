{-# LANGUAGE QuasiQuotes #-}

module Test.DataConnector.MockAgent.QueryRelationshipsSpec
  ( spec,
  )
where

import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
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

spec :: SpecWith TestEnvironment
spec =
  Fixture.runWithLocalTestEnvironment
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Fixture.DataConnector)
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
  let source = defaultSource DataConnector
      backendType = defaultBackendTypeString DataConnector
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
          - table: [Genre]
          - table: [MediaType]
          - table: [Track]
            object_relationships:
              - name: Album
                using:
                  manual_configuration:
                    remote_table: [Album]
                    column_mapping:
                      AlbumId: AlbumId
              - name: Genre
                using:
                  manual_configuration:
                    remote_table: [Genre]
                    column_mapping:
                      GenreId: GenreId
              - name: MediaType
                using:
                  manual_configuration:
                    remote_table: [MediaType]
                    column_mapping:
                      MediaTypeId: MediaTypeId
          - table: [Employee]
            array_relationships:
              - name: SupportRepForCustomers
                using:
                  manual_configuration:
                    remote_table: [Customer]
                    column_mapping:
                      EmployeeId: SupportRepId
            select_permissions:
              - role: *testRoleName
                permission:
                  columns:
                    - EmployeeId
                    - FirstName
                    - LastName
                    - Country
                  filter:
                    SupportRepForCustomers:
                      Country:
                        _ceq: [ "$", "Country" ]
          - table: [Customer]
            object_relationships:
              - name: SupportRep
                using:
                  manual_configuration:
                    remote_table: [Employee]
                    column_mapping:
                      SupportRepId: EmployeeId
            select_permissions:
              - role: *testRoleName
                permission:
                  columns:
                    - CustomerId
                    - FirstName
                    - LastName
                    - Country
                    - SupportRepId
                  filter:
                    SupportRep:
                      Country:
                        _ceq: [ "$", "Country" ]
        configuration: {}
      |]

--------------------------------------------------------------------------------

tests :: Fixture.Options -> SpecWith (TestEnvironment, DataConnector.MockAgentEnvironment)
tests opts = do
  describe "Object Relationships Tests" $ do
    it "works with multiple object relationships" $
      DataConnector.runMockedTest opts $
        let required =
              DataConnector.TestCaseRequired
                { _givenRequired =
                    let albums =
                          [ [ ("Name", API.mkColumnFieldValue $ Aeson.String "For Those About To Rock (We Salute You)"),
                              ( "Genre",
                                API.mkRelationshipFieldValue $
                                  rowsResponse
                                    [ [("Name", API.mkColumnFieldValue $ Aeson.String "Rock")]
                                    ]
                              ),
                              ( "MediaType",
                                API.mkRelationshipFieldValue $
                                  rowsResponse
                                    [ [("Name", API.mkColumnFieldValue $ Aeson.String "MPEG audio file")]
                                    ]
                              )
                            ]
                          ]
                     in DataConnector.chinookMock {DataConnector._queryResponse = \_ -> rowsResponse albums},
                  _whenRequestRequired =
                    [graphql|
                      query getTrack {
                        Track(limit: 1) {
                          Name
                          Genre {
                            Name
                          }
                          MediaType {
                            Name
                          }
                        }
                      }
                    |],
                  _thenRequired =
                    [yaml|
                      data:
                        Track:
                          - Genre:
                              Name: "Rock"
                            MediaType:
                              Name: "MPEG audio file"
                            Name: "For Those About To Rock (We Salute You)"
                    |]
                }
         in (DataConnector.defaultTestCase required)
              { _whenQuery =
                  Just
                    ( API.QueryRequest
                        { _qrTable = API.TableName ("Track" :| []),
                          _qrTableRelationships =
                            [ API.TableRelationships
                                { _trSourceTable = API.TableName ("Track" :| []),
                                  _trRelationships =
                                    HashMap.fromList
                                      [ ( API.RelationshipName "Genre",
                                          API.Relationship
                                            { _rTargetTable = API.TableName ("Genre" :| []),
                                              _rRelationshipType = API.ObjectRelationship,
                                              _rColumnMapping = HashMap.fromList [(API.ColumnName "GenreId", API.ColumnName "GenreId")]
                                            }
                                        ),
                                        ( API.RelationshipName "MediaType",
                                          API.Relationship
                                            { _rTargetTable = API.TableName ("MediaType" :| []),
                                              _rRelationshipType = API.ObjectRelationship,
                                              _rColumnMapping =
                                                HashMap.fromList
                                                  [(API.ColumnName "MediaTypeId", API.ColumnName "MediaTypeId")]
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
                                      [ ("Name", API.ColumnField (API.ColumnName "Name")),
                                        ( "Genre",
                                          API.RelField
                                            ( API.RelationshipField
                                                (API.RelationshipName "Genre")
                                                API.Query
                                                  { _qFields =
                                                      Just $
                                                        KM.fromList
                                                          [ ("Name", API.ColumnField (API.ColumnName "Name"))
                                                          ],
                                                    _qAggregates = Nothing,
                                                    _qLimit = Nothing,
                                                    _qOffset = Nothing,
                                                    _qWhere = Just (API.And []),
                                                    _qOrderBy = Nothing
                                                  }
                                            )
                                        ),
                                        ( "MediaType",
                                          API.RelField
                                            ( API.RelationshipField
                                                (API.RelationshipName "MediaType")
                                                API.Query
                                                  { _qFields =
                                                      Just $
                                                        KM.fromList
                                                          [ ("Name", API.ColumnField (API.ColumnName "Name"))
                                                          ],
                                                    _qAggregates = Nothing,
                                                    _qLimit = Nothing,
                                                    _qOffset = Nothing,
                                                    _qWhere = Just (API.And []),
                                                    _qOrderBy = Nothing
                                                  }
                                            )
                                        )
                                      ],
                                _qAggregates = Nothing,
                                _qLimit = Just 1,
                                _qOffset = Nothing,
                                _qWhere = Just (API.And []),
                                _qOrderBy = Nothing
                              }
                        }
                    )
              }

    it "works with an order by that navigates relationships" $
      DataConnector.runMockedTest opts $
        let required =
              DataConnector.TestCaseRequired
                { _givenRequired =
                    let albums =
                          [ [ ( "Album",
                                API.mkRelationshipFieldValue $
                                  rowsResponse
                                    [ [ ( "Artist",
                                          API.mkRelationshipFieldValue $
                                            rowsResponse
                                              [[("Name", API.mkColumnFieldValue $ Aeson.String "Zeca Pagodinho")]]
                                        )
                                      ]
                                    ]
                              ),
                              ("Name", API.mkColumnFieldValue $ Aeson.String "Camarão que Dorme e Onda Leva")
                            ]
                          ]
                     in DataConnector.chinookMock {DataConnector._queryResponse = \_ -> rowsResponse albums},
                  _whenRequestRequired =
                    [graphql|
                      query getTrack {
                        Track(order_by: [{Album: {Artist: {Name: desc}}}, { Name: asc }], limit: 1) {
                          Album {
                            Artist {
                              Name
                            }
                          }
                          Name
                        }
                      }
                    |],
                  _thenRequired =
                    [yaml|
                      data:
                        Track:
                          - Album:
                              Artist:
                                Name: Zeca Pagodinho
                            Name: Camarão que Dorme e Onda Leva
                    |]
                }
         in (DataConnector.defaultTestCase required)
              { _whenQuery =
                  Just
                    ( API.QueryRequest
                        { _qrTable = API.TableName ("Track" :| []),
                          _qrTableRelationships =
                            [ API.TableRelationships
                                { _trSourceTable = API.TableName ("Track" :| []),
                                  _trRelationships =
                                    HashMap.fromList
                                      [ ( API.RelationshipName "Album",
                                          API.Relationship
                                            { _rTargetTable = API.TableName ("Album" :| []),
                                              _rRelationshipType = API.ObjectRelationship,
                                              _rColumnMapping = HashMap.fromList [(API.ColumnName "AlbumId", API.ColumnName "AlbumId")]
                                            }
                                        )
                                      ]
                                },
                              API.TableRelationships
                                { _trSourceTable = API.TableName ("Album" :| []),
                                  _trRelationships =
                                    HashMap.fromList
                                      [ ( API.RelationshipName "Artist",
                                          API.Relationship
                                            { _rTargetTable = API.TableName ("Artist" :| []),
                                              _rRelationshipType = API.ObjectRelationship,
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
                                      [ ("Name", API.ColumnField (API.ColumnName "Name")),
                                        ( "Album",
                                          API.RelField
                                            ( API.RelationshipField
                                                (API.RelationshipName "Album")
                                                API.Query
                                                  { _qFields =
                                                      Just $
                                                        KM.fromList
                                                          [ ( "Artist",
                                                              API.RelField
                                                                ( API.RelationshipField
                                                                    (API.RelationshipName "Artist")
                                                                    API.Query
                                                                      { _qFields =
                                                                          Just $
                                                                            KM.fromList
                                                                              [ ("Name", API.ColumnField (API.ColumnName "Name"))
                                                                              ],
                                                                        _qAggregates = Nothing,
                                                                        _qLimit = Nothing,
                                                                        _qOffset = Nothing,
                                                                        _qWhere = Just (API.And []),
                                                                        _qOrderBy = Nothing
                                                                      }
                                                                )
                                                            )
                                                          ],
                                                    _qAggregates = Nothing,
                                                    _qLimit = Nothing,
                                                    _qOffset = Nothing,
                                                    _qWhere = Just (API.And []),
                                                    _qOrderBy = Nothing
                                                  }
                                            )
                                        )
                                      ],
                                _qAggregates = Nothing,
                                _qLimit = Just 1,
                                _qOffset = Nothing,
                                _qWhere = Just (API.And []),
                                _qOrderBy =
                                  Just $
                                    API.OrderBy
                                      ( HashMap.fromList
                                          [ ( API.RelationshipName "Album",
                                              API.OrderByRelation
                                                (Just $ API.And [])
                                                ( HashMap.fromList
                                                    [ ( API.RelationshipName "Artist",
                                                        API.OrderByRelation
                                                          (Just $ API.And [])
                                                          mempty
                                                      )
                                                    ]
                                                )
                                            )
                                          ]
                                      )
                                      ( NE.fromList
                                          [ API.OrderByElement [API.RelationshipName "Album", API.RelationshipName "Artist"] (API.OrderByColumn (API.ColumnName "Name")) API.Descending,
                                            API.OrderByElement [] (API.OrderByColumn (API.ColumnName "Name")) API.Ascending
                                          ]
                                      )
                              }
                        }
                    )
              }

    it "works with an order by that navigates a relationship with table permissions" $
      DataConnector.runMockedTest opts $
        let required =
              DataConnector.TestCaseRequired
                { _givenRequired =
                    let albums =
                          [ [ ("EmployeeId", API.mkColumnFieldValue $ Aeson.Number 3)
                            ]
                          ]
                     in DataConnector.chinookMock {DataConnector._queryResponse = \_ -> rowsResponse albums},
                  _whenRequestRequired =
                    [graphql|
                      query getEmployee {
                        Employee(limit: 1, order_by: {SupportRepForCustomers_aggregate: {count: desc}}) {
                          EmployeeId
                        }
                      }
                    |],
                  _thenRequired =
                    [yaml|
                      data:
                        Employee:
                          - EmployeeId: 3
                    |]
                }
         in (DataConnector.defaultTestCase required)
              { _whenRequestHeaders = [("X-Hasura-Role", testRoleName)],
                _whenQuery =
                  Just
                    ( API.QueryRequest
                        { _qrTable = API.TableName ("Employee" :| []),
                          _qrTableRelationships =
                            [ API.TableRelationships
                                { _trSourceTable = API.TableName ("Customer" :| []),
                                  _trRelationships =
                                    HashMap.fromList
                                      [ ( API.RelationshipName "SupportRep",
                                          API.Relationship
                                            { _rTargetTable = API.TableName ("Employee" :| []),
                                              _rRelationshipType = API.ObjectRelationship,
                                              _rColumnMapping = HashMap.fromList [(API.ColumnName "SupportRepId", API.ColumnName "EmployeeId")]
                                            }
                                        )
                                      ]
                                },
                              API.TableRelationships
                                { _trSourceTable = API.TableName ("Employee" :| []),
                                  _trRelationships =
                                    HashMap.fromList
                                      [ ( API.RelationshipName "SupportRepForCustomers",
                                          API.Relationship
                                            { _rTargetTable = API.TableName ("Customer" :| []),
                                              _rRelationshipType = API.ArrayRelationship,
                                              _rColumnMapping = HashMap.fromList [(API.ColumnName "EmployeeId", API.ColumnName "SupportRepId")]
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
                                      [ ("EmployeeId", API.ColumnField (API.ColumnName "EmployeeId"))
                                      ],
                                _qAggregates = Nothing,
                                _qLimit = Just 1,
                                _qOffset = Nothing,
                                _qWhere =
                                  Just $
                                    API.ApplyBinaryComparisonOperator
                                      API.Equal
                                      (API.ComparisonColumn [API.RelationshipName "SupportRepForCustomers"] (API.ColumnName "Country"))
                                      (API.AnotherColumn (API.ComparisonColumn [] (API.ColumnName "Country"))),
                                _qOrderBy =
                                  Just $
                                    API.OrderBy
                                      ( HashMap.fromList
                                          [ ( API.RelationshipName "SupportRepForCustomers",
                                              API.OrderByRelation
                                                ( Just $
                                                    API.ApplyBinaryComparisonOperator
                                                      API.Equal
                                                      (API.ComparisonColumn [API.RelationshipName "SupportRep"] (API.ColumnName "Country"))
                                                      (API.AnotherColumn (API.ComparisonColumn [] (API.ColumnName "Country")))
                                                )
                                                mempty
                                            )
                                          ]
                                      )
                                      (API.OrderByElement [API.RelationshipName "SupportRepForCustomers"] API.OrderByStarCountAggregate API.Descending :| [])
                              }
                        }
                    )
              }

rowsResponse :: [[(Aeson.Key, API.FieldValue)]] -> API.QueryResponse
rowsResponse rows = API.QueryResponse (Just $ KM.fromList <$> rows) Nothing
