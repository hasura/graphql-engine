{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.DataConnector.MockAgent.QueryRelationshipsSpec (spec) where

--------------------------------------------------------------------------------

import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty qualified as NE
import Harness.Backend.DataConnector.Mock (TestCase (..))
import Harness.Backend.DataConnector.Mock qualified as Mock
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (yaml)
import Harness.Test.BackendType (defaultBackendTypeString, defaultSource)
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
            { Fixture.mkLocalTestEnvironment = Mock.mkLocalTestEnvironment,
              Fixture.setupTeardown = \(testEnv, mockEnv) ->
                [Mock.setupAction sourceMetadata Mock.agentConfig (testEnv, mockEnv)]
            }
        ]
    )
    tests

--------------------------------------------------------------------------------

testRoleName :: ByteString
testRoleName = "test-role"

sourceMetadata :: Aeson.Value
sourceMetadata =
  let source = defaultSource Fixture.DataConnectorMock
      backendType = defaultBackendTypeString Fixture.DataConnectorMock
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

tests :: Fixture.Options -> SpecWith (TestEnvironment, Mock.MockAgentEnvironment)
tests opts = do
  describe "Object Relationships Tests" $ do
    it "works with multiple object relationships" $
      Mock.runTest opts $
        let required =
              Mock.TestCaseRequired
                { _givenRequired =
                    let albums =
                          [ [ (API.FieldName "Name", API.mkColumnFieldValue $ Aeson.String "For Those About To Rock (We Salute You)"),
                              ( API.FieldName "Genre",
                                API.mkRelationshipFieldValue $
                                  rowsResponse
                                    [ [(API.FieldName "Name", API.mkColumnFieldValue $ Aeson.String "Rock")]
                                    ]
                              ),
                              ( API.FieldName "MediaType",
                                API.mkRelationshipFieldValue $
                                  rowsResponse
                                    [ [(API.FieldName "Name", API.mkColumnFieldValue $ Aeson.String "MPEG audio file")]
                                    ]
                              )
                            ]
                          ]
                     in Mock.chinookMock {Mock._queryResponse = \_ -> Right (rowsResponse albums)},
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
         in (Mock.defaultTestCase required)
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
                                    HashMap.fromList
                                      [ (API.FieldName "Name", API.ColumnField (API.ColumnName "Name") API.StringTy),
                                        ( API.FieldName "Genre",
                                          API.RelField
                                            ( API.RelationshipField
                                                (API.RelationshipName "Genre")
                                                API.Query
                                                  { _qFields =
                                                      Just $
                                                        HashMap.fromList
                                                          [ (API.FieldName "Name", API.ColumnField (API.ColumnName "Name") API.StringTy)
                                                          ],
                                                    _qAggregates = Nothing,
                                                    _qLimit = Nothing,
                                                    _qOffset = Nothing,
                                                    _qWhere = Nothing,
                                                    _qOrderBy = Nothing
                                                  }
                                            )
                                        ),
                                        ( API.FieldName "MediaType",
                                          API.RelField
                                            ( API.RelationshipField
                                                (API.RelationshipName "MediaType")
                                                API.Query
                                                  { _qFields =
                                                      Just $
                                                        HashMap.fromList
                                                          [ (API.FieldName "Name", API.ColumnField (API.ColumnName "Name") API.StringTy)
                                                          ],
                                                    _qAggregates = Nothing,
                                                    _qLimit = Nothing,
                                                    _qOffset = Nothing,
                                                    _qWhere = Nothing,
                                                    _qOrderBy = Nothing
                                                  }
                                            )
                                        )
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

    it "works with an order by that navigates relationships" $
      Mock.runTest opts $
        let required =
              Mock.TestCaseRequired
                { _givenRequired =
                    let albums =
                          [ [ ( API.FieldName "Album",
                                API.mkRelationshipFieldValue $
                                  rowsResponse
                                    [ [ ( API.FieldName "Artist",
                                          API.mkRelationshipFieldValue $
                                            rowsResponse
                                              [[(API.FieldName "Name", API.mkColumnFieldValue $ Aeson.String "Zeca Pagodinho")]]
                                        )
                                      ]
                                    ]
                              ),
                              (API.FieldName "Name", API.mkColumnFieldValue $ Aeson.String "Camarão que Dorme e Onda Leva")
                            ]
                          ]
                     in Mock.chinookMock {Mock._queryResponse = \_ -> Right (rowsResponse albums)},
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
         in (Mock.defaultTestCase required)
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
                                    HashMap.fromList
                                      [ (API.FieldName "Name", API.ColumnField (API.ColumnName "Name") API.StringTy),
                                        ( API.FieldName "Album",
                                          API.RelField
                                            ( API.RelationshipField
                                                (API.RelationshipName "Album")
                                                API.Query
                                                  { _qFields =
                                                      Just $
                                                        HashMap.fromList
                                                          [ ( API.FieldName "Artist",
                                                              API.RelField
                                                                ( API.RelationshipField
                                                                    (API.RelationshipName "Artist")
                                                                    API.Query
                                                                      { _qFields =
                                                                          Just $
                                                                            HashMap.fromList
                                                                              [ (API.FieldName "Name", API.ColumnField (API.ColumnName "Name") API.StringTy)
                                                                              ],
                                                                        _qAggregates = Nothing,
                                                                        _qLimit = Nothing,
                                                                        _qOffset = Nothing,
                                                                        _qWhere = Nothing,
                                                                        _qOrderBy = Nothing
                                                                      }
                                                                )
                                                            )
                                                          ],
                                                    _qAggregates = Nothing,
                                                    _qLimit = Nothing,
                                                    _qOffset = Nothing,
                                                    _qWhere = Nothing,
                                                    _qOrderBy = Nothing
                                                  }
                                            )
                                        )
                                      ],
                                _qAggregates = Nothing,
                                _qLimit = Just 1,
                                _qOffset = Nothing,
                                _qWhere = Nothing,
                                _qOrderBy =
                                  Just $
                                    API.OrderBy
                                      ( HashMap.fromList
                                          [ ( API.RelationshipName "Album",
                                              API.OrderByRelation
                                                Nothing
                                                ( HashMap.fromList
                                                    [ ( API.RelationshipName "Artist",
                                                        API.OrderByRelation
                                                          Nothing
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
      Mock.runTest opts $
        let required =
              Mock.TestCaseRequired
                { _givenRequired =
                    let albums =
                          [ [ (API.FieldName "EmployeeId", API.mkColumnFieldValue $ Aeson.Number 3)
                            ]
                          ]
                     in Mock.chinookMock {Mock._queryResponse = \_ -> Right (rowsResponse albums)},
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
         in (Mock.defaultTestCase required)
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
                                    HashMap.fromList
                                      [ (API.FieldName "EmployeeId", API.ColumnField (API.ColumnName "EmployeeId") API.NumberTy)
                                      ],
                                _qAggregates = Nothing,
                                _qLimit = Just 1,
                                _qOffset = Nothing,
                                _qWhere =
                                  Just $
                                    API.Exists (API.RelatedTable $ API.RelationshipName "SupportRepForCustomers") $
                                      API.ApplyBinaryComparisonOperator
                                        API.Equal
                                        (API.ComparisonColumn API.CurrentTable (API.ColumnName "Country") API.StringTy)
                                        (API.AnotherColumn (API.ComparisonColumn API.QueryTable (API.ColumnName "Country") API.StringTy)),
                                _qOrderBy =
                                  Just $
                                    API.OrderBy
                                      ( HashMap.fromList
                                          [ ( API.RelationshipName "SupportRepForCustomers",
                                              API.OrderByRelation
                                                ( Just $
                                                    API.Exists (API.RelatedTable $ API.RelationshipName "SupportRep") $
                                                      API.ApplyBinaryComparisonOperator
                                                        API.Equal
                                                        (API.ComparisonColumn API.CurrentTable (API.ColumnName "Country") API.StringTy)
                                                        (API.AnotherColumn (API.ComparisonColumn API.QueryTable (API.ColumnName "Country") API.StringTy))
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

rowsResponse :: [[(API.FieldName, API.FieldValue)]] -> API.QueryResponse
rowsResponse rows = API.QueryResponse (Just $ HashMap.fromList <$> rows) Nothing
