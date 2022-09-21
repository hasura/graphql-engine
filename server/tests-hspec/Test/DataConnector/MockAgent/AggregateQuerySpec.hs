{-# LANGUAGE QuasiQuotes #-}

module Test.DataConnector.MockAgent.AggregateQuerySpec
  ( spec,
  )
where

import Data.Aeson qualified as Aeson
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
        [ (Fixture.fixture $ Fixture.Backend Fixture.DataConnectorMock)
            { Fixture.mkLocalTestEnvironment =
                DataConnector.mkLocalTestEnvironmentMock,
              Fixture.setupTeardown = \(testEnv, mockEnv) ->
                [DataConnector.setupMockAction sourceMetadata DataConnector.mockBackendConfig (testEnv, mockEnv)]
            }
        ]
    )
    tests

sourceMetadata :: Aeson.Value
sourceMetadata =
  let source = defaultSource DataConnectorMock
      backendType = defaultBackendTypeString DataConnectorMock
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
          - table: [Invoice]
            array_relationships:
              - name: InvoiceLines
                using:
                  manual_configuration:
                    remote_table: [InvoiceLine]
                    column_mapping:
                      InvoiceId: InvoiceId
          - table: [InvoiceLine]
            object_relationships:
              - name: Invoice
                using:
                  manual_configuration:
                    remote_table: [Invoice]
                    column_mapping:
                      InvoiceId: InvoiceId
        configuration: {}
      |]

--------------------------------------------------------------------------------

tests :: Fixture.Options -> SpecWith (TestEnvironment, DataConnector.MockAgentEnvironment)
tests opts = describe "Aggregate Query Tests" $ do
  it "works with multiple nodes fields and through array relations" $
    DataConnector.runMockedTest opts $
      let required =
            DataConnector.TestCaseRequired
              { _givenRequired =
                  let response =
                        [ [ (API.FieldName "ArtistIds_Id", API.mkColumnFieldValue $ Aeson.Number 1),
                            (API.FieldName "ArtistNames_Name", API.mkColumnFieldValue $ Aeson.String "AC/DC"),
                            ( API.FieldName "nodes_Albums",
                              API.mkRelationshipFieldValue $
                                rowsResponse
                                  [ [(API.FieldName "nodes_Title", API.mkColumnFieldValue $ Aeson.String "For Those About To Rock We Salute You")],
                                    [(API.FieldName "nodes_Title", API.mkColumnFieldValue $ Aeson.String "Let There Be Rock")]
                                  ]
                            )
                          ]
                        ]
                   in DataConnector.chinookMock {DataConnector._queryResponse = \_ -> rowsResponse response},
                _whenRequestRequired =
                  [graphql|
                    query getArtist {
                      Artist_aggregate(limit: 1) {
                        ArtistIds: nodes {
                          Id: ArtistId
                        }
                        ArtistNames: nodes {
                          Name
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
                        ArtistIds:
                          - Id: 1
                        ArtistNames:
                          - Name: AC/DC
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
                                    [ (API.FieldName "ArtistIds_Id", API.ColumnField (API.ColumnName "ArtistId")),
                                      (API.FieldName "ArtistNames_Name", API.ColumnField (API.ColumnName "Name")),
                                      ( API.FieldName "nodes_Albums",
                                        API.RelField
                                          ( API.RelationshipField
                                              (API.RelationshipName "Albums")
                                              API.Query
                                                { _qFields =
                                                    Just $
                                                      HashMap.fromList
                                                        [ (API.FieldName "nodes_Title", API.ColumnField (API.ColumnName "Title"))
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

  it "works with multiple aggregate fields and through array relations" $
    DataConnector.runMockedTest opts $
      let required =
            DataConnector.TestCaseRequired
              { _givenRequired =
                  let aggregates =
                        [ (API.FieldName "counts_count", Aeson.Number 2),
                          (API.FieldName "counts_uniqueBillingCountries", Aeson.Number 2),
                          (API.FieldName "ids_minimum_Id", Aeson.Number 1),
                          (API.FieldName "ids_max_InvoiceId", Aeson.Number 2)
                        ]
                      rows =
                        [ [ ( API.FieldName "nodes_Lines",
                              API.mkRelationshipFieldValue $
                                aggregatesResponse
                                  [ (API.FieldName "aggregate_count", Aeson.Number 2)
                                  ]
                            )
                          ],
                          [ ( API.FieldName "nodes_Lines",
                              API.mkRelationshipFieldValue $
                                aggregatesResponse
                                  [ (API.FieldName "aggregate_count", Aeson.Number 4)
                                  ]
                            )
                          ]
                        ]
                   in DataConnector.chinookMock {DataConnector._queryResponse = \_ -> aggregatesAndRowsResponse aggregates rows},
                _whenRequestRequired =
                  [graphql|
                    query getInvoices {
                      Invoice_aggregate(limit: 2) {
                        counts: aggregate {
                          count
                          uniqueBillingCountries: count(column: BillingCountry, distinct: true)
                        }
                        ids: aggregate {
                          minimum: min {
                            Id: InvoiceId
                          }
                          max {
                            InvoiceId
                          }
                        }
                        nodes {
                          Lines: InvoiceLines_aggregate {
                            aggregate {
                              count
                            }
                          }
                        }
                      }
                    }
                  |],
                _thenRequired =
                  [yaml|
                    data:
                      Invoice_aggregate:
                        counts:
                          count: 2
                          uniqueBillingCountries: 2
                        ids:
                          minimum:
                            Id: 1
                          max:
                            InvoiceId: 2
                        nodes:
                          - Lines:
                              aggregate:
                                count: 2
                          - Lines:
                              aggregate:
                                count: 4
                    |]
              }
       in (DataConnector.defaultTestCase required)
            { _whenQuery =
                Just
                  ( API.QueryRequest
                      { _qrTable = API.TableName ("Invoice" :| []),
                        _qrTableRelationships =
                          [ API.TableRelationships
                              { _trSourceTable = API.TableName ("Invoice" :| []),
                                _trRelationships =
                                  HashMap.fromList
                                    [ ( API.RelationshipName "InvoiceLines",
                                        API.Relationship
                                          { _rTargetTable = API.TableName ("InvoiceLine" :| []),
                                            _rRelationshipType = API.ArrayRelationship,
                                            _rColumnMapping = HashMap.fromList [(API.ColumnName "InvoiceId", API.ColumnName "InvoiceId")]
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
                                    [ ( API.FieldName "nodes_Lines",
                                        API.RelField
                                          ( API.RelationshipField
                                              (API.RelationshipName "InvoiceLines")
                                              API.Query
                                                { _qFields = Nothing,
                                                  _qAggregates =
                                                    Just $
                                                      HashMap.fromList
                                                        [(API.FieldName "aggregate_count", API.StarCount)],
                                                  _qLimit = Nothing,
                                                  _qOffset = Nothing,
                                                  _qWhere = Nothing,
                                                  _qOrderBy = Nothing
                                                }
                                          )
                                      )
                                    ],
                              _qAggregates =
                                Just $
                                  HashMap.fromList
                                    [ (API.FieldName "counts_count", API.StarCount),
                                      (API.FieldName "counts_uniqueBillingCountries", API.ColumnCount (API.ColumnCountAggregate (API.ColumnName "BillingCountry") True)),
                                      (API.FieldName "ids_minimum_Id", API.SingleColumn (API.SingleColumnAggregate API.Min (API.ColumnName "InvoiceId"))),
                                      (API.FieldName "ids_max_InvoiceId", API.SingleColumn (API.SingleColumnAggregate API.Max (API.ColumnName "InvoiceId")))
                                    ],
                              _qLimit = Just 2,
                              _qOffset = Nothing,
                              _qWhere = Nothing,
                              _qOrderBy = Nothing
                            }
                      }
                  )
            }

rowsResponse :: [[(API.FieldName, API.FieldValue)]] -> API.QueryResponse
rowsResponse rows = API.QueryResponse (Just $ HashMap.fromList <$> rows) Nothing

aggregatesResponse :: [(API.FieldName, Aeson.Value)] -> API.QueryResponse
aggregatesResponse aggregates = API.QueryResponse Nothing (Just $ HashMap.fromList aggregates)

aggregatesAndRowsResponse :: [(API.FieldName, Aeson.Value)] -> [[(API.FieldName, API.FieldValue)]] -> API.QueryResponse
aggregatesAndRowsResponse aggregates rows = API.QueryResponse (Just $ HashMap.fromList <$> rows) (Just $ HashMap.fromList aggregates)
