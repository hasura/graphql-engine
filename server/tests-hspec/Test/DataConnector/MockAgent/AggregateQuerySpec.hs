{-# LANGUAGE QuasiQuotes #-}

module Test.DataConnector.MockAgent.AggregateQuerySpec
  ( spec,
  )
where

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
          - table: Invoice
            array_relationships:
              - name: InvoiceLines
                using:
                  manual_configuration:
                    remote_table: InvoiceLine
                    column_mapping:
                      InvoiceId: InvoiceId
          - table: InvoiceLine
            object_relationships:
              - name: Invoice
                using:
                  manual_configuration:
                    remote_table: Invoice
                    column_mapping:
                      InvoiceId: InvoiceId
        configuration: {}
      |]

--------------------------------------------------------------------------------

tests :: Context.Options -> SpecWith (TestEnvironment, DataConnector.MockAgentEnvironment)
tests opts = describe "Aggregate Query Tests" $ do
  it "works with multiple nodes fields and through array relations" $
    DataConnector.runMockedTest opts $
      let required =
            DataConnector.TestCaseRequired
              { _givenRequired =
                  let response =
                        [ [ ("ArtistIds_Id", API.mkColumnFieldValue $ Aeson.Number 1),
                            ("ArtistNames_Name", API.mkColumnFieldValue $ Aeson.String "AC/DC"),
                            ( "nodes_Albums",
                              API.mkRelationshipFieldValue $
                                rowsResponse
                                  [ [("nodes_Title", API.mkColumnFieldValue $ Aeson.String "For Those About To Rock We Salute You")],
                                    [("nodes_Title", API.mkColumnFieldValue $ Aeson.String "Let There Be Rock")]
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
                                    [ ("ArtistIds_Id", API.ColumnField (API.ColumnName "ArtistId")),
                                      ("ArtistNames_Name", API.ColumnField (API.ColumnName "Name")),
                                      ( "nodes_Albums",
                                        API.RelField
                                          ( API.RelationshipField
                                              (API.RelationshipName "Albums")
                                              API.Query
                                                { _qFields =
                                                    Just $
                                                      KM.fromList
                                                        [ ("nodes_Title", API.ColumnField (API.ColumnName "Title"))
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

  it "works with multiple aggregate fields and through array relations" $
    DataConnector.runMockedTest opts $
      let required =
            DataConnector.TestCaseRequired
              { _givenRequired =
                  let aggregates =
                        [ ("counts_count", API.Number 2),
                          ("counts_uniqueBillingCountries", API.Number 2),
                          ("ids_minimum_Id", API.Number 1),
                          ("ids_max_InvoiceId", API.Number 2)
                        ]
                      rows =
                        [ [ ( "nodes_Lines",
                              API.mkRelationshipFieldValue $
                                aggregatesResponse
                                  [ ("aggregate_count", API.Number 2)
                                  ]
                            )
                          ],
                          [ ( "nodes_Lines",
                              API.mkRelationshipFieldValue $
                                aggregatesResponse
                                  [ ("aggregate_count", API.Number 4)
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
                      { _qrTable = API.TableName "Invoice",
                        _qrTableRelationships =
                          [ API.TableRelationships
                              { _trSourceTable = API.TableName "Invoice",
                                _trRelationships =
                                  HashMap.fromList
                                    [ ( API.RelationshipName "InvoiceLines",
                                        API.Relationship
                                          { _rTargetTable = API.TableName "InvoiceLine",
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
                                  KM.fromList
                                    [ ( "nodes_Lines",
                                        API.RelField
                                          ( API.RelationshipField
                                              (API.RelationshipName "InvoiceLines")
                                              API.Query
                                                { _qFields = Nothing,
                                                  _qAggregates =
                                                    Just $
                                                      KM.fromList
                                                        [("aggregate_count", API.StarCount)],
                                                  _qLimit = Nothing,
                                                  _qOffset = Nothing,
                                                  _qWhere = Just (API.And []),
                                                  _qOrderBy = Nothing
                                                }
                                          )
                                      )
                                    ],
                              _qAggregates =
                                Just $
                                  KM.fromList
                                    [ ("counts_count", API.StarCount),
                                      ("counts_uniqueBillingCountries", API.ColumnCount (API.ColumnCountAggregate (API.ColumnName "BillingCountry") True)),
                                      ("ids_minimum_Id", API.SingleColumn (API.SingleColumnAggregate API.Min (API.ColumnName "InvoiceId"))),
                                      ("ids_max_InvoiceId", API.SingleColumn (API.SingleColumnAggregate API.Max (API.ColumnName "InvoiceId")))
                                    ],
                              _qLimit = Just 2,
                              _qOffset = Nothing,
                              _qWhere = Just (API.And []),
                              _qOrderBy = Nothing
                            }
                      }
                  )
            }

rowsResponse :: [[(Aeson.Key, API.FieldValue)]] -> API.QueryResponse
rowsResponse rows = API.QueryResponse (Just $ KM.fromList <$> rows) Nothing

aggregatesResponse :: [(Aeson.Key, API.Value)] -> API.QueryResponse
aggregatesResponse aggregates = API.QueryResponse Nothing (Just $ KM.fromList aggregates)

aggregatesAndRowsResponse :: [(Aeson.Key, API.Value)] -> [[(Aeson.Key, API.FieldValue)]] -> API.QueryResponse
aggregatesAndRowsResponse aggregates rows = API.QueryResponse (Just $ KM.fromList <$> rows) (Just $ KM.fromList aggregates)
