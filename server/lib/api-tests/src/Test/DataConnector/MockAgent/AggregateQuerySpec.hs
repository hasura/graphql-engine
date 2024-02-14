{-# LANGUAGE QuasiQuotes #-}

module Test.DataConnector.MockAgent.AggregateQuerySpec (spec) where

--------------------------------------------------------------------------------

import Control.Lens ((.~), (?~))
import Data.Aeson qualified as J
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty qualified as NE
import Data.Set qualified as Set
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
import Test.DataConnector.MockAgent.TestHelpers
import Test.Hspec (SpecWith, describe, shouldBe)

--------------------------------------------------------------------------------

spec :: SpecWith GlobalTestEnvironment
spec =
  Fixture.runWithLocalTestEnvironment
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Mock.backendTypeMetadata)
            { Fixture.mkLocalTestEnvironment =
                Mock.mkLocalTestEnvironment,
              Fixture.setupTeardown = \(testEnv, mockEnv) ->
                [Mock.setupAction sourceMetadata Mock.agentConfig (testEnv, mockEnv)]
            }
        ]
    )
    tests

--------------------------------------------------------------------------------

sourceMetadata :: J.Value
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

tests :: SpecWith (TestEnvironment, Mock.MockAgentEnvironment)
tests = describe "Aggregate Query Tests" $ do
  mockAgentGraphqlTest "works with multiple nodes fields and through array relations" $ \_testEnv performGraphqlRequest -> do
    let headers = []
    let graphqlRequest =
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
          |]
    let queryResponse =
          mkRowsQueryResponse
            [ [ ("ArtistIds_Id", API.mkColumnFieldValue $ J.Number 1),
                ("ArtistNames_Name", API.mkColumnFieldValue $ J.String "AC/DC"),
                ( "nodes_Albums",
                  API.mkRelationshipFieldValue
                    $ mkRowsQueryResponse
                      [ [("nodes_Title", API.mkColumnFieldValue $ J.String "For Those About To Rock We Salute You")],
                        [("nodes_Title", API.mkColumnFieldValue $ J.String "Let There Be Rock")]
                      ]
                )
              ]
            ]
    let mockConfig = mockQueryResponse queryResponse

    MockRequestResults {..} <- performGraphqlRequest mockConfig headers graphqlRequest

    _mrrResponse
      `shouldBeYaml` [yaml|
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

    _mrrRecordedRequest
      `shouldBe` Just
        ( Query
            $ mkTableRequest
              (mkTableName "Artist")
              ( emptyQuery
                  & API.qFields
                  ?~ mkFieldsMap
                    [ ("ArtistIds_Id", API.ColumnField (API.ColumnName "ArtistId") (API.ScalarType "number") Nothing),
                      ("ArtistNames_Name", API.ColumnField (API.ColumnName "Name") (API.ScalarType "string") Nothing),
                      ( "nodes_Albums",
                        API.RelField
                          ( API.RelationshipField
                              (API.RelationshipName "Albums")
                              ( emptyQuery
                                  & API.qFields
                                  ?~ mkFieldsMap [("nodes_Title", API.ColumnField (API.ColumnName "Title") (API.ScalarType "string") Nothing)]
                              )
                          )
                      )
                    ]
                    & API.qLimit
                  ?~ 1
              )
            & API.qrRelationships
            .~ Set.fromList
              [ API.RTable
                  API.TableRelationships
                    { _trelSourceTable = mkTableName "Artist",
                      _trelRelationships =
                        HashMap.fromList
                          [ ( API.RelationshipName "Albums",
                              API.Relationship
                                { _rTarget = mkTableTarget "Album",
                                  _rRelationshipType = API.ArrayRelationship,
                                  _rColumnMapping = API.ColumnPathMapping $ HashMap.fromList [(API.mkColumnSelector $ API.ColumnName "ArtistId", API.mkColumnSelector $ API.ColumnName "ArtistId")]
                                }
                            )
                          ]
                    }
              ]
        )

  mockAgentGraphqlTest "works with multiple aggregate fields and through array relations" $ \_testEnv performGraphqlRequest -> do
    let headers = []
    let graphqlRequest =
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
          |]
    let aggregates =
          [ ("counts_count", J.Number 2),
            ("counts_uniqueBillingCountries", J.Number 2),
            ("ids_minimum_Id", J.Number 1),
            ("ids_max_InvoiceId", J.Number 2)
          ]
        rows =
          [ [ ( "nodes_Lines",
                API.mkRelationshipFieldValue
                  $ mkAggregatesQueryResponse
                    [ ("aggregate_count", J.Number 2)
                    ]
              )
            ],
            [ ( "nodes_Lines",
                API.mkRelationshipFieldValue
                  $ mkAggregatesQueryResponse
                    [ ("aggregate_count", J.Number 4)
                    ]
              )
            ]
          ]
    let mockConfig = mockQueryResponse (mkQueryResponse rows aggregates)

    MockRequestResults {..} <- performGraphqlRequest mockConfig headers graphqlRequest

    _mrrResponse
      `shouldBeYaml` [yaml|
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

    _mrrRecordedRequest
      `shouldBe` Just
        ( Query
            $ mkTableRequest
              (mkTableName "Invoice")
              ( emptyQuery
                  & API.qFields
                  ?~ mkFieldsMap
                    [ ( "nodes_Lines",
                        API.RelField
                          ( API.RelationshipField
                              (API.RelationshipName "InvoiceLines")
                              ( emptyQuery & API.qAggregates ?~ mkFieldsMap [("aggregate_count", API.StarCount)]
                              )
                          )
                      )
                    ]
                    & API.qAggregates
                  ?~ mkFieldsMap
                    [ ("counts_count", API.StarCount),
                      ("counts_uniqueBillingCountries", API.ColumnCount (API.ColumnCountAggregate (API.ColumnName "BillingCountry") Nothing True)),
                      ("ids_minimum_Id", API.SingleColumn (singleColumnAggregateMin (API.ColumnName "InvoiceId") (API.ScalarType "number"))),
                      ("ids_max_InvoiceId", API.SingleColumn (singleColumnAggregateMax (API.ColumnName "InvoiceId") (API.ScalarType "number")))
                    ]
                    & API.qLimit
                  ?~ 2
                    & API.qAggregatesLimit
                  ?~ 2
              )
            & API.qrRelationships
            .~ Set.fromList
              [ API.RTable
                  API.TableRelationships
                    { _trelSourceTable = mkTableName "Invoice",
                      _trelRelationships =
                        HashMap.fromList
                          [ ( API.RelationshipName "InvoiceLines",
                              API.Relationship
                                { _rTarget = mkTableTarget "InvoiceLine",
                                  _rRelationshipType = API.ArrayRelationship,
                                  _rColumnMapping = API.ColumnPathMapping $ HashMap.fromList [(API.mkColumnSelector $ API.ColumnName "InvoiceId", API.mkColumnSelector $ API.ColumnName "InvoiceId")]
                                }
                            )
                          ]
                    }
              ]
        )

singleColumnAggregateMax :: API.ColumnName -> API.ScalarType -> API.SingleColumnAggregate
singleColumnAggregateMax columnName scalarType = API.SingleColumnAggregate (API.SingleColumnAggregateFunction [G.name|max|]) columnName Nothing scalarType

singleColumnAggregateMin :: API.ColumnName -> API.ScalarType -> API.SingleColumnAggregate
singleColumnAggregateMin columnName scalarType = API.SingleColumnAggregate (API.SingleColumnAggregateFunction [G.name|min|]) columnName Nothing scalarType
