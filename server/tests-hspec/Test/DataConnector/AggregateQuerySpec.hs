{-# LANGUAGE QuasiQuotes #-}

module Test.DataConnector.AggregateQuerySpec
  ( spec,
  )
where

import Data.Aeson qualified as Aeson
import Data.List.NonEmpty qualified as NE
import Harness.Backend.DataConnector qualified as DataConnector
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (yaml)
import Harness.Test.BackendType (BackendType (..), defaultBackendTypeString, defaultSource)
import Harness.Test.Context qualified as Context
import Harness.TestEnvironment (TestEnvironment)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

spec :: SpecWith TestEnvironment
spec =
  Context.runWithLocalTestEnvironment
    ( NE.fromList
        [ Context.Context
            { name = Context.Backend Context.DataConnector,
              mkLocalTestEnvironment = Context.noLocalTestEnvironment,
              setup = DataConnector.setupFixture sourceMetadata DataConnector.defaultBackendConfig,
              teardown = DataConnector.teardown,
              customOptions = Nothing
            }
        ]
    )
    tests

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

tests :: Context.Options -> SpecWith (TestEnvironment, a)
tests opts = describe "Aggregate Query Tests" $ do
  describe "Nodes Tests" $ do
    it "works with simple query" $ \(testEnvironment, _) ->
      shouldReturnYaml
        opts
        ( GraphqlEngine.postGraphql
            testEnvironment
            [graphql|
              query getAlbum {
                Album_aggregate(limit: 2) {
                  nodes {
                    AlbumId
                    Title
                  }
                }
              }
            |]
        )
        [yaml|
          data:
            Album_aggregate:
              nodes:
                - AlbumId: 1
                  Title: For Those About To Rock We Salute You
                - AlbumId: 2
                  Title: Balls to the Wall
        |]

    it "works with multiple nodes fields" $ \(testEnvironment, _) ->
      shouldReturnYaml
        opts
        ( GraphqlEngine.postGraphql
            testEnvironment
            [graphql|
              query getAlbum {
                Album_aggregate(limit: 2) {
                  AlbumIds: nodes {
                    AlbumId
                  }
                  Titles: nodes {
                    Title
                  }
                }
              }
            |]
        )
        [yaml|
          data:
            Album_aggregate:
              AlbumIds:
                - AlbumId: 1
                - AlbumId: 2
              Titles:
                - Title: For Those About To Rock We Salute You
                - Title: Balls to the Wall
        |]

    it "works with object relations" $ \(testEnvironment, _) ->
      shouldReturnYaml
        opts
        ( GraphqlEngine.postGraphql
            testEnvironment
            [graphql|
              query getAlbum {
                Album_aggregate(limit: 2) {
                  nodes {
                    AlbumId
                    Artist {
                      Name
                    }
                  }
                }
              }
            |]
        )
        [yaml|
          data:
            Album_aggregate:
              nodes:
                - AlbumId: 1
                  Artist:
                    Name: AC/DC
                - AlbumId: 2
                  Artist:
                    Name: Accept
        |]

    it "works with array relations" $ \(testEnvironment, _) ->
      shouldReturnYaml
        opts
        ( GraphqlEngine.postGraphql
            testEnvironment
            [graphql|
              query getArtist {
                Artist_aggregate(limit: 2) {
                  nodes {
                    ArtistId
                    Albums: Albums_aggregate {
                      nodes {
                        Title
                      }
                    }
                  }
                }
              }
            |]
        )
        [yaml|
          data:
            Artist_aggregate:
              nodes:
                - ArtistId: 1
                  Albums:
                    nodes:
                      - Title: For Those About To Rock We Salute You
                      - Title: Let There Be Rock
                - ArtistId: 2
                  Albums:
                    nodes:
                      - Title: Balls to the Wall
                      - Title: Restless and Wild
        |]

  describe "Aggregate Tests" $ do
    it "works with count queries" $ \(testEnvironment, _) ->
      shouldReturnYaml
        opts
        ( GraphqlEngine.postGraphql
            testEnvironment
            [graphql|
              query getInvoices {
                Invoice_aggregate {
                  aggregate {
                    count
                    countColumn: count(column: BillingState)
                    countColumnDistinct: count(column: BillingState, distinct: true)
                  }
                }
              }
            |]
        )
        [yaml|
          data:
            Invoice_aggregate:
              aggregate:
                count: 412
                countColumn: 210
                countColumnDistinct: 25
        |]

    it "works with single column queries" $ \(testEnvironment, _) ->
      shouldReturnYaml
        opts
        ( GraphqlEngine.postGraphql
            testEnvironment
            [graphql|
              query getInvoices {
                Invoice_aggregate {
                  aggregate {
                    max {
                      Total
                    }
                    min {
                      Total
                    }
                    stddev {
                      Total
                    }
                    stddev_pop {
                      Total
                    }
                    stddev_samp {
                      Total
                    }
                    sum {
                      Total
                    }
                    var_pop {
                      Total
                    }
                    var_samp {
                      Total
                    }
                    variance {
                      Total
                    }
                  }
                }
              }
            |]
        )
        [yaml|
          data:
            Invoice_aggregate:
              aggregate:
                max:
                  Total: 25.86
                min:
                  Total: 0.99
                stddev:
                  Total: 4.745319693568103
                stddev_pop:
                  Total: 4.739557311729622
                stddev_samp:
                  Total: 4.745319693568103
                sum:
                  Total: 2328.600000000004
                var_pop:
                  Total: 22.463403511169727
                var_samp:
                  Total: 22.518058994165273
                variance:
                  Total: 22.518058994165273
        |]

    it "min and max works on string fields" $ \(testEnvironment, _) ->
      shouldReturnYaml
        opts
        ( GraphqlEngine.postGraphql
            testEnvironment
            [graphql|
              query getArtists {
                Artist_aggregate {
                  aggregate {
                    max {
                      Name
                    }
                    min {
                      Name
                    }
                  }
                }
              }
            |]
        )
        [yaml|
          data:
            Artist_aggregate:
              aggregate:
                max:
                  Name: Zeca Pagodinho
                min:
                  Name: A Cor Do Som
        |]

    it "works across array relationships from regular queries" $ \(testEnvironment, _) ->
      shouldReturnYaml
        opts
        ( GraphqlEngine.postGraphql
            testEnvironment
            [graphql|
              query getInvoices {
                Invoice(limit: 5) {
                  InvoiceId
                  InvoiceLines_aggregate {
                    aggregate {
                      count
                    }
                  }
                }
              }
            |]
        )
        [yaml|
          data:
            Invoice:
              - InvoiceId: 1
                InvoiceLines_aggregate:
                  aggregate:
                    count: 2
              - InvoiceId: 2
                InvoiceLines_aggregate:
                  aggregate:
                    count: 4
              - InvoiceId: 3
                InvoiceLines_aggregate:
                  aggregate:
                    count: 6
              - InvoiceId: 4
                InvoiceLines_aggregate:
                  aggregate:
                    count: 9
              - InvoiceId: 5
                InvoiceLines_aggregate:
                  aggregate:
                    count: 14
        |]

    it "works across array relationships from aggregate queries via nodes" $ \(testEnvironment, _) ->
      shouldReturnYaml
        opts
        ( GraphqlEngine.postGraphql
            testEnvironment
            [graphql|
              query getInvoices {
                Invoice_aggregate(limit: 5) {
                  nodes {
                    InvoiceId
                    InvoiceLines_aggregate {
                      aggregate {
                        count
                      }
                    }
                  }
                }
              }
            |]
        )
        [yaml|
          data:
            Invoice_aggregate:
              nodes:
                - InvoiceId: 1
                  InvoiceLines_aggregate:
                    aggregate:
                      count: 2
                - InvoiceId: 2
                  InvoiceLines_aggregate:
                    aggregate:
                      count: 4
                - InvoiceId: 3
                  InvoiceLines_aggregate:
                    aggregate:
                      count: 6
                - InvoiceId: 4
                  InvoiceLines_aggregate:
                    aggregate:
                      count: 9
                - InvoiceId: 5
                  InvoiceLines_aggregate:
                    aggregate:
                      count: 14
        |]
