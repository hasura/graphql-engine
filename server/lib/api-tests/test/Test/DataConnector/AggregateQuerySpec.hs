{-# LANGUAGE QuasiQuotes #-}

module Test.DataConnector.AggregateQuerySpec
  ( spec,
  )
where

import Data.Aeson qualified as Aeson
import Harness.Backend.DataConnector qualified as DataConnector
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (yaml)
import Harness.Test.BackendType (BackendType (..), defaultBackendTypeString, defaultSource)
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (TestEnvironment)
import Harness.TestEnvironment qualified as TE
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

spec :: SpecWith TestEnvironment
spec =
  Fixture.runWithLocalTestEnvironment
    ( ( \(DataConnector.TestSourceConfig backendType backendConfig sourceConfig _md) ->
          (Fixture.fixture $ Fixture.Backend backendType)
            { Fixture.setupTeardown =
                \(testEnv, _) -> [DataConnector.setupFixtureAction (sourceMetadata backendType sourceConfig) backendConfig testEnv]
            }
      )
        <$> DataConnector.backendConfigs
    )
    tests

sourceMetadata :: BackendType -> Aeson.Value -> Aeson.Value
sourceMetadata backendType config =
  let source = defaultSource backendType
      backendTypeString = defaultBackendTypeString backendType
   in [yaml|
        name : *source
        kind: *backendTypeString
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
        configuration: *config
      |]

--------------------------------------------------------------------------------

tests :: Fixture.Options -> SpecWith (TestEnvironment, a)
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

    it "works with object relations" $ \(testEnvironment, _) -> do
      -- NOTE: Ordering is required due to datasets non-matching orders
      shouldReturnYaml
        opts
        ( GraphqlEngine.postGraphql
            testEnvironment
            [graphql|
              query getAlbum {
                Album_aggregate(order_by: {AlbumId: asc}, limit: 2) {
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

    it "works with single column queries" $ \(testEnvironment, _) -> do
      -- NOTE: This test is specialized for the reference agent to support more statistical functions.
      -- This should really be derived from the agent's capabilities.
      let referenceQuery =
            [graphql|
          query getInvoices {
            Invoice_aggregate {
              aggregate {
                max { Total }
                min { Total }
                stddev { Total }
                stddev_pop { Total }
                stddev_samp { Total }
                sum { Total }
                var_pop { Total }
                var_samp { Total }
                variance { Total }
              }
            }
          }
        |]

          generalQuery =
            [graphql|
          query getInvoices {
            Invoice_aggregate {
              aggregate {
                max { Total }
                min { Total }
                sum { Total }
              }
            }
          }
        |]

          referenceResults =
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

          generalResults =
            [yaml|
          data:
            Invoice_aggregate:
              aggregate:
                max:
                  Total: 25.86
                min:
                  Total: 0.99
                sum:
                  Total: 2328.6
        |]

      if (TE.backendType testEnvironment == Just Fixture.DataConnectorReference)
        then
          shouldReturnYaml
            opts
            ( GraphqlEngine.postGraphql
                testEnvironment
                referenceQuery
            )
            referenceResults
        else
          shouldReturnYaml
            opts
            ( GraphqlEngine.postGraphql
                testEnvironment
                generalQuery
            )
            generalResults

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

    it "works across array relationships from regular queries" $ \(testEnvironment, _) -> do
      -- NOTE: Ordering is added to allow SQLite chinook dataset to return ordered results
      shouldReturnYaml
        opts
        ( GraphqlEngine.postGraphql
            testEnvironment
            [graphql|
              query getInvoices {
                Invoice(limit: 5, order_by: {InvoiceId: asc}) {
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

    it "works across array relationships from aggregate queries via nodes" $ \(testEnvironment, _) -> do
      -- NOTE: Ordering present so that out-of-order rows are sorted for SQLite
      shouldReturnYaml
        opts
        ( GraphqlEngine.postGraphql
            testEnvironment
            [graphql|
              query getInvoices {
                Invoice_aggregate(limit: 5, order_by: {InvoiceId: asc}) {
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
