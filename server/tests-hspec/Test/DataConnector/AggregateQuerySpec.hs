{-# LANGUAGE QuasiQuotes #-}

module Test.DataConnector.AggregateQuerySpec
  ( spec,
  )
where

import Data.Aeson qualified as Aeson
import Harness.Backend.DataConnector qualified as DataConnector
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (shouldReturnYaml, yaml)
import Harness.Test.BackendType (BackendType (..), defaultBackendTypeString, defaultSource)
import Harness.Test.Context qualified as Context
import Harness.TestEnvironment (TestEnvironment)
import Test.Hspec (SpecWith, describe, it)
import Prelude

spec :: SpecWith TestEnvironment
spec =
  Context.runWithLocalTestEnvironment
    [ Context.Context
        { name = Context.Backend Context.DataConnector,
          mkLocalTestEnvironment = Context.noLocalTestEnvironment,
          setup = DataConnector.setupFixture sourceMetadata DataConnector.defaultBackendConfig,
          teardown = DataConnector.teardown,
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

tests :: Context.Options -> SpecWith (TestEnvironment, a)
tests opts = describe "Nodes Tests" $ do
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
