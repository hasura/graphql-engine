{-# LANGUAGE QuasiQuotes #-}

-- | Query Tests for Data Connector Backend
module Test.DataConnector.QuerySpec
  ( spec,
  )
where

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- Reference Agent Query Tests

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
            remote_table: Artist
            column_mapping:
              ArtistId: ArtistId
  - table: Artist
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
            remote_table: Album
            column_mapping:
              ArtistId: ArtistId
  - table: Playlist
  - table: PlaylistTrack
    object_relationships:
      - name: Playlist
        using:
          manual_configuration:
            remote_table: Playlist
            column_mapping:
              PlaylistId: PlaylistId
      - name: Track
        using:
          manual_configuration:
            remote_table: Track
            column_mapping:
              TrackId: TrackId
  - table: Track
configuration: {}
|]

--------------------------------------------------------------------------------

tests :: Context.Options -> SpecWith (TestEnvironment, a)
tests opts = describe "Queries" $ do
  describe "Basic Tests" $ do
    it "works with simple object query" $ \(testEnvironment, _) ->
      shouldReturnYaml
        opts
        ( GraphqlEngine.postGraphql
            testEnvironment
            [graphql|
              query getAlbum {
                albums(limit: 1) {
                  id
                  title
                }
              }
            |]
        )
        [yaml|
          data:
            albums:
              - id: 1
                title: For Those About To Rock We Salute You
        |]

    it "works with order_by id" $ \(testEnvironment, _) ->
      shouldReturnYaml
        opts
        ( GraphqlEngine.postGraphql
            testEnvironment
            [graphql|
              query getAlbum {
                albums(limit: 3, order_by: {id: asc}) {
                  id
                  title
                }
              }
            |]
        )
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

    it "works with a primary key" $ \(testEnvironment, _) ->
      shouldReturnYaml
        opts
        ( GraphqlEngine.postGraphql
            testEnvironment
            [graphql|
              query getAlbum {
                albums_by_pk(id: 1) {
                  id
                  title
                }
              }
            |]
        )
        [yaml|
          data:
            albums_by_pk:
              - id: 1
                title: "For Those About To Rock We Salute You"
        |]

    it "works with non existent primary key" $ \(testEnvironment, _) ->
      shouldReturnYaml
        opts
        ( GraphqlEngine.postGraphql
            testEnvironment
            [graphql|
              query getAlbum {
                albums_by_pk(id: 999999) {
                  id
                  title
                }
              }
            |]
        )
        [yaml|
          data:
            albums_by_pk: []
        |]

    it "works with a composite primary key" $ \(testEnvironment, _) ->
      shouldReturnYaml
        opts
        ( GraphqlEngine.postGraphql
            testEnvironment
            [graphql|
              query getAlbum {
                  PlaylistTrack_by_pk(PlaylistId: 1, TrackId: 2) {
                    Playlist {
                      Name
                    }
                    Track {
                      Name
                    }
                  }
              }
            |]
        )
        [yaml|
          data:
            PlaylistTrack_by_pk:
              - Playlist:
                  Name: "Music"
                Track:
                  Name: "Balls to the Wall"
        |]

  it "works with pagination" $ \(testEnvironment, _) ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          testEnvironment
          [graphql|
            query getAlbum {
              albums (limit: 3, offset: 2) {
                id
              }
            }
          |]
      )
      [yaml|
        data:
          albums:
            - id: 3
            - id: 4
            - id: 5
      |]

  describe "Array Relationships" $ do
    it "joins on album id" $ \(testEnvironment, _) ->
      shouldReturnYaml
        opts
        ( GraphqlEngine.postGraphql
            testEnvironment
            [graphql|
              query getArtist {
                artists_by_pk(id: 1) {
                  id
                  name
                  albums {
                    title
                  }
                }
              }
            |]
        )
        [yaml|
          data:
            artists_by_pk:
              - name: AC/DC
                id: 1
                albums:
                  - title: For Those About To Rock We Salute You
                  - title: Let There Be Rock
        |]

  describe "Object Relationships" $ do
    it "joins on artist id" $ \(testEnvironment, _) ->
      shouldReturnYaml
        opts
        ( GraphqlEngine.postGraphql
            testEnvironment
            [graphql|
              query getAlbum {
                albums_by_pk(id: 1) {
                  id
                  title
                  artist {
                    name
                  }
                }
              }
            |]
        )
        [yaml|
          data:
            albums_by_pk:
              - id: 1
                title: "For Those About To Rock We Salute You"
                artist:
                  name: "AC/DC"
        |]

  describe "Where Clause Tests" $ do
    it "works with '_in' predicate" $ \(testEnvironment, _) ->
      shouldReturnYaml
        opts
        ( GraphqlEngine.postGraphql
            testEnvironment
            [graphql|
              query getAlbum {
                albums(where: {id: {_in: [1, 3, 5]}}) {
                  id
                  title
                }
              }
            |]
        )
        [yaml|
          data:
            albums:
            - id: 1
              title: For Those About To Rock We Salute You
            - id: 3
              title: Restless and Wild
            - id: 5
              title: Big Ones
        |]

    it "works with '_nin' predicate" $ \(testEnvironment, _) ->
      shouldReturnYaml
        opts
        ( GraphqlEngine.postGraphql
            testEnvironment
            [graphql|
              query getAlbum {
                albums(where: {id: {_in: [1, 3, 5]}, title: {_nin: ["Big Ones"]}}) {
                  id
                  title
                }
              }
            |]
        )
        [yaml|
          data:
            albums:
            - id: 1
              title: For Those About To Rock We Salute You
            - id: 3
              title: Restless and Wild
        |]

    it "works with '_eq' predicate" $ \(testEnvironment, _) ->
      shouldReturnYaml
        opts
        ( GraphqlEngine.postGraphql
            testEnvironment
            [graphql|
              query getAlbum {
                albums(where: {id: {_eq: 1}}) {
                  id
                  title
                }
              }
            |]
        )
        [yaml|
          data:
            albums:
              - id: 1
                title: For Those About To Rock We Salute You
        |]

    it "works with '_neq' predicate" $ \(testEnvironment, _) ->
      shouldReturnYaml
        opts
        ( GraphqlEngine.postGraphql
            testEnvironment
            [graphql|
              query getAlbum {
                albums(where: {id: {_neq: 2, _in: [1, 2, 3]}}) {
                  id
                  title
                }
              }
            |]
        )
        [yaml|
          data:
            albums:
              - id: 1
                title: For Those About To Rock We Salute You
              - id: 3
                title: Restless and Wild
        |]

    it "works with '_lt' predicate" $ \(testEnvironment, _) ->
      shouldReturnYaml
        opts
        ( GraphqlEngine.postGraphql
            testEnvironment
            [graphql|
              query getAlbum {
                albums(where: {id: {_lt: 2}}) {
                  id
                  title
                }
              }
            |]
        )
        [yaml|
          data:
            albums:
              - id: 1
                title: For Those About To Rock We Salute You
        |]

    it "works with '_lte' predicate" $ \(testEnvironment, _) ->
      shouldReturnYaml
        opts
        ( GraphqlEngine.postGraphql
            testEnvironment
            [graphql|
              query getArtists {
                artists(where: {id: {_lte: 2}}) {
                  id
                  name
                }
              }
            |]
        )
        [yaml|
          data:
            artists:
              - id: 1
                name: AC/DC
              - id: 2
                name: Accept
        |]

    it "works with '_gt' predicate" $ \(testEnvironment, _) ->
      shouldReturnYaml
        opts
        ( GraphqlEngine.postGraphql
            testEnvironment
            [graphql|
              query getArtists {
                artists(where: {id: {_gt: 274}}) {
                  id
                  name
                }
              }
            |]
        )
        [yaml|
          data:
            artists:
              - id: 275
                name: Philip Glass Ensemble
        |]

    it "works with '_gte' predicate" $ \(testEnvironment, _) ->
      shouldReturnYaml
        opts
        ( GraphqlEngine.postGraphql
            testEnvironment
            [graphql|
              query getArtists {
                artists(where: {id: {_gte: 274}}) {
                  id
                  name
                }
              }
            |]
        )
        [yaml|
          data:
            artists:
              - id: 274
                name: Nash Ensemble
              - id: 275
                name: Philip Glass Ensemble
        |]
