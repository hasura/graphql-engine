{-# LANGUAGE QuasiQuotes #-}

-- | Query Tests for Data Connector Backend
module Test.DataConnector.QuerySpec
  ( spec,
  )
where

--------------------------------------------------------------------------------

import Data.List.NonEmpty qualified as NE
import Harness.Backend.DataConnector.Chinook qualified as Chinook
import Harness.Backend.DataConnector.Chinook.Reference qualified as Reference
import Harness.Backend.DataConnector.Chinook.Sqlite qualified as Sqlite
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (yaml)
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (TestEnvironment)
import Harness.TestEnvironment qualified as TE
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it, pendingWith)

--------------------------------------------------------------------------------
-- Reference Agent Query Tests

spec :: SpecWith TestEnvironment
spec =
  Fixture.runWithLocalTestEnvironment
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Fixture.DataConnectorReference)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [Chinook.setupAction Chinook.referenceSourceConfig Reference.agentConfig testEnv]
            },
          (Fixture.fixture $ Fixture.Backend Fixture.DataConnectorSqlite)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [Chinook.setupAction Chinook.sqliteSourceConfig Sqlite.agentConfig testEnv]
            }
        ]
    )
    tests

--------------------------------------------------------------------------------

tests :: Fixture.Options -> SpecWith (TestEnvironment, a)
tests opts = describe "Queries" $ do
  describe "Basic Tests" $ do
    it "works with simple object query" $ \(testEnvironment, _) ->
      shouldReturnYaml
        opts
        ( GraphqlEngine.postGraphql
            testEnvironment
            [graphql|
              query getAlbum {
                Album(limit: 1) {
                  AlbumId
                  Title
                }
              }
            |]
        )
        [yaml|
          data:
            Album:
              - AlbumId: 1
                Title: For Those About To Rock We Salute You
        |]

    it "works with a primary key" $ \(testEnvironment, _) ->
      shouldReturnYaml
        opts
        ( GraphqlEngine.postGraphql
            testEnvironment
            [graphql|
              query getAlbum {
                Album_by_pk(AlbumId: 1) {
                  AlbumId
                  Title
                }
              }
            |]
        )
        [yaml|
          data:
            Album_by_pk:
              AlbumId: 1
              Title: "For Those About To Rock We Salute You"
        |]

    it "works with non existent primary key" $ \(testEnvironment, _) ->
      shouldReturnYaml
        opts
        ( GraphqlEngine.postGraphql
            testEnvironment
            [graphql|
              query getAlbum {
                Album_by_pk(AlbumId: 999999) {
                  AlbumId
                  Title
                }
              }
            |]
        )
        [yaml|
          data:
            Album_by_pk: null
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
              Playlist:
                Name: "Music"
              Track:
                Name: "Balls to the Wall"
        |]

    it "works with pagination" $ \(testEnvironment, _) -> do
      -- NOTE: We order by in this pagination test to ensure that the rows are ordered correctly (which they are not in db.chinook.sqlite)
      shouldReturnYaml
        opts
        ( GraphqlEngine.postGraphql
            testEnvironment
            [graphql|
              query getAlbum {
                Album (limit: 3, offset: 2, order_by: {AlbumId: asc}) {
                  AlbumId
                }
              }
            |]
        )
        [yaml|
          data:
            Album:
              - AlbumId: 3
              - AlbumId: 4
              - AlbumId: 5
        |]

  describe "Array Relationships" $ do
    describe "Manual" $ do
      it "joins on album id" $ \(testEnvironment, _) ->
        shouldReturnYaml
          opts
          ( GraphqlEngine.postGraphql
              testEnvironment
              [graphql|
                query getArtist {
                  Artist(where: {ArtistId: {_eq: 1}}) {
                    ArtistId
                    Name
                    Albums {
                      Title
                    }
                  }
                }
              |]
          )
          [yaml|
            data:
              Artist:
                - Name: AC/DC
                  ArtistId: 1
                  Albums:
                    - Title: For Those About To Rock We Salute You
                    - Title: Let There Be Rock
          |]

    describe "Foreign Key Constraint On" do
      it "joins on playlist_id" $ \(testEnvironment, _) -> do
        -- NOTE: Ordering is used for the query due to inconsistencies in data-set ordering.
        shouldReturnYaml
          opts
          ( GraphqlEngine.postGraphql
              testEnvironment
              [graphql|
                query getPlaylist {
                    Playlist(where: {PlaylistId: {_eq: 1}}) {
                      Tracks (order_by: {TrackId: desc}, limit: 3) {
                        TrackId
                      }
                    }
                }
              |]
          )
          [yaml|
            data:
              Playlist:
                - Tracks:
                    - TrackId: 3503
                    - TrackId: 3502
                    - TrackId: 3501
          |]

  describe "Object Relationships" do
    describe "Manual" do
      it "joins on artist id" $ \(testEnvironment, _) ->
        shouldReturnYaml
          opts
          ( GraphqlEngine.postGraphql
              testEnvironment
              [graphql|
                query getAlbum {
                  Album(where: {AlbumId: {_eq: 1}}) {
                    AlbumId
                    Title
                    Artist {
                      Name
                    }
                  }
                }
              |]
          )
          [yaml|
            data:
              Album:
                - AlbumId: 1
                  Title: "For Those About To Rock We Salute You"
                  Artist:
                    Name: "AC/DC"
          |]

    describe "Foreign Key Constraint On" $ do
      it "joins on PlaylistId" $ \(testEnvironment, _) ->
        shouldReturnYaml
          opts
          ( GraphqlEngine.postGraphql
              testEnvironment
              [graphql|
                query getPlaylist {
                    PlaylistTrack(where: {PlaylistId: {_eq: 1}, TrackId: {_eq: 2}}) {
                      Playlist {
                        Name
                      }
                    }
                }
              |]
          )
          [yaml|
            data:
              PlaylistTrack:
                - Playlist:
                    Name: "Music"
          |]

  describe "Where Clause Tests" $ do
    it "works with '_in' predicate" $ \(testEnvironment, _) ->
      shouldReturnYaml
        opts
        ( GraphqlEngine.postGraphql
            testEnvironment
            [graphql|
              query getAlbum {
                Album(where: {AlbumId: {_in: [1, 3, 5]}}) {
                  AlbumId
                  Title
                }
              }
            |]
        )
        [yaml|
          data:
            Album:
            - AlbumId: 1
              Title: For Those About To Rock We Salute You
            - AlbumId: 3
              Title: Restless and Wild
            - AlbumId: 5
              Title: Big Ones
        |]

    it "works with '_nin' predicate" $ \(testEnvironment, _) ->
      shouldReturnYaml
        opts
        ( GraphqlEngine.postGraphql
            testEnvironment
            [graphql|
              query getAlbum {
                Album(where: {AlbumId: {_in: [1, 3, 5]}, Title: {_nin: ["Big Ones"]}}) {
                  AlbumId
                  Title
                }
              }
            |]
        )
        [yaml|
          data:
            Album:
            - AlbumId: 1
              Title: For Those About To Rock We Salute You
            - AlbumId: 3
              Title: Restless and Wild
        |]

    it "works with '_eq' predicate" $ \(testEnvironment, _) ->
      shouldReturnYaml
        opts
        ( GraphqlEngine.postGraphql
            testEnvironment
            [graphql|
              query getAlbum {
                Album(where: {AlbumId: {_eq: 1}}) {
                  AlbumId
                  Title
                }
              }
            |]
        )
        [yaml|
          data:
            Album:
              - AlbumId: 1
                Title: For Those About To Rock We Salute You
        |]

    it "works with '_neq' predicate" $ \(testEnvironment, _) ->
      shouldReturnYaml
        opts
        ( GraphqlEngine.postGraphql
            testEnvironment
            [graphql|
              query getAlbum {
                Album(where: {AlbumId: {_neq: 2, _in: [1, 2, 3]}}) {
                  AlbumId
                  Title
                }
              }
            |]
        )
        [yaml|
          data:
            Album:
              - AlbumId: 1
                Title: For Those About To Rock We Salute You
              - AlbumId: 3
                Title: Restless and Wild
        |]

    it "works with '_lt' predicate" $ \(testEnvironment, _) ->
      shouldReturnYaml
        opts
        ( GraphqlEngine.postGraphql
            testEnvironment
            [graphql|
              query getAlbum {
                Album(where: {AlbumId: {_lt: 2}}) {
                  AlbumId
                  Title
                }
              }
            |]
        )
        [yaml|
          data:
            Album:
              - AlbumId: 1
                Title: For Those About To Rock We Salute You
        |]

    it "works with '_lte' predicate" $ \(testEnvironment, _) ->
      shouldReturnYaml
        opts
        ( GraphqlEngine.postGraphql
            testEnvironment
            [graphql|
              query getArtists {
                Artist(where: {ArtistId: {_lte: 2}}) {
                  ArtistId
                  Name
                }
              }
            |]
        )
        [yaml|
          data:
            Artist:
              - ArtistId: 1
                Name: AC/DC
              - ArtistId: 2
                Name: Accept
        |]

    it "works with '_gt' predicate" $ \(testEnvironment, _) ->
      shouldReturnYaml
        opts
        ( GraphqlEngine.postGraphql
            testEnvironment
            [graphql|
              query getArtists {
                Artist(where: {ArtistId: {_gt: 274}}) {
                  ArtistId
                  Name
                }
              }
            |]
        )
        [yaml|
          data:
            Artist:
              - ArtistId: 275
                Name: Philip Glass Ensemble
        |]

    it "works with '_gte' predicate" $ \(testEnvironment, _) ->
      shouldReturnYaml
        opts
        ( GraphqlEngine.postGraphql
            testEnvironment
            [graphql|
              query getArtists {
                Artist(where: {ArtistId: {_gte: 274}}) {
                  ArtistId
                  Name
                }
              }
            |]
        )
        [yaml|
          data:
            Artist:
              - ArtistId: 274
                Name: Nash Ensemble
              - ArtistId: 275
                Name: Philip Glass Ensemble
        |]

  describe "Order By Tests" $ do
    it "works with order_by id asc" $ \(testEnvironment, _) ->
      shouldReturnYaml
        opts
        ( GraphqlEngine.postGraphql
            testEnvironment
            [graphql|
              query getAlbum {
                Album(limit: 3, order_by: {AlbumId: asc}) {
                  AlbumId
                  Title
                }
              }
            |]
        )
        [yaml|
          data:
            Album:
              - AlbumId: 1
                Title: For Those About To Rock We Salute You
              - AlbumId: 2
                Title: Balls to the Wall
              - AlbumId: 3
                Title: Restless and Wild
        |]

    it "works with order_by id desc" $ \(testEnvironment, _) ->
      shouldReturnYaml
        opts
        ( GraphqlEngine.postGraphql
            testEnvironment
            [graphql|
              query getAlbum {
                Album(limit: 3, order_by: {AlbumId: desc}) {
                  AlbumId
                  Title
                }
              }
            |]
        )
        [yaml|
          data:
            Album:
              - AlbumId: 347
                Title: Koyaanisqatsi (Soundtrack from the Motion Picture)
              - AlbumId: 346
                Title: 'Mozart: Chamber Music'
              - AlbumId: 345
                Title: 'Monteverdi: L''Orfeo'
        |]

    it "can order by an aggregate" $ \(testEnvironment, _) -> do
      when (TE.backendType testEnvironment == Just Fixture.DataConnectorSqlite) (pendingWith "TODO: Test currently broken for SQLite DataConnector")
      shouldReturnYaml
        opts
        ( GraphqlEngine.postGraphql
            testEnvironment
            [graphql|
              query getArtists {
                Artist(limit: 3, order_by: {Albums_aggregate: {count: desc}}) {
                  Name
                  Albums_aggregate {
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
            Artist:
              - Name: Iron Maiden
                Albums_aggregate:
                  aggregate:
                    count: 21
              - Name: Led Zeppelin
                Albums_aggregate:
                  aggregate:
                    count: 14
              - Name: Deep Purple
                Albums_aggregate:
                  aggregate:
                    count: 11
        |]

    it "can order by a related field" $ \(testEnvironment, _) -> do
      when (TE.backendType testEnvironment == Just Fixture.DataConnectorSqlite) (pendingWith "TODO: Test currently broken for SQLite DataConnector")
      shouldReturnYaml
        opts
        ( GraphqlEngine.postGraphql
            testEnvironment
            [graphql|
              query getAlbums {
                Album(limit: 4, order_by: [{Artist: {Name: asc}}, {Title: desc}]) {
                  Artist {
                    Name
                  }
                  Title
                }
              }
            |]
        )
        [yaml|
          data:
            Album:
              - Artist:
                  Name: AC/DC
                Title: Let There Be Rock
              - Artist:
                  Name: AC/DC
                Title: For Those About To Rock We Salute You
              - Artist:
                  Name: Aaron Copland & London Symphony Orchestra
                Title: A Copland Celebration, Vol. I
              - Artist:
                  Name: Aaron Goldberg
                Title: Worlds
        |]
  describe "Custom scalar types and operators" $ do
    it "works with custom scalar types and comparison operators" $ \(testEnvironment, _) -> do
      when (TE.backendType testEnvironment == Just Fixture.DataConnectorSqlite) do
        pendingWith "TODO: Test currently broken for SQLite DataConnector"
      shouldReturnYaml
        opts
        ( GraphqlEngine.postGraphql
            testEnvironment
            [graphql|
              query MyQuery {
                Employee(where: {BirthDate: {in_year: 1965}}) {
                  BirthDate
                  LastName
                }
              }
            |]
        )
        [yaml|
          data:
            Employee:
            - BirthDate: '1965-03-03T00:00:00-08:00'
              LastName: Johnson
        |]
