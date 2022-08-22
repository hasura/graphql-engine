module Test.QuerySpec.OrderBySpec (spec) where

import Control.Arrow ((>>>))
import Control.Lens (ix, (&), (.~), (?~), (^.), (^?), _1, _2, _3, _Just)
import Control.Monad (when)
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.HashMap.Strict qualified as HashMap
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (isJust)
import Data.Ord (Down (..))
import Hasura.Backends.DataConnector.API
import Servant.API (NamedRoutes)
import Servant.Client (Client, (//))
import Test.Data qualified as Data
import Test.Expectations (jsonShouldBe, rowsShouldBe)
import Test.Hspec (Spec, describe, it)
import Prelude

spec :: Client IO (NamedRoutes Routes) -> SourceName -> Config -> Capabilities -> Spec
spec api sourceName config Capabilities {..} = describe "Order By in Queries" $ do
  it "can order results in ascending order" $ do
    let orderBy = OrderBy mempty $ Data.orderByColumn [] "Title" Ascending :| []
    let query = albumsQueryRequest & qrQuery . qOrderBy ?~ orderBy
    receivedAlbums <- (api // _query) sourceName config query

    let expectedAlbums = sortOn (^? ix "Title") Data.albumsRows
    Data.responseRows receivedAlbums `rowsShouldBe` expectedAlbums
    _qrAggregates receivedAlbums `jsonShouldBe` Nothing

  it "can order results in descending order" $ do
    let orderBy = OrderBy mempty $ Data.orderByColumn [] "Title" Descending :| []
    let query = albumsQueryRequest & qrQuery . qOrderBy ?~ orderBy
    receivedAlbums <- (api // _query) sourceName config query

    let expectedAlbums = sortOn (Down . (^? ix "Title")) Data.albumsRows
    Data.responseRows receivedAlbums `rowsShouldBe` expectedAlbums
    _qrAggregates receivedAlbums `jsonShouldBe` Nothing

  it "can use multiple order by elements to order results" $ do
    let orderBy = OrderBy mempty $ Data.orderByColumn [] "ArtistId" Ascending :| [Data.orderByColumn [] "Title" Descending]
    let query = albumsQueryRequest & qrQuery . qOrderBy ?~ orderBy
    receivedAlbums <- (api // _query) sourceName config query

    let expectedAlbums =
          sortOn (\album -> (album ^? ix "ArtistId", Down (album ^? ix "Title"))) Data.albumsRows

    Data.responseRows receivedAlbums `rowsShouldBe` expectedAlbums
    _qrAggregates receivedAlbums `jsonShouldBe` Nothing

  when (isJust cRelationships) $ orderByWithRelationshipsSpec api sourceName config

orderByWithRelationshipsSpec :: Client IO (NamedRoutes Routes) -> SourceName -> Config -> Spec
orderByWithRelationshipsSpec api sourceName config = describe "involving relationships" $ do
  it "can order results by a column in a related table" $ do
    let orderByRelations = HashMap.fromList [(Data.artistRelationshipName, OrderByRelation Nothing mempty)]
    let orderBy = OrderBy orderByRelations $ Data.orderByColumn [Data.artistRelationshipName] "Name" Ascending :| []
    let query =
          albumsQueryRequest
            & qrQuery . qOrderBy ?~ orderBy
            & qrTableRelationships .~ [Data.onlyKeepRelationships [Data.artistRelationshipName] Data.albumsTableRelationships]
    receivedAlbums <- (api // _query) sourceName config query

    let getRelatedArtist (album :: KeyMap FieldValue) =
          (album ^? ix "ArtistId" . Data._ColumnFieldNumber) >>= \artistId -> Data.artistsRowsById ^? ix artistId

    let expectedAlbums =
          Data.albumsRows
            & fmap (\album -> (album, getRelatedArtist album))
            & sortOn ((^? _2 . _Just . ix "Name"))
            & fmap fst

    Data.responseRows receivedAlbums `rowsShouldBe` expectedAlbums
    _qrAggregates receivedAlbums `jsonShouldBe` Nothing

  it "can order results by a column in a related table where the related table is filtered" $ do
    let artistTableFilter = ApplyBinaryComparisonOperator GreaterThan (Data.localComparisonColumn "Name") (ScalarValue $ String "N")
    let orderByRelations = HashMap.fromList [(Data.artistRelationshipName, OrderByRelation (Just artistTableFilter) mempty)]
    let orderBy = OrderBy orderByRelations $ Data.orderByColumn [Data.artistRelationshipName] "Name" Ascending :| []
    let query =
          albumsQueryRequest
            & qrQuery . qOrderBy ?~ orderBy
            & qrTableRelationships .~ [Data.onlyKeepRelationships [Data.artistRelationshipName] Data.albumsTableRelationships]
    receivedAlbums <- (api // _query) sourceName config query

    let getRelatedArtist (album :: KeyMap FieldValue) = do
          artist <- (album ^? ix "ArtistId" . Data._ColumnFieldNumber) >>= \artistId -> Data.artistsRowsById ^? ix artistId
          if artist ^? ix "Name" . Data._ColumnFieldString > Just "N"
            then pure artist
            else Nothing

    let expectedAlbums =
          Data.albumsRows
            & fmap (\album -> (album, getRelatedArtist album))
            & sortOn ((^? _2 . _Just . ix "Name") >>> toNullsLastOrdering)
            & fmap fst

    Data.responseRows receivedAlbums `rowsShouldBe` expectedAlbums
    _qrAggregates receivedAlbums `jsonShouldBe` Nothing

  it "can order results by a column in a related table of a related table" $ do
    let orderByRelations =
          HashMap.fromList
            [ ( Data.albumRelationshipName,
                OrderByRelation
                  Nothing
                  ( HashMap.fromList
                      [ ( Data.artistRelationshipName,
                          OrderByRelation
                            Nothing
                            mempty
                        )
                      ]
                  )
              )
            ]
    let orderBy =
          OrderBy orderByRelations $
            NonEmpty.fromList
              [ Data.orderByColumn [Data.albumRelationshipName, Data.artistRelationshipName] "Name" Descending,
                Data.orderByColumn [] "Name" Ascending
              ]
    let query =
          tracksQueryRequest
            & qrQuery . qOrderBy ?~ orderBy
            & qrTableRelationships
              .~ [ Data.onlyKeepRelationships [Data.albumRelationshipName] Data.tracksTableRelationships,
                   Data.onlyKeepRelationships [Data.artistRelationshipName] Data.albumsTableRelationships
                 ]
    receivedTracks <- (api // _query) sourceName config query

    let getRelatedArtist (track :: KeyMap FieldValue) = do
          albumId <- track ^? ix "AlbumId" . Data._ColumnFieldNumber
          album <- Data.albumsRowsById ^? ix albumId
          artistId <- album ^? ix "ArtistId" . Data._ColumnFieldNumber
          Data.artistsRowsById ^? ix artistId

    let expectedTracks =
          Data.tracksRows
            & fmap (\track -> (Data.filterColumnsByQueryFields (_qrQuery tracksQueryRequest) track, getRelatedArtist track, track ^? ix "Name"))
            & sortOn (\row -> (Down (row ^? _2 . _Just . ix "Name"), row ^. _3))
            & fmap (^. _1)

    Data.responseRows receivedTracks `rowsShouldBe` expectedTracks
    _qrAggregates receivedTracks `jsonShouldBe` Nothing

  it "can order results by an aggregate of a related table" $ do
    let orderByRelations = HashMap.fromList [(Data.albumsRelationshipName, OrderByRelation Nothing mempty)]
    let orderBy = OrderBy orderByRelations $ OrderByElement [Data.albumsRelationshipName] OrderByStarCountAggregate Descending :| []
    let query =
          artistsQueryRequest
            & qrQuery . qOrderBy ?~ orderBy
            & qrTableRelationships .~ [Data.onlyKeepRelationships [Data.albumsRelationshipName] Data.artistsTableRelationships]
    receivedArtists <- (api // _query) sourceName config query

    let getAlbumsCount (artist :: KeyMap FieldValue) = do
          artistId <- artist ^? ix "ArtistId" . Data._ColumnFieldNumber
          let albums = filter (\album -> album ^? ix "ArtistId" . Data._ColumnFieldNumber == Just artistId) Data.albumsRows
          pure $ length albums

    let expectedArtists =
          Data.artistsRows
            & fmap (\artist -> (artist, getAlbumsCount artist))
            & sortOn (Down . (^. _2))
            & fmap fst

    Data.responseRows receivedArtists `rowsShouldBe` expectedArtists
    _qrAggregates receivedArtists `jsonShouldBe` Nothing

  it "can order results by an aggregate of a related table where the related table is filtered" $ do
    let albumTableFilter = ApplyBinaryComparisonOperator GreaterThan (Data.localComparisonColumn "Title") (ScalarValue $ String "N")
    let orderByRelations = HashMap.fromList [(Data.albumsRelationshipName, OrderByRelation (Just albumTableFilter) mempty)]
    let orderBy = OrderBy orderByRelations $ OrderByElement [Data.albumsRelationshipName] OrderByStarCountAggregate Descending :| []
    let query =
          artistsQueryRequest
            & qrQuery . qOrderBy ?~ orderBy
            & qrTableRelationships .~ [Data.onlyKeepRelationships [Data.albumsRelationshipName] Data.artistsTableRelationships]
    receivedArtists <- (api // _query) sourceName config query

    let getAlbumsCount (artist :: KeyMap FieldValue) = do
          artistId <- artist ^? ix "ArtistId" . Data._ColumnFieldNumber
          let albums = filter (\album -> album ^? ix "ArtistId" . Data._ColumnFieldNumber == Just artistId && album ^? ix "Title" . Data._ColumnFieldString > Just "N") Data.albumsRows
          pure $ length albums

    let expectedArtists =
          Data.artistsRows
            & fmap (\artist -> (artist, getAlbumsCount artist))
            & sortOn (Down . (^. _2))
            & fmap fst

    Data.responseRows receivedArtists `rowsShouldBe` expectedArtists
    _qrAggregates receivedArtists `jsonShouldBe` Nothing

  it "can order results by an aggregate of a related table's related table" $ do
    let orderByRelations =
          HashMap.fromList
            [ ( Data.artistRelationshipName,
                OrderByRelation
                  Nothing
                  ( HashMap.fromList
                      [ ( Data.albumsRelationshipName,
                          OrderByRelation
                            Nothing
                            mempty
                        )
                      ]
                  )
              )
            ]
    let orderBy =
          OrderBy orderByRelations $
            NonEmpty.fromList
              [ OrderByElement [Data.artistRelationshipName, Data.albumsRelationshipName] OrderByStarCountAggregate Descending,
                OrderByElement [] (OrderByColumn $ ColumnName "Title") Ascending
              ]
    let query =
          albumsQueryRequest
            & qrQuery . qOrderBy ?~ orderBy
            & qrTableRelationships
              .~ [ Data.onlyKeepRelationships [Data.artistRelationshipName] Data.albumsTableRelationships,
                   Data.onlyKeepRelationships [Data.albumsRelationshipName] Data.artistsTableRelationships
                 ]
    receivedAlbums <- (api // _query) sourceName config query

    let getTotalArtistAlbumsCount (album :: KeyMap FieldValue) = do
          artistId <- album ^? ix "ArtistId" . Data._ColumnFieldNumber
          let albums = filter (\album' -> album' ^? ix "ArtistId" . Data._ColumnFieldNumber == Just artistId) Data.albumsRows
          pure $ length albums

    let expectedArtists =
          Data.albumsRows
            & fmap (\album -> (album, getTotalArtistAlbumsCount album, album ^? ix "Title"))
            & sortOn (\row -> (Down (row ^. _2), (row ^. _3)))
            & fmap (^. _1)

    Data.responseRows receivedAlbums `rowsShouldBe` expectedArtists
    _qrAggregates receivedAlbums `jsonShouldBe` Nothing

albumsQueryRequest :: QueryRequest
albumsQueryRequest =
  let fields = KeyMap.fromList [("AlbumId", Data.columnField "AlbumId"), ("ArtistId", Data.columnField "ArtistId"), ("Title", Data.columnField "Title")]
      query = Data.emptyQuery & qFields ?~ fields
   in QueryRequest Data.albumsTableName [] query

artistsQueryRequest :: QueryRequest
artistsQueryRequest =
  let fields = KeyMap.fromList [("ArtistId", Data.columnField "ArtistId"), ("Name", Data.columnField "Name")]
      query = Data.emptyQuery & qFields ?~ fields
   in QueryRequest Data.artistsTableName [] query

tracksQueryRequest :: QueryRequest
tracksQueryRequest =
  let fields = KeyMap.fromList [("TrackId", Data.columnField "TrackId"), ("Name", Data.columnField "Name")]
      query = Data.emptyQuery & qFields ?~ fields
   in QueryRequest Data.tracksTableName [] query

data NullableOrdered a
  = NullFirst
  | Some a
  | NullLast
  deriving stock (Eq, Ord, Show)

toNullsLastOrdering :: Maybe a -> NullableOrdered a
toNullsLastOrdering = maybe NullLast Some
