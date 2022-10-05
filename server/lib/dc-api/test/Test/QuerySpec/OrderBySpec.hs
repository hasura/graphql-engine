module Test.QuerySpec.OrderBySpec (spec) where

import Control.Arrow ((>>>))
import Control.Lens (ix, (&), (.~), (?~), (^.), (^?), _1, _2, _3, _Just)
import Control.Monad (when)
import Data.Aeson (Value (..))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (isJust)
import Data.Ord (Down (..))
import Hasura.Backends.DataConnector.API
import Servant.API (NamedRoutes)
import Servant.Client (Client, (//))
import Test.Data (TestData (..))
import Test.Data qualified as Data
import Test.Expectations (jsonShouldBe, rowsShouldBe)
import Test.Hspec (Spec, describe, it)
import Prelude

spec :: TestData -> Client IO (NamedRoutes Routes) -> SourceName -> Config -> Capabilities -> Spec
spec TestData {..} api sourceName config Capabilities {..} = describe "Order By in Queries" $ do
  it "can order results in ascending order" $ do
    let orderBy = OrderBy mempty $ _tdOrderByColumn [] "Title" Ascending :| []
    let query = albumsQueryRequest & qrQuery . qOrderBy ?~ orderBy
    receivedAlbums <- (api // _query) sourceName config query

    let expectedAlbums = sortOn (^? Data.field "Title") _tdAlbumsRows
    Data.responseRows receivedAlbums `rowsShouldBe` expectedAlbums
    _qrAggregates receivedAlbums `jsonShouldBe` Nothing

  it "can order results in descending order" $ do
    let orderBy = OrderBy mempty $ _tdOrderByColumn [] "Title" Descending :| []
    let query = albumsQueryRequest & qrQuery . qOrderBy ?~ orderBy
    receivedAlbums <- (api // _query) sourceName config query

    let expectedAlbums = sortOn (Down . (^? Data.field "Title")) _tdAlbumsRows
    Data.responseRows receivedAlbums `rowsShouldBe` expectedAlbums
    _qrAggregates receivedAlbums `jsonShouldBe` Nothing

  it "can use multiple order by elements to order results" $ do
    let orderBy = OrderBy mempty $ _tdOrderByColumn [] "ArtistId" Ascending :| [_tdOrderByColumn [] "Title" Descending]
    let query = albumsQueryRequest & qrQuery . qOrderBy ?~ orderBy
    receivedAlbums <- (api // _query) sourceName config query

    let expectedAlbums =
          sortOn (\album -> (album ^? Data.field "ArtistId", Down (album ^? Data.field "Title"))) _tdAlbumsRows

    Data.responseRows receivedAlbums `rowsShouldBe` expectedAlbums
    _qrAggregates receivedAlbums `jsonShouldBe` Nothing

  when (isJust _cRelationships) . describe "involving relationships" $ do
    it "can order results by a column in a related table" $ do
      let orderByRelations = HashMap.fromList [(_tdArtistRelationshipName, OrderByRelation Nothing mempty)]
      let orderBy = OrderBy orderByRelations $ _tdOrderByColumn [_tdArtistRelationshipName] "Name" Ascending :| []
      let query =
            albumsQueryRequest
              & qrQuery . qOrderBy ?~ orderBy
              & qrTableRelationships .~ [Data.onlyKeepRelationships [_tdArtistRelationshipName] _tdAlbumsTableRelationships]
      receivedAlbums <- (api // _query) sourceName config query

      let getRelatedArtist (album :: HashMap FieldName FieldValue) =
            (album ^? Data.field "ArtistId" . Data._ColumnFieldNumber) >>= \artistId -> _tdArtistsRowsById ^? ix artistId

      let expectedAlbums =
            _tdAlbumsRows
              & fmap (\album -> (album, getRelatedArtist album))
              & sortOn ((^? _2 . _Just . Data.field "Name"))
              & fmap fst

      Data.responseRows receivedAlbums `rowsShouldBe` expectedAlbums
      _qrAggregates receivedAlbums `jsonShouldBe` Nothing

    it "can order results by a column in a related table where the related table is filtered" $ do
      let artistTableFilter = ApplyBinaryComparisonOperator GreaterThan (_tdCurrentComparisonColumn "Name") (ScalarValue $ String "N")
      let orderByRelations = HashMap.fromList [(_tdArtistRelationshipName, OrderByRelation (Just artistTableFilter) mempty)]
      let orderBy = OrderBy orderByRelations $ _tdOrderByColumn [_tdArtistRelationshipName] "Name" Ascending :| []
      let query =
            albumsQueryRequest
              & qrQuery . qOrderBy ?~ orderBy
              & qrTableRelationships .~ [Data.onlyKeepRelationships [_tdArtistRelationshipName] _tdAlbumsTableRelationships]
      receivedAlbums <- (api // _query) sourceName config query

      let getRelatedArtist (album :: HashMap FieldName FieldValue) = do
            artist <- (album ^? Data.field "ArtistId" . Data._ColumnFieldNumber) >>= \artistId -> _tdArtistsRowsById ^? ix artistId
            if artist ^? Data.field "Name" . Data._ColumnFieldString > Just "N"
              then pure artist
              else Nothing

      let expectedAlbums =
            _tdAlbumsRows
              & fmap (\album -> (album, getRelatedArtist album))
              & sortOn ((^? _2 . _Just . Data.field "Name") >>> toNullsLastOrdering)
              & fmap fst

      Data.responseRows receivedAlbums `rowsShouldBe` expectedAlbums
      _qrAggregates receivedAlbums `jsonShouldBe` Nothing

    it "can order results by a column in a related table of a related table" $ do
      let orderByRelations =
            HashMap.fromList
              [ ( _tdAlbumRelationshipName,
                  OrderByRelation
                    Nothing
                    ( HashMap.fromList
                        [ ( _tdArtistRelationshipName,
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
                [ _tdOrderByColumn [_tdAlbumRelationshipName, _tdArtistRelationshipName] "Name" Descending,
                  _tdOrderByColumn [] "Name" Ascending
                ]
      let query =
            tracksQueryRequest
              & qrQuery . qOrderBy ?~ orderBy
              & qrTableRelationships
                .~ [ Data.onlyKeepRelationships [_tdAlbumRelationshipName] _tdTracksTableRelationships,
                     Data.onlyKeepRelationships [_tdArtistRelationshipName] _tdAlbumsTableRelationships
                   ]
      receivedTracks <- (api // _query) sourceName config query

      let getRelatedArtist (track :: HashMap FieldName FieldValue) = do
            albumId <- track ^? Data.field "AlbumId" . Data._ColumnFieldNumber
            album <- _tdAlbumsRowsById ^? ix albumId
            artistId <- album ^? Data.field "ArtistId" . Data._ColumnFieldNumber
            _tdArtistsRowsById ^? ix artistId

      let expectedTracks =
            _tdTracksRows
              & fmap (\track -> (Data.filterColumnsByQueryFields (_qrQuery tracksQueryRequest) track, getRelatedArtist track, track ^? Data.field "Name"))
              & sortOn (\row -> (Down (row ^? _2 . _Just . Data.field "Name"), row ^. _3))
              & fmap (^. _1)

      Data.responseRows receivedTracks `rowsShouldBe` expectedTracks
      _qrAggregates receivedTracks `jsonShouldBe` Nothing

    it "can order results by an aggregate of a related table" $ do
      let orderByRelations = HashMap.fromList [(_tdAlbumsRelationshipName, OrderByRelation Nothing mempty)]
      let orderBy = OrderBy orderByRelations $ OrderByElement [_tdAlbumsRelationshipName] OrderByStarCountAggregate Descending :| []
      let query =
            artistsQueryRequest
              & qrQuery . qOrderBy ?~ orderBy
              & qrTableRelationships .~ [Data.onlyKeepRelationships [_tdAlbumsRelationshipName] _tdArtistsTableRelationships]
      receivedArtists <- (api // _query) sourceName config query

      let getAlbumsCount (artist :: HashMap FieldName FieldValue) = do
            artistId <- artist ^? Data.field "ArtistId" . Data._ColumnFieldNumber
            let albums = filter (\album -> album ^? Data.field "ArtistId" . Data._ColumnFieldNumber == Just artistId) _tdAlbumsRows
            pure $ length albums

      let expectedArtists =
            _tdArtistsRows
              & fmap (\artist -> (artist, getAlbumsCount artist))
              & sortOn (Down . (^. _2))
              & fmap fst

      Data.responseRows receivedArtists `rowsShouldBe` expectedArtists
      _qrAggregates receivedArtists `jsonShouldBe` Nothing

    it "can order results by an aggregate of a related table where the related table is filtered" $ do
      let albumTableFilter = ApplyBinaryComparisonOperator GreaterThan (_tdCurrentComparisonColumn "Title") (ScalarValue $ String "N")
      let orderByRelations = HashMap.fromList [(_tdAlbumsRelationshipName, OrderByRelation (Just albumTableFilter) mempty)]
      let orderBy = OrderBy orderByRelations $ OrderByElement [_tdAlbumsRelationshipName] OrderByStarCountAggregate Descending :| []
      let query =
            artistsQueryRequest
              & qrQuery . qOrderBy ?~ orderBy
              & qrTableRelationships .~ [Data.onlyKeepRelationships [_tdAlbumsRelationshipName] _tdArtistsTableRelationships]
      receivedArtists <- (api // _query) sourceName config query

      let getAlbumsCount (artist :: HashMap FieldName FieldValue) = do
            artistId <- artist ^? Data.field "ArtistId" . Data._ColumnFieldNumber
            let albums = filter (\album -> album ^? Data.field "ArtistId" . Data._ColumnFieldNumber == Just artistId && album ^? Data.field "Title" . Data._ColumnFieldString > Just "N") _tdAlbumsRows
            pure $ length albums

      let expectedArtists =
            _tdArtistsRows
              & fmap (\artist -> (artist, getAlbumsCount artist))
              & sortOn (Down . (^. _2))
              & fmap fst

      Data.responseRows receivedArtists `rowsShouldBe` expectedArtists
      _qrAggregates receivedArtists `jsonShouldBe` Nothing

    it "can order results by an aggregate of a related table's related table" $ do
      let orderByRelations =
            HashMap.fromList
              [ ( _tdArtistRelationshipName,
                  OrderByRelation
                    Nothing
                    ( HashMap.fromList
                        [ ( _tdAlbumsRelationshipName,
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
                [ OrderByElement [_tdArtistRelationshipName, _tdAlbumsRelationshipName] OrderByStarCountAggregate Descending,
                  _tdOrderByColumn [] "Title" Ascending
                ]
      let query =
            albumsQueryRequest
              & qrQuery . qOrderBy ?~ orderBy
              & qrTableRelationships
                .~ [ Data.onlyKeepRelationships [_tdArtistRelationshipName] _tdAlbumsTableRelationships,
                     Data.onlyKeepRelationships [_tdAlbumsRelationshipName] _tdArtistsTableRelationships
                   ]
      receivedAlbums <- (api // _query) sourceName config query

      let getTotalArtistAlbumsCount (album :: HashMap FieldName FieldValue) = do
            artistId <- album ^? Data.field "ArtistId" . Data._ColumnFieldNumber
            let albums = filter (\album' -> album' ^? Data.field "ArtistId" . Data._ColumnFieldNumber == Just artistId) _tdAlbumsRows
            pure $ length albums

      let expectedArtists =
            _tdAlbumsRows
              & fmap (\album -> (album, getTotalArtistAlbumsCount album, album ^? Data.field "Title"))
              & sortOn (\row -> (Down (row ^. _2), (row ^. _3)))
              & fmap (^. _1)

      Data.responseRows receivedAlbums `rowsShouldBe` expectedArtists
      _qrAggregates receivedAlbums `jsonShouldBe` Nothing
  where
    albumsQueryRequest :: QueryRequest
    albumsQueryRequest =
      let fields = Data.mkFieldsMap [("AlbumId", _tdColumnField "AlbumId"), ("ArtistId", _tdColumnField "ArtistId"), ("Title", _tdColumnField "Title")]
          query = Data.emptyQuery & qFields ?~ fields
       in QueryRequest _tdAlbumsTableName [] query

    artistsQueryRequest :: QueryRequest
    artistsQueryRequest =
      let fields = Data.mkFieldsMap [("ArtistId", _tdColumnField "ArtistId"), ("Name", _tdColumnField "Name")]
          query = Data.emptyQuery & qFields ?~ fields
       in QueryRequest _tdArtistsTableName [] query

    tracksQueryRequest :: QueryRequest
    tracksQueryRequest =
      let fields = Data.mkFieldsMap [("TrackId", _tdColumnField "TrackId"), ("Name", _tdColumnField "Name")]
          query = Data.emptyQuery & qFields ?~ fields
       in QueryRequest _tdTracksTableName [] query

data NullableOrdered a
  = NullFirst
  | Some a
  | NullLast
  deriving stock (Eq, Ord, Show)

toNullsLastOrdering :: Maybe a -> NullableOrdered a
toNullsLastOrdering = maybe NullLast Some
