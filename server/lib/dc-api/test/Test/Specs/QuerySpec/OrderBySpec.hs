module Test.Specs.QuerySpec.OrderBySpec (spec) where

import Control.Arrow ((>>>))
import Control.Lens (ix, (%~), (&), (.~), (?~), (^.), (^?), _1, _2, _3, _Just)
import Control.Monad (when)
import Data.Aeson (Value (..))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe, isJust)
import Data.Ord (Down (..))
import Hasura.Backends.DataConnector.API
import Test.AgentClient (queryGuarded)
import Test.Data (TestData (..))
import Test.Data qualified as Data
import Test.Expectations (jsonShouldBe, rowsShouldBe)
import Test.Sandwich (describe)
import Test.TestHelpers (AgentTestSpec, it)
import Prelude

spec :: TestData -> SourceName -> Config -> Capabilities -> AgentTestSpec
spec TestData {..} sourceName config Capabilities {..} = describe "Order By in Queries" $ do
  it "can order results in ascending order" $ do
    let orderBy = OrderBy mempty $ _tdOrderByColumn [] "Title" Ascending :| []
    let query = albumsQueryRequest & qrQuery . qOrderBy ?~ orderBy
    receivedAlbums <- queryGuarded sourceName config query

    let expectedAlbums = sortOn (^? Data.field "Title") _tdAlbumsRows
    Data.responseRows receivedAlbums `rowsShouldBe` expectedAlbums
    _qrAggregates receivedAlbums `jsonShouldBe` Nothing

  it "can order results in descending order" $ do
    let orderBy = OrderBy mempty $ _tdOrderByColumn [] "Title" Descending :| []
    let query = albumsQueryRequest & qrQuery . qOrderBy ?~ orderBy
    receivedAlbums <- queryGuarded sourceName config query

    let expectedAlbums = sortOn (Down . (^? Data.field "Title")) _tdAlbumsRows
    Data.responseRows receivedAlbums `rowsShouldBe` expectedAlbums
    _qrAggregates receivedAlbums `jsonShouldBe` Nothing

  it "can use multiple order by elements to order results" $ do
    let orderBy = OrderBy mempty $ _tdOrderByColumn [] "ArtistId" Ascending :| [_tdOrderByColumn [] "Title" Descending]
    let query = albumsQueryRequest & qrQuery . qOrderBy ?~ orderBy
    receivedAlbums <- queryGuarded sourceName config query

    let expectedAlbums =
          sortOn (\album -> (album ^? Data.field "ArtistId", Down (album ^? Data.field "Title"))) _tdAlbumsRows

    Data.responseRows receivedAlbums `rowsShouldBe` expectedAlbums
    _qrAggregates receivedAlbums `jsonShouldBe` Nothing

  it "orders nulls last when sorting ascending" $ do
    let orderBy =
          OrderBy mempty $
            NonEmpty.fromList
              [ _tdOrderByColumn [] "BillingState" Ascending,
                _tdOrderByColumn [] "InvoiceId" Ascending -- Required for a stable sort
              ]
    let query = invoicesQueryRequest & qrQuery . qOrderBy ?~ orderBy
    receivedInvoices <- queryGuarded sourceName config query

    let expectedInvoices =
          _tdInvoicesRows
            & sortOn (\row -> (row ^? Data.field "BillingState", row ^? Data.field "InvoiceId"))
            & fmap (Data.filterColumnsByQueryFields (invoicesQueryRequest ^. qrQuery))

    Data.responseRows receivedInvoices `rowsShouldBe` expectedInvoices
    _qrAggregates receivedInvoices `jsonShouldBe` Nothing

  it "orders nulls first when sorting descending" $ do
    let orderBy =
          OrderBy mempty $
            NonEmpty.fromList
              [ _tdOrderByColumn [] "BillingState" Descending,
                _tdOrderByColumn [] "InvoiceId" Descending -- Required for a stable sort
              ]
    let query = invoicesQueryRequest & qrQuery . qOrderBy ?~ orderBy
    receivedInvoices <- queryGuarded sourceName config query

    let expectedInvoices =
          _tdInvoicesRows
            & sortOn (\row -> (Down $ row ^? Data.field "BillingState", Down $ row ^? Data.field "InvoiceId"))
            & fmap (Data.filterColumnsByQueryFields (invoicesQueryRequest ^. qrQuery))

    Data.responseRows receivedInvoices `rowsShouldBe` expectedInvoices
    _qrAggregates receivedInvoices `jsonShouldBe` Nothing

  when (isJust _cRelationships) . describe "involving relationships" $ do
    it "can order results by a column in a related table" $ do
      let orderByRelations = HashMap.fromList [(_tdArtistRelationshipName, OrderByRelation Nothing mempty)]
      let orderBy =
            OrderBy orderByRelations $
              NonEmpty.fromList
                [ _tdOrderByColumn [_tdArtistRelationshipName] "Name" Ascending,
                  _tdOrderByColumn [] "AlbumId" Ascending
                ]
      let query =
            albumsQueryRequest
              & qrQuery . qOrderBy ?~ orderBy
              & qrTableRelationships .~ [Data.onlyKeepRelationships [_tdArtistRelationshipName] _tdAlbumsTableRelationships]
      receivedAlbums <- queryGuarded sourceName config query

      let getRelatedArtist (album :: HashMap FieldName FieldValue) =
            (album ^? Data.field "ArtistId" . Data._ColumnFieldNumber) >>= \artistId -> _tdArtistsRowsById ^? ix artistId

      let expectedAlbums =
            _tdAlbumsRows
              & fmap (\album -> (album, getRelatedArtist album))
              & sortOn (\row -> (row ^? _2 . _Just . Data.field "Name", row ^? _1 . Data.field "AlbumId"))
              & fmap fst

      Data.responseRows receivedAlbums `rowsShouldBe` expectedAlbums
      _qrAggregates receivedAlbums `jsonShouldBe` Nothing

    it "can order results by a column in a related table where the related table is filtered" $ do
      let artistTableFilter = ApplyBinaryComparisonOperator GreaterThan (_tdCurrentComparisonColumn "Name" _tdStringType) (ScalarValue (String "N") _tdStringType)
      let orderByRelations = HashMap.fromList [(_tdArtistRelationshipName, OrderByRelation (Just artistTableFilter) mempty)]
      let orderBy =
            OrderBy orderByRelations $
              NonEmpty.fromList
                [ _tdOrderByColumn [_tdArtistRelationshipName] "Name" Ascending,
                  _tdOrderByColumn [] "AlbumId" Ascending
                ]
      let query =
            albumsQueryRequest
              & qrQuery . qOrderBy ?~ orderBy
              & qrTableRelationships .~ [Data.onlyKeepRelationships [_tdArtistRelationshipName] _tdAlbumsTableRelationships]
      receivedAlbums <- queryGuarded sourceName config query

      let getRelatedArtist (album :: HashMap FieldName FieldValue) = do
            artist <- (album ^? Data.field "ArtistId" . Data._ColumnFieldNumber) >>= \artistId -> _tdArtistsRowsById ^? ix artistId
            if artist ^? Data.field "Name" . Data._ColumnFieldString > Just "N"
              then pure artist
              else Nothing

      let expectedAlbums =
            _tdAlbumsRows
              & fmap (\album -> (album, getRelatedArtist album))
              & sortOn (\row -> (row ^? _2 . _Just . Data.field "Name" & toNullsLastOrdering, row ^? _1 . Data.field "AlbumId"))
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
                  _tdOrderByColumn [] "Name" Ascending,
                  _tdOrderByColumn [] "TrackId" Ascending
                ]
      let query =
            tracksQueryRequest
              & qrQuery . qOrderBy ?~ orderBy
              & qrTableRelationships
                .~ [ Data.onlyKeepRelationships [_tdAlbumRelationshipName] _tdTracksTableRelationships,
                     Data.onlyKeepRelationships [_tdArtistRelationshipName] _tdAlbumsTableRelationships
                   ]
      receivedTracks <- queryGuarded sourceName config query

      let getRelatedArtist (track :: HashMap FieldName FieldValue) = do
            albumId <- track ^? Data.field "AlbumId" . Data._ColumnFieldNumber
            album <- _tdAlbumsRowsById ^? ix albumId
            artistId <- album ^? Data.field "ArtistId" . Data._ColumnFieldNumber
            _tdArtistsRowsById ^? ix artistId

      let expectedTracks =
            _tdTracksRows
              & fmap (\track -> (Data.filterColumnsByQueryFields (_qrQuery tracksQueryRequest) track, getRelatedArtist track, track))
              & sortOn (\row -> (Down (row ^? _2 . _Just . Data.field "Name"), row ^? _3 . Data.field "Name", row ^? _3 . Data.field "TrackId"))
              & fmap (^. _1)

      Data.responseRows receivedTracks `rowsShouldBe` expectedTracks
      _qrAggregates receivedTracks `jsonShouldBe` Nothing

    it "can order results separately within each array relationship" $ do
      -- Assemble a query of artists (desc by name) -> albums (desc by title) -> tracks (desc by name)
      -- But only a paginated subset of each, in order to reduce the size of final resultset to something manageable
      let tracksLimit = 5
      let albumsLimit = 3
      let artistsOffset = 75
      let artistsLimit = 5
      let tracksOrdering = OrderBy mempty $ _tdOrderByColumn [] "Name" Descending :| []
      let tracksField =
            RelField . RelationshipField _tdTracksRelationshipName $
              tracksQuery
                & qOrderBy ?~ tracksOrdering
                & qLimit ?~ tracksLimit
      let albumsOrdering = OrderBy mempty $ _tdOrderByColumn [] "Title" Descending :| []
      let albumsField =
            RelField . RelationshipField _tdAlbumsRelationshipName $
              albumsQuery
                & qFields . _Just %~ Data.insertField "Tracks" tracksField
                & qOrderBy ?~ albumsOrdering
                & qLimit ?~ albumsLimit
      let artistsOrdering = OrderBy mempty $ _tdOrderByColumn [] "Name" Descending :| []
      let query =
            artistsQueryRequest
              & qrQuery
                %~ ( qFields . _Just %~ Data.insertField "Albums" albumsField
                       >>> qOrderBy ?~ artistsOrdering
                       >>> qOffset ?~ artistsOffset
                       >>> qLimit ?~ artistsLimit
                   )
              & qrTableRelationships
                .~ [ Data.onlyKeepRelationships [_tdTracksRelationshipName] _tdAlbumsTableRelationships,
                     Data.onlyKeepRelationships [_tdAlbumsRelationshipName] _tdArtistsTableRelationships
                   ]
      receivedArtists <- queryGuarded sourceName config query

      let joinInTracks (album :: HashMap FieldName FieldValue) = fromMaybe album $ do
            albumId <- album ^? Data.field "AlbumId" . Data._ColumnFieldNumber
            let albums =
                  _tdTracksRows
                    & filter ((^? Data.field "AlbumId" . Data._ColumnFieldNumber) >>> (== Just albumId))
                    & sortOn ((^? Data.field "Name") >>> Down)
                    & take tracksLimit
                    & Data.filterColumns ["TrackId", "Name"]
            pure $ Data.insertField "Tracks" (mkSubqueryResponse (Just albums) Nothing) album

      let joinInAlbums (artist :: HashMap FieldName FieldValue) = fromMaybe artist $ do
            artistId <- artist ^? Data.field "ArtistId" . Data._ColumnFieldNumber
            let albums =
                  _tdAlbumsRows
                    & filter ((^? Data.field "ArtistId" . Data._ColumnFieldNumber) >>> (== Just artistId))
                    & sortOn ((^? Data.field "Title") >>> Down)
                    & take albumsLimit
                    & fmap joinInTracks
            pure $ Data.insertField "Albums" (mkSubqueryResponse (Just albums) Nothing) artist

      let expectedArtists =
            _tdArtistsRows
              & sortOn ((^? Data.field "Name") >>> Down)
              & drop artistsOffset
              & take artistsLimit
              & fmap joinInAlbums

      Data.responseRows receivedArtists `rowsShouldBe` expectedArtists
      _qrAggregates receivedArtists `jsonShouldBe` Nothing

    it "can order results by an aggregate of a related table" $ do
      let orderByRelations = HashMap.fromList [(_tdAlbumsRelationshipName, OrderByRelation Nothing mempty)]
      let orderBy =
            OrderBy orderByRelations $
              NonEmpty.fromList
                [ OrderByElement [_tdAlbumsRelationshipName] OrderByStarCountAggregate Descending,
                  _tdOrderByColumn [] "Name" Ascending
                ]
      let query =
            artistsQueryRequest
              & qrQuery . qOrderBy ?~ orderBy
              & qrTableRelationships .~ [Data.onlyKeepRelationships [_tdAlbumsRelationshipName] _tdArtistsTableRelationships]
      receivedArtists <- queryGuarded sourceName config query

      let getAlbumsCount (artist :: HashMap FieldName FieldValue) = do
            artistId <- artist ^? Data.field "ArtistId" . Data._ColumnFieldNumber
            let albums = filter (\album -> album ^? Data.field "ArtistId" . Data._ColumnFieldNumber == Just artistId) _tdAlbumsRows
            pure $ length albums

      let expectedArtists =
            _tdArtistsRows
              & fmap (\artist -> (artist, getAlbumsCount artist))
              & sortOn (\row -> (Down (row ^. _2), row ^? _1 . Data.field "Name"))
              & fmap fst

      Data.responseRows receivedArtists `rowsShouldBe` expectedArtists
      _qrAggregates receivedArtists `jsonShouldBe` Nothing

    it "can order results by an aggregate of a related table where the related table is filtered" $ do
      let albumTableFilter = ApplyBinaryComparisonOperator GreaterThan (_tdCurrentComparisonColumn "Title" _tdStringType) (ScalarValue (String "N") _tdStringType)
      let orderByRelations = HashMap.fromList [(_tdAlbumsRelationshipName, OrderByRelation (Just albumTableFilter) mempty)]
      let orderBy =
            OrderBy orderByRelations $
              NonEmpty.fromList
                [ OrderByElement [_tdAlbumsRelationshipName] OrderByStarCountAggregate Descending,
                  _tdOrderByColumn [] "Name" Ascending
                ]
      let query =
            artistsQueryRequest
              & qrQuery . qOrderBy ?~ orderBy
              & qrTableRelationships .~ [Data.onlyKeepRelationships [_tdAlbumsRelationshipName] _tdArtistsTableRelationships]
      receivedArtists <- queryGuarded sourceName config query

      let getAlbumsCount (artist :: HashMap FieldName FieldValue) = do
            artistId <- artist ^? Data.field "ArtistId" . Data._ColumnFieldNumber
            let albums = filter (\album -> album ^? Data.field "ArtistId" . Data._ColumnFieldNumber == Just artistId && album ^? Data.field "Title" . Data._ColumnFieldString > Just "N") _tdAlbumsRows
            pure $ length albums

      let expectedArtists =
            _tdArtistsRows
              & fmap (\artist -> (artist, getAlbumsCount artist))
              & sortOn (\row -> (Down (row ^. _2), row ^? _1 . Data.field "Name"))
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
      receivedAlbums <- queryGuarded sourceName config query

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
    albumsQuery :: Query
    albumsQuery =
      let fields = Data.mkFieldsMap [("AlbumId", _tdColumnField "AlbumId" _tdIntType), ("ArtistId", _tdColumnField "ArtistId" _tdIntType), ("Title", _tdColumnField "Title" _tdStringType)]
       in Data.emptyQuery & qFields ?~ fields

    albumsQueryRequest :: QueryRequest
    albumsQueryRequest =
      QueryRequest _tdAlbumsTableName [] albumsQuery

    artistsQueryRequest :: QueryRequest
    artistsQueryRequest =
      let fields = Data.mkFieldsMap [("ArtistId", _tdColumnField "ArtistId" _tdIntType), ("Name", _tdColumnField "Name" _tdStringType)]
          query = Data.emptyQuery & qFields ?~ fields
       in QueryRequest _tdArtistsTableName [] query

    tracksQuery :: Query
    tracksQuery =
      let fields = Data.mkFieldsMap [("TrackId", _tdColumnField "TrackId" _tdIntType), ("Name", _tdColumnField "Name" _tdStringType)]
       in Data.emptyQuery & qFields ?~ fields

    tracksQueryRequest :: QueryRequest
    tracksQueryRequest =
      QueryRequest _tdTracksTableName [] tracksQuery

    invoicesQueryRequest :: QueryRequest
    invoicesQueryRequest =
      let fields = Data.mkFieldsMap [("InvoiceId", _tdColumnField "InvoiceId" _tdIntType), ("BillingState", _tdColumnField "BillingState" _tdStringType)]
          query = Data.emptyQuery & qFields ?~ fields
       in QueryRequest _tdInvoicesTableName [] query

data NullableOrdered a
  = NullFirst
  | Some a
  | NullLast
  deriving stock (Eq, Ord, Show)

toNullsLastOrdering :: Maybe a -> NullableOrdered a
toNullsLastOrdering = maybe NullLast Some

mkSubqueryResponse :: Maybe [HashMap FieldName FieldValue] -> Maybe (HashMap FieldName Value) -> FieldValue
mkSubqueryResponse rows aggregates =
  mkRelationshipFieldValue $ QueryResponse rows aggregates
