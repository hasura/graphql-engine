{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}

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
import Data.Set qualified as Set
import Hasura.Backends.DataConnector.API
import Hasura.Backends.DataConnector.API.V0.Relationships as API
import Language.GraphQL.Draft.Syntax.QQ qualified as G
import Test.AgentAPI (queryGuarded)
import Test.Data (TestData (..))
import Test.Data qualified as Data
import Test.Expectations (jsonShouldBe, rowsShouldBe)
import Test.Sandwich (describe)
import Test.TestHelpers (AgentDatasetTestSpec, it)
import Prelude

spec :: TestData -> Capabilities -> AgentDatasetTestSpec
spec TestData {..} Capabilities {..} = describe "Order By in Queries" $ do
  it "can order results in ascending order" $ do
    let orderBy = OrderBy mempty $ _tdOrderByColumn [] "Title" Ascending :| []
    let query = albumsQueryRequest & qrQuery . qOrderBy ?~ orderBy
    receivedAlbums <- queryGuarded query

    let expectedAlbums = sortOn (^? Data.field "Title") _tdAlbumsRows
    Data.responseRows receivedAlbums `rowsShouldBe` expectedAlbums
    _qrAggregates receivedAlbums `jsonShouldBe` Nothing

  it "can order results in descending order" $ do
    let orderBy = OrderBy mempty $ _tdOrderByColumn [] "Title" Descending :| []
    let query = albumsQueryRequest & qrQuery . qOrderBy ?~ orderBy
    receivedAlbums <- queryGuarded query

    let expectedAlbums = sortOn (Down . (^? Data.field "Title")) _tdAlbumsRows
    Data.responseRows receivedAlbums `rowsShouldBe` expectedAlbums
    _qrAggregates receivedAlbums `jsonShouldBe` Nothing

  it "can use multiple order by elements to order results" $ do
    let orderBy = OrderBy mempty $ _tdOrderByColumn [] "ArtistId" Ascending :| [_tdOrderByColumn [] "Title" Descending]
    let query = albumsQueryRequest & qrQuery . qOrderBy ?~ orderBy
    receivedAlbums <- queryGuarded query

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
    receivedInvoices <- queryGuarded query

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
    receivedInvoices <- queryGuarded query

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
              & qrRelationships .~ Set.fromList [API.RTable $ Data.onlyKeepRelationships [_tdArtistRelationshipName] _tdAlbumsTableRelationships]
      receivedAlbums <- queryGuarded query

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
      let artistTableFilter = ApplyBinaryComparisonOperator GreaterThan (_tdCurrentComparisonColumn "Name" artistNameScalarType) (Data.scalarValueComparison (String "N") artistNameScalarType)
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
              & qrRelationships .~ Set.fromList [API.RTable $ Data.onlyKeepRelationships [_tdArtistRelationshipName] _tdAlbumsTableRelationships]
      receivedAlbums <- queryGuarded query

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
              & qrRelationships
                .~ Set.fromList
                  [ API.RTable $ Data.onlyKeepRelationships [_tdAlbumRelationshipName] _tdTracksTableRelationships,
                    API.RTable $ Data.onlyKeepRelationships [_tdArtistRelationshipName] _tdAlbumsTableRelationships
                  ]
      receivedTracks <- queryGuarded query

      let getRelatedArtist (track :: HashMap FieldName FieldValue) = do
            albumId <- track ^? Data.field "AlbumId" . Data._ColumnFieldNumber
            album <- _tdAlbumsRowsById ^? ix albumId
            artistId <- album ^? Data.field "ArtistId" . Data._ColumnFieldNumber
            _tdArtistsRowsById ^? ix artistId

      let expectedTracks =
            _tdTracksRows
              & fmap (\track -> (Data.filterColumnsByQueryFields (tracksQueryRequest ^. qrQuery) track, getRelatedArtist track, track))
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
              & qrRelationships
                .~ Set.fromList
                  [ API.RTable $ Data.onlyKeepRelationships [_tdTracksRelationshipName] _tdAlbumsTableRelationships,
                    API.RTable $ Data.onlyKeepRelationships [_tdAlbumsRelationshipName] _tdArtistsTableRelationships
                  ]
      receivedArtists <- queryGuarded query

      let joinInTracks (album :: HashMap FieldName FieldValue) = fromMaybe album $ do
            albumId <- album ^? Data.field "AlbumId" . Data._ColumnFieldNumber
            let albums =
                  _tdTracksRows
                    & filter ((^? Data.field "AlbumId" . Data._ColumnFieldNumber) >>> (== Just albumId))
                    & sortOn ((^? Data.field "Name") >>> Down)
                    & take tracksLimit
                    & Data.filterColumns ["TrackId", "Name"]
            pure $ Data.insertField "Tracks" (Data.mkSubqueryFieldValue (Just albums) Nothing) album

      let joinInAlbums (artist :: HashMap FieldName FieldValue) = fromMaybe artist $ do
            artistId <- artist ^? Data.field "ArtistId" . Data._ColumnFieldNumber
            let albums =
                  _tdAlbumsRows
                    & filter ((^? Data.field "ArtistId" . Data._ColumnFieldNumber) >>> (== Just artistId))
                    & sortOn ((^? Data.field "Title") >>> Down)
                    & take albumsLimit
                    & fmap joinInTracks
            pure $ Data.insertField "Albums" (Data.mkSubqueryFieldValue (Just albums) Nothing) artist

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
              & qrRelationships .~ Set.fromList [API.RTable $ Data.onlyKeepRelationships [_tdAlbumsRelationshipName] _tdArtistsTableRelationships]
      receivedArtists <- queryGuarded query

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
      let albumTableFilter = ApplyBinaryComparisonOperator GreaterThan (_tdCurrentComparisonColumn "Title" albumTitleScalarType) (Data.scalarValueComparison (String "N") albumTitleScalarType)
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
              & qrRelationships .~ Set.fromList [API.RTable $ Data.onlyKeepRelationships [_tdAlbumsRelationshipName] _tdArtistsTableRelationships]
      receivedArtists <- queryGuarded query

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
              & qrRelationships
                .~ Set.fromList
                  [ API.RTable $ Data.onlyKeepRelationships [_tdArtistRelationshipName] _tdAlbumsTableRelationships,
                    API.RTable $ Data.onlyKeepRelationships [_tdAlbumsRelationshipName] _tdArtistsTableRelationships
                  ]
      receivedAlbums <- queryGuarded query

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

    it "can order results by an aggregate function applied to a column of a related table" $ do
      -- Order albums by their highest track id descending
      let orderByRelations =
            HashMap.fromList
              [ ( _tdTracksRelationshipName,
                  OrderByRelation
                    Nothing
                    mempty
                )
              ]
      let orderBy =
            OrderBy orderByRelations $
              NonEmpty.fromList
                [ OrderByElement [_tdTracksRelationshipName] (orderBySingleColumnAggregateMax (_tdColumnName "TrackId") trackIdNameScalarType) Descending
                ]
      let query =
            albumsQueryRequest
              & qrQuery . qOrderBy ?~ orderBy
              & qrRelationships
                .~ Set.fromList [API.RTable $ Data.onlyKeepRelationships [_tdTracksRelationshipName] _tdAlbumsTableRelationships]
      receivedAlbums <- queryGuarded query

      let findRelatedTracks (album :: HashMap FieldName FieldValue) = fromMaybe [] do
            albumId <- album ^? Data.field "AlbumId" . Data._ColumnFieldNumber
            pure $ filter (\track -> track ^? Data.field "AlbumId" . Data._ColumnFieldNumber == Just albumId) _tdTracksRows

      let expectedAlbums =
            _tdAlbumsRows
              & fmap (\album -> (album, maximum $ (findRelatedTracks album & fmap (\track -> track ^? Data.field "TrackId" . Data._ColumnFieldNumber))))
              & sortOn (\row -> (Down (row ^. _2)))
              & fmap (^. _1)

      Data.responseRows receivedAlbums `rowsShouldBe` expectedAlbums
      _qrAggregates receivedAlbums `jsonShouldBe` Nothing
  where
    albumsQuery :: Query
    albumsQuery =
      let fields = Data.mkFieldsMap [("AlbumId", _tdColumnField _tdAlbumsTableName "AlbumId"), ("ArtistId", _tdColumnField _tdAlbumsTableName "ArtistId"), ("Title", _tdColumnField _tdAlbumsTableName "Title")]
       in Data.emptyQuery & qFields ?~ fields

    albumsQueryRequest :: QueryRequest
    albumsQueryRequest =
      TableQueryRequest _tdAlbumsTableName mempty mempty mempty albumsQuery Nothing

    artistsQueryRequest :: QueryRequest
    artistsQueryRequest =
      let fields = Data.mkFieldsMap [("ArtistId", _tdColumnField _tdArtistsTableName "ArtistId"), ("Name", _tdColumnField _tdArtistsTableName "Name")]
          query = Data.emptyQuery & qFields ?~ fields
       in TableQueryRequest _tdArtistsTableName mempty mempty mempty query Nothing

    tracksQuery :: Query
    tracksQuery =
      let fields = Data.mkFieldsMap [("TrackId", _tdColumnField _tdTracksTableName "TrackId"), ("Name", _tdColumnField _tdTracksTableName "Name")]
       in Data.emptyQuery & qFields ?~ fields

    tracksQueryRequest :: QueryRequest
    tracksQueryRequest =
      TableQueryRequest _tdTracksTableName mempty mempty mempty tracksQuery Nothing

    invoicesQueryRequest :: QueryRequest
    invoicesQueryRequest =
      let fields = Data.mkFieldsMap [("InvoiceId", _tdColumnField _tdInvoicesTableName "InvoiceId"), ("BillingState", _tdColumnField _tdInvoicesTableName "BillingState")]
          query = Data.emptyQuery & qFields ?~ fields
       in TableQueryRequest _tdInvoicesTableName mempty mempty mempty query Nothing

    orderBySingleColumnAggregateMax :: ColumnName -> ScalarType -> OrderByTarget
    orderBySingleColumnAggregateMax columnName resultType = OrderBySingleColumnAggregate $ SingleColumnAggregate (SingleColumnAggregateFunction [G.name|max|]) columnName Nothing resultType

    albumTitleScalarType = _tdFindColumnScalarType _tdAlbumsTableName "Title"
    artistNameScalarType = _tdFindColumnScalarType _tdArtistsTableName "Name"
    trackIdNameScalarType = _tdFindColumnScalarType _tdTracksTableName "TrackId"

data NullableOrdered a
  = NullFirst
  | Some a
  | NullLast
  deriving stock (Eq, Ord, Show)

toNullsLastOrdering :: Maybe a -> NullableOrdered a
toNullsLastOrdering = maybe NullLast Some
