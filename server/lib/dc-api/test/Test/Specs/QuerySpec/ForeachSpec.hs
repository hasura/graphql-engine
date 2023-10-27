module Test.Specs.QuerySpec.ForeachSpec (spec) where

import Control.Arrow ((>>>))
import Control.Lens (ix, (%~), (&), (.~), (<&>), (?~), (^?), _Just)
import Data.Aeson qualified as J
import Data.Foldable (for_)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (maybeToList)
import Data.Ord (Down (..))
import Data.Set qualified as Set
import Data.Text (Text)
import Hasura.Backends.DataConnector.API
import Hasura.Backends.DataConnector.API.V0.Relationships as API
import Test.AgentAPI (queryGuarded)
import Test.Data (TestData (..))
import Test.Data qualified as Data
import Test.Expectations (jsonShouldBe, rowsShouldBe)
import Test.Sandwich (describe)
import Test.TestHelpers (AgentDatasetTestSpec, it)
import Prelude

spec :: TestData -> Capabilities -> AgentDatasetTestSpec
spec TestData {..} Capabilities {..} = describe "Foreach Queries" $ do
  it "foreach ids are used to filter results" $ do
    let foreachIds =
          NonEmpty.fromList
            [ mkForeachIds _tdAlbumsTableName [("ArtistId", J.Number 2)],
              mkForeachIds _tdAlbumsTableName [("ArtistId", J.Number 5)]
            ]
    let query = albumsQueryRequest & qrForeach ?~ foreachIds
    receivedForeachResponse <- queryGuarded query

    let getAlbumsByArtistId artistId =
          _tdAlbumsRows
            & filter (\album -> album ^? Data.field "ArtistId" . Data._ColumnFieldNumber == Just artistId)
            & sortOn (^? Data.field "AlbumId")

    let expectedForeachResponseRows =
          mkForeachResponseRows
            [ getAlbumsByArtistId 2,
              getAlbumsByArtistId 5
            ]

    Data.responseRows receivedForeachResponse `rowsShouldBe` expectedForeachResponseRows
    _qrAggregates receivedForeachResponse `jsonShouldBe` Nothing

  it "using composite foreach ids works" $ do
    let foreachIds =
          NonEmpty.fromList
            [ mkForeachIds _tdPlaylistTracksTableName [("PlaylistId", J.Number 8), ("TrackId", J.Number 1870)],
              mkForeachIds _tdPlaylistTracksTableName [("PlaylistId", J.Number 1), ("TrackId", J.Number 2192)],
              mkForeachIds _tdPlaylistTracksTableName [("PlaylistId", J.Number 5), ("TrackId", J.Number 1227)]
            ]
    let query = playlistTracksQueryRequest & qrForeach ?~ foreachIds
    receivedForeachResponse <- queryGuarded query

    let getPlaylistTrack playlistId trackId =
          _tdPlaylistTracksRows
            & filter (\playlistTrack -> playlistTrack ^? Data.field "PlaylistId" . Data._ColumnFieldNumber == Just playlistId && playlistTrack ^? Data.field "TrackId" . Data._ColumnFieldNumber == Just trackId)

    let expectedForeachResponseRows =
          mkForeachResponseRows
            [ getPlaylistTrack 8 1870,
              getPlaylistTrack 1 2192,
              getPlaylistTrack 5 1227
            ]

    Data.responseRows receivedForeachResponse `rowsShouldBe` expectedForeachResponseRows
    _qrAggregates receivedForeachResponse `jsonShouldBe` Nothing

  it "further filtering can be applied within each foreach subquery" $ do
    let foreachIds =
          NonEmpty.fromList
            [ mkForeachIds _tdAlbumsTableName [("ArtistId", J.Number 1)],
              mkForeachIds _tdAlbumsTableName [("ArtistId", J.Number 2)]
            ]
    let albumTitles = ["Restless and Wild", "For Those About To Rock We Salute You"]
    let whereExp = ApplyBinaryArrayComparisonOperator In (_tdCurrentComparisonColumn "Title" albumTitleScalarType) (J.String <$> albumTitles) albumTitleScalarType
    let query =
          albumsQueryRequest
            & qrForeach ?~ foreachIds
            & qrQuery . qWhere ?~ whereExp
    receivedForeachResponse <- queryGuarded query

    let getAlbumsByArtistId artistId =
          _tdAlbumsRows
            & filter
              ( \album ->
                  album ^? Data.field "ArtistId" . Data._ColumnFieldNumber == Just artistId
                    && album ^? Data.field "Title" . Data._ColumnFieldString `elem` (Just <$> albumTitles)
              )
            & sortOn (^? Data.field "AlbumId")

    let expectedForeachResponseRows =
          mkForeachResponseRows
            [ getAlbumsByArtistId 1,
              getAlbumsByArtistId 2
            ]

    Data.responseRows receivedForeachResponse `rowsShouldBe` expectedForeachResponseRows
    _qrAggregates receivedForeachResponse `jsonShouldBe` Nothing

  it "pagination and ordering can be applied within each foreach subquery" $ do
    let foreachIds =
          NonEmpty.fromList
            [ mkForeachIds _tdAlbumsTableName [("ArtistId", J.Number 50)],
              mkForeachIds _tdAlbumsTableName [("ArtistId", J.Number 22)],
              mkForeachIds _tdAlbumsTableName [("ArtistId", J.Number 27)]
            ]
    let orderBy = OrderBy mempty $ _tdOrderByColumn [] "AlbumId" Descending :| []
    let query =
          albumsQueryRequest
            & qrForeach ?~ foreachIds
            & qrQuery
              %~ ( qOrderBy ?~ orderBy
                     >>> qOffset ?~ 1
                     >>> qLimit ?~ 5
                 )
    receivedForeachResponse <- queryGuarded query

    let getAlbumsByArtistId artistId =
          _tdAlbumsRows
            & filter (\album -> album ^? Data.field "ArtistId" . Data._ColumnFieldNumber == Just artistId)
            & sortOn ((^? Data.field "AlbumId") >>> Down)
            & drop 1
            & take 5

    let expectedForeachResponseRows =
          mkForeachResponseRows
            [ getAlbumsByArtistId 50,
              getAlbumsByArtistId 22,
              getAlbumsByArtistId 27
            ]

    Data.responseRows receivedForeachResponse `rowsShouldBe` expectedForeachResponseRows
    _qrAggregates receivedForeachResponse `jsonShouldBe` Nothing

  it "aggregations can be used within each foreach subquery" $ do
    let foreachIds =
          NonEmpty.fromList
            [ mkForeachIds _tdAlbumsTableName [("ArtistId", J.Number 50)],
              mkForeachIds _tdAlbumsTableName [("ArtistId", J.Number 22)],
              mkForeachIds _tdAlbumsTableName [("ArtistId", J.Number 27)]
            ]
    let query =
          albumsQueryRequest
            & qrForeach ?~ foreachIds
            & qrQuery
              %~ ( qFields .~ Nothing
                     >>> qAggregates ?~ Data.mkFieldsMap [("AlbumsCount", StarCount)]
                 )
    receivedForeachResponse <- queryGuarded query

    let getAlbumsAggregateByArtistId artistId =
          let albumsCount = length $ filter (\album -> album ^? Data.field "ArtistId" . Data._ColumnFieldNumber == Just artistId) _tdAlbumsRows
              aggregates = Data.mkFieldsMap [("AlbumsCount", J.Number $ fromIntegral albumsCount)]
           in Data.mkSubqueryAggregatesFieldValue aggregates

    let expectedForeachResponseRows =
          mkForeachResponses
            [ getAlbumsAggregateByArtistId 50,
              getAlbumsAggregateByArtistId 22,
              getAlbumsAggregateByArtistId 27
            ]

    Data.responseRows receivedForeachResponse `rowsShouldBe` expectedForeachResponseRows
    _qrAggregates receivedForeachResponse `jsonShouldBe` Nothing

  for_ _cRelationships $ \_relationshipsCapabilities -> describe "Relationships" $ do
    it "object relationships can be navigated within each foreach subquery" $ do
      let foreachIds =
            NonEmpty.fromList
              [ mkForeachIds _tdAlbumsTableName [("ArtistId", J.Number 82)],
                mkForeachIds _tdAlbumsTableName [("ArtistId", J.Number 68)]
              ]
      let query =
            albumsQueryRequest
              & qrForeach ?~ foreachIds
              -- Add the Artist object relationship field
              & qrQuery . qFields . _Just . Data.fieldAt "Artist" ?~ RelField (RelationshipField _tdArtistRelationshipName artistsQuery)
              & qrRelationships .~ Set.fromList [API.RTable $ Data.onlyKeepRelationships [_tdArtistRelationshipName] _tdAlbumsTableRelationships]
      receivedForeachResponse <- queryGuarded query

      let joinInArtist (album :: HashMap FieldName FieldValue) =
            let artist = (album ^? Data.field "ArtistId" . Data._ColumnFieldNumber) >>= \artistId -> _tdArtistsRowsById ^? ix artistId
                artistPropVal = maybeToList artist
             in Data.insertField "Artist" (Data.mkSubqueryRowsFieldValue artistPropVal) album

      let getAlbumsByArtistId artistId =
            _tdAlbumsRows
              & filter (\album -> album ^? Data.field "ArtistId" . Data._ColumnFieldNumber == Just artistId)
              & sortOn (^? Data.field "AlbumId")
              & fmap joinInArtist

      let expectedForeachResponseRows =
            mkForeachResponseRows
              [ getAlbumsByArtistId 82,
                getAlbumsByArtistId 68
              ]

      Data.responseRows receivedForeachResponse `rowsShouldBe` expectedForeachResponseRows
      _qrAggregates receivedForeachResponse `jsonShouldBe` Nothing

    it "array relationships can be navigated within each foreach subquery" $ do
      let foreachIds =
            NonEmpty.fromList
              [ mkForeachIds _tdAlbumsTableName [("ArtistId", J.Number 3)],
                mkForeachIds _tdAlbumsTableName [("ArtistId", J.Number 2)]
              ]
      let query =
            albumsQueryRequest
              & qrForeach ?~ foreachIds
              -- Add the Tracks array relationship field
              & qrQuery . qFields . _Just . Data.fieldAt "Tracks" ?~ RelField (RelationshipField _tdTracksRelationshipName tracksQuery)
              & qrRelationships .~ Set.fromList [API.RTable $ Data.onlyKeepRelationships [_tdTracksRelationshipName] _tdAlbumsTableRelationships]
      receivedForeachResponse <- queryGuarded query

      let joinInTracks (album :: HashMap FieldName FieldValue) =
            let albumId = album ^? Data.field "AlbumId" . Data._ColumnFieldNumber
                tracks = _tdTracksRows & filter (\track -> track ^? Data.field "AlbumId" . Data._ColumnFieldNumber == albumId)
                trimmedTracks = Data.filterColumnsByQueryFields tracksQuery <$> tracks
             in Data.insertField "Tracks" (Data.mkSubqueryRowsFieldValue trimmedTracks) album

      let getAlbumsByArtistId artistId =
            _tdAlbumsRows
              & filter (\album -> album ^? Data.field "ArtistId" . Data._ColumnFieldNumber == Just artistId)
              & sortOn (^? Data.field "AlbumId")
              & fmap joinInTracks

      let expectedForeachResponseRows =
            mkForeachResponseRows
              [ getAlbumsByArtistId 3,
                getAlbumsByArtistId 2
              ]

      Data.responseRows receivedForeachResponse `rowsShouldBe` expectedForeachResponseRows
      _qrAggregates receivedForeachResponse `jsonShouldBe` Nothing
  where
    artistsQuery :: Query
    artistsQuery =
      let fields = Data.mkFieldsMap [("ArtistId", _tdColumnField _tdArtistsTableName "ArtistId"), ("Name", _tdColumnField _tdArtistsTableName "Name")]
       in Data.emptyQuery & qFields ?~ fields

    tracksQuery :: Query
    tracksQuery =
      let fields = Data.mkFieldsMap [("TrackId", _tdColumnField _tdTracksTableName "TrackId"), ("Name", _tdColumnField _tdTracksTableName "Name")]
          orderBy = OrderBy mempty $ _tdOrderByColumn [] "TrackId" Ascending :| []
       in Data.emptyQuery & qFields ?~ fields & qOrderBy ?~ orderBy

    albumsQueryRequest :: QueryRequest
    albumsQueryRequest =
      let fields = Data.mkFieldsMap [("AlbumId", _tdColumnField _tdAlbumsTableName "AlbumId"), ("ArtistId", _tdColumnField _tdAlbumsTableName "ArtistId"), ("Title", _tdColumnField _tdAlbumsTableName "Title")]
          orderBy = OrderBy mempty $ _tdOrderByColumn [] "AlbumId" Ascending :| []
          query =
            Data.emptyQuery
              & qFields ?~ fields
              & qOrderBy ?~ orderBy
       in TableQueryRequest _tdAlbumsTableName mempty mempty mempty query Nothing

    playlistTracksQueryRequest :: QueryRequest
    playlistTracksQueryRequest =
      let fields = Data.mkFieldsMap [("PlaylistId", _tdColumnField _tdPlaylistTracksTableName "PlaylistId"), ("TrackId", _tdColumnField _tdPlaylistTracksTableName "TrackId")]
          query = Data.emptyQuery & qFields ?~ fields
       in TableQueryRequest _tdPlaylistTracksTableName mempty mempty mempty query Nothing

    mkForeachIds :: TableName -> [(Text, J.Value)] -> HashMap ColumnName ScalarValue
    mkForeachIds tableName =
      fmap (\(columnName, columnValue) -> (_tdColumnName columnName, ScalarValue columnValue (_tdFindColumnScalarType tableName columnName)))
        >>> HashMap.fromList

    mkForeachResponseRows :: [[HashMap FieldName FieldValue]] -> [HashMap FieldName FieldValue]
    mkForeachResponseRows foreachResults =
      foreachResults
        <&> (\resultRows -> Data.mkFieldsMap [("query", Data.mkSubqueryRowsFieldValue resultRows)])

    mkForeachResponses :: [FieldValue] -> [HashMap FieldName FieldValue]
    mkForeachResponses foreachResults =
      foreachResults
        <&> (\result -> Data.mkFieldsMap [("query", result)])

    albumTitleScalarType = _tdFindColumnScalarType _tdAlbumsTableName "Title"
