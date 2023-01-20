module Test.Specs.QuerySpec.BasicSpec (spec) where

import Control.Arrow ((>>>))
import Control.Lens ((%~), (&), (?~))
import Data.HashMap.Strict qualified as HashMap
import Hasura.Backends.DataConnector.API
import Test.AgentClient (queryGuarded)
import Test.Data (TestData (..))
import Test.Data qualified as Data
import Test.Expectations (jsonShouldBe, rowsShouldBe)
import Test.Sandwich (describe)
import Test.TestHelpers (AgentTestSpec, it)
import Prelude

spec :: TestData -> SourceName -> Config -> AgentTestSpec
spec TestData {..} sourceName config = describe "Basic Queries" $ do
  describe "Column Fields" $ do
    it "can query for a list of artists" $ do
      let query = artistsQueryRequest
      receivedArtists <- Data.sortResponseRowsBy "ArtistId" <$> queryGuarded sourceName config query

      let expectedArtists = _tdArtistsRows
      Data.responseRows receivedArtists `rowsShouldBe` expectedArtists
      _qrAggregates receivedArtists `jsonShouldBe` Nothing

    it "can query for a list of albums with a subset of columns" $ do
      let fields = Data.mkFieldsMap [("ArtistId", _tdColumnField _tdAlbumsTableName "ArtistId"), ("Title", _tdColumnField _tdAlbumsTableName "Title")]
      let query = albumsQueryRequest & qrQuery . qFields ?~ fields
      receivedAlbums <- Data.sortResponseRowsBy "Title" <$> queryGuarded sourceName config query

      let filterToRequiredProperties =
            HashMap.filterWithKey (\(FieldName propName) _value -> propName == "ArtistId" || propName == "Title")

      let expectedAlbums = Data.sortBy (FieldName "Title") $ filterToRequiredProperties <$> _tdAlbumsRows
      Data.responseRows receivedAlbums `rowsShouldBe` expectedAlbums
      _qrAggregates receivedAlbums `jsonShouldBe` Nothing

    it "can project columns into fields with different names" $ do
      let fields = Data.mkFieldsMap [("Artist_Id", _tdColumnField _tdArtistsTableName "ArtistId"), ("Artist_Name", _tdColumnField _tdArtistsTableName "Name")]
      let query = artistsQueryRequest & qrQuery . qFields ?~ fields
      receivedArtists <- Data.sortResponseRowsBy "ArtistId" <$> queryGuarded sourceName config query

      let renameProperties =
            HashMap.mapKeys
              ( \case
                  (FieldName "ArtistId") -> (FieldName "Artist_Id")
                  (FieldName "Name") -> (FieldName "Artist_Name")
                  other -> other
              )

      let expectedArtists = Data.sortBy (FieldName "ArtistId") $ renameProperties <$> _tdArtistsRows
      Data.responseRows receivedArtists `rowsShouldBe` expectedArtists
      _qrAggregates receivedArtists `jsonShouldBe` Nothing

  describe "Limit & Offset" $ do
    it "can use limit and offset to paginate results" $ do
      let allQuery = artistsQueryRequest
      let page1Query = artistsQueryRequest & qrQuery %~ (qLimit ?~ 10 >>> qOffset ?~ 0)
      let page2Query = artistsQueryRequest & qrQuery %~ (qLimit ?~ 10 >>> qOffset ?~ 10)

      allArtists <- Data.responseRows <$> queryGuarded sourceName config allQuery
      page1Artists <- Data.responseRows <$> queryGuarded sourceName config page1Query
      page2Artists <- Data.responseRows <$> queryGuarded sourceName config page2Query

      page1Artists `rowsShouldBe` take 10 allArtists
      page2Artists `rowsShouldBe` take 10 (drop 10 allArtists)
  where
    artistsQueryRequest :: QueryRequest
    artistsQueryRequest =
      let fields = Data.mkFieldsMap [("ArtistId", _tdColumnField _tdArtistsTableName "ArtistId"), ("Name", _tdColumnField _tdArtistsTableName "Name")]
          query = Data.emptyQuery & qFields ?~ fields
       in QueryRequest _tdArtistsTableName [] query

    albumsQueryRequest :: QueryRequest
    albumsQueryRequest =
      let fields = Data.mkFieldsMap [("AlbumId", _tdColumnField _tdAlbumsTableName "AlbumId"), ("ArtistId", _tdColumnField _tdAlbumsTableName "ArtistId"), ("Title", _tdColumnField _tdAlbumsTableName "Title")]
          query = Data.emptyQuery & qFields ?~ fields
       in QueryRequest _tdAlbumsTableName [] query
