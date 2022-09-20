module Test.QuerySpec.BasicSpec (spec) where

import Control.Arrow ((>>>))
import Control.Lens ((%~), (&), (?~))
import Data.Aeson.KeyMap qualified as KeyMap
import Hasura.Backends.DataConnector.API
import Servant.API (NamedRoutes)
import Servant.Client (Client, (//))
import Test.Data qualified as Data
import Test.Expectations (jsonShouldBe, rowsShouldBe)
import Test.Hspec (Spec, describe, it)
import Prelude

spec :: Client IO (NamedRoutes Routes) -> SourceName -> Config -> Spec
spec api sourceName config = describe "Basic Queries" $ do
  describe "Column Fields" $ do
    it "can query for a list of artists" $ do
      let query = artistsQueryRequest
      receivedArtists <- Data.sortResponseRowsBy "ArtistId" <$> (api // _query) sourceName config query

      let expectedArtists = Data.artistsRows
      Data.responseRows receivedArtists `rowsShouldBe` expectedArtists
      _qrAggregates receivedArtists `jsonShouldBe` Nothing

    it "can query for a list of albums with a subset of columns" $ do
      let fields = KeyMap.fromList [("ArtistId", Data.columnField "ArtistId"), ("Title", Data.columnField "Title")]
      let query = albumsQueryRequest & qrQuery . qFields ?~ fields
      receivedAlbums <- Data.sortResponseRowsBy "Title" <$> (api // _query) sourceName config query

      let filterToRequiredProperties =
            KeyMap.filterWithKey (\propName _value -> propName == "ArtistId" || propName == "Title")

      let expectedAlbums = Data.sortBy "Title" $ filterToRequiredProperties <$> Data.albumsRows
      Data.responseRows receivedAlbums `rowsShouldBe` expectedAlbums
      _qrAggregates receivedAlbums `jsonShouldBe` Nothing

    it "can project columns into fields with different names" $ do
      let fields = KeyMap.fromList [("Artist_Id", Data.columnField "ArtistId"), ("Artist_Name", Data.columnField "Name")]
      let query = artistsQueryRequest & qrQuery . qFields ?~ fields
      receivedArtists <- Data.sortResponseRowsBy "ArtistId" <$> (api // _query) sourceName config query

      let renameProperties =
            KeyMap.mapKeyVal
              ( \case
                  "ArtistId" -> "Artist_Id"
                  "Name" -> "Artist_Name"
                  other -> other
              )
              id

      let expectedArtists = Data.sortBy "ArtistId" $ renameProperties <$> Data.artistsRows
      Data.responseRows receivedArtists `rowsShouldBe` expectedArtists
      _qrAggregates receivedArtists `jsonShouldBe` Nothing

  describe "Limit & Offset" $ do
    it "can use limit and offset to paginate results" $ do
      let allQuery = artistsQueryRequest
      let page1Query = artistsQueryRequest & qrQuery %~ (qLimit ?~ 10 >>> qOffset ?~ 0)
      let page2Query = artistsQueryRequest & qrQuery %~ (qLimit ?~ 10 >>> qOffset ?~ 10)

      allArtists <- Data.responseRows <$> (api // _query) sourceName config allQuery
      page1Artists <- Data.responseRows <$> (api // _query) sourceName config page1Query
      page2Artists <- Data.responseRows <$> (api // _query) sourceName config page2Query

      page1Artists `rowsShouldBe` take 10 allArtists
      page2Artists `rowsShouldBe` take 10 (drop 10 allArtists)

artistsQueryRequest :: QueryRequest
artistsQueryRequest =
  let fields = KeyMap.fromList [("ArtistId", Data.columnField "ArtistId"), ("Name", Data.columnField "Name")]
      query = Data.emptyQuery & qFields ?~ fields
   in QueryRequest Data.artistsTableName [] query

albumsQueryRequest :: QueryRequest
albumsQueryRequest =
  let fields = KeyMap.fromList [("AlbumId", Data.columnField "AlbumId"), ("ArtistId", Data.columnField "ArtistId"), ("Title", Data.columnField "Title")]
      query = Data.emptyQuery & qFields ?~ fields
   in QueryRequest Data.albumsTableName [] query
