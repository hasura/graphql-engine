module Test.QuerySpec.BasicSpec (spec) where

import Control.Arrow ((>>>))
import Control.Lens (ix, (%~), (&), (?~), (^?))
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

  describe "Where" $ do
    it "can filter using an equality expression" $ do
      let where' = ApplyBinaryComparisonOperator Equal (Data.localComparisonColumn "AlbumId") (ScalarValue (Number 2))
      let query = albumsQueryRequest & qrQuery . qWhere ?~ where'
      receivedAlbums <- Data.sortResponseRowsBy "AlbumId" <$> (api // _query) sourceName config query

      let expectedAlbums =
            filter ((== Just 2) . (^? ix "AlbumId" . Data._ColumnFieldNumber)) Data.albumsRows

      Data.responseRows receivedAlbums `rowsShouldBe` expectedAlbums
      _qrAggregates receivedAlbums `jsonShouldBe` Nothing

    it "can filter using an inequality expression" $ do
      let where' = Not (ApplyBinaryComparisonOperator Equal (Data.localComparisonColumn "AlbumId") (ScalarValue (Number 2)))
      let query = albumsQueryRequest & qrQuery . qWhere ?~ where'
      receivedAlbums <- Data.sortResponseRowsBy "AlbumId" <$> (api // _query) sourceName config query

      let expectedAlbums =
            filter ((/= Just 2) . (^? ix "AlbumId" . Data._ColumnFieldNumber)) Data.albumsRows

      Data.responseRows receivedAlbums `rowsShouldBe` expectedAlbums
      _qrAggregates receivedAlbums `jsonShouldBe` Nothing

    it "can filter using an in expression" $ do
      let where' = ApplyBinaryArrayComparisonOperator In (Data.localComparisonColumn "AlbumId") [Number 2, Number 3]
      let query = albumsQueryRequest & qrQuery . qWhere ?~ where'
      receivedAlbums <- Data.sortResponseRowsBy "AlbumId" <$> (api // _query) sourceName config query

      let expectedAlbums =
            filter (flip elem [Just 2, Just 3] . (^? ix "AlbumId" . Data._ColumnFieldNumber)) Data.albumsRows

      Data.responseRows receivedAlbums `rowsShouldBe` expectedAlbums
      _qrAggregates receivedAlbums `jsonShouldBe` Nothing

    it "can negate an in expression filter using a not expression" $ do
      let where' = Not (ApplyBinaryArrayComparisonOperator In (Data.localComparisonColumn "AlbumId") [Number 2, Number 3])
      let query = albumsQueryRequest & qrQuery . qWhere ?~ where'
      receivedAlbums <- Data.sortResponseRowsBy "AlbumId" <$> (api // _query) sourceName config query

      let expectedAlbums =
            filter (flip notElem [Just 2, Just 3] . (^? ix "AlbumId" . Data._ColumnFieldNumber)) Data.albumsRows

      Data.responseRows receivedAlbums `rowsShouldBe` expectedAlbums
      _qrAggregates receivedAlbums `jsonShouldBe` Nothing

    it "can combine filters using an and expression" $ do
      let where1 = ApplyBinaryComparisonOperator Equal (Data.localComparisonColumn "ArtistId") (ScalarValue (Number 58))
      let where2 = ApplyBinaryComparisonOperator Equal (Data.localComparisonColumn "Title") (ScalarValue (String "Stormbringer"))
      let where' = And [where1, where2]
      let query = albumsQueryRequest & qrQuery . qWhere ?~ where'
      receivedAlbums <- Data.sortResponseRowsBy "AlbumId" <$> (api // _query) sourceName config query

      let expectedAlbums =
            filter
              ( \album ->
                  (album ^? ix "ArtistId" . Data._ColumnFieldNumber == Just 58) && (album ^? ix "Title" . Data._ColumnFieldString == Just "Stormbringer")
              )
              Data.albumsRows

      Data.responseRows receivedAlbums `rowsShouldBe` expectedAlbums
      _qrAggregates receivedAlbums `jsonShouldBe` Nothing

    it "treats an empty and expression as 'true'" $ do
      let where' = And []
      let query = albumsQueryRequest & qrQuery . qWhere ?~ where'
      receivedAlbums <- Data.sortResponseRowsBy "AlbumId" <$> (api // _query) sourceName config query

      Data.responseRows receivedAlbums `rowsShouldBe` Data.albumsRows
      _qrAggregates receivedAlbums `jsonShouldBe` Nothing

    it "can combine filters using an or expression" $ do
      let where1 = ApplyBinaryComparisonOperator Equal (Data.localComparisonColumn "AlbumId") (ScalarValue (Number 2))
      let where2 = ApplyBinaryComparisonOperator Equal (Data.localComparisonColumn "AlbumId") (ScalarValue (Number 3))
      let where' = Or [where1, where2]
      let query = albumsQueryRequest & qrQuery . qWhere ?~ where'
      receivedAlbums <- Data.sortResponseRowsBy "AlbumId" <$> (api // _query) sourceName config query

      let expectedAlbums =
            filter (flip elem [Just 2, Just 3] . (^? ix "AlbumId" . Data._ColumnFieldNumber)) Data.albumsRows

      Data.responseRows receivedAlbums `rowsShouldBe` expectedAlbums
      _qrAggregates receivedAlbums `jsonShouldBe` Nothing

    it "treats an empty or expression as 'false'" $ do
      let where' = Or []
      let query = albumsQueryRequest & qrQuery . qWhere ?~ where'
      receivedAlbums <- (api // _query) sourceName config query

      Data.responseRows receivedAlbums `rowsShouldBe` []
      _qrAggregates receivedAlbums `jsonShouldBe` Nothing

    it "can filter by applying the greater than operator" $ do
      let where' = ApplyBinaryComparisonOperator GreaterThan (Data.localComparisonColumn "AlbumId") (ScalarValue (Number 300))
      let query = albumsQueryRequest & qrQuery . qWhere ?~ where'
      receivedAlbums <- Data.sortResponseRowsBy "AlbumId" <$> (api // _query) sourceName config query

      let expectedAlbums =
            filter ((> Just 300) . (^? ix "AlbumId" . Data._ColumnFieldNumber)) Data.albumsRows

      Data.responseRows receivedAlbums `rowsShouldBe` expectedAlbums
      _qrAggregates receivedAlbums `jsonShouldBe` Nothing

    it "can filter by applying the greater than or equal operator" $ do
      let where' = ApplyBinaryComparisonOperator GreaterThanOrEqual (Data.localComparisonColumn "AlbumId") (ScalarValue (Number 300))
      let query = albumsQueryRequest & qrQuery . qWhere ?~ where'
      receivedAlbums <- Data.sortResponseRowsBy "AlbumId" <$> (api // _query) sourceName config query

      let expectedAlbums =
            filter ((>= Just 300) . (^? ix "AlbumId" . Data._ColumnFieldNumber)) Data.albumsRows

      Data.responseRows receivedAlbums `rowsShouldBe` expectedAlbums
      _qrAggregates receivedAlbums `jsonShouldBe` Nothing

    it "can filter by applying the less than operator" $ do
      let where' = ApplyBinaryComparisonOperator LessThan (Data.localComparisonColumn "AlbumId") (ScalarValue (Number 100))
      let query = albumsQueryRequest & qrQuery . qWhere ?~ where'
      receivedAlbums <- Data.sortResponseRowsBy "AlbumId" <$> (api // _query) sourceName config query

      let expectedAlbums =
            filter ((< Just 100) . (^? ix "AlbumId" . Data._ColumnFieldNumber)) Data.albumsRows

      Data.responseRows receivedAlbums `rowsShouldBe` expectedAlbums
      _qrAggregates receivedAlbums `jsonShouldBe` Nothing

    it "can filter by applying the less than or equal operator" $ do
      let where' = ApplyBinaryComparisonOperator LessThanOrEqual (Data.localComparisonColumn "AlbumId") (ScalarValue (Number 100))
      let query = albumsQueryRequest & qrQuery . qWhere ?~ where'
      receivedAlbums <- Data.sortResponseRowsBy "AlbumId" <$> (api // _query) sourceName config query

      let expectedAlbums =
            filter ((<= Just 100) . (^? ix "AlbumId" . Data._ColumnFieldNumber)) Data.albumsRows

      Data.responseRows receivedAlbums `rowsShouldBe` expectedAlbums
      _qrAggregates receivedAlbums `jsonShouldBe` Nothing

    it "can filter using a greater than operator with a column comparison" $ do
      let where' = ApplyBinaryComparisonOperator GreaterThan (Data.localComparisonColumn "AlbumId") (AnotherColumn (Data.localComparisonColumn "ArtistId"))
      let query = albumsQueryRequest & qrQuery . qWhere ?~ where'
      receivedAlbums <- Data.sortResponseRowsBy "AlbumId" <$> (api // _query) sourceName config query

      let expectedAlbums =
            filter (\album -> (album ^? ix "AlbumId" . Data._ColumnFieldNumber) > (album ^? ix "ArtistId" . Data._ColumnFieldNumber)) Data.albumsRows

      Data.responseRows receivedAlbums `rowsShouldBe` expectedAlbums
      _qrAggregates receivedAlbums `jsonShouldBe` Nothing

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
