module Test.QuerySpec.BasicSpec (spec) where

import Autodocodec.Extended (ValueWrapper (..), ValueWrapper3 (ValueWrapper3))
import Control.Arrow ((>>>))
import Control.Lens (ix, (%~), (&), (.~), (^?))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Ord (Down (..))
import Data.Text (Text)
import Hasura.Backends.DataConnector.API
import Servant.API (NamedRoutes)
import Servant.Client (Client, (//))
import Test.Data qualified as Data
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Prelude

spec :: Client IO (NamedRoutes Routes) -> SourceName -> Config -> Spec
spec api sourceName config = describe "Basic Queries" $ do
  describe "Column Fields" $ do
    it "can query for a list of artists" $ do
      let query = artistsQueryRequest
      receivedArtists <- Data.sortResponseRowsBy "ArtistId" <$> (api // _query) sourceName config query

      let expectedArtists = Data.artistsAsJson
      Data.responseRows receivedArtists `shouldBe` expectedArtists
      _qrAggregates receivedArtists `shouldBe` Nothing

    it "can query for a list of albums with a subset of columns" $ do
      let fields = KeyMap.fromList [("ArtistId", columnField "ArtistId"), ("Title", columnField "Title")]
      let query = albumsQueryRequest & qrQuery . qFields .~ Just fields
      receivedAlbums <- Data.sortResponseRowsBy "Title" <$> (api // _query) sourceName config query

      let filterToRequiredProperties =
            KeyMap.filterWithKey (\propName _value -> propName == "ArtistId" || propName == "Title")

      let expectedAlbums = Data.sortBy "Title" $ filterToRequiredProperties <$> Data.albumsAsJson
      Data.responseRows receivedAlbums `shouldBe` expectedAlbums
      _qrAggregates receivedAlbums `shouldBe` Nothing

    it "can project columns into fields with different names" $ do
      let fields = KeyMap.fromList [("Artist_Id", columnField "ArtistId"), ("Artist_Name", columnField "Name")]
      let query = artistsQueryRequest & qrQuery . qFields .~ Just fields
      receivedArtists <- Data.sortResponseRowsBy "ArtistId" <$> (api // _query) sourceName config query

      let renameProperties =
            KeyMap.mapKeyVal
              ( \case
                  "ArtistId" -> "Artist_Id"
                  "Name" -> "Artist_Name"
                  other -> other
              )
              id

      let expectedArtists = Data.sortBy "ArtistId" $ renameProperties <$> Data.artistsAsJson
      Data.responseRows receivedArtists `shouldBe` expectedArtists
      _qrAggregates receivedArtists `shouldBe` Nothing

  describe "Limit & Offset" $ do
    it "can use limit and offset to paginate results" $ do
      let allQuery = artistsQueryRequest
      let page1Query = artistsQueryRequest & qrQuery %~ (qLimit .~ Just 10 >>> qOffset .~ Just 0)
      let page2Query = artistsQueryRequest & qrQuery %~ (qLimit .~ Just 10 >>> qOffset .~ Just 10)

      allArtists <- Data.responseRows <$> (api // _query) sourceName config allQuery
      page1Artists <- Data.responseRows <$> (api // _query) sourceName config page1Query
      page2Artists <- Data.responseRows <$> (api // _query) sourceName config page2Query

      page1Artists `shouldBe` take 10 allArtists
      page2Artists `shouldBe` take 10 (drop 10 allArtists)

  describe "Order By" $ do
    it "can use order by to order results in ascending order" $ do
      let orderBy = OrderBy (ColumnName "Title") Ascending :| []
      let query = albumsQueryRequest & qrQuery . qOrderBy .~ Just orderBy
      receivedAlbums <- (api // _query) sourceName config query

      let expectedAlbums = sortOn (^? ix "Title") Data.albumsAsJson
      Data.responseRows receivedAlbums `shouldBe` expectedAlbums
      _qrAggregates receivedAlbums `shouldBe` Nothing

    it "can use order by to order results in descending order" $ do
      let orderBy = OrderBy (ColumnName "Title") Descending :| []
      let query = albumsQueryRequest & qrQuery . qOrderBy .~ Just orderBy
      receivedAlbums <- (api // _query) sourceName config query

      let expectedAlbums = sortOn (Down . (^? ix "Title")) Data.albumsAsJson
      Data.responseRows receivedAlbums `shouldBe` expectedAlbums
      _qrAggregates receivedAlbums `shouldBe` Nothing

    it "can use multiple order bys to order results" $ do
      let orderBy = OrderBy (ColumnName "ArtistId") Ascending :| [OrderBy (ColumnName "Title") Descending]
      let query = albumsQueryRequest & qrQuery . qOrderBy .~ Just orderBy
      receivedAlbums <- (api // _query) sourceName config query

      let expectedAlbums =
            sortOn (\album -> (album ^? ix "ArtistId", Down (album ^? ix "Title"))) Data.albumsAsJson

      Data.responseRows receivedAlbums `shouldBe` expectedAlbums
      _qrAggregates receivedAlbums `shouldBe` Nothing

  describe "Where" $ do
    it "can filter using an equality expression" $ do
      let where' = ApplyBinaryComparisonOperator (ValueWrapper3 Equal (localComparisonColumn "AlbumId") (ScalarValue (ValueWrapper (Number 2))))
      let query = albumsQueryRequest & qrQuery . qWhere .~ Just where'
      receivedAlbums <- Data.sortResponseRowsBy "AlbumId" <$> (api // _query) sourceName config query

      let expectedAlbums =
            filter ((== Just 2) . (^? ix "AlbumId" . Data._ColumnFieldNumber)) Data.albumsAsJson

      Data.responseRows receivedAlbums `shouldBe` expectedAlbums
      _qrAggregates receivedAlbums `shouldBe` Nothing

    it "can filter using an inequality expression" $ do
      let where' = Not (ValueWrapper (ApplyBinaryComparisonOperator (ValueWrapper3 Equal (localComparisonColumn "AlbumId") (ScalarValue (ValueWrapper (Number 2))))))
      let query = albumsQueryRequest & qrQuery . qWhere .~ Just where'
      receivedAlbums <- Data.sortResponseRowsBy "AlbumId" <$> (api // _query) sourceName config query

      let expectedAlbums =
            filter ((/= Just 2) . (^? ix "AlbumId" . Data._ColumnFieldNumber)) Data.albumsAsJson

      Data.responseRows receivedAlbums `shouldBe` expectedAlbums
      _qrAggregates receivedAlbums `shouldBe` Nothing

    it "can filter using an in expression" $ do
      let where' = ApplyBinaryArrayComparisonOperator (ValueWrapper3 In (localComparisonColumn "AlbumId") [Number 2, Number 3])
      let query = albumsQueryRequest & qrQuery . qWhere .~ Just where'
      receivedAlbums <- Data.sortResponseRowsBy "AlbumId" <$> (api // _query) sourceName config query

      let expectedAlbums =
            filter (flip elem [Just 2, Just 3] . (^? ix "AlbumId" . Data._ColumnFieldNumber)) Data.albumsAsJson

      Data.responseRows receivedAlbums `shouldBe` expectedAlbums
      _qrAggregates receivedAlbums `shouldBe` Nothing

    it "can negate an in expression filter using a not expression" $ do
      let where' = Not (ValueWrapper (ApplyBinaryArrayComparisonOperator (ValueWrapper3 In (localComparisonColumn "AlbumId") [Number 2, Number 3])))
      let query = albumsQueryRequest & qrQuery . qWhere .~ Just where'
      receivedAlbums <- Data.sortResponseRowsBy "AlbumId" <$> (api // _query) sourceName config query

      let expectedAlbums =
            filter (flip notElem [Just 2, Just 3] . (^? ix "AlbumId" . Data._ColumnFieldNumber)) Data.albumsAsJson

      Data.responseRows receivedAlbums `shouldBe` expectedAlbums
      _qrAggregates receivedAlbums `shouldBe` Nothing

    it "can combine filters using an and expression" $ do
      let where1 = ApplyBinaryComparisonOperator (ValueWrapper3 Equal (localComparisonColumn "ArtistId") (ScalarValue (ValueWrapper (Number 58))))
      let where2 = ApplyBinaryComparisonOperator (ValueWrapper3 Equal (localComparisonColumn "Title") (ScalarValue (ValueWrapper (String "Stormbringer"))))
      let where' = And (ValueWrapper [where1, where2])
      let query = albumsQueryRequest & qrQuery . qWhere .~ Just where'
      receivedAlbums <- Data.sortResponseRowsBy "AlbumId" <$> (api // _query) sourceName config query

      let expectedAlbums =
            filter
              ( \album ->
                  (album ^? ix "ArtistId" . Data._ColumnFieldNumber == Just 58) && (album ^? ix "Title" . Data._ColumnFieldString == Just "Stormbringer")
              )
              Data.albumsAsJson

      Data.responseRows receivedAlbums `shouldBe` expectedAlbums
      _qrAggregates receivedAlbums `shouldBe` Nothing

    it "can combine filters using an or expression" $ do
      let where1 = ApplyBinaryComparisonOperator (ValueWrapper3 Equal (localComparisonColumn "AlbumId") (ScalarValue (ValueWrapper (Number 2))))
      let where2 = ApplyBinaryComparisonOperator (ValueWrapper3 Equal (localComparisonColumn "AlbumId") (ScalarValue (ValueWrapper (Number 3))))
      let where' = Or (ValueWrapper [where1, where2])
      let query = albumsQueryRequest & qrQuery . qWhere .~ Just where'
      receivedAlbums <- Data.sortResponseRowsBy "AlbumId" <$> (api // _query) sourceName config query

      let expectedAlbums =
            filter (flip elem [Just 2, Just 3] . (^? ix "AlbumId" . Data._ColumnFieldNumber)) Data.albumsAsJson

      Data.responseRows receivedAlbums `shouldBe` expectedAlbums
      _qrAggregates receivedAlbums `shouldBe` Nothing

    it "can filter by applying the greater than operator" $ do
      let where' = ApplyBinaryComparisonOperator (ValueWrapper3 GreaterThan (localComparisonColumn "AlbumId") (ScalarValue (ValueWrapper (Number 300))))
      let query = albumsQueryRequest & qrQuery . qWhere .~ Just where'
      receivedAlbums <- Data.sortResponseRowsBy "AlbumId" <$> (api // _query) sourceName config query

      let expectedAlbums =
            filter ((> Just 300) . (^? ix "AlbumId" . Data._ColumnFieldNumber)) Data.albumsAsJson

      Data.responseRows receivedAlbums `shouldBe` expectedAlbums
      _qrAggregates receivedAlbums `shouldBe` Nothing

    it "can filter by applying the greater than or equal operator" $ do
      let where' = ApplyBinaryComparisonOperator (ValueWrapper3 GreaterThanOrEqual (localComparisonColumn "AlbumId") (ScalarValue (ValueWrapper (Number 300))))
      let query = albumsQueryRequest & qrQuery . qWhere .~ Just where'
      receivedAlbums <- Data.sortResponseRowsBy "AlbumId" <$> (api // _query) sourceName config query

      let expectedAlbums =
            filter ((>= Just 300) . (^? ix "AlbumId" . Data._ColumnFieldNumber)) Data.albumsAsJson

      Data.responseRows receivedAlbums `shouldBe` expectedAlbums
      _qrAggregates receivedAlbums `shouldBe` Nothing

    it "can filter by applying the less than operator" $ do
      let where' = ApplyBinaryComparisonOperator (ValueWrapper3 LessThan (localComparisonColumn "AlbumId") (ScalarValue (ValueWrapper (Number 100))))
      let query = albumsQueryRequest & qrQuery . qWhere .~ Just where'
      receivedAlbums <- Data.sortResponseRowsBy "AlbumId" <$> (api // _query) sourceName config query

      let expectedAlbums =
            filter ((< Just 100) . (^? ix "AlbumId" . Data._ColumnFieldNumber)) Data.albumsAsJson

      Data.responseRows receivedAlbums `shouldBe` expectedAlbums
      _qrAggregates receivedAlbums `shouldBe` Nothing

    it "can filter by applying the less than or equal operator" $ do
      let where' = ApplyBinaryComparisonOperator (ValueWrapper3 LessThanOrEqual (localComparisonColumn "AlbumId") (ScalarValue (ValueWrapper (Number 100))))
      let query = albumsQueryRequest & qrQuery . qWhere .~ Just where'
      receivedAlbums <- Data.sortResponseRowsBy "AlbumId" <$> (api // _query) sourceName config query

      let expectedAlbums =
            filter ((<= Just 100) . (^? ix "AlbumId" . Data._ColumnFieldNumber)) Data.albumsAsJson

      Data.responseRows receivedAlbums `shouldBe` expectedAlbums
      _qrAggregates receivedAlbums `shouldBe` Nothing

    it "can filter using a greater than operator with a column comparison" $ do
      let where' = ApplyBinaryComparisonOperator (ValueWrapper3 GreaterThan (localComparisonColumn "AlbumId") (AnotherColumn (ValueWrapper (localComparisonColumn "ArtistId"))))
      let query = albumsQueryRequest & qrQuery . qWhere .~ Just where'
      receivedAlbums <- Data.sortResponseRowsBy "AlbumId" <$> (api // _query) sourceName config query

      let expectedAlbums =
            filter (\album -> (album ^? ix "AlbumId" . Data._ColumnFieldNumber) > (album ^? ix "ArtistId" . Data._ColumnFieldNumber)) Data.albumsAsJson

      Data.responseRows receivedAlbums `shouldBe` expectedAlbums
      _qrAggregates receivedAlbums `shouldBe` Nothing

artistsQueryRequest :: QueryRequest
artistsQueryRequest =
  let fields = KeyMap.fromList [("ArtistId", columnField "ArtistId"), ("Name", columnField "Name")]
      tableName = TableName "Artist"
      query = Query (Just fields) Nothing Nothing Nothing Nothing Nothing
   in QueryRequest tableName [] query

albumsQueryRequest :: QueryRequest
albumsQueryRequest =
  let fields = KeyMap.fromList [("AlbumId", columnField "AlbumId"), ("ArtistId", columnField "ArtistId"), ("Title", columnField "Title")]
      tableName = TableName "Album"
      query = Query (Just fields) Nothing Nothing Nothing Nothing Nothing
   in QueryRequest tableName [] query

columnField :: Text -> Field
columnField = ColumnField . ValueWrapper . ColumnName

localComparisonColumn :: Text -> ComparisonColumn
localComparisonColumn columnName = ComparisonColumn [] $ ColumnName columnName
