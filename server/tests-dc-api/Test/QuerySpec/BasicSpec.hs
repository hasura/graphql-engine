module Test.QuerySpec.BasicSpec (spec) where

import Autodocodec.Extended (ValueWrapper (..), ValueWrapper3 (ValueWrapper3))
import Control.Lens (ix, (^?))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Lens (AsNumber (_Number), AsPrimitive (_String))
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
      let query = artistsQuery
      receivedArtists <- fmap (Data.sortBy "ArtistId" . getQueryResponse) $ (api // _query) sourceName config query

      let expectedArtists = Data.artistsAsJson
      receivedArtists `shouldBe` expectedArtists

    it "can query for a list of albums with a subset of columns" $ do
      let fields = KeyMap.fromList [("ArtistId", columnField "ArtistId"), ("Title", columnField "Title")]
      let query = albumsQuery {fields}
      receivedAlbums <- (Data.sortBy "Title" . getQueryResponse) <$> (api // _query) sourceName config query

      let filterToRequiredProperties =
            KeyMap.filterWithKey (\propName _value -> propName == "ArtistId" || propName == "Title")

      let expectedAlbums = Data.sortBy "Title" $ filterToRequiredProperties <$> Data.albumsAsJson
      receivedAlbums `shouldBe` expectedAlbums

    it "can project columns into fields with different names" $ do
      let fields = KeyMap.fromList [("Artist_Id", columnField "ArtistId"), ("Artist_Name", columnField "Name")]
      let query = artistsQuery {fields}
      receivedArtists <- (Data.sortBy "ArtistId" . getQueryResponse) <$> (api // _query) sourceName config query

      let renameProperties =
            KeyMap.mapKeyVal
              ( \case
                  "ArtistId" -> "Artist_Id"
                  "Name" -> "Artist_Name"
                  other -> other
              )
              id

      let expectedArtists = Data.sortBy "ArtistId" $ renameProperties <$> Data.artistsAsJson
      receivedArtists `shouldBe` expectedArtists

  describe "Limit & Offset" $ do
    it "can use limit and offset to paginate results" $ do
      let allQuery = artistsQuery
      let page1Query = artistsQuery {limit = Just 10, offset = Just 0}
      let page2Query = artistsQuery {limit = Just 10, offset = Just 10}

      allArtists <- getQueryResponse <$> (api // _query) sourceName config allQuery
      page1Artists <- getQueryResponse <$> (api // _query) sourceName config page1Query
      page2Artists <- getQueryResponse <$> (api // _query) sourceName config page2Query

      page1Artists `shouldBe` take 10 allArtists
      page2Artists `shouldBe` take 10 (drop 10 allArtists)

  describe "Order By" $ do
    it "can use order by to order results in ascending order" $ do
      let orderBy = OrderBy (ColumnName "Title") Ascending :| []
      let query = albumsQuery {orderBy = Just orderBy}
      receivedAlbums <- getQueryResponse <$> (api // _query) sourceName config query

      let expectedAlbums = sortOn (^? ix "Title") Data.albumsAsJson
      receivedAlbums `shouldBe` expectedAlbums

    it "can use order by to order results in descending order" $ do
      let orderBy = OrderBy (ColumnName "Title") Descending :| []
      let query = albumsQuery {orderBy = Just orderBy}
      receivedAlbums <- getQueryResponse <$> (api // _query) sourceName config query

      let expectedAlbums = sortOn (Down . (^? ix "Title")) Data.albumsAsJson
      receivedAlbums `shouldBe` expectedAlbums

    it "can use multiple order bys to order results" $ do
      let orderBy = OrderBy (ColumnName "ArtistId") Ascending :| [OrderBy (ColumnName "Title") Descending]
      let query = albumsQuery {orderBy = Just orderBy}
      receivedAlbums <- getQueryResponse <$> (api // _query) sourceName config query

      let expectedAlbums =
            sortOn (\album -> (album ^? ix "ArtistId", Down (album ^? ix "Title"))) Data.albumsAsJson

      receivedAlbums `shouldBe` expectedAlbums

  describe "Where" $ do
    it "can filter using an equality expression" $ do
      let where' = ApplyBinaryComparisonOperator (ValueWrapper3 Equal (ColumnName "AlbumId") (ScalarValue (ValueWrapper (Number 2))))
      let query = albumsQuery {where_ = Just where'}
      receivedAlbums <- (Data.sortBy "AlbumId" . getQueryResponse) <$> (api // _query) sourceName config query

      let expectedAlbums =
            filter ((== Just 2) . (^? ix "AlbumId" . _Number)) Data.albumsAsJson

      receivedAlbums `shouldBe` expectedAlbums

    it "can filter using an inequality expression" $ do
      let where' = Not (ValueWrapper (ApplyBinaryComparisonOperator (ValueWrapper3 Equal (ColumnName "AlbumId") (ScalarValue (ValueWrapper (Number 2))))))
      let query = albumsQuery {where_ = Just where'}
      receivedAlbums <- (Data.sortBy "AlbumId" . getQueryResponse) <$> (api // _query) sourceName config query

      let expectedAlbums =
            filter ((/= Just 2) . (^? ix "AlbumId" . _Number)) Data.albumsAsJson

      receivedAlbums `shouldBe` expectedAlbums

    it "can filter using an in expression" $ do
      let where' = ApplyBinaryArrayComparisonOperator (ValueWrapper3 In (ColumnName "AlbumId") (ScalarValue . ValueWrapper <$> [Number 2, Number 3]))
      let query = albumsQuery {where_ = Just where'}
      receivedAlbums <- (Data.sortBy "AlbumId" . getQueryResponse) <$> (api // _query) sourceName config query

      let expectedAlbums =
            filter (flip elem [Just 2, Just 3] . (^? ix "AlbumId" . _Number)) Data.albumsAsJson

      receivedAlbums `shouldBe` expectedAlbums

    it "can negate an in expression filter using a not expression" $ do
      let where' = Not (ValueWrapper (ApplyBinaryArrayComparisonOperator (ValueWrapper3 In (ColumnName "AlbumId") (ScalarValue . ValueWrapper <$> [Number 2, Number 3]))))
      let query = albumsQuery {where_ = Just where'}
      receivedAlbums <- (Data.sortBy "AlbumId" . getQueryResponse) <$> (api // _query) sourceName config query

      let expectedAlbums =
            filter (flip notElem [Just 2, Just 3] . (^? ix "AlbumId" . _Number)) Data.albumsAsJson

      receivedAlbums `shouldBe` expectedAlbums

    it "can combine filters using an and expression" $ do
      let where1 = ApplyBinaryComparisonOperator (ValueWrapper3 Equal (ColumnName "ArtistId") (ScalarValue (ValueWrapper (Number 58))))
      let where2 = ApplyBinaryComparisonOperator (ValueWrapper3 Equal (ColumnName "Title") (ScalarValue (ValueWrapper (String "Stormbringer"))))
      let where' = And (ValueWrapper [where1, where2])
      let query = albumsQuery {where_ = Just where'}
      receivedAlbums <- (Data.sortBy "AlbumId" . getQueryResponse) <$> (api // _query) sourceName config query

      let expectedAlbums =
            filter
              ( \album ->
                  (album ^? ix "ArtistId" . _Number == Just 58) && (album ^? ix "Title" . _String == Just "Stormbringer")
              )
              Data.albumsAsJson

      receivedAlbums `shouldBe` expectedAlbums

    it "can combine filters using an or expression" $ do
      let where1 = ApplyBinaryComparisonOperator (ValueWrapper3 Equal (ColumnName "AlbumId") (ScalarValue (ValueWrapper (Number 2))))
      let where2 = ApplyBinaryComparisonOperator (ValueWrapper3 Equal (ColumnName "AlbumId") (ScalarValue (ValueWrapper (Number 3))))
      let where' = Or (ValueWrapper [where1, where2])
      let query = albumsQuery {where_ = Just where'}
      receivedAlbums <- (Data.sortBy "AlbumId" . getQueryResponse) <$> (api // _query) sourceName config query

      let expectedAlbums =
            filter (flip elem [Just 2, Just 3] . (^? ix "AlbumId" . _Number)) Data.albumsAsJson

      receivedAlbums `shouldBe` expectedAlbums

    it "can filter by applying the greater than operator" $ do
      let where' = ApplyBinaryComparisonOperator (ValueWrapper3 GreaterThan (ColumnName "AlbumId") (ScalarValue (ValueWrapper (Number 300))))
      let query = albumsQuery {where_ = Just where'}
      receivedAlbums <- (Data.sortBy "AlbumId" . getQueryResponse) <$> (api // _query) sourceName config query

      let expectedAlbums =
            filter ((> Just 300) . (^? ix "AlbumId" . _Number)) Data.albumsAsJson

      receivedAlbums `shouldBe` expectedAlbums

    it "can filter by applying the greater than or equal operator" $ do
      let where' = ApplyBinaryComparisonOperator (ValueWrapper3 GreaterThanOrEqual (ColumnName "AlbumId") (ScalarValue (ValueWrapper (Number 300))))
      let query = albumsQuery {where_ = Just where'}
      receivedAlbums <- (Data.sortBy "AlbumId" . getQueryResponse) <$> (api // _query) sourceName config query

      let expectedAlbums =
            filter ((>= Just 300) . (^? ix "AlbumId" . _Number)) Data.albumsAsJson

      receivedAlbums `shouldBe` expectedAlbums

    it "can filter by applying the less than operator" $ do
      let where' = ApplyBinaryComparisonOperator (ValueWrapper3 LessThan (ColumnName "AlbumId") (ScalarValue (ValueWrapper (Number 100))))
      let query = albumsQuery {where_ = Just where'}
      receivedAlbums <- (Data.sortBy "AlbumId" . getQueryResponse) <$> (api // _query) sourceName config query

      let expectedAlbums =
            filter ((< Just 100) . (^? ix "AlbumId" . _Number)) Data.albumsAsJson

      receivedAlbums `shouldBe` expectedAlbums

    it "can filter by applying the less than or equal operator" $ do
      let where' = ApplyBinaryComparisonOperator (ValueWrapper3 LessThanOrEqual (ColumnName "AlbumId") (ScalarValue (ValueWrapper (Number 100))))
      let query = albumsQuery {where_ = Just where'}
      receivedAlbums <- (Data.sortBy "AlbumId" . getQueryResponse) <$> (api // _query) sourceName config query

      let expectedAlbums =
            filter ((<= Just 100) . (^? ix "AlbumId" . _Number)) Data.albumsAsJson

      receivedAlbums `shouldBe` expectedAlbums

    it "can filter using a greater than operator with a column comparison" $ do
      let where' = ApplyBinaryComparisonOperator (ValueWrapper3 GreaterThan (ColumnName "AlbumId") (AnotherColumn (ValueWrapper (ColumnName "ArtistId"))))
      let query = albumsQuery {where_ = Just where'}
      receivedAlbums <- (Data.sortBy "AlbumId" . getQueryResponse) <$> (api // _query) sourceName config query

      let expectedAlbums =
            filter (\album -> (album ^? ix "AlbumId" . _Number) > (album ^? ix "ArtistId" . _Number)) Data.albumsAsJson

      receivedAlbums `shouldBe` expectedAlbums

artistsQuery :: Query
artistsQuery =
  let fields = KeyMap.fromList [("ArtistId", columnField "ArtistId"), ("Name", columnField "Name")]
      tableName = TableName "Artist"
   in Query fields tableName Nothing Nothing Nothing Nothing

albumsQuery :: Query
albumsQuery =
  let fields = KeyMap.fromList [("AlbumId", columnField "AlbumId"), ("ArtistId", columnField "ArtistId"), ("Title", columnField "Title")]
      tableName = TableName "Album"
   in Query fields tableName Nothing Nothing Nothing Nothing

columnField :: Text -> Field
columnField = ColumnField . ValueWrapper . ColumnName
