module Test.QuerySpec.AggregatesSpec (spec) where

import Control.Arrow ((>>>))
import Control.Lens (ix, (%~), (&), (.~), (^?))
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (mapMaybe)
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
spec api sourceName config = describe "Aggregate Queries" $ do
  describe "Star Count" $ do
    it "counts all rows" $ do
      let aggregates = KeyMap.fromList [("count_all", StarCount)]
      let queryRequest = invoicesQueryRequest aggregates
      response <- (api // _query) sourceName config queryRequest

      let invoiceCount = length Data.invoicesAsJson
      let expectedAggregates = KeyMap.fromList [("count_all", Number $ fromIntegral invoiceCount)]

      Data.responseAggregates response `shouldBe` expectedAggregates

    it "counts all rows, after applying filters" $ do
      let where' = ApplyBinaryComparisonOperator Equal (localComparisonColumn "BillingCity") (ScalarValue (String "Oslo"))
      let aggregates = KeyMap.fromList [("count_all", StarCount)]
      let queryRequest = invoicesQueryRequest aggregates & qrQuery . qWhere .~ Just where'
      response <- (api // _query) sourceName config queryRequest

      let invoiceCount = length $ filter ((^? ix "BillingCity" . Data._ColumnFieldString) >>> (== Just "Oslo")) Data.invoicesAsJson
      let expectedAggregates = KeyMap.fromList [("count_all", Number $ fromIntegral invoiceCount)]

      Data.responseAggregates response `shouldBe` expectedAggregates

    it "counts all rows, after applying pagination" $ do
      let aggregates = KeyMap.fromList [("count_all", StarCount)]
      let queryRequest = invoicesQueryRequest aggregates & qrQuery %~ (qLimit .~ Just 20 >>> qOffset .~ Just 400)
      response <- (api // _query) sourceName config queryRequest

      let invoiceCount = length . take 20 $ drop 400 Data.invoicesAsJson
      let expectedAggregates = KeyMap.fromList [("count_all", Number $ fromIntegral invoiceCount)]

      Data.responseAggregates response `shouldBe` expectedAggregates

  describe "Column Count" $ do
    it "counts all rows with non-null columns - single column" $ do
      let aggregates = KeyMap.fromList [("count_cols", ColumnCount $ ColumnCountAggregate (ColumnName "BillingState" :| []) False)]
      let queryRequest = invoicesQueryRequest aggregates
      response <- (api // _query) sourceName config queryRequest

      let invoiceCount = length $ filter ((^? ix "BillingState" . Data._ColumnFieldString) >>> (/= Nothing)) Data.invoicesAsJson
      let expectedAggregates = KeyMap.fromList [("count_cols", Number $ fromIntegral invoiceCount)]

      Data.responseAggregates response `shouldBe` expectedAggregates

    it "can count all rows with non-null values in a column, after applying pagination and filtering" $ do
      let where' = ApplyBinaryComparisonOperator GreaterThanOrEqual (localComparisonColumn "InvoiceId") (ScalarValue (Number 380))
      let aggregates = KeyMap.fromList [("count_cols", ColumnCount $ ColumnCountAggregate (ColumnName "BillingState" :| []) False)]
      let queryRequest = invoicesQueryRequest aggregates & qrQuery %~ (qLimit .~ Just 20 >>> qWhere .~ Just where')
      response <- (api // _query) sourceName config queryRequest

      let invoiceCount =
            Data.invoicesAsJson
              & filter ((^? ix "InvoiceId" . Data._ColumnFieldNumber) >>> (>= Just 380))
              & take 20
              & mapMaybe ((^? ix "BillingState" . Data._ColumnFieldString))
              & length

      let expectedAggregates = KeyMap.fromList [("count_cols", Number $ fromIntegral invoiceCount)]

      Data.responseAggregates response `shouldBe` expectedAggregates

    it "can count all rows with distinct non-null values in a column" $ do
      let aggregates = KeyMap.fromList [("count_cols", ColumnCount $ ColumnCountAggregate (ColumnName "BillingState" :| []) True)]
      let queryRequest = invoicesQueryRequest aggregates
      response <- (api // _query) sourceName config queryRequest

      let billingStateCount = length . HashSet.fromList $ mapMaybe ((^? ix "BillingState" . Data._ColumnFieldString)) Data.invoicesAsJson
      let expectedAggregates = KeyMap.fromList [("count_cols", Number $ fromIntegral billingStateCount)]

      Data.responseAggregates response `shouldBe` expectedAggregates

    it "can count all rows with distinct non-null values in a column, after applying pagination and filtering" $ do
      let where' = ApplyBinaryComparisonOperator GreaterThanOrEqual (localComparisonColumn "InvoiceId") (ScalarValue (Number 380))
      let aggregates = KeyMap.fromList [("count_cols", ColumnCount $ ColumnCountAggregate (ColumnName "BillingState" :| []) True)]
      let queryRequest = invoicesQueryRequest aggregates & qrQuery %~ (qLimit .~ Just 20 >>> qWhere .~ Just where')
      response <- (api // _query) sourceName config queryRequest

      let billingStateCount =
            Data.invoicesAsJson
              & filter ((^? ix "InvoiceId" . Data._ColumnFieldNumber) >>> (>= Just 380))
              & take 20
              & mapMaybe ((^? ix "BillingState" . Data._ColumnFieldString))
              & HashSet.fromList
              & length

      let expectedAggregates = KeyMap.fromList [("count_cols", Number $ fromIntegral billingStateCount)]

      Data.responseAggregates response `shouldBe` expectedAggregates

  describe "Single Column Function" $ do
    it "can get the max total from all rows" $ do
      let aggregates = KeyMap.fromList [("max", SingleColumn $ SingleColumnAggregate Max (ColumnName "Total"))]
      let queryRequest = invoicesQueryRequest aggregates
      response <- (api // _query) sourceName config queryRequest

      let maxTotal = maximum $ mapMaybe ((^? ix "Total" . Data._ColumnFieldNumber)) Data.invoicesAsJson
      let expectedAggregates = KeyMap.fromList [("max", Number maxTotal)]

      Data.responseAggregates response `shouldBe` expectedAggregates

    it "can get the max total from all rows, after applying pagination, filtering and ordering" $ do
      let where' = ApplyBinaryComparisonOperator Equal (localComparisonColumn "BillingCountry") (ScalarValue (String "USA"))
      let orderBy = OrderBy (ColumnName "BillingPostalCode") Descending :| [OrderBy (ColumnName "InvoiceId") Ascending]
      let aggregates = KeyMap.fromList [("max", SingleColumn $ SingleColumnAggregate Max (ColumnName "Total"))]
      let queryRequest = invoicesQueryRequest aggregates & qrQuery %~ (qLimit .~ Just 20 >>> qWhere .~ Just where' >>> qOrderBy .~ Just orderBy)
      response <- (api // _query) sourceName config queryRequest

      let maxTotal =
            Data.invoicesAsJson
              & filter ((^? ix "BillingCountry" . Data._ColumnFieldString) >>> (== Just "USA"))
              & sortOn (Down . (^? ix "BillingPostalCode"))
              & take 20
              & mapMaybe ((^? ix "Total" . Data._ColumnFieldNumber))
              & maximum

      let expectedAggregates = KeyMap.fromList [("max", Number maxTotal)]

      Data.responseAggregates response `shouldBe` expectedAggregates

  describe "Multiple Aggregates and Returning Rows" $ do
    it "can get the max total from all rows, the count and the distinct count, simultaneously" $ do
      let aggregates =
            KeyMap.fromList
              [ ("count", StarCount),
                ("distinctBillingStates", ColumnCount $ ColumnCountAggregate (ColumnName "BillingState" :| []) True),
                ("maxTotal", SingleColumn $ SingleColumnAggregate Max (ColumnName "Total"))
              ]
      let queryRequest = invoicesQueryRequest aggregates
      response <- (api // _query) sourceName config queryRequest

      let invoiceCount = length Data.invoicesAsJson
      let billingStateCount = length . HashSet.fromList $ mapMaybe ((^? ix "BillingState" . Data._ColumnFieldString)) Data.invoicesAsJson
      let maxTotal = maximum $ mapMaybe ((^? ix "Total" . Data._ColumnFieldNumber)) Data.invoicesAsJson

      let expectedAggregates =
            KeyMap.fromList
              [ ("count", Number $ fromIntegral invoiceCount),
                ("distinctBillingStates", Number $ fromIntegral billingStateCount),
                ("maxTotal", Number maxTotal)
              ]

      Data.responseAggregates response `shouldBe` expectedAggregates

    it "can reuse the same aggregate twice" $ do
      let aggregates =
            KeyMap.fromList
              [ ("minInvoiceId", SingleColumn $ SingleColumnAggregate Min (ColumnName "InvoiceId")),
                ("minTotal", SingleColumn $ SingleColumnAggregate Min (ColumnName "Total"))
              ]
      let queryRequest = invoicesQueryRequest aggregates
      response <- (api // _query) sourceName config queryRequest

      let maxInvoiceId = minimum $ mapMaybe ((^? ix "InvoiceId" . Data._ColumnFieldNumber)) Data.invoicesAsJson
      let maxTotal = minimum $ mapMaybe ((^? ix "Total" . Data._ColumnFieldNumber)) Data.invoicesAsJson

      let expectedAggregates =
            KeyMap.fromList
              [ ("minInvoiceId", Number maxInvoiceId),
                ("minTotal", Number maxTotal)
              ]

      Data.responseAggregates response `shouldBe` expectedAggregates

    it "can also query for the rows involved in the aggregate" $ do
      let fields =
            KeyMap.fromList
              [ ("InvoiceId", columnField "InvoiceId"),
                ("BillingCountry", columnField "BillingCountry")
              ]
      let where' = ApplyBinaryComparisonOperator Equal (localComparisonColumn "BillingCountry") (ScalarValue (String "Canada"))
      let orderBy = OrderBy (ColumnName "BillingAddress") Ascending :| [OrderBy (ColumnName "InvoiceId") Ascending]
      let aggregates = KeyMap.fromList [("min", SingleColumn $ SingleColumnAggregate Min (ColumnName "Total"))]
      let queryRequest = invoicesQueryRequest aggregates & qrQuery %~ (qFields .~ Just fields >>> qLimit .~ Just 30 >>> qWhere .~ Just where' >>> qOrderBy .~ Just orderBy)
      response <- (api // _query) sourceName config queryRequest

      let invoiceRows =
            Data.invoicesAsJson
              & filter ((^? ix "BillingCountry" . Data._ColumnFieldString) >>> (== Just "Canada"))
              & sortOn (^? ix "BillingAddress")
              & take 30

      let maxTotal =
            invoiceRows
              & mapMaybe ((^? ix "Total" . Data._ColumnFieldNumber))
              & minimum

      let expectedAggregates = KeyMap.fromList [("min", Number maxTotal)]
      let expectedRows = Data.filterColumnsByQueryFields (_qrQuery queryRequest) <$> invoiceRows

      Data.responseRows response `shouldBe` expectedRows
      Data.responseAggregates response `shouldBe` expectedAggregates

  describe "Aggregates via Relationships" $ do
    it "can query aggregates via an array relationship" $ do
      let query = artistsWithAlbumsQuery id & qrQuery . qLimit .~ Just 5
      receivedArtists <- (api // _query) sourceName config query

      let joinInAlbums (artist :: KeyMap FieldValue) =
            let artistId = artist ^? ix "ArtistId" . Data._ColumnFieldNumber
                albumFilter artistId' album = album ^? ix "ArtistId" . Data._ColumnFieldNumber == Just artistId'
                albums = maybe [] (\artistId' -> filter (albumFilter artistId') Data.albumsAsJson) artistId
                aggregates = KeyMap.fromList [("count", Number . fromIntegral $ length albums)]
             in KeyMap.insert "Albums" (mkSubqueryResponse Nothing (Just aggregates)) artist

      let expectedAlbums =
            Data.artistsAsJson
              & take 5
              & fmap joinInAlbums

      Data.responseRows receivedArtists `shouldBe` expectedAlbums

    it "can query aggregates via an array relationship and include the rows in that relationship" $ do
      let albumFields =
            KeyMap.fromList
              [ ("AlbumId", columnField "AlbumId"),
                ("Title", columnField "Title")
              ]
      let query = artistsWithAlbumsQuery (qFields .~ Just albumFields) & qrQuery . qLimit .~ Just 5
      receivedArtists <- (api // _query) sourceName config query

      let joinInAlbums (artist :: KeyMap FieldValue) =
            let artistId = artist ^? ix "ArtistId" . Data._ColumnFieldNumber
                albumFilter artistId' album = album ^? ix "ArtistId" . Data._ColumnFieldNumber == Just artistId'
                albums = maybe [] (\artistId' -> filter (albumFilter artistId') Data.albumsAsJson) artistId
                albums' = KeyMap.delete "ArtistId" <$> albums
                aggregates = KeyMap.fromList [("count", Number . fromIntegral $ length albums)]
             in KeyMap.insert "Albums" (mkSubqueryResponse (Just albums') (Just aggregates)) artist

      let expectedAlbums =
            Data.artistsAsJson
              & take 5
              & fmap joinInAlbums

      Data.responseRows receivedArtists `shouldBe` expectedAlbums

artistsTableName :: TableName
artistsTableName = TableName "Artist"

albumsTableName :: TableName
albumsTableName = TableName "Album"

albumsRelationshipName :: RelationshipName
albumsRelationshipName = RelationshipName "Albums"

artistsTableRelationships :: TableRelationships
artistsTableRelationships =
  let joinFieldMapping =
        HashMap.fromList
          [ (ColumnName "ArtistId", ColumnName "ArtistId")
          ]
   in TableRelationships
        artistsTableName
        ( HashMap.fromList
            [ (albumsRelationshipName, Relationship albumsTableName ArrayRelationship joinFieldMapping)
            ]
        )

artistsWithAlbumsQuery :: (Query -> Query) -> QueryRequest
artistsWithAlbumsQuery modifySubquery =
  let albumAggregates = KeyMap.fromList [("count", StarCount)]
      albumsSubquery = Query Nothing (Just albumAggregates) Nothing Nothing Nothing Nothing & modifySubquery
      artistFields =
        KeyMap.fromList
          [ ("ArtistId", columnField "ArtistId"),
            ("Name", columnField "Name"),
            ("Albums", RelField $ RelationshipField albumsRelationshipName albumsSubquery)
          ]
      artistOrderBy = OrderBy (ColumnName "ArtistId") Ascending :| []
      artistQuery = Query (Just artistFields) Nothing Nothing Nothing Nothing (Just artistOrderBy)
   in QueryRequest artistsTableName [artistsTableRelationships] artistQuery

invoicesQueryRequest :: KeyMap Aggregate -> QueryRequest
invoicesQueryRequest aggregates =
  let tableName = TableName "Invoice"
      query = Query Nothing (Just aggregates) Nothing Nothing Nothing Nothing
   in QueryRequest tableName [] query

mkSubqueryResponse :: Maybe [KeyMap FieldValue] -> Maybe (KeyMap Value) -> FieldValue
mkSubqueryResponse rows aggregates =
  mkRelationshipFieldValue $ QueryResponse rows aggregates

localComparisonColumn :: Text -> ComparisonColumn
localComparisonColumn columnName = ComparisonColumn [] $ ColumnName columnName

columnField :: Text -> Field
columnField = ColumnField . ColumnName
