module Test.QuerySpec.AggregatesSpec (spec) where

import Control.Arrow ((>>>))
import Control.Lens (ix, (%~), (&), (?~), (^?))
import Control.Monad (when)
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.HashSet qualified as HashSet
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Ord (Down (..))
import Hasura.Backends.DataConnector.API
import Servant.API (NamedRoutes)
import Servant.Client (Client, (//))
import Test.Data qualified as Data
import Test.Expectations (jsonShouldBe, rowsShouldBe)
import Test.Hspec (Spec, describe, it)
import Prelude

spec :: Client IO (NamedRoutes Routes) -> SourceName -> Config -> Maybe RelationshipCapabilities -> Spec
spec api sourceName config relationshipCapabilities = describe "Aggregate Queries" $ do
  describe "Star Count" $ do
    it "counts all rows" $ do
      let aggregates = KeyMap.fromList [("count_all", StarCount)]
      let queryRequest = invoicesQueryRequest aggregates
      response <- (api // _query) sourceName config queryRequest

      let invoiceCount = length Data.invoicesRows
      let expectedAggregates = KeyMap.fromList [("count_all", Number $ fromIntegral invoiceCount)]

      Data.responseAggregates response `jsonShouldBe` expectedAggregates
      Data.responseRows response `rowsShouldBe` []

    it "counts all rows, after applying filters" $ do
      let where' = ApplyBinaryComparisonOperator Equal (Data.localComparisonColumn "BillingCity") (ScalarValue (String "Oslo"))
      let aggregates = KeyMap.fromList [("count_all", StarCount)]
      let queryRequest = invoicesQueryRequest aggregates & qrQuery . qWhere ?~ where'
      response <- (api // _query) sourceName config queryRequest

      let invoiceCount = length $ filter ((^? ix "BillingCity" . Data._ColumnFieldString) >>> (== Just "Oslo")) Data.invoicesRows
      let expectedAggregates = KeyMap.fromList [("count_all", Number $ fromIntegral invoiceCount)]

      Data.responseAggregates response `jsonShouldBe` expectedAggregates
      Data.responseRows response `rowsShouldBe` []

    it "counts all rows, after applying pagination" $ do
      let aggregates = KeyMap.fromList [("count_all", StarCount)]
      let queryRequest = invoicesQueryRequest aggregates & qrQuery %~ (qLimit ?~ 20 >>> qOffset ?~ 400)
      response <- (api // _query) sourceName config queryRequest

      let invoiceCount = length . take 20 $ drop 400 Data.invoicesRows
      let expectedAggregates = KeyMap.fromList [("count_all", Number $ fromIntegral invoiceCount)]

      Data.responseAggregates response `jsonShouldBe` expectedAggregates
      Data.responseRows response `rowsShouldBe` []

  describe "Column Count" $ do
    it "counts all rows with non-null columns" $ do
      let aggregates = KeyMap.fromList [("count_cols", ColumnCount $ ColumnCountAggregate (ColumnName "BillingState") False)]
      let queryRequest = invoicesQueryRequest aggregates
      response <- (api // _query) sourceName config queryRequest

      let invoiceCount = length $ filter ((^? ix "BillingState" . Data._ColumnFieldString) >>> (/= Nothing)) Data.invoicesRows
      let expectedAggregates = KeyMap.fromList [("count_cols", Number $ fromIntegral invoiceCount)]

      Data.responseAggregates response `jsonShouldBe` expectedAggregates
      Data.responseRows response `rowsShouldBe` []

    it "can count all rows with non-null values in a column, after applying pagination and filtering" $ do
      let where' = ApplyBinaryComparisonOperator GreaterThanOrEqual (Data.localComparisonColumn "InvoiceId") (ScalarValue (Number 380))
      let aggregates = KeyMap.fromList [("count_cols", ColumnCount $ ColumnCountAggregate (ColumnName "BillingState") False)]
      let queryRequest = invoicesQueryRequest aggregates & qrQuery %~ (qLimit ?~ 20 >>> qWhere ?~ where')
      response <- (api // _query) sourceName config queryRequest

      let invoiceCount =
            Data.invoicesRows
              & filter ((^? ix "InvoiceId" . Data._ColumnFieldNumber) >>> (>= Just 380))
              & take 20
              & mapMaybe ((^? ix "BillingState" . Data._ColumnFieldString))
              & length

      let expectedAggregates = KeyMap.fromList [("count_cols", Number $ fromIntegral invoiceCount)]

      Data.responseAggregates response `jsonShouldBe` expectedAggregates
      Data.responseRows response `rowsShouldBe` []

    it "can count all rows with distinct non-null values in a column" $ do
      let aggregates = KeyMap.fromList [("count_cols", ColumnCount $ ColumnCountAggregate (ColumnName "BillingState") True)]
      let queryRequest = invoicesQueryRequest aggregates
      response <- (api // _query) sourceName config queryRequest

      let billingStateCount = length . HashSet.fromList $ mapMaybe ((^? ix "BillingState" . Data._ColumnFieldString)) Data.invoicesRows
      let expectedAggregates = KeyMap.fromList [("count_cols", Number $ fromIntegral billingStateCount)]

      Data.responseAggregates response `jsonShouldBe` expectedAggregates
      Data.responseRows response `rowsShouldBe` []

    it "can count all rows with distinct non-null values in a column, after applying pagination and filtering" $ do
      let where' = ApplyBinaryComparisonOperator GreaterThanOrEqual (Data.localComparisonColumn "InvoiceId") (ScalarValue (Number 380))
      let aggregates = KeyMap.fromList [("count_cols", ColumnCount $ ColumnCountAggregate (ColumnName "BillingState") True)]
      let queryRequest = invoicesQueryRequest aggregates & qrQuery %~ (qLimit ?~ 20 >>> qWhere ?~ where')
      response <- (api // _query) sourceName config queryRequest

      let billingStateCount =
            Data.invoicesRows
              & filter ((^? ix "InvoiceId" . Data._ColumnFieldNumber) >>> (>= Just 380))
              & take 20
              & mapMaybe ((^? ix "BillingState" . Data._ColumnFieldString))
              & HashSet.fromList
              & length

      let expectedAggregates = KeyMap.fromList [("count_cols", Number $ fromIntegral billingStateCount)]

      Data.responseAggregates response `jsonShouldBe` expectedAggregates
      Data.responseRows response `rowsShouldBe` []

  describe "Single Column Function" $ do
    it "can get the max total from all rows" $ do
      let aggregates = KeyMap.fromList [("max", SingleColumn $ SingleColumnAggregate Max (ColumnName "Total"))]
      let queryRequest = invoicesQueryRequest aggregates
      response <- (api // _query) sourceName config queryRequest

      let maxTotal = maximum $ mapMaybe ((^? ix "Total" . Data._ColumnFieldNumber)) Data.invoicesRows
      let expectedAggregates = KeyMap.fromList [("max", Number maxTotal)]

      Data.responseAggregates response `jsonShouldBe` expectedAggregates
      Data.responseRows response `rowsShouldBe` []

    it "can get the max total from all rows, after applying pagination, filtering and ordering" $ do
      let where' = ApplyBinaryComparisonOperator Equal (Data.localComparisonColumn "BillingCountry") (ScalarValue (String "USA"))
      let orderBy = OrderBy mempty $ Data.orderByColumn [] "BillingPostalCode" Descending :| [Data.orderByColumn [] "InvoiceId" Ascending]
      let aggregates = KeyMap.fromList [("max", SingleColumn $ SingleColumnAggregate Max (ColumnName "Total"))]
      let queryRequest = invoicesQueryRequest aggregates & qrQuery %~ (qLimit ?~ 20 >>> qWhere ?~ where' >>> qOrderBy ?~ orderBy)
      response <- (api // _query) sourceName config queryRequest

      let maxTotal =
            Data.invoicesRows
              & filter ((^? ix "BillingCountry" . Data._ColumnFieldString) >>> (== Just "USA"))
              & sortOn (Down . (^? ix "BillingPostalCode"))
              & take 20
              & mapMaybe ((^? ix "Total" . Data._ColumnFieldNumber))
              & maximum

      let expectedAggregates = KeyMap.fromList [("max", Number maxTotal)]

      Data.responseAggregates response `jsonShouldBe` expectedAggregates
      Data.responseRows response `rowsShouldBe` []

    it "can get the min and max of a non-numeric comparable type such as a string" $ do
      let aggregates =
            KeyMap.fromList
              [ ("min", SingleColumn $ SingleColumnAggregate Min (ColumnName "Name")),
                ("max", SingleColumn $ SingleColumnAggregate Max (ColumnName "Name"))
              ]
      let queryRequest = artistsQueryRequest aggregates
      response <- (api // _query) sourceName config queryRequest

      let names = mapMaybe ((^? ix "Name" . Data._ColumnFieldString)) Data.artistsRows
      let expectedAggregates =
            KeyMap.fromList
              [ ("min", aggregate (String . minimum) names),
                ("max", aggregate (String . maximum) names)
              ]

      Data.responseAggregates response `jsonShouldBe` expectedAggregates
      Data.responseRows response `rowsShouldBe` []

    it "aggregates over empty row lists results in nulls" $ do
      let where' = ApplyBinaryComparisonOperator LessThan (Data.localComparisonColumn "ArtistId") (ScalarValue (Number 0))
      let aggregates = KeyMap.fromList [("min", SingleColumn $ SingleColumnAggregate Min (ColumnName "Name"))]
      let queryRequest = artistsQueryRequest aggregates & qrQuery . qWhere ?~ where'
      response <- (api // _query) sourceName config queryRequest

      let expectedAggregates = KeyMap.fromList [("min", Null)]

      Data.responseAggregates response `jsonShouldBe` expectedAggregates
      Data.responseRows response `rowsShouldBe` []

  describe "Multiple Aggregates and Returning Rows" $ do
    it "can get the max total from all rows, the count and the distinct count, simultaneously" $ do
      let aggregates =
            KeyMap.fromList
              [ ("count", StarCount),
                ("distinctBillingStates", ColumnCount $ ColumnCountAggregate (ColumnName "BillingState") True),
                ("maxTotal", SingleColumn $ SingleColumnAggregate Max (ColumnName "Total"))
              ]
      let queryRequest = invoicesQueryRequest aggregates
      response <- (api // _query) sourceName config queryRequest

      let invoiceCount = length Data.invoicesRows
      let billingStateCount = length . HashSet.fromList $ mapMaybe ((^? ix "BillingState" . Data._ColumnFieldString)) Data.invoicesRows
      let maxTotal = aggregate (Number . maximum) $ mapMaybe ((^? ix "Total" . Data._ColumnFieldNumber)) Data.invoicesRows

      let expectedAggregates =
            KeyMap.fromList
              [ ("count", Number $ fromIntegral invoiceCount),
                ("distinctBillingStates", Number $ fromIntegral billingStateCount),
                ("maxTotal", maxTotal)
              ]

      Data.responseAggregates response `jsonShouldBe` expectedAggregates
      Data.responseRows response `rowsShouldBe` []

    it "can reuse the same aggregate twice" $ do
      let aggregates =
            KeyMap.fromList
              [ ("minInvoiceId", SingleColumn $ SingleColumnAggregate Min (ColumnName "InvoiceId")),
                ("minTotal", SingleColumn $ SingleColumnAggregate Min (ColumnName "Total"))
              ]
      let queryRequest = invoicesQueryRequest aggregates
      response <- (api // _query) sourceName config queryRequest

      let maxInvoiceId = aggregate (Number . minimum) $ mapMaybe ((^? ix "InvoiceId" . Data._ColumnFieldNumber)) Data.invoicesRows
      let maxTotal = aggregate (Number . minimum) $ mapMaybe ((^? ix "Total" . Data._ColumnFieldNumber)) Data.invoicesRows

      let expectedAggregates =
            KeyMap.fromList
              [ ("minInvoiceId", maxInvoiceId),
                ("minTotal", maxTotal)
              ]

      Data.responseAggregates response `jsonShouldBe` expectedAggregates
      Data.responseRows response `rowsShouldBe` []

    it "can also query for the rows involved in the aggregate" $ do
      let fields =
            KeyMap.fromList
              [ ("InvoiceId", Data.columnField "InvoiceId"),
                ("BillingCountry", Data.columnField "BillingCountry")
              ]
      let where' = ApplyBinaryComparisonOperator Equal (Data.localComparisonColumn "BillingCountry") (ScalarValue (String "Canada"))
      let orderBy = OrderBy mempty $ Data.orderByColumn [] "BillingAddress" Ascending :| [Data.orderByColumn [] "InvoiceId" Ascending]
      let aggregates = KeyMap.fromList [("min", SingleColumn $ SingleColumnAggregate Min (ColumnName "Total"))]
      let queryRequest = invoicesQueryRequest aggregates & qrQuery %~ (qFields ?~ fields >>> qLimit ?~ 30 >>> qWhere ?~ where' >>> qOrderBy ?~ orderBy)
      response <- (api // _query) sourceName config queryRequest

      let invoiceRows =
            Data.invoicesRows
              & filter ((^? ix "BillingCountry" . Data._ColumnFieldString) >>> (== Just "Canada"))
              & sortOn (^? ix "BillingAddress")
              & take 30

      let maxTotal =
            invoiceRows
              & mapMaybe ((^? ix "Total" . Data._ColumnFieldNumber))
              & aggregate (Number . minimum)

      let expectedAggregates = KeyMap.fromList [("min", maxTotal)]
      let expectedRows = Data.filterColumnsByQueryFields (_qrQuery queryRequest) <$> invoiceRows

      Data.responseRows response `rowsShouldBe` expectedRows
      Data.responseAggregates response `jsonShouldBe` expectedAggregates

  when (isJust relationshipCapabilities) $
    describe "Aggregates via Relationships" $ do
      it "can query aggregates via an array relationship" $ do
        let query = artistsWithAlbumsQuery id & qrQuery . qLimit ?~ 5
        receivedArtists <- (api // _query) sourceName config query

        let joinInAlbums (artist :: KeyMap FieldValue) = fromMaybe artist $ do
              artistId <- artist ^? ix "ArtistId" . Data._ColumnFieldNumber
              let albums =
                    Data.albumsRows
                      & filter ((^? ix "ArtistId" . Data._ColumnFieldNumber) >>> (== Just artistId))
              let aggregates = KeyMap.fromList [("count", Number . fromIntegral $ length albums)]
              pure $ KeyMap.insert "Albums" (mkSubqueryResponse Nothing (Just aggregates)) artist

        let expectedArtists =
              Data.artistsRows
                & take 5
                & fmap joinInAlbums

        Data.responseRows receivedArtists `rowsShouldBe` expectedArtists
        Data.responseAggregates receivedArtists `jsonShouldBe` mempty

      it "can query aggregates via an array relationship and include the rows in that relationship" $ do
        let albumFields =
              KeyMap.fromList
                [ ("AlbumId", Data.columnField "AlbumId"),
                  ("Title", Data.columnField "Title")
                ]
        let query = artistsWithAlbumsQuery (qFields ?~ albumFields) & qrQuery . qLimit ?~ 5
        receivedArtists <- (api // _query) sourceName config query

        let joinInAlbums (artist :: KeyMap FieldValue) = fromMaybe artist $ do
              artistId <- artist ^? ix "ArtistId" . Data._ColumnFieldNumber
              let albums =
                    Data.albumsRows
                      & filter ((^? ix "ArtistId" . Data._ColumnFieldNumber) >>> (== Just artistId))
                      & Data.filterColumns ["AlbumId", "Title"]
              let aggregates = KeyMap.fromList [("count", Number . fromIntegral $ length albums)]
              pure $ KeyMap.insert "Albums" (mkSubqueryResponse (Just albums) (Just aggregates)) artist

        let expectedArtists =
              Data.artistsRows
                & take 5
                & fmap joinInAlbums

        Data.responseRows receivedArtists `rowsShouldBe` expectedArtists
        Data.responseAggregates receivedArtists `jsonShouldBe` mempty

      it "can query with many nested relationships, with aggregates at multiple levels, with filtering, pagination and ordering" $ do
        receivedArtists <- (api // _query) sourceName config deeplyNestedArtistsQuery

        let joinInMediaType (track :: KeyMap FieldValue) = fromMaybe track $ do
              mediaTypeId <- track ^? ix "MediaTypeId" . Data._ColumnFieldNumber
              let mediaTypes =
                    Data.mediaTypesRows
                      & filter ((^? ix "MediaTypeId" . Data._ColumnFieldNumber) >>> (== Just mediaTypeId))
                      & Data.filterColumns ["Name"]
              pure $ KeyMap.insert "nodes_MediaType" (mkSubqueryResponse (Just mediaTypes) Nothing) track

        let joinInInvoiceLines (track :: KeyMap FieldValue) = fromMaybe track $ do
              trackId <- track ^? ix "TrackId" . Data._ColumnFieldNumber
              let invoiceLines =
                    Data.invoiceLinesRows
                      & filter ((^? ix "TrackId" . Data._ColumnFieldNumber) >>> (== Just trackId))
              let getQuantity invoiceLine = invoiceLine ^? ix "Quantity" . Data._ColumnFieldNumber
              let invoiceLinesAggregates = KeyMap.fromList [("aggregate_sum_Quantity", aggregate (Number . sum) $ mapMaybe getQuantity invoiceLines)]
              pure $ KeyMap.insert "nodes_InvoiceLines_aggregate" (mkSubqueryResponse Nothing (Just invoiceLinesAggregates)) track

        let joinInTracks (album :: KeyMap FieldValue) = fromMaybe album $ do
              albumId <- album ^? ix "AlbumId" . Data._ColumnFieldNumber
              let tracks =
                    Data.tracksRows
                      & filter
                        ( \track ->
                            track ^? ix "AlbumId" . Data._ColumnFieldNumber == Just albumId
                              && track ^? ix "Milliseconds" . Data._ColumnFieldNumber < Just 300000
                        )
                      & sortOn (Down . (^? ix "Name" . Data._ColumnFieldString))
                      & fmap (joinInMediaType >>> joinInInvoiceLines)
                      & Data.renameColumns [("Name", "nodes_Name")]
                      & Data.filterColumns ["nodes_Name", "nodes_MediaType", "nodes_InvoiceLines_aggregate"]
              let tracksAggregates = KeyMap.fromList [("aggregate_count", Number . fromIntegral $ length tracks)]
              pure $ KeyMap.insert "nodes_Tracks_aggregate" (mkSubqueryResponse (Just tracks) (Just tracksAggregates)) album

        let joinInAlbums (artist :: KeyMap FieldValue) = fromMaybe artist $ do
              artistId <- artist ^? ix "ArtistId" . Data._ColumnFieldNumber
              let albums =
                    Data.albumsRows
                      & filter ((^? ix "ArtistId" . Data._ColumnFieldNumber) >>> (== Just artistId))
                      & fmap joinInTracks
                      & Data.renameColumns [("Title", "nodes_Title")]
                      & Data.filterColumns ["nodes_Title", "nodes_Tracks_aggregate"]
              pure $ KeyMap.insert "Albums_aggregate" (mkSubqueryResponse (Just albums) Nothing) artist

        let expectedArtists =
              Data.artistsRows
                & sortOn (Down . (^? ix "Name"))
                & filter ((^? ix "Name" . Data._ColumnFieldString) >>> (\name -> name > Just "A" && name < Just "B"))
                & drop 1
                & take 3
                & fmap joinInAlbums
                & Data.filterColumns ["Name", "Albums_aggregate"]

        Data.responseRows receivedArtists `rowsShouldBe` expectedArtists
        Data.responseAggregates receivedArtists `jsonShouldBe` mempty

artistsWithAlbumsQuery :: (Query -> Query) -> QueryRequest
artistsWithAlbumsQuery modifySubquery =
  let albumAggregates = KeyMap.fromList [("count", StarCount)]
      albumsSubquery = Data.emptyQuery & qAggregates ?~ albumAggregates & modifySubquery
      artistFields =
        KeyMap.fromList
          [ ("ArtistId", Data.columnField "ArtistId"),
            ("Name", Data.columnField "Name"),
            ("Albums", RelField $ RelationshipField Data.albumsRelationshipName albumsSubquery)
          ]
      artistOrderBy = OrderBy mempty $ Data.orderByColumn [] "ArtistId" Ascending :| []
      artistQuery = Data.emptyQuery & qFields ?~ artistFields & qOrderBy ?~ artistOrderBy
      artistsTableRelationships = Data.onlyKeepRelationships [Data.albumsRelationshipName] Data.artistsTableRelationships
   in QueryRequest Data.artistsTableName [artistsTableRelationships] artistQuery

-- | This query is basically what would be generated by this complex HGE GraphQL query
-- @
--   query {
--     Artist(where: {_and: [{Name: {_gt: "A"}}, {Name: {_lt: "B"}}]}, limit: 3, offset: 1, order_by: {Name: desc}) {
--       Name
--       Albums_aggregate {
--         nodes {
--           Title
--           Tracks_aggregate(where: {Milliseconds: {_lt: 300000}}, order_by: {Name: desc}) {
--             aggregate {
--               count
--             }
--             nodes {
--               Name
--               MediaType {
--                 Name
--               }
--               InvoiceLines_aggregate {
--                 aggregate {
--                   sum {
--                     Quantity
--                   }
--                 }
--               }
--             }
--           }
--         }
--       }
--     }
--   }
-- @
deeplyNestedArtistsQuery :: QueryRequest
deeplyNestedArtistsQuery =
  let invoiceLinesAggregates = KeyMap.fromList [("aggregate_sum_Quantity", SingleColumn $ SingleColumnAggregate Sum (ColumnName "Quantity"))]
      invoiceLinesSubquery = Data.emptyQuery & qAggregates ?~ invoiceLinesAggregates
      mediaTypeFields = KeyMap.fromList [("Name", Data.columnField "Name")]
      mediaTypeSubquery = Data.emptyQuery & qFields ?~ mediaTypeFields
      tracksFields =
        KeyMap.fromList
          [ ("nodes_Name", Data.columnField "Name"),
            ("nodes_MediaType", RelField $ RelationshipField Data.mediaTypeRelationshipName mediaTypeSubquery),
            ("nodes_InvoiceLines_aggregate", RelField $ RelationshipField Data.invoiceLinesRelationshipName invoiceLinesSubquery)
          ]
      tracksAggregates = KeyMap.fromList [("aggregate_count", StarCount)]
      tracksWhere = ApplyBinaryComparisonOperator LessThan (Data.localComparisonColumn "Milliseconds") (ScalarValue $ Number 300000)
      tracksOrderBy = OrderBy mempty $ Data.orderByColumn [] "Name" Descending :| []
      tracksSubquery = Query (Just tracksFields) (Just tracksAggregates) Nothing Nothing (Just tracksWhere) (Just tracksOrderBy)
      albumsFields =
        KeyMap.fromList
          [ ("nodes_Title", Data.columnField "Title"),
            ("nodes_Tracks_aggregate", RelField $ RelationshipField Data.tracksRelationshipName tracksSubquery)
          ]
      albumsSubquery = Data.emptyQuery & qFields ?~ albumsFields
      artistFields =
        KeyMap.fromList
          [ ("Name", Data.columnField "Name"),
            ("Albums_aggregate", RelField $ RelationshipField Data.albumsRelationshipName albumsSubquery)
          ]
      artistWhere =
        And
          [ ApplyBinaryComparisonOperator GreaterThan (Data.localComparisonColumn "Name") (ScalarValue $ String "A"),
            ApplyBinaryComparisonOperator LessThan (Data.localComparisonColumn "Name") (ScalarValue $ String "B")
          ]
      artistOrderBy = OrderBy mempty $ Data.orderByColumn [] "Name" Descending :| []
      artistQuery = Query (Just artistFields) Nothing (Just 3) (Just 1) (Just artistWhere) (Just artistOrderBy)
   in QueryRequest
        Data.artistsTableName
        [ Data.onlyKeepRelationships [Data.albumsRelationshipName] Data.artistsTableRelationships,
          Data.onlyKeepRelationships [Data.tracksRelationshipName] Data.albumsTableRelationships,
          Data.onlyKeepRelationships [Data.invoiceLinesRelationshipName, Data.mediaTypeRelationshipName] Data.tracksTableRelationships
        ]
        artistQuery

artistsQueryRequest :: KeyMap Aggregate -> QueryRequest
artistsQueryRequest aggregates =
  let query = Data.emptyQuery & qAggregates ?~ aggregates
   in QueryRequest Data.artistsTableName [] query

invoicesQueryRequest :: KeyMap Aggregate -> QueryRequest
invoicesQueryRequest aggregates =
  let query = Data.emptyQuery & qAggregates ?~ aggregates
   in QueryRequest Data.invoicesTableName [] query

mkSubqueryResponse :: Maybe [KeyMap FieldValue] -> Maybe (KeyMap Value) -> FieldValue
mkSubqueryResponse rows aggregates =
  mkRelationshipFieldValue $ QueryResponse rows aggregates

aggregate :: (NonEmpty a -> Value) -> [a] -> Value
aggregate aggFn values =
  maybe Null aggFn $ NonEmpty.nonEmpty values
