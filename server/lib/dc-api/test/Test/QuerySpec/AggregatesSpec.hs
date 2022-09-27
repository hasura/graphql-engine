module Test.QuerySpec.AggregatesSpec (spec) where

import Control.Arrow ((>>>))
import Control.Lens ((%~), (&), (?~), (^?))
import Control.Monad (when)
import Data.Aeson (Value (..))
import Data.HashMap.Strict (HashMap)
import Data.HashSet qualified as HashSet
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Ord (Down (..))
import Hasura.Backends.DataConnector.API
import Servant.API (NamedRoutes)
import Servant.Client (Client, (//))
import Test.Data (TestData (..))
import Test.Data qualified as Data
import Test.Expectations (jsonShouldBe, rowsShouldBe)
import Test.Hspec (Spec, describe, it)
import Prelude

spec :: TestData -> Client IO (NamedRoutes Routes) -> SourceName -> Config -> Maybe RelationshipCapabilities -> Spec
spec TestData {..} api sourceName config relationshipCapabilities = describe "Aggregate Queries" $ do
  describe "Star Count" $ do
    it "counts all rows" $ do
      let aggregates = Data.mkFieldsMap [("count_all", StarCount)]
      let queryRequest = invoicesQueryRequest aggregates
      response <- (api // _query) sourceName config queryRequest

      let invoiceCount = length _tdInvoicesRows
      let expectedAggregates = Data.mkFieldsMap [("count_all", Number $ fromIntegral invoiceCount)]

      Data.responseAggregates response `jsonShouldBe` expectedAggregates
      Data.responseRows response `rowsShouldBe` []

    it "counts all rows, after applying filters" $ do
      let where' = ApplyBinaryComparisonOperator Equal (Data.currentComparisonColumn "BillingCity") (ScalarValue (String "Oslo"))
      let aggregates = Data.mkFieldsMap [("count_all", StarCount)]
      let queryRequest = invoicesQueryRequest aggregates & qrQuery . qWhere ?~ where'
      response <- (api // _query) sourceName config queryRequest

      let invoiceCount = length $ filter ((^? Data.field "BillingCity" . Data._ColumnFieldString) >>> (== Just "Oslo")) _tdInvoicesRows
      let expectedAggregates = Data.mkFieldsMap [("count_all", Number $ fromIntegral invoiceCount)]

      Data.responseAggregates response `jsonShouldBe` expectedAggregates
      Data.responseRows response `rowsShouldBe` []

    it "counts all rows, after applying pagination" $ do
      let aggregates = Data.mkFieldsMap [("count_all", StarCount)]
      let queryRequest = invoicesQueryRequest aggregates & qrQuery %~ (qLimit ?~ 20 >>> qOffset ?~ 400)
      response <- (api // _query) sourceName config queryRequest

      let invoiceCount = length . take 20 $ drop 400 _tdInvoicesRows
      let expectedAggregates = Data.mkFieldsMap [("count_all", Number $ fromIntegral invoiceCount)]

      Data.responseAggregates response `jsonShouldBe` expectedAggregates
      Data.responseRows response `rowsShouldBe` []

  describe "Column Count" $ do
    it "counts all rows with non-null columns" $ do
      let aggregates = Data.mkFieldsMap [("count_cols", ColumnCount $ ColumnCountAggregate (ColumnName "BillingState") False)]
      let queryRequest = invoicesQueryRequest aggregates
      response <- (api // _query) sourceName config queryRequest

      let invoiceCount = length $ filter ((^? Data.field "BillingState" . Data._ColumnFieldString) >>> (/= Nothing)) _tdInvoicesRows
      let expectedAggregates = Data.mkFieldsMap [("count_cols", Number $ fromIntegral invoiceCount)]

      Data.responseAggregates response `jsonShouldBe` expectedAggregates
      Data.responseRows response `rowsShouldBe` []

    it "can count all rows with non-null values in a column, after applying pagination and filtering" $ do
      let where' = ApplyBinaryComparisonOperator GreaterThanOrEqual (Data.currentComparisonColumn "InvoiceId") (ScalarValue (Number 380))
      let aggregates = Data.mkFieldsMap [("count_cols", ColumnCount $ ColumnCountAggregate (ColumnName "BillingState") False)]
      let queryRequest = invoicesQueryRequest aggregates & qrQuery %~ (qLimit ?~ 20 >>> qWhere ?~ where')
      response <- (api // _query) sourceName config queryRequest

      let invoiceCount =
            _tdInvoicesRows
              & filter ((^? Data.field "InvoiceId" . Data._ColumnFieldNumber) >>> (>= Just 380))
              & take 20
              & mapMaybe ((^? Data.field "BillingState" . Data._ColumnFieldString))
              & length

      let expectedAggregates = Data.mkFieldsMap [("count_cols", Number $ fromIntegral invoiceCount)]

      Data.responseAggregates response `jsonShouldBe` expectedAggregates
      Data.responseRows response `rowsShouldBe` []

    it "can count all rows with distinct non-null values in a column" $ do
      let aggregates = Data.mkFieldsMap [("count_cols", ColumnCount $ ColumnCountAggregate (ColumnName "BillingState") True)]
      let queryRequest = invoicesQueryRequest aggregates
      response <- (api // _query) sourceName config queryRequest

      let billingStateCount = length . HashSet.fromList $ mapMaybe ((^? Data.field "BillingState" . Data._ColumnFieldString)) _tdInvoicesRows
      let expectedAggregates = Data.mkFieldsMap [("count_cols", Number $ fromIntegral billingStateCount)]

      Data.responseAggregates response `jsonShouldBe` expectedAggregates
      Data.responseRows response `rowsShouldBe` []

    it "can count all rows with distinct non-null values in a column, after applying pagination and filtering" $ do
      let where' = ApplyBinaryComparisonOperator GreaterThanOrEqual (Data.currentComparisonColumn "InvoiceId") (ScalarValue (Number 380))
      let aggregates = Data.mkFieldsMap [("count_cols", ColumnCount $ ColumnCountAggregate (ColumnName "BillingState") True)]
      let queryRequest = invoicesQueryRequest aggregates & qrQuery %~ (qLimit ?~ 20 >>> qWhere ?~ where')
      response <- (api // _query) sourceName config queryRequest

      let billingStateCount =
            _tdInvoicesRows
              & filter ((^? Data.field "InvoiceId" . Data._ColumnFieldNumber) >>> (>= Just 380))
              & take 20
              & mapMaybe ((^? Data.field "BillingState" . Data._ColumnFieldString))
              & HashSet.fromList
              & length

      let expectedAggregates = Data.mkFieldsMap [("count_cols", Number $ fromIntegral billingStateCount)]

      Data.responseAggregates response `jsonShouldBe` expectedAggregates
      Data.responseRows response `rowsShouldBe` []

  describe "Single Column Function" $ do
    it "can get the max total from all rows" $ do
      let aggregates = Data.mkFieldsMap [("max", SingleColumn $ SingleColumnAggregate Max (ColumnName "Total"))]
      let queryRequest = invoicesQueryRequest aggregates
      response <- (api // _query) sourceName config queryRequest

      let maxTotal = maximum $ mapMaybe ((^? Data.field "Total" . Data._ColumnFieldNumber)) _tdInvoicesRows
      let expectedAggregates = Data.mkFieldsMap [("max", Number maxTotal)]

      Data.responseAggregates response `jsonShouldBe` expectedAggregates
      Data.responseRows response `rowsShouldBe` []

    it "can get the max total from all rows, after applying pagination, filtering and ordering" $ do
      let where' = ApplyBinaryComparisonOperator Equal (Data.currentComparisonColumn "BillingCountry") (ScalarValue (String "USA"))
      let orderBy = OrderBy mempty $ Data.orderByColumn [] "BillingPostalCode" Descending :| [Data.orderByColumn [] "InvoiceId" Ascending]
      let aggregates = Data.mkFieldsMap [("max", SingleColumn $ SingleColumnAggregate Max (ColumnName "Total"))]
      let queryRequest = invoicesQueryRequest aggregates & qrQuery %~ (qLimit ?~ 20 >>> qWhere ?~ where' >>> qOrderBy ?~ orderBy)
      response <- (api // _query) sourceName config queryRequest

      let maxTotal =
            _tdInvoicesRows
              & filter ((^? Data.field "BillingCountry" . Data._ColumnFieldString) >>> (== Just "USA"))
              & sortOn (Down . (^? Data.field "BillingPostalCode"))
              & take 20
              & mapMaybe ((^? Data.field "Total" . Data._ColumnFieldNumber))
              & maximum

      let expectedAggregates = Data.mkFieldsMap [("max", Number maxTotal)]

      Data.responseAggregates response `jsonShouldBe` expectedAggregates
      Data.responseRows response `rowsShouldBe` []

    it "can get the min and max of a non-numeric comparable type such as a string" $ do
      let aggregates =
            Data.mkFieldsMap
              [ ("min", SingleColumn $ SingleColumnAggregate Min (ColumnName "Name")),
                ("max", SingleColumn $ SingleColumnAggregate Max (ColumnName "Name"))
              ]
      let queryRequest = artistsQueryRequest aggregates
      response <- (api // _query) sourceName config queryRequest

      let names = mapMaybe ((^? Data.field "Name" . Data._ColumnFieldString)) _tdArtistsRows
      let expectedAggregates =
            Data.mkFieldsMap
              [ ("min", aggregate (String . minimum) names),
                ("max", aggregate (String . maximum) names)
              ]

      Data.responseAggregates response `jsonShouldBe` expectedAggregates
      Data.responseRows response `rowsShouldBe` []

    it "aggregates over empty row lists results in nulls" $ do
      let where' = ApplyBinaryComparisonOperator LessThan (Data.currentComparisonColumn "ArtistId") (ScalarValue (Number 0))
      let aggregates = Data.mkFieldsMap [("min", SingleColumn $ SingleColumnAggregate Min (ColumnName "Name"))]
      let queryRequest = artistsQueryRequest aggregates & qrQuery . qWhere ?~ where'
      response <- (api // _query) sourceName config queryRequest

      let expectedAggregates = Data.mkFieldsMap [("min", Null)]

      Data.responseAggregates response `jsonShouldBe` expectedAggregates
      Data.responseRows response `rowsShouldBe` []

  describe "Multiple Aggregates and Returning Rows" $ do
    it "can get the max total from all rows, the count and the distinct count, simultaneously" $ do
      let aggregates =
            Data.mkFieldsMap
              [ ("count", StarCount),
                ("distinctBillingStates", ColumnCount $ ColumnCountAggregate (ColumnName "BillingState") True),
                ("maxTotal", SingleColumn $ SingleColumnAggregate Max (ColumnName "Total"))
              ]
      let queryRequest = invoicesQueryRequest aggregates
      response <- (api // _query) sourceName config queryRequest

      let invoiceCount = length _tdInvoicesRows
      let billingStateCount = length . HashSet.fromList $ mapMaybe ((^? Data.field "BillingState" . Data._ColumnFieldString)) _tdInvoicesRows
      let maxTotal = aggregate (Number . maximum) $ mapMaybe ((^? Data.field "Total" . Data._ColumnFieldNumber)) _tdInvoicesRows

      let expectedAggregates =
            Data.mkFieldsMap
              [ ("count", Number $ fromIntegral invoiceCount),
                ("distinctBillingStates", Number $ fromIntegral billingStateCount),
                ("maxTotal", maxTotal)
              ]

      Data.responseAggregates response `jsonShouldBe` expectedAggregates
      Data.responseRows response `rowsShouldBe` []

    it "can reuse the same aggregate twice" $ do
      let aggregates =
            Data.mkFieldsMap
              [ ("minInvoiceId", SingleColumn $ SingleColumnAggregate Min (ColumnName "InvoiceId")),
                ("minTotal", SingleColumn $ SingleColumnAggregate Min (ColumnName "Total"))
              ]
      let queryRequest = invoicesQueryRequest aggregates
      response <- (api // _query) sourceName config queryRequest

      let maxInvoiceId = aggregate (Number . minimum) $ mapMaybe ((^? Data.field "InvoiceId" . Data._ColumnFieldNumber)) _tdInvoicesRows
      let maxTotal = aggregate (Number . minimum) $ mapMaybe ((^? Data.field "Total" . Data._ColumnFieldNumber)) _tdInvoicesRows

      let expectedAggregates =
            Data.mkFieldsMap
              [ ("minInvoiceId", maxInvoiceId),
                ("minTotal", maxTotal)
              ]

      Data.responseAggregates response `jsonShouldBe` expectedAggregates
      Data.responseRows response `rowsShouldBe` []

    it "can also query for the rows involved in the aggregate" $ do
      let fields =
            Data.mkFieldsMap
              [ ("InvoiceId", Data.columnField "InvoiceId"),
                ("BillingCountry", Data.columnField "BillingCountry")
              ]
      let where' = ApplyBinaryComparisonOperator Equal (Data.currentComparisonColumn "BillingCountry") (ScalarValue (String "Canada"))
      let orderBy = OrderBy mempty $ Data.orderByColumn [] "BillingAddress" Ascending :| [Data.orderByColumn [] "InvoiceId" Ascending]
      let aggregates = Data.mkFieldsMap [("min", SingleColumn $ SingleColumnAggregate Min (ColumnName "Total"))]
      let queryRequest = invoicesQueryRequest aggregates & qrQuery %~ (qFields ?~ fields >>> qLimit ?~ 30 >>> qWhere ?~ where' >>> qOrderBy ?~ orderBy)
      response <- (api // _query) sourceName config queryRequest

      let invoiceRows =
            _tdInvoicesRows
              & filter ((^? Data.field "BillingCountry" . Data._ColumnFieldString) >>> (== Just "Canada"))
              & sortOn (^? Data.field "BillingAddress")
              & take 30

      let maxTotal =
            invoiceRows
              & mapMaybe ((^? Data.field "Total" . Data._ColumnFieldNumber))
              & aggregate (Number . minimum)

      let expectedAggregates = Data.mkFieldsMap [("min", maxTotal)]
      let expectedRows = Data.filterColumnsByQueryFields (_qrQuery queryRequest) <$> invoiceRows

      Data.responseRows response `rowsShouldBe` expectedRows
      Data.responseAggregates response `jsonShouldBe` expectedAggregates

  when (isJust relationshipCapabilities) $
    describe "Aggregates via Relationships" $ do
      it "can query aggregates via an array relationship" $ do
        let query = artistsWithAlbumsQuery id & qrQuery . qLimit ?~ 5
        receivedArtists <- (api // _query) sourceName config query

        let joinInAlbums (artist :: HashMap FieldName FieldValue) = fromMaybe artist $ do
              artistId <- artist ^? Data.field "ArtistId" . Data._ColumnFieldNumber
              let albums =
                    _tdAlbumsRows
                      & filter ((^? Data.field "ArtistId" . Data._ColumnFieldNumber) >>> (== Just artistId))
              let aggregates = Data.mkFieldsMap [("count", Number . fromIntegral $ length albums)]
              pure $ Data.insertField "Albums" (mkSubqueryResponse Nothing (Just aggregates)) artist

        let expectedArtists =
              _tdArtistsRows
                & take 5
                & fmap joinInAlbums

        Data.responseRows receivedArtists `rowsShouldBe` expectedArtists
        Data.responseAggregates receivedArtists `jsonShouldBe` mempty

      it "can query aggregates via an array relationship and include the rows in that relationship" $ do
        let albumFields =
              Data.mkFieldsMap
                [ ("AlbumId", Data.columnField "AlbumId"),
                  ("Title", Data.columnField "Title")
                ]
        let query = artistsWithAlbumsQuery (qFields ?~ albumFields) & qrQuery . qLimit ?~ 5
        receivedArtists <- (api // _query) sourceName config query

        let joinInAlbums (artist :: HashMap FieldName FieldValue) = fromMaybe artist $ do
              artistId <- artist ^? Data.field "ArtistId" . Data._ColumnFieldNumber
              let albums =
                    _tdAlbumsRows
                      & filter ((^? Data.field "ArtistId" . Data._ColumnFieldNumber) >>> (== Just artistId))
                      & Data.filterColumns ["AlbumId", "Title"]
              let aggregates = Data.mkFieldsMap [("count", Number . fromIntegral $ length albums)]
              pure $ Data.insertField "Albums" (mkSubqueryResponse (Just albums) (Just aggregates)) artist

        let expectedArtists =
              _tdArtistsRows
                & take 5
                & fmap joinInAlbums

        Data.responseRows receivedArtists `rowsShouldBe` expectedArtists
        Data.responseAggregates receivedArtists `jsonShouldBe` mempty

      it "can query with many nested relationships, with aggregates at multiple levels, with filtering, pagination and ordering" $ do
        receivedArtists <- (api // _query) sourceName config deeplyNestedArtistsQuery

        let joinInMediaType (track :: HashMap FieldName FieldValue) = fromMaybe track $ do
              mediaTypeId <- track ^? Data.field "MediaTypeId" . Data._ColumnFieldNumber
              let mediaTypes =
                    _tdMediaTypesRows
                      & filter ((^? Data.field "MediaTypeId" . Data._ColumnFieldNumber) >>> (== Just mediaTypeId))
                      & Data.filterColumns ["Name"]
              pure $ Data.insertField "nodes_MediaType" (mkSubqueryResponse (Just mediaTypes) Nothing) track

        let joinInInvoiceLines (track :: HashMap FieldName FieldValue) = fromMaybe track $ do
              trackId <- track ^? Data.field "TrackId" . Data._ColumnFieldNumber
              let invoiceLines =
                    _tdInvoiceLinesRows
                      & filter ((^? Data.field "TrackId" . Data._ColumnFieldNumber) >>> (== Just trackId))
              let getQuantity invoiceLine = invoiceLine ^? Data.field "Quantity" . Data._ColumnFieldNumber
              let invoiceLinesAggregates = Data.mkFieldsMap [("aggregate_sum_Quantity", aggregate (Number . sum) $ mapMaybe getQuantity invoiceLines)]
              pure $ Data.insertField "nodes_InvoiceLines_aggregate" (mkSubqueryResponse Nothing (Just invoiceLinesAggregates)) track

        let joinInTracks (album :: HashMap FieldName FieldValue) = fromMaybe album $ do
              albumId <- album ^? Data.field "AlbumId" . Data._ColumnFieldNumber
              let tracks =
                    _tdTracksRows
                      & filter
                        ( \track ->
                            track ^? Data.field "AlbumId" . Data._ColumnFieldNumber == Just albumId
                              && track ^? Data.field "Milliseconds" . Data._ColumnFieldNumber < Just 300000
                        )
                      & sortOn (Down . (^? Data.field "Name" . Data._ColumnFieldString))
                      & fmap (joinInMediaType >>> joinInInvoiceLines)
                      & Data.renameColumns [("Name", "nodes_Name")]
                      & Data.filterColumns ["nodes_Name", "nodes_MediaType", "nodes_InvoiceLines_aggregate"]
              let tracksAggregates = Data.mkFieldsMap [("aggregate_count", Number . fromIntegral $ length tracks)]
              pure $ Data.insertField "nodes_Tracks_aggregate" (mkSubqueryResponse (Just tracks) (Just tracksAggregates)) album

        let joinInAlbums (artist :: HashMap FieldName FieldValue) = fromMaybe artist $ do
              artistId <- artist ^? Data.field "ArtistId" . Data._ColumnFieldNumber
              let albums =
                    _tdAlbumsRows
                      & filter ((^? Data.field "ArtistId" . Data._ColumnFieldNumber) >>> (== Just artistId))
                      & fmap joinInTracks
                      & Data.renameColumns [("Title", "nodes_Title")]
                      & Data.filterColumns ["nodes_Title", "nodes_Tracks_aggregate"]
              pure $ Data.insertField "Albums_aggregate" (mkSubqueryResponse (Just albums) Nothing) artist

        let expectedArtists =
              _tdArtistsRows
                & sortOn (Down . (^? Data.field "Name"))
                & filter ((^? Data.field "Name" . Data._ColumnFieldString) >>> (\name -> name > Just "A" && name < Just "B"))
                & drop 1
                & take 3
                & fmap joinInAlbums
                & Data.filterColumns ["Name", "Albums_aggregate"]

        Data.responseRows receivedArtists `rowsShouldBe` expectedArtists
        Data.responseAggregates receivedArtists `jsonShouldBe` mempty
  where
    artistsWithAlbumsQuery :: (Query -> Query) -> QueryRequest
    artistsWithAlbumsQuery modifySubquery =
      let albumAggregates = Data.mkFieldsMap [("count", StarCount)]
          albumsSubquery = Data.emptyQuery & qAggregates ?~ albumAggregates & modifySubquery
          artistFields =
            Data.mkFieldsMap
              [ ("ArtistId", Data.columnField "ArtistId"),
                ("Name", Data.columnField "Name"),
                ("Albums", RelField $ RelationshipField _tdAlbumsRelationshipName albumsSubquery)
              ]
          artistOrderBy = OrderBy mempty $ Data.orderByColumn [] "ArtistId" Ascending :| []
          artistQuery = Data.emptyQuery & qFields ?~ artistFields & qOrderBy ?~ artistOrderBy
          artistsTableRelationships = Data.onlyKeepRelationships [_tdAlbumsRelationshipName] _tdArtistsTableRelationships
       in QueryRequest _tdArtistsTableName [artistsTableRelationships] artistQuery

    -- This query is basically what would be generated by this complex HGE GraphQL query
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
      let invoiceLinesAggregates = Data.mkFieldsMap [("aggregate_sum_Quantity", SingleColumn $ SingleColumnAggregate Sum (ColumnName "Quantity"))]
          invoiceLinesSubquery = Data.emptyQuery & qAggregates ?~ invoiceLinesAggregates
          mediaTypeFields = Data.mkFieldsMap [("Name", Data.columnField "Name")]
          mediaTypeSubquery = Data.emptyQuery & qFields ?~ mediaTypeFields
          tracksFields =
            Data.mkFieldsMap
              [ ("nodes_Name", Data.columnField "Name"),
                ("nodes_MediaType", RelField $ RelationshipField _tdMediaTypeRelationshipName mediaTypeSubquery),
                ("nodes_InvoiceLines_aggregate", RelField $ RelationshipField _tdInvoiceLinesRelationshipName invoiceLinesSubquery)
              ]
          tracksAggregates = Data.mkFieldsMap [("aggregate_count", StarCount)]
          tracksWhere = ApplyBinaryComparisonOperator LessThan (Data.currentComparisonColumn "Milliseconds") (ScalarValue $ Number 300000)
          tracksOrderBy = OrderBy mempty $ Data.orderByColumn [] "Name" Descending :| []
          tracksSubquery = Query (Just tracksFields) (Just tracksAggregates) Nothing Nothing (Just tracksWhere) (Just tracksOrderBy)
          albumsFields =
            Data.mkFieldsMap
              [ ("nodes_Title", Data.columnField "Title"),
                ("nodes_Tracks_aggregate", RelField $ RelationshipField _tdTracksRelationshipName tracksSubquery)
              ]
          albumsSubquery = Data.emptyQuery & qFields ?~ albumsFields
          artistFields =
            Data.mkFieldsMap
              [ ("Name", Data.columnField "Name"),
                ("Albums_aggregate", RelField $ RelationshipField _tdAlbumsRelationshipName albumsSubquery)
              ]
          artistWhere =
            And
              [ ApplyBinaryComparisonOperator GreaterThan (Data.currentComparisonColumn "Name") (ScalarValue $ String "A"),
                ApplyBinaryComparisonOperator LessThan (Data.currentComparisonColumn "Name") (ScalarValue $ String "B")
              ]
          artistOrderBy = OrderBy mempty $ Data.orderByColumn [] "Name" Descending :| []
          artistQuery = Query (Just artistFields) Nothing (Just 3) (Just 1) (Just artistWhere) (Just artistOrderBy)
       in QueryRequest
            _tdArtistsTableName
            [ Data.onlyKeepRelationships [_tdAlbumsRelationshipName] _tdArtistsTableRelationships,
              Data.onlyKeepRelationships [_tdTracksRelationshipName] _tdAlbumsTableRelationships,
              Data.onlyKeepRelationships [_tdInvoiceLinesRelationshipName, _tdMediaTypeRelationshipName] _tdTracksTableRelationships
            ]
            artistQuery

    artistsQueryRequest :: HashMap FieldName Aggregate -> QueryRequest
    artistsQueryRequest aggregates =
      let query = Data.emptyQuery & qAggregates ?~ aggregates
       in QueryRequest _tdArtistsTableName [] query

    invoicesQueryRequest :: HashMap FieldName Aggregate -> QueryRequest
    invoicesQueryRequest aggregates =
      let query = Data.emptyQuery & qAggregates ?~ aggregates
       in QueryRequest _tdInvoicesTableName [] query

    mkSubqueryResponse :: Maybe [HashMap FieldName FieldValue] -> Maybe (HashMap FieldName Value) -> FieldValue
    mkSubqueryResponse rows aggregates =
      mkRelationshipFieldValue $ QueryResponse rows aggregates

    aggregate :: (NonEmpty a -> Value) -> [a] -> Value
    aggregate aggFn values =
      maybe Null aggFn $ NonEmpty.nonEmpty values
