{-# LANGUAGE QuasiQuotes #-}

module Test.Specs.QuerySpec.AggregatesSpec (spec) where

import Control.Arrow ((>>>))
import Control.Lens (ix, (%~), (&), (.~), (?~), (^.), (^?), _Just)
import Control.Monad (when)
import Data.Aeson (Value (..))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe, isJust, mapMaybe)
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

spec :: TestData -> Maybe RelationshipCapabilities -> AgentDatasetTestSpec
spec TestData {..} relationshipCapabilities = describe "Aggregate Queries" $ do
  describe "Star Count" $ do
    it "counts all rows" $ do
      let aggregates = Data.mkFieldsMap [("count_all", StarCount)]
      let queryRequest = invoicesQueryRequest aggregates
      response <- queryGuarded queryRequest

      let invoiceCount = length _tdInvoicesRows
      let expectedAggregates = Data.mkFieldsMap [("count_all", Number $ fromIntegral invoiceCount)]

      Data.responseAggregates response `jsonShouldBe` expectedAggregates
      Data.responseRows response `rowsShouldBe` []

    it "counts all rows, after applying filters" $ do
      let where' = ApplyBinaryComparisonOperator Equal (_tdCurrentComparisonColumn "BillingCity" billingCityScalarType) (Data.scalarValueComparison (String "Oslo") billingCityScalarType)
      let aggregates = Data.mkFieldsMap [("count_all", StarCount)]
      let queryRequest = invoicesQueryRequest aggregates & qrQuery . qWhere ?~ where'
      response <- queryGuarded queryRequest

      let invoiceCount = length $ filter ((^? Data.field "BillingCity" . Data._ColumnFieldString) >>> (== Just "Oslo")) _tdInvoicesRows
      let expectedAggregates = Data.mkFieldsMap [("count_all", Number $ fromIntegral invoiceCount)]

      Data.responseAggregates response `jsonShouldBe` expectedAggregates
      Data.responseRows response `rowsShouldBe` []

    it "counts all rows, after applying pagination" $ do
      let offset = 400
      let aggregatesLimit = 20
      let aggregates = Data.mkFieldsMap [("count_all", StarCount)]
      let queryRequest = invoicesQueryRequest aggregates & qrQuery %~ (qAggregatesLimit ?~ aggregatesLimit >>> qOffset ?~ offset)
      response <- queryGuarded queryRequest

      let invoiceCount = length . take aggregatesLimit $ drop offset _tdInvoicesRows
      let expectedAggregates = Data.mkFieldsMap [("count_all", Number $ fromIntegral invoiceCount)]

      Data.responseAggregates response `jsonShouldBe` expectedAggregates
      Data.responseRows response `rowsShouldBe` []

    it "limit does not limit the count aggregation" $ do
      let limit = 20
      let aggregates = Data.mkFieldsMap [("count_all", StarCount)]
      let queryRequest = invoicesQueryRequest aggregates & qrQuery . qLimit ?~ limit
      response <- queryGuarded queryRequest

      let invoiceCount = length _tdInvoicesRows
      let expectedAggregates = Data.mkFieldsMap [("count_all", Number $ fromIntegral invoiceCount)]

      Data.responseAggregates response `jsonShouldBe` expectedAggregates
      Data.responseRows response `rowsShouldBe` []

  describe "Column Count" $ do
    it "counts all rows with non-null columns" $ do
      let aggregates = Data.mkFieldsMap [("count_cols", ColumnCount $ ColumnCountAggregate (_tdColumnName "BillingState") Nothing False)]
      let queryRequest = invoicesQueryRequest aggregates
      response <- queryGuarded queryRequest

      let invoiceCount = length $ filter ((^? Data.field "BillingState" . Data._ColumnFieldString) >>> (/= Nothing)) _tdInvoicesRows
      let expectedAggregates = Data.mkFieldsMap [("count_cols", Number $ fromIntegral invoiceCount)]

      Data.responseAggregates response `jsonShouldBe` expectedAggregates
      Data.responseRows response `rowsShouldBe` []

    it "can count all rows with non-null values in a column, after applying pagination and filtering" $ do
      let aggregatesLimit = 50
      let where' = ApplyBinaryComparisonOperator GreaterThanOrEqual (_tdCurrentComparisonColumn "InvoiceId" invoiceIdScalarType) (Data.scalarValueComparison (Number 380) invoiceIdScalarType)
      let aggregates = Data.mkFieldsMap [("count_cols", ColumnCount $ ColumnCountAggregate (_tdColumnName "BillingState") Nothing False)]
      let queryRequest = invoicesQueryRequest aggregates & qrQuery %~ (qAggregatesLimit ?~ aggregatesLimit >>> qWhere ?~ where')
      response <- queryGuarded queryRequest

      let invoiceCount =
            _tdInvoicesRows
              & filter ((^? Data.field "InvoiceId" . Data._ColumnFieldNumber) >>> (>= Just 380))
              & take aggregatesLimit
              & mapMaybe ((^? Data.field "BillingState" . Data._ColumnFieldString))
              & length

      let expectedAggregates = Data.mkFieldsMap [("count_cols", Number $ fromIntegral invoiceCount)]

      Data.responseAggregates response `jsonShouldBe` expectedAggregates
      Data.responseRows response `rowsShouldBe` []

    it "can count all rows with distinct non-null values in a column" $ do
      let aggregates = Data.mkFieldsMap [("count_cols", ColumnCount $ ColumnCountAggregate (_tdColumnName "BillingState") Nothing True)]
      let queryRequest = invoicesQueryRequest aggregates
      response <- queryGuarded queryRequest

      let billingStateCount = length . HashSet.fromList $ mapMaybe ((^? Data.field "BillingState" . Data._ColumnFieldString)) _tdInvoicesRows
      let expectedAggregates = Data.mkFieldsMap [("count_cols", Number $ fromIntegral billingStateCount)]

      Data.responseAggregates response `jsonShouldBe` expectedAggregates
      Data.responseRows response `rowsShouldBe` []

    it "can count all rows with distinct non-null values in a column, after applying pagination and filtering" $ do
      let aggregatesLimit = 20
      let where' = ApplyBinaryComparisonOperator GreaterThanOrEqual (_tdCurrentComparisonColumn "InvoiceId" invoiceIdScalarType) (Data.scalarValueComparison (Number 380) invoiceIdScalarType)
      -- It is important to add an explicit order by for this query as different database engines will order implicitly resulting in incorrect results
      let orderBy = OrderBy mempty $ _tdOrderByColumn [] "InvoiceId" Ascending :| []
      let aggregates = Data.mkFieldsMap [("count_cols", ColumnCount $ ColumnCountAggregate (_tdColumnName "BillingState") Nothing True)]
      let queryRequest = invoicesQueryRequest aggregates & qrQuery %~ (qAggregatesLimit ?~ aggregatesLimit >>> qWhere ?~ where' >>> qOrderBy ?~ orderBy)
      response <- queryGuarded queryRequest

      let billingStateCount =
            _tdInvoicesRows
              & filter ((^? Data.field "InvoiceId" . Data._ColumnFieldNumber) >>> (>= Just 380))
              & take aggregatesLimit
              & mapMaybe ((^? Data.field "BillingState" . Data._ColumnFieldString))
              & HashSet.fromList
              & length

      let expectedAggregates = Data.mkFieldsMap [("count_cols", Number $ fromIntegral billingStateCount)]

      Data.responseAggregates response `jsonShouldBe` expectedAggregates
      Data.responseRows response `rowsShouldBe` []

    it "limit does not limit the column count aggregation" $ do
      let limit = 50
      let aggregates = Data.mkFieldsMap [("count_cols", ColumnCount $ ColumnCountAggregate (_tdColumnName "BillingState") Nothing False)]
      let queryRequest = invoicesQueryRequest aggregates & qrQuery . qLimit ?~ limit
      response <- queryGuarded queryRequest

      let invoiceCount =
            _tdInvoicesRows
              & mapMaybe ((^? Data.field "BillingState" . Data._ColumnFieldString))
              & length

      let expectedAggregates = Data.mkFieldsMap [("count_cols", Number $ fromIntegral invoiceCount)]

      Data.responseAggregates response `jsonShouldBe` expectedAggregates
      Data.responseRows response `rowsShouldBe` []

  describe "Single Column Function" $ do
    it "can get the max total from all rows" $ do
      let aggregates = Data.mkFieldsMap [("max", singleColumnAggregateMax (_tdColumnName "Total") invoiceTotalScalarType)]
      let queryRequest = invoicesQueryRequest aggregates
      response <- queryGuarded queryRequest

      let maxTotal = maximum $ mapMaybe ((^? Data.field "Total" . Data._ColumnFieldNumber)) _tdInvoicesRows
      let expectedAggregates = Data.mkFieldsMap [("max", Number maxTotal)]

      Data.responseAggregates response `jsonShouldBe` expectedAggregates
      Data.responseRows response `rowsShouldBe` []

    it "can get the max total from all rows, after applying pagination, filtering and ordering" $ do
      let aggregatesLimit = 20
      let where' = ApplyBinaryComparisonOperator Equal (_tdCurrentComparisonColumn "BillingCountry" billingCountryScalarType) (Data.scalarValueComparison (String "USA") billingCountryScalarType)
      let orderBy = OrderBy mempty $ _tdOrderByColumn [] "BillingPostalCode" Descending :| [_tdOrderByColumn [] "InvoiceId" Ascending]
      let aggregates = Data.mkFieldsMap [("max", singleColumnAggregateMax (_tdColumnName "Total") invoiceTotalScalarType)]
      let queryRequest = invoicesQueryRequest aggregates & qrQuery %~ (qAggregatesLimit ?~ aggregatesLimit >>> qWhere ?~ where' >>> qOrderBy ?~ orderBy)
      response <- queryGuarded queryRequest

      let maxTotal =
            _tdInvoicesRows
              & filter ((^? Data.field "BillingCountry" . Data._ColumnFieldString) >>> (== Just "USA"))
              & sortOn (Down . (^? Data.field "BillingPostalCode"))
              & take aggregatesLimit
              & mapMaybe ((^? Data.field "Total" . Data._ColumnFieldNumber))
              & maximum

      let expectedAggregates = Data.mkFieldsMap [("max", Number maxTotal)]

      Data.responseAggregates response `jsonShouldBe` expectedAggregates
      Data.responseRows response `rowsShouldBe` []

    it "can get the min and max of a non-numeric comparable type such as a string" $ do
      let aggregates =
            Data.mkFieldsMap
              [ ("min", singleColumnAggregateMin (_tdColumnName "Name") artistNameScalarType),
                ("max", singleColumnAggregateMax (_tdColumnName "Name") artistNameScalarType)
              ]
      let queryRequest = artistsQueryRequest aggregates
      response <- queryGuarded queryRequest

      let names = mapMaybe ((^? Data.field "Name" . Data._ColumnFieldString)) _tdArtistsRows
      let expectedAggregates =
            Data.mkFieldsMap
              [ ("min", aggregate (String . minimum) names),
                ("max", aggregate (String . maximum) names)
              ]

      Data.responseAggregates response `jsonShouldBe` expectedAggregates
      Data.responseRows response `rowsShouldBe` []

    it "aggregates over empty row lists results in nulls" $ do
      let where' = ApplyBinaryComparisonOperator LessThan (_tdCurrentComparisonColumn "ArtistId" artistIdScalarType) (Data.scalarValueComparison (Number 0) artistIdScalarType)
      let aggregates = Data.mkFieldsMap [("min", singleColumnAggregateMin (_tdColumnName "Name") artistNameScalarType)]
      let queryRequest = artistsQueryRequest aggregates & qrQuery . qWhere ?~ where'
      response <- queryGuarded queryRequest

      let expectedAggregates = Data.mkFieldsMap [("min", Null)]

      Data.responseAggregates response `jsonShouldBe` expectedAggregates
      Data.responseRows response `rowsShouldBe` []

    it "limit does not limit the single column function aggregation" $ do
      let limit = 20
      let aggregates = Data.mkFieldsMap [("max", singleColumnAggregateMax (_tdColumnName "Total") invoiceTotalScalarType)]
      let queryRequest = invoicesQueryRequest aggregates & qrQuery . qLimit ?~ limit
      response <- queryGuarded queryRequest

      let maxTotal =
            _tdInvoicesRows
              & mapMaybe ((^? Data.field "Total" . Data._ColumnFieldNumber))
              & maximum

      let expectedAggregates = Data.mkFieldsMap [("max", Number maxTotal)]

      Data.responseAggregates response `jsonShouldBe` expectedAggregates
      Data.responseRows response `rowsShouldBe` []

  describe "Multiple Aggregates and Returning Rows" $ do
    it "can get the max total from all rows, the count and the distinct count, simultaneously" $ do
      let aggregates =
            Data.mkFieldsMap
              [ ("count", StarCount),
                ("distinctBillingStates", ColumnCount $ ColumnCountAggregate (_tdColumnName "BillingState") Nothing True),
                ("maxTotal", singleColumnAggregateMax (_tdColumnName "Total") invoiceTotalScalarType)
              ]
      let queryRequest = invoicesQueryRequest aggregates
      response <- queryGuarded queryRequest

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
              [ ("minInvoiceId", singleColumnAggregateMin (_tdColumnName "InvoiceId") invoiceIdScalarType),
                ("minTotal", singleColumnAggregateMin (_tdColumnName "Total") invoiceTotalScalarType)
              ]
      let queryRequest = invoicesQueryRequest aggregates
      response <- queryGuarded queryRequest

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
      let limit = 30
      let fields =
            Data.mkFieldsMap
              [ ("InvoiceId", _tdColumnField _tdInvoicesTableName "InvoiceId"),
                ("BillingCountry", _tdColumnField _tdInvoicesTableName "BillingCountry")
              ]
      let where' = ApplyBinaryComparisonOperator Equal (_tdCurrentComparisonColumn "BillingCountry" billingCountryScalarType) (Data.scalarValueComparison (String "Canada") billingCountryScalarType)
      let orderBy = OrderBy mempty $ _tdOrderByColumn [] "BillingAddress" Ascending :| [_tdOrderByColumn [] "InvoiceId" Ascending]
      let aggregates = Data.mkFieldsMap [("min", singleColumnAggregateMin (_tdColumnName "Total") invoiceTotalScalarType)]
      let queryRequest = invoicesQueryRequest aggregates & qrQuery %~ (qFields ?~ fields >>> qLimit ?~ limit >>> qAggregatesLimit ?~ limit >>> qWhere ?~ where' >>> qOrderBy ?~ orderBy)
      response <- queryGuarded queryRequest

      let invoiceRows =
            _tdInvoicesRows
              & filter ((^? Data.field "BillingCountry" . Data._ColumnFieldString) >>> (== Just "Canada"))
              & sortOn (^? Data.field "BillingAddress")
              & take limit

      let maxTotal =
            invoiceRows
              & mapMaybe ((^? Data.field "Total" . Data._ColumnFieldNumber))
              & aggregate (Number . minimum)

      let expectedAggregates = Data.mkFieldsMap [("min", maxTotal)]
      let expectedRows = Data.filterColumnsByQueryFields (queryRequest ^. qrQuery) <$> invoiceRows

      Data.responseRows response `rowsShouldBe` expectedRows
      Data.responseAggregates response `jsonShouldBe` expectedAggregates

    it "limit limits the number of returned rows but not the rows considered by the aggregate function" $ do
      let limit = 20
      let fields = Data.mkFieldsMap [("InvoiceId", _tdColumnField _tdInvoicesTableName "InvoiceId")]
      let orderBy = OrderBy mempty $ _tdOrderByColumn [] "InvoiceId" Ascending :| []
      let aggregates = Data.mkFieldsMap [("count_all", StarCount)]
      let queryRequest = invoicesQueryRequest aggregates & qrQuery %~ (qFields ?~ fields >>> qLimit ?~ limit >>> qOrderBy ?~ orderBy)
      response <- queryGuarded queryRequest

      let invoiceCount = length _tdInvoicesRows
      let expectedAggregates = Data.mkFieldsMap [("count_all", Number $ fromIntegral invoiceCount)]
      let expectedRows = take limit $ Data.filterColumnsByQueryFields (queryRequest ^. qrQuery) <$> _tdInvoicesRows

      Data.responseAggregates response `jsonShouldBe` expectedAggregates
      Data.responseRows response `rowsShouldBe` expectedRows

    it "aggregates limit is applied separately to row limit" $ do
      let aggregatesLimit = 30
      let limit = 20
      let fields =
            Data.mkFieldsMap
              [ ("InvoiceId", _tdColumnField _tdInvoicesTableName "InvoiceId"),
                ("BillingCountry", _tdColumnField _tdInvoicesTableName "BillingCountry")
              ]
      let where' = ApplyBinaryComparisonOperator Equal (_tdCurrentComparisonColumn "BillingCountry" billingCountryScalarType) (Data.scalarValueComparison (String "Canada") billingCountryScalarType)
      let orderBy = OrderBy mempty $ _tdOrderByColumn [] "BillingAddress" Ascending :| [_tdOrderByColumn [] "InvoiceId" Ascending]
      let aggregates = Data.mkFieldsMap [("min", singleColumnAggregateMin (_tdColumnName "Total") invoiceTotalScalarType)]
      let queryRequest = invoicesQueryRequest aggregates & qrQuery %~ (qFields ?~ fields >>> qAggregatesLimit ?~ aggregatesLimit >>> qLimit ?~ limit >>> qWhere ?~ where' >>> qOrderBy ?~ orderBy)
      response <- queryGuarded queryRequest

      let aggregateLimitedInvoiceRows =
            _tdInvoicesRows
              & filter ((^? Data.field "BillingCountry" . Data._ColumnFieldString) >>> (== Just "Canada"))
              & sortOn (^? Data.field "BillingAddress")
              & take aggregatesLimit

      -- Limit is smaller than aggregatesLimit, so we can just take from the aggregateLimitedInvoiceRows
      let invoiceRows = take limit aggregateLimitedInvoiceRows

      let maxTotal =
            aggregateLimitedInvoiceRows
              & take limit
              & mapMaybe ((^? Data.field "Total" . Data._ColumnFieldNumber))
              & aggregate (Number . minimum)

      let expectedAggregates = Data.mkFieldsMap [("min", maxTotal)]
      let expectedRows = Data.filterColumnsByQueryFields (queryRequest ^. qrQuery) <$> invoiceRows

      Data.responseRows response `rowsShouldBe` expectedRows
      Data.responseAggregates response `jsonShouldBe` expectedAggregates

  when (isJust relationshipCapabilities) $
    describe "Aggregates via Relationships" $ do
      it "can query aggregates via an array relationship" $ do
        let limit = 5
        let query = artistsWithAlbumsQuery id & qrQuery . qLimit ?~ limit
        receivedArtists <- queryGuarded query

        let joinInAlbums (artist :: HashMap FieldName FieldValue) = fromMaybe artist $ do
              artistId <- artist ^? Data.field "ArtistId" . Data._ColumnFieldNumber
              let albums =
                    _tdAlbumsRows
                      & filter ((^? Data.field "ArtistId" . Data._ColumnFieldNumber) >>> (== Just artistId))
              let aggregates = Data.mkFieldsMap [("count", Number . fromIntegral $ length albums)]
              pure $ Data.insertField "Albums" (Data.mkSubqueryFieldValue Nothing (Just aggregates)) artist

        let expectedArtists =
              _tdArtistsRows
                & take limit
                & fmap joinInAlbums

        Data.responseRows receivedArtists `rowsShouldBe` expectedArtists
        Data.responseAggregates receivedArtists `jsonShouldBe` mempty

      it "can query aggregates via an array relationship and include the rows in that relationship" $ do
        let limit = 5
        let albumFields =
              Data.mkFieldsMap
                [ ("AlbumId", _tdColumnField _tdAlbumsTableName "AlbumId"),
                  ("Title", _tdColumnField _tdAlbumsTableName "Title")
                ]
        let query = artistsWithAlbumsQuery (qFields ?~ albumFields) & qrQuery . qLimit ?~ limit
        receivedArtists <- queryGuarded query

        let joinInAlbums (artist :: HashMap FieldName FieldValue) = fromMaybe artist $ do
              artistId <- artist ^? Data.field "ArtistId" . Data._ColumnFieldNumber
              let albums =
                    _tdAlbumsRows
                      & filter ((^? Data.field "ArtistId" . Data._ColumnFieldNumber) >>> (== Just artistId))
                      & Data.filterColumns ["AlbumId", "Title"]
              let aggregates = Data.mkFieldsMap [("count", Number . fromIntegral $ length albums)]
              pure $ Data.insertField "Albums" (Data.mkSubqueryFieldValue (Just albums) (Just aggregates)) artist

        let sortAlbums (artistRows :: [HashMap FieldName FieldValue]) =
              artistRows & traverse . Data.field "Albums" . Data._RelationshipFieldRows %~ sortOn (^? Data.field "AlbumId")

        let expectedArtists =
              _tdArtistsRows
                & take limit
                & fmap joinInAlbums
                & sortAlbums

        -- Ignore the sort order of the related albums by sorting them in the response and the expected data
        let receivedArtistRows = sortAlbums $ Data.responseRows receivedArtists

        receivedArtistRows `rowsShouldBe` expectedArtists
        Data.responseAggregates receivedArtists `jsonShouldBe` mempty

      it "can query with many nested relationships, with aggregates at multiple levels, with filtering, pagination and ordering" $ do
        receivedArtists <- queryGuarded deeplyNestedArtistsQuery

        let joinInMediaType (track :: HashMap FieldName FieldValue) = fromMaybe track $ do
              mediaTypeId <- track ^? Data.field "MediaTypeId" . Data._ColumnFieldNumber
              let mediaTypes =
                    _tdMediaTypesRows
                      & filter ((^? Data.field "MediaTypeId" . Data._ColumnFieldNumber) >>> (== Just mediaTypeId))
                      & Data.filterColumns ["Name"]
              pure $ Data.insertField "nodes_MediaType" (Data.mkSubqueryFieldValue (Just mediaTypes) Nothing) track

        let joinInInvoiceLines (track :: HashMap FieldName FieldValue) = fromMaybe track $ do
              trackId <- track ^? Data.field "TrackId" . Data._ColumnFieldNumber
              let invoiceLines =
                    _tdInvoiceLinesRows
                      & filter ((^? Data.field "TrackId" . Data._ColumnFieldNumber) >>> (== Just trackId))
              let getQuantity invoiceLine = invoiceLine ^? Data.field "Quantity" . Data._ColumnFieldNumber
              let invoiceLinesAggregates = Data.mkFieldsMap [("aggregate_sum_Quantity", aggregate (Number . sum) $ mapMaybe getQuantity invoiceLines)]
              pure $ Data.insertField "nodes_InvoiceLines_aggregate" (Data.mkSubqueryFieldValue Nothing (Just invoiceLinesAggregates)) track

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
              pure $ Data.insertField "nodes_Tracks_aggregate" (Data.mkSubqueryFieldValue (Just tracks) (Just tracksAggregates)) album

        let joinInAlbums (artist :: HashMap FieldName FieldValue) = fromMaybe artist $ do
              artistId <- artist ^? Data.field "ArtistId" . Data._ColumnFieldNumber
              let albums =
                    _tdAlbumsRows
                      & filter ((^? Data.field "ArtistId" . Data._ColumnFieldNumber) >>> (== Just artistId))
                      & sortOn (^? Data.field "Title" . Data._ColumnFieldString)
                      & fmap joinInTracks
                      & Data.renameColumns [("Title", "nodes_Title")]
                      & Data.filterColumns ["nodes_Title", "nodes_Tracks_aggregate"]
              pure $ Data.insertField "Albums_aggregate" (Data.mkSubqueryFieldValue (Just albums) Nothing) artist

        let expectedArtists =
              _tdArtistsRows
                & sortOn (Down . (^? Data.field "Name"))
                & filter ((^? Data.field "Name" . Data._ColumnFieldString) >>> (\name -> name > Just "A" && name < Just "B"))
                & drop 1
                & take 3
                & fmap joinInAlbums
                & Data.filterColumns ["Name", "Albums_aggregate"]

        let receivedArtistRows = Data.responseRows receivedArtists

        receivedArtistRows `rowsShouldBe` expectedArtists
        Data.responseAggregates receivedArtists `jsonShouldBe` mempty

  describe "Aggregates over ordered and paginated tables" $ do
    it "orders by a column" $ do
      let offset = 2
      let aggregatesLimit = 5
      let orderBy = OrderBy mempty $ _tdOrderByColumn [] "Title" Ascending :| []
      let aggregates = Data.mkFieldsMap [("max", singleColumnAggregateMax (_tdColumnName "Title") albumTitleScalarType)]
      let queryRequest =
            albumsQueryRequest
              & qrQuery
                %~ ( qAggregates ?~ aggregates
                       >>> qOrderBy ?~ orderBy
                       >>> qOffset ?~ offset
                       >>> qAggregatesLimit ?~ aggregatesLimit
                   )
      response <- queryGuarded queryRequest

      let names =
            _tdAlbumsRows
              & sortOn ((^? Data.field "Title"))
              & drop offset
              & take aggregatesLimit
              & mapMaybe (^? Data.field "Title" . Data._ColumnFieldString)

      let expectedAggregates = Data.mkFieldsMap [("max", aggregate (String . maximum) names)]

      Data.responseAggregates response `jsonShouldBe` expectedAggregates
      Data.responseRows response `rowsShouldBe` []

    when (isJust relationshipCapabilities) . describe "involving related tables in the ordering" $ do
      it "orders by a column" $ do
        let offset = 10
        let aggregatesLimit = 50
        let orderByRelations = HashMap.fromList [(_tdArtistRelationshipName, OrderByRelation Nothing mempty)]
        let orderBy =
              OrderBy orderByRelations $
                NonEmpty.fromList
                  [ _tdOrderByColumn [_tdArtistRelationshipName] "Name" Ascending,
                    _tdOrderByColumn [] "AlbumId" Ascending
                  ]
        let aggregates = Data.mkFieldsMap [("max", singleColumnAggregateMax (_tdColumnName "Title") albumTitleScalarType)]
        let queryRequest =
              albumsQueryRequest
                & qrRelationships .~ Set.fromList [API.RTable $ Data.onlyKeepRelationships [_tdArtistRelationshipName] _tdAlbumsTableRelationships]
                & qrQuery
                  %~ ( qAggregates ?~ aggregates
                         >>> qOrderBy ?~ orderBy
                         >>> qOffset ?~ offset
                         >>> qAggregatesLimit ?~ aggregatesLimit
                     )
        response <- queryGuarded queryRequest

        let getRelatedArtist (album :: HashMap FieldName FieldValue) =
              (album ^? Data.field "ArtistId" . Data._ColumnFieldNumber) >>= \artistId -> _tdArtistsRowsById ^? ix artistId

        let names =
              _tdAlbumsRows
                & sortOn (\album -> getRelatedArtist album ^? _Just . Data.field "Name")
                & drop offset
                & take aggregatesLimit
                & mapMaybe (^? Data.field "Title" . Data._ColumnFieldString)

        let expectedAggregates = Data.mkFieldsMap [("max", aggregate (String . maximum) names)]

        Data.responseAggregates response `jsonShouldBe` expectedAggregates
        Data.responseRows response `rowsShouldBe` []

      it "orders by an aggregate" $ do
        let offset = 15
        let aggregatesLimit = 10
        let orderByRelations = HashMap.fromList [(_tdTracksRelationshipName, OrderByRelation Nothing mempty)]
        let orderBy =
              OrderBy orderByRelations $
                NonEmpty.fromList
                  [ OrderByElement [_tdTracksRelationshipName] OrderByStarCountAggregate Descending,
                    _tdOrderByColumn [] "Title" Descending
                  ]
        let aggregates = Data.mkFieldsMap [("max", singleColumnAggregateMax (_tdColumnName "Title") albumTitleScalarType)]
        let queryRequest =
              albumsQueryRequest
                & qrRelationships .~ Set.fromList [API.RTable $ Data.onlyKeepRelationships [_tdTracksRelationshipName] _tdAlbumsTableRelationships]
                & qrQuery
                  %~ ( qAggregates ?~ aggregates
                         >>> qOrderBy ?~ orderBy
                         >>> qOffset ?~ offset
                         >>> qAggregatesLimit ?~ aggregatesLimit
                     )
        response <- queryGuarded queryRequest

        let getRelatedTracksCount (album :: HashMap FieldName FieldValue) = fromMaybe 0 $ do
              albumId <- (album ^? Data.field "AlbumId" . Data._ColumnFieldNumber)
              _tdTracksRows & filter (\track -> track ^? Data.field "AlbumId" . Data._ColumnFieldNumber == Just albumId) & length & pure

        let names =
              _tdAlbumsRows
                & sortOn (\album -> (Down $ getRelatedTracksCount album, Down $ album ^? Data.field "Title" . Data._ColumnFieldString))
                & drop offset
                & take aggregatesLimit
                & mapMaybe (^? Data.field "Title" . Data._ColumnFieldString)

        let expectedAggregates = Data.mkFieldsMap [("max", aggregate (String . maximum) names)]

        Data.responseAggregates response `jsonShouldBe` expectedAggregates
        Data.responseRows response `rowsShouldBe` []
  where
    artistsWithAlbumsQuery :: (Query -> Query) -> QueryRequest
    artistsWithAlbumsQuery modifySubquery =
      let albumAggregates = Data.mkFieldsMap [("count", StarCount)]
          albumsSubquery = Data.emptyQuery & qAggregates ?~ albumAggregates & modifySubquery
          artistFields =
            Data.mkFieldsMap
              [ ("ArtistId", _tdColumnField _tdArtistsTableName "ArtistId"),
                ("Name", _tdColumnField _tdArtistsTableName "Name"),
                ("Albums", RelField $ RelationshipField _tdAlbumsRelationshipName albumsSubquery)
              ]
          artistOrderBy = OrderBy mempty $ _tdOrderByColumn [] "ArtistId" Ascending :| []
          artistQuery = Data.emptyQuery & qFields ?~ artistFields & qOrderBy ?~ artistOrderBy
          artistsTableRelationships = Data.onlyKeepRelationships [_tdAlbumsRelationshipName] _tdArtistsTableRelationships
       in TableQueryRequest _tdArtistsTableName (Set.fromList [API.RTable artistsTableRelationships]) mempty mempty artistQuery Nothing

    -- This query is basically what would be generated by this complex HGE GraphQL query
    -- @
    --   query {
    --     Artist(where: {_and: [{Name: {_gt: "A"}}, {Name: {_lt: "B"}}]}, limit: 3, offset: 1, order_by: {Name: desc}) {
    --       Name
    --       Albums_aggregate(order_by: {Title: asc}) {
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
      let invoiceLinesAggregates = Data.mkFieldsMap [("aggregate_sum_Quantity", singleColumnAggregateSum (_tdColumnName "Quantity") invoiceLineQuantityScalarType)]
          invoiceLinesSubquery = Data.emptyQuery & qAggregates ?~ invoiceLinesAggregates
          mediaTypeFields = Data.mkFieldsMap [("Name", _tdColumnField _tdMediaTypesTableName "Name")]
          mediaTypeSubquery = Data.emptyQuery & qFields ?~ mediaTypeFields
          tracksFields =
            Data.mkFieldsMap
              [ ("nodes_Name", _tdColumnField _tdTracksTableName "Name"),
                ("nodes_MediaType", RelField $ RelationshipField _tdMediaTypeRelationshipName mediaTypeSubquery),
                ("nodes_InvoiceLines_aggregate", RelField $ RelationshipField _tdInvoiceLinesRelationshipName invoiceLinesSubquery)
              ]
          tracksAggregates = Data.mkFieldsMap [("aggregate_count", StarCount)]
          tracksWhere = ApplyBinaryComparisonOperator LessThan (_tdCurrentComparisonColumn "Milliseconds" millisecondsScalarType) (Data.scalarValueComparison (Number 300000) millisecondsScalarType)
          tracksOrderBy = OrderBy mempty $ _tdOrderByColumn [] "Name" Descending :| []
          tracksSubquery = Query (Just tracksFields) (Just tracksAggregates) Nothing Nothing Nothing (Just tracksWhere) (Just tracksOrderBy)
          albumsFields =
            Data.mkFieldsMap
              [ ("nodes_Title", _tdColumnField _tdAlbumsTableName "Title"),
                ("nodes_Tracks_aggregate", RelField $ RelationshipField _tdTracksRelationshipName tracksSubquery)
              ]
          albumsOrderBy = OrderBy mempty $ _tdOrderByColumn [] "Title" Ascending :| []
          albumsSubquery = Data.emptyQuery & qFields ?~ albumsFields & qOrderBy ?~ albumsOrderBy
          artistFields =
            Data.mkFieldsMap
              [ ("Name", _tdColumnField _tdArtistsTableName "Name"),
                ("Albums_aggregate", RelField $ RelationshipField _tdAlbumsRelationshipName albumsSubquery)
              ]
          artistWhere =
            Data.mkAndExpr
              [ ApplyBinaryComparisonOperator GreaterThan (_tdCurrentComparisonColumn "Name" artistNameScalarType) (Data.scalarValueComparison (String "A") artistNameScalarType),
                ApplyBinaryComparisonOperator LessThan (_tdCurrentComparisonColumn "Name" artistNameScalarType) (Data.scalarValueComparison (String "B") artistNameScalarType)
              ]
          artistOrderBy = OrderBy mempty $ _tdOrderByColumn [] "Name" Descending :| []
          artistQuery = Query (Just artistFields) Nothing Nothing (Just 3) (Just 1) (Just artistWhere) (Just artistOrderBy)
       in TableQueryRequest
            _tdArtistsTableName
            ( Set.fromList
                [ API.RTable $ Data.onlyKeepRelationships [_tdAlbumsRelationshipName] _tdArtistsTableRelationships,
                  API.RTable $ Data.onlyKeepRelationships [_tdTracksRelationshipName] _tdAlbumsTableRelationships,
                  API.RTable $ Data.onlyKeepRelationships [_tdInvoiceLinesRelationshipName, _tdMediaTypeRelationshipName] _tdTracksTableRelationships
                ]
            )
            mempty
            mempty
            artistQuery
            Nothing

    artistsQueryRequest :: HashMap FieldName Aggregate -> QueryRequest
    artistsQueryRequest aggregates =
      let query = Data.emptyQuery & qAggregates ?~ aggregates
       in TableQueryRequest _tdArtistsTableName mempty mempty mempty query Nothing

    invoicesQueryRequest :: HashMap FieldName Aggregate -> QueryRequest
    invoicesQueryRequest aggregates =
      let query = Data.emptyQuery & qAggregates ?~ aggregates
       in TableQueryRequest _tdInvoicesTableName mempty mempty mempty query Nothing

    albumsQueryRequest :: QueryRequest
    albumsQueryRequest =
      TableQueryRequest _tdAlbumsTableName mempty mempty mempty Data.emptyQuery Nothing

    aggregate :: (NonEmpty a -> Value) -> [a] -> Value
    aggregate aggFn values =
      maybe Null aggFn $ NonEmpty.nonEmpty values

    singleColumnAggregateMax :: ColumnName -> ScalarType -> Aggregate
    singleColumnAggregateMax columnName resultType = SingleColumn $ SingleColumnAggregate (SingleColumnAggregateFunction [G.name|max|]) columnName Nothing resultType

    singleColumnAggregateMin :: ColumnName -> ScalarType -> Aggregate
    singleColumnAggregateMin columnName resultType = SingleColumn $ SingleColumnAggregate (SingleColumnAggregateFunction [G.name|min|]) columnName Nothing resultType

    singleColumnAggregateSum :: ColumnName -> ScalarType -> Aggregate
    singleColumnAggregateSum columnName resultType = SingleColumn $ SingleColumnAggregate (SingleColumnAggregateFunction [G.name|sum|]) columnName Nothing resultType

    billingCityScalarType = _tdFindColumnScalarType _tdInvoicesTableName "BillingCity"
    billingCountryScalarType = _tdFindColumnScalarType _tdInvoicesTableName "BillingCountry"
    invoiceIdScalarType = _tdFindColumnScalarType _tdInvoicesTableName "InvoiceId"
    invoiceTotalScalarType = _tdFindColumnScalarType _tdInvoicesTableName "Total"
    invoiceLineQuantityScalarType = _tdFindColumnScalarType _tdInvoiceLinesTableName "Quantity"
    artistIdScalarType = _tdFindColumnScalarType _tdArtistsTableName "ArtistId"
    artistNameScalarType = _tdFindColumnScalarType _tdArtistsTableName "Name"
    albumTitleScalarType = _tdFindColumnScalarType _tdAlbumsTableName "Title"
    millisecondsScalarType = _tdFindColumnScalarType _tdTracksTableName "Milliseconds"
