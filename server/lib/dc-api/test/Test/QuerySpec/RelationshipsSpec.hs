module Test.QuerySpec.RelationshipsSpec (spec) where

import Control.Arrow ((>>>))
import Control.Lens (Traversal', ix, (&), (?~), (^.), (^..), (^?), _Just)
import Control.Monad (when)
import Data.HashMap.Strict (HashMap)
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (maybeToList)
import Hasura.Backends.DataConnector.API
import Servant.API (NamedRoutes)
import Servant.Client (Client, (//))
import Test.Data (TestData (..))
import Test.Data qualified as Data
import Test.Expectations (jsonShouldBe, rowsShouldBe)
import Test.Hspec (Spec, describe, it)
import Prelude

spec :: TestData -> Client IO (NamedRoutes Routes) -> SourceName -> Config -> Maybe SubqueryComparisonCapabilities -> Spec
spec TestData {..} api sourceName config subqueryComparisonCapabilities = describe "Relationship Queries" $ do
  it "perform an object relationship query by joining artist to albums" $ do
    let query = albumsWithArtistQuery id
    receivedAlbums <- Data.sortResponseRowsBy "AlbumId" <$> (api // _query) sourceName config query

    let joinInArtist (album :: HashMap FieldName FieldValue) =
          let artist = (album ^? Data.field "ArtistId" . Data._ColumnFieldNumber) >>= \artistId -> _tdArtistsRowsById ^? ix artistId
              artistPropVal = maybeToList artist
           in Data.insertField "Artist" (mkSubqueryResponse artistPropVal) album
    let removeArtistId = Data.deleteField "ArtistId"

    let expectedAlbums = (removeArtistId . joinInArtist) <$> _tdAlbumsRows
    Data.responseRows receivedAlbums `rowsShouldBe` expectedAlbums
    _qrAggregates receivedAlbums `jsonShouldBe` Nothing

  it "perform an array relationship query by joining albums to artists" $ do
    let query = artistsWithAlbumsQuery id
    receivedArtists <- Data.sortResponseRowsBy "ArtistId" <$> (api // _query) sourceName config query

    let joinInAlbums (artist :: HashMap FieldName FieldValue) =
          let artistId = artist ^? Data.field "ArtistId" . Data._ColumnFieldNumber
              albumFilter artistId' album = album ^? Data.field "ArtistId" . Data._ColumnFieldNumber == Just artistId'
              albums = maybe [] (\artistId' -> filter (albumFilter artistId') _tdAlbumsRows) artistId
              albums' = Data.deleteField "ArtistId" <$> albums
           in Data.insertField "Albums" (mkSubqueryResponse albums') artist

    let expectedAlbums = joinInAlbums <$> _tdArtistsRows
    Data.responseRows receivedArtists `rowsShouldBe` expectedAlbums
    _qrAggregates receivedArtists `jsonShouldBe` Nothing

  it "perform an array relationship query by joining albums to artists with pagination of albums" $ do
    let albumsOrdering = OrderBy mempty $ NonEmpty.fromList [OrderByElement [] (OrderByColumn $ ColumnName "AlbumId") Ascending]
    let query = artistsWithAlbumsQuery (qOffset ?~ 1 >>> qLimit ?~ 2 >>> qOrderBy ?~ albumsOrdering)
    receivedArtists <- Data.sortResponseRowsBy "ArtistId" <$> (api // _query) sourceName config query

    let joinInAlbums (artist :: HashMap FieldName FieldValue) = do
          let artistId = artist ^? Data.field "ArtistId" . Data._ColumnFieldNumber
              albumFilter artistId' album = album ^? Data.field "ArtistId" . Data._ColumnFieldNumber == Just artistId'
              albums = maybe [] (\artistId' -> filter (albumFilter artistId') _tdAlbumsRows) artistId
              paginatedAlbums = albums & sortOn (^? Data.field "ArtistId") & drop 1 & take 2
              paginatedAlbums' = Data.deleteField "ArtistId" <$> paginatedAlbums
           in Data.insertField "Albums" (mkSubqueryResponse paginatedAlbums') artist

    let expectedAlbums = joinInAlbums <$> _tdArtistsRows
    Data.responseRows receivedArtists `rowsShouldBe` expectedAlbums
    _qrAggregates receivedArtists `jsonShouldBe` Nothing

  when ((_ctccSupportsRelations <$> subqueryComparisonCapabilities) == Just True) $
    describe "Cross related table comparisons" $ do
      it "perform an object relationship query by joining employee to customers and filter comparing columns across the object relationship" $ do
        -- Join Employee to Customers via SupportRep, and only get those customers that have a rep
        -- that is in the same country as the customer
        -- This sort of thing would come from a permissions filter on Customer that looks like:
        -- { SupportRep: { Country: { _ceq: [ "$", "Country" ] } } }
        let where' =
              Exists (RelatedTable _tdSupportRepRelationshipName) $
                ApplyBinaryComparisonOperator
                  Equal
                  (Data.currentComparisonColumn "Country")
                  (AnotherColumn (Data.queryComparisonColumn "Country"))
        let query = customersWithSupportRepQuery id & qrQuery . qWhere ?~ where'
        receivedCustomers <- Data.sortResponseRowsBy "CustomerId" <$> (api // _query) sourceName config query

        let joinInSupportRep (customer :: HashMap FieldName FieldValue) =
              let supportRep = (customer ^? Data.field "SupportRepId" . Data._ColumnFieldNumber) >>= \employeeId -> _tdEmployeesRowsById ^? ix employeeId
                  supportRepPropVal = maybeToList $ Data.filterColumnsByQueryFields employeesQuery <$> supportRep
               in Data.insertField "SupportRep" (mkSubqueryResponse supportRepPropVal) customer

        let filterCustomersBySupportRepCountry (customer :: HashMap FieldName FieldValue) =
              let customerCountry = customer ^? Data.field "Country" . Data._ColumnFieldString
                  supportRepCountry = customer ^.. Data.field "SupportRep" . subqueryRows . Data.field "Country" . Data._ColumnFieldString
               in maybe False (`elem` supportRepCountry) customerCountry

        let expectedCustomers = filter filterCustomersBySupportRepCountry $ Data.filterColumnsByQueryFields (query ^. qrQuery) . joinInSupportRep <$> _tdCustomersRows
        Data.responseRows receivedCustomers `rowsShouldBe` expectedCustomers
        _qrAggregates receivedCustomers `jsonShouldBe` Nothing

      it "perform an array relationship query by joining customers to employees and filter comparing columns across the array relationship" $ do
        -- Join Customers to Employees via SupportRepForCustomers, and only get those employees that are reps for
        -- customers that are in the same country as the employee
        -- This sort of thing would come from a permissions filter on Employees that looks like:
        -- { SupportRepForCustomers: { Country: { _ceq: [ "$", "Country" ] } } }
        let where' =
              Exists (RelatedTable _tdSupportRepForCustomersRelationshipName) $
                ApplyBinaryComparisonOperator
                  Equal
                  (Data.currentComparisonColumn "Country")
                  (AnotherColumn (Data.queryComparisonColumn "Country"))
        let query = employeesWithCustomersQuery id & qrQuery . qWhere ?~ where'
        receivedEmployees <- Data.sortResponseRowsBy "EmployeeId" <$> (api // _query) sourceName config query

        let joinInCustomers (employee :: HashMap FieldName FieldValue) =
              let employeeId = employee ^? Data.field "EmployeeId" . Data._ColumnFieldNumber
                  customerFilter employeeId' customer = customer ^? Data.field "SupportRepId" . Data._ColumnFieldNumber == Just employeeId'
                  customers = maybe [] (\employeeId' -> filter (customerFilter employeeId') _tdCustomersRows) employeeId
                  customers' = Data.filterColumnsByQueryFields customersQuery <$> customers
               in Data.insertField "SupportRepForCustomers" (mkSubqueryResponse customers') employee

        let filterEmployeesByCustomerCountry (employee :: HashMap FieldName FieldValue) =
              let employeeCountry = employee ^? Data.field "Country" . Data._ColumnFieldString
                  customerCountries = employee ^.. Data.field "SupportRepForCustomers" . subqueryRows . Data.field "Country" . Data._ColumnFieldString
               in maybe False (`elem` customerCountries) employeeCountry

        let expectedEmployees = filter filterEmployeesByCustomerCountry $ Data.filterColumnsByQueryFields (query ^. qrQuery) . joinInCustomers <$> _tdEmployeesRows
        Data.responseRows receivedEmployees `rowsShouldBe` expectedEmployees
        _qrAggregates receivedEmployees `jsonShouldBe` Nothing

      it "perform an object relationship query by joining employee to customers but filter employees by comparing columns on the employee" $ do
        -- Join Employee to Customers via SupportRep, and only get those customers that have a rep
        -- However, the Employee table is filtered with a permission rule that compares columns on that table.
        -- This Employee table permissions filter would look like:
        -- { FirstName: { _cgt: ["LastName"] } }
        let customersWhere =
              Exists (RelatedTable _tdSupportRepRelationshipName) $
                And
                  [ ( ApplyBinaryComparisonOperator
                        GreaterThan
                        (Data.currentComparisonColumn "FirstName")
                        (AnotherColumn (Data.currentComparisonColumn "LastName"))
                    ),
                    (Not (ApplyUnaryComparisonOperator IsNull (Data.currentComparisonColumn "EmployeeId")))
                  ]

        let employeesWhere =
              ApplyBinaryComparisonOperator
                GreaterThan
                (Data.currentComparisonColumn "FirstName")
                (AnotherColumn (Data.currentComparisonColumn "LastName"))

        let query = customersWithSupportRepQuery (\q -> q & qWhere ?~ employeesWhere) & qrQuery . qWhere ?~ customersWhere
        receivedCustomers <- Data.sortResponseRowsBy "CustomerId" <$> (api // _query) sourceName config query

        let joinInSupportRep (customer :: HashMap FieldName FieldValue) =
              let supportRep = do
                    employeeId <- (customer ^? Data.field "SupportRepId" . Data._ColumnFieldNumber)
                    employee <- _tdEmployeesRowsById ^? ix employeeId
                    firstName <- employee ^? Data.field "FirstName"
                    lastName <- employee ^? Data.field "LastName"
                    if firstName > lastName then pure employee else Nothing
                  supportRepPropVal = maybeToList $ Data.filterColumnsByQueryFields employeesQuery <$> supportRep
               in Data.insertField "SupportRep" (mkSubqueryResponse supportRepPropVal) customer

        let filterCustomersBySupportRepExistence (customer :: HashMap FieldName FieldValue) =
              let supportRep = customer ^.. Data.field "SupportRep" . subqueryRows
               in not (null supportRep)

        let expectedCustomers = filter filterCustomersBySupportRepExistence $ Data.filterColumnsByQueryFields (query ^. qrQuery) . joinInSupportRep <$> _tdCustomersRows
        Data.responseRows receivedCustomers `rowsShouldBe` expectedCustomers
        _qrAggregates receivedCustomers `jsonShouldBe` Nothing
  where
    albumsWithArtistQuery :: (Query -> Query) -> QueryRequest
    albumsWithArtistQuery modifySubquery =
      let artistsSubquery = modifySubquery artistsQuery
          fields =
            Data.mkFieldsMap
              [ ("AlbumId", Data.columnField "AlbumId"),
                ("Title", Data.columnField "Title"),
                ("Artist", RelField $ RelationshipField _tdArtistRelationshipName artistsSubquery)
              ]
          query = albumsQuery & qFields ?~ fields
       in QueryRequest _tdAlbumsTableName [Data.onlyKeepRelationships [_tdArtistRelationshipName] _tdAlbumsTableRelationships] query

    artistsWithAlbumsQuery :: (Query -> Query) -> QueryRequest
    artistsWithAlbumsQuery modifySubquery =
      let albumFields = Data.mkFieldsMap [("AlbumId", Data.columnField "AlbumId"), ("Title", Data.columnField "Title")]
          albumsSort = OrderBy mempty $ Data.orderByColumn [] "AlbumId" Ascending :| []
          albumsSubquery = albumsQuery & qFields ?~ albumFields & qOrderBy ?~ albumsSort & modifySubquery
          fields =
            Data.mkFieldsMap
              [ ("ArtistId", Data.columnField "ArtistId"),
                ("Name", Data.columnField "Name"),
                ("Albums", RelField $ RelationshipField _tdAlbumsRelationshipName albumsSubquery)
              ]
          query = artistsQuery & qFields ?~ fields
       in QueryRequest _tdArtistsTableName [Data.onlyKeepRelationships [_tdAlbumsRelationshipName] _tdArtistsTableRelationships] query

    employeesWithCustomersQuery :: (Query -> Query) -> QueryRequest
    employeesWithCustomersQuery modifySubquery =
      let customersSort = OrderBy mempty $ Data.orderByColumn [] "CustomerId" Ascending :| []
          customersSubquery = customersQuery & qOrderBy ?~ customersSort & modifySubquery
          fields =
            Data.queryFields employeesQuery
              <> Data.mkFieldsMap
                [ ("SupportRepForCustomers", RelField $ RelationshipField _tdSupportRepForCustomersRelationshipName customersSubquery)
                ]
          query = employeesQuery & qFields ?~ fields
       in QueryRequest _tdEmployeesTableName [Data.onlyKeepRelationships [_tdSupportRepForCustomersRelationshipName] _tdEmployeesTableRelationships] query

    customersWithSupportRepQuery :: (Query -> Query) -> QueryRequest
    customersWithSupportRepQuery modifySubquery =
      let supportRepSubquery = employeesQuery & modifySubquery
          fields =
            Data.queryFields customersQuery
              <> Data.mkFieldsMap
                [ ("SupportRep", RelField $ RelationshipField _tdSupportRepRelationshipName supportRepSubquery)
                ]
          query = customersQuery & qFields ?~ fields
       in QueryRequest _tdCustomersTableName [Data.onlyKeepRelationships [_tdSupportRepRelationshipName] _tdCustomersTableRelationships] query

    artistsQuery :: Query
    artistsQuery =
      let fields = Data.mkFieldsMap [("ArtistId", Data.columnField "ArtistId"), ("Name", Data.columnField "Name")]
       in Data.emptyQuery & qFields ?~ fields

    albumsQuery :: Query
    albumsQuery =
      let fields = Data.mkFieldsMap [("AlbumId", Data.columnField "AlbumId"), ("ArtistId", Data.columnField "ArtistId"), ("Title", Data.columnField "Title")]
       in Data.emptyQuery & qFields ?~ fields

    customersQuery :: Query
    customersQuery =
      let fields =
            Data.mkFieldsMap
              [ ("CustomerId", Data.columnField "CustomerId"),
                ("FirstName", Data.columnField "FirstName"),
                ("LastName", Data.columnField "LastName"),
                ("Country", Data.columnField "Country"),
                ("SupportRepId", Data.columnField "SupportRepId")
              ]
       in Data.emptyQuery & qFields ?~ fields

    employeesQuery :: Query
    employeesQuery =
      let fields =
            Data.mkFieldsMap
              [ ("EmployeeId", Data.columnField "EmployeeId"),
                ("FirstName", Data.columnField "FirstName"),
                ("LastName", Data.columnField "LastName"),
                ("Country", Data.columnField "Country")
              ]
       in Data.emptyQuery & qFields ?~ fields

    mkSubqueryResponse :: [HashMap FieldName FieldValue] -> FieldValue
    mkSubqueryResponse rows =
      mkRelationshipFieldValue $ QueryResponse (Just rows) Nothing

    subqueryRows :: Traversal' FieldValue (HashMap FieldName FieldValue)
    subqueryRows = _RelationshipFieldValue . qrRows . _Just . traverse
