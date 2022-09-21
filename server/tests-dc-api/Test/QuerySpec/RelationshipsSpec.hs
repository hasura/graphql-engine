module Test.QuerySpec.RelationshipsSpec (spec) where

import Control.Arrow ((>>>))
import Control.Lens (Traversal', ix, (&), (?~), (^.), (^..), (^?), _Just)
import Control.Monad (when)
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (maybeToList)
import Hasura.Backends.DataConnector.API
import Servant.API (NamedRoutes)
import Servant.Client (Client, (//))
import Test.Data qualified as Data
import Test.Expectations (jsonShouldBe, rowsShouldBe)
import Test.Hspec (Spec, describe, it)
import Prelude

spec :: Client IO (NamedRoutes Routes) -> SourceName -> Config -> Maybe CrossTableComparisonCapabilities -> Spec
spec api sourceName config crossTableComparisonCapabilities = describe "Relationship Queries" $ do
  it "perform an object relationship query by joining artist to albums" $ do
    let query = albumsWithArtistQuery id
    receivedAlbums <- Data.sortResponseRowsBy "AlbumId" <$> (api // _query) sourceName config query

    let joinInArtist (album :: KeyMap FieldValue) =
          let artist = (album ^? ix "ArtistId" . Data._ColumnFieldNumber) >>= \artistId -> Data.artistsRowsById ^? ix artistId
              artistPropVal = maybeToList artist
           in KeyMap.insert "Artist" (mkSubqueryResponse artistPropVal) album
    let removeArtistId = KeyMap.delete "ArtistId"

    let expectedAlbums = (removeArtistId . joinInArtist) <$> Data.albumsRows
    Data.responseRows receivedAlbums `rowsShouldBe` expectedAlbums
    _qrAggregates receivedAlbums `jsonShouldBe` Nothing

  it "perform an array relationship query by joining albums to artists" $ do
    let query = artistsWithAlbumsQuery id
    receivedArtists <- Data.sortResponseRowsBy "ArtistId" <$> (api // _query) sourceName config query

    let joinInAlbums (artist :: KeyMap FieldValue) =
          let artistId = artist ^? ix "ArtistId" . Data._ColumnFieldNumber
              albumFilter artistId' album = album ^? ix "ArtistId" . Data._ColumnFieldNumber == Just artistId'
              albums = maybe [] (\artistId' -> filter (albumFilter artistId') Data.albumsRows) artistId
              albums' = KeyMap.delete "ArtistId" <$> albums
           in KeyMap.insert "Albums" (mkSubqueryResponse albums') artist

    let expectedAlbums = joinInAlbums <$> Data.artistsRows
    Data.responseRows receivedArtists `rowsShouldBe` expectedAlbums
    _qrAggregates receivedArtists `jsonShouldBe` Nothing

  it "perform an array relationship query by joining albums to artists with pagination of albums" $ do
    let albumsOrdering = OrderBy mempty $ NonEmpty.fromList [OrderByElement [] (OrderByColumn $ ColumnName "AlbumId") Ascending]
    let query = artistsWithAlbumsQuery (qOffset ?~ 1 >>> qLimit ?~ 2 >>> qOrderBy ?~ albumsOrdering)
    receivedArtists <- Data.sortResponseRowsBy "ArtistId" <$> (api // _query) sourceName config query

    let joinInAlbums (artist :: KeyMap FieldValue) = do
          let artistId = artist ^? ix "ArtistId" . Data._ColumnFieldNumber
              albumFilter artistId' album = album ^? ix "ArtistId" . Data._ColumnFieldNumber == Just artistId'
              albums = maybe [] (\artistId' -> filter (albumFilter artistId') Data.albumsRows) artistId
              paginatedAlbums = albums & sortOn (^? ix "ArtistId") & drop 1 & take 2
              paginatedAlbums' = KeyMap.delete "ArtistId" <$> paginatedAlbums
           in KeyMap.insert "Albums" (mkSubqueryResponse paginatedAlbums') artist

    let expectedAlbums = joinInAlbums <$> Data.artistsRows
    Data.responseRows receivedArtists `rowsShouldBe` expectedAlbums
    _qrAggregates receivedArtists `jsonShouldBe` Nothing

  when ((_ctccSupportsRelations <$> crossTableComparisonCapabilities) == Just True) $
    describe "Cross related table comparisons" $ do
      it "perform an object relationship query by joining employee to customers and filter comparing columns across the object relationship" $ do
        -- Join Employee to Customers via SupportRep, and only get those customers that have a rep
        -- that is in the same country as the customer
        -- This sort of thing would come from a permissions filter on Customer that looks like:
        -- { SupportRep: { Country: { _ceq: [ "$", "Country" ] } } }
        let where' =
              Exists (RelatedTable Data.supportRepRelationshipName) $
                ApplyBinaryComparisonOperator
                  Equal
                  (Data.currentComparisonColumn "Country")
                  (AnotherColumn (Data.queryComparisonColumn "Country"))
        let query = customersWithSupportRepQuery id & qrQuery . qWhere ?~ where'
        receivedCustomers <- Data.sortResponseRowsBy "CustomerId" <$> (api // _query) sourceName config query

        let joinInSupportRep (customer :: KeyMap FieldValue) =
              let supportRep = (customer ^? ix "SupportRepId" . Data._ColumnFieldNumber) >>= \employeeId -> Data.employeesRowsById ^? ix employeeId
                  supportRepPropVal = maybeToList $ Data.filterColumnsByQueryFields employeesQuery <$> supportRep
               in KeyMap.insert "SupportRep" (mkSubqueryResponse supportRepPropVal) customer

        let filterCustomersBySupportRepCountry (customer :: KeyMap FieldValue) =
              let customerCountry = customer ^? ix "Country" . Data._ColumnFieldString
                  supportRepCountry = customer ^.. ix "SupportRep" . subqueryRows . ix "Country" . Data._ColumnFieldString
               in maybe False (`elem` supportRepCountry) customerCountry

        let expectedCustomers = filter filterCustomersBySupportRepCountry $ Data.filterColumnsByQueryFields (query ^. qrQuery) . joinInSupportRep <$> Data.customersRows
        Data.responseRows receivedCustomers `rowsShouldBe` expectedCustomers
        _qrAggregates receivedCustomers `jsonShouldBe` Nothing

      it "perform an array relationship query by joining customers to employees and filter comparing columns across the array relationship" $ do
        -- Join Customers to Employees via SupportRepForCustomers, and only get those employees that are reps for
        -- customers that are in the same country as the employee
        -- This sort of thing would come from a permissions filter on Employees that looks like:
        -- { SupportRepForCustomers: { Country: { _ceq: [ "$", "Country" ] } } }
        let where' =
              Exists (RelatedTable Data.supportRepForCustomersRelationshipName) $
                ApplyBinaryComparisonOperator
                  Equal
                  (Data.currentComparisonColumn "Country")
                  (AnotherColumn (Data.queryComparisonColumn "Country"))
        let query = employeesWithCustomersQuery id & qrQuery . qWhere ?~ where'
        receivedEmployees <- Data.sortResponseRowsBy "EmployeeId" <$> (api // _query) sourceName config query

        let joinInCustomers (employee :: KeyMap FieldValue) =
              let employeeId = employee ^? ix "EmployeeId" . Data._ColumnFieldNumber
                  customerFilter employeeId' customer = customer ^? ix "SupportRepId" . Data._ColumnFieldNumber == Just employeeId'
                  customers = maybe [] (\employeeId' -> filter (customerFilter employeeId') Data.customersRows) employeeId
                  customers' = Data.filterColumnsByQueryFields customersQuery <$> customers
               in KeyMap.insert "SupportRepForCustomers" (mkSubqueryResponse customers') employee

        let filterEmployeesByCustomerCountry (employee :: KeyMap FieldValue) =
              let employeeCountry = employee ^? ix "Country" . Data._ColumnFieldString
                  customerCountries = employee ^.. ix "SupportRepForCustomers" . subqueryRows . ix "Country" . Data._ColumnFieldString
               in maybe False (`elem` customerCountries) employeeCountry

        let expectedEmployees = filter filterEmployeesByCustomerCountry $ Data.filterColumnsByQueryFields (query ^. qrQuery) . joinInCustomers <$> Data.employeesRows
        Data.responseRows receivedEmployees `rowsShouldBe` expectedEmployees
        _qrAggregates receivedEmployees `jsonShouldBe` Nothing

      it "perform an object relationship query by joining employee to customers but filter employees by comparing columns on the employee" $ do
        -- Join Employee to Customers via SupportRep, and only get those customers that have a rep
        -- However, the Employee table is filtered with a permission rule that compares columns on that table.
        -- This Employee table permissions filter would look like:
        -- { FirstName: { _cgt: ["LastName"] } }
        let customersWhere =
              Exists (RelatedTable Data.supportRepRelationshipName) $
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

        let joinInSupportRep (customer :: KeyMap FieldValue) =
              let supportRep = do
                    employeeId <- (customer ^? ix "SupportRepId" . Data._ColumnFieldNumber)
                    employee <- Data.employeesRowsById ^? ix employeeId
                    firstName <- employee ^? ix "FirstName"
                    lastName <- employee ^? ix "LastName"
                    if firstName > lastName then pure employee else Nothing
                  supportRepPropVal = maybeToList $ Data.filterColumnsByQueryFields employeesQuery <$> supportRep
               in KeyMap.insert "SupportRep" (mkSubqueryResponse supportRepPropVal) customer

        let filterCustomersBySupportRepExistence (customer :: KeyMap FieldValue) =
              let supportRep = customer ^.. ix "SupportRep" . subqueryRows
               in not (null supportRep)

        let expectedCustomers = filter filterCustomersBySupportRepExistence $ Data.filterColumnsByQueryFields (query ^. qrQuery) . joinInSupportRep <$> Data.customersRows
        Data.responseRows receivedCustomers `rowsShouldBe` expectedCustomers
        _qrAggregates receivedCustomers `jsonShouldBe` Nothing

albumsWithArtistQuery :: (Query -> Query) -> QueryRequest
albumsWithArtistQuery modifySubquery =
  let artistsSubquery = modifySubquery artistsQuery
      fields =
        KeyMap.fromList
          [ ("AlbumId", Data.columnField "AlbumId"),
            ("Title", Data.columnField "Title"),
            ("Artist", RelField $ RelationshipField Data.artistRelationshipName artistsSubquery)
          ]
      query = albumsQuery & qFields ?~ fields
   in QueryRequest Data.albumsTableName [Data.onlyKeepRelationships [Data.artistRelationshipName] Data.albumsTableRelationships] query

artistsWithAlbumsQuery :: (Query -> Query) -> QueryRequest
artistsWithAlbumsQuery modifySubquery =
  let albumFields = KeyMap.fromList [("AlbumId", Data.columnField "AlbumId"), ("Title", Data.columnField "Title")]
      albumsSort = OrderBy mempty $ Data.orderByColumn [] "AlbumId" Ascending :| []
      albumsSubquery = albumsQuery & qFields ?~ albumFields & qOrderBy ?~ albumsSort & modifySubquery
      fields =
        KeyMap.fromList
          [ ("ArtistId", Data.columnField "ArtistId"),
            ("Name", Data.columnField "Name"),
            ("Albums", RelField $ RelationshipField Data.albumsRelationshipName albumsSubquery)
          ]
      query = artistsQuery & qFields ?~ fields
   in QueryRequest Data.artistsTableName [Data.onlyKeepRelationships [Data.albumsRelationshipName] Data.artistsTableRelationships] query

employeesWithCustomersQuery :: (Query -> Query) -> QueryRequest
employeesWithCustomersQuery modifySubquery =
  let customersSort = OrderBy mempty $ Data.orderByColumn [] "CustomerId" Ascending :| []
      customersSubquery = customersQuery & qOrderBy ?~ customersSort & modifySubquery
      fields =
        Data.queryFields employeesQuery
          <> KeyMap.fromList
            [ ("SupportRepForCustomers", RelField $ RelationshipField Data.supportRepForCustomersRelationshipName customersSubquery)
            ]
      query = employeesQuery & qFields ?~ fields
   in QueryRequest Data.employeesTableName [Data.onlyKeepRelationships [Data.supportRepForCustomersRelationshipName] Data.employeesTableRelationships] query

customersWithSupportRepQuery :: (Query -> Query) -> QueryRequest
customersWithSupportRepQuery modifySubquery =
  let supportRepSubquery = employeesQuery & modifySubquery
      fields =
        Data.queryFields customersQuery
          <> KeyMap.fromList
            [ ("SupportRep", RelField $ RelationshipField Data.supportRepRelationshipName supportRepSubquery)
            ]
      query = customersQuery & qFields ?~ fields
   in QueryRequest Data.customersTableName [Data.onlyKeepRelationships [Data.supportRepRelationshipName] Data.customersTableRelationships] query

artistsQuery :: Query
artistsQuery =
  let fields = KeyMap.fromList [("ArtistId", Data.columnField "ArtistId"), ("Name", Data.columnField "Name")]
   in Data.emptyQuery & qFields ?~ fields

albumsQuery :: Query
albumsQuery =
  let fields = KeyMap.fromList [("AlbumId", Data.columnField "AlbumId"), ("ArtistId", Data.columnField "ArtistId"), ("Title", Data.columnField "Title")]
   in Data.emptyQuery & qFields ?~ fields

customersQuery :: Query
customersQuery =
  let fields =
        KeyMap.fromList
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
        KeyMap.fromList
          [ ("EmployeeId", Data.columnField "EmployeeId"),
            ("FirstName", Data.columnField "FirstName"),
            ("LastName", Data.columnField "LastName"),
            ("Country", Data.columnField "Country")
          ]
   in Data.emptyQuery & qFields ?~ fields

mkSubqueryResponse :: [KeyMap FieldValue] -> FieldValue
mkSubqueryResponse rows =
  mkRelationshipFieldValue $ QueryResponse (Just rows) Nothing

subqueryRows :: Traversal' FieldValue (KeyMap FieldValue)
subqueryRows = _RelationshipFieldValue . qrRows . _Just . traverse
