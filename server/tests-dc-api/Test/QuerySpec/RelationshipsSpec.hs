module Test.QuerySpec.RelationshipsSpec (spec) where

import Control.Lens (Traversal', ix, (&), (?~), (^.), (^..), (^?), _Just)
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (maybeToList)
import Hasura.Backends.DataConnector.API
import Servant.API (NamedRoutes)
import Servant.Client (Client, (//))
import Test.Data qualified as Data
import Test.Expectations (jsonShouldBe, rowsShouldBe)
import Test.Hspec (Spec, describe, it)
import Prelude

spec :: Client IO (NamedRoutes Routes) -> SourceName -> Config -> Spec
spec api sourceName config = describe "Relationship Queries" $ do
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

  it "perform an object relationship query by joining employee to customers and filter comparing columns across the object relationship" $ do
    -- Join Employee to Customers via SupportRep, and only get those customers that have a rep
    -- that is in the same country as the customer
    -- This sort of thing would come from a permissions filter on Customer that looks like:
    -- { SupportRep: { Country: { _ceq: [ "$", "Country" ] } } }
    let where' =
          ApplyBinaryComparisonOperator
            Equal
            (Data.comparisonColumn [Data.supportRepRelationshipName] "Country")
            (AnotherColumn (Data.localComparisonColumn "Country"))
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
          ApplyBinaryComparisonOperator
            Equal
            (Data.comparisonColumn [Data.supportRepForCustomersRelationshipName] "Country")
            (AnotherColumn (Data.localComparisonColumn "Country"))
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
      albumsSort = OrderBy (ColumnName "AlbumId") Ascending :| []
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
  let customersSort = OrderBy (ColumnName "CustomerId") Ascending :| []
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
