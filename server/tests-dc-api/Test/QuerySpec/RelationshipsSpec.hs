module Test.QuerySpec.RelationshipsSpec (spec) where

import Autodocodec.Extended (ValueWrapper (..), ValueWrapper3 (..))
import Control.Lens (Traversal', ix, (&), (.~), (^.), (^..), (^?), _Just)
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (maybeToList)
import Data.Text (Text)
import Hasura.Backends.DataConnector.API
import Servant.API (NamedRoutes)
import Servant.Client (Client, (//))
import Test.Data qualified as Data
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Prelude

spec :: Client IO (NamedRoutes Routes) -> SourceName -> Config -> Spec
spec api sourceName config = describe "Relationship Queries" $ do
  it "perform an object relationship query by joining artist to albums" $ do
    let query = albumsWithArtistQuery id
    receivedAlbums <- Data.sortResponseRowsBy "AlbumId" <$> (api // _query) sourceName config query

    let joinInArtist (album :: KeyMap FieldValue) =
          let artist = (album ^? ix "ArtistId" . Data._ColumnFieldNumber) >>= \artistId -> Data.artistsAsJsonById ^? ix artistId
              artistPropVal = maybeToList artist
           in KeyMap.insert "Artist" (mkSubqueryResponse artistPropVal) album
    let removeArtistId = KeyMap.delete "ArtistId"

    let expectedAlbums = (removeArtistId . joinInArtist) <$> Data.albumsAsJson
    Data.responseRows receivedAlbums `shouldBe` expectedAlbums
    _qrAggregates receivedAlbums `shouldBe` Nothing

  it "perform an array relationship query by joining albums to artists" $ do
    let query = artistsWithAlbumsQuery id
    receivedArtists <- Data.sortResponseRowsBy "ArtistId" <$> (api // _query) sourceName config query

    let joinInAlbums (artist :: KeyMap FieldValue) =
          let artistId = artist ^? ix "ArtistId" . Data._ColumnFieldNumber
              albumFilter artistId' album = album ^? ix "ArtistId" . Data._ColumnFieldNumber == Just artistId'
              albums = maybe [] (\artistId' -> filter (albumFilter artistId') Data.albumsAsJson) artistId
              albums' = KeyMap.delete "ArtistId" <$> albums
           in KeyMap.insert "Albums" (mkSubqueryResponse albums') artist

    let expectedAlbums = joinInAlbums <$> Data.artistsAsJson
    Data.responseRows receivedArtists `shouldBe` expectedAlbums
    _qrAggregates receivedArtists `shouldBe` Nothing

  it "perform an object relationship query by joining employee to customers and filter comparing columns across the object relationship" $ do
    -- Join Employee to Customers via SupportRep, and only get those customers that have a rep
    -- that is in the same country as the customer
    -- This sort of thing would come from a permissions filter on Customer that looks like:
    -- { SupportRep: { Country: { _ceq: [ "$", "Country" ] } } }
    let where' =
          ApplyBinaryComparisonOperator
            ( ValueWrapper3
                Equal
                (comparisonColumn [supportRepRelationshipName] "Country")
                (AnotherColumn (ValueWrapper (comparisonColumn [] "Country")))
            )
    let query = customersWithSupportRepQuery id & qrQuery . qWhere .~ Just where'
    receivedCustomers <- Data.sortResponseRowsBy "CustomerId" <$> (api // _query) sourceName config query

    let joinInSupportRep (customer :: KeyMap FieldValue) =
          let supportRep = (customer ^? ix "SupportRepId" . Data._ColumnFieldNumber) >>= \employeeId -> Data.employeesAsJsonById ^? ix employeeId
              supportRepPropVal = maybeToList $ Data.filterColumnsByQueryFields employeesQuery <$> supportRep
           in KeyMap.insert "SupportRep" (mkSubqueryResponse supportRepPropVal) customer

    let filterCustomersBySupportRepCountry (customer :: KeyMap FieldValue) =
          let customerCountry = customer ^? ix "Country" . Data._ColumnFieldString
              supportRepCountry = customer ^.. ix "SupportRep" . subqueryRows . ix "Country" . Data._ColumnFieldString
           in maybe False (`elem` supportRepCountry) customerCountry

    let expectedCustomers = filter filterCustomersBySupportRepCountry $ Data.filterColumnsByQueryFields (query ^. qrQuery) . joinInSupportRep <$> Data.customersAsJson
    Data.responseRows receivedCustomers `shouldBe` expectedCustomers
    _qrAggregates receivedCustomers `shouldBe` Nothing

  it "perform an array relationship query by joining customers to employees and filter comparing columns across the array relationship" $ do
    -- Join Customers to Employees via SupportRepForCustomers, and only get those employees that are reps for
    -- customers that are in the same country as the employee
    -- This sort of thing would come from a permissions filter on Employees that looks like:
    -- { SupportRepForCustomers: { Country: { _ceq: [ "$", "Country" ] } } }
    let where' =
          ApplyBinaryComparisonOperator
            ( ValueWrapper3
                Equal
                (comparisonColumn [supportRepForCustomersRelationshipName] "Country")
                (AnotherColumn (ValueWrapper (comparisonColumn [] "Country")))
            )
    let query = employeesWithCustomersQuery id & qrQuery . qWhere .~ Just where'
    receivedEmployees <- Data.sortResponseRowsBy "EmployeeId" <$> (api // _query) sourceName config query

    let joinInCustomers (employee :: KeyMap FieldValue) =
          let employeeId = employee ^? ix "EmployeeId" . Data._ColumnFieldNumber
              customerFilter employeeId' customer = customer ^? ix "SupportRepId" . Data._ColumnFieldNumber == Just employeeId'
              customers = maybe [] (\employeeId' -> filter (customerFilter employeeId') Data.customersAsJson) employeeId
              customers' = Data.filterColumnsByQueryFields customersQuery <$> customers
           in KeyMap.insert "SupportRepForCustomers" (mkSubqueryResponse customers') employee

    let filterEmployeesByCustomerCountry (employee :: KeyMap FieldValue) =
          let employeeCountry = employee ^? ix "Country" . Data._ColumnFieldString
              customerCountries = employee ^.. ix "SupportRepForCustomers" . subqueryRows . ix "Country" . Data._ColumnFieldString
           in maybe False (`elem` customerCountries) employeeCountry

    let expectedEmployees = filter filterEmployeesByCustomerCountry $ Data.filterColumnsByQueryFields (query ^. qrQuery) . joinInCustomers <$> Data.employeesAsJson
    Data.responseRows receivedEmployees `shouldBe` expectedEmployees
    _qrAggregates receivedEmployees `shouldBe` Nothing

albumsWithArtistQuery :: (Query -> Query) -> QueryRequest
albumsWithArtistQuery modifySubquery =
  let artistsSubquery = modifySubquery artistsQuery
      fields =
        KeyMap.fromList
          [ ("AlbumId", columnField "AlbumId"),
            ("Title", columnField "Title"),
            ("Artist", RelField $ RelationshipField artistRelationshipName artistsSubquery)
          ]
      query = albumsQuery {_qFields = Just fields}
   in QueryRequest albumsTableName [albumsTableRelationships] query

artistsWithAlbumsQuery :: (Query -> Query) -> QueryRequest
artistsWithAlbumsQuery modifySubquery =
  let albumFields = KeyMap.fromList [("AlbumId", columnField "AlbumId"), ("Title", columnField "Title")]
      albumsSort = OrderBy (ColumnName "AlbumId") Ascending :| []
      albumsSubquery = albumsQuery & qFields .~ Just albumFields & qOrderBy .~ Just albumsSort & modifySubquery
      fields =
        KeyMap.fromList
          [ ("ArtistId", columnField "ArtistId"),
            ("Name", columnField "Name"),
            ("Albums", RelField $ RelationshipField albumsRelationshipName albumsSubquery)
          ]
      query = artistsQuery {_qFields = Just fields}
   in QueryRequest artistsTableName [artistsTableRelationships] query

employeesWithCustomersQuery :: (Query -> Query) -> QueryRequest
employeesWithCustomersQuery modifySubquery =
  let customersSort = OrderBy (ColumnName "CustomerId") Ascending :| []
      customersSubquery = customersQuery & qOrderBy .~ Just customersSort & modifySubquery
      fields =
        Data.queryFields employeesQuery
          <> KeyMap.fromList
            [ ("SupportRepForCustomers", RelField $ RelationshipField supportRepForCustomersRelationshipName customersSubquery)
            ]
      query = employeesQuery {_qFields = Just fields}
   in QueryRequest employeesTableName [employeesTableRelationships] query

customersWithSupportRepQuery :: (Query -> Query) -> QueryRequest
customersWithSupportRepQuery modifySubquery =
  let supportRepSubquery = employeesQuery & modifySubquery
      fields =
        Data.queryFields customersQuery
          <> KeyMap.fromList
            [ ("SupportRep", RelField $ RelationshipField supportRepRelationshipName supportRepSubquery)
            ]
      query = customersQuery {_qFields = Just fields}
   in QueryRequest customersTableName [customersTableRelationships] query

artistsTableName :: TableName
artistsTableName = TableName "Artist"

albumsTableName :: TableName
albumsTableName = TableName "Album"

customersTableName :: TableName
customersTableName = TableName "Customer"

employeesTableName :: TableName
employeesTableName = TableName "Employee"

artistRelationshipName :: RelationshipName
artistRelationshipName = RelationshipName "Artist"

albumsRelationshipName :: RelationshipName
albumsRelationshipName = RelationshipName "Albums"

supportRepForCustomersRelationshipName :: RelationshipName
supportRepForCustomersRelationshipName = RelationshipName "SupportRepForCustomers"

supportRepRelationshipName :: RelationshipName
supportRepRelationshipName = RelationshipName "SupportRep"

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

albumsTableRelationships :: TableRelationships
albumsTableRelationships =
  let joinFieldMapping =
        HashMap.fromList
          [ (ColumnName "ArtistId", ColumnName "ArtistId")
          ]
   in TableRelationships
        albumsTableName
        ( HashMap.fromList
            [ (artistRelationshipName, Relationship artistsTableName ObjectRelationship joinFieldMapping)
            ]
        )

employeesTableRelationships :: TableRelationships
employeesTableRelationships =
  let joinFieldMapping =
        HashMap.fromList
          [ (ColumnName "EmployeeId", ColumnName "SupportRepId")
          ]
   in TableRelationships
        employeesTableName
        ( HashMap.fromList
            [ (supportRepForCustomersRelationshipName, Relationship customersTableName ArrayRelationship joinFieldMapping)
            ]
        )

customersTableRelationships :: TableRelationships
customersTableRelationships =
  let joinFieldMapping =
        HashMap.fromList
          [ (ColumnName "SupportRepId", ColumnName "EmployeeId")
          ]
   in TableRelationships
        customersTableName
        ( HashMap.fromList
            [ (supportRepRelationshipName, Relationship employeesTableName ObjectRelationship joinFieldMapping)
            ]
        )

artistsQuery :: Query
artistsQuery =
  let fields = KeyMap.fromList [("ArtistId", columnField "ArtistId"), ("Name", columnField "Name")]
   in Query (Just fields) Nothing Nothing Nothing Nothing Nothing

albumsQuery :: Query
albumsQuery =
  let fields = KeyMap.fromList [("AlbumId", columnField "AlbumId"), ("ArtistId", columnField "ArtistId"), ("Title", columnField "Title")]
   in Query (Just fields) Nothing Nothing Nothing Nothing Nothing

customersQuery :: Query
customersQuery =
  let fields =
        KeyMap.fromList
          [ ("CustomerId", columnField "CustomerId"),
            ("FirstName", columnField "FirstName"),
            ("LastName", columnField "LastName"),
            ("Country", columnField "Country"),
            ("SupportRepId", columnField "SupportRepId")
          ]
   in Query (Just fields) Nothing Nothing Nothing Nothing Nothing

employeesQuery :: Query
employeesQuery =
  let fields =
        KeyMap.fromList
          [ ("EmployeeId", columnField "EmployeeId"),
            ("FirstName", columnField "FirstName"),
            ("LastName", columnField "LastName"),
            ("Country", columnField "Country")
          ]
   in Query (Just fields) Nothing Nothing Nothing Nothing Nothing

columnField :: Text -> Field
columnField = ColumnField . ValueWrapper . ColumnName

comparisonColumn :: [RelationshipName] -> Text -> ComparisonColumn
comparisonColumn path columnName = ComparisonColumn path $ ColumnName columnName

mkSubqueryResponse :: [KeyMap FieldValue] -> FieldValue
mkSubqueryResponse rows =
  mkRelationshipFieldValue $ QueryResponse (Just rows) Nothing

subqueryRows :: Traversal' FieldValue (KeyMap FieldValue)
subqueryRows = _RelationshipFieldValue . qrRows . _Just . traverse
