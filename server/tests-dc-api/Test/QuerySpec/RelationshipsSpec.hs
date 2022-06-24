module Test.QuerySpec.RelationshipsSpec (spec) where

import Autodocodec.Extended (ValueWrapper (..), ValueWrapper3 (..))
import Control.Lens (ix, (&), (.~), (^.), (^..), (^?))
import Data.Aeson (Object, Value (..))
import Data.Aeson qualified as J
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Lens (_Array, _Number, _String)
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Vector qualified as Vector
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
    receivedAlbums <- (Data.sortBy "AlbumId" . getQueryResponse) <$> (api // _query) sourceName config query

    let joinInArtist (album :: Object) =
          let artist = (album ^? ix "ArtistId" . _Number) >>= \artistId -> Data.artistsAsJsonById ^? ix artistId
              artistPropVal = maybe J.Null Object artist
           in KeyMap.insert "Artist" artistPropVal album
    let removeArtistId = KeyMap.delete "ArtistId"

    let expectedAlbums = (removeArtistId . joinInArtist) <$> Data.albumsAsJson
    receivedAlbums `shouldBe` expectedAlbums

  it "perform an array relationship query by joining albums to artists" $ do
    let query = artistsWithAlbumsQuery id
    receivedArtists <- (Data.sortBy "ArtistId" . getQueryResponse) <$> (api // _query) sourceName config query

    let joinInAlbums (artist :: Object) =
          let artistId = artist ^? ix "ArtistId" . _Number
              albumFilter artistId' album = album ^? ix "ArtistId" . _Number == Just artistId'
              albums = maybe [] (\artistId' -> filter (albumFilter artistId') Data.albumsAsJson) artistId
              albums' = Object . KeyMap.delete "ArtistId" <$> albums
           in KeyMap.insert "Albums" (Array . Vector.fromList $ albums') artist

    let expectedAlbums = joinInAlbums <$> Data.artistsAsJson
    receivedArtists `shouldBe` expectedAlbums

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
    receivedCustomers <- (Data.sortBy "CustomerId" . getQueryResponse) <$> (api // _query) sourceName config query

    let joinInSupportRep (customer :: Object) =
          let supportRep = (customer ^? ix "SupportRepId" . _Number) >>= \employeeId -> Data.employeesAsJsonById ^? ix employeeId
              supportRepPropVal = maybe J.Null (Object . Data.filterColumnsByQueryFields employeesQuery) supportRep
           in KeyMap.insert "SupportRep" supportRepPropVal customer

    let filterCustomersBySupportRepCountry (customer :: Object) =
          let customerCountry = customer ^? ix "Country" . _String
              supportRepCountry = customer ^? ix "SupportRep" . ix "Country" . _String
              comparison = (==) <$> customerCountry <*> supportRepCountry
           in fromMaybe False comparison

    let expectedCustomers = filter filterCustomersBySupportRepCountry $ Data.filterColumnsByQueryFields (query ^. qrQuery) . joinInSupportRep <$> Data.customersAsJson
    receivedCustomers `shouldBe` expectedCustomers

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
    receivedEmployees <- (Data.sortBy "EmployeeId" . getQueryResponse) <$> (api // _query) sourceName config query

    let joinInCustomers (employee :: Object) =
          let employeeId = employee ^? ix "EmployeeId" . _Number
              customerFilter employeeId' customer = customer ^? ix "SupportRepId" . _Number == Just employeeId'
              customers = maybe [] (\employeeId' -> filter (customerFilter employeeId') Data.customersAsJson) employeeId
              customers' = Object . Data.filterColumnsByQueryFields customersQuery <$> customers
           in KeyMap.insert "SupportRepForCustomers" (Array . Vector.fromList $ customers') employee

    let filterEmployeesByCustomerCountry (employee :: Object) =
          let employeeCountry = employee ^? ix "Country" . _String
              customerCountries = employee ^.. ix "SupportRepForCustomers" . _Array . traverse . ix "Country" . _String
           in maybe False (`elem` customerCountries) employeeCountry

    let expectedEmployees = filter filterEmployeesByCustomerCountry $ Data.filterColumnsByQueryFields (query ^. qrQuery) . joinInCustomers <$> Data.employeesAsJson
    receivedEmployees `shouldBe` expectedEmployees

albumsWithArtistQuery :: (Query -> Query) -> QueryRequest
albumsWithArtistQuery modifySubquery =
  let artistsSubquery = modifySubquery artistsQuery
      fields =
        KeyMap.fromList
          [ ("AlbumId", columnField "AlbumId"),
            ("Title", columnField "Title"),
            ("Artist", RelField $ RelationshipField artistRelationshipName artistsSubquery)
          ]
      query = albumsQuery {_qFields = fields}
   in QueryRequest albumsTableName [albumsTableRelationships] query

artistsWithAlbumsQuery :: (Query -> Query) -> QueryRequest
artistsWithAlbumsQuery modifySubquery =
  let albumFields = KeyMap.fromList [("AlbumId", columnField "AlbumId"), ("Title", columnField "Title")]
      albumsSort = OrderBy (ColumnName "AlbumId") Ascending :| []
      albumsSubquery = albumsQuery & qFields .~ albumFields & qOrderBy .~ Just albumsSort & modifySubquery
      fields =
        KeyMap.fromList
          [ ("ArtistId", columnField "ArtistId"),
            ("Name", columnField "Name"),
            ("Albums", RelField $ RelationshipField albumsRelationshipName albumsSubquery)
          ]
      query = artistsQuery {_qFields = fields}
   in QueryRequest artistsTableName [artistsTableRelationships] query

employeesWithCustomersQuery :: (Query -> Query) -> QueryRequest
employeesWithCustomersQuery modifySubquery =
  let customersSort = OrderBy (ColumnName "CustomerId") Ascending :| []
      customersSubquery = customersQuery & qOrderBy .~ Just customersSort & modifySubquery
      fields =
        _qFields employeesQuery
          <> KeyMap.fromList
            [ ("SupportRepForCustomers", RelField $ RelationshipField supportRepForCustomersRelationshipName customersSubquery)
            ]
      query = employeesQuery {_qFields = fields}
   in QueryRequest employeesTableName [employeesTableRelationships] query

customersWithSupportRepQuery :: (Query -> Query) -> QueryRequest
customersWithSupportRepQuery modifySubquery =
  let supportRepSubquery = employeesQuery & modifySubquery
      fields =
        _qFields customersQuery
          <> KeyMap.fromList
            [ ("SupportRep", RelField $ RelationshipField supportRepRelationshipName supportRepSubquery)
            ]
      query = customersQuery {_qFields = fields}
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
   in Query fields Nothing Nothing Nothing Nothing

albumsQuery :: Query
albumsQuery =
  let fields = KeyMap.fromList [("AlbumId", columnField "AlbumId"), ("ArtistId", columnField "ArtistId"), ("Title", columnField "Title")]
   in Query fields Nothing Nothing Nothing Nothing

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
   in Query fields Nothing Nothing Nothing Nothing

employeesQuery :: Query
employeesQuery =
  let fields =
        KeyMap.fromList
          [ ("EmployeeId", columnField "EmployeeId"),
            ("FirstName", columnField "FirstName"),
            ("LastName", columnField "LastName"),
            ("Country", columnField "Country")
          ]
   in Query fields Nothing Nothing Nothing Nothing

columnField :: Text -> Field
columnField = ColumnField . ValueWrapper . ColumnName

comparisonColumn :: [RelationshipName] -> Text -> ComparisonColumn
comparisonColumn path columnName = ComparisonColumn path $ ColumnName columnName
