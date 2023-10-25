module Test.Specs.QuerySpec.RelationshipsSpec (spec) where

import Control.Arrow ((>>>))
import Control.Lens (Traversal', ix, (&), (?~), (^.), (^..), (^?), _Just)
import Control.Monad (when)
import Data.Aeson (Value (..))
import Data.HashMap.Strict (HashMap)
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe, maybeToList)
import Data.Set qualified as Set
import Hasura.Backends.DataConnector.API
import Hasura.Backends.DataConnector.API.V0.Relationships as API
import Test.AgentAPI (queryGuarded)
import Test.Data (TestData (..))
import Test.Data qualified as Data
import Test.Expectations (jsonShouldBe, rowsShouldBe)
import Test.Sandwich (describe)
import Test.TestHelpers (AgentDatasetTestSpec, it)
import Prelude

spec :: TestData -> Maybe SubqueryComparisonCapabilities -> AgentDatasetTestSpec
spec TestData {..} subqueryComparisonCapabilities = describe "Relationship Queries" $ do
  it "perform an object relationship query by joining artist to albums" $ do
    let query = albumsWithArtistQuery id
    receivedAlbums <- Data.sortResponseRowsBy "AlbumId" <$> queryGuarded query

    let joinInArtist (album :: HashMap FieldName FieldValue) =
          let artist = (album ^? Data.field "ArtistId" . Data._ColumnFieldNumber) >>= \artistId -> _tdArtistsRowsById ^? ix artistId
              artistPropVal = maybeToList artist
           in Data.insertField "Artist" (Data.mkSubqueryRowsFieldValue artistPropVal) album
    let removeArtistId = Data.deleteField "ArtistId"

    let expectedAlbums = (removeArtistId . joinInArtist) <$> _tdAlbumsRows
    Data.responseRows receivedAlbums `rowsShouldBe` expectedAlbums
    _qrAggregates receivedAlbums `jsonShouldBe` Nothing

  it "perform an object relationship query by joining artist to albums but with zero artist fields" $ do
    let query = albumsWithArtistQuery (qFields ?~ mempty)
    receivedAlbums <- Data.sortResponseRowsBy "AlbumId" <$> queryGuarded query

    let joinInArtist (album :: HashMap FieldName FieldValue) =
          let artist = (album ^? Data.field "ArtistId" . Data._ColumnFieldNumber) >>= \artistId -> _tdArtistsRowsById ^? ix artistId
              artistPropVal = Data.filterColumns [] $ maybeToList artist
           in Data.insertField "Artist" (Data.mkSubqueryRowsFieldValue artistPropVal) album
    let removeArtistId = Data.deleteField "ArtistId"

    let expectedAlbums = (removeArtistId . joinInArtist) <$> _tdAlbumsRows
    Data.responseRows receivedAlbums `rowsShouldBe` expectedAlbums
    _qrAggregates receivedAlbums `jsonShouldBe` Nothing

  it "perform an array relationship query by joining albums to artists" $ do
    let query = artistsWithAlbumsQuery id
    receivedArtists <- Data.sortResponseRowsBy "ArtistId" <$> queryGuarded query

    let joinInAlbums (artist :: HashMap FieldName FieldValue) =
          let artistId = artist ^? Data.field "ArtistId" . Data._ColumnFieldNumber
              albumFilter artistId' album = album ^? Data.field "ArtistId" . Data._ColumnFieldNumber == Just artistId'
              albums = maybe [] (\artistId' -> filter (albumFilter artistId') _tdAlbumsRows) artistId
              albums' = Data.deleteField "ArtistId" <$> albums
           in Data.insertField "Albums" (Data.mkSubqueryRowsFieldValue albums') artist

    let expectedAlbums = joinInAlbums <$> _tdArtistsRows
    Data.responseRows receivedArtists `rowsShouldBe` expectedAlbums
    _qrAggregates receivedArtists `jsonShouldBe` Nothing

  it "perform an array relationship query by joining albums to artists but with zero album fields" $ do
    let query = artistsWithAlbumsQuery (qFields ?~ mempty)
    receivedArtists <- Data.sortResponseRowsBy "ArtistId" <$> queryGuarded query

    let joinInAlbums (artist :: HashMap FieldName FieldValue) =
          let artistId = artist ^? Data.field "ArtistId" . Data._ColumnFieldNumber
              albumFilter artistId' album = album ^? Data.field "ArtistId" . Data._ColumnFieldNumber == Just artistId'
              albums = maybe [] (\artistId' -> filter (albumFilter artistId') _tdAlbumsRows) artistId
              albums' = Data.filterColumns [] albums
           in Data.insertField "Albums" (Data.mkSubqueryRowsFieldValue albums') artist

    let expectedAlbums = joinInAlbums <$> _tdArtistsRows
    Data.responseRows receivedArtists `rowsShouldBe` expectedAlbums
    _qrAggregates receivedArtists `jsonShouldBe` Nothing

  it "perform an array relationship query by joining albums to artists with pagination of albums" $ do
    let albumsOrdering = OrderBy mempty $ NonEmpty.fromList [_tdOrderByColumn [] "AlbumId" Ascending]
    let query = artistsWithAlbumsQuery (qOffset ?~ 1 >>> qLimit ?~ 2 >>> qOrderBy ?~ albumsOrdering)
    receivedArtists <- Data.sortResponseRowsBy "ArtistId" <$> queryGuarded query

    let joinInAlbums (artist :: HashMap FieldName FieldValue) = do
          let artistId = artist ^? Data.field "ArtistId" . Data._ColumnFieldNumber
              albumFilter artistId' album = album ^? Data.field "ArtistId" . Data._ColumnFieldNumber == Just artistId'
              albums = maybe [] (\artistId' -> filter (albumFilter artistId') _tdAlbumsRows) artistId
              paginatedAlbums = albums & sortOn (^? Data.field "ArtistId") & drop 1 & take 2
              paginatedAlbums' = Data.deleteField "ArtistId" <$> paginatedAlbums
           in Data.insertField "Albums" (Data.mkSubqueryRowsFieldValue paginatedAlbums') artist

    let expectedAlbums = joinInAlbums <$> _tdArtistsRows
    Data.responseRows receivedArtists `rowsShouldBe` expectedAlbums
    _qrAggregates receivedArtists `jsonShouldBe` Nothing

  it "can filter in object relationships" $ do
    let artistWhere = ApplyBinaryComparisonOperator GreaterThanOrEqual (_tdCurrentComparisonColumn "Name" artistNameScalarType) (Data.scalarValueComparison (String "H") artistNameScalarType)
    let query = albumsWithArtistQuery (qWhere ?~ artistWhere)
    receivedAlbums <- Data.sortResponseRowsBy "AlbumId" <$> queryGuarded query

    let joinInArtist (album :: HashMap FieldName FieldValue) =
          let artist = do
                artistId <- album ^? Data.field "ArtistId" . Data._ColumnFieldNumber
                artist' <- _tdArtistsRowsById ^? ix artistId
                if (artist' ^? Data.field "Name" . Data._ColumnFieldString) >= Just "H" then Just artist' else Nothing
              artistPropVal = maybeToList artist
           in Data.insertField "Artist" (Data.mkSubqueryRowsFieldValue artistPropVal) album
    let removeArtistId = Data.deleteField "ArtistId"

    let expectedAlbums = (removeArtistId . joinInArtist) <$> _tdAlbumsRows
    Data.responseRows receivedAlbums `rowsShouldBe` expectedAlbums
    _qrAggregates receivedAlbums `jsonShouldBe` Nothing

  it "can filter in array relationships" $ do
    let albumsWhere = ApplyBinaryComparisonOperator GreaterThanOrEqual (_tdCurrentComparisonColumn "Title" albumTitleScalarType) (Data.scalarValueComparison (String "O") albumTitleScalarType)
    let query = artistsWithAlbumsQuery (qWhere ?~ albumsWhere)
    receivedArtists <- Data.sortResponseRowsBy "ArtistId" <$> queryGuarded query

    let joinInAlbums (artist :: HashMap FieldName FieldValue) =
          let albums = fromMaybe [] $ do
                artistId <- artist ^? Data.field "ArtistId" . Data._ColumnFieldNumber
                _tdAlbumsRows
                  & filter (\album -> album ^? Data.field "ArtistId" . Data._ColumnFieldNumber == Just artistId && album ^? Data.field "Title" . Data._ColumnFieldString >= Just "O")
                  & fmap (Data.deleteField "ArtistId")
                  & sortOn (^? Data.field "ArtistId")
                  & pure
           in Data.insertField "Albums" (Data.mkSubqueryRowsFieldValue albums) artist

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
                  (_tdCurrentComparisonColumn "Country" employeeCountryScalarType)
                  (AnotherColumnComparison (_tdQueryComparisonColumn "Country" employeeCountryScalarType))
        let query = customersWithSupportRepQuery id & qrQuery . qWhere ?~ where'
        receivedCustomers <- Data.sortResponseRowsBy "CustomerId" <$> queryGuarded query

        let joinInSupportRep (customer :: HashMap FieldName FieldValue) =
              let supportRep = (customer ^? Data.field "SupportRepId" . Data._ColumnFieldNumber) >>= \employeeId -> _tdEmployeesRowsById ^? ix employeeId
                  supportRepPropVal = maybeToList $ Data.filterColumnsByQueryFields employeesQuery <$> supportRep
               in Data.insertField "SupportRep" (Data.mkSubqueryRowsFieldValue supportRepPropVal) customer

        let filterCustomersBySupportRepCountry (customer :: HashMap FieldName FieldValue) =
              let customerCountry = customer ^? Data.field "Country" . Data._ColumnFieldString
                  supportRepCountry = customer ^.. Data.field "SupportRep" . subqueryRows . Data.field "Country" . Data._ColumnFieldString
               in any (`elem` supportRepCountry) customerCountry

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
                  (_tdCurrentComparisonColumn "Country" employeeCountryScalarType)
                  (AnotherColumnComparison (_tdQueryComparisonColumn "Country" employeeCountryScalarType))
        let query = employeesWithCustomersQuery id & qrQuery . qWhere ?~ where'
        receivedEmployees <- Data.sortResponseRowsBy "EmployeeId" <$> queryGuarded query

        let joinInCustomers (employee :: HashMap FieldName FieldValue) =
              let employeeId = employee ^? Data.field "EmployeeId" . Data._ColumnFieldNumber
                  customerFilter employeeId' customer = customer ^? Data.field "SupportRepId" . Data._ColumnFieldNumber == Just employeeId'
                  customers = maybe [] (\employeeId' -> filter (customerFilter employeeId') _tdCustomersRows) employeeId
                  customers' = Data.filterColumnsByQueryFields customersQuery <$> customers
               in Data.insertField "SupportRepForCustomers" (Data.mkSubqueryRowsFieldValue customers') employee

        let filterEmployeesByCustomerCountry (employee :: HashMap FieldName FieldValue) =
              let employeeCountry = employee ^? Data.field "Country" . Data._ColumnFieldString
                  customerCountries = employee ^.. Data.field "SupportRepForCustomers" . subqueryRows . Data.field "Country" . Data._ColumnFieldString
               in any (`elem` customerCountries) employeeCountry

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
                Data.mkAndExpr
                  [ ( ApplyBinaryComparisonOperator
                        GreaterThan
                        (_tdCurrentComparisonColumn "FirstName" employeeFirstNameScalarType)
                        (AnotherColumnComparison (_tdCurrentComparisonColumn "LastName" employeeLastNameScalarType))
                    ),
                    (Not (ApplyUnaryComparisonOperator IsNull (_tdCurrentComparisonColumn "EmployeeId" employeeIdScalarType)))
                  ]

        let employeesWhere =
              ApplyBinaryComparisonOperator
                GreaterThan
                (_tdCurrentComparisonColumn "FirstName" employeeFirstNameScalarType)
                (AnotherColumnComparison (_tdCurrentComparisonColumn "LastName" employeeLastNameScalarType))

        let query = customersWithSupportRepQuery (\q -> q & qWhere ?~ employeesWhere) & qrQuery . qWhere ?~ customersWhere
        receivedCustomers <- Data.sortResponseRowsBy "CustomerId" <$> queryGuarded query

        let joinInSupportRep (customer :: HashMap FieldName FieldValue) =
              let supportRep = do
                    employeeId <- (customer ^? Data.field "SupportRepId" . Data._ColumnFieldNumber)
                    employee <- _tdEmployeesRowsById ^? ix employeeId
                    firstName <- employee ^? Data.field "FirstName"
                    lastName <- employee ^? Data.field "LastName"
                    if firstName > lastName then pure employee else Nothing
                  supportRepPropVal = maybeToList $ Data.filterColumnsByQueryFields employeesQuery <$> supportRep
               in Data.insertField "SupportRep" (Data.mkSubqueryRowsFieldValue supportRepPropVal) customer

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
              [ ("AlbumId", _tdColumnField _tdAlbumsTableName "AlbumId"),
                ("Title", _tdColumnField _tdAlbumsTableName "Title"),
                ("Artist", RelField $ RelationshipField _tdArtistRelationshipName artistsSubquery)
              ]
          query = albumsQuery & qFields ?~ fields
       in TableQueryRequest _tdAlbumsTableName (Set.fromList [API.RTable $ Data.onlyKeepRelationships [_tdArtistRelationshipName] _tdAlbumsTableRelationships]) mempty mempty query Nothing

    artistsWithAlbumsQuery :: (Query -> Query) -> QueryRequest
    artistsWithAlbumsQuery modifySubquery =
      let albumFields = Data.mkFieldsMap [("AlbumId", _tdColumnField _tdAlbumsTableName "AlbumId"), ("Title", _tdColumnField _tdAlbumsTableName "Title")]
          albumsSort = OrderBy mempty $ _tdOrderByColumn [] "AlbumId" Ascending :| []
          albumsSubquery = albumsQuery & qFields ?~ albumFields & qOrderBy ?~ albumsSort & modifySubquery
          fields =
            Data.mkFieldsMap
              [ ("ArtistId", _tdColumnField _tdArtistsTableName "ArtistId"),
                ("Name", _tdColumnField _tdArtistsTableName "Name"),
                ("Albums", RelField $ RelationshipField _tdAlbumsRelationshipName albumsSubquery)
              ]
          query = artistsQuery & qFields ?~ fields
       in TableQueryRequest _tdArtistsTableName (Set.fromList [API.RTable $ Data.onlyKeepRelationships [_tdAlbumsRelationshipName] _tdArtistsTableRelationships]) mempty mempty query Nothing

    employeesWithCustomersQuery :: (Query -> Query) -> QueryRequest
    employeesWithCustomersQuery modifySubquery =
      let customersSort = OrderBy mempty $ _tdOrderByColumn [] "CustomerId" Ascending :| []
          customersSubquery = customersQuery & qOrderBy ?~ customersSort & modifySubquery
          fields =
            Data.queryFields employeesQuery
              <> Data.mkFieldsMap
                [ ("SupportRepForCustomers", RelField $ RelationshipField _tdSupportRepForCustomersRelationshipName customersSubquery)
                ]
          query = employeesQuery & qFields ?~ fields
       in TableQueryRequest _tdEmployeesTableName (Set.fromList [API.RTable $ Data.onlyKeepRelationships [_tdSupportRepForCustomersRelationshipName] _tdEmployeesTableRelationships]) mempty mempty query Nothing

    customersWithSupportRepQuery :: (Query -> Query) -> QueryRequest
    customersWithSupportRepQuery modifySubquery =
      let supportRepSubquery = employeesQuery & modifySubquery
          fields =
            Data.queryFields customersQuery
              <> Data.mkFieldsMap
                [ ("SupportRep", RelField $ RelationshipField _tdSupportRepRelationshipName supportRepSubquery)
                ]
          query = customersQuery & qFields ?~ fields
       in TableQueryRequest _tdCustomersTableName (Set.fromList [API.RTable $ Data.onlyKeepRelationships [_tdSupportRepRelationshipName] _tdCustomersTableRelationships]) mempty mempty query Nothing

    artistsQuery :: Query
    artistsQuery =
      let fields = Data.mkFieldsMap [("ArtistId", _tdColumnField _tdArtistsTableName "ArtistId"), ("Name", _tdColumnField _tdArtistsTableName "Name")]
       in Data.emptyQuery & qFields ?~ fields

    albumsQuery :: Query
    albumsQuery =
      let fields = Data.mkFieldsMap [("AlbumId", _tdColumnField _tdAlbumsTableName "AlbumId"), ("ArtistId", _tdColumnField _tdAlbumsTableName "ArtistId"), ("Title", _tdColumnField _tdAlbumsTableName "Title")]
       in Data.emptyQuery & qFields ?~ fields

    customersQuery :: Query
    customersQuery =
      let fields =
            Data.mkFieldsMap
              [ ("CustomerId", _tdColumnField _tdCustomersTableName "CustomerId"),
                ("FirstName", _tdColumnField _tdCustomersTableName "FirstName"),
                ("LastName", _tdColumnField _tdCustomersTableName "LastName"),
                ("Country", _tdColumnField _tdCustomersTableName "Country"),
                ("SupportRepId", _tdColumnField _tdCustomersTableName "SupportRepId")
              ]
       in Data.emptyQuery & qFields ?~ fields

    employeesQuery :: Query
    employeesQuery =
      let fields =
            Data.mkFieldsMap
              [ ("EmployeeId", _tdColumnField _tdEmployeesTableName "EmployeeId"),
                ("FirstName", _tdColumnField _tdEmployeesTableName "FirstName"),
                ("LastName", _tdColumnField _tdEmployeesTableName "LastName"),
                ("Country", _tdColumnField _tdEmployeesTableName "Country")
              ]
       in Data.emptyQuery & qFields ?~ fields

    subqueryRows :: Traversal' FieldValue (HashMap FieldName FieldValue)
    subqueryRows = _RelationshipFieldValue . qrRows . _Just . traverse

    albumTitleScalarType = _tdFindColumnScalarType _tdAlbumsTableName "Title"
    artistNameScalarType = _tdFindColumnScalarType _tdArtistsTableName "Name"
    employeeIdScalarType = _tdFindColumnScalarType _tdEmployeesTableName "EmployeeId"
    employeeCountryScalarType = _tdFindColumnScalarType _tdEmployeesTableName "Country"
    employeeFirstNameScalarType = _tdFindColumnScalarType _tdEmployeesTableName "FirstName"
    employeeLastNameScalarType = _tdFindColumnScalarType _tdEmployeesTableName "LastName"
