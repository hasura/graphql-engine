module Test.Specs.MutationSpec.InsertSpec (spec) where

import Control.Arrow ((>>>))
import Control.Lens (ix, (&), (.~), (?~), (^?), _Just)
import Control.Monad (when)
import Data.Aeson qualified as J
import Data.Foldable (find, for_)
import Data.Functor ((<&>))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe, listToMaybe, maybeToList)
import Data.Scientific (Scientific)
import Data.Set qualified as Set
import Data.Text (Text)
import Hasura.Backends.DataConnector.API
import Test.AgentAPI (mutationExpectError, mutationGuarded, queryGuarded)
import Test.AgentDatasets (chinookTemplate, usesDataset)
import Test.Data (EdgeCasesTestData (..), TestData (..))
import Test.Data qualified as Data
import Test.Expectations (mutationResponseShouldBe, rowsShouldBe)
import Test.Sandwich (describe, shouldBe)
import Test.TestHelpers (AgentTestSpec, it)
import Test.TestHelpers qualified as Test
import Prelude

spec :: TestData -> Maybe EdgeCasesTestData -> Capabilities -> AgentTestSpec
spec TestData {..} edgeCasesTestData Capabilities {..} = describe "Insert Mutations" $ do
  usesDataset chinookTemplate $ it "can insert a single row" $ do
    let insertOperation = mkInsertOperation _tdArtistsTableName & imoRows .~ take 1 newArtists
    let mutationRequest =
          Data.emptyMutationRequest
            & mrOperations .~ [InsertOperation insertOperation]
            & mrInsertSchema .~ Set.fromList [artistsInsertSchema]

    response <- mutationGuarded mutationRequest

    let expectedResult = MutationOperationResults 1 Nothing
    response `mutationResponseShouldBe` MutationResponse [expectedResult]

    receivedArtists <- Data.sortResponseRowsBy "ArtistId" <$> queryGuarded (artistsQueryRequest [artistsStartingId])
    Data.responseRows receivedArtists `rowsShouldBe` take 1 (expectedInsertedArtists artistsStartingId)

  usesDataset chinookTemplate $ it "can insert when the field names do not match the column names" $ do
    let row =
          RowObject . Data.mkFieldsMap $
            [ ("artist_name", mkColumnInsertFieldValue $ J.String "Taylor Swift")
            ]
    let insertOperation = mkInsertOperation _tdArtistsTableName & imoRows .~ [row]
    let insertSchema =
          mkTableInsertSchema _tdSchemaTables _tdArtistsTableName $
            [ ("artist_id", ColumnInsert (_tdColumnInsertSchema _tdArtistsTableName "ArtistId")),
              ("artist_name", ColumnInsert (_tdColumnInsertSchema _tdArtistsTableName "Name"))
            ]
    let mutationRequest =
          Data.emptyMutationRequest
            & mrOperations .~ [InsertOperation insertOperation]
            & mrInsertSchema .~ Set.fromList [insertSchema]

    response <- mutationGuarded mutationRequest

    let expectedResult = MutationOperationResults 1 Nothing
    response `mutationResponseShouldBe` MutationResponse [expectedResult]

    receivedArtists <- Data.sortResponseRowsBy "ArtistId" <$> queryGuarded (artistsQueryRequest [artistsStartingId])
    Data.responseRows receivedArtists `rowsShouldBe` take 1 (expectedInsertedArtists artistsStartingId)

  usesDataset chinookTemplate $ it "can insert multiple rows" $ do
    let insertOperation = mkInsertOperation _tdArtistsTableName & imoRows .~ newArtists
    let mutationRequest =
          Data.emptyMutationRequest
            & mrOperations .~ [InsertOperation insertOperation]
            & mrInsertSchema .~ Set.fromList [artistsInsertSchema]

    response <- mutationGuarded mutationRequest

    let expectedResult = MutationOperationResults 4 Nothing
    response `mutationResponseShouldBe` MutationResponse [expectedResult]

    receivedArtists <- Data.sortResponseRowsBy "ArtistId" <$> queryGuarded (artistsQueryRequest $ Data.autoIncPks artistsStartingId newArtists)
    Data.responseRows receivedArtists `rowsShouldBe` expectedInsertedArtists artistsStartingId

  usesDataset chinookTemplate $ it "can insert using multiple operations" $ do
    let newAlbumRows = take 2 newAcdcAlbums
    let insertOperation1 = mkInsertOperation _tdArtistsTableName & imoRows .~ take 1 newArtists
    let insertOperation2 = mkInsertOperation _tdArtistsTableName & imoRows .~ drop 1 newArtists
    let insertOperation3 = mkInsertOperation _tdAlbumsTableName & imoRows .~ newAlbumRows
    let mutationRequest =
          Data.emptyMutationRequest
            & mrOperations .~ [InsertOperation insertOperation1, InsertOperation insertOperation2, InsertOperation insertOperation3]
            & mrInsertSchema .~ Set.fromList [albumsInsertSchema, artistsInsertSchema]

    response <- mutationGuarded mutationRequest

    let expectedResult1 = MutationOperationResults 1 Nothing
    let expectedResult2 = MutationOperationResults 3 Nothing
    let expectedResult3 = MutationOperationResults 2 Nothing

    response `mutationResponseShouldBe` MutationResponse [expectedResult1, expectedResult2, expectedResult3]

    receivedArtists <- Data.sortResponseRowsBy "ArtistId" <$> queryGuarded (artistsQueryRequest $ Data.autoIncPks artistsStartingId newArtists)
    Data.responseRows receivedArtists `rowsShouldBe` expectedInsertedArtists artistsStartingId

    receivedAlbums <- Data.sortResponseRowsBy "AlbumId" <$> queryGuarded (albumsQueryRequest $ Data.autoIncPks albumsStartingId newAlbumRows)
    Data.responseRows receivedAlbums `rowsShouldBe` take 2 (expectedInsertedAcdcAlbums albumsStartingId)

  usesDataset chinookTemplate $ it "can insert multiple rows with differing column sets" $ do
    let insertOperation = mkInsertOperation _tdEmployeesTableName & imoRows .~ newEmployees
    let mutationRequest =
          Data.emptyMutationRequest
            & mrOperations .~ [InsertOperation insertOperation]
            & mrInsertSchema .~ Set.fromList [employeesInsertSchema]

    response <- mutationGuarded mutationRequest

    let expectedResult = MutationOperationResults 2 Nothing
    response `mutationResponseShouldBe` MutationResponse [expectedResult]

    receivedEmployees <- Data.sortResponseRowsBy "EmployeeId" <$> queryGuarded (employeesQueryRequest $ Data.autoIncPks employeesStartingId newEmployees)
    Data.responseRows receivedEmployees `rowsShouldBe` expectedInsertedEmployees employeesStartingId

  describe "post-insert checks" $ do
    usesDataset chinookTemplate $ it "can insert when post insert check passes" $ do
      let rows = take 1 newAcdcAlbums ++ take 1 newApocalypticaAlbums
      let insertOperation =
            mkInsertOperation _tdAlbumsTableName
              & imoRows .~ rows
              & imoPostInsertCheck
                ?~ Data.mkOrExpr
                  [ ApplyBinaryComparisonOperator Equal (_tdCurrentComparisonColumn "ArtistId" artistIdScalarType) (Data.scalarValueComparison (J.Number acdcArtistId) artistIdScalarType),
                    ApplyBinaryComparisonOperator Equal (_tdCurrentComparisonColumn "ArtistId" artistIdScalarType) (Data.scalarValueComparison (J.Number apocalypticaArtistId) artistIdScalarType)
                  ]
      let mutationRequest =
            Data.emptyMutationRequest
              & mrOperations .~ [InsertOperation insertOperation]
              & mrInsertSchema .~ Set.fromList [albumsInsertSchema]

      response <- mutationGuarded mutationRequest

      let expectedResult = MutationOperationResults 2 Nothing

      response `mutationResponseShouldBe` MutationResponse [expectedResult]

      let expectedAlbums =
            ( take 1 (expectedInsertedAcdcAlbums albumsStartingId)
                ++ take 1 (expectedInsertedApocalypticaAlbums (albumsStartingId + 1))
            )

      receivedAlbums <- Data.sortResponseRowsBy "AlbumId" <$> queryGuarded (albumsQueryRequest $ Data.autoIncPks albumsStartingId rows)
      Data.responseRows receivedAlbums `rowsShouldBe` expectedAlbums

    usesDataset chinookTemplate $ it "fails to insert when post insert check fails" $ do
      let rows = take 1 newAcdcAlbums ++ take 1 newApocalypticaAlbums
      let insertOperation =
            mkInsertOperation _tdAlbumsTableName
              & imoRows .~ rows
              & imoPostInsertCheck ?~ ApplyBinaryComparisonOperator Equal (_tdCurrentComparisonColumn "ArtistId" artistIdScalarType) (Data.scalarValueComparison (J.Number acdcArtistId) artistIdScalarType)
      let mutationRequest =
            Data.emptyMutationRequest
              & mrOperations .~ [InsertOperation insertOperation]
              & mrInsertSchema .~ Set.fromList [albumsInsertSchema]

      response <- mutationExpectError mutationRequest
      _crType response `shouldBe` MutationPermissionCheckFailure

      receivedAlbums <- Data.sortResponseRowsBy "AlbumId" <$> queryGuarded (albumsQueryRequest $ Data.autoIncPks albumsStartingId rows)
      Data.responseRows receivedAlbums `rowsShouldBe` []

    for_ (_cComparisons >>= _ccSubqueryComparisonCapabilities) $ \_subqueryComparisonCapabilities -> do
      usesDataset chinookTemplate $ it "can insert when post insert check against unrelated table passes" $ do
        let rows = take 1 newAcdcAlbums ++ take 1 newApocalypticaAlbums
        let someEmployeeIdThatExists = 1
        let insertOperation =
              mkInsertOperation _tdAlbumsTableName
                & imoRows .~ rows
                & imoPostInsertCheck
                  ?~ Exists
                    (UnrelatedTable _tdEmployeesTableName)
                    (ApplyBinaryComparisonOperator Equal (_tdCurrentComparisonColumn "EmployeeId" employeeIdScalarType) (Data.scalarValueComparison (J.Number someEmployeeIdThatExists) employeeIdScalarType))
        let mutationRequest =
              Data.emptyMutationRequest
                & mrOperations .~ [InsertOperation insertOperation]
                & mrInsertSchema .~ Set.singleton albumsInsertSchema
                & mrRelationships .~ Set.singleton (RTable $ Data.onlyKeepRelationships [_tdArtistRelationshipName] _tdAlbumsTableRelationships)

        response <- mutationGuarded mutationRequest

        let expectedResult = MutationOperationResults 2 Nothing
        response `mutationResponseShouldBe` MutationResponse [expectedResult]

        let expectedAlbums =
              ( take 1 (expectedInsertedAcdcAlbums albumsStartingId)
                  ++ take 1 (expectedInsertedApocalypticaAlbums (albumsStartingId + 1))
              )

        receivedAlbums <- Data.sortResponseRowsBy "AlbumId" <$> queryGuarded (albumsQueryRequest $ Data.autoIncPks albumsStartingId rows)
        Data.responseRows receivedAlbums `rowsShouldBe` expectedAlbums

      usesDataset chinookTemplate $ it "fails to insert when post insert check against unrelated table fails" $ do
        let rows = take 1 newAcdcAlbums ++ take 1 newApocalypticaAlbums
        let someEmployeeIdThatDoesNotExist = 50
        let insertOperation =
              mkInsertOperation _tdAlbumsTableName
                & imoRows .~ rows
                & imoPostInsertCheck
                  ?~ Exists
                    (UnrelatedTable _tdEmployeesTableName)
                    (ApplyBinaryComparisonOperator Equal (_tdCurrentComparisonColumn "EmployeeId" employeeIdScalarType) (Data.scalarValueComparison (J.Number someEmployeeIdThatDoesNotExist) employeeIdScalarType))
        let mutationRequest =
              Data.emptyMutationRequest
                & mrOperations .~ [InsertOperation insertOperation]
                & mrInsertSchema .~ Set.singleton albumsInsertSchema
                & mrRelationships .~ Set.singleton (RTable $ Data.onlyKeepRelationships [_tdArtistRelationshipName] _tdAlbumsTableRelationships)

        response <- mutationExpectError mutationRequest
        _crType response `shouldBe` MutationPermissionCheckFailure

        receivedAlbums <- Data.sortResponseRowsBy "AlbumId" <$> queryGuarded (albumsQueryRequest $ Data.autoIncPks albumsStartingId rows)
        Data.responseRows receivedAlbums `rowsShouldBe` []

    when ((_cComparisons >>= _ccSubqueryComparisonCapabilities <&> _ctccSupportsRelations) == Just True) $ do
      usesDataset chinookTemplate $ it "can insert when post insert check against related table passes" $ do
        let rows = take 1 newAcdcAlbums ++ take 1 newApocalypticaAlbums
        let insertOperation =
              mkInsertOperation _tdAlbumsTableName
                & imoRows .~ rows
                & imoPostInsertCheck
                  ?~ Exists
                    (RelatedTable _tdArtistRelationshipName)
                    ( Data.mkOrExpr
                        [ ApplyBinaryComparisonOperator Equal (_tdCurrentComparisonColumn "Name" artistNameScalarType) (Data.scalarValueComparison (J.String "AC/DC") artistNameScalarType),
                          ApplyBinaryComparisonOperator Equal (_tdCurrentComparisonColumn "Name" artistNameScalarType) (Data.scalarValueComparison (J.String "Apocalyptica") artistNameScalarType)
                        ]
                    )
        let mutationRequest =
              Data.emptyMutationRequest
                & mrOperations .~ [InsertOperation insertOperation]
                & mrInsertSchema .~ Set.singleton albumsInsertSchema
                & mrRelationships .~ Set.singleton (RTable $ Data.onlyKeepRelationships [_tdArtistRelationshipName] _tdAlbumsTableRelationships)

        response <- mutationGuarded mutationRequest

        let expectedResult = MutationOperationResults 2 Nothing
        response `mutationResponseShouldBe` MutationResponse [expectedResult]

        let expectedAlbums =
              ( take 1 (expectedInsertedAcdcAlbums albumsStartingId)
                  ++ take 1 (expectedInsertedApocalypticaAlbums (albumsStartingId + 1))
              )

        receivedAlbums <- Data.sortResponseRowsBy "AlbumId" <$> queryGuarded (albumsQueryRequest $ Data.autoIncPks albumsStartingId rows)
        Data.responseRows receivedAlbums `rowsShouldBe` expectedAlbums

      usesDataset chinookTemplate $ it "fails to insert when post insert check against related table fails" $ do
        let rows = take 1 newAcdcAlbums ++ take 1 newApocalypticaAlbums
        let insertOperation =
              mkInsertOperation _tdAlbumsTableName
                & imoRows .~ rows
                & imoPostInsertCheck
                  ?~ Exists
                    (RelatedTable _tdArtistRelationshipName)
                    (ApplyBinaryComparisonOperator Equal (_tdCurrentComparisonColumn "Name" artistNameScalarType) (Data.scalarValueComparison (J.String "AC/DC") artistNameScalarType))
        let mutationRequest =
              Data.emptyMutationRequest
                & mrOperations .~ [InsertOperation insertOperation]
                & mrInsertSchema .~ Set.singleton albumsInsertSchema
                & mrRelationships .~ Set.singleton (RTable $ Data.onlyKeepRelationships [_tdArtistRelationshipName] _tdAlbumsTableRelationships)

        response <- mutationExpectError mutationRequest
        _crType response `shouldBe` MutationPermissionCheckFailure

        receivedAlbums <- Data.sortResponseRowsBy "AlbumId" <$> queryGuarded (albumsQueryRequest $ Data.autoIncPks albumsStartingId rows)
        Data.responseRows receivedAlbums `rowsShouldBe` []

  for_ (_cMutations >>= _mcReturningCapabilities) $ \_returningCapabilities -> describe "returning" $ do
    usesDataset chinookTemplate $ it "can return all inserted columns including the auto-generated primary key" $ do
      let returning =
            Data.mkFieldsMap
              [ ("AlbumId", _tdColumnField _tdAlbumsTableName "AlbumId"),
                ("ArtistId", _tdColumnField _tdAlbumsTableName "ArtistId"),
                ("Title", _tdColumnField _tdAlbumsTableName "Title")
              ]
      let insertOperation =
            mkInsertOperation _tdAlbumsTableName
              & imoRows .~ newAcdcAlbums
              & imoReturningFields .~ returning
      let mutationRequest =
            Data.emptyMutationRequest
              & mrOperations .~ [InsertOperation insertOperation]
              & mrInsertSchema .~ Set.fromList [albumsInsertSchema]

      response <- mutationGuarded mutationRequest

      let expectedResult = MutationOperationResults 3 (Just (expectedInsertedAcdcAlbums albumsStartingId))

      response `mutationResponseShouldBe` MutationResponse [expectedResult]

    usesDataset chinookTemplate $ it "can insert rows with missing nullable fields and return nulls for those fields" $ do
      let returning =
            ["EmployeeId", "FirstName", "LastName", "Title", "ReportsTo", "BirthDate", "HireDate", "Address", "City", "State", "Country", "PostalCode", "Phone", "Fax", "Email"]
              & fmap (\column -> (column, _tdColumnField _tdEmployeesTableName column))
              & Data.mkFieldsMap
      let insertOperation =
            mkInsertOperation _tdEmployeesTableName
              & imoRows .~ newEmployees
              & imoReturningFields .~ returning
      let mutationRequest =
            Data.emptyMutationRequest
              & mrOperations .~ [InsertOperation insertOperation]
              & mrInsertSchema .~ Set.fromList [employeesInsertSchema]

      response <- mutationGuarded mutationRequest

      let expectedResult = MutationOperationResults 2 (Just (expectedInsertedEmployees employeesStartingId))

      response `mutationResponseShouldBe` MutationResponse [expectedResult]

    for_ _cRelationships $ \_relationshipCapabilities -> do
      usesDataset chinookTemplate $ it "can return rows from an object relationship" $ do
        let rows = take 1 newAcdcAlbums ++ take 1 newApocalypticaAlbums

        let returning =
              Data.mkFieldsMap
                [ ("AlbumId", _tdColumnField _tdAlbumsTableName "AlbumId"),
                  ("Title", _tdColumnField _tdAlbumsTableName "Title"),
                  ( "Artist",
                    ( RelField
                        ( RelationshipField _tdArtistRelationshipName $
                            Data.emptyQuery
                              & qFields
                                ?~ Data.mkFieldsMap
                                  [ ("ArtistId", _tdColumnField _tdArtistsTableName "ArtistId"),
                                    ("Name", _tdColumnField _tdArtistsTableName "Name")
                                  ]
                        )
                    )
                  )
                ]

        let insertOperation =
              mkInsertOperation _tdAlbumsTableName
                & imoRows .~ rows
                & imoReturningFields .~ returning

        let mutationRequest =
              Data.emptyMutationRequest
                & mrOperations .~ [InsertOperation insertOperation]
                & mrInsertSchema .~ Set.singleton albumsInsertSchema
                & mrRelationships .~ Set.singleton (RTable $ Data.onlyKeepRelationships [_tdArtistRelationshipName] _tdAlbumsTableRelationships)

        response <- mutationGuarded mutationRequest

        let joinInArtist (album :: HashMap FieldName FieldValue) =
              let artist = (album ^? Data.field "ArtistId" . Data._ColumnFieldNumber) >>= \artistId -> _tdArtistsRowsById ^? ix artistId
                  artistPropVal = maybeToList artist
               in Data.insertField "Artist" (Data.mkSubqueryRowsFieldValue artistPropVal) album

        let removeArtistId = Data.deleteField "ArtistId"

        let expectedRows =
              ( take 1 (expectedInsertedAcdcAlbums albumsStartingId)
                  ++ take 1 (expectedInsertedApocalypticaAlbums (albumsStartingId + 1))
              )
                & fmap (joinInArtist >>> removeArtistId)

        let expectedResult = MutationOperationResults 2 (Just expectedRows)

        response `mutationResponseShouldBe` MutationResponse [expectedResult]

      usesDataset chinookTemplate $ it "can return rows from an array relationship" $ do
        let returning =
              Data.mkFieldsMap
                [ ("EmployeeId", _tdColumnField _tdEmployeesTableName "EmployeeId"),
                  ("FirstName", _tdColumnField _tdEmployeesTableName "FirstName"),
                  ("LastName", _tdColumnField _tdEmployeesTableName "LastName"),
                  ( "ReportsToEmployee",
                    ( RelField
                        ( RelationshipField _tdReportsToEmployeeRelationshipName $
                            Data.emptyQuery
                              & qFields
                                ?~ Data.mkFieldsMap
                                  [ ("EmployeeId", _tdColumnField _tdEmployeesTableName "EmployeeId"),
                                    ("FirstName", _tdColumnField _tdEmployeesTableName "FirstName"),
                                    ("LastName", _tdColumnField _tdEmployeesTableName "LastName"),
                                    ( "SupportRepForCustomers",
                                      ( RelField
                                          ( RelationshipField _tdSupportRepForCustomersRelationshipName $
                                              Data.emptyQuery
                                                & qFields
                                                  ?~ Data.mkFieldsMap
                                                    [ ("CustomerId", _tdColumnField _tdCustomersTableName "CustomerId"),
                                                      ("FirstName", _tdColumnField _tdCustomersTableName "FirstName"),
                                                      ("LastName", _tdColumnField _tdCustomersTableName "LastName")
                                                    ]
                                                & qOrderBy ?~ OrderBy mempty (_tdOrderByColumn [] "CustomerId" Ascending :| [])
                                          )
                                      )
                                    )
                                  ]
                        )
                    )
                  )
                ]
        let insertOperation =
              mkInsertOperation _tdEmployeesTableName
                & imoRows .~ newEmployees
                & imoReturningFields .~ returning
        let mutationRequest =
              Data.emptyMutationRequest
                & mrOperations .~ [InsertOperation insertOperation]
                & mrInsertSchema .~ Set.fromList [employeesInsertSchema]
                & mrRelationships
                  .~ Set.singleton (RTable $ Data.onlyKeepRelationships [_tdReportsToEmployeeRelationshipName, _tdSupportRepForCustomersRelationshipName] _tdEmployeesTableRelationships)

        response <- mutationGuarded mutationRequest

        let joinInSupportRepForCustomers (employee :: HashMap FieldName FieldValue) =
              let customers = fromMaybe [] $ do
                    employeeId <- employee ^? Data.field "EmployeeId" . Data._ColumnFieldNumber
                    _tdCustomersRows
                      & filter (\customer -> customer ^? Data.field "SupportRepId" . Data._ColumnFieldNumber == Just employeeId)
                      & sortOn (^? Data.field "CustomerId")
                      & pure
                  shapedCustomers = Data.filterColumns ["CustomerId", "FirstName", "LastName"] customers
               in Data.insertField "SupportRepForCustomers" (Data.mkSubqueryRowsFieldValue shapedCustomers) employee

        let joinInReportsToEmployee (employee :: HashMap FieldName FieldValue) =
              let reportsToEmployee = (employee ^? Data.field "ReportsTo" . Data._ColumnFieldNumber) >>= \employeeId -> _tdEmployeesRowsById ^? ix employeeId
                  reportsToEmployeeWithSupportRepForCustomers = joinInSupportRepForCustomers <$> reportsToEmployee
                  trimEmployeeFields = Data.filterColumns ["EmployeeId", "FirstName", "LastName", "SupportRepForCustomers"]
               in Data.insertField "ReportsToEmployee" (Data.mkSubqueryRowsFieldValue (trimEmployeeFields $ maybeToList reportsToEmployeeWithSupportRepForCustomers)) employee

        let expectedRows =
              expectedInsertedEmployees employeesStartingId
                & fmap joinInReportsToEmployee
                & Data.filterColumns ["EmployeeId", "FirstName", "LastName", "ReportsToEmployee"]

        let expectedResult = MutationOperationResults 2 (Just expectedRows)

        response `mutationResponseShouldBe` MutationResponse [expectedResult]

      usesDataset chinookTemplate $ it "can aggregate rows across an array relationship" $ do
        let returning =
              Data.mkFieldsMap
                [ ("EmployeeId", _tdColumnField _tdEmployeesTableName "EmployeeId"),
                  ("FirstName", _tdColumnField _tdEmployeesTableName "FirstName"),
                  ("LastName", _tdColumnField _tdEmployeesTableName "LastName"),
                  ( "ReportsToEmployee",
                    ( RelField
                        ( RelationshipField _tdReportsToEmployeeRelationshipName $
                            Data.emptyQuery
                              & qFields
                                ?~ Data.mkFieldsMap
                                  [ ("EmployeeId", _tdColumnField _tdEmployeesTableName "EmployeeId"),
                                    ("FirstName", _tdColumnField _tdEmployeesTableName "FirstName"),
                                    ("LastName", _tdColumnField _tdEmployeesTableName "LastName"),
                                    ( "SupportRepForCustomers",
                                      ( RelField
                                          ( RelationshipField _tdSupportRepForCustomersRelationshipName $
                                              Data.emptyQuery & qAggregates ?~ Data.mkFieldsMap [("CustomerCount", StarCount)]
                                          )
                                      )
                                    )
                                  ]
                        )
                    )
                  )
                ]
        let insertOperation =
              mkInsertOperation _tdEmployeesTableName
                & imoRows .~ newEmployees
                & imoReturningFields .~ returning
        let mutationRequest =
              Data.emptyMutationRequest
                & mrOperations .~ [InsertOperation insertOperation]
                & mrInsertSchema .~ Set.fromList [employeesInsertSchema]
                & mrRelationships
                  .~ Set.singleton (RTable $ Data.onlyKeepRelationships [_tdReportsToEmployeeRelationshipName, _tdSupportRepForCustomersRelationshipName] _tdEmployeesTableRelationships)

        response <- mutationGuarded mutationRequest

        let joinInSupportRepForCustomers (employee :: HashMap FieldName FieldValue) =
              let employeeId = employee ^? Data.field "EmployeeId" . Data._ColumnFieldNumber
                  customerCount = _tdCustomersRows & filter (\customer -> customer ^? Data.field "SupportRepId" . Data._ColumnFieldNumber == employeeId) & length
                  aggregates = Data.mkFieldsMap [("CustomerCount", J.Number $ fromIntegral customerCount)]
               in Data.insertField "SupportRepForCustomers" (Data.mkSubqueryAggregatesFieldValue aggregates) employee

        let joinInReportsToEmployee (employee :: HashMap FieldName FieldValue) =
              let reportsToEmployee = (employee ^? Data.field "ReportsTo" . Data._ColumnFieldNumber) >>= \employeeId -> _tdEmployeesRowsById ^? ix employeeId
                  reportsToEmployeeWithSupportRepForCustomers = joinInSupportRepForCustomers <$> reportsToEmployee
                  trimEmployeeFields = Data.filterColumns ["EmployeeId", "FirstName", "LastName", "SupportRepForCustomers"]
               in Data.insertField "ReportsToEmployee" (Data.mkSubqueryRowsFieldValue (trimEmployeeFields $ maybeToList reportsToEmployeeWithSupportRepForCustomers)) employee

        let expectedRows =
              expectedInsertedEmployees employeesStartingId
                & fmap joinInReportsToEmployee
                & Data.filterColumns ["EmployeeId", "FirstName", "LastName", "ReportsToEmployee"]

        let expectedResult = MutationOperationResults 2 (Just expectedRows)

        response `mutationResponseShouldBe` MutationResponse [expectedResult]

      usesDataset chinookTemplate $ it "inserted rows are returned even when returned again from across a relationship" $ do
        -- This scenario inserts some albums onto an artist, then returns them,
        -- joined to their artist and also returns all albums on that artist.
        -- We expect to see the new albums included in the artists list of albums.
        let returning =
              Data.mkFieldsMap
                [ ("AlbumId", _tdColumnField _tdAlbumsTableName "AlbumId"),
                  ("Title", _tdColumnField _tdAlbumsTableName "Title"),
                  ( "Artist",
                    ( RelField
                        ( RelationshipField _tdArtistRelationshipName $
                            Data.emptyQuery
                              & qFields
                                ?~ Data.mkFieldsMap
                                  [ ("ArtistId", _tdColumnField _tdArtistsTableName "ArtistId"),
                                    ("Name", _tdColumnField _tdArtistsTableName "Name"),
                                    ( "Albums",
                                      ( RelField
                                          ( RelationshipField _tdAlbumsRelationshipName $
                                              Data.emptyQuery
                                                & qFields
                                                  ?~ Data.mkFieldsMap
                                                    [ ("AlbumId", _tdColumnField _tdAlbumsTableName "AlbumId"),
                                                      ("Title", _tdColumnField _tdAlbumsTableName "Title")
                                                    ]
                                                & qOrderBy ?~ OrderBy mempty (_tdOrderByColumn [] "AlbumId" Ascending :| [])
                                          )
                                      )
                                    )
                                  ]
                        )
                    )
                  )
                ]
        let insertOperation =
              mkInsertOperation _tdAlbumsTableName
                & imoRows .~ newAcdcAlbums
                & imoReturningFields .~ returning
        let mutationRequest =
              Data.emptyMutationRequest
                & mrOperations .~ [InsertOperation insertOperation]
                & mrInsertSchema .~ Set.fromList [albumsInsertSchema]
                & mrRelationships
                  .~ Set.fromList
                    [ RTable $ Data.onlyKeepRelationships [_tdArtistRelationshipName] _tdAlbumsTableRelationships,
                      RTable $ Data.onlyKeepRelationships [_tdAlbumsRelationshipName] _tdArtistsTableRelationships
                    ]

        response <- mutationGuarded mutationRequest

        let expectedNewAlbums = expectedInsertedAcdcAlbums albumsStartingId

        let joinInAlbums (artist :: HashMap FieldName FieldValue) =
              let albums = fromMaybe [] $ do
                    artistId <- artist ^? Data.field "ArtistId" . Data._ColumnFieldNumber
                    (expectedNewAlbums ++ _tdAlbumsRows)
                      & filter (\album -> album ^? Data.field "ArtistId" . Data._ColumnFieldNumber == Just artistId)
                      & sortOn (^? Data.field "AlbumId")
                      & pure
                  shapedAlbums = Data.deleteField "ArtistId" <$> albums
               in Data.insertField "Albums" (Data.mkSubqueryRowsFieldValue shapedAlbums) artist

        let joinInArtist (album :: HashMap FieldName FieldValue) =
              let artist = (album ^? Data.field "ArtistId" . Data._ColumnFieldNumber) >>= \artistId -> _tdArtistsRowsById ^? ix artistId
                  artistWithAlbums = joinInAlbums <$> artist
               in Data.insertField "Artist" (Data.mkSubqueryRowsFieldValue (maybeToList artistWithAlbums)) album
        let removeArtistId = Data.deleteField "ArtistId"

        let expectedRows =
              expectedNewAlbums
                & fmap (joinInArtist >>> removeArtistId)

        let expectedResult = MutationOperationResults 3 (Just expectedRows)

        response `mutationResponseShouldBe` MutationResponse [expectedResult]

  for_ (_cMutations >>= _mcReturningCapabilities) $ \_returningCapabilities -> describe "edge cases" $ do
    edgeCaseTest _ectdNoPrimaryKeyTableName "can insert into a table with no primary key" $ \EdgeCasesTestData {..} -> do
      let rows =
            [ RowObject . Data.mkFieldsMap $
                [ ("FirstName", mkColumnInsertFieldValue $ J.String "James"),
                  ("LastName", mkColumnInsertFieldValue $ J.String "Kirk")
                ],
              RowObject . Data.mkFieldsMap $
                [ ("FirstName", mkColumnInsertFieldValue $ J.String "Christopher"),
                  ("LastName", mkColumnInsertFieldValue $ J.String "Pike")
                ]
            ]
      let returning =
            Data.mkFieldsMap
              [ ("FirstName", _ectdColumnField _ectdNoPrimaryKeyTableName "FirstName"),
                ("LastName", _ectdColumnField _ectdNoPrimaryKeyTableName "LastName")
              ]
      let insertOperation =
            mkInsertOperation _ectdNoPrimaryKeyTableName
              & imoRows .~ rows
              & imoReturningFields .~ returning
      let insertSchema = _ectdMkDefaultTableInsertSchema _ectdNoPrimaryKeyTableName
      let mutationRequest =
            Data.emptyMutationRequest
              & mrOperations .~ [InsertOperation insertOperation]
              & mrInsertSchema .~ Set.fromList [insertSchema]

      response <- mutationGuarded mutationRequest

      let expectedRows =
            [ Data.mkFieldsMap $
                [ ("FirstName", mkColumnFieldValue $ J.String "James"),
                  ("LastName", mkColumnFieldValue $ J.String "Kirk")
                ],
              Data.mkFieldsMap $
                [ ("FirstName", mkColumnFieldValue $ J.String "Christopher"),
                  ("LastName", mkColumnFieldValue $ J.String "Pike")
                ]
            ]
      let expectedResult = MutationOperationResults 2 (Just expectedRows)

      response `mutationResponseShouldBe` MutationResponse [expectedResult]

    edgeCaseTest _ectdDefaultedPrimaryKeyTableName "can insert into a table with a defaulted primary key" $ \EdgeCasesTestData {..} -> do
      let rows =
            [ RowObject . Data.mkFieldsMap $
                [ ("Message", mkColumnInsertFieldValue $ J.String "A message")
                ]
            ]
      let returning =
            Data.mkFieldsMap
              [ ("TimestampKey", _ectdColumnField _ectdDefaultedPrimaryKeyTableName "TimestampKey"),
                ("Message", _ectdColumnField _ectdDefaultedPrimaryKeyTableName "Message")
              ]
      let insertOperation =
            mkInsertOperation _ectdDefaultedPrimaryKeyTableName
              & imoRows .~ rows
              & imoReturningFields .~ returning
      let insertSchema = _ectdMkDefaultTableInsertSchema _ectdDefaultedPrimaryKeyTableName
      let mutationRequest =
            Data.emptyMutationRequest
              & mrOperations .~ [InsertOperation insertOperation]
              & mrInsertSchema .~ Set.fromList [insertSchema]

      response <- mutationGuarded mutationRequest

      let dbGeneratedTimestampValue = response ^? mrOperationResults . ix 0 . morReturning . _Just . ix 0 . Data.field "TimestampKey" . Data._ColumnFieldString
      let expectedRow =
            Data.mkFieldsMap $
              -- Use the timestamp generated by the DB as the expected value, if it exists, or use a placeholder as a fallback to fail against
              [ ("TimestampKey", mkColumnFieldValue $ J.String $ fromMaybe "<some DB-generated value>" dbGeneratedTimestampValue),
                ("Message", mkColumnFieldValue $ J.String "A message")
              ]
      let expectedResult = MutationOperationResults 1 (Just [expectedRow])

      response `mutationResponseShouldBe` MutationResponse [expectedResult]

    edgeCaseTest _ectdAllColumnsDefaultableTableName "can insert into a table with all defaultable columns" $ \EdgeCasesTestData {..} -> do
      let rows =
            [ RowObject . Data.mkFieldsMap $ []
            ]
      let returning =
            Data.mkFieldsMap
              [ ("Id", _ectdColumnField _ectdAllColumnsDefaultableTableName "Id"),
                ("Message", _ectdColumnField _ectdAllColumnsDefaultableTableName "Message"),
                ("Importance", _ectdColumnField _ectdAllColumnsDefaultableTableName "Importance")
              ]
      let insertOperation =
            mkInsertOperation _ectdAllColumnsDefaultableTableName
              & imoRows .~ rows
              & imoReturningFields .~ returning
      let insertSchema = _ectdMkDefaultTableInsertSchema _ectdAllColumnsDefaultableTableName
      let mutationRequest =
            Data.emptyMutationRequest
              & mrOperations .~ [InsertOperation insertOperation]
              & mrInsertSchema .~ Set.fromList [insertSchema]

      response <- mutationGuarded mutationRequest

      let expectedRow =
            Data.mkFieldsMap $
              [ ("Id", mkColumnFieldValue $ J.Number 4),
                ("Message", mkColumnFieldValue $ J.Null),
                ("Importance", mkColumnFieldValue $ J.Number 100)
              ]
      let expectedResult = MutationOperationResults 1 (Just [expectedRow])

      response `mutationResponseShouldBe` MutationResponse [expectedResult]
  where
    edgeCaseTest = Test.edgeCaseTest edgeCasesTestData

    mkTableInsertSchema :: [TableInfo] -> TableName -> [(Text, InsertFieldSchema)] -> TableInsertSchema
    mkTableInsertSchema schemaTables tableName insertFields =
      TableInsertSchema
        { _tisTable = tableName,
          _tisPrimaryKey = _tiPrimaryKey $ findTableInfo schemaTables tableName,
          _tisFields = Data.mkFieldsMap insertFields
        }

    findTableInfo :: [TableInfo] -> TableName -> TableInfo
    findTableInfo tables tableName =
      tables
        & find (\TableInfo {..} -> _tiName == tableName)
        & fromMaybe (error $ "Can't find table " <> show tableName <> " in schema")

    mkInsertOperation :: TableName -> InsertMutationOperation
    mkInsertOperation tableName = InsertMutationOperation tableName [] Nothing mempty

    mkFieldsFromExpectedData :: TableName -> [HashMap FieldName FieldValue] -> HashMap FieldName Field
    mkFieldsFromExpectedData tableName expectedRows =
      expectedRows
        & listToMaybe
        & maybe mempty (HashMap.mapWithKey (\fieldName _fieldValue -> _tdColumnField tableName (unFieldName fieldName)))

    artistsQueryRequest :: [Integer] -> QueryRequest
    artistsQueryRequest artistIds =
      let query =
            Data.emptyQuery
              & qFields ?~ mkFieldsFromExpectedData _tdArtistsTableName (expectedInsertedArtists artistsStartingId)
              & qWhere ?~ ApplyBinaryArrayComparisonOperator In (_tdCurrentComparisonColumn "ArtistId" artistIdScalarType) (J.Number . fromInteger <$> artistIds) artistIdScalarType
       in TableQueryRequest _tdArtistsTableName mempty mempty mempty query Nothing

    albumsQueryRequest :: [Integer] -> QueryRequest
    albumsQueryRequest albumIds =
      let query =
            Data.emptyQuery
              & qFields ?~ mkFieldsFromExpectedData _tdAlbumsTableName (expectedInsertedAcdcAlbums albumsStartingId)
              & qWhere ?~ ApplyBinaryArrayComparisonOperator In (_tdCurrentComparisonColumn "AlbumId" albumIdScalarType) (J.Number . fromInteger <$> albumIds) albumIdScalarType
       in TableQueryRequest _tdAlbumsTableName mempty mempty mempty query Nothing

    employeesQueryRequest :: [Integer] -> QueryRequest
    employeesQueryRequest employeeIds =
      let query =
            Data.emptyQuery
              & qFields ?~ mkFieldsFromExpectedData _tdEmployeesTableName (expectedInsertedEmployees employeesStartingId)
              & qWhere ?~ ApplyBinaryArrayComparisonOperator In (_tdCurrentComparisonColumn "EmployeeId" albumIdScalarType) (J.Number . fromInteger <$> employeeIds) employeeIdScalarType
       in TableQueryRequest _tdEmployeesTableName mempty mempty mempty query Nothing

    artistsInsertSchema :: TableInsertSchema
    artistsInsertSchema = _tdMkDefaultTableInsertSchema _tdArtistsTableName

    albumsInsertSchema :: TableInsertSchema
    albumsInsertSchema = _tdMkDefaultTableInsertSchema _tdAlbumsTableName

    employeesInsertSchema :: TableInsertSchema
    employeesInsertSchema = _tdMkDefaultTableInsertSchema _tdEmployeesTableName

    artistsStartingId :: Integer
    artistsStartingId = 276

    albumsStartingId :: Integer
    albumsStartingId = 348

    employeesStartingId :: Integer
    employeesStartingId = 9

    newArtists :: [RowObject]
    newArtists =
      [ RowObject . Data.mkFieldsMap $
          [ ("Name", mkColumnInsertFieldValue $ J.String "Taylor Swift")
          ],
        RowObject . Data.mkFieldsMap $
          [ ("Name", mkColumnInsertFieldValue $ J.String "John Williams")
          ],
        RowObject . Data.mkFieldsMap $
          [ ("Name", mkColumnInsertFieldValue $ J.String "Nightwish")
          ],
        RowObject . Data.mkFieldsMap $
          [ ("Name", mkColumnInsertFieldValue $ J.String "Gareth Emery")
          ]
      ]

    expectedInsertedArtists :: Integer -> [HashMap FieldName FieldValue]
    expectedInsertedArtists startingId =
      Data.insertAutoIncPk "ArtistId" startingId $
        [ Data.mkFieldsMap
            [("Name", mkColumnFieldValue $ J.String "Taylor Swift")],
          Data.mkFieldsMap
            [("Name", mkColumnFieldValue $ J.String "John Williams")],
          Data.mkFieldsMap
            [("Name", mkColumnFieldValue $ J.String "Nightwish")],
          Data.mkFieldsMap
            [("Name", mkColumnFieldValue $ J.String "Gareth Emery")]
        ]

    acdcArtistId :: Scientific
    acdcArtistId = 1

    newAcdcAlbums :: [RowObject]
    newAcdcAlbums =
      [ RowObject . Data.mkFieldsMap $
          [ ("ArtistId", mkColumnInsertFieldValue $ J.Number acdcArtistId),
            ("Title", mkColumnInsertFieldValue $ J.String "Dirty Deeds Done Dirt Cheap")
          ],
        RowObject . Data.mkFieldsMap $
          [ ("ArtistId", mkColumnInsertFieldValue $ J.Number acdcArtistId),
            ("Title", mkColumnInsertFieldValue $ J.String "Highway to Hell")
          ],
        RowObject . Data.mkFieldsMap $
          [ ("ArtistId", mkColumnInsertFieldValue $ J.Number acdcArtistId),
            ("Title", mkColumnInsertFieldValue $ J.String "Ballbreaker")
          ]
      ]

    expectedInsertedAcdcAlbums :: Integer -> [HashMap FieldName FieldValue]
    expectedInsertedAcdcAlbums startingId =
      Data.insertAutoIncPk "AlbumId" startingId $
        [ Data.mkFieldsMap
            [ ("ArtistId", mkColumnFieldValue $ J.Number acdcArtistId),
              ("Title", mkColumnFieldValue $ J.String "Dirty Deeds Done Dirt Cheap")
            ],
          Data.mkFieldsMap
            [ ("ArtistId", mkColumnFieldValue $ J.Number acdcArtistId),
              ("Title", mkColumnFieldValue $ J.String "Highway to Hell")
            ],
          Data.mkFieldsMap
            [ ("ArtistId", mkColumnFieldValue $ J.Number acdcArtistId),
              ("Title", mkColumnFieldValue $ J.String "Ballbreaker")
            ]
        ]

    apocalypticaArtistId :: Scientific
    apocalypticaArtistId = 7

    newApocalypticaAlbums :: [RowObject]
    newApocalypticaAlbums =
      [ RowObject . Data.mkFieldsMap $
          [ ("ArtistId", mkColumnInsertFieldValue $ J.Number apocalypticaArtistId),
            ("Title", mkColumnInsertFieldValue $ J.String "Cult")
          ]
      ]

    expectedInsertedApocalypticaAlbums :: Integer -> [HashMap FieldName FieldValue]
    expectedInsertedApocalypticaAlbums startingId =
      Data.insertAutoIncPk "AlbumId" startingId $
        [ Data.mkFieldsMap
            [ ("ArtistId", mkColumnFieldValue $ J.Number apocalypticaArtistId),
              ("Title", mkColumnFieldValue $ J.String "Cult")
            ]
        ]

    newEmployees :: [RowObject]
    newEmployees =
      [ RowObject . Data.mkFieldsMap $
          [ ("FirstName", mkColumnInsertFieldValue $ J.String "Luke"),
            ("LastName", mkColumnInsertFieldValue $ J.String "Skywalker"),
            ("Email", mkColumnInsertFieldValue $ J.String "luke@rebelalliance.com"),
            ("City", mkColumnInsertFieldValue $ J.String "Mos Eisley"),
            ("Country", mkColumnInsertFieldValue $ J.String "Tatooine"),
            ("ReportsTo", mkColumnInsertFieldValue $ J.Number 3)
          ],
        RowObject . Data.mkFieldsMap $
          [ ("FirstName", mkColumnInsertFieldValue $ J.String "Han"),
            ("LastName", mkColumnInsertFieldValue $ J.String "Solo"),
            ("Title", mkColumnInsertFieldValue $ J.String "Mr")
          ]
      ]

    expectedInsertedEmployees :: Integer -> [HashMap FieldName FieldValue]
    expectedInsertedEmployees startingId =
      Data.insertAutoIncPk "EmployeeId" startingId $
        [ Data.mkFieldsMap
            [ ("FirstName", mkColumnFieldValue $ J.String "Luke"),
              ("LastName", mkColumnFieldValue $ J.String "Skywalker"),
              ("Title", mkColumnFieldValue $ J.Null),
              ("ReportsTo", mkColumnFieldValue $ J.Number 3),
              ("BirthDate", mkColumnFieldValue $ J.Null),
              ("HireDate", mkColumnFieldValue $ J.Null),
              ("Address", mkColumnFieldValue $ J.Null),
              ("City", mkColumnFieldValue $ J.String "Mos Eisley"),
              ("State", mkColumnFieldValue $ J.Null),
              ("Country", mkColumnFieldValue $ J.String "Tatooine"),
              ("PostalCode", mkColumnFieldValue $ J.Null),
              ("Phone", mkColumnFieldValue $ J.Null),
              ("Fax", mkColumnFieldValue $ J.Null),
              ("Email", mkColumnFieldValue $ J.String "luke@rebelalliance.com")
            ],
          Data.mkFieldsMap
            [ ("FirstName", mkColumnFieldValue $ J.String "Han"),
              ("LastName", mkColumnFieldValue $ J.String "Solo"),
              ("Title", mkColumnFieldValue $ J.String "Mr"),
              ("ReportsTo", mkColumnFieldValue $ J.Null),
              ("BirthDate", mkColumnFieldValue $ J.Null),
              ("HireDate", mkColumnFieldValue $ J.Null),
              ("Address", mkColumnFieldValue $ J.Null),
              ("City", mkColumnFieldValue $ J.Null),
              ("State", mkColumnFieldValue $ J.Null),
              ("Country", mkColumnFieldValue $ J.Null),
              ("PostalCode", mkColumnFieldValue $ J.Null),
              ("Phone", mkColumnFieldValue $ J.Null),
              ("Fax", mkColumnFieldValue $ J.Null),
              ("Email", mkColumnFieldValue $ J.Null)
            ]
        ]

    artistIdScalarType = _tdFindColumnScalarType _tdArtistsTableName "ArtistId"
    artistNameScalarType = _tdFindColumnScalarType _tdArtistsTableName "Name"
    albumIdScalarType = _tdFindColumnScalarType _tdAlbumsTableName "AlbumId"
    employeeIdScalarType = _tdFindColumnScalarType _tdEmployeesTableName "EmployeeId"
