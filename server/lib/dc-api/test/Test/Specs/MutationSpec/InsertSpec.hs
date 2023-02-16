module Test.Specs.MutationSpec.InsertSpec (spec) where

import Control.Arrow ((>>>))
import Control.Lens (ix, (&), (.~), (?~), (^?), _Just)
import Control.Monad (when)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Free (Free)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson qualified as J
import Data.Foldable (for_)
import Data.Functor ((<&>))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe, isJust, maybeToList)
import Data.Scientific (Scientific)
import Data.Text qualified as Text
import GHC.Stack (HasCallStack)
import Hasura.Backends.DataConnector.API
import Test.AgentAPI (mutationExpectError, mutationGuarded)
import Test.AgentClient (AgentClientT, HasAgentClient)
import Test.AgentDatasets (HasDatasetContext, chinookTemplate, testingEdgeCasesTemplate, usesDataset)
import Test.AgentTestContext
import Test.Data (EdgeCasesTestData (..), TestData (..))
import Test.Data qualified as Data
import Test.Expectations (mutationResponseShouldBe)
import Test.Sandwich (ExampleT, HasBaseContext, describe, pendingWith, shouldBe)
import Test.Sandwich.Internal (SpecCommand)
import Test.TestHelpers (AgentTestSpec, it)
import Prelude

spec :: TestData -> Maybe EdgeCasesTestData -> Capabilities -> AgentTestSpec
spec TestData {..} edgeCasesTestData Capabilities {..} = describe "Insert Mutations" $ do
  usesDataset chinookTemplate $ it "can insert a single row" $ do
    let insertOperation = artistsInsertOperation & imoRows .~ take 1 newArtists
    let mutationRequest =
          Data.emptyMutationRequest
            & mrOperations .~ [InsertOperation insertOperation]
            & mrInsertSchema .~ [artistsInsertSchema]

    response <- mutationGuarded mutationRequest

    let expectedResult = MutationOperationResults 1 Nothing

    response `mutationResponseShouldBe` MutationResponse [expectedResult]

  usesDataset chinookTemplate $ it "can insert when the field names do not match the column names" $ do
    let row =
          RowObject . Data.mkFieldsMap $
            [ ("artist_name", mkColumnInsertFieldValue $ J.String "Taylor Swift")
            ]
    let insertOperation = artistsInsertOperation & imoRows .~ [row]
    let insertSchema =
          TableInsertSchema _tdArtistsTableName $
            Data.mkFieldsMap
              [ ("artist_name", ColumnInsert (_tdColumnInsertSchema _tdArtistsTableName "Name"))
              ]
    let mutationRequest =
          Data.emptyMutationRequest
            & mrOperations .~ [InsertOperation insertOperation]
            & mrInsertSchema .~ [insertSchema]

    response <- mutationGuarded mutationRequest

    let expectedResult = MutationOperationResults 1 Nothing

    response `mutationResponseShouldBe` MutationResponse [expectedResult]

  usesDataset chinookTemplate $ it "can insert multiple rows" $ do
    let insertOperation = artistsInsertOperation & imoRows .~ newArtists
    let mutationRequest =
          Data.emptyMutationRequest
            & mrOperations .~ [InsertOperation insertOperation]
            & mrInsertSchema .~ [artistsInsertSchema]

    response <- mutationGuarded mutationRequest

    let expectedResult = MutationOperationResults 4 Nothing

    response `mutationResponseShouldBe` MutationResponse [expectedResult]

  usesDataset chinookTemplate $ it "can insert using multiple operations" $ do
    let insertOperation1 = artistsInsertOperation & imoRows .~ take 1 newArtists
    let insertOperation2 = artistsInsertOperation & imoRows .~ drop 1 newArtists
    let insertOperation3 = albumsInsertOperation & imoRows .~ take 2 newAcdcAlbums
    let mutationRequest =
          Data.emptyMutationRequest
            & mrOperations .~ [InsertOperation insertOperation1, InsertOperation insertOperation2, InsertOperation insertOperation3]
            & mrInsertSchema .~ [albumsInsertSchema, artistsInsertSchema]

    response <- mutationGuarded mutationRequest

    let expectedResult1 = MutationOperationResults 1 Nothing
    let expectedResult2 = MutationOperationResults 3 Nothing
    let expectedResult3 = MutationOperationResults 2 Nothing

    response `mutationResponseShouldBe` MutationResponse [expectedResult1, expectedResult2, expectedResult3]

  usesDataset chinookTemplate $ it "can insert multiple rows with differing column sets" $ do
    let insertOperation = employeesInsertOperation & imoRows .~ newEmployees
    let mutationRequest =
          Data.emptyMutationRequest
            & mrOperations .~ [InsertOperation insertOperation]
            & mrInsertSchema .~ [employeesInsertSchema]

    response <- mutationGuarded mutationRequest

    let expectedResult = MutationOperationResults 2 Nothing

    response `mutationResponseShouldBe` MutationResponse [expectedResult]

  describe "post-insert checks" $ do
    usesDataset chinookTemplate $ it "can insert when post insert check passes" $ do
      let rows = take 1 newAcdcAlbums ++ take 1 newApocalypticaAlbums
      let insertOperation =
            albumsInsertOperation
              & imoRows .~ rows
              & imoPostInsertCheck
                ?~ Or
                  [ ApplyBinaryComparisonOperator Equal (_tdCurrentComparisonColumn "ArtistId" artistIdScalarType) (ScalarValue (J.Number acdcArtistId) artistIdScalarType),
                    ApplyBinaryComparisonOperator Equal (_tdCurrentComparisonColumn "ArtistId" artistIdScalarType) (ScalarValue (J.Number apocalypticaArtistId) artistIdScalarType)
                  ]
      let mutationRequest =
            Data.emptyMutationRequest
              & mrOperations .~ [InsertOperation insertOperation]
              & mrInsertSchema .~ [albumsInsertSchema]

      response <- mutationGuarded mutationRequest

      let expectedResult = MutationOperationResults 2 Nothing

      response `mutationResponseShouldBe` MutationResponse [expectedResult]

    usesDataset chinookTemplate $ it "fails to insert when post insert check fails" $ do
      let rows = take 1 newAcdcAlbums ++ take 1 newApocalypticaAlbums
      let insertOperation =
            albumsInsertOperation
              & imoRows .~ rows
              & imoPostInsertCheck ?~ ApplyBinaryComparisonOperator Equal (_tdCurrentComparisonColumn "ArtistId" artistIdScalarType) (ScalarValue (J.Number acdcArtistId) artistIdScalarType)
      let mutationRequest =
            Data.emptyMutationRequest
              & mrOperations .~ [InsertOperation insertOperation]
              & mrInsertSchema .~ [albumsInsertSchema]

      response <- mutationExpectError mutationRequest

      _crType response `shouldBe` MutationPermissionCheckFailure

    when (isJust $ _cComparisons >>= _ccSubqueryComparisonCapabilities) $ do
      usesDataset chinookTemplate $ it "can insert when post insert check against unrelated table passes" $ do
        let rows = take 1 newAcdcAlbums ++ take 1 newApocalypticaAlbums
        let someEmployeeIdThatExists = 1
        let insertOperation =
              albumsInsertOperation
                & imoRows .~ rows
                & imoPostInsertCheck
                  ?~ Exists
                    (UnrelatedTable _tdEmployeesTableName)
                    (ApplyBinaryComparisonOperator Equal (_tdCurrentComparisonColumn "EmployeeId" employeeIdScalarType) (ScalarValue (J.Number someEmployeeIdThatExists) employeeIdScalarType))
        let mutationRequest =
              Data.emptyMutationRequest
                & mrOperations .~ [InsertOperation insertOperation]
                & mrInsertSchema .~ [albumsInsertSchema]
                & mrTableRelationships .~ [Data.onlyKeepRelationships [_tdArtistRelationshipName] _tdAlbumsTableRelationships]

        response <- mutationGuarded mutationRequest

        let expectedResult = MutationOperationResults 2 Nothing

        response `mutationResponseShouldBe` MutationResponse [expectedResult]

      usesDataset chinookTemplate $ it "fails to insert when post insert check against unrelated table fails" $ do
        let rows = take 1 newAcdcAlbums ++ take 1 newApocalypticaAlbums
        let someEmployeeIdThatDoesNotExist = 50
        let insertOperation =
              albumsInsertOperation
                & imoRows .~ rows
                & imoPostInsertCheck
                  ?~ Exists
                    (UnrelatedTable _tdEmployeesTableName)
                    (ApplyBinaryComparisonOperator Equal (_tdCurrentComparisonColumn "EmployeeId" employeeIdScalarType) (ScalarValue (J.Number someEmployeeIdThatDoesNotExist) employeeIdScalarType))
        let mutationRequest =
              Data.emptyMutationRequest
                & mrOperations .~ [InsertOperation insertOperation]
                & mrInsertSchema .~ [albumsInsertSchema]
                & mrTableRelationships .~ [Data.onlyKeepRelationships [_tdArtistRelationshipName] _tdAlbumsTableRelationships]

        response <- mutationExpectError mutationRequest

        _crType response `shouldBe` MutationPermissionCheckFailure

    when ((_cComparisons >>= _ccSubqueryComparisonCapabilities <&> _ctccSupportsRelations) == Just True) $ do
      usesDataset chinookTemplate $ it "can insert when post insert check against related table passes" $ do
        let rows = take 1 newAcdcAlbums ++ take 1 newApocalypticaAlbums
        let insertOperation =
              albumsInsertOperation
                & imoRows .~ rows
                & imoPostInsertCheck
                  ?~ Exists
                    (RelatedTable _tdArtistRelationshipName)
                    ( Or
                        [ ApplyBinaryComparisonOperator Equal (_tdCurrentComparisonColumn "Name" artistNameScalarType) (ScalarValue (J.String "AC/DC") artistNameScalarType),
                          ApplyBinaryComparisonOperator Equal (_tdCurrentComparisonColumn "Name" artistNameScalarType) (ScalarValue (J.String "Apocalyptica") artistNameScalarType)
                        ]
                    )
        let mutationRequest =
              Data.emptyMutationRequest
                & mrOperations .~ [InsertOperation insertOperation]
                & mrInsertSchema .~ [albumsInsertSchema]
                & mrTableRelationships .~ [Data.onlyKeepRelationships [_tdArtistRelationshipName] _tdAlbumsTableRelationships]

        response <- mutationGuarded mutationRequest

        let expectedResult = MutationOperationResults 2 Nothing

        response `mutationResponseShouldBe` MutationResponse [expectedResult]

      usesDataset chinookTemplate $ it "fails to insert when post insert check against related table fails" $ do
        let rows = take 1 newAcdcAlbums ++ take 1 newApocalypticaAlbums
        let insertOperation =
              albumsInsertOperation
                & imoRows .~ rows
                & imoPostInsertCheck
                  ?~ Exists
                    (RelatedTable _tdArtistRelationshipName)
                    (ApplyBinaryComparisonOperator Equal (_tdCurrentComparisonColumn "Name" artistNameScalarType) (ScalarValue (J.String "AC/DC") artistNameScalarType))
        let mutationRequest =
              Data.emptyMutationRequest
                & mrOperations .~ [InsertOperation insertOperation]
                & mrInsertSchema .~ [albumsInsertSchema]
                & mrTableRelationships .~ [Data.onlyKeepRelationships [_tdArtistRelationshipName] _tdAlbumsTableRelationships]

        response <- mutationExpectError mutationRequest

        _crType response `shouldBe` MutationPermissionCheckFailure

  describe "returning" $ do
    usesDataset chinookTemplate $ it "can return all inserted columns including the auto-generated primary key" $ do
      let returning =
            Data.mkFieldsMap
              [ ("AlbumId", _tdColumnField _tdAlbumsTableName "AlbumId"),
                ("ArtistId", _tdColumnField _tdAlbumsTableName "ArtistId"),
                ("Title", _tdColumnField _tdAlbumsTableName "Title")
              ]
      let insertOperation =
            albumsInsertOperation
              & imoRows .~ newAcdcAlbums
              & imoReturningFields .~ returning
      let mutationRequest =
            Data.emptyMutationRequest
              & mrOperations .~ [InsertOperation insertOperation]
              & mrInsertSchema .~ [albumsInsertSchema]

      response <- mutationGuarded mutationRequest

      let expectedResult = MutationOperationResults 3 (Just (expectedInsertedAcdcAlbums albumsStartingId))

      response `mutationResponseShouldBe` MutationResponse [expectedResult]

    usesDataset chinookTemplate $ it "can insert rows with missing nullable fields and return nulls for those fields" $ do
      let returning =
            ["EmployeeId", "FirstName", "LastName", "Title", "ReportsTo", "BirthDate", "HireDate", "Address", "City", "State", "Country", "PostalCode", "Phone", "Fax", "Email"]
              & fmap (\column -> (column, _tdColumnField _tdEmployeesTableName column))
              & Data.mkFieldsMap
      let insertOperation =
            employeesInsertOperation
              & imoRows .~ newEmployees
              & imoReturningFields .~ returning
      let mutationRequest =
            Data.emptyMutationRequest
              & mrOperations .~ [InsertOperation insertOperation]
              & mrInsertSchema .~ [employeesInsertSchema]

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
              albumsInsertOperation
                & imoRows .~ rows
                & imoReturningFields .~ returning

        let mutationRequest =
              Data.emptyMutationRequest
                & mrOperations .~ [InsertOperation insertOperation]
                & mrInsertSchema .~ [albumsInsertSchema]
                & mrTableRelationships .~ [Data.onlyKeepRelationships [_tdArtistRelationshipName] _tdAlbumsTableRelationships]

        response <- mutationGuarded mutationRequest

        let joinInArtist (album :: HashMap FieldName FieldValue) =
              let artist = (album ^? Data.field "ArtistId" . Data._ColumnFieldNumber) >>= \artistId -> _tdArtistsRowsById ^? ix artistId
                  artistPropVal = maybeToList artist
               in Data.insertField "Artist" (mkSubqueryResponse artistPropVal) album

        let removeArtistId = Data.deleteField "ArtistId"

        let expectedRows =
              ( take 1 (expectedInsertedAcdcAlbums albumsStartingId)
                  ++ take 1 (expectedInsertedApocalypticaAlbums (albumsStartingId + 1))
              )
                & fmap (joinInArtist >>> removeArtistId)

        let expectedResult = MutationOperationResults 2 (Just expectedRows)

        response `mutationResponseShouldBe` MutationResponse [expectedResult]

      usesDataset chinookTemplate $ it "can return rows from an array relationship" $ do
        let rows = take 1 newApocalypticaAlbums
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
              albumsInsertOperation
                & imoRows .~ rows
                & imoReturningFields .~ returning
        let mutationRequest =
              Data.emptyMutationRequest
                & mrOperations .~ [InsertOperation insertOperation]
                & mrInsertSchema .~ [albumsInsertSchema]
                & mrTableRelationships
                  .~ [ Data.onlyKeepRelationships [_tdArtistRelationshipName] _tdAlbumsTableRelationships,
                       Data.onlyKeepRelationships [_tdAlbumsRelationshipName] _tdArtistsTableRelationships
                     ]

        response <- mutationGuarded mutationRequest

        let expectedNewAlbums = expectedInsertedApocalypticaAlbums albumsStartingId

        let joinInAlbums (artist :: HashMap FieldName FieldValue) =
              let albums = fromMaybe [] $ do
                    artistId <- artist ^? Data.field "ArtistId" . Data._ColumnFieldNumber
                    (expectedNewAlbums ++ _tdAlbumsRows)
                      & filter (\album -> album ^? Data.field "ArtistId" . Data._ColumnFieldNumber == Just artistId)
                      & sortOn (^? Data.field "AlbumId")
                      & pure
                  shapedAlbums = Data.deleteField "ArtistId" <$> albums
               in Data.insertField "Albums" (mkSubqueryResponse shapedAlbums) artist

        let joinInArtist (album :: HashMap FieldName FieldValue) =
              let artist = (album ^? Data.field "ArtistId" . Data._ColumnFieldNumber) >>= \artistId -> _tdArtistsRowsById ^? ix artistId
                  artistWithAlbums = joinInAlbums <$> artist
               in Data.insertField "Artist" (mkSubqueryResponse (maybeToList artistWithAlbums)) album
        let removeArtistId = Data.deleteField "ArtistId"

        let expectedRows =
              expectedNewAlbums
                & fmap (joinInArtist >>> removeArtistId)

        let expectedResult = MutationOperationResults 1 (Just expectedRows)

        response `mutationResponseShouldBe` MutationResponse [expectedResult]

  describe "edge cases" $ do
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
      let insertSchema =
            TableInsertSchema _ectdNoPrimaryKeyTableName $
              HashMap.fromList
                [ (FieldName "FirstName", ColumnInsert (_ectdColumnInsertSchema _ectdNoPrimaryKeyTableName "FirstName")),
                  (FieldName "LastName", ColumnInsert (_ectdColumnInsertSchema _ectdNoPrimaryKeyTableName "LastName"))
                ]
      let mutationRequest =
            Data.emptyMutationRequest
              & mrOperations .~ [InsertOperation insertOperation]
              & mrInsertSchema .~ [insertSchema]

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
      let insertSchema =
            TableInsertSchema _ectdDefaultedPrimaryKeyTableName $
              HashMap.fromList
                [ (FieldName "Message", ColumnInsert (_ectdColumnInsertSchema _ectdDefaultedPrimaryKeyTableName "Message"))
                ]
      let mutationRequest =
            Data.emptyMutationRequest
              & mrOperations .~ [InsertOperation insertOperation]
              & mrInsertSchema .~ [insertSchema]

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
      let insertSchema =
            TableInsertSchema _ectdAllColumnsDefaultableTableName $ HashMap.fromList []
      let mutationRequest =
            Data.emptyMutationRequest
              & mrOperations .~ [InsertOperation insertOperation]
              & mrInsertSchema .~ [insertSchema]

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
    edgeCaseTest ::
      (HasCallStack, HasAgentClient context, HasAgentTestContext context, HasBaseContext context, MonadThrow m, MonadIO m) =>
      (EdgeCasesTestData -> TableName) ->
      String ->
      (forall testContext. (HasBaseContext testContext, HasAgentClient testContext, HasAgentTestContext testContext, HasDatasetContext testContext) => EdgeCasesTestData -> AgentClientT (ExampleT testContext m) ()) ->
      Free (SpecCommand context m) ()
    edgeCaseTest expectedTable name test = do
      case edgeCasesTestData of
        Nothing -> it name $ pendingWith (testingEdgeCasesTemplateName <> " dataset template does not exist")
        Just edgeCasesTestData'@EdgeCasesTestData {..} ->
          if _ectdTableExists (expectedTable edgeCasesTestData')
            then usesDataset testingEdgeCasesTemplate $ it name $ test edgeCasesTestData'
            else it name $ pendingWith (Text.unpack (tableNameToText (expectedTable edgeCasesTestData')) <> " table does not exist within the " <> testingEdgeCasesTemplateName <> " dataset")
      where
        testingEdgeCasesTemplateName = Text.unpack (_unDatasetTemplateName testingEdgeCasesTemplate)

    mkSubqueryResponse :: [HashMap FieldName FieldValue] -> FieldValue
    mkSubqueryResponse rows =
      mkRelationshipFieldValue $ QueryResponse (Just rows) Nothing

    mkInsertOperation :: TableName -> InsertMutationOperation
    mkInsertOperation tableName = InsertMutationOperation tableName [] Nothing mempty

    artistsInsertOperation :: InsertMutationOperation
    artistsInsertOperation = InsertMutationOperation _tdArtistsTableName [] Nothing mempty

    albumsInsertOperation :: InsertMutationOperation
    albumsInsertOperation = InsertMutationOperation _tdAlbumsTableName [] Nothing mempty

    employeesInsertOperation :: InsertMutationOperation
    employeesInsertOperation = InsertMutationOperation _tdEmployeesTableName [] Nothing mempty

    artistsInsertSchema :: TableInsertSchema
    artistsInsertSchema =
      TableInsertSchema _tdArtistsTableName $
        HashMap.fromList
          [ (FieldName "Name", ColumnInsert (_tdColumnInsertSchema _tdArtistsTableName "Name"))
          ]

    albumsInsertSchema :: TableInsertSchema
    albumsInsertSchema =
      TableInsertSchema _tdAlbumsTableName $
        HashMap.fromList
          [ (FieldName "ArtistId", ColumnInsert (_tdColumnInsertSchema _tdAlbumsTableName "ArtistId")),
            (FieldName "Title", ColumnInsert (_tdColumnInsertSchema _tdAlbumsTableName "Title"))
          ]

    employeesInsertSchema :: TableInsertSchema
    employeesInsertSchema =
      TableInsertSchema _tdEmployeesTableName $
        HashMap.fromList
          [ (FieldName "FirstName", ColumnInsert (_tdColumnInsertSchema _tdEmployeesTableName "FirstName")),
            (FieldName "LastName", ColumnInsert (_tdColumnInsertSchema _tdEmployeesTableName "LastName")),
            (FieldName "Title", ColumnInsert (_tdColumnInsertSchema _tdEmployeesTableName "Title")),
            (FieldName "Email", ColumnInsert (_tdColumnInsertSchema _tdEmployeesTableName "Email")),
            (FieldName "City", ColumnInsert (_tdColumnInsertSchema _tdEmployeesTableName "City")),
            (FieldName "Country", ColumnInsert (_tdColumnInsertSchema _tdEmployeesTableName "Country"))
          ]

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
            ("Country", mkColumnInsertFieldValue $ J.String "Tatooine")
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
              ("ReportsTo", mkColumnFieldValue $ J.Null),
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
    employeeIdScalarType = _tdFindColumnScalarType _tdEmployeesTableName "EmployeeId"
