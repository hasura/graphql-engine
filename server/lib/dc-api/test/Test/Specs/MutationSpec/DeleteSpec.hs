module Test.Specs.MutationSpec.DeleteSpec (spec) where

import Control.Lens (ix, (&), (.~), (?~), (^?))
import Control.Monad (when)
import Data.Aeson qualified as J
import Data.Foldable (for_)
import Data.Functor ((<&>))
import Data.HashMap.Strict (HashMap)
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe, maybeToList)
import Data.Set qualified as Set
import Hasura.Backends.DataConnector.API
import Test.AgentAPI (mutationGuarded, queryGuarded)
import Test.AgentDatasets (chinookTemplate, usesDataset)
import Test.Data (EdgeCasesTestData (..), TestData (..))
import Test.Data qualified as Data
import Test.Expectations (mutationResponseShouldBe, rowsShouldBe)
import Test.Sandwich (describe)
import Test.TestHelpers (AgentTestSpec, it)
import Test.TestHelpers qualified as Test
import Prelude

spec :: TestData -> Maybe EdgeCasesTestData -> Capabilities -> AgentTestSpec
spec TestData {..} edgeCasesTestData Capabilities {..} = describe "Delete Mutations" $ do
  usesDataset chinookTemplate $ it "can delete all rows" $ do
    let deleteOperation = mkDeleteOperation _tdInvoiceLinesTableName
    let mutationRequest = Data.emptyMutationRequest & mrOperations .~ [DeleteOperation deleteOperation]

    response <- mutationGuarded mutationRequest

    let expectedResult = MutationOperationResults 2240 Nothing
    response `mutationResponseShouldBe` MutationResponse [expectedResult]

    receivedInvoiceLines <- Data.sortResponseRowsBy "InvoiceLineId" <$> queryGuarded invoiceLinesQueryRequest
    Data.responseRows receivedInvoiceLines `rowsShouldBe` []

  usesDataset chinookTemplate $ it "can delete a specific row" $ do
    let whereExp = ApplyBinaryComparisonOperator Equal (_tdCurrentComparisonColumn "InvoiceLineId" invoiceLineIdScalarType) (Data.scalarValueComparison (J.Number 420) invoiceLineIdScalarType)
    let deleteOperation =
          mkDeleteOperation _tdInvoiceLinesTableName
            & dmoWhere ?~ whereExp
    let mutationRequest = Data.emptyMutationRequest & mrOperations .~ [DeleteOperation deleteOperation]

    response <- mutationGuarded mutationRequest

    let expectedResult = MutationOperationResults 1 Nothing
    response `mutationResponseShouldBe` MutationResponse [expectedResult]

    let expectedRemainingRows =
          _tdInvoiceLinesRows
            & filter (\invoiceLine -> invoiceLine ^? Data.field "InvoiceLineId" . Data._ColumnFieldNumber /= Just 420)

    receivedInvoiceLines <- Data.sortResponseRowsBy "InvoiceLineId" <$> queryGuarded invoiceLinesQueryRequest
    Data.responseRows receivedInvoiceLines `rowsShouldBe` expectedRemainingRows

  usesDataset chinookTemplate $ it "can delete a range of rows" $ do
    let whereExp =
          Data.mkAndExpr
            [ ApplyBinaryComparisonOperator GreaterThan (_tdCurrentComparisonColumn "InvoiceLineId" invoiceLineIdScalarType) (Data.scalarValueComparison (J.Number 10) invoiceLineIdScalarType),
              ApplyBinaryComparisonOperator LessThanOrEqual (_tdCurrentComparisonColumn "InvoiceLineId" invoiceLineIdScalarType) (Data.scalarValueComparison (J.Number 20) invoiceLineIdScalarType)
            ]
    let deleteOperation =
          mkDeleteOperation _tdInvoiceLinesTableName
            & dmoWhere ?~ whereExp
    let mutationRequest = Data.emptyMutationRequest & mrOperations .~ [DeleteOperation deleteOperation]

    response <- mutationGuarded mutationRequest

    let expectedResult = MutationOperationResults 10 Nothing
    response `mutationResponseShouldBe` MutationResponse [expectedResult]

    let expectedRemainingRows =
          _tdInvoiceLinesRows
            & filter
              ( \invoiceLine ->
                  invoiceLine ^? Data.field "InvoiceLineId" . Data._ColumnFieldNumber <= Just 10
                    || invoiceLine ^? Data.field "InvoiceLineId" . Data._ColumnFieldNumber > Just 20
              )

    receivedInvoiceLines <- Data.sortResponseRowsBy "InvoiceLineId" <$> queryGuarded invoiceLinesQueryRequest
    Data.responseRows receivedInvoiceLines `rowsShouldBe` expectedRemainingRows

  usesDataset chinookTemplate $ it "can perform multiple delete operations" $ do
    let whereExp1 = ApplyBinaryComparisonOperator LessThan (_tdCurrentComparisonColumn "InvoiceId" invoiceIdScalarType) (Data.scalarValueComparison (J.Number 3) invoiceIdScalarType)
    let whereExp2 = ApplyBinaryComparisonOperator GreaterThan (_tdCurrentComparisonColumn "InvoiceId" invoiceIdScalarType) (Data.scalarValueComparison (J.Number 410) invoiceIdScalarType)
    let deleteOperation1 =
          mkDeleteOperation _tdInvoiceLinesTableName
            & dmoWhere ?~ whereExp1
    let deleteOperation2 =
          mkDeleteOperation _tdInvoiceLinesTableName
            & dmoWhere ?~ whereExp2
    let mutationRequest = Data.emptyMutationRequest & mrOperations .~ [DeleteOperation deleteOperation1, DeleteOperation deleteOperation2]

    response <- mutationGuarded mutationRequest

    let expectedResult1 = MutationOperationResults 6 Nothing
    let expectedResult2 = MutationOperationResults 15 Nothing
    response `mutationResponseShouldBe` MutationResponse [expectedResult1, expectedResult2]

    let expectedRemainingRows =
          _tdInvoiceLinesRows
            & filter
              ( \invoiceLine ->
                  invoiceLine ^? Data.field "InvoiceId" . Data._ColumnFieldNumber >= Just 3
                    && invoiceLine ^? Data.field "InvoiceId" . Data._ColumnFieldNumber <= Just 410
              )

    receivedInvoiceLines <- Data.sortResponseRowsBy "InvoiceLineId" <$> queryGuarded invoiceLinesQueryRequest
    Data.responseRows receivedInvoiceLines `rowsShouldBe` expectedRemainingRows

  when ((_cComparisons >>= _ccSubqueryComparisonCapabilities <&> _ctccSupportsRelations) == Just True) $ do
    usesDataset chinookTemplate $ it "can delete rows filtered by a related table" $ do
      let whereExp =
            Exists (RelatedTable _tdTrackRelationshipName) $
              ApplyBinaryComparisonOperator Equal (_tdCurrentComparisonColumn "Composer" composerScalarType) (Data.scalarValueComparison (J.String "Eric Clapton") composerScalarType)
      let deleteOperation =
            mkDeleteOperation _tdInvoiceLinesTableName
              & dmoWhere ?~ whereExp
      let tableRelationships = Set.singleton $ RTable $ Data.onlyKeepRelationships [_tdTrackRelationshipName] _tdInvoiceLinesTableRelationships
      let mutationRequest =
            Data.emptyMutationRequest
              & mrOperations .~ [DeleteOperation deleteOperation]
              & mrRelationships .~ tableRelationships

      response <- mutationGuarded mutationRequest

      let expectedResult = MutationOperationResults 3 Nothing
      response `mutationResponseShouldBe` MutationResponse [expectedResult]

      let expectedRemainingRows =
            _tdInvoiceLinesRows
              & filter
                ( \invoiceLine -> fromMaybe True $ do
                    trackId <- invoiceLine ^? Data.field "TrackId" . Data._ColumnFieldNumber
                    track <- _tdTracksRowsById ^? ix trackId
                    pure $ track ^? Data.field "Composer" . Data._ColumnFieldString /= Just "Eric Clapton"
                )

      receivedInvoiceLines <- Data.sortResponseRowsBy "InvoiceLineId" <$> queryGuarded (invoiceLinesQueryRequest & qrRelationships .~ tableRelationships)
      Data.responseRows receivedInvoiceLines `rowsShouldBe` expectedRemainingRows

  for_ (_cMutations >>= _mcReturningCapabilities) $ \_returningCapabilities -> describe "returning" $ do
    usesDataset chinookTemplate $ it "returns deleted rows" $ do
      let whereExp = ApplyBinaryComparisonOperator Equal (_tdCurrentComparisonColumn "InvoiceId" invoiceIdScalarType) (Data.scalarValueComparison (J.Number 10) invoiceIdScalarType)
      let deleteOperation =
            mkDeleteOperation _tdInvoiceLinesTableName
              & dmoWhere ?~ whereExp
              & dmoReturningFields .~ invoiceLinesFields
      let mutationRequest = Data.emptyMutationRequest & mrOperations .~ [DeleteOperation deleteOperation]

      response <- mutationGuarded mutationRequest

      let expectedDeletedRows =
            _tdInvoiceLinesRows
              & filter (\invoiceLine -> invoiceLine ^? Data.field "InvoiceId" . Data._ColumnFieldNumber == Just 10)

      let expectedResult = MutationOperationResults 6 (Just expectedDeletedRows)
      response `mutationResponseShouldBe` MutationResponse [expectedResult]

    for_ _cRelationships $ \_relationshipCapabilities -> do
      usesDataset chinookTemplate $ it "can return deleted rows joined to additional rows from an object relationship" $ do
        let whereExp = ApplyBinaryComparisonOperator Equal (_tdCurrentComparisonColumn "InvoiceId" invoiceIdScalarType) (Data.scalarValueComparison (J.Number 37) invoiceIdScalarType)
        let deleteOperation =
              mkDeleteOperation _tdInvoiceLinesTableName
                & dmoWhere ?~ whereExp
                & dmoReturningFields
                  .~ invoiceLinesFields
                    <> Data.mkFieldsMap
                      [ ( "Track",
                          ( RelField
                              ( RelationshipField _tdTrackRelationshipName $
                                  Data.emptyQuery
                                    & qFields
                                      ?~ Data.mkFieldsMap
                                        [ ("TrackId", _tdColumnField _tdTracksTableName "TrackId"),
                                          ("Name", _tdColumnField _tdTracksTableName "Name")
                                        ]
                              )
                          )
                        )
                      ]
        let tableRelationships = Set.singleton $ RTable $ Data.onlyKeepRelationships [_tdTrackRelationshipName] _tdInvoiceLinesTableRelationships
        let mutationRequest =
              Data.emptyMutationRequest
                & mrOperations .~ [DeleteOperation deleteOperation]
                & mrRelationships .~ tableRelationships

        response <- mutationGuarded mutationRequest

        let joinInTrack (invoiceLine :: HashMap FieldName FieldValue) =
              let track = (invoiceLine ^? Data.field "TrackId" . Data._ColumnFieldNumber) >>= \trackId -> _tdTracksRowsById ^? ix trackId
                  trackTrimmed = Data.filterColumns ["TrackId", "Name"] $ maybeToList track
               in Data.insertField "Track" (Data.mkSubqueryRowsFieldValue trackTrimmed) invoiceLine

        let expectedDeletedRows =
              _tdInvoiceLinesRows
                & filter (\invoiceLine -> invoiceLine ^? Data.field "InvoiceId" . Data._ColumnFieldNumber == Just 37)
                & fmap joinInTrack

        let expectedResult = MutationOperationResults 4 (Just expectedDeletedRows)
        response `mutationResponseShouldBe` MutationResponse [expectedResult]

      usesDataset chinookTemplate $ it "can return deleted rows joined to additional rows from an array relationship" $ do
        let whereExp = ApplyBinaryComparisonOperator Equal (_tdCurrentComparisonColumn "InvoiceLineId" invoiceLineIdScalarType) (Data.scalarValueComparison (J.Number 2) invoiceLineIdScalarType)
        let deleteOperation =
              mkDeleteOperation _tdInvoiceLinesTableName
                & dmoWhere ?~ whereExp
                & dmoReturningFields
                  .~ invoiceLinesFields
                    <> Data.mkFieldsMap
                      [ ( "Track",
                          ( RelField
                              ( RelationshipField _tdTrackRelationshipName $
                                  Data.emptyQuery
                                    & qFields
                                      ?~ Data.mkFieldsMap
                                        [ ("TrackId", _tdColumnField _tdTracksTableName "TrackId"),
                                          ("Name", _tdColumnField _tdTracksTableName "Name"),
                                          ( "PlaylistTracks",
                                            ( RelField
                                                ( RelationshipField _tdPlaylistTracksRelationshipName $
                                                    Data.emptyQuery
                                                      & qFields
                                                        ?~ Data.mkFieldsMap
                                                          [ ("PlaylistId", _tdColumnField _tdPlaylistTracksTableName "PlaylistId"),
                                                            ("TrackId", _tdColumnField _tdPlaylistTracksTableName "TrackId")
                                                          ]
                                                      & qOrderBy
                                                        ?~ OrderBy
                                                          mempty
                                                          ( NonEmpty.fromList
                                                              [ _tdOrderByColumn [] "PlaylistId" Ascending,
                                                                _tdOrderByColumn [] "TrackId" Ascending
                                                              ]
                                                          )
                                                )
                                            )
                                          )
                                        ]
                              )
                          )
                        )
                      ]
        let tableRelationships =
              Set.fromList
                [ RTable $ Data.onlyKeepRelationships [_tdTrackRelationshipName] _tdInvoiceLinesTableRelationships,
                  RTable $ Data.onlyKeepRelationships [_tdPlaylistTracksRelationshipName] _tdTracksTableRelationships
                ]
        let mutationRequest =
              Data.emptyMutationRequest
                & mrOperations .~ [DeleteOperation deleteOperation]
                & mrRelationships .~ tableRelationships

        response <- mutationGuarded mutationRequest

        let joinInPlaylistTracks (track :: HashMap FieldName FieldValue) =
              let trackId = track ^? Data.field "TrackId" . Data._ColumnFieldNumber
                  playlistTracks =
                    _tdPlaylistTracksRows
                      & filter (\playlistTrack -> playlistTrack ^? Data.field "TrackId" . Data._ColumnFieldNumber == trackId)
                      & sortOn (\playlistTrack -> (playlistTrack ^? Data.field "PlaylistId", playlistTrack ^? Data.field "TrackId"))
               in Data.insertField "PlaylistTracks" (Data.mkSubqueryRowsFieldValue playlistTracks) track

        let joinInTrack (invoiceLine :: HashMap FieldName FieldValue) =
              let track = (invoiceLine ^? Data.field "TrackId" . Data._ColumnFieldNumber) >>= \trackId -> _tdTracksRowsById ^? ix trackId
                  trackTrimmed = Data.filterColumns ["TrackId", "Name"] $ maybeToList track
               in Data.insertField "Track" (Data.mkSubqueryRowsFieldValue (joinInPlaylistTracks <$> trackTrimmed)) invoiceLine

        let expectedDeletedRows =
              _tdInvoiceLinesRows
                & filter (\invoiceLine -> invoiceLine ^? Data.field "InvoiceLineId" . Data._ColumnFieldNumber == Just 2)
                & fmap joinInTrack

        let expectedResult = MutationOperationResults 1 (Just expectedDeletedRows)
        response `mutationResponseShouldBe` MutationResponse [expectedResult]

      usesDataset chinookTemplate $ it "can return aggregates of array relationships joined to deleted rows" $ do
        let whereExp = ApplyBinaryComparisonOperator Equal (_tdCurrentComparisonColumn "InvoiceLineId" invoiceLineIdScalarType) (Data.scalarValueComparison (J.Number 2) invoiceLineIdScalarType)
        let deleteOperation =
              mkDeleteOperation _tdInvoiceLinesTableName
                & dmoWhere ?~ whereExp
                & dmoReturningFields
                  .~ invoiceLinesFields
                    <> Data.mkFieldsMap
                      [ ( "Track",
                          ( RelField
                              ( RelationshipField _tdTrackRelationshipName $
                                  Data.emptyQuery
                                    & qFields
                                      ?~ Data.mkFieldsMap
                                        [ ("TrackId", _tdColumnField _tdTracksTableName "TrackId"),
                                          ("Name", _tdColumnField _tdTracksTableName "Name"),
                                          ( "PlaylistTracks",
                                            ( RelField
                                                ( RelationshipField _tdPlaylistTracksRelationshipName $
                                                    Data.emptyQuery & qAggregates ?~ Data.mkFieldsMap [("PlaylistTrackCount", StarCount)]
                                                )
                                            )
                                          )
                                        ]
                              )
                          )
                        )
                      ]
        let tableRelationships =
              Set.fromList
                [ RTable $ Data.onlyKeepRelationships [_tdTrackRelationshipName] _tdInvoiceLinesTableRelationships,
                  RTable $ Data.onlyKeepRelationships [_tdPlaylistTracksRelationshipName] _tdTracksTableRelationships
                ]
        let mutationRequest =
              Data.emptyMutationRequest
                & mrOperations .~ [DeleteOperation deleteOperation]
                & mrRelationships .~ tableRelationships

        response <- mutationGuarded mutationRequest

        let joinInPlaylistTracks (track :: HashMap FieldName FieldValue) =
              let trackId = track ^? Data.field "TrackId" . Data._ColumnFieldNumber
                  playlistTracksCount = _tdPlaylistTracksRows & filter (\playlistTrack -> playlistTrack ^? Data.field "TrackId" . Data._ColumnFieldNumber == trackId) & length
                  aggregates = Data.mkFieldsMap [("PlaylistTrackCount", J.Number $ fromIntegral playlistTracksCount)]
               in Data.insertField "PlaylistTracks" (Data.mkSubqueryAggregatesFieldValue aggregates) track

        let joinInTrack (invoiceLine :: HashMap FieldName FieldValue) =
              let track = (invoiceLine ^? Data.field "TrackId" . Data._ColumnFieldNumber) >>= \trackId -> _tdTracksRowsById ^? ix trackId
                  trackTrimmed = Data.filterColumns ["TrackId", "Name"] $ maybeToList track
               in Data.insertField "Track" (Data.mkSubqueryRowsFieldValue (joinInPlaylistTracks <$> trackTrimmed)) invoiceLine

        let expectedDeletedRows =
              _tdInvoiceLinesRows
                & filter (\invoiceLine -> invoiceLine ^? Data.field "InvoiceLineId" . Data._ColumnFieldNumber == Just 2)
                & fmap joinInTrack

        let expectedResult = MutationOperationResults 1 (Just expectedDeletedRows)
        response `mutationResponseShouldBe` MutationResponse [expectedResult]

      usesDataset chinookTemplate $ it "deleted rows are not returned when returning them again across a relationship" $ do
        -- In this scenario we delete two invoice lines off an invoice, then we
        -- return those deleted lines, joining to their invoice and then back again
        -- to the invoice's lines, which should _not_ contain the two lines that were deleted
        let whereExp = ApplyBinaryArrayComparisonOperator In (_tdCurrentComparisonColumn "InvoiceLineId" invoiceLineIdScalarType) [J.Number 4, J.Number 5] invoiceLineIdScalarType
        let deleteOperation =
              mkDeleteOperation _tdInvoiceLinesTableName
                & dmoWhere ?~ whereExp
                & dmoReturningFields
                  .~ invoiceLinesFields
                    <> Data.mkFieldsMap
                      [ ( "Invoice",
                          ( RelField
                              ( RelationshipField _tdInvoiceRelationshipName $
                                  Data.emptyQuery
                                    & qFields
                                      ?~ Data.mkFieldsMap
                                        [ ("InvoiceId", _tdColumnField _tdInvoicesTableName "InvoiceId"),
                                          ("Total", _tdColumnField _tdInvoicesTableName "Total"),
                                          ( "InvoiceLines",
                                            ( RelField
                                                ( RelationshipField _tdInvoiceLinesRelationshipName $
                                                    Data.emptyQuery
                                                      & qFields ?~ invoiceLinesFields
                                                      & qOrderBy ?~ OrderBy mempty (_tdOrderByColumn [] "InvoiceLineId" Ascending :| [])
                                                )
                                            )
                                          )
                                        ]
                              )
                          )
                        )
                      ]
        let tableRelationships =
              Set.fromList
                [ RTable $ Data.onlyKeepRelationships [_tdInvoiceRelationshipName] _tdInvoiceLinesTableRelationships,
                  RTable $ Data.onlyKeepRelationships [_tdInvoiceLinesRelationshipName] _tdInvoicesTableRelationships
                ]
        let mutationRequest =
              Data.emptyMutationRequest
                & mrOperations .~ [DeleteOperation deleteOperation]
                & mrRelationships .~ tableRelationships

        response <- mutationGuarded mutationRequest

        let remainingInvoiceLines =
              _tdInvoiceLinesRows & filter (\invoiceLine -> invoiceLine ^? Data.field "InvoiceLineId" . Data._ColumnFieldNumber `notElem` [Just 4, Just 5])

        let joinInInvoiceLines (invoice :: HashMap FieldName FieldValue) =
              let invoiceId = invoice ^? Data.field "InvoiceId" . Data._ColumnFieldNumber
                  invoiceLines = remainingInvoiceLines & filter (\invoiceLine -> invoiceLine ^? Data.field "InvoiceId" . Data._ColumnFieldNumber == invoiceId)
               in Data.insertField "InvoiceLines" (Data.mkSubqueryRowsFieldValue invoiceLines) invoice

        let joinInInvoice (invoiceLine :: HashMap FieldName FieldValue) =
              let invoice = (invoiceLine ^? Data.field "InvoiceId" . Data._ColumnFieldNumber) >>= \invoiceId -> _tdInvoicesRowsById ^? ix invoiceId
                  invoiceTrimmed = Data.filterColumns ["InvoiceId", "Total"] $ maybeToList invoice
               in Data.insertField "Invoice" (Data.mkSubqueryRowsFieldValue (joinInInvoiceLines <$> invoiceTrimmed)) invoiceLine

        let expectedDeletedRows =
              _tdInvoiceLinesRows
                & filter (\invoiceLine -> invoiceLine ^? Data.field "InvoiceLineId" . Data._ColumnFieldNumber `elem` [Just 4, Just 5])
                & fmap joinInInvoice

        let expectedResult = MutationOperationResults 2 (Just expectedDeletedRows)
        response `mutationResponseShouldBe` MutationResponse [expectedResult]

  for_ (_cMutations >>= _mcReturningCapabilities) $ \_returningCapabilities ->
    describe "edge cases" $
      edgeCaseTest _ectdNoPrimaryKeyTableName "can delete rows in a table with no primary key" $ \EdgeCasesTestData {..} -> do
        let firstNameScalarType = _ectdFindColumnScalarType _ectdNoPrimaryKeyTableName "FirstName"
        let lastNameScalarType = _ectdFindColumnScalarType _ectdNoPrimaryKeyTableName "LastName"
        let whereExp =
              Data.mkAndExpr
                [ ApplyBinaryComparisonOperator Equal (_ectdCurrentComparisonColumn "FirstName" firstNameScalarType) (Data.scalarValueComparison (J.String "Beverly") firstNameScalarType),
                  ApplyBinaryComparisonOperator Equal (_ectdCurrentComparisonColumn "LastName" lastNameScalarType) (Data.scalarValueComparison (J.String "Crusher") lastNameScalarType)
                ]
        let returning =
              Data.mkFieldsMap
                [ ("FirstName", _ectdColumnField _ectdNoPrimaryKeyTableName "FirstName"),
                  ("LastName", _ectdColumnField _ectdNoPrimaryKeyTableName "LastName")
                ]
        let deleteOperation =
              mkDeleteOperation _ectdNoPrimaryKeyTableName
                & dmoWhere ?~ whereExp
                & dmoReturningFields .~ returning
        let mutationRequest = Data.emptyMutationRequest & mrOperations .~ [DeleteOperation deleteOperation]

        response <- mutationGuarded mutationRequest

        let expectedDeletedRows =
              [ Data.mkFieldsMap $
                  [ ("FirstName", mkColumnFieldValue $ J.String "Beverly"),
                    ("LastName", mkColumnFieldValue $ J.String "Crusher")
                  ]
              ]

        let expectedResult = MutationOperationResults 1 (Just expectedDeletedRows)
        response `mutationResponseShouldBe` MutationResponse [expectedResult]
  where
    edgeCaseTest = Test.edgeCaseTest edgeCasesTestData

    mkDeleteOperation :: TableName -> DeleteMutationOperation
    mkDeleteOperation tableName = DeleteMutationOperation tableName Nothing mempty

    invoiceLinesFields :: HashMap FieldName Field
    invoiceLinesFields =
      Data.mkFieldsMap
        [ ("InvoiceId", _tdColumnField _tdInvoiceLinesTableName "InvoiceId"),
          ("InvoiceLineId", _tdColumnField _tdInvoiceLinesTableName "InvoiceLineId"),
          ("TrackId", _tdColumnField _tdInvoiceLinesTableName "TrackId"),
          ("UnitPrice", _tdColumnField _tdInvoiceLinesTableName "UnitPrice"),
          ("Quantity", _tdColumnField _tdInvoiceLinesTableName "Quantity")
        ]

    invoiceLinesQueryRequest :: QueryRequest
    invoiceLinesQueryRequest =
      let query = Data.emptyQuery & qFields ?~ invoiceLinesFields & qOrderBy ?~ OrderBy mempty (_tdOrderByColumn [] "InvoiceId" Ascending :| [])
       in TableQueryRequest _tdInvoiceLinesTableName mempty mempty mempty query Nothing

    invoiceIdScalarType = _tdFindColumnScalarType _tdInvoiceLinesTableName "InvoiceId"
    invoiceLineIdScalarType = _tdFindColumnScalarType _tdInvoiceLinesTableName "InvoiceLineId"
    composerScalarType = _tdFindColumnScalarType _tdTracksTableName "Composer"
