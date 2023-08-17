{-# LANGUAGE QuasiQuotes #-}

module Test.Specs.MutationSpec.UpdateSpec (spec) where

import Control.Lens (ix, (%~), (&), (.~), (?~), (^?))
import Control.Monad (when)
import Data.Aeson qualified as J
import Data.Foldable (for_)
import Data.Functor ((<&>))
import Data.HashMap.Strict (HashMap)
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (catMaybes, mapMaybe, maybeToList)
import Data.Set qualified as Set
import Hasura.Backends.DataConnector.API
import Language.GraphQL.Draft.Syntax.QQ qualified as G
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
spec TestData {..} edgeCasesTestData Capabilities {..} = describe "Update Mutations" $ do
  usesDataset chinookTemplate $ it "can set the value of a column on all rows" $ do
    let updateOperation =
          mkUpdateOperation _tdArtistsTableName
            & umoUpdates .~ Set.fromList [SetColumn $ _tdRowColumnOperatorValue _tdArtistsTableName "Name" (J.String "Uniformity")]
    let mutationRequest = Data.emptyMutationRequest & mrOperations .~ [UpdateOperation updateOperation]

    response <- mutationGuarded mutationRequest

    let expectedResult = MutationOperationResults 275 Nothing
    response `mutationResponseShouldBe` MutationResponse [expectedResult]

    let expectedModifiedRows =
          _tdArtistsRows
            & fmap (\artist -> artist & Data.field "Name" . Data._ColumnFieldString .~ "Uniformity")

    receivedArtists <- Data.sortResponseRowsBy "ArtistId" <$> queryGuarded (artistsQueryRequest (And mempty))
    Data.responseRows receivedArtists `rowsShouldBe` expectedModifiedRows

  usesDataset chinookTemplate $ it "can set the value of a column on a specific row" $ do
    let whereExp = ApplyBinaryComparisonOperator Equal (_tdCurrentComparisonColumn "ArtistId" artistIdScalarType) (Data.scalarValueComparison (J.Number 1) artistIdScalarType)
    let updateOperation =
          mkUpdateOperation _tdArtistsTableName
            & umoUpdates .~ Set.fromList [SetColumn $ _tdRowColumnOperatorValue _tdArtistsTableName "Name" (J.String "AySeeDeeSee")]
            & umoWhere ?~ whereExp
    let mutationRequest = Data.emptyMutationRequest & mrOperations .~ [UpdateOperation updateOperation]

    response <- mutationGuarded mutationRequest

    let expectedResult = MutationOperationResults 1 Nothing
    response `mutationResponseShouldBe` MutationResponse [expectedResult]

    let expectedModifiedRows =
          _tdArtistsRowsById ^? ix 1
            & fmap (\artist -> artist & Data.field "Name" . Data._ColumnFieldString .~ "AySeeDeeSee")
            & maybeToList

    receivedArtists <- Data.sortResponseRowsBy "ArtistId" <$> queryGuarded (artistsQueryRequest whereExp)
    Data.responseRows receivedArtists `rowsShouldBe` expectedModifiedRows

  usesDataset chinookTemplate $ it "can set the value of a column on a range of rows" $ do
    let whereExp =
          Data.mkAndExpr
            [ ApplyBinaryComparisonOperator GreaterThan (_tdCurrentComparisonColumn "ArtistId" artistIdScalarType) (Data.scalarValueComparison (J.Number 10) artistIdScalarType),
              ApplyBinaryComparisonOperator LessThanOrEqual (_tdCurrentComparisonColumn "ArtistId" artistIdScalarType) (Data.scalarValueComparison (J.Number 20) artistIdScalarType)
            ]
    let updateOperation =
          mkUpdateOperation _tdArtistsTableName
            & umoUpdates .~ Set.fromList [SetColumn $ _tdRowColumnOperatorValue _tdArtistsTableName "Name" (J.String "Nameless")]
            & umoWhere ?~ whereExp
    let mutationRequest = Data.emptyMutationRequest & mrOperations .~ [UpdateOperation updateOperation]

    response <- mutationGuarded mutationRequest

    let expectedResult = MutationOperationResults 10 Nothing
    response `mutationResponseShouldBe` MutationResponse [expectedResult]

    let expectedModifiedRows =
          _tdArtistsRows
            & filter (\artist -> artist ^? Data.field "ArtistId" . Data._ColumnFieldNumber > Just 10 && artist ^? Data.field "ArtistId" . Data._ColumnFieldNumber <= Just 20)
            & fmap (\artist -> artist & Data.field "Name" . Data._ColumnFieldString .~ "Nameless")

    receivedArtists <- Data.sortResponseRowsBy "ArtistId" <$> queryGuarded (artistsQueryRequest whereExp)
    Data.responseRows receivedArtists `rowsShouldBe` expectedModifiedRows

  when ((_cComparisons >>= _ccSubqueryComparisonCapabilities <&> _ctccSupportsRelations) == Just True) $ do
    usesDataset chinookTemplate $ it "can set the value of a column on rows filtered by a related table" $ do
      let whereExp =
            Exists (RelatedTable _tdAlbumsRelationshipName) $
              ApplyBinaryComparisonOperator Equal (_tdCurrentComparisonColumn "Title" albumTitleScalarType) (Data.scalarValueComparison (J.String "Master Of Puppets") albumTitleScalarType)
      let updateOperation =
            mkUpdateOperation _tdArtistsTableName
              & umoUpdates .~ Set.fromList [SetColumn $ _tdRowColumnOperatorValue _tdArtistsTableName "Name" (J.String "Metalika")]
              & umoWhere ?~ whereExp
      let tableRelationships = Set.singleton $ RTable $ Data.onlyKeepRelationships [_tdAlbumsRelationshipName] _tdArtistsTableRelationships
      let mutationRequest =
            Data.emptyMutationRequest
              & mrOperations .~ [UpdateOperation updateOperation]
              & mrRelationships .~ tableRelationships

      response <- mutationGuarded mutationRequest

      let expectedResult = MutationOperationResults 1 Nothing
      response `mutationResponseShouldBe` MutationResponse [expectedResult]

      let expectedModifiedRows =
            _tdArtistsRows
              & filter
                ( \artist ->
                    let artistId = artist ^? Data.field "ArtistId" . Data._ColumnFieldNumber
                     in _tdAlbumsRows
                          & any
                            ( \album ->
                                album ^? Data.field "ArtistId" . Data._ColumnFieldNumber == artistId
                                  && album ^? Data.field "Title" . Data._ColumnFieldString == Just "Master Of Puppets"
                            )
                )
              & fmap (\artist -> artist & Data.field "Name" . Data._ColumnFieldString .~ "Metalika")

      receivedArtists <- Data.sortResponseRowsBy "ArtistId" <$> queryGuarded (artistsQueryRequest whereExp & qrRelationships .~ tableRelationships)
      Data.responseRows receivedArtists `rowsShouldBe` expectedModifiedRows

  usesDataset chinookTemplate $ it "can set the value of a column differently using multiple operations" $ do
    let whereExp1 = ApplyBinaryArrayComparisonOperator In (_tdCurrentComparisonColumn "ArtistId" artistIdScalarType) [J.Number 50, J.Number 51] artistIdScalarType
    let updateOperation1 =
          mkUpdateOperation _tdArtistsTableName
            & umoUpdates .~ Set.fromList [SetColumn $ _tdRowColumnOperatorValue _tdArtistsTableName "Name" (J.String "Renamed 1")]
            & umoWhere ?~ whereExp1
    let whereExp2 = ApplyBinaryComparisonOperator LessThan (_tdCurrentComparisonColumn "ArtistId" artistIdScalarType) (Data.scalarValueComparison (J.Number 6) artistIdScalarType)
    let updateOperation2 =
          mkUpdateOperation _tdArtistsTableName
            & umoUpdates .~ Set.fromList [SetColumn $ _tdRowColumnOperatorValue _tdArtistsTableName "Name" (J.String "Renamed 2")]
            & umoWhere ?~ whereExp2
    let mutationRequest = Data.emptyMutationRequest & mrOperations .~ [UpdateOperation updateOperation1, UpdateOperation updateOperation2]

    response <- mutationGuarded mutationRequest

    response
      `mutationResponseShouldBe` MutationResponse
        [ MutationOperationResults 2 Nothing,
          MutationOperationResults 5 Nothing
        ]

    let expectedModifiedRows =
          [ [_tdArtistsRowsById ^? ix 50, _tdArtistsRowsById ^? ix 51]
              & catMaybes
              & fmap (\artist -> artist & Data.field "Name" . Data._ColumnFieldString .~ "Renamed 1"),
            _tdArtistsRows
              & filter (\artist -> artist ^? Data.field "ArtistId" . Data._ColumnFieldNumber < Just 6)
              & fmap (\artist -> artist & Data.field "Name" . Data._ColumnFieldString .~ "Renamed 2")
          ]
            & concat
            & sortOn (^? Data.field "ArtistId")

    receivedArtists <- Data.sortResponseRowsBy "ArtistId" <$> queryGuarded (artistsQueryRequest (Data.mkOrExpr [whereExp1, whereExp2]))
    Data.responseRows receivedArtists `rowsShouldBe` expectedModifiedRows

  usesDataset chinookTemplate $ it "multiple update operations are run sequentially" $ do
    -- This test performs two updates whose updated row sets overlap each other, so sequential updates should
    -- result in the second update applying to the overlapping rows
    let whereExp1 = ApplyBinaryArrayComparisonOperator In (_tdCurrentComparisonColumn "Name" artistNameScalarType) [J.String "AC/DC", J.String "Audioslave"] artistNameScalarType
    let updateOperation1 =
          mkUpdateOperation _tdArtistsTableName
            & umoUpdates .~ Set.fromList [SetColumn $ _tdRowColumnOperatorValue _tdArtistsTableName "Name" (J.String "Renamed 1")]
            & umoWhere ?~ whereExp1
    let whereExp2 = ApplyBinaryComparisonOperator LessThanOrEqual (_tdCurrentComparisonColumn "ArtistId" artistIdScalarType) (Data.scalarValueComparison (J.Number 5) artistIdScalarType)
    let updateOperation2 =
          mkUpdateOperation _tdArtistsTableName
            & umoUpdates .~ Set.fromList [SetColumn $ _tdRowColumnOperatorValue _tdArtistsTableName "Name" (J.String "Renamed 2")]
            & umoWhere ?~ whereExp2
    let mutationRequest = Data.emptyMutationRequest & mrOperations .~ [UpdateOperation updateOperation1, UpdateOperation updateOperation2]

    response <- mutationGuarded mutationRequest

    response
      `mutationResponseShouldBe` MutationResponse
        [ MutationOperationResults 2 Nothing,
          MutationOperationResults 5 Nothing
        ]

    let expectedWhereExp1 artist = artist ^? Data.field "Name" . Data._ColumnFieldString `elem` [Just "AC/DC", Just "Audioslave"]
    let expectedWhereExp2 artist = artist ^? Data.field "ArtistId" . Data._ColumnFieldNumber <= Just 5
    let expectedModifiedRows =
          [ _tdArtistsRows
              & filter (\artist -> expectedWhereExp1 artist && not (expectedWhereExp2 artist))
              & fmap (\artist -> artist & Data.field "Name" . Data._ColumnFieldString .~ "Renamed 1"),
            _tdArtistsRows
              & filter expectedWhereExp2
              & fmap (\artist -> artist & Data.field "Name" . Data._ColumnFieldString .~ "Renamed 2")
          ]
            & concat
            & sortOn (^? Data.field "ArtistId")

    -- We can't use whereExp1 since we've renamed the artists!
    let alternateWhereExp1 = ApplyBinaryArrayComparisonOperator In (_tdCurrentComparisonColumn "ArtistId" artistIdScalarType) [J.Number 1, J.Number 8] artistIdScalarType
    receivedArtists <- Data.sortResponseRowsBy "ArtistId" <$> queryGuarded (artistsQueryRequest (Data.mkOrExpr [alternateWhereExp1, whereExp2]))
    Data.responseRows receivedArtists `rowsShouldBe` expectedModifiedRows

  usesDataset chinookTemplate $ it "can increment the value of a column" $ do
    let whereExp = ApplyBinaryComparisonOperator Equal (_tdCurrentComparisonColumn "InvoiceId" invoiceIdScalarType) (Data.scalarValueComparison (J.Number 2) invoiceIdScalarType)
    let updateOperation =
          mkUpdateOperation _tdInvoiceLinesTableName
            & umoUpdates .~ Set.fromList [CustomUpdateColumnOperator incOperator $ _tdRowColumnOperatorValue _tdInvoiceLinesTableName "Quantity" (J.Number 3)]
            & umoWhere ?~ whereExp
    let mutationRequest = Data.emptyMutationRequest & mrOperations .~ [UpdateOperation updateOperation]

    response <- mutationGuarded mutationRequest

    let expectedResult = MutationOperationResults 4 Nothing
    response `mutationResponseShouldBe` MutationResponse [expectedResult]

    let expectedModifiedRows =
          _tdInvoiceLinesRows
            & filter (\invoiceLine -> invoiceLine ^? Data.field "InvoiceId" . Data._ColumnFieldNumber == Just 2)
            & fmap (\invoiceLine -> invoiceLine & Data.field "Quantity" . Data._ColumnFieldNumber %~ (+ 3))

    receivedInvoiceLines <- Data.sortResponseRowsBy "InvoiceLineId" <$> queryGuarded (invoiceLinesQueryRequest whereExp)
    Data.responseRows receivedInvoiceLines `rowsShouldBe` expectedModifiedRows

  usesDataset chinookTemplate $ it "can make multiple updates in one operation" $ do
    let whereExp = ApplyBinaryComparisonOperator Equal (_tdCurrentComparisonColumn "InvoiceId" invoiceIdScalarType) (Data.scalarValueComparison (J.Number 299) invoiceIdScalarType)
    let updateOperation =
          mkUpdateOperation _tdInvoiceLinesTableName
            & umoUpdates
              .~ Set.fromList
                [ CustomUpdateColumnOperator incOperator $ _tdRowColumnOperatorValue _tdInvoiceLinesTableName "UnitPrice" (J.Number 3),
                  SetColumn $ _tdRowColumnOperatorValue _tdInvoiceLinesTableName "Quantity" (J.Number 2)
                ]
            & umoWhere ?~ whereExp
    let mutationRequest = Data.emptyMutationRequest & mrOperations .~ [UpdateOperation updateOperation]

    response <- mutationGuarded mutationRequest

    let expectedResult = MutationOperationResults 14 Nothing
    response `mutationResponseShouldBe` MutationResponse [expectedResult]

    let expectedModifiedRows =
          _tdInvoiceLinesRows
            & filter (\invoiceLine -> invoiceLine ^? Data.field "InvoiceId" . Data._ColumnFieldNumber == Just 299)
            & fmap
              ( \invoiceLine ->
                  invoiceLine
                    & Data.field "Quantity" . Data._ColumnFieldNumber .~ 2
                    & Data.field "UnitPrice" . Data._ColumnFieldNumber %~ (+ 3)
              )

    receivedInvoiceLines <- Data.sortResponseRowsBy "InvoiceLineId" <$> queryGuarded (invoiceLinesQueryRequest whereExp)
    Data.responseRows receivedInvoiceLines `rowsShouldBe` expectedModifiedRows

  describe "post-update checks" $ do
    usesDataset chinookTemplate $ it "can update when the post-update check passes" $ do
      let whereExp =
            Data.mkOrExpr
              [ ApplyBinaryComparisonOperator Equal (_tdCurrentComparisonColumn "ArtistId" artistIdScalarType) (Data.scalarValueComparison (J.Number 1) artistIdScalarType),
                ApplyBinaryComparisonOperator Equal (_tdCurrentComparisonColumn "ArtistId" artistIdScalarType) (Data.scalarValueComparison (J.Number 2) artistIdScalarType)
              ]
      let postUpdateExp =
            ApplyBinaryComparisonOperator Equal (_tdCurrentComparisonColumn "Name" artistNameScalarType) (Data.scalarValueComparison (J.String "Some other name") artistNameScalarType)
      let updateOperation =
            mkUpdateOperation _tdArtistsTableName
              & umoUpdates .~ Set.fromList [SetColumn $ _tdRowColumnOperatorValue _tdArtistsTableName "Name" (J.String "Some other name")]
              & umoWhere ?~ whereExp
              & umoPostUpdateCheck ?~ postUpdateExp
      let mutationRequest = Data.emptyMutationRequest & mrOperations .~ [UpdateOperation updateOperation]

      response <- mutationGuarded mutationRequest

      let expectedResult = MutationOperationResults 2 Nothing
      response `mutationResponseShouldBe` MutationResponse [expectedResult]

      let expectedModifiedRows =
            [_tdArtistsRowsById ^? ix 1, _tdArtistsRowsById ^? ix 2]
              & catMaybes
              & fmap (\artist -> artist & Data.field "Name" . Data._ColumnFieldString .~ "Some other name")

      receivedArtists <- Data.sortResponseRowsBy "ArtistId" <$> queryGuarded (artistsQueryRequest whereExp)
      Data.responseRows receivedArtists `rowsShouldBe` expectedModifiedRows

    usesDataset chinookTemplate $ it "fails to update when the post-update check fails" $ do
      let whereExp = ApplyBinaryComparisonOperator Equal (_tdCurrentComparisonColumn "InvoiceId" invoiceIdScalarType) (Data.scalarValueComparison (J.Number 299) invoiceIdScalarType)
      let postUpdateExp = ApplyBinaryComparisonOperator LessThanOrEqual (_tdCurrentComparisonColumn "UnitPrice" invoiceLineUnitPriceScalarType) (Data.scalarValueComparison (J.Number 1.99) invoiceLineUnitPriceScalarType)
      let updateOperation =
            mkUpdateOperation _tdInvoiceLinesTableName
              & umoUpdates .~ Set.fromList [CustomUpdateColumnOperator incOperator $ _tdRowColumnOperatorValue _tdInvoiceLinesTableName "UnitPrice" (J.Number 1)]
              & umoWhere ?~ whereExp
              & umoPostUpdateCheck ?~ postUpdateExp
      let mutationRequest = Data.emptyMutationRequest & mrOperations .~ [UpdateOperation updateOperation]

      response <- mutationExpectError mutationRequest

      _crType response `shouldBe` MutationPermissionCheckFailure

      let expectedUnchangedRows =
            _tdInvoiceLinesRows
              & filter (\invoiceLine -> invoiceLine ^? Data.field "InvoiceId" . Data._ColumnFieldNumber == Just 299)

      receivedInvoiceLines <- Data.sortResponseRowsBy "InvoiceLineId" <$> queryGuarded (invoiceLinesQueryRequest whereExp)
      Data.responseRows receivedInvoiceLines `rowsShouldBe` expectedUnchangedRows

    for_ (_cComparisons >>= _ccSubqueryComparisonCapabilities) $ \_subqueryComparisonCapabilities -> do
      usesDataset chinookTemplate $ it "can update when post update check against unrelated table passes" $ do
        let whereExp = ApplyBinaryComparisonOperator Equal (_tdCurrentComparisonColumn "InvoiceId" invoiceIdScalarType) (Data.scalarValueComparison (J.Number 299) invoiceIdScalarType)
        let postUpdateExp =
              Exists (UnrelatedTable _tdCustomersTableName) $
                ApplyBinaryComparisonOperator Equal (_tdCurrentComparisonColumn "CustomerId" customerIdScalarType) (Data.scalarValueComparison (J.Number 2) customerIdScalarType)
        let updateOperation =
              mkUpdateOperation _tdInvoiceLinesTableName
                & umoUpdates .~ Set.fromList [CustomUpdateColumnOperator incOperator $ _tdRowColumnOperatorValue _tdInvoiceLinesTableName "UnitPrice" (J.Number 1)]
                & umoWhere ?~ whereExp
                & umoPostUpdateCheck ?~ postUpdateExp
        let mutationRequest = Data.emptyMutationRequest & mrOperations .~ [UpdateOperation updateOperation]

        response <- mutationGuarded mutationRequest

        let expectedResult = MutationOperationResults 14 Nothing
        response `mutationResponseShouldBe` MutationResponse [expectedResult]

        let expectedModifiedRows =
              _tdInvoiceLinesRows
                & filter (\invoiceLine -> invoiceLine ^? Data.field "InvoiceId" . Data._ColumnFieldNumber == Just 299)
                & fmap (\invoiceLine -> invoiceLine & Data.field "UnitPrice" . Data._ColumnFieldNumber %~ (+ 1))

        receivedInvoiceLines <- Data.sortResponseRowsBy "InvoiceLineId" <$> queryGuarded (invoiceLinesQueryRequest whereExp)
        Data.responseRows receivedInvoiceLines `rowsShouldBe` expectedModifiedRows

      usesDataset chinookTemplate $ it "fails to update when post update check against unrelated table fails" $ do
        let whereExp = ApplyBinaryComparisonOperator Equal (_tdCurrentComparisonColumn "InvoiceId" invoiceIdScalarType) (Data.scalarValueComparison (J.Number 299) invoiceIdScalarType)
        let postUpdateExp =
              Exists (UnrelatedTable _tdCustomersTableName) $
                ApplyBinaryComparisonOperator Equal (_tdCurrentComparisonColumn "CustomerId" customerIdScalarType) (Data.scalarValueComparison (J.Number 666) customerIdScalarType)
        let updateOperation =
              mkUpdateOperation _tdInvoiceLinesTableName
                & umoUpdates .~ Set.fromList [CustomUpdateColumnOperator incOperator $ _tdRowColumnOperatorValue _tdInvoiceLinesTableName "UnitPrice" (J.Number 1)]
                & umoWhere ?~ whereExp
                & umoPostUpdateCheck ?~ postUpdateExp
        let mutationRequest = Data.emptyMutationRequest & mrOperations .~ [UpdateOperation updateOperation]

        response <- mutationExpectError mutationRequest

        _crType response `shouldBe` MutationPermissionCheckFailure

        let expectedUnchangedRows =
              _tdInvoiceLinesRows
                & filter (\invoiceLine -> invoiceLine ^? Data.field "InvoiceId" . Data._ColumnFieldNumber == Just 299)

        receivedInvoiceLines <- Data.sortResponseRowsBy "InvoiceLineId" <$> queryGuarded (invoiceLinesQueryRequest whereExp)
        Data.responseRows receivedInvoiceLines `rowsShouldBe` expectedUnchangedRows

    when ((_cComparisons >>= _ccSubqueryComparisonCapabilities <&> _ctccSupportsRelations) == Just True) $ do
      usesDataset chinookTemplate $ it "can update when post update check against related table passes" $ do
        let whereExp = ApplyBinaryComparisonOperator Equal (_tdCurrentComparisonColumn "InvoiceId" invoiceIdScalarType) (Data.scalarValueComparison (J.Number 299) invoiceIdScalarType)
        let postUpdateExp =
              Exists (RelatedTable _tdInvoiceRelationshipName) $
                ApplyBinaryComparisonOperator Equal (_tdCurrentComparisonColumn "CustomerId" customerIdScalarType) (Data.scalarValueComparison (J.Number 17) customerIdScalarType)
        let updateOperation =
              mkUpdateOperation _tdInvoiceLinesTableName
                & umoUpdates .~ Set.fromList [SetColumn $ _tdRowColumnOperatorValue _tdInvoiceLinesTableName "InvoiceId" (J.Number 298)]
                & umoWhere ?~ whereExp
                & umoPostUpdateCheck ?~ postUpdateExp
        let tableRelationships = Set.singleton (RTable $ Data.onlyKeepRelationships [_tdInvoiceRelationshipName] _tdInvoiceLinesTableRelationships)
        let mutationRequest =
              Data.emptyMutationRequest
                & mrOperations .~ [UpdateOperation updateOperation]
                & mrRelationships .~ tableRelationships

        response <- mutationGuarded mutationRequest

        let expectedResult = MutationOperationResults 14 Nothing
        response `mutationResponseShouldBe` MutationResponse [expectedResult]

        let expectedModifiedRows =
              _tdInvoiceLinesRows
                & filter (\invoiceLine -> invoiceLine ^? Data.field "InvoiceId" . Data._ColumnFieldNumber == Just 299)
                & fmap (\invoiceLine -> invoiceLine & Data.field "InvoiceId" . Data._ColumnFieldNumber .~ 298)

        -- Need a different where expression for querying the modified rows because the invoice lines have moved
        -- to a different invoice and therefore we can't re-query them by the original invoice id any more
        let invoiceLineIds = expectedModifiedRows & mapMaybe (^? Data.field "InvoiceLineId" . Data._ColumnFieldNumber) & fmap J.Number
        let alternateWhereExp = ApplyBinaryArrayComparisonOperator In (_tdCurrentComparisonColumn "InvoiceLineId" invoiceLineIdScalarType) invoiceLineIds invoiceLineIdScalarType

        receivedInvoiceLines <- Data.sortResponseRowsBy "InvoiceLineId" <$> queryGuarded (invoiceLinesQueryRequest alternateWhereExp & qrRelationships .~ tableRelationships)
        Data.responseRows receivedInvoiceLines `rowsShouldBe` expectedModifiedRows

      usesDataset chinookTemplate $ it "fails to update when post update check against related table fails" $ do
        let whereExp = ApplyBinaryComparisonOperator Equal (_tdCurrentComparisonColumn "InvoiceId" invoiceIdScalarType) (Data.scalarValueComparison (J.Number 299) invoiceIdScalarType)
        let postUpdateExp =
              Exists (RelatedTable _tdInvoiceRelationshipName) $
                ApplyBinaryComparisonOperator Equal (_tdCurrentComparisonColumn "CustomerId" customerIdScalarType) (Data.scalarValueComparison (J.Number 26) customerIdScalarType)
        let updateOperation =
              mkUpdateOperation _tdInvoiceLinesTableName
                & umoUpdates .~ Set.fromList [SetColumn $ _tdRowColumnOperatorValue _tdInvoiceLinesTableName "InvoiceId" (J.Number 298)]
                & umoWhere ?~ whereExp
                & umoPostUpdateCheck ?~ postUpdateExp
        let tableRelationships = Set.singleton (RTable $ Data.onlyKeepRelationships [_tdInvoiceRelationshipName] _tdInvoiceLinesTableRelationships)
        let mutationRequest =
              Data.emptyMutationRequest
                & mrOperations .~ [UpdateOperation updateOperation]
                & mrRelationships .~ tableRelationships

        response <- mutationExpectError mutationRequest

        _crType response `shouldBe` MutationPermissionCheckFailure

        let expectedUnchangedRows =
              _tdInvoiceLinesRows
                & filter (\invoiceLine -> invoiceLine ^? Data.field "InvoiceId" . Data._ColumnFieldNumber == Just 299)

        receivedInvoiceLines <- Data.sortResponseRowsBy "InvoiceLineId" <$> queryGuarded (invoiceLinesQueryRequest whereExp)
        Data.responseRows receivedInvoiceLines `rowsShouldBe` expectedUnchangedRows

  for_ (_cMutations >>= _mcReturningCapabilities) $ \_returningCapabilities -> describe "returning" $ do
    usesDataset chinookTemplate $ it "returns updated rows" $ do
      let whereExp = ApplyBinaryComparisonOperator Equal (_tdCurrentComparisonColumn "InvoiceId" invoiceIdScalarType) (Data.scalarValueComparison (J.Number 299) invoiceIdScalarType)
      let updateOperation =
            mkUpdateOperation _tdInvoiceLinesTableName
              & umoUpdates
                .~ Set.fromList
                  [ SetColumn $ _tdRowColumnOperatorValue _tdInvoiceLinesTableName "InvoiceId" (J.Number 298),
                    CustomUpdateColumnOperator incOperator $ _tdRowColumnOperatorValue _tdInvoiceLinesTableName "UnitPrice" (J.Number 1)
                  ]
              & umoWhere ?~ whereExp
              & umoReturningFields .~ invoiceLinesFields
      let mutationRequest =
            Data.emptyMutationRequest
              & mrOperations .~ [UpdateOperation updateOperation]

      response <- mutationGuarded mutationRequest

      let expectedModifiedRows =
            _tdInvoiceLinesRows
              & filter (\invoiceLine -> invoiceLine ^? Data.field "InvoiceId" . Data._ColumnFieldNumber == Just 299)
              & fmap
                ( \invoiceLine ->
                    invoiceLine
                      & Data.field "InvoiceId" . Data._ColumnFieldNumber .~ 298
                      & Data.field "UnitPrice" . Data._ColumnFieldNumber %~ (+ 1)
                )

      let expectedResult = MutationOperationResults 14 (Just expectedModifiedRows)
      response `mutationResponseShouldBe` MutationResponse [expectedResult]

    for_ _cRelationships $ \_relationshipCapabilities -> do
      usesDataset chinookTemplate $ it "can return rows from an object relationship" $ do
        let whereExp = ApplyBinaryComparisonOperator Equal (_tdCurrentComparisonColumn "InvoiceId" invoiceIdScalarType) (Data.scalarValueComparison (J.Number 299) invoiceIdScalarType)
        let updateOperation =
              mkUpdateOperation _tdInvoiceLinesTableName
                & umoUpdates
                  .~ Set.fromList
                    [ SetColumn $ _tdRowColumnOperatorValue _tdInvoiceLinesTableName "InvoiceId" (J.Number 298),
                      CustomUpdateColumnOperator incOperator $ _tdRowColumnOperatorValue _tdInvoiceLinesTableName "UnitPrice" (J.Number 1)
                    ]
                & umoWhere ?~ whereExp
                & umoReturningFields
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
        let tableRelationships = Set.singleton (RTable $ Data.onlyKeepRelationships [_tdTrackRelationshipName] _tdInvoiceLinesTableRelationships)
        let mutationRequest =
              Data.emptyMutationRequest
                & mrOperations .~ [UpdateOperation updateOperation]
                & mrRelationships .~ tableRelationships

        response <- mutationGuarded mutationRequest

        let joinInTrack (invoiceLine :: HashMap FieldName FieldValue) =
              let track = (invoiceLine ^? Data.field "TrackId" . Data._ColumnFieldNumber) >>= \trackId -> _tdTracksRowsById ^? ix trackId
                  trackTrimmed = Data.filterColumns ["TrackId", "Name"] $ maybeToList track
               in Data.insertField "Track" (Data.mkSubqueryRowsFieldValue trackTrimmed) invoiceLine

        let expectedModifiedRows =
              _tdInvoiceLinesRows
                & filter (\invoiceLine -> invoiceLine ^? Data.field "InvoiceId" . Data._ColumnFieldNumber == Just 299)
                & fmap
                  ( \invoiceLine ->
                      invoiceLine
                        & Data.field "InvoiceId" . Data._ColumnFieldNumber .~ 298
                        & Data.field "UnitPrice" . Data._ColumnFieldNumber %~ (+ 1)
                        & joinInTrack
                  )

        let expectedResult = MutationOperationResults 14 (Just expectedModifiedRows)
        response `mutationResponseShouldBe` MutationResponse [expectedResult]

      usesDataset chinookTemplate $ it "can return rows from an array relationship" $ do
        let whereExp = ApplyBinaryComparisonOperator Equal (_tdCurrentComparisonColumn "ArtistId" artistIdScalarType) (Data.scalarValueComparison (J.Number 1) artistIdScalarType)
        let updateOperation =
              mkUpdateOperation _tdArtistsTableName
                & umoUpdates .~ Set.fromList [SetColumn $ _tdRowColumnOperatorValue _tdArtistsTableName "Name" (J.String "AySeeDeeSee")]
                & umoWhere ?~ whereExp
                & umoReturningFields
                  .~ artistsFields
                    <> Data.mkFieldsMap
                      [ ( "Albums",
                          ( RelField
                              ( RelationshipField _tdAlbumsRelationshipName $
                                  Data.emptyQuery
                                    & qFields
                                      ?~ Data.mkFieldsMap
                                        [ ("AlbumId", _tdColumnField _tdAlbumsTableName "AlbumId"),
                                          ("ArtistId", _tdColumnField _tdAlbumsTableName "ArtistId"),
                                          ("Title", _tdColumnField _tdAlbumsTableName "Title")
                                        ]
                                    & qOrderBy ?~ OrderBy mempty (_tdOrderByColumn [] "AlbumId" Ascending :| [])
                              )
                          )
                        )
                      ]
        let tableRelationships = Set.singleton (RTable $ Data.onlyKeepRelationships [_tdAlbumsRelationshipName] _tdArtistsTableRelationships)
        let mutationRequest =
              Data.emptyMutationRequest
                & mrOperations .~ [UpdateOperation updateOperation]
                & mrRelationships .~ tableRelationships

        response <- mutationGuarded mutationRequest

        let joinInAlbums (artist :: HashMap FieldName FieldValue) =
              let artistId = (artist ^? Data.field "ArtistId" . Data._ColumnFieldNumber)
                  albums = _tdAlbumsRows & filter (\album -> album ^? Data.field "ArtistId" . Data._ColumnFieldNumber == artistId)
               in Data.insertField "Albums" (Data.mkSubqueryRowsFieldValue albums) artist

        let expectedModifiedRows =
              _tdArtistsRowsById ^? ix 1
                & fmap
                  ( \artist ->
                      artist
                        & Data.field "Name" . Data._ColumnFieldString .~ "AySeeDeeSee"
                        & joinInAlbums
                  )
                & maybeToList

        let expectedResult = MutationOperationResults 1 (Just expectedModifiedRows)
        response `mutationResponseShouldBe` MutationResponse [expectedResult]

      usesDataset chinookTemplate $ it "can aggregate rows from across an array relationship" $ do
        let whereExp = ApplyBinaryComparisonOperator Equal (_tdCurrentComparisonColumn "ArtistId" artistIdScalarType) (Data.scalarValueComparison (J.Number 1) artistIdScalarType)
        let updateOperation =
              mkUpdateOperation _tdArtistsTableName
                & umoUpdates .~ Set.fromList [SetColumn $ _tdRowColumnOperatorValue _tdArtistsTableName "Name" (J.String "AySeeDeeSee")]
                & umoWhere ?~ whereExp
                & umoReturningFields
                  .~ artistsFields
                    <> Data.mkFieldsMap
                      [ ( "Albums",
                          ( RelField
                              ( RelationshipField _tdAlbumsRelationshipName $
                                  Data.emptyQuery & qAggregates ?~ Data.mkFieldsMap [("AlbumsCount", StarCount)]
                              )
                          )
                        )
                      ]
        let tableRelationships = Set.singleton (RTable $ Data.onlyKeepRelationships [_tdAlbumsRelationshipName] _tdArtistsTableRelationships)
        let mutationRequest =
              Data.emptyMutationRequest
                & mrOperations .~ [UpdateOperation updateOperation]
                & mrRelationships .~ tableRelationships

        response <- mutationGuarded mutationRequest

        let joinInAlbums (artist :: HashMap FieldName FieldValue) =
              let artistId = artist ^? Data.field "ArtistId" . Data._ColumnFieldNumber
                  albumsCount = _tdAlbumsRows & filter (\album -> album ^? Data.field "ArtistId" . Data._ColumnFieldNumber == artistId) & length
                  aggregates = Data.mkFieldsMap [("AlbumsCount", J.Number $ fromIntegral albumsCount)]
               in Data.insertField "Albums" (Data.mkSubqueryAggregatesFieldValue aggregates) artist

        let expectedModifiedRows =
              _tdArtistsRowsById ^? ix 1
                & fmap
                  ( \artist ->
                      artist
                        & Data.field "Name" . Data._ColumnFieldString .~ "AySeeDeeSee"
                        & joinInAlbums
                  )
                & maybeToList

        let expectedResult = MutationOperationResults 1 (Just expectedModifiedRows)
        response `mutationResponseShouldBe` MutationResponse [expectedResult]

      usesDataset chinookTemplate $ it "updated rows are returned even when returned again from across a relationship" $ do
        -- In this scenario we move a set of invoice lines from one invoice to another
        -- and then return the invoice lines, joined to the invoice and then back to the invoice lines.
        -- We expect to see both the existing invoice lines and the moved invoice lines on the invoice.
        let whereExp = ApplyBinaryComparisonOperator Equal (_tdCurrentComparisonColumn "InvoiceId" invoiceIdScalarType) (Data.scalarValueComparison (J.Number 299) invoiceIdScalarType)
        let updateOperation =
              mkUpdateOperation _tdInvoiceLinesTableName
                & umoUpdates
                  .~ Set.fromList
                    [ SetColumn $ _tdRowColumnOperatorValue _tdInvoiceLinesTableName "InvoiceId" (J.Number 298),
                      CustomUpdateColumnOperator incOperator $ _tdRowColumnOperatorValue _tdInvoiceLinesTableName "UnitPrice" (J.Number 1)
                    ]
                & umoWhere ?~ whereExp
                & umoReturningFields
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
                & mrOperations .~ [UpdateOperation updateOperation]
                & mrRelationships .~ tableRelationships

        response <- mutationGuarded mutationRequest

        let modifiedInvoiceLines =
              _tdInvoiceLinesRows
                & filter (\invoiceLine -> invoiceLine ^? Data.field "InvoiceId" . Data._ColumnFieldNumber == Just 299)
                & fmap
                  ( \invoiceLine ->
                      invoiceLine
                        & Data.field "InvoiceId" . Data._ColumnFieldNumber .~ 298
                        & Data.field "UnitPrice" . Data._ColumnFieldNumber %~ (+ 1)
                  )

        let joinInInvoiceLines (invoice :: HashMap FieldName FieldValue) =
              let invoiceId = invoice ^? Data.field "InvoiceId" . Data._ColumnFieldNumber
                  invoiceLines = (_tdInvoiceLinesRows <> modifiedInvoiceLines) & filter (\invoiceLine -> invoiceLine ^? Data.field "InvoiceId" . Data._ColumnFieldNumber == invoiceId)
               in Data.insertField "InvoiceLines" (Data.mkSubqueryRowsFieldValue invoiceLines) invoice

        let joinInInvoice (invoiceLine :: HashMap FieldName FieldValue) =
              let invoice = (invoiceLine ^? Data.field "InvoiceId" . Data._ColumnFieldNumber) >>= \invoiceId -> _tdInvoicesRowsById ^? ix invoiceId
                  invoiceTrimmed = Data.filterColumns ["InvoiceId", "Total"] $ maybeToList invoice
               in Data.insertField "Invoice" (Data.mkSubqueryRowsFieldValue (joinInInvoiceLines <$> invoiceTrimmed)) invoiceLine

        let expectedModifiedRows = joinInInvoice <$> modifiedInvoiceLines

        let expectedResult = MutationOperationResults 14 (Just expectedModifiedRows)
        response `mutationResponseShouldBe` MutationResponse [expectedResult]

  for_ (_cMutations >>= _mcReturningCapabilities) $ \_returningCapabilities ->
    describe "edge cases" $
      edgeCaseTest _ectdNoPrimaryKeyTableName "can update rows in a table with no primary key" $ \EdgeCasesTestData {..} -> do
        let firstNameScalarType = _ectdFindColumnScalarType _ectdNoPrimaryKeyTableName "FirstName"
        let lastNameScalarType = _ectdFindColumnScalarType _ectdNoPrimaryKeyTableName "LastName"
        let whereExp =
              Data.mkAndExpr
                [ ApplyBinaryComparisonOperator Equal (_ectdCurrentComparisonColumn "FirstName" firstNameScalarType) (Data.scalarValueComparison (J.String "Will") firstNameScalarType),
                  ApplyBinaryComparisonOperator Equal (_ectdCurrentComparisonColumn "LastName" lastNameScalarType) (Data.scalarValueComparison (J.String "Riker") lastNameScalarType)
                ]
        let returning =
              Data.mkFieldsMap
                [ ("FirstName", _ectdColumnField _ectdNoPrimaryKeyTableName "FirstName"),
                  ("LastName", _ectdColumnField _ectdNoPrimaryKeyTableName "LastName")
                ]
        let updateOperation =
              mkUpdateOperation _ectdNoPrimaryKeyTableName
                & umoUpdates .~ Set.fromList [SetColumn $ _ectdRowColumnOperatorValue _ectdNoPrimaryKeyTableName "FirstName" (J.String "William")]
                & umoWhere ?~ whereExp
                & umoReturningFields .~ returning
        let mutationRequest = Data.emptyMutationRequest & mrOperations .~ [UpdateOperation updateOperation]

        response <- mutationGuarded mutationRequest

        let expectedModifiedRows =
              [ Data.mkFieldsMap $
                  [ ("FirstName", mkColumnFieldValue $ J.String "William"),
                    ("LastName", mkColumnFieldValue $ J.String "Riker")
                  ]
              ]

        let expectedResult = MutationOperationResults 1 (Just expectedModifiedRows)
        response `mutationResponseShouldBe` MutationResponse [expectedResult]
  where
    edgeCaseTest = Test.edgeCaseTest edgeCasesTestData

    mkUpdateOperation :: TableName -> UpdateMutationOperation
    mkUpdateOperation tableName = UpdateMutationOperation tableName Nothing mempty Nothing mempty

    artistsFields :: HashMap FieldName Field
    artistsFields =
      Data.mkFieldsMap
        [ ("ArtistId", _tdColumnField _tdArtistsTableName "ArtistId"),
          ("Name", _tdColumnField _tdArtistsTableName "Name")
        ]

    artistsQueryRequest :: Expression -> QueryRequest
    artistsQueryRequest whereExp =
      let query = Data.emptyQuery & qFields ?~ artistsFields & qWhere ?~ whereExp
       in TableQueryRequest _tdArtistsTableName mempty mempty mempty query Nothing

    invoiceLinesFields :: HashMap FieldName Field
    invoiceLinesFields =
      Data.mkFieldsMap
        [ ("InvoiceId", _tdColumnField _tdInvoiceLinesTableName "InvoiceId"),
          ("InvoiceLineId", _tdColumnField _tdInvoiceLinesTableName "InvoiceLineId"),
          ("TrackId", _tdColumnField _tdInvoiceLinesTableName "TrackId"),
          ("UnitPrice", _tdColumnField _tdInvoiceLinesTableName "UnitPrice"),
          ("Quantity", _tdColumnField _tdInvoiceLinesTableName "Quantity")
        ]

    invoiceLinesQueryRequest :: Expression -> QueryRequest
    invoiceLinesQueryRequest whereExp =
      let query = Data.emptyQuery & qFields ?~ invoiceLinesFields & qWhere ?~ whereExp
       in TableQueryRequest _tdInvoiceLinesTableName mempty mempty mempty query Nothing

    incOperator :: UpdateColumnOperatorName
    incOperator = UpdateColumnOperatorName $ [G.name|inc|]

    albumTitleScalarType = _tdFindColumnScalarType _tdAlbumsTableName "Title"
    artistIdScalarType = _tdFindColumnScalarType _tdArtistsTableName "ArtistId"
    artistNameScalarType = _tdFindColumnScalarType _tdArtistsTableName "Name"
    customerIdScalarType = _tdFindColumnScalarType _tdCustomersTableName "CustomerId"
    invoiceLineIdScalarType = _tdFindColumnScalarType _tdInvoiceLinesTableName "InvoiceLineId"
    invoiceIdScalarType = _tdFindColumnScalarType _tdInvoiceLinesTableName "InvoiceId"
    invoiceLineUnitPriceScalarType = _tdFindColumnScalarType _tdInvoiceLinesTableName "UnitPrice"
