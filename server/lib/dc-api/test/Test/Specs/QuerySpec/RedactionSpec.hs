{-# LANGUAGE QuasiQuotes #-}

module Test.Specs.QuerySpec.RedactionSpec (spec) where

import Control.Lens (ix, (%~), (&), (.~), (<&>), (?~), (^.), (^?), _2, _3, _Just)
import Control.Monad (when)
import Data.Aeson (Value (..))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.List (sortOn)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe, isJust, mapMaybe, maybeToList)
import Data.Ord (Down (..))
import Data.Set qualified as Set
import Hasura.Backends.DataConnector.API
import Language.GraphQL.Draft.Syntax.QQ qualified as G
import Test.AgentAPI (queryGuarded)
import Test.Data (TestData (..))
import Test.Data qualified as Data
import Test.Expectations (jsonShouldBe, rowsShouldBe)
import Test.Sandwich (describe)
import Test.TestHelpers (AgentDatasetTestSpec, it)
import Prelude

spec :: TestData -> Capabilities -> AgentDatasetTestSpec
spec TestData {..} Capabilities {..} = describe "Data Redaction in Queries" $ do
  describe "Column Field Redaction" $ do
    it "a column field can be redacted" $ do
      let redactionExpressions =
            TargetRedactionExpressions (TNTable _tdInvoicesTableName) $
              HashMap.fromList [astridsInvoicesOnlyRedactionExp]
      let query =
            invoiceQueryRequest
              & qrRedactionExpressions .~ Set.singleton redactionExpressions
              & qrQuery . qFields . _Just . Data.field "BillingCity" . _ColumnField . _3 ?~ fst astridsInvoicesOnlyRedactionExp
      receivedInvoices <- Data.sortResponseRowsBy "InvoiceId" <$> queryGuarded query

      let expectedInvoices =
            _tdInvoicesRows
              <&> (\row -> row & Data.field "BillingCity" %~ applyRedaction astridsInvoicesOnly row)
              <&> Data.filterColumnsByQueryFields (query ^. qrQuery)

      Data.responseRows receivedInvoices `rowsShouldBe` expectedInvoices
      _qrAggregates receivedInvoices `jsonShouldBe` Nothing

    it "multiple column fields can be redacted with different expressions" $ do
      let redactionExpressions =
            TargetRedactionExpressions (TNTable _tdInvoicesTableName) $
              HashMap.fromList [astridsInvoicesOnlyRedactionExp, frankAndRichardsInvoicesOnlyRedactionExp]
      let query =
            invoiceQueryRequest
              & qrRedactionExpressions .~ Set.singleton redactionExpressions
              & qrQuery . qFields . _Just . Data.field "BillingCity" . _ColumnField . _3 ?~ fst astridsInvoicesOnlyRedactionExp
              & qrQuery . qFields . _Just . Data.field "BillingCountry" . _ColumnField . _3 ?~ fst frankAndRichardsInvoicesOnlyRedactionExp
      receivedInvoices <- Data.sortResponseRowsBy "InvoiceId" <$> queryGuarded query

      let expectedInvoices =
            _tdInvoicesRows
              <&> ( \row ->
                      row
                        & Data.field "BillingCity" %~ applyRedaction astridsInvoicesOnly row
                        & Data.field "BillingCountry" %~ applyRedaction frankAndRichardsInvoicesOnly row
                  )
              <&> Data.filterColumnsByQueryFields (query ^. qrQuery)

      Data.responseRows receivedInvoices `rowsShouldBe` expectedInvoices
      _qrAggregates receivedInvoices `jsonShouldBe` Nothing

    it "redaction is applied to specific column fields, not columns themselves more generally" $ do
      let redactionExpressions =
            TargetRedactionExpressions (TNTable _tdInvoicesTableName) $
              HashMap.fromList [astridsInvoicesOnlyRedactionExp]
      let query =
            invoiceQueryRequest
              & qrRedactionExpressions .~ Set.singleton redactionExpressions
              -- Copy the BillingCity field to UnredactedBillingCity before we apply the redaction expression to it
              & qrQuery . qFields . _Just . Data.fieldAt "UnredactedBillingCity" .~ (invoiceQueryRequest ^? qrQuery . qFields . _Just . Data.field "BillingCity")
              & qrQuery . qFields . _Just . Data.field "BillingCity" . _ColumnField . _3 ?~ fst astridsInvoicesOnlyRedactionExp
      receivedInvoices <- Data.sortResponseRowsBy "InvoiceId" <$> queryGuarded query

      let expectedInvoices =
            _tdInvoicesRows
              <&> ( \row ->
                      row
                        & Data.fieldAt "UnredactedBillingCity" .~ (row ^? Data.field "BillingCity")
                        & Data.field "BillingCity" %~ applyRedaction astridsInvoicesOnly row
                  )
              <&> Data.filterColumnsByQueryFields (query ^. qrQuery)

      Data.responseRows receivedInvoices `rowsShouldBe` expectedInvoices
      _qrAggregates receivedInvoices `jsonShouldBe` Nothing

    when (isJust $ _cRelationships) $ do
      it "redaction expressions can navigate relationships" $ do
        let redactionExpressions =
              TargetRedactionExpressions (TNTable _tdInvoicesTableName) $
                HashMap.fromList [elliesInvoicesOnlyRedactionExp]
        let query =
              invoiceQueryRequest
                & qrRelationships .~ Set.singleton (RTable $ Data.onlyKeepRelationships [_tdCustomerRelationshipName] _tdInvoicesTableRelationships)
                & qrRedactionExpressions .~ Set.singleton redactionExpressions
                & qrQuery . qFields . _Just . Data.field "BillingCity" . _ColumnField . _3 ?~ fst elliesInvoicesOnlyRedactionExp
        receivedInvoices <- Data.sortResponseRowsBy "InvoiceId" <$> queryGuarded query

        let expectedInvoices =
              _tdInvoicesRows
                <&> (\row -> row & Data.field "BillingCity" %~ applyRedaction elliesInvoicesOnly row)
                <&> Data.filterColumnsByQueryFields (query ^. qrQuery)

        Data.responseRows receivedInvoices `rowsShouldBe` expectedInvoices
        _qrAggregates receivedInvoices `jsonShouldBe` Nothing

      it "different tables can have different redaction expressions" $ do
        let redactionExpressions =
              Set.fromList
                [ TargetRedactionExpressions (TNTable _tdInvoicesTableName) $ HashMap.fromList [astridsInvoicesOnlyRedactionExp],
                  TargetRedactionExpressions (TNTable _tdCustomersTableName) $ HashMap.fromList [germanCustomersOnlyRedactionExp]
                ]
        let customerSubquery =
              Data.emptyQuery
                & qFields
                  ?~ Data.mkFieldsMap
                    [ ("CustomerId", _tdColumnField _tdCustomersTableName "CustomerId"),
                      ("Country", _tdColumnField _tdCustomersTableName "Country"),
                      ("City", _tdColumnField _tdCustomersTableName "City" & _ColumnField . _3 ?~ fst germanCustomersOnlyRedactionExp)
                    ]
        let customerRelationshipField =
              RelField $ RelationshipField _tdCustomerRelationshipName customerSubquery
        let query =
              invoiceQueryRequest
                & qrRelationships .~ Set.singleton (RTable $ Data.onlyKeepRelationships [_tdCustomerRelationshipName] _tdInvoicesTableRelationships)
                & qrRedactionExpressions .~ redactionExpressions
                & qrQuery . qFields . _Just . Data.field "BillingCity" . _ColumnField . _3 ?~ fst astridsInvoicesOnlyRedactionExp
                & qrQuery . qFields . _Just . Data.fieldAt "Customer" ?~ customerRelationshipField
        receivedInvoices <- Data.sortResponseRowsBy "InvoiceId" <$> queryGuarded query

        let joinInCustomer (invoice :: HashMap FieldName FieldValue) =
              let customer =
                    invoice ^? Data.field "CustomerId" . Data._ColumnFieldNumber
                      >>= (\customerId -> _tdCustomersRowsById ^? ix customerId)
                      <&> (\customer' -> customer' & Data.field "City" %~ applyRedaction germanCustomersOnly customer')
                      <&> Data.filterColumnsByQueryFields customerSubquery
                  customerPropVal = maybeToList customer
               in Data.insertField "Customer" (Data.mkSubqueryRowsFieldValue customerPropVal) invoice

        let expectedInvoices =
              _tdInvoicesRows
                <&> joinInCustomer
                <&> (\row -> row & Data.field "BillingCity" %~ applyRedaction astridsInvoicesOnly row)
                <&> Data.filterColumnsByQueryFields (query ^. qrQuery)

        Data.responseRows receivedInvoices `rowsShouldBe` expectedInvoices
        _qrAggregates receivedInvoices `jsonShouldBe` Nothing

  describe "Aggregation Redaction" $ do
    it "redaction is applied to the input of a single column aggregation function" $ do
      let redactionExpressions =
            TargetRedactionExpressions (TNTable _tdInvoicesTableName) $
              HashMap.fromList [frankAndRichardsInvoicesOnlyRedactionExp]
      let aggregates = Data.mkFieldsMap [("min", singleColumnAggregateMin (_tdColumnName "BillingCity") billingCityScalarType (fst frankAndRichardsInvoicesOnlyRedactionExp))]
      let query =
            invoiceQueryRequest
              & qrRedactionExpressions .~ Set.singleton redactionExpressions
              & qrQuery . qAggregates ?~ aggregates
              & qrQuery . qFields .~ Nothing
      response <- queryGuarded query

      let minimumBillingCity =
            _tdInvoicesRows
              <&> (\row -> row & Data.field "BillingCity" %~ applyRedaction frankAndRichardsInvoicesOnly row)
              & mapMaybe ((^? Data.field "BillingCity" . Data._ColumnFieldString))
              & minimum

      let expectedAggregates = Data.mkFieldsMap [("min", String minimumBillingCity)]

      Data.responseAggregates response `jsonShouldBe` expectedAggregates
      Data.responseRows response `rowsShouldBe` []

    it "redaction is applied to the input of a column count aggregation function" $ do
      let redactionExpressions =
            TargetRedactionExpressions (TNTable _tdInvoicesTableName) $
              HashMap.fromList [frankAndRichardsInvoicesOnlyRedactionExp]
      let aggregates = Data.mkFieldsMap [("distinctCount", ColumnCount $ ColumnCountAggregate (_tdColumnName "BillingCity") (Just $ fst frankAndRichardsInvoicesOnlyRedactionExp) True)]
      let query =
            invoiceQueryRequest
              & qrRedactionExpressions .~ Set.singleton redactionExpressions
              & qrQuery . qAggregates ?~ aggregates
              & qrQuery . qFields .~ Nothing
      response <- queryGuarded query

      let distinctBillingCitiesCount =
            _tdInvoicesRows
              <&> (\row -> row & Data.field "BillingCity" %~ applyRedaction frankAndRichardsInvoicesOnly row)
              & mapMaybe ((^? Data.field "BillingCity" . Data._ColumnFieldString))
              & Set.fromList
              & Set.size

      let expectedAggregates = Data.mkFieldsMap [("distinctCount", Number $ fromIntegral distinctBillingCitiesCount)]

      Data.responseAggregates response `jsonShouldBe` expectedAggregates
      Data.responseRows response `rowsShouldBe` []

  describe "OrderBy Redaction" $ do
    it "ordering is applied over the redacted column values" $ do
      let redactionExpressions =
            TargetRedactionExpressions (TNTable _tdInvoicesTableName) $
              HashMap.fromList [frankAndRichardsInvoicesOnlyRedactionExp]
      let orderBy =
            OrderBy mempty $
              NonEmpty.fromList
                [ _tdOrderByColumn [] "BillingCity" Ascending & obeTarget . _OrderByColumn . _2 ?~ fst frankAndRichardsInvoicesOnlyRedactionExp,
                  _tdOrderByColumn [] "InvoiceId" Descending
                ]
      let query =
            invoiceQueryRequest
              & qrRedactionExpressions .~ Set.singleton redactionExpressions
              & qrQuery . qOrderBy ?~ orderBy
      receivedInvoices <- queryGuarded query

      let expectedInvoices =
            _tdInvoicesRows
              & sortOn (\row -> (applyRedaction frankAndRichardsInvoicesOnly row <$> row ^? Data.field "BillingCity", Down (row ^? Data.field "InvoiceId")))
              <&> Data.filterColumnsByQueryFields (query ^. qrQuery)

      Data.responseRows receivedInvoices `rowsShouldBe` expectedInvoices
      _qrAggregates receivedInvoices `jsonShouldBe` Nothing

    when (isJust $ _cRelationships) $ do
      it "ordering over aggregates uses aggregations that are applied over redacted column values" $ do
        let redactionExpressions =
              TargetRedactionExpressions (TNTable _tdInvoicesTableName) $
                HashMap.fromList [invoiceIdsLte150OnlyRedactionExp]
        let orderByRelations =
              HashMap.fromList
                [ ( _tdInvoicesRelationshipName,
                    OrderByRelation
                      Nothing
                      mempty
                  )
                ]
        let orderBy =
              OrderBy orderByRelations $
                NonEmpty.fromList
                  [ OrderByElement [_tdInvoicesRelationshipName] (orderBySingleColumnAggregateMax (_tdColumnName "InvoiceId") invoiceIdScalarType (fst invoiceIdsLte150OnlyRedactionExp)) Descending,
                    _tdOrderByColumn [] "CustomerId" Ascending
                  ]
        let query =
              customerQueryRequest
                & qrRelationships .~ Set.singleton (RTable $ Data.onlyKeepRelationships [_tdInvoicesRelationshipName] _tdCustomersTableRelationships)
                & qrRedactionExpressions .~ Set.singleton redactionExpressions
                & qrQuery . qOrderBy ?~ orderBy
        receivedCustomers <- queryGuarded query

        let findRelatedInvoices (customer :: HashMap FieldName FieldValue) = fromMaybe [] do
              customerId <- customer ^? Data.field "CustomerId" . Data._ColumnFieldNumber
              pure $ filter (\invoice -> invoice ^? Data.field "CustomerId" . Data._ColumnFieldNumber == Just customerId) _tdInvoicesRows

        let expectedInvoices =
              _tdCustomersRows
                & sortOn
                  ( \customer ->
                      let maxInvoiceId =
                            findRelatedInvoices customer
                              & mapMaybe
                                ( \row -> do
                                    redactedValue <- applyRedaction invoiceIdsLte150Only row <$> row ^? Data.field "InvoiceId"
                                    -- Only return numbers for aggregation, eliminate nulls
                                    redactedValue ^? Data._ColumnFieldNumber
                                )
                              & NonEmpty.nonEmpty
                              <&> maximum
                       in (Down maxInvoiceId, customer ^? Data.field "CustomerId")
                  )
                <&> Data.filterColumnsByQueryFields (query ^. qrQuery)

        Data.responseRows receivedCustomers `rowsShouldBe` expectedInvoices
        _qrAggregates receivedCustomers `jsonShouldBe` Nothing

  describe "Filtering Redaction" $ do
    it "filtering is performed over redacted column values" $ do
      let redactionExpressions =
            TargetRedactionExpressions (TNTable _tdInvoicesTableName) $
              HashMap.fromList [astridsInvoicesOnlyRedactionExp]
      let whereExp =
            ApplyBinaryComparisonOperator
              GreaterThanOrEqual
              (_tdCurrentComparisonColumn "Total" totalScalarType & ccRedactionExpression ?~ fst astridsInvoicesOnlyRedactionExp)
              (Data.scalarValueComparison (Number 5) totalScalarType)
      let query =
            invoiceQueryRequest
              & qrRedactionExpressions .~ Set.singleton redactionExpressions
              & qrQuery . qWhere ?~ whereExp
      receivedInvoices <- Data.sortResponseRowsBy "InvoiceId" <$> queryGuarded query

      let expectedInvoices =
            _tdInvoicesRows
              & filter
                ( \row -> fromMaybe False $ do
                    redactedTotal <- applyRedaction astridsInvoicesOnly row <$> row ^? Data.field "Total"
                    total <- redactedTotal ^? Data._ColumnFieldNumber -- Eliminate nulls
                    pure $ total > 5
                )
              <&> Data.filterColumnsByQueryFields (query ^. qrQuery)

      Data.responseRows receivedInvoices `rowsShouldBe` expectedInvoices
      _qrAggregates receivedInvoices `jsonShouldBe` Nothing
  where
    astridsInvoicesOnly :: HashMap FieldName FieldValue -> Bool
    astridsInvoicesOnly row = row ^? Data.field "CustomerId" . Data._ColumnFieldNumber == Just 7

    astridsInvoicesOnlyRedactionExp :: (RedactionExpressionName, RedactionExpression)
    astridsInvoicesOnlyRedactionExp =
      ( RedactionExpressionName "AstridsInvoicesOnly",
        RedactionExpression $ ApplyBinaryComparisonOperator Equal (_tdCurrentComparisonColumn "CustomerId" customerIdScalarType) (Data.scalarValueComparison (Number 7) customerIdScalarType)
      )

    frankAndRichardsInvoicesOnly :: HashMap FieldName FieldValue -> Bool
    frankAndRichardsInvoicesOnly row = row ^? Data.field "CustomerId" . Data._ColumnFieldNumber `elem` [Just 24, Just 26]

    frankAndRichardsInvoicesOnlyRedactionExp :: (RedactionExpressionName, RedactionExpression)
    frankAndRichardsInvoicesOnlyRedactionExp =
      ( RedactionExpressionName "FrankAndRichardsInvoicesOnly",
        RedactionExpression $ ApplyBinaryArrayComparisonOperator In (_tdCurrentComparisonColumn "CustomerId" customerIdScalarType) [Number 24, Number 26] (customerIdScalarType)
      )

    elliesInvoicesOnly :: HashMap FieldName FieldValue -> Bool
    elliesInvoicesOnly row = fromMaybe False $ do
      customerId <- row ^? Data.field "CustomerId" . Data._ColumnFieldNumber
      customerRow <- HashMap.lookup customerId _tdCustomersRowsById
      pure $ customerRow ^? Data.field "FirstName" . Data._ColumnFieldString == Just "Ellie"

    elliesInvoicesOnlyRedactionExp :: (RedactionExpressionName, RedactionExpression)
    elliesInvoicesOnlyRedactionExp =
      ( RedactionExpressionName "ElliesInvoicesOnly",
        RedactionExpression $
          Exists (RelatedTable _tdCustomerRelationshipName) $
            ApplyBinaryComparisonOperator Equal (_tdCurrentComparisonColumn "FirstName" firstNameScalarType) (Data.scalarValueComparison (String "Ellie") firstNameScalarType)
      )

    invoiceIdsLte150Only :: HashMap FieldName FieldValue -> Bool
    invoiceIdsLte150Only row = row ^? Data.field "InvoiceId" . Data._ColumnFieldNumber <= Just 150

    invoiceIdsLte150OnlyRedactionExp :: (RedactionExpressionName, RedactionExpression)
    invoiceIdsLte150OnlyRedactionExp =
      ( RedactionExpressionName "InvoiceIdsLte150Only",
        RedactionExpression $ ApplyBinaryComparisonOperator LessThanOrEqual (_tdCurrentComparisonColumn "InvoiceId" invoiceIdScalarType) (Data.scalarValueComparison (Number 150) invoiceIdScalarType)
      )

    germanCustomersOnly :: HashMap FieldName FieldValue -> Bool
    germanCustomersOnly row = row ^? Data.field "Country" . Data._ColumnFieldString == Just "Germany"

    germanCustomersOnlyRedactionExp :: (RedactionExpressionName, RedactionExpression)
    germanCustomersOnlyRedactionExp =
      ( RedactionExpressionName "GermanCustomersOnly",
        RedactionExpression $ ApplyBinaryComparisonOperator Equal (_tdCurrentComparisonColumn "Country" countryScalarType) (Data.scalarValueComparison (String "Germany") countryScalarType)
      )

    applyRedaction :: (HashMap FieldName FieldValue -> Bool) -> HashMap FieldName FieldValue -> FieldValue -> FieldValue
    applyRedaction redactionPredicate row fieldValue = if redactionPredicate row then fieldValue else nullFieldValue

    invoiceQueryRequest :: QueryRequest
    invoiceQueryRequest =
      let fields =
            Data.mkFieldsMap
              [ ("InvoiceId", _tdColumnField _tdInvoicesTableName "InvoiceId"),
                ("CustomerId", _tdColumnField _tdInvoicesTableName "CustomerId"),
                ("BillingCountry", _tdColumnField _tdInvoicesTableName "BillingCountry"),
                ("BillingCity", _tdColumnField _tdInvoicesTableName "BillingCity")
              ]
          query = Data.emptyQuery & qFields ?~ fields
       in TableQueryRequest _tdInvoicesTableName mempty mempty mempty query Nothing

    customerQueryRequest :: QueryRequest
    customerQueryRequest =
      let fields =
            Data.mkFieldsMap
              [ ("CustomerId", _tdColumnField _tdCustomersTableName "CustomerId"),
                ("FirstName", _tdColumnField _tdCustomersTableName "FirstName"),
                ("LastName", _tdColumnField _tdCustomersTableName "LastName")
              ]
          query = Data.emptyQuery & qFields ?~ fields
       in TableQueryRequest _tdCustomersTableName mempty mempty mempty query Nothing

    singleColumnAggregateMin :: ColumnName -> ScalarType -> RedactionExpressionName -> Aggregate
    singleColumnAggregateMin columnName resultType redactionExpName = SingleColumn $ SingleColumnAggregate (SingleColumnAggregateFunction [G.name|min|]) columnName (Just redactionExpName) resultType

    orderBySingleColumnAggregateMax :: ColumnName -> ScalarType -> RedactionExpressionName -> OrderByTarget
    orderBySingleColumnAggregateMax columnName resultType redactionExpName = OrderBySingleColumnAggregate $ SingleColumnAggregate (SingleColumnAggregateFunction [G.name|max|]) columnName (Just redactionExpName) resultType

    invoiceIdScalarType = _tdFindColumnScalarType _tdInvoicesTableName "InvoiceId"
    customerIdScalarType = _tdFindColumnScalarType _tdInvoicesTableName "CustomerId"
    billingCityScalarType = _tdFindColumnScalarType _tdInvoicesTableName "BillingCity"
    totalScalarType = _tdFindColumnScalarType _tdInvoicesTableName "Total"
    firstNameScalarType = _tdFindColumnScalarType _tdCustomersTableName "FirstName"
    countryScalarType = _tdFindColumnScalarType _tdCustomersTableName "Country"
