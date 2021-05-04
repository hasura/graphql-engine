module Hasura.GraphQL.Schema.OrderBy
  ( orderByExp
  ) where

import           Hasura.Prelude

import qualified Language.GraphQL.Draft.Syntax as G

import           Data.Text.Extended

import qualified Hasura.GraphQL.Parser         as P
import qualified Hasura.RQL.IR.OrderBy         as IR
import qualified Hasura.RQL.IR.Select          as IR

import           Hasura.GraphQL.Parser         (InputFieldsParser, Kind (..), Parser,
                                                UnpreparedValue)
import           Hasura.GraphQL.Parser.Class
import           Hasura.GraphQL.Schema.Backend
import           Hasura.GraphQL.Schema.Common
import           Hasura.GraphQL.Schema.Table
import           Hasura.RQL.Types


-- | Corresponds to an object type for an order by.
--
-- > input table_order_by {
-- >   col1: order_by
-- >   col2: order_by
-- >   .     .
-- >   .     .
-- >   coln: order_by
-- >   obj-rel: <remote-table>_order_by
-- > }
orderByExp
  :: forall m n r b. (BackendSchema b, MonadSchema n m, MonadTableInfo r m, MonadRole r m)
  => TableName b
  -> SelPermInfo b
  -> m (Parser 'Input n [IR.AnnOrderByItemG b (UnpreparedValue b)])
orderByExp table selectPermissions = memoizeOn 'orderByExp table $ do
  tableGQLName <- getTableGQLName @b table
  let name = tableGQLName <> $$(G.litName "_order_by")
  let description = G.Description $
        "Ordering options when selecting data from " <> table <<> "."
  tableFields  <- tableSelectFields table selectPermissions
  fieldParsers <- sequenceA . catMaybes <$> traverse mkField tableFields
  pure $ concat . catMaybes <$> P.object name (Just description) fieldParsers
  where
    mkField
      :: FieldInfo b
      -> m (Maybe (InputFieldsParser n (Maybe [IR.AnnOrderByItemG b (UnpreparedValue b)])))
    mkField fieldInfo = runMaybeT $
      case fieldInfo of
        FIColumn columnInfo -> do
          let fieldName = pgiName columnInfo
          pure $ P.fieldOptional fieldName Nothing (orderByOperator @b)
            <&> fmap (pure . mkOrderByItemG @b (IR.AOCColumn columnInfo)) . join
        FIRelationship relationshipInfo -> do
          let remoteTable = riRTable relationshipInfo
          fieldName <- hoistMaybe $ G.mkName $ relNameToTxt $ riName relationshipInfo
          perms <- MaybeT $ tableSelectPermissions remoteTable
          let newPerms = fmapAnnBoolExp partialSQLExpToUnpreparedValue $ spiFilter perms
          case riType relationshipInfo of
            ObjRel -> do
              otherTableParser <- lift $ orderByExp remoteTable perms
              pure $ do
                otherTableOrderBy <- join <$> P.fieldOptional fieldName Nothing (P.nullable otherTableParser)
                pure $ fmap (map $ fmap $ IR.AOCObjectRelation relationshipInfo newPerms) otherTableOrderBy
            ArrRel -> do
              let aggregateFieldName = fieldName <> $$(G.litName "_aggregate")
              aggregationParser <- lift $ orderByAggregation remoteTable perms
              pure $ do
                aggregationOrderBy <- join <$> P.fieldOptional aggregateFieldName Nothing (P.nullable aggregationParser)
                pure $ fmap (map $ fmap $ IR.AOCArrayAggregation relationshipInfo newPerms) aggregationOrderBy
        FIComputedField _ -> empty
        FIRemoteRelationship _ -> empty


-- FIXME!
-- those parsers are directly using Postgres' SQL representation of
-- order, rather than using a general intermediary representation

orderByAggregation
  :: forall m n r b. (BackendSchema b, MonadSchema n m, MonadTableInfo r m, MonadRole r m)
  => TableName b
  -> SelPermInfo b
  -> m (Parser 'Input n [IR.OrderByItemG b (IR.AnnAggregateOrderBy b)])
orderByAggregation table selectPermissions = memoizeOn 'orderByAggregation table do
  -- WIP NOTE
  -- there is heavy duplication between this and Select.tableAggregationFields
  -- it might be worth putting some of it in common, just to avoid issues when
  -- we change one but not the other?
  tableGQLName <- getTableGQLName @b table
  allColumns   <- tableSelectColumns table selectPermissions
  let numColumns  = onlyNumCols allColumns
      compColumns = onlyComparableCols allColumns
      numFields   = catMaybes <$> traverse mkField numColumns
      compFields  = catMaybes <$> traverse mkField compColumns
      aggFields   = fmap (concat . catMaybes . concat) $ sequenceA $ catMaybes
        [ -- count
          Just $ P.fieldOptional $$(G.litName "count") Nothing (orderByOperator @b)
            <&> pure . fmap (pure . mkOrderByItemG @b IR.AAOCount) . join
        , -- operators on numeric columns
          if null numColumns then Nothing else Just $
          for numericAggOperators \operator ->
            parseOperator operator tableGQLName numFields
        , -- operators on comparable columns
          if null compColumns then Nothing else Just $
          for comparisonAggOperators \operator ->
            parseOperator operator tableGQLName compFields
        ]
  let objectName  = tableGQLName <> $$(G.litName "_aggregate_order_by")
      description = G.Description $ "order by aggregate values of table " <>> table
  pure $ P.object objectName (Just description) aggFields
  where
    mkField :: ColumnInfo b -> InputFieldsParser n (Maybe (ColumnInfo b, (BasicOrderType b, NullsOrderType b)))
    mkField columnInfo =
      P.fieldOptional (pgiName columnInfo) (pgiDescription columnInfo) (orderByOperator @b)
        <&> fmap (columnInfo,) . join

    parseOperator
      :: G.Name
      -> G.Name
      -> InputFieldsParser n [(ColumnInfo b, (BasicOrderType b, NullsOrderType b))]
      -> InputFieldsParser n (Maybe [IR.OrderByItemG b (IR.AnnAggregateOrderBy b)])
    parseOperator operator tableGQLName columns =
      let opText     = G.unName operator
          objectName = tableGQLName <> $$(G.litName "_") <> operator <> $$(G.litName "_order_by")
          objectDesc = Just $ G.Description $ "order by " <> opText <> "() on columns of table " <>> table
      in  P.fieldOptional operator Nothing (P.object objectName objectDesc columns)
        `mapField` map (\(col, info) -> mkOrderByItemG (IR.AAOOp opText col) info)

orderByOperator
  :: forall b n
   . (BackendSchema b, MonadParse n)
  => Parser 'Both n (Maybe (BasicOrderType b, NullsOrderType b))
orderByOperator =
  P.nullable $ P.enum $$(G.litName "order_by") (Just "column ordering options") $ orderByOperators @b

mkOrderByItemG :: forall b a . a -> (BasicOrderType b, NullsOrderType b) -> IR.OrderByItemG b a
mkOrderByItemG column (orderType, nullsOrder) =
  IR.OrderByItemG { obiType   = Just orderType
                  , obiColumn = column
                  , obiNulls  = Just  nullsOrder
                  }
