module Hasura.GraphQL.Schema.OrderBy
  ( orderByExp
  ) where

import           Hasura.Prelude

import qualified Data.List.NonEmpty                 as NE
import qualified Language.GraphQL.Draft.Syntax      as G

import           Data.Text.Extended

import qualified Hasura.Backends.Postgres.SQL.DML   as PG
import qualified Hasura.GraphQL.Parser              as P
import qualified Hasura.RQL.IR.OrderBy              as IR
import qualified Hasura.RQL.IR.Select               as IR

import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.GraphQL.Parser              (InputFieldsParser, Kind (..), Parser,
                                                     UnpreparedValue)
import           Hasura.GraphQL.Parser.Class
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
  :: forall m n r. (MonadSchema n m, MonadTableInfo r m, MonadRole r m)
  => QualifiedTable
  -> SelPermInfo 'Postgres
  -> m (Parser 'Input n [IR.AnnOrderByItemG 'Postgres (UnpreparedValue 'Postgres)])
orderByExp table selectPermissions = memoizeOn 'orderByExp table $ do
  tableGQLName <- getTableGQLName table
  let name = tableGQLName <> $$(G.litName "_order_by")
  let description = G.Description $
        "Ordering options when selecting data from " <> table <<> "."
  tableFields  <- tableSelectFields table selectPermissions
  fieldParsers <- sequenceA . catMaybes <$> traverse mkField tableFields
  pure $ concat . catMaybes <$> P.object name (Just description) fieldParsers
  where
    mkField
      :: FieldInfo 'Postgres
      -> m (Maybe (InputFieldsParser n (Maybe [IR.AnnOrderByItemG 'Postgres (UnpreparedValue 'Postgres)])))
    mkField fieldInfo = runMaybeT $
      case fieldInfo of
        FIColumn columnInfo -> do
          let fieldName = pgiName columnInfo
          pure $ P.fieldOptional fieldName Nothing orderByOperator
            <&> fmap (pure . mkOrderByItemG (IR.AOCColumn columnInfo)) . join
        FIRelationship relationshipInfo -> do
          let remoteTable = riRTable relationshipInfo
          fieldName <- MaybeT $ pure $ G.mkName $ relNameToTxt $ riName relationshipInfo
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



-- local definitions

type OrderInfo = (PG.OrderType, PG.NullsOrder)


-- FIXME!
-- those parsers are directly using Postgres' SQL representation of
-- order, rather than using a general intermediary representation

orderByAggregation
  :: forall m n r. (MonadSchema n m, MonadTableInfo r m, MonadRole r m)
  => QualifiedTable
  -> SelPermInfo 'Postgres
  -> m (Parser 'Input n [IR.OrderByItemG 'Postgres (IR.AnnAggregateOrderBy 'Postgres)])
orderByAggregation table selectPermissions = do
  -- WIP NOTE
  -- there is heavy duplication between this and Select.tableAggregationFields
  -- it might be worth putting some of it in common, just to avoid issues when
  -- we change one but not the other?
  tableGQLName <- getTableGQLName table
  allColumns   <- tableSelectColumns table selectPermissions
  let numColumns  = onlyNumCols allColumns
      compColumns = onlyComparableCols allColumns
      numFields   = catMaybes <$> traverse mkField numColumns
      compFields  = catMaybes <$> traverse mkField compColumns
      aggFields   = fmap (concat . catMaybes . concat) $ sequenceA $ catMaybes
        [ -- count
          Just $ P.fieldOptional $$(G.litName "count") Nothing orderByOperator
            <&> pure . fmap (pure . mkOrderByItemG IR.AAOCount) . join
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
    mkField :: ColumnInfo 'Postgres -> InputFieldsParser n (Maybe (ColumnInfo 'Postgres, OrderInfo))
    mkField columnInfo =
      P.fieldOptional (pgiName columnInfo) (pgiDescription columnInfo) orderByOperator
        <&> fmap (columnInfo,) . join

    parseOperator
      :: G.Name
      -> G.Name
      -> InputFieldsParser n [(ColumnInfo 'Postgres, OrderInfo)]
      -> InputFieldsParser n (Maybe [IR.OrderByItemG 'Postgres (IR.AnnAggregateOrderBy 'Postgres)])
    parseOperator operator tableGQLName columns =
      let opText     = G.unName operator
          objectName = tableGQLName <> $$(G.litName "_") <> operator <> $$(G.litName "_order_by")
          objectDesc = Just $ G.Description $ "order by " <> opText <> "() on columns of table " <>> table
      in  P.fieldOptional operator Nothing (P.object objectName objectDesc columns)
        `mapField` map (\(col, info) -> mkOrderByItemG (IR.AAOOp opText col) info)

orderByOperator :: MonadParse m => Parser 'Both m (Maybe OrderInfo)
orderByOperator =
  P.nullable $ P.enum $$(G.litName "order_by") (Just "column ordering options") $ NE.fromList
    [ ( define $$(G.litName "asc") "in ascending order, nulls last"
      , (PG.OTAsc, PG.NLast)
      )
    , ( define $$(G.litName "asc_nulls_first") "in ascending order, nulls first"
      , (PG.OTAsc, PG.NFirst)
      )
    , ( define $$(G.litName "asc_nulls_last") "in ascending order, nulls last"
      , (PG.OTAsc, PG.NLast)
      )
    , ( define $$(G.litName "desc") "in descending order, nulls first"
      , (PG.OTDesc, PG.NFirst)
      )
    , ( define $$(G.litName "desc_nulls_first") "in descending order, nulls first"
      , (PG.OTDesc, PG.NFirst)
      )
    , ( define $$(G.litName "desc_nulls_last") "in descending order, nulls last"
      , (PG.OTDesc, PG.NLast)
      )
    ]
  where
    define name desc = P.mkDefinition name (Just desc) P.EnumValueInfo



-- local helpers

mkOrderByItemG :: a -> OrderInfo -> IR.OrderByItemG 'Postgres a
mkOrderByItemG column (orderType, nullsOrder) =
  IR.OrderByItemG { obiType   = Just orderType
                  , obiColumn = column
                  , obiNulls  = Just  nullsOrder
                  }

aliasToName :: G.Name -> FieldName
aliasToName = FieldName . G.unName
