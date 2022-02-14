{-# LANGUAGE ApplicativeDo #-}

module Hasura.GraphQL.Schema.OrderBy
  ( orderByExp,
  )
where

import Data.Has
import Data.Text.Extended
import Hasura.GraphQL.Parser
  ( InputFieldsParser,
    Kind (..),
    Parser,
    UnpreparedValue,
  )
import Hasura.GraphQL.Parser qualified as P
import Hasura.GraphQL.Parser.Class
import Hasura.GraphQL.Schema.Backend
import Hasura.GraphQL.Schema.Common
import Hasura.GraphQL.Schema.Table
import Hasura.Prelude
import Hasura.RQL.IR.OrderBy qualified as IR
import Hasura.RQL.IR.Select qualified as IR
import Hasura.RQL.Types
import Language.GraphQL.Draft.Syntax qualified as G

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
orderByExp ::
  forall m n r b.
  (BackendSchema b, MonadSchema n m, MonadTableInfo r m, MonadRole r m, Has P.MkTypename r) =>
  SourceName ->
  TableInfo b ->
  SelPermInfo b ->
  m (Parser 'Input n [IR.AnnotatedOrderByItemG b (UnpreparedValue b)])
orderByExp sourceName tableInfo selectPermissions = memoizeOn 'orderByExp (sourceName, tableInfoName tableInfo) $ do
  tableGQLName <- getTableGQLName tableInfo
  name <- P.mkTypename $ tableGQLName <> $$(G.litName "_order_by")
  let description =
        G.Description $
          "Ordering options when selecting data from " <> tableInfoName tableInfo <<> "."
  tableFields <- tableSelectFields sourceName tableInfo selectPermissions
  fieldParsers <- sequenceA . catMaybes <$> traverse mkField tableFields
  pure $ concat . catMaybes <$> P.object name (Just description) fieldParsers
  where
    mkField ::
      FieldInfo b ->
      m (Maybe (InputFieldsParser n (Maybe [IR.AnnotatedOrderByItemG b (UnpreparedValue b)])))
    mkField fieldInfo = runMaybeT $
      case fieldInfo of
        FIColumn columnInfo -> do
          let fieldName = ciName columnInfo
          pure $
            P.fieldOptional fieldName Nothing (orderByOperator @b)
              <&> fmap (pure . mkOrderByItemG @b (IR.AOCColumn columnInfo)) . join
        FIRelationship relationshipInfo -> do
          remoteTableInfo <- askTableInfo @b sourceName $ riRTable relationshipInfo
          fieldName <- hoistMaybe $ G.mkName $ relNameToTxt $ riName relationshipInfo
          perms <- MaybeT $ tableSelectPermissions remoteTableInfo
          let newPerms = fmap partialSQLExpToUnpreparedValue <$> spiFilter perms
          case riType relationshipInfo of
            ObjRel -> do
              otherTableParser <- lift $ orderByExp sourceName remoteTableInfo perms
              pure $ do
                otherTableOrderBy <- join <$> P.fieldOptional fieldName Nothing (P.nullable otherTableParser)
                pure $ fmap (map $ fmap $ IR.AOCObjectRelation relationshipInfo newPerms) otherTableOrderBy
            ArrRel -> do
              let aggregateFieldName = fieldName <> $$(G.litName "_aggregate")
              aggregationParser <- lift $ orderByAggregation sourceName remoteTableInfo perms
              pure $ do
                aggregationOrderBy <- join <$> P.fieldOptional aggregateFieldName Nothing (P.nullable aggregationParser)
                pure $ fmap (map $ fmap $ IR.AOCArrayAggregation relationshipInfo newPerms) aggregationOrderBy
        FIComputedField ComputedFieldInfo {..} -> do
          let ComputedFieldFunction {..} = _cfiFunction
              mkComputedFieldOrderBy =
                let functionArgs =
                      flip IR.FunctionArgsExp mempty $
                        IR.functionArgsWithTableRowAndSession P.UVSession _cffTableArgument _cffSessionArgument
                 in IR.ComputedFieldOrderBy _cfiXComputedFieldInfo _cfiName _cffName functionArgs
          fieldName <- hoistMaybe $ G.mkName $ toTxt _cfiName
          guard $ _cffInputArgs == mempty -- No input arguments other than table row and session argument
          case _cfiReturnType of
            CFRScalar scalarType -> do
              let computedFieldOrderBy = mkComputedFieldOrderBy $ IR.CFOBEScalar scalarType
              pure $
                P.fieldOptional fieldName Nothing (orderByOperator @b)
                  <&> fmap (pure . mkOrderByItemG @b (IR.AOCComputedField computedFieldOrderBy)) . join
            CFRSetofTable table -> do
              let aggregateFieldName = fieldName <> $$(G.litName "_aggregate")
              tableInfo' <- askTableInfo @b sourceName table
              perms <- MaybeT $ tableSelectPermissions tableInfo'
              let newPerms = fmap partialSQLExpToUnpreparedValue <$> spiFilter perms
              aggregationParser <- lift $ orderByAggregation sourceName tableInfo' perms
              pure $ do
                aggregationOrderBy <- join <$> P.fieldOptional aggregateFieldName Nothing (P.nullable aggregationParser)
                pure $
                  fmap
                    ( map $
                        fmap $
                          IR.AOCComputedField
                            . mkComputedFieldOrderBy
                            . IR.CFOBETableAggregation table newPerms
                    )
                    aggregationOrderBy
        FIRemoteRelationship _ -> empty

-- FIXME!
-- those parsers are directly using Postgres' SQL representation of
-- order, rather than using a general intermediary representation

orderByAggregation ::
  forall m n r b.
  (BackendSchema b, MonadSchema n m, MonadTableInfo r m, MonadRole r m, Has P.MkTypename r) =>
  SourceName ->
  TableInfo b ->
  SelPermInfo b ->
  m (Parser 'Input n [IR.OrderByItemG b (IR.AnnotatedAggregateOrderBy b)])
orderByAggregation sourceName tableInfo selectPermissions = memoizeOn 'orderByAggregation (sourceName, tableName) do
  -- WIP NOTE
  -- there is heavy duplication between this and Select.tableAggregationFields
  -- it might be worth putting some of it in common, just to avoid issues when
  -- we change one but not the other?
  tableGQLName <- getTableGQLName @b tableInfo
  allColumns <- tableSelectColumns sourceName tableInfo selectPermissions
  mkTypename <- asks getter
  let numColumns = onlyNumCols allColumns
      compColumns = onlyComparableCols allColumns
      numFields = catMaybes <$> traverse mkField numColumns
      compFields = catMaybes <$> traverse mkField compColumns
      aggFields =
        fmap (concat . catMaybes . concat) $
          sequenceA $
            catMaybes
              [ -- count
                Just $
                  P.fieldOptional $$(G.litName "count") Nothing (orderByOperator @b)
                    <&> pure . fmap (pure . mkOrderByItemG @b IR.AAOCount) . join,
                -- operators on numeric columns
                if null numColumns
                  then Nothing
                  else Just $
                    for numericAggOperators \operator ->
                      parseOperator mkTypename operator tableGQLName numFields,
                -- operators on comparable columns
                if null compColumns
                  then Nothing
                  else Just $
                    for comparisonAggOperators \operator ->
                      parseOperator mkTypename operator tableGQLName compFields
              ]
  objectName <- P.mkTypename $ tableGQLName <> $$(G.litName "_aggregate_order_by")
  let description = G.Description $ "order by aggregate values of table " <>> tableName
  pure $ P.object objectName (Just description) aggFields
  where
    tableName = tableInfoName tableInfo

    mkField :: ColumnInfo b -> InputFieldsParser n (Maybe (ColumnInfo b, (BasicOrderType b, NullsOrderType b)))
    mkField columnInfo =
      P.fieldOptional (ciName columnInfo) (ciDescription columnInfo) (orderByOperator @b)
        <&> fmap (columnInfo,) . join

    parseOperator ::
      P.MkTypename ->
      G.Name ->
      G.Name ->
      InputFieldsParser n [(ColumnInfo b, (BasicOrderType b, NullsOrderType b))] ->
      InputFieldsParser n (Maybe [IR.OrderByItemG b (IR.AnnotatedAggregateOrderBy b)])
    parseOperator mkTypename operator tableGQLName columns =
      let opText = G.unName operator
          objectName = P.runMkTypename mkTypename $ tableGQLName <> $$(G.litName "_") <> operator <> $$(G.litName "_order_by")
          objectDesc = Just $ G.Description $ "order by " <> opText <> "() on columns of table " <>> tableName
       in P.fieldOptional operator Nothing (P.object objectName objectDesc columns)
            `mapField` map (\(col, info) -> mkOrderByItemG (IR.AAOOp opText col) info)

orderByOperator ::
  forall b n.
  (BackendSchema b, MonadParse n) =>
  Parser 'Both n (Maybe (BasicOrderType b, NullsOrderType b))
orderByOperator =
  P.nullable $ P.enum $$(G.litName "order_by") (Just "column ordering options") $ orderByOperators @b

mkOrderByItemG :: forall b a. a -> (BasicOrderType b, NullsOrderType b) -> IR.OrderByItemG b a
mkOrderByItemG column (orderType, nullsOrder) =
  IR.OrderByItemG
    { obiType = Just orderType,
      obiColumn = column,
      obiNulls = Just nullsOrder
    }
