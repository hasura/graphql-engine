{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

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
  )
import Hasura.GraphQL.Parser qualified as P
import Hasura.GraphQL.Parser.Class
import Hasura.GraphQL.Parser.Constants qualified as G
import Hasura.GraphQL.Schema.Backend
import Hasura.GraphQL.Schema.Common
import Hasura.GraphQL.Schema.Table
import Hasura.Prelude
import Hasura.RQL.IR.OrderBy qualified as IR
import Hasura.RQL.IR.Select qualified as IR
import Hasura.RQL.IR.Value qualified as IR
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.ComputedField
import Hasura.RQL.Types.Function
import Hasura.RQL.Types.Relationships.Local
import Hasura.RQL.Types.SchemaCache hiding (askTableInfo)
import Hasura.RQL.Types.Source
import Hasura.RQL.Types.SourceCustomization
import Hasura.RQL.Types.Table
import Language.GraphQL.Draft.Syntax qualified as G

{-# INLINE orderByOperator #-}
orderByOperator ::
  forall b n.
  (BackendSchema b, MonadParse n) =>
  NamingCase ->
  Parser 'Both n (Maybe (BasicOrderType b, NullsOrderType b))
orderByOperator tCase = case tCase of
  HasuraCase -> orderByOperatorsHasuraCase @b
  GraphqlCase -> orderByOperatorsGraphqlCase @b

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
  forall b r m n.
  MonadBuildSchema b r m n =>
  SourceInfo b ->
  TableInfo b ->
  m (Parser 'Input n [IR.AnnotatedOrderByItemG b (IR.UnpreparedValue b)])
orderByExp sourceInfo tableInfo = memoizeOn 'orderByExp (_siName sourceInfo, tableInfoName tableInfo) $ do
  tableGQLName <- getTableGQLName tableInfo
  tCase <- asks getter
  name <- P.mkTypename $ tableGQLName <> G.__order_by
  let description =
        G.Description $
          "Ordering options when selecting data from " <> tableInfoName tableInfo <<> "."
  tableFields <- tableSelectFields sourceInfo tableInfo
  fieldParsers <- sequenceA . catMaybes <$> traverse (mkField tCase) tableFields
  pure $ concat . catMaybes <$> P.object name (Just description) fieldParsers
  where
    mkField ::
      NamingCase ->
      FieldInfo b ->
      m (Maybe (InputFieldsParser n (Maybe [IR.AnnotatedOrderByItemG b (IR.UnpreparedValue b)])))
    mkField tCase fieldInfo = runMaybeT $
      case fieldInfo of
        FIColumn columnInfo -> do
          let fieldName = ciName columnInfo
          pure $
            P.fieldOptional
              fieldName
              Nothing
              (orderByOperator @b tCase)
              <&> fmap (pure . mkOrderByItemG @b (IR.AOCColumn columnInfo)) . join
        FIRelationship relationshipInfo -> do
          remoteTableInfo <- askTableInfo sourceInfo $ riRTable relationshipInfo
          fieldName <- hoistMaybe $ G.mkName $ relNameToTxt $ riName relationshipInfo
          perms <- MaybeT $ tableSelectPermissions remoteTableInfo
          let newPerms = fmap partialSQLExpToUnpreparedValue <$> spiFilter perms
          case riType relationshipInfo of
            ObjRel -> do
              otherTableParser <- lift $ orderByExp sourceInfo remoteTableInfo
              pure $ do
                otherTableOrderBy <- join <$> P.fieldOptional fieldName Nothing (P.nullable otherTableParser)
                pure $ fmap (map $ fmap $ IR.AOCObjectRelation relationshipInfo newPerms) otherTableOrderBy
            ArrRel -> do
              let aggregateFieldName = fieldName <> G.__aggregate
              aggregationParser <- lift $ orderByAggregation sourceInfo remoteTableInfo
              pure $ do
                aggregationOrderBy <- join <$> P.fieldOptional aggregateFieldName Nothing (P.nullable aggregationParser)
                pure $ fmap (map $ fmap $ IR.AOCArrayAggregation relationshipInfo newPerms) aggregationOrderBy
        FIComputedField ComputedFieldInfo {..} -> do
          let ComputedFieldFunction {..} = _cfiFunction
              mkComputedFieldOrderBy =
                let functionArgs =
                      flip FunctionArgsExp mempty $
                        fromComputedFieldImplicitArguments @b IR.UVSession _cffComputedFieldImplicitArgs
                 in IR.ComputedFieldOrderBy _cfiXComputedFieldInfo _cfiName _cffName functionArgs
          fieldName <- hoistMaybe $ G.mkName $ toTxt _cfiName
          guard $ _cffInputArgs == mempty -- No input arguments other than table row and session argument
          case computedFieldReturnType @b _cfiReturnType of
            ReturnsScalar scalarType -> do
              let computedFieldOrderBy = mkComputedFieldOrderBy $ IR.CFOBEScalar scalarType
              pure $
                P.fieldOptional
                  fieldName
                  Nothing
                  (orderByOperator @b tCase)
                  <&> fmap (pure . mkOrderByItemG @b (IR.AOCComputedField computedFieldOrderBy)) . join
            ReturnsTable table -> do
              let aggregateFieldName = fieldName <> G.__aggregate
              tableInfo' <- askTableInfo sourceInfo table
              perms <- MaybeT $ tableSelectPermissions tableInfo'
              let newPerms = fmap partialSQLExpToUnpreparedValue <$> spiFilter perms
              aggregationParser <- lift $ orderByAggregation sourceInfo tableInfo'
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
            ReturnsOthers -> empty
        FIRemoteRelationship _ -> empty

-- FIXME!
-- those parsers are directly using Postgres' SQL representation of
-- order, rather than using a general intermediary representation

orderByAggregation ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  SourceInfo b ->
  TableInfo b ->
  m (Parser 'Input n [IR.OrderByItemG b (IR.AnnotatedAggregateOrderBy b)])
orderByAggregation sourceInfo tableInfo = memoizeOn 'orderByAggregation (_siName sourceInfo, tableName) do
  -- WIP NOTE
  -- there is heavy duplication between this and Select.tableAggregationFields
  -- it might be worth putting some of it in common, just to avoid issues when
  -- we change one but not the other?
  tableGQLName <- getTableGQLName @b tableInfo
  tCase <- asks getter
  allColumns <- tableSelectColumns sourceInfo tableInfo
  mkTypename <- asks getter
  let numColumns = onlyNumCols allColumns
      compColumns = onlyComparableCols allColumns
      numFields = catMaybes <$> traverse (mkField tCase) numColumns
      compFields = catMaybes <$> traverse (mkField tCase) compColumns
      aggFields =
        fmap (concat . catMaybes . concat) $
          sequenceA $
            catMaybes
              [ -- count
                Just $
                  P.fieldOptional
                    G._count
                    Nothing
                    (orderByOperator @b tCase)
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
  objectName <- P.mkTypename $ tableGQLName <> G.__aggregate_order_by
  let description = G.Description $ "order by aggregate values of table " <>> tableName
  pure $ P.object objectName (Just description) aggFields
  where
    tableName = tableInfoName tableInfo

    mkField :: NamingCase -> ColumnInfo b -> InputFieldsParser n (Maybe (ColumnInfo b, (BasicOrderType b, NullsOrderType b)))
    mkField tCase columnInfo =
      P.fieldOptional
        (ciName columnInfo)
        (ciDescription columnInfo)
        (orderByOperator @b tCase)
        <&> fmap (columnInfo,) . join

    parseOperator ::
      P.MkTypename ->
      G.Name ->
      G.Name ->
      InputFieldsParser n [(ColumnInfo b, (BasicOrderType b, NullsOrderType b))] ->
      InputFieldsParser n (Maybe [IR.OrderByItemG b (IR.AnnotatedAggregateOrderBy b)])
    parseOperator mkTypename operator tableGQLName columns =
      let opText = G.unName operator
          objectName = P.runMkTypename mkTypename $ tableGQLName <> G.__ <> operator <> G.__order_by
          objectDesc = Just $ G.Description $ "order by " <> opText <> "() on columns of table " <>> tableName
       in P.fieldOptional operator Nothing (P.object objectName objectDesc columns)
            `mapField` map (\(col, info) -> mkOrderByItemG (IR.AAOOp opText col) info)

orderByOperatorsHasuraCase ::
  forall b n.
  (BackendSchema b, MonadParse n) =>
  Parser 'Both n (Maybe (BasicOrderType b, NullsOrderType b))
orderByOperatorsHasuraCase = orderByOperator' @b HasuraCase

orderByOperatorsGraphqlCase ::
  forall b n.
  (BackendSchema b, MonadParse n) =>
  Parser 'Both n (Maybe (BasicOrderType b, NullsOrderType b))
orderByOperatorsGraphqlCase = orderByOperator' @b GraphqlCase

orderByOperator' ::
  forall b n.
  (BackendSchema b, MonadParse n) =>
  NamingCase ->
  Parser 'Both n (Maybe (BasicOrderType b, NullsOrderType b))
orderByOperator' tCase =
  P.nullable $ P.enum (applyFieldNameCaseCust tCase G._order_by) (Just "column ordering options") $ orderByOperators @b tCase

mkOrderByItemG :: forall b a. a -> (BasicOrderType b, NullsOrderType b) -> IR.OrderByItemG b a
mkOrderByItemG column (orderType, nullsOrder) =
  IR.OrderByItemG
    { obiType = Just orderType,
      obiColumn = column,
      obiNulls = Just nullsOrder
    }
