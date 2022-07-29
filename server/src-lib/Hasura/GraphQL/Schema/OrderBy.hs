{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Hasura.GraphQL.Schema.OrderBy
  ( orderByExp,
  )
where

import Data.Has
import Data.Text.Casing qualified as C
import Data.Text.Extended
import Hasura.GraphQL.Parser.Class
import Hasura.GraphQL.Schema.Backend
import Hasura.GraphQL.Schema.Common
import Hasura.GraphQL.Schema.NamingCase
import Hasura.GraphQL.Schema.Parser
  ( InputFieldsParser,
    Kind (..),
    Parser,
  )
import Hasura.GraphQL.Schema.Parser qualified as P
import Hasura.GraphQL.Schema.Table
import Hasura.GraphQL.Schema.Typename
import Hasura.Name qualified as Name
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
  SourceInfo b ->
  Parser 'Both n (Maybe (BasicOrderType b, NullsOrderType b))
orderByOperator tCase sourceInfo = case tCase of
  HasuraCase -> orderByOperatorsHasuraCase @b sourceInfo
  GraphqlCase -> orderByOperatorsGraphqlCase @b sourceInfo

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
  name <- mkTypename $ tableGQLName <> Name.__order_by
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
    mkField tCase fieldInfo = runMaybeT $ do
      roleName <- retrieve scRole
      case fieldInfo of
        FIColumn columnInfo -> do
          let fieldName = ciName columnInfo
          pure $
            P.fieldOptional
              fieldName
              Nothing
              (orderByOperator @b tCase sourceInfo)
              <&> fmap (pure . mkOrderByItemG @b (IR.AOCColumn columnInfo)) . join
        FIRelationship relationshipInfo -> do
          remoteTableInfo <- askTableInfo sourceInfo $ riRTable relationshipInfo
          perms <- hoistMaybe $ tableSelectPermissions roleName remoteTableInfo
          fieldName <- hoistMaybe $ G.mkName $ relNameToTxt $ riName relationshipInfo
          let newPerms = fmap partialSQLExpToUnpreparedValue <$> spiFilter perms
          case riType relationshipInfo of
            ObjRel -> do
              otherTableParser <- lift $ orderByExp sourceInfo remoteTableInfo
              pure $ do
                otherTableOrderBy <- join <$> P.fieldOptional fieldName Nothing (P.nullable otherTableParser)
                pure $ fmap (map $ fmap $ IR.AOCObjectRelation relationshipInfo newPerms) otherTableOrderBy
            ArrRel -> do
              let aggregateFieldName = applyFieldNameCaseIdentifier tCase $ C.fromTuple (fieldName, [G.convertNameToSuffix Name._aggregate])
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
                  (orderByOperator @b tCase sourceInfo)
                  <&> fmap (pure . mkOrderByItemG @b (IR.AOCComputedField computedFieldOrderBy)) . join
            ReturnsTable table -> do
              let aggregateFieldName = applyFieldNameCaseIdentifier tCase $ C.fromTuple (fieldName, [G.convertNameToSuffix Name._aggregate])
              tableInfo' <- askTableInfo sourceInfo table
              perms <- hoistMaybe $ tableSelectPermissions roleName tableInfo'
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
  makeTypename <- asks getter
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
                    Name._count
                    Nothing
                    (orderByOperator @b tCase sourceInfo)
                    <&> pure . fmap (pure . mkOrderByItemG @b IR.AAOCount) . join,
                -- operators on numeric columns
                if null numColumns
                  then Nothing
                  else Just $
                    for numericAggOperators \operator ->
                      parseOperator makeTypename operator tableGQLName numFields,
                -- operators on comparable columns
                if null compColumns
                  then Nothing
                  else Just $
                    for comparisonAggOperators \operator ->
                      parseOperator makeTypename operator tableGQLName compFields
              ]
  objectName <- mkTypename $ tableGQLName <> Name.__aggregate_order_by
  let description = G.Description $ "order by aggregate values of table " <>> tableName
  pure $ P.object objectName (Just description) aggFields
  where
    tableName = tableInfoName tableInfo

    mkField :: NamingCase -> ColumnInfo b -> InputFieldsParser n (Maybe (ColumnInfo b, (BasicOrderType b, NullsOrderType b)))
    mkField tCase columnInfo =
      P.fieldOptional
        (ciName columnInfo)
        (ciDescription columnInfo)
        (orderByOperator @b tCase sourceInfo)
        <&> fmap (columnInfo,) . join

    parseOperator ::
      MkTypename ->
      G.Name ->
      G.Name ->
      InputFieldsParser n [(ColumnInfo b, (BasicOrderType b, NullsOrderType b))] ->
      InputFieldsParser n (Maybe [IR.OrderByItemG b (IR.AnnotatedAggregateOrderBy b)])
    parseOperator makeTypename operator tableGQLName columns =
      let opText = G.unName operator
          objectName = runMkTypename makeTypename $ tableGQLName <> Name.__ <> operator <> Name.__order_by
          objectDesc = Just $ G.Description $ "order by " <> opText <> "() on columns of table " <>> tableName
       in P.fieldOptional operator Nothing (P.object objectName objectDesc columns)
            `mapField` map (\(col, info) -> mkOrderByItemG (IR.AAOOp opText col) info)

orderByOperatorsHasuraCase ::
  forall b n.
  (BackendSchema b, MonadParse n) =>
  SourceInfo b ->
  Parser 'Both n (Maybe (BasicOrderType b, NullsOrderType b))
orderByOperatorsHasuraCase = orderByOperator' @b HasuraCase

orderByOperatorsGraphqlCase ::
  forall b n.
  (BackendSchema b, MonadParse n) =>
  SourceInfo b ->
  Parser 'Both n (Maybe (BasicOrderType b, NullsOrderType b))
orderByOperatorsGraphqlCase = orderByOperator' @b GraphqlCase

orderByOperator' ::
  forall b n.
  (BackendSchema b, MonadParse n) =>
  NamingCase ->
  SourceInfo b ->
  Parser 'Both n (Maybe (BasicOrderType b, NullsOrderType b))
orderByOperator' tCase sourceInfo =
  let (sourcePrefix, orderOperators) = orderByOperators @b sourceInfo tCase
   in P.nullable $ P.enum (applyTypeNameCaseCust tCase sourcePrefix) (Just "column ordering options") $ orderOperators

mkOrderByItemG :: forall b a. a -> (BasicOrderType b, NullsOrderType b) -> IR.OrderByItemG b a
mkOrderByItemG column (orderType, nullsOrder) =
  IR.OrderByItemG
    { obiType = Just orderType,
      obiColumn = column,
      obiNulls = Just nullsOrder
    }
