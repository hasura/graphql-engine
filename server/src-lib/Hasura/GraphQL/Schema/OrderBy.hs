{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Hasura.GraphQL.Schema.OrderBy
  ( tableOrderByExp,
    logicalModelOrderByExp,
  )
where

import Control.Lens ((^?))
import Data.Has
import Data.HashMap.Strict.Extended qualified as HashMap
import Data.Text.Casing qualified as C
import Data.Text.Extended
import Hasura.Base.Error
import Hasura.Function.Cache
import Hasura.GraphQL.Parser.Class
import Hasura.GraphQL.Schema.Backend
import Hasura.GraphQL.Schema.Common
import Hasura.GraphQL.Schema.Parser
  ( InputFieldsParser,
    Kind (..),
    Parser,
  )
import Hasura.GraphQL.Schema.Parser qualified as P
import Hasura.GraphQL.Schema.Table
import Hasura.GraphQL.Schema.Typename
import Hasura.LogicalModel.Cache (LogicalModelInfo (..))
import Hasura.LogicalModel.Common (getSelPermInfoForLogicalModel, logicalModelFieldsToFieldInfo)
import Hasura.LogicalModel.Types (LogicalModelName (..))
import Hasura.Name qualified as Name
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.IR.OrderBy qualified as IR
import Hasura.RQL.IR.Select qualified as IR
import Hasura.RQL.IR.Value qualified as IR
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.ComputedField
import Hasura.RQL.Types.NamingCase
import Hasura.RQL.Types.Relationships.Local
import Hasura.RQL.Types.SchemaCache hiding (askTableInfo)
import Hasura.RQL.Types.Source
import Hasura.RQL.Types.SourceCustomization
import Hasura.Table.Cache
import Language.GraphQL.Draft.Syntax qualified as G
import Type.Reflection

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
-- TODO: When there are no columns accessible to a role, the
-- `<table>_order_by` will be an empty input object. In such a case,
-- we can avoid exposing the `order_by` argument.
logicalModelOrderByExp ::
  forall b r m n.
  ( MonadBuildSchema b r m n
  ) =>
  LogicalModelInfo b ->
  SchemaT r m (Parser 'Input n [IR.AnnotatedOrderByItemG b (IR.UnpreparedValue b)])
logicalModelOrderByExp logicalModel = do
  roleName <- retrieve scRole
  let name = getLogicalModelName (_lmiName logicalModel)
      selectPermissions = getSelPermInfoForLogicalModel roleName logicalModel
      fieldInfos = HashMap.elems $ logicalModelFieldsToFieldInfo $ _lmiFields logicalModel
      description =
        G.Description
          $ "Ordering options when selecting data from "
          <> name
          <<> "."
      memoizeKey = name
  orderByExpInternal (C.fromCustomName name) description selectPermissions fieldInfos memoizeKey

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
orderByExpInternal ::
  forall b r m n name.
  (Ord name, Typeable name, MonadBuildSchema b r m n) =>
  C.GQLNameIdentifier ->
  G.Description ->
  Maybe (SelPermInfo b) ->
  [FieldInfo b] ->
  name ->
  SchemaT r m (Parser 'Input n [IR.AnnotatedOrderByItemG b (IR.UnpreparedValue b)])
orderByExpInternal gqlName description selectPermissions tableFields memoizeKey = do
  sourceInfo <- asks getter
  P.memoizeOn 'orderByExpInternal (_siName sourceInfo, memoizeKey) do
    let customization = _siCustomization sourceInfo
        tCase = _rscNamingConvention customization
        mkTypename = runMkTypename $ _rscTypeNames customization
    let name = mkTypename $ applyTypeNameCaseIdentifier tCase $ mkTableOrderByTypeName gqlName
    fieldParsers <- sequenceA . catMaybes <$> traverse (mkField sourceInfo tCase) tableFields
    pure $ concat . catMaybes <$> P.object name (Just description) fieldParsers
  where
    mkField ::
      SourceInfo b ->
      NamingCase ->
      FieldInfo b ->
      SchemaT r m (Maybe (InputFieldsParser n (Maybe [IR.AnnotatedOrderByItemG b (IR.UnpreparedValue b)])))
    mkField sourceInfo tCase fieldInfo = runMaybeT $ do
      selectPermissions' <- hoistMaybe selectPermissions
      roleName <- retrieve scRole
      case fieldInfo of
        FIColumn (SCIScalarColumn columnInfo) -> do
          let !fieldName = ciName columnInfo
          let redactionExp = fromMaybe NoRedaction $ getRedactionExprForColumn selectPermissions' (ciColumn columnInfo)
          orderByOperator @b tCase sourceInfo
            & P.fieldOptional fieldName Nothing
            <&> (fmap (pure . mkOrderByItemG @b (IR.AOCColumn columnInfo redactionExp)) . join)
            & pure
        FIColumn (SCIObjectColumn nestedObjectInfo@NestedObjectInfo {..}) -> do
          let !fieldName = _noiName
          logicalModelInfo <-
            HashMap.lookup _noiType (_siLogicalModels sourceInfo)
              `onNothing` throw500 ("Logical model " <> _noiType <<> " not found in source " <>> (_siName sourceInfo))
          nestedParser <- lift $ logicalModelOrderByExp @b @r @m @n logicalModelInfo
          pure $ do
            nestedOrderBy <- P.fieldOptional fieldName Nothing nestedParser
            pure $ fmap (map $ fmap $ IR.AOCNestedObject nestedObjectInfo) nestedOrderBy
        FIColumn (SCIArrayColumn _) -> empty
        FIRelationship relationshipInfo -> do
          case riTarget relationshipInfo of
            RelTargetNativeQuery _ -> hoistMaybe Nothing -- we do not support ordering by a nested Native Query yet
            RelTargetTable remoteTableName -> do
              remoteTableInfo <- askTableInfo remoteTableName
              perms <- hoistMaybe $ tableSelectPermissions roleName remoteTableInfo
              fieldName <- hoistMaybe $ G.mkName $ relNameToTxt $ riName relationshipInfo
              let newPerms = fmap partialSQLExpToUnpreparedValue <$> spiFilter perms
              case riType relationshipInfo of
                ObjRel -> do
                  otherTableParser <- lift $ tableOrderByExp remoteTableInfo
                  pure $ do
                    otherTableOrderBy <- join <$> P.fieldOptional fieldName Nothing (P.nullable otherTableParser)
                    pure $ fmap (map $ fmap $ IR.AOCObjectRelation relationshipInfo newPerms) otherTableOrderBy
                ArrRel -> do
                  let aggregateFieldName = applyFieldNameCaseIdentifier tCase $ C.fromAutogeneratedTuple (fieldName, [G.convertNameToSuffix Name._aggregate])
                  aggregationParser <- lift $ orderByAggregation sourceInfo remoteTableInfo
                  pure $ do
                    aggregationOrderBy <- join <$> P.fieldOptional aggregateFieldName Nothing (P.nullable aggregationParser)
                    pure $ fmap (map $ fmap $ IR.AOCArrayAggregation relationshipInfo newPerms) aggregationOrderBy
        FIComputedField ComputedFieldInfo {..} -> do
          let ComputedFieldFunction {..} = _cfiFunction
              mkComputedFieldOrderBy =
                let functionArgs =
                      flip FunctionArgsExp mempty
                        $ fromComputedFieldImplicitArguments @b IR.UVSession _cffComputedFieldImplicitArgs
                 in IR.ComputedFieldOrderBy _cfiXComputedFieldInfo _cfiName _cffName functionArgs
          fieldName <- hoistMaybe $ G.mkName $ toTxt _cfiName
          guard $ _cffInputArgs == mempty -- No input arguments other than table row and session argument
          case computedFieldReturnType @b _cfiReturnType of
            ReturnsScalar scalarType -> do
              let redactionExp = fromMaybe NoRedaction $ getRedactionExprForComputedField selectPermissions' _cfiName
              let computedFieldOrderBy = mkComputedFieldOrderBy $ IR.CFOBEScalar scalarType redactionExp
              pure
                $ P.fieldOptional
                  fieldName
                  Nothing
                  (orderByOperator @b tCase sourceInfo)
                <&> fmap (pure . mkOrderByItemG @b (IR.AOCComputedField computedFieldOrderBy))
                . join
            ReturnsTable table -> do
              let aggregateFieldName = applyFieldNameCaseIdentifier tCase $ C.fromAutogeneratedTuple (fieldName, [G.convertNameToSuffix Name._aggregate])
              tableInfo' <- askTableInfo table
              perms <- hoistMaybe $ tableSelectPermissions roleName tableInfo'
              let newPerms = fmap partialSQLExpToUnpreparedValue <$> spiFilter perms
              aggregationParser <- lift $ orderByAggregation sourceInfo tableInfo'
              pure $ do
                aggregationOrderBy <- join <$> P.fieldOptional aggregateFieldName Nothing (P.nullable aggregationParser)
                pure
                  $ fmap
                    ( map
                        $ fmap
                        $ IR.AOCComputedField
                        . mkComputedFieldOrderBy
                        . IR.CFOBETableAggregation table newPerms
                    )
                    aggregationOrderBy
            ReturnsOthers -> empty
        FIRemoteRelationship _ -> empty

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
tableOrderByExp ::
  forall b r m n.
  (MonadBuildSchema b r m n) =>
  TableInfo b ->
  SchemaT r m (Parser 'Input n [IR.AnnotatedOrderByItemG b (IR.UnpreparedValue b)])
tableOrderByExp tableInfo = do
  roleName <- retrieve scRole
  tableGQLName <- getTableIdentifierName tableInfo
  tableFields <- tableSelectFields tableInfo
  let selectPermissions = tableSelectPermissions roleName tableInfo
  let description =
        G.Description
          $ "Ordering options when selecting data from "
          <> tableInfoName tableInfo
          <<> "."
      memoizeKey = tableInfoName tableInfo
  orderByExpInternal tableGQLName description selectPermissions tableFields memoizeKey

-- FIXME!
-- those parsers are directly using Postgres' SQL representation of
-- order, rather than using a general intermediary representation

orderByAggregation ::
  forall b r m n.
  (MonadBuildSchema b r m n) =>
  SourceInfo b ->
  TableInfo b ->
  SchemaT r m (Parser 'Input n [IR.OrderByItemG b (IR.AnnotatedAggregateOrderBy b (IR.UnpreparedValue b))])
orderByAggregation sourceInfo tableInfo = P.memoizeOn 'orderByAggregation (_siName sourceInfo, tableName) do
  -- WIP NOTE
  -- there is heavy duplication between this and Select.tableAggregationFields
  -- it might be worth putting some of it in common, just to avoid issues when
  -- we change one but not the other?
  tableGQLName <- getTableIdentifierName @b tableInfo
  let customization = _siCustomization sourceInfo
      tCase = _rscNamingConvention customization
      mkTypename = _rscTypeNames customization
  tableIdentifierName <- getTableIdentifierName @b tableInfo
  allScalarColumns <- mapMaybe (\(column, redactionExp) -> column ^? _SCIScalarColumn <&> (,redactionExp)) <$> tableSelectColumns tableInfo
  let numColumns = stdAggOpColumns tCase $ filter (isNumCol . fst) allScalarColumns
      compColumns = stdAggOpColumns tCase $ filter (isComparableCol . fst) allScalarColumns
      numOperatorsAndColumns = HashMap.fromList $ (,numColumns) <$> numericAggOperators
      compOperatorsAndColumns = HashMap.fromList $ (,compColumns) <$> comparisonAggOperators
      customOperatorsAndColumns =
        HashMap.mapKeys (C.fromCustomName)
          $ getCustomAggOpsColumns tCase allScalarColumns
          <$> getCustomAggregateOperators @b (_siConfiguration sourceInfo)
      allOperatorsAndColumns =
        HashMap.catMaybes
          $ HashMap.unionsWith (<>) [numOperatorsAndColumns, compOperatorsAndColumns, customOperatorsAndColumns]
      aggFields =
        fmap (concat . catMaybes . concat)
          $ sequenceA
          $ catMaybes
            [ -- count
              Just
                $ P.fieldOptional
                  Name._count
                  Nothing
                  (orderByOperator @b tCase sourceInfo)
                <&> pure
                . fmap (pure . mkOrderByItemG @b IR.AAOCount)
                . join,
              -- other operators
              if null allOperatorsAndColumns
                then Nothing
                else Just
                  $ for (HashMap.toList allOperatorsAndColumns) \(operator, fields) -> do
                    parseOperator mkTypename operator tableGQLName tCase fields
            ]
  let objectName = runMkTypename mkTypename $ applyTypeNameCaseIdentifier tCase $ mkTableAggregateOrderByTypeName tableIdentifierName
      description = G.Description $ "order by aggregate values of table " <>> tableName
  pure $ P.object objectName (Just description) aggFields
  where
    tableName = tableInfoName tableInfo

    stdAggOpColumns ::
      NamingCase ->
      [(ColumnInfo b, AnnRedactionExpUnpreparedValue b)] ->
      Maybe (InputFieldsParser n [(ColumnInfo b, ColumnType b, AnnRedactionExpUnpreparedValue b, (BasicOrderType b, NullsOrderType b))])
    stdAggOpColumns tCase columns =
      columns
        -- ALl std aggregate functions return the same type as the column used with it
        & fmap (\(colInfo, redactionExp) -> (colInfo, ciType colInfo, redactionExp))
        & mkAgOpsFields tCase

    -- Build an InputFieldsParser only if the column list is non-empty
    mkAgOpsFields ::
      NamingCase ->
      -- Assoc list of column types with the type returned by the aggregate function when it is applied to that column
      [(ColumnInfo b, ColumnType b, AnnRedactionExpUnpreparedValue b)] ->
      Maybe (InputFieldsParser n [(ColumnInfo b, ColumnType b, AnnRedactionExpUnpreparedValue b, (BasicOrderType b, NullsOrderType b))])
    mkAgOpsFields tCase =
      fmap (fmap (catMaybes . toList) . traverse (mkField tCase)) . nonEmpty

    getCustomAggOpsColumns ::
      NamingCase ->
      -- All columns
      [(ColumnInfo b, AnnRedactionExpUnpreparedValue b)] ->
      -- Map of type the aggregate function accepts to the type it returns
      HashMap (ScalarType b) (ScalarType b) ->
      Maybe (InputFieldsParser n [(ColumnInfo b, ColumnType b, AnnRedactionExpUnpreparedValue b, (BasicOrderType b, NullsOrderType b))])
    getCustomAggOpsColumns tCase allColumns typeMap =
      allColumns
        -- Filter by columns with a scalar type supported by this aggregate function
        -- and retrieve the result type of the aggregate function
        & mapMaybe
          ( \(columnInfo, redactionExp) ->
              case ciType columnInfo of
                ColumnEnumReference _ -> Nothing
                ColumnScalar scalarType ->
                  (\resultType -> (columnInfo, ColumnScalar resultType, redactionExp)) <$> HashMap.lookup scalarType typeMap
          )
        & mkAgOpsFields tCase

    mkField ::
      NamingCase ->
      (ColumnInfo b, ColumnType b, AnnRedactionExpUnpreparedValue b) ->
      InputFieldsParser n (Maybe (ColumnInfo b, ColumnType b, AnnRedactionExpUnpreparedValue b, (BasicOrderType b, NullsOrderType b)))
    mkField tCase (columnInfo, resultType, redactionExp) =
      P.fieldOptional
        (ciName columnInfo)
        (ciDescription columnInfo)
        (orderByOperator @b tCase sourceInfo)
        <&> fmap (columnInfo,resultType,redactionExp,)
        . join

    parseOperator ::
      MkTypename ->
      C.GQLNameIdentifier ->
      C.GQLNameIdentifier ->
      NamingCase ->
      InputFieldsParser n [(ColumnInfo b, ColumnType b, AnnRedactionExpUnpreparedValue b, (BasicOrderType b, NullsOrderType b))] ->
      InputFieldsParser n (Maybe [IR.OrderByItemG b (IR.AnnotatedAggregateOrderBy b (IR.UnpreparedValue b))])
    parseOperator makeTypename operator tableGQLName tCase columns =
      let opText = G.unName $ applyFieldNameCaseIdentifier tCase operator
          opTypeName = applyTypeNameCaseIdentifier tCase $ mkTableAggregateOrderByOpTypeName tableGQLName operator
          opFieldName = applyFieldNameCaseIdentifier tCase operator
          objectName = runMkTypename makeTypename opTypeName
          objectDesc = Just $ G.Description $ "order by " <> opText <> "() on columns of table " <>> tableName
       in P.fieldOptional opFieldName Nothing (P.object objectName objectDesc columns)
            `mapField` map (\(col, resultType, redactionExp, info) -> mkOrderByItemG (IR.AAOOp $ IR.AggregateOrderByColumn opText resultType col redactionExp) info)

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
