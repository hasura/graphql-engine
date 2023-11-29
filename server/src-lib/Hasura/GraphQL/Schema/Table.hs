{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Helper functions for generating the schema of database tables
module Hasura.GraphQL.Schema.Table
  ( getTableGQLName,
    tableSelectColumnsEnum,
    tableSelectColumnsPredEnum,
    tableUpdateColumnsEnum,
    updateColumnsPlaceholderParser,
    tableSelectPermissions,
    tableSelectFields,
    tableColumns,
    tableSelectColumns,
    tableSelectComputedFields,
    tableUpdateColumns,
    getTableIdentifierName,
  )
where

import Control.Lens ((^?))
import Data.Has
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as Set
import Data.Text.Casing (GQLNameIdentifier)
import Data.Text.Casing qualified as C
import Data.Text.Extended
import Hasura.Base.Error (QErr)
import Hasura.GraphQL.Schema.Backend
import Hasura.GraphQL.Schema.Common
import Hasura.GraphQL.Schema.Parser (Kind (..), Parser)
import Hasura.GraphQL.Schema.Parser qualified as P
import Hasura.GraphQL.Schema.Typename
import Hasura.LogicalModel.Common (getSelPermInfoForLogicalModel)
import Hasura.Name qualified as Name
import Hasura.NativeQuery.Cache (NativeQueryInfo (_nqiReturns))
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp (AnnRedactionExpPartialSQL, AnnRedactionExpUnpreparedValue)
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.ComputedField
import Hasura.RQL.Types.Relationships.Local
import Hasura.RQL.Types.Roles (RoleName)
import Hasura.RQL.Types.SchemaCache hiding (askTableInfo)
import Hasura.RQL.Types.Source
import Hasura.RQL.Types.SourceCustomization
import Hasura.Table.Cache
import Language.GraphQL.Draft.Syntax qualified as G

-- | Helper function to get the table GraphQL name. A table may have a
-- custom name configured with it. When the custom name exists, the GraphQL nodes
-- that are generated according to the custom name. For example: Let's say,
-- we have a table called `users address`, the name of the table is not GraphQL
-- compliant so we configure the table with a GraphQL compliant name,
-- say `users_address`
-- The generated top-level nodes of this table will be like `users_address`,
-- `insert_users_address` etc
getTableGQLName ::
  forall b m.
  (Backend b, MonadError QErr m) =>
  TableInfo b ->
  m G.Name
getTableGQLName tableInfo = do
  let coreInfo = _tiCoreInfo tableInfo
      tableName = _tciName coreInfo
      tableCustomName = _tcCustomName $ _tciCustomConfig coreInfo
  tableCustomName
    `onNothing` tableGraphQLName @b tableName
    `onLeft` throwError

-- | similar to @getTableGQLName@ but returns table name as a list with name pieces
--   instead of concatenating schema and table name together.
getTableIdentifierName ::
  forall b m.
  (Backend b, MonadError QErr m) =>
  TableInfo b ->
  m (C.GQLNameIdentifier)
getTableIdentifierName tableInfo =
  let coreInfo = _tiCoreInfo tableInfo
      tableName = _tciName coreInfo
      tableCustomName = fmap C.fromCustomName $ _tcCustomName $ _tciCustomConfig coreInfo
   in onNothing
        tableCustomName
        (liftEither $ getTableIdentifier @b tableName)

-- | Table select columns enum
--
-- Parser for an enum type that matches the columns of the given
-- table. Used as a parameter for "distinct", among others. Maps to
-- the table_select_column object.
--
-- Return Nothing if there's no column the current user has "select"
-- permissions for.
tableSelectColumnsEnum ::
  forall b r m n.
  (MonadBuildSchema b r m n) =>
  TableInfo b ->
  SchemaT r m (Maybe (Parser 'Both n (Column b, AnnRedactionExpUnpreparedValue b)))
tableSelectColumnsEnum tableInfo = do
  customization <- retrieve $ _siCustomization @b
  let tCase = _rscNamingConvention customization
      mkTypename = runMkTypename $ _rscTypeNames customization
  tableGQLName <- getTableIdentifierName @b tableInfo
  columnsWithRedactionExps <- tableSelectColumns tableInfo
  let columns = fst <$> columnsWithRedactionExps
  let enumName = mkTypename $ applyTypeNameCaseIdentifier tCase $ mkTableSelectColumnTypeName tableGQLName
      description =
        Just
          $ G.Description
          $ "select columns of table "
          <>> tableInfoName tableInfo
  -- We noticed many 'Definition's allocated, from 'define' below, so memoize
  -- to gain more sharing and lower memory residency.
  let columnDefinitions =
        columnsWithRedactionExps
          <&> ( \(structuredColumnInfo, redactionExp) ->
                  let definition = define $ structuredColumnInfoName structuredColumnInfo
                      column = structuredColumnInfoColumn structuredColumnInfo
                   in (definition, (column, redactionExp))
              )
          & nonEmpty
  case columnDefinitions of
    Nothing -> pure Nothing
    Just columnDefinitions' ->
      Just
        <$> P.memoizeOn
          'tableSelectColumnsEnum
          (enumName, description, columns)
          (pure $ P.enum enumName description columnDefinitions')
  where
    define name =
      P.Definition name (Just $ G.Description "column name") Nothing [] P.EnumValueInfo

-- | Table select columns enum of a certain type.
--
-- Parser for an enum type that matches, of a given table, certain columns which
-- satisfy a predicate.  Used as a parameter for aggregation predicate
-- arguments, among others. Maps to the table_select_column object.
--
-- Return Nothing if there's no column the current user has "select"
-- permissions for.
tableSelectColumnsPredEnum ::
  forall b r m n.
  (MonadBuildSchema b r m n) =>
  (ColumnType b -> Bool) ->
  GQLNameIdentifier ->
  TableInfo b ->
  SchemaT r m (Maybe (Parser 'Both n (Column b, AnnRedactionExpUnpreparedValue b)))
tableSelectColumnsPredEnum columnPredicate predName tableInfo = do
  customization <- retrieve $ _siCustomization @b
  let tCase = _rscNamingConvention customization
      mkTypename = runMkTypename $ _rscTypeNames customization
      predName' = applyFieldNameCaseIdentifier tCase predName
  tableGQLName <- getTableIdentifierName @b tableInfo
  columns <- filter (columnPredicate . ciType . fst) . mapMaybe (\(column, redactionExp) -> (,redactionExp) <$> (column ^? _SCIScalarColumn)) <$> tableSelectColumns tableInfo
  let enumName = mkTypename $ applyTypeNameCaseIdentifier tCase $ mkSelectColumnPredTypeName tableGQLName predName
      description =
        Just
          $ G.Description
          $ "select \""
          <> G.unName predName'
          <> "\" columns of table "
          <>> tableInfoName tableInfo
  pure
    $ P.enum enumName description
    <$> nonEmpty
      [ ( define $ ciName column,
          (ciColumn column, redactionExp)
        )
        | (column, redactionExp) <- columns
      ]
  where
    define name =
      P.Definition name (Just $ G.Description "column name") Nothing [] P.EnumValueInfo

-- | Table update columns enum
--
-- Parser for an enum type that matches the columns of the given
-- table. Used for conflict resolution in "insert" mutations, among
-- others. Maps to the table_update_column object.
tableUpdateColumnsEnum ::
  forall b r m n.
  (MonadBuildSchema b r m n) =>
  TableInfo b ->
  SchemaT r m (Maybe (Parser 'Both n (Column b)))
tableUpdateColumnsEnum tableInfo = do
  roleName <- retrieve scRole
  customization <- retrieve $ _siCustomization @b
  let tCase = _rscNamingConvention customization
      mkTypename = runMkTypename $ _rscTypeNames customization
  tableGQLName <- getTableIdentifierName tableInfo
  let enumName = mkTypename $ applyTypeNameCaseIdentifier tCase $ mkTableUpdateColumnTypeName tableGQLName
      tableName = tableInfoName tableInfo
      enumDesc = Just $ G.Description $ "update columns of table " <>> tableName
      enumValues = do
        column <- tableUpdateColumns roleName tableInfo
        pure (define $ ciName column, ciColumn column)
  pure $ P.enum enumName enumDesc <$> nonEmpty enumValues
  where
    define name = P.Definition name (Just $ G.Description "column name") Nothing [] P.EnumValueInfo

-- If there's no column for which the current user has "update"
-- permissions, this functions returns an enum that only contains a
-- placeholder, so as to still allow this type to exist in the schema.
updateColumnsPlaceholderParser ::
  forall b r m n.
  (MonadBuildSchema b r m n) =>
  TableInfo b ->
  SchemaT r m (Parser 'Both n (Maybe (Column b)))
updateColumnsPlaceholderParser tableInfo = do
  customization <- retrieve $ _siCustomization @b
  let tCase = _rscNamingConvention customization
      mkTypename = runMkTypename $ _rscTypeNames customization
  maybeEnum <- tableUpdateColumnsEnum tableInfo
  case maybeEnum of
    Just e -> pure $ Just <$> e
    Nothing -> do
      tableGQLName <- getTableIdentifierName tableInfo
      let enumName = mkTypename $ applyTypeNameCaseIdentifier tCase $ mkTableUpdateColumnTypeName tableGQLName
      pure
        $ P.enum enumName (Just $ G.Description $ "placeholder for update columns of table " <> tableInfoName tableInfo <<> " (current role has no relevant permissions)")
        $ pure
          ( P.Definition @_ @P.EnumValueInfo Name.__PLACEHOLDER (Just $ G.Description "placeholder (do not use)") Nothing [] P.EnumValueInfo,
            Nothing
          )

tableSelectPermissions :: RoleName -> TableInfo b -> Maybe (SelPermInfo b)
tableSelectPermissions role tableInfo = _permSel $ getRolePermInfo role tableInfo

tableSelectFields ::
  forall b r m.
  ( Backend b,
    MonadError QErr m,
    MonadReader r m,
    Has SchemaContext r,
    Has (SourceInfo b) r
  ) =>
  TableInfo b ->
  m [FieldInfo b]
tableSelectFields tableInfo = do
  roleName <- retrieve scRole
  let tableFields = _tciFieldInfoMap . _tiCoreInfo $ tableInfo
      permissions = tableSelectPermissions roleName tableInfo
  filterM (canBeSelected roleName permissions) $ HashMap.elems tableFields
  where
    canBeSelected _ Nothing _ = pure False
    canBeSelected _ (Just permissions) (FIColumn (SCIScalarColumn (columnInfo))) =
      pure $! HashMap.member (ciColumn columnInfo) (spiCols permissions)
    canBeSelected _ (Just permissions) (FIColumn (SCIObjectColumn NestedObjectInfo {..})) =
      pure $! HashMap.member _noiColumn (spiCols permissions)
    canBeSelected role permissions (FIColumn (SCIArrayColumn NestedArrayInfo {..})) =
      canBeSelected role permissions (FIColumn _naiColumnInfo)
    canBeSelected role _ (FIRelationship relationshipInfo) = do
      case riTarget relationshipInfo of
        RelTargetNativeQuery nativeQueryName -> do
          nativeQueryInfo <- askNativeQueryInfo nativeQueryName
          pure $! isJust $ getSelPermInfoForLogicalModel @b role (_nqiReturns nativeQueryInfo)
        RelTargetTable tableName -> do
          tableInfo' <- askTableInfo tableName
          pure $! isJust $ tableSelectPermissions @b role tableInfo'
    canBeSelected role (Just permissions) (FIComputedField computedFieldInfo) =
      case computedFieldReturnType @b (_cfiReturnType computedFieldInfo) of
        ReturnsScalar _ ->
          pure $! HashMap.member (_cfiName computedFieldInfo) $ spiComputedFields permissions
        ReturnsTable tableName -> do
          tableInfo' <- askTableInfo tableName
          pure $! isJust $ tableSelectPermissions @b role tableInfo'
        ReturnsOthers -> pure False
    canBeSelected _ _ (FIRemoteRelationship _) = pure True

tableColumns ::
  forall b. TableInfo b -> [ColumnInfo b]
tableColumns tableInfo =
  sortOn ciPosition . mapMaybe columnInfo . HashMap.elems . _tciFieldInfoMap . _tiCoreInfo $ tableInfo
  where
    columnInfo (FIColumn (SCIScalarColumn ci)) = Just ci
    columnInfo _ = Nothing

-- | Get the columns of a table that may be selected under the given select
-- permissions.
tableSelectColumns ::
  forall b r m.
  ( Backend b,
    MonadError QErr m,
    MonadReader r m,
    Has SchemaContext r,
    Has (SourceInfo b) r
  ) =>
  TableInfo b ->
  m [(StructuredColumnInfo b, AnnRedactionExpUnpreparedValue b)]
tableSelectColumns tableInfo = do
  roleName <- retrieve scRole
  case spiCols <$> tableSelectPermissions roleName tableInfo of
    Nothing -> pure []
    Just columnPermissions ->
      mapMaybe (getColumnsAndRedactionExps columnPermissions) <$> tableSelectFields tableInfo
  where
    getColumnsAndRedactionExps ::
      HashMap (Column b) (AnnRedactionExpPartialSQL b) ->
      FieldInfo b ->
      Maybe ((StructuredColumnInfo b, AnnRedactionExpUnpreparedValue b))
    getColumnsAndRedactionExps columnPermissions = \case
      FIColumn structuredColumnInfo -> do
        redactionExp <- HashMap.lookup (structuredColumnInfoColumn structuredColumnInfo) columnPermissions
        pure (structuredColumnInfo, partialSQLExpToUnpreparedValue <$> redactionExp)
      _ ->
        Nothing

-- | Get the computed fields of a table that may be selected under the given
-- select permissions.
tableSelectComputedFields ::
  forall b r m.
  ( Backend b,
    MonadError QErr m,
    MonadReader r m,
    Has SchemaContext r,
    Has (SourceInfo b) r
  ) =>
  TableInfo b ->
  m [ComputedFieldInfo b]
tableSelectComputedFields tableInfo =
  mapMaybe computedFieldInfo <$> tableSelectFields tableInfo
  where
    computedFieldInfo (FIComputedField cfi) = Just cfi
    computedFieldInfo _ = Nothing

-- | Get the columns of a table that my be updated under the given update
-- permissions.
tableUpdateColumns ::
  forall b.
  (Backend b) =>
  RoleName ->
  TableInfo b ->
  [ColumnInfo b]
tableUpdateColumns role tableInfo =
  let permissions = _permUpd $ getRolePermInfo role tableInfo
   in filter (isUpdatable permissions) $ tableColumns tableInfo
  where
    isUpdatable :: Maybe (UpdPermInfo b) -> ColumnInfo b -> Bool
    isUpdatable (Just permissions) columnInfo = columnIsUpdatable && columnIsPermitted && columnHasNoPreset
      where
        columnIsUpdatable = _cmIsUpdatable (ciMutability columnInfo)
        columnIsPermitted = Set.member (ciColumn columnInfo) (upiCols permissions)
        columnHasNoPreset = not (HashMap.member (ciColumn columnInfo) (upiSet permissions))
    isUpdatable Nothing _ = False
