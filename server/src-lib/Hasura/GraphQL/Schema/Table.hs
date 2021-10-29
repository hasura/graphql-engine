-- | Helper functions for generating the schema of database tables
module Hasura.GraphQL.Schema.Table
  ( getTableGQLName,
    tableSelectColumnsEnum,
    tableUpdateColumnsEnum,
    tablePermissions,
    tableSelectPermissions,
    tableSelectFields,
    tableColumns,
    tableSelectColumns,
    tableUpdateColumns,
  )
where

import Data.Has
import Data.HashMap.Strict qualified as Map
import Data.HashSet qualified as Set
import Data.Text.Extended
import Hasura.Base.Error (QErr)
import Hasura.GraphQL.Parser (Kind (..), Parser)
import Hasura.GraphQL.Parser qualified as P
import Hasura.GraphQL.Parser.Class
import Hasura.GraphQL.Schema.Backend
import Hasura.Prelude
import Hasura.RQL.DML.Internal (getRolePermInfo)
import Hasura.RQL.Types
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

-- | Table select columns enum
--
-- Parser for an enum type that matches the columns of the given
-- table. Used as a parameter for "distinct", among others. Maps to
-- the table_select_column object.
--
-- Return Nothing if there's no column the current user has "select"
-- permissions for.
tableSelectColumnsEnum ::
  forall m n r b.
  (BackendSchema b, MonadSchema n m, MonadRole r m, MonadTableInfo r m, Has P.MkTypename r) =>
  SourceName ->
  TableInfo b ->
  SelPermInfo b ->
  m (Maybe (Parser 'Both n (Column b)))
tableSelectColumnsEnum sourceName tableInfo selectPermissions = do
  tableGQLName <- getTableGQLName @b tableInfo
  columns <- tableSelectColumns sourceName tableInfo selectPermissions
  enumName <- P.mkTypename $ tableGQLName <> $$(G.litName "_select_column")
  let description =
        Just $
          G.Description $
            "select columns of table " <>> tableInfoName tableInfo
  pure $
    P.enum enumName description
      <$> nonEmpty
        [ ( define $ pgiName column,
            pgiColumn column
          )
          | column <- columns
        ]
  where
    define name =
      P.mkDefinition name (Just $ G.Description "column name") P.EnumValueInfo

-- | Table update columns enum
--
-- Parser for an enum type that matches the columns of the given
-- table. Used for conflict resolution in "insert" mutations, among
-- others. Maps to the table_update_column object.
--
-- If there's no column for which the current user has "update"
-- permissions, this functions returns an enum that only contains a
-- placeholder, so as to still allow this type to exist in the schema.
tableUpdateColumnsEnum ::
  forall m n r b.
  (BackendSchema b, MonadSchema n m, MonadError QErr m, MonadReader r m, Has P.MkTypename r) =>
  TableInfo b ->
  UpdPermInfo b ->
  m (Parser 'Both n (Maybe (Column b)))
tableUpdateColumnsEnum tableInfo updatePermissions = do
  tableGQLName <- getTableGQLName tableInfo
  columns <- tableUpdateColumns tableInfo updatePermissions
  enumName <- P.mkTypename $ tableGQLName <> $$(G.litName "_update_column")
  let tableName = tableInfoName tableInfo
      enumDesc = Just $ G.Description $ "update columns of table " <>> tableName
      altDesc = Just $ G.Description $ "placeholder for update columns of table " <> tableName <<> " (current role has no relevant permissions)"
      enumValues = do
        column <- columns
        pure (define $ pgiName column, Just $ pgiColumn column)
  pure $ case nonEmpty enumValues of
    Just values -> P.enum enumName enumDesc values
    Nothing -> P.enum enumName altDesc $ pure (placeholder, Nothing)
  where
    define name = P.mkDefinition name (Just $ G.Description "column name") P.EnumValueInfo
    placeholder = P.mkDefinition @P.EnumValueInfo $$(G.litName "_PLACEHOLDER") (Just $ G.Description "placeholder (do not use)") P.EnumValueInfo

tablePermissions ::
  forall m n r b.
  (Backend b, MonadSchema n m, MonadRole r m) =>
  TableInfo b ->
  m (Maybe (RolePermInfo b))
tablePermissions tableInfo = do
  roleName <- askRoleName
  pure $ getRolePermInfo roleName tableInfo

tableSelectPermissions ::
  forall b r m n.
  (Backend b, MonadSchema n m, MonadRole r m) =>
  TableInfo b ->
  m (Maybe (SelPermInfo b))
tableSelectPermissions tableInfo = (_permSel =<<) <$> tablePermissions tableInfo

tableSelectFields ::
  forall m n r b.
  (Backend b, MonadSchema n m, MonadTableInfo r m, MonadRole r m) =>
  SourceName ->
  TableInfo b ->
  SelPermInfo b ->
  m [FieldInfo b]
tableSelectFields sourceName tableInfo permissions = do
  let tableFields = _tciFieldInfoMap . _tiCoreInfo $ tableInfo
  filterM canBeSelected $ Map.elems tableFields
  where
    canBeSelected (FIColumn columnInfo) =
      pure $ Map.member (pgiColumn columnInfo) (spiCols permissions)
    canBeSelected (FIRelationship relationshipInfo) = do
      tableInfo' <- askTableInfo sourceName $ riRTable relationshipInfo
      isJust <$> tableSelectPermissions @b tableInfo'
    canBeSelected (FIComputedField computedFieldInfo) =
      case _cfiReturnType computedFieldInfo of
        CFRScalar _ ->
          pure $ Map.member (_cfiName computedFieldInfo) $ spiScalarComputedFields permissions
        CFRSetofTable tableName -> do
          tableInfo' <- askTableInfo sourceName tableName
          isJust <$> tableSelectPermissions @b tableInfo'
    canBeSelected (FIRemoteRelationship _) = pure True

tableColumns ::
  forall b. TableInfo b -> [ColumnInfo b]
tableColumns tableInfo =
  mapMaybe columnInfo . Map.elems . _tciFieldInfoMap . _tiCoreInfo $ tableInfo
  where
    columnInfo (FIColumn ci) = Just ci
    columnInfo _ = Nothing

tableSelectColumns ::
  forall m n r b.
  (Backend b, MonadSchema n m, MonadTableInfo r m, MonadRole r m) =>
  SourceName ->
  TableInfo b ->
  SelPermInfo b ->
  m [ColumnInfo b]
tableSelectColumns sourceName tableInfo permissions =
  mapMaybe columnInfo <$> tableSelectFields sourceName tableInfo permissions
  where
    columnInfo (FIColumn ci) = Just ci
    columnInfo _ = Nothing

tableUpdateColumns ::
  forall m n b.
  (Backend b, MonadSchema n m) =>
  TableInfo b ->
  UpdPermInfo b ->
  m [ColumnInfo b]
tableUpdateColumns tableInfo permissions = do
  let tableFields = _tciFieldInfoMap . _tiCoreInfo $ tableInfo
  pure $ mapMaybe isUpdatable $ Map.elems tableFields
  where
    isUpdatable (FIColumn columnInfo) =
      if Set.member (pgiColumn columnInfo) (upiCols permissions)
        && not (Map.member (pgiColumn columnInfo) (upiSet permissions))
        then Just columnInfo
        else Nothing
    isUpdatable _ = Nothing
