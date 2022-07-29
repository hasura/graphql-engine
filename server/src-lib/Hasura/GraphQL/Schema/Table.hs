-- | Helper functions for generating the schema of database tables
module Hasura.GraphQL.Schema.Table
  ( getTableGQLName,
    tableSelectColumnsEnum,
    tableUpdateColumnsEnum,
    updateColumnsPlaceholderParser,
    tableSelectPermissions,
    tableSelectFields,
    tableColumns,
    tableSelectColumns,
    tableUpdateColumns,
    getTableIdentifierName,
  )
where

import Data.Has
import Data.HashMap.Strict qualified as Map
import Data.HashSet qualified as Set
import Data.Text.Casing
import Data.Text.Extended
import Hasura.Base.Error (QErr)
import Hasura.GraphQL.Schema.Backend
import Hasura.GraphQL.Schema.Common
import Hasura.GraphQL.Schema.Parser (Kind (..), Parser)
import Hasura.GraphQL.Schema.Parser qualified as P
import Hasura.GraphQL.Schema.Typename (mkTypename)
import Hasura.Name qualified as Name
import Hasura.Prelude
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.ComputedField
import Hasura.RQL.Types.Relationships.Local
import Hasura.RQL.Types.SchemaCache hiding (askTableInfo)
import Hasura.RQL.Types.Source
import Hasura.RQL.Types.Table
import Hasura.Session (RoleName)
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
  m (GQLNameIdentifier)
getTableIdentifierName tableInfo =
  let coreInfo = _tiCoreInfo tableInfo
      tableName = _tciName coreInfo
      tableCustomName = _tcCustomName $ _tciCustomConfig coreInfo
   in maybe
        (liftEither $ getTableIdentifier @b tableName)
        (pure . (`Identifier` []))
        tableCustomName

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
  MonadBuildSchema b r m n =>
  SourceInfo b ->
  TableInfo b ->
  m (Maybe (Parser 'Both n (Column b)))
tableSelectColumnsEnum sourceInfo tableInfo = do
  tableGQLName <- getTableGQLName @b tableInfo
  columns <- tableSelectColumns sourceInfo tableInfo
  enumName <- mkTypename $ tableGQLName <> Name.__select_column
  let description =
        Just $
          G.Description $
            "select columns of table " <>> tableInfoName tableInfo
  pure $
    P.enum enumName description
      <$> nonEmpty
        [ ( define $ ciName column,
            ciColumn column
          )
          | column <- columns
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
  MonadBuildSchema b r m n =>
  TableInfo b ->
  m (Maybe (Parser 'Both n (Column b)))
tableUpdateColumnsEnum tableInfo = do
  roleName <- retrieve scRole
  tableGQLName <- getTableGQLName tableInfo
  enumName <- mkTypename $ tableGQLName <> Name.__update_column
  let tableName = tableInfoName tableInfo
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
  MonadBuildSchema backend r m n =>
  TableInfo backend ->
  m (Parser 'Both n (Maybe (Column backend)))
updateColumnsPlaceholderParser tableInfo = do
  maybeEnum <- tableUpdateColumnsEnum tableInfo
  case maybeEnum of
    Just e -> pure $ Just <$> e
    Nothing -> do
      tableGQLName <- getTableGQLName tableInfo
      enumName <- mkTypename $ tableGQLName <> Name.__update_column
      pure $
        P.enum enumName (Just $ G.Description $ "placeholder for update columns of table " <> tableInfoName tableInfo <<> " (current role has no relevant permissions)") $
          pure
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
    Has SchemaContext r
  ) =>
  SourceInfo b ->
  TableInfo b ->
  m [FieldInfo b]
tableSelectFields sourceInfo tableInfo = do
  roleName <- retrieve scRole
  let tableFields = _tciFieldInfoMap . _tiCoreInfo $ tableInfo
      permissions = tableSelectPermissions roleName tableInfo
  filterM (canBeSelected roleName permissions) $ Map.elems tableFields
  where
    canBeSelected _ Nothing _ = pure False
    canBeSelected _ (Just permissions) (FIColumn columnInfo) =
      pure $ Map.member (ciColumn columnInfo) (spiCols permissions)
    canBeSelected role _ (FIRelationship relationshipInfo) = do
      tableInfo' <- askTableInfo sourceInfo $ riRTable relationshipInfo
      pure $ isJust $ tableSelectPermissions @b role tableInfo'
    canBeSelected role (Just permissions) (FIComputedField computedFieldInfo) =
      case computedFieldReturnType @b (_cfiReturnType computedFieldInfo) of
        ReturnsScalar _ ->
          pure $ Map.member (_cfiName computedFieldInfo) $ spiComputedFields permissions
        ReturnsTable tableName -> do
          tableInfo' <- askTableInfo sourceInfo tableName
          pure $ isJust $ tableSelectPermissions @b role tableInfo'
        ReturnsOthers -> pure False
    canBeSelected _ _ (FIRemoteRelationship _) = pure True

tableColumns ::
  forall b. TableInfo b -> [ColumnInfo b]
tableColumns tableInfo =
  mapMaybe columnInfo . Map.elems . _tciFieldInfoMap . _tiCoreInfo $ tableInfo
  where
    columnInfo (FIColumn ci) = Just ci
    columnInfo _ = Nothing

-- | Get the columns of a table that my be selected under the given select
-- permissions.
tableSelectColumns ::
  forall b r m.
  ( Backend b,
    MonadError QErr m,
    MonadReader r m,
    Has SchemaContext r
  ) =>
  SourceInfo b ->
  TableInfo b ->
  m [ColumnInfo b]
tableSelectColumns sourceInfo tableInfo =
  mapMaybe columnInfo <$> tableSelectFields sourceInfo tableInfo
  where
    columnInfo (FIColumn ci) = Just ci
    columnInfo _ = Nothing

-- | Get the columns of a table that my be updated under the given update
-- permissions.
tableUpdateColumns ::
  forall b.
  Backend b =>
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
        columnHasNoPreset = not (Map.member (ciColumn columnInfo) (upiSet permissions))
    isUpdatable Nothing _ = False
