-- | Helper functions for generating the schema of database tables
module Hasura.GraphQL.Schema.Table
  ( tableSelectColumnsEnum
  , tableUpdateColumnsEnum
  , tablePermissions
  , tableSelectPermissions
  , tableUpdatePermissions
  , tableDeletePermissions
  , tableSelectFields
  , tableColumns
  , tableSelectColumns
  , tableUpdateColumns
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict                as Map
import qualified Data.HashSet                       as Set
import qualified Language.GraphQL.Draft.Syntax      as G

import           Data.Text.Extended

import qualified Hasura.GraphQL.Parser              as P

import           Hasura.GraphQL.Parser              (Kind (..), Parser)
import           Hasura.GraphQL.Parser.Class
import           Hasura.GraphQL.Schema.Backend
import           Hasura.RQL.DML.Internal            (getRolePermInfo)
import           Hasura.RQL.Types

-- | Table select columns enum
--
-- Parser for an enum type that matches the columns of the given
-- table. Used as a parameter for "distinct", among others. Maps to
-- the table_select_column object.
--
-- Return Nothing if there's no column the current user has "select"
-- permissions for.
tableSelectColumnsEnum
  :: forall m n r b
   . (BackendSchema b, MonadSchema n m, MonadRole r m, MonadTableInfo b r m)
  => TableName b
  -> SelPermInfo b
  -> m (Maybe (Parser 'Both n (Column b)))
tableSelectColumnsEnum table selectPermissions = do
  tableGQLName <- getTableGQLName @b table
  columns      <- tableSelectColumns table selectPermissions
  let enumName    = tableGQLName <> $$(G.litName "_select_column")
      description = Just $ G.Description $
        "select columns of table " <>> table
  pure $ P.enum enumName description <$> nonEmpty
    [ ( define $ pgiName column
      , pgiColumn column
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
-- Return Nothing if there's no column the current user has "update"
-- permissions for.
tableUpdateColumnsEnum
  :: forall m n r b
   . (BackendSchema b, MonadSchema n m, MonadRole r m, MonadTableInfo b r m)
  => TableName b
  -> UpdPermInfo b
  -> m (Maybe (Parser 'Both n (Column b)))
tableUpdateColumnsEnum table updatePermissions = do
  tableGQLName <- getTableGQLName @b table
  columns      <- tableUpdateColumns table updatePermissions
  let enumName    = tableGQLName <> $$(G.litName "_update_column")
      description = Just $ G.Description $
        "update columns of table " <>> table
  pure $ P.enum enumName description <$> nonEmpty
    [ ( define $ pgiName column
      , pgiColumn column
      )
    | column <- columns
    ]
  where
    define name =
      P.mkDefinition name (Just $ G.Description "column name") P.EnumValueInfo

tablePermissions
  :: forall m n r b. (Backend b, MonadSchema n m, MonadTableInfo b r m, MonadRole r m)
  => TableName b
  -> m (Maybe (RolePermInfo b))
tablePermissions table = do
  roleName  <- askRoleName
  tableInfo <- askTableInfo table
  pure $ getRolePermInfo roleName tableInfo

tableSelectPermissions
  :: forall m n r b. (Backend b, MonadSchema n m, MonadTableInfo b r m, MonadRole r m)
  => TableName b
  -> m (Maybe (SelPermInfo b))
tableSelectPermissions table = (_permSel =<<) <$> tablePermissions table

tableUpdatePermissions
  :: forall m n r b. (Backend b, MonadSchema n m, MonadTableInfo b r m, MonadRole r m)
  => TableName b
  -> m (Maybe (UpdPermInfo b))
tableUpdatePermissions table = (_permUpd =<<) <$> tablePermissions table

tableDeletePermissions
  :: forall m n r b. (Backend b, MonadSchema n m, MonadTableInfo b r m, MonadRole r m)
  => TableName b
  -> m (Maybe (DelPermInfo b))
tableDeletePermissions table = (_permDel =<<) <$> tablePermissions table

tableSelectFields
  :: forall m n r b. (Backend b, MonadSchema n m, MonadTableInfo b r m, MonadRole r m)
  => TableName b
  -> SelPermInfo b
  -> m [FieldInfo b]
tableSelectFields table permissions = do
  tableFields <- _tciFieldInfoMap . _tiCoreInfo <$> askTableInfo table
  filterM canBeSelected $ Map.elems tableFields
  where
    canBeSelected (FIColumn columnInfo) =
      pure $ Set.member (pgiColumn columnInfo) (spiCols permissions)
    canBeSelected (FIRelationship relationshipInfo) =
      isJust <$> tableSelectPermissions @_ @_ @_ @b (riRTable relationshipInfo)
    canBeSelected (FIComputedField computedFieldInfo) =
      case _cfiReturnType computedFieldInfo of
        CFRScalar _ ->
          pure $ Set.member (_cfiName computedFieldInfo) $ spiScalarComputedFields permissions
        CFRSetofTable tableName ->
          isJust <$> tableSelectPermissions @_ @_ @_ @b tableName
    -- TODO (from master): Derive permissions for remote relationships
    canBeSelected (FIRemoteRelationship _) = pure True

tableColumns
  :: forall m n r b. (Backend b, MonadSchema n m, MonadTableInfo b r m)
  => TableName b
  -> m [ColumnInfo b]
tableColumns table =
  mapMaybe columnInfo . Map.elems . _tciFieldInfoMap . _tiCoreInfo <$> askTableInfo table
  where
    columnInfo (FIColumn ci) = Just ci
    columnInfo _             = Nothing

tableSelectColumns
  :: forall m n r b. (Backend b, MonadSchema n m, MonadTableInfo b r m, MonadRole r m)
  => TableName b
  -> SelPermInfo b
  -> m [ColumnInfo b]
tableSelectColumns table permissions =
  mapMaybe columnInfo <$> tableSelectFields table permissions
  where
    columnInfo (FIColumn ci) = Just ci
    columnInfo _             = Nothing

tableUpdateColumns
  :: forall m n r b. (Backend b, MonadSchema n m, MonadTableInfo b r m)
  => TableName b
  -> UpdPermInfo b
  -> m [ColumnInfo b]
tableUpdateColumns table permissions = do
  tableFields <- _tciFieldInfoMap . _tiCoreInfo <$> askTableInfo table
  pure $ mapMaybe isUpdatable $ Map.elems tableFields
  where
    isUpdatable (FIColumn columnInfo) =
      if Set.member (pgiColumn columnInfo) (upiCols permissions)
         && not (Map.member (pgiColumn columnInfo) (upiSet permissions))
      then Just columnInfo
      else Nothing
    isUpdatable _ = Nothing
