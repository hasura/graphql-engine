module Hasura.GraphQL.Schema.Table
  ( tableColumnsEnum
  , tableSelectPermissions
  , tableSelectFields
  , tableSelectColumns
  ) where

import           Hasura.Prelude

import           Data.Maybe                    (isJust)

import qualified Data.HashMap.Strict           as Map
import qualified Data.HashSet                  as Set
import qualified Language.GraphQL.Draft.Syntax as G

import qualified Hasura.GraphQL.Parser         as P

import           Hasura.GraphQL.Parser         (Kind (..), Parser)
import           Hasura.GraphQL.Parser.Class
import           Hasura.GraphQL.Parser.Column  (qualifiedObjectToName)
import           Hasura.RQL.Types
import           Hasura.SQL.Types


-- | Table columns enum
--
-- Parser for an enum type that matches the columns of the given
-- table. Used as a parameter for "distinct", among others. Maps to
-- the table_select_columns object.
--
-- Return Nothing if there's no column the current user has access to.
tableColumnsEnum
  :: (MonadSchema n m, MonadError QErr m)
  => QualifiedTable
  -> SelPermInfo
  -> m (Maybe (Parser 'Both n PGCol))
tableColumnsEnum table selectPermissions = do
  tableName <- qualifiedObjectToName table
  columns   <- tableSelectColumns table selectPermissions
  let enumName    = tableName <> $$(G.litName "_select_columns")
      description = Just $ G.Description $
        "select columns of table \"" <> G.unName tableName <> "\""
  pure $ P.enum enumName description <$> nonEmpty
    [ ( define $ pgiName column
      , pgiColumn column
      )
    | column <- columns
    ]
  where
    define name =
      P.mkDefinition name (Just $ G.Description "column name") P.EnumValueInfo


tableSelectPermissions
  :: forall m n. (MonadSchema n m)
  => QualifiedTable
  -> m (Maybe SelPermInfo)
tableSelectPermissions table = do
  roleName  <- askRoleName
  tableInfo <- _tiRolePermInfoMap <$> askTableInfo table
  pure $ _permSel =<< Map.lookup roleName tableInfo

tableSelectFields
  :: forall m n. (MonadSchema n m)
  => QualifiedTable
  -> SelPermInfo
  -> m [FieldInfo]
tableSelectFields table permissions = do
  -- TODO: memoize this?
  tableFields <- _tciFieldInfoMap . _tiCoreInfo <$> askTableInfo table
  filterM canBeSelected $ Map.elems tableFields
  where
    canBeSelected (FIColumn columnInfo) =
      pure $ Set.member (pgiColumn columnInfo) (spiCols permissions)
    canBeSelected (FIRelationship relationshipInfo) =
      isJust <$> tableSelectPermissions (riRTable relationshipInfo)
    canBeSelected (FIComputedField computedFieldInfo) =
      case _cfiReturnType computedFieldInfo of
        CFRScalar _ ->
          pure $ Set.member (_cfiName computedFieldInfo) $ spiScalarComputedFields permissions
        CFRSetofTable tableName ->
          isJust <$> tableSelectPermissions tableName

tableSelectColumns
  :: forall m n. (MonadSchema n m)
  => QualifiedTable
  -> SelPermInfo
  -> m [PGColumnInfo]
tableSelectColumns table permissions =
  mapMaybe columnInfo <$> tableSelectFields table permissions
  where
    columnInfo (FIColumn ci) = Just ci
    columnInfo _             = Nothing
