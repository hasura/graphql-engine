module Hasura.GraphQL.Schema.Table
  ( tableSelectPermissions
  , tableSelectFields
  , tableSelectColumns
  ) where

import           Hasura.Prelude

import           Data.Maybe                  (isJust)

import qualified Data.HashMap.Strict         as Map
import qualified Data.HashSet                as Set

import           Hasura.GraphQL.Parser.Class
import           Hasura.RQL.Types
import           Hasura.SQL.Types

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
