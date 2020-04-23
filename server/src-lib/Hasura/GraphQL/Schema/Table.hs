module Hasura.GraphQL.Schema.Table
  ( tableSelectPermissions
  , tableSelectFields
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
  -> m [FieldInfo]
tableSelectFields table = do
  -- TODO: memoize this?
  tableFields <- _tciFieldInfoMap . _tiCoreInfo <$> askTableInfo table
  tableSelectPermissions table >>= \case
    Nothing    -> pure []
    Just perms -> filterM (canBeSelected perms) $ Map.elems tableFields
  where
    canBeSelected perms (FIColumn columnInfo) =
      pure $ Set.member (pgiColumn columnInfo) (spiCols perms)
    canBeSelected _     (FIRelationship relationshipInfo) =
      isJust <$> tableSelectPermissions (riRTable relationshipInfo)
    canBeSelected perms (FIComputedField computedFieldInfo) =
      case _cfiReturnType computedFieldInfo of
        CFRScalar _ ->
          pure $ Set.member (_cfiName computedFieldInfo) $ spiScalarComputedFields perms
        CFRSetofTable tableName ->
          isJust <$> tableSelectPermissions tableName
