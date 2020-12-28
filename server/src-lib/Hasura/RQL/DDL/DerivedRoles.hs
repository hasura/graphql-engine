module Hasura.RQL.DDL.DerivedRoles
  ( runAddDerivedRole
  , runDropDerivedRole
  , dropDerivedRoleInMetadata
  )
where

import           Hasura.Prelude

import           Control.Lens.Extended
import           Data.Text.Extended

import qualified Data.HashMap.Strict          as Map
import qualified Data.HashSet                 as Set
import qualified Data.HashMap.Strict.InsOrd   as OMap

import           Hasura.EncJSON
import           Hasura.RQL.Types
import           Hasura.Session


runAddDerivedRole
  :: (MonadTx m, CacheRWM m, MetadataM m) => AddDerivedRole -> m EncJSON
runAddDerivedRole addDerivedRoleQ@(AddDerivedRole derivedRoleName roleSet) = do
  when (derivedRoleName `elem` roleSet) $
    throw400 InvalidParams "the derived role name cannot be in the role combination"
  schemaCache <- askSchemaCache
  let derivedRolesMap = scDerivedRoles schemaCache
      derivedRoles = Map.keys derivedRolesMap
  let tableCache = scTables schemaCache
      actionsCache = scActions schemaCache
      -- TODO : include the remote schema permission's roles (when merged)
      -- here as well
      nonDerivedRoles :: Set.HashSet RoleName
      nonDerivedRoles =
        (tableCache ^.. folded.tiRolePermInfoMap.to Map.keys.folded)
        <> (actionsCache ^.. folded.aiPermissions.to Map.keys.folded)
      allRoles :: Set.HashSet RoleName
      allRoles = nonDerivedRoles <> Set.fromList derivedRoles
  when (derivedRoleName `elem` allRoles) $
    throw400 AlreadyExists $ derivedRoleName <<> " role already exists"
  for_ roleSet $ \role ->
    unless (role `Set.member` nonDerivedRoles) $
      throw400 NotFound $ role <<> " not found. A derived role can only be created out of existing roles"
  -- TODO: check if the role set already exists in the derived roles
  let derivedRolesInRoleSet = roleSet `Set.intersection` Set.fromList derivedRoles
  unless (derivedRolesInRoleSet == Set.empty) $
    throw400 ConstraintError $
    "the role combination for creating a derived role, cannot include an existing derived role"
  buildSchemaCacheFor (MODerivedRole derivedRoleName)
    $ MetadataModifier
    $ metaExperimentalFeatures %~ Just . \case
       Nothing -> ExperimentalFeatures $ OMap.singleton derivedRoleName addDerivedRoleQ
       Just ef -> ef & (efDerivedRoles %~ OMap.insert derivedRoleName addDerivedRoleQ)
  pure successMsg

dropDerivedRoleInMetadata :: RoleName -> MetadataModifier
dropDerivedRoleInMetadata roleName =
  MetadataModifier $ metaExperimentalFeatures._Just.efDerivedRoles %~ OMap.delete roleName

runDropDerivedRole
  :: (MonadTx m, CacheRWM m, MetadataM m)
  => DropDerivedRole
  -> m EncJSON
runDropDerivedRole (DropDerivedRole roleName) = do
  derivedRoles <- scDerivedRoles <$> askSchemaCache
  unless (roleName `Map.member` derivedRoles) $
    throw400 NotExists $ roleName <<> " derived role doesn't exist"
  buildSchemaCacheFor (MODerivedRole roleName) (dropDerivedRoleInMetadata roleName)
  pure successMsg
