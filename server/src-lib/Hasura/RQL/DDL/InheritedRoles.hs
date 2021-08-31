module Hasura.RQL.DDL.InheritedRoles
  ( runAddInheritedRole
  , runDropInheritedRole
  , dropInheritedRoleInMetadata
  , resolveInheritedRole
  )
where

import           Hasura.Prelude

import           Data.Text.Extended

import qualified Data.HashMap.Strict.InsOrd as OMap
import qualified Data.HashSet               as Set

import           Hasura.Base.Error
import           Hasura.EncJSON
import           Hasura.RQL.Types
import           Hasura.Server.Types        (ExperimentalFeature (..))
import           Hasura.Session


runAddInheritedRole
  :: ( MonadError QErr m
     , CacheRWM m
     , MetadataM m
     , HasServerConfigCtx m
     )
  => InheritedRole
  -> m EncJSON
runAddInheritedRole addInheritedRoleQ@(Role inheritedRoleName (ParentRoles parentRoles)) = do
  experimentalFeatures <- _sccExperimentalFeatures <$> askServerConfigCtx
  unless (EFInheritedRoles `elem` experimentalFeatures) $
    throw400 ConstraintViolation $
      "inherited role can only be added when inherited_roles enabled" <>
      " in the experimental features"
  when (inheritedRoleName `elem` parentRoles) $
    throw400 InvalidParams "an inherited role name cannot be in the role combination"
  buildSchemaCacheFor (MOInheritedRole inheritedRoleName)
    $ MetadataModifier
    $ metaInheritedRoles %~ OMap.insert inheritedRoleName addInheritedRoleQ
  pure successMsg

dropInheritedRoleInMetadata :: RoleName -> MetadataModifier
dropInheritedRoleInMetadata roleName =
  MetadataModifier $ metaInheritedRoles %~ OMap.delete roleName

runDropInheritedRole
  :: (MonadError QErr m, CacheRWM m, MetadataM m)
  => DropInheritedRole
  -> m EncJSON
runDropInheritedRole (DropInheritedRole roleName) = do
  inheritedRolesMetadata <- _metaInheritedRoles <$> getMetadata
  unless (roleName `OMap.member` inheritedRolesMetadata) $
    throw400 NotExists $ roleName <<> " inherited role doesn't exist"
  buildSchemaCacheFor (MOInheritedRole roleName) (dropInheritedRoleInMetadata roleName)
  pure successMsg

-- | `resolveInheritedRole` resolves an inherited role by checking if
-- all the parent roles of an inherited role exists and report
-- the dependencies of the inherited role which will be the list
-- of the parent roles
resolveInheritedRole
  :: MonadError QErr m
  => HashSet RoleName
  -> InheritedRole
  -> m (Role, [SchemaDependency])
resolveInheritedRole allRoles (Role roleName (ParentRoles parentRoles)) = do
  let missingParentRoles = Set.filter (`notElem` allRoles) parentRoles
  unless (Set.null missingParentRoles) $
    let errMessage roles =
          "the following parent role(s) are not found: "
          <> roles <> " which are required to construct the inherited role: " <>> roleName
    in throw400 NotExists $ errMessage $ commaSeparated $ Set.map roleNameToTxt missingParentRoles
  let schemaDependencies =
        map (\parentRole -> SchemaDependency (SORole parentRole) DRParentRole) $ toList parentRoles
  pure $ (Role roleName $ ParentRoles parentRoles, schemaDependencies)
