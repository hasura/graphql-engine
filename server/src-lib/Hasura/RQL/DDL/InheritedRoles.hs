module Hasura.RQL.DDL.InheritedRoles
  ( runAddInheritedRole
  , runDropInheritedRole
  , dropInheritedRoleInMetadata
  )
where

import           Hasura.Prelude

import           Data.Text.Extended

import qualified Data.HashMap.Strict.InsOrd as OMap

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
  => AddInheritedRole
  -> m EncJSON
runAddInheritedRole addInheritedRoleQ@(AddInheritedRole inheritedRoleName roleSet) = do
  experimentalFeatures <- _sccExperimentalFeatures <$> askServerConfigCtx
  unless (EFInheritedRoles `elem` experimentalFeatures) $
    throw400 ConstraintViolation $
      "inherited role can only be added when inherited_roles enabled" <>
      " in the experimental features"
  when (inheritedRoleName `elem` roleSet) $
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
