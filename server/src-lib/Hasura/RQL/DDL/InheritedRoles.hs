module Hasura.RQL.DDL.InheritedRoles
  ( runAddInheritedRole,
    runDropInheritedRole,
    dropInheritedRoleInMetadata,
    resolveInheritedRole,
  )
where

import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.HashSet qualified as Set
import Data.Sequence qualified as Seq
import Data.Text.Extended
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.Prelude
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Metadata
import Hasura.RQL.Types.Metadata.Object
import Hasura.RQL.Types.Roles
import Hasura.RQL.Types.SchemaCache
import Hasura.RQL.Types.SchemaCache.Build

runAddInheritedRole ::
  ( MonadError QErr m,
    CacheRWM m,
    MetadataM m
  ) =>
  InheritedRole ->
  m EncJSON
runAddInheritedRole addInheritedRoleQ@(Role inheritedRoleName (ParentRoles parentRoles)) = do
  when (inheritedRoleName `elem` parentRoles)
    $ throw400 InvalidParams "an inherited role name cannot be in the role combination"
  buildSchemaCacheFor (MOInheritedRole inheritedRoleName)
    $ MetadataModifier
    $ metaInheritedRoles
    %~ InsOrdHashMap.insert inheritedRoleName addInheritedRoleQ
  pure successMsg

dropInheritedRoleInMetadata :: RoleName -> MetadataModifier
dropInheritedRoleInMetadata roleName =
  MetadataModifier $ metaInheritedRoles %~ InsOrdHashMap.delete roleName

runDropInheritedRole ::
  (MonadError QErr m, CacheRWM m, MetadataM m) =>
  DropInheritedRole ->
  m EncJSON
runDropInheritedRole (DropInheritedRole roleName) = do
  inheritedRolesMetadata <- _metaInheritedRoles <$> getMetadata
  unless (roleName `InsOrdHashMap.member` inheritedRolesMetadata)
    $ throw400 NotExists
    $ roleName
    <<> " inherited role doesn't exist"
  buildSchemaCacheFor (MOInheritedRole roleName) (dropInheritedRoleInMetadata roleName)
  pure successMsg

-- | `resolveInheritedRole` resolves an inherited role by checking if
-- all the parent roles of an inherited role exists and report
-- the dependencies of the inherited role which will be the list
-- of the parent roles
resolveInheritedRole ::
  (MonadError QErr m) =>
  HashSet RoleName ->
  InheritedRole ->
  m (Role, Seq SchemaDependency)
resolveInheritedRole allRoles (Role roleName (ParentRoles parentRoles)) = do
  let missingParentRoles = Set.filter (`notElem` allRoles) parentRoles
  unless (Set.null missingParentRoles)
    $ let errMessage roles =
            "the following parent role(s) are not found: "
              <> roles
              <> " which are required to construct the inherited role: "
              <>> roleName
       in throw400 NotExists $ errMessage $ commaSeparated $ Set.map roleNameToTxt missingParentRoles
  let schemaDependencies =
        fmap (\parentRole -> SchemaDependency (SORole parentRole) DRParentRole) (Seq.fromList (toList parentRoles))
  pure $ (Role roleName $ ParentRoles parentRoles, schemaDependencies)
