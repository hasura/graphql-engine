module Hasura.RQL.PermissionSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as Set
import Hasura.Prelude
import Hasura.RQL.DDL.Schema.Cache
import Hasura.RQL.DDL.Schema.Cache.Permission
import Hasura.RQL.Types.Action
import Hasura.RQL.Types.Roles
import Test.Hspec

spec :: Spec
spec = do
  booleanPermissionSpec

mkRoleNameE :: Text -> RoleName
mkRoleNameE = fromMaybe (error "rolename error") . mkRoleName

orderRolesE :: [Role] -> OrderedRoles
orderRolesE = either (error "orderRoles error") id . runExcept . orderRoles

-- | spec to test permissions inheritance for boolean permissions (actions and custom function permissions)
booleanPermissionSpec :: Spec
booleanPermissionSpec = do
  let role1Name = mkRoleNameE "role1"
      role2Name = mkRoleNameE "role2"
      role3Name = mkRoleNameE "role3"
      inheritedRole1Name = mkRoleNameE "inheritedRole1"
      inheritedRole2Name = mkRoleNameE "inheritedRole2"
      inheritedRole3Name = mkRoleNameE "inheritedRole3"
      role1 = Role role1Name $ ParentRoles mempty
      role2 = Role role2Name $ ParentRoles mempty
      role3 = Role role3Name $ ParentRoles mempty
      inheritedRole1 = Role inheritedRole1Name $ ParentRoles $ Set.fromList [role1Name, role2Name]
      inheritedRole2 = Role inheritedRole2Name $ ParentRoles $ Set.fromList [role3Name, inheritedRole1Name]
      inheritedRole3 = Role inheritedRole3Name $ ParentRoles $ Set.fromList [role1Name, role2Name]
      orderedRoles = orderRolesE [role1, role2, role3, inheritedRole1, inheritedRole2, inheritedRole3]
      metadataPermissions =
        HashMap.fromList $ [(role3Name, ActionPermissionInfo role3Name), (inheritedRole1Name, ActionPermissionInfo inheritedRole1Name)]
      processedPermissions = mkBooleanPermissionMap ActionPermissionInfo metadataPermissions orderedRoles
  describe "Action Permissions" $ do
    it "overrides the inherited permission for a role if permission already exists in the metadata"
      $ HashMap.lookup inheritedRole1Name processedPermissions
      `shouldBe` (Just (ActionPermissionInfo inheritedRole1Name))
    it "when a role doesn't have a metadata permission and at least one of its parents has, then the inherited role should inherit the permission"
      $ HashMap.lookup inheritedRole2Name processedPermissions
      `shouldBe` (Just (ActionPermissionInfo inheritedRole2Name))
    it "when a role doesn't have a metadata permission and none of the parents have permissions, then the inherited role should not inherit the permission"
      $ HashMap.lookup inheritedRole3Name processedPermissions
      `shouldBe` Nothing
