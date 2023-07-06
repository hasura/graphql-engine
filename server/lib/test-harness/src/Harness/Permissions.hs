-- | An entry point to import both types and metadata command builders for
-- permissions.
module Harness.Permissions
  ( Types.Permission (..),
    Types.SelectPermissionDetails (..),
    Types.UpdatePermissionDetails (..),
    Types.InsertPermissionDetails (..),
    Types.DeletePermissionDetails (..),
    Types.selectPermission,
    Types.updatePermission,
    Types.insertPermission,
    Types.deletePermission,
    Metadata.createPermissionMetadata,
    Metadata.dropPermissionMetadata,
  )
where

import Harness.Permissions.Metadata qualified as Metadata
import Harness.Permissions.Types qualified as Types
