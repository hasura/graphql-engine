module Harness.Permissions.Types
  ( Permission (..),
    InsertPermissionDetails (..),
    insertPermission,
    SelectPermissionDetails (..),
    selectPermission,
    LogicalModelSelectPermissionDetails (..),
    logicalModelSelectPermission,
    UpdatePermissionDetails (..),
    updatePermission,
    DeletePermissionDetails (..),
    deletePermission,
    InheritedRoleDetails (..),
  )
where

import Data.Aeson (Value (Null), object)
import Hasura.Prelude

-- | Data type used to model permissions to be setup in tests.
-- Each case of this type mirrors the fields in the correspond permission
-- tracking metadata API payload.
data Permission
  = SelectPermission SelectPermissionDetails
  | LogicalModelSelectPermission LogicalModelSelectPermissionDetails
  | UpdatePermission UpdatePermissionDetails
  | InsertPermission InsertPermissionDetails
  | DeletePermission DeletePermissionDetails
  | InheritedRole InheritedRoleDetails
  deriving (Eq, Show)

data SelectPermissionDetails = SelectPermissionDetails
  { selectPermissionSource :: Maybe Text,
    selectPermissionTable :: Text,
    selectPermissionRole :: Text,
    selectPermissionColumns :: [Text],
    selectPermissionComputedFields :: [Text],
    selectPermissionRows :: Value,
    selectPermissionAllowAggregations :: Bool,
    selectPermissionLimit :: Value
  }
  deriving (Eq, Show)

data LogicalModelSelectPermissionDetails = LogicalModelSelectPermissionDetails
  { lmSelectPermissionSource :: Maybe Text,
    lmSelectPermissionName :: Text,
    lmSelectPermissionRole :: Text,
    lmSelectPermissionColumns :: [Text],
    lmSelectPermissionFilter :: Value
  }
  deriving (Eq, Show)

data UpdatePermissionDetails = UpdatePermissionDetails
  { updatePermissionSource :: Maybe Text,
    updatePermissionTable :: Text,
    updatePermissionRole :: Text,
    updatePermissionColumns :: [Text],
    updatePermissionRows :: Value,
    updatePermissionValidationWebhook :: Maybe Text
  }
  deriving (Eq, Show)

data InsertPermissionDetails = InsertPermissionDetails
  { insertPermissionSource :: Maybe Text,
    insertPermissionTable :: Text,
    insertPermissionRole :: Text,
    insertPermissionColumns :: [Text],
    insertPermissionRows :: Value,
    insertPermissionValidationWebhook :: Maybe Text
  }
  deriving (Eq, Show)

data DeletePermissionDetails = DeletePermissionDetails
  { deletePermissionSource :: Maybe Text,
    deletePermissionTable :: Text,
    deletePermissionRole :: Text,
    deletePermissionRows :: Value,
    deletePermissionValidationWebhook :: Maybe Text
  }
  deriving (Eq, Show)

data InheritedRoleDetails = InheritedRoleDetails
  { inheritedRoleName :: Text,
    inheritedRoleRoleSet :: [Text]
  }
  deriving (Eq, Show)

selectPermission :: SelectPermissionDetails
selectPermission =
  SelectPermissionDetails
    { selectPermissionSource = Nothing,
      selectPermissionTable = mempty,
      selectPermissionRole = "test-role",
      selectPermissionColumns = mempty,
      selectPermissionComputedFields = mempty,
      selectPermissionRows = object [],
      selectPermissionAllowAggregations = False,
      selectPermissionLimit = Null
    }

logicalModelSelectPermission :: LogicalModelSelectPermissionDetails
logicalModelSelectPermission =
  LogicalModelSelectPermissionDetails
    { lmSelectPermissionSource = Nothing,
      lmSelectPermissionName = "",
      lmSelectPermissionRole = "",
      lmSelectPermissionColumns = [],
      lmSelectPermissionFilter = object []
    }

updatePermission :: UpdatePermissionDetails
updatePermission =
  UpdatePermissionDetails
    { updatePermissionSource = Nothing,
      updatePermissionTable = mempty,
      updatePermissionRole = "test-role",
      updatePermissionColumns = mempty,
      updatePermissionRows = object [],
      updatePermissionValidationWebhook = Nothing
    }

insertPermission :: InsertPermissionDetails
insertPermission =
  InsertPermissionDetails
    { insertPermissionSource = Nothing,
      insertPermissionTable = mempty,
      insertPermissionRole = "test-role",
      insertPermissionColumns = mempty,
      insertPermissionRows = object [],
      insertPermissionValidationWebhook = Nothing
    }

deletePermission :: DeletePermissionDetails
deletePermission =
  DeletePermissionDetails
    { deletePermissionSource = Nothing,
      deletePermissionTable = mempty,
      deletePermissionRole = "test-role",
      deletePermissionRows = object [],
      deletePermissionValidationWebhook = Nothing
    }
