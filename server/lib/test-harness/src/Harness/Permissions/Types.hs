module Harness.Permissions.Types
  ( Permission (..),
    InsertPermissionDetails (..),
    insertPermission,
    SelectPermissionDetails (..),
    selectPermission,
    UpdatePermissionDetails (..),
    updatePermission,
  )
where

import Data.Aeson (Value (Null), object)
import Hasura.Prelude

-- | Data type used to model permissions to be setup in tests.
-- Each case of this type mirrors the fields in the correspond permission
-- tracking metadata API payload.
data Permission
  = SelectPermission SelectPermissionDetails
  | UpdatePermission UpdatePermissionDetails
  | InsertPermission InsertPermissionDetails
  deriving (Eq, Show)

data SelectPermissionDetails = SelectPermissionDetails
  { selectPermissionSource :: Maybe Text,
    selectPermissionTable :: Text,
    selectPermissionRole :: Text,
    selectPermissionColumns :: [Text],
    selectPermissionRows :: Value,
    selectPermissionAllowAggregations :: Bool,
    selectPermissionLimit :: Value
  }
  deriving (Eq, Show)

data UpdatePermissionDetails = UpdatePermissionDetails
  { updatePermissionSource :: Maybe Text,
    updatePermissionTable :: Text,
    updatePermissionRole :: Text,
    updatePermissionColumns :: [Text],
    updatePermissionRows :: Value
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

selectPermission :: SelectPermissionDetails
selectPermission =
  SelectPermissionDetails
    { selectPermissionSource = Nothing,
      selectPermissionTable = mempty,
      selectPermissionRole = "test-role",
      selectPermissionColumns = mempty,
      selectPermissionRows = object [],
      selectPermissionAllowAggregations = False,
      selectPermissionLimit = Null
    }

updatePermission :: UpdatePermissionDetails
updatePermission =
  UpdatePermissionDetails
    { updatePermissionSource = Nothing,
      updatePermissionTable = mempty,
      updatePermissionRole = "test-role",
      updatePermissionColumns = mempty,
      updatePermissionRows = object []
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
