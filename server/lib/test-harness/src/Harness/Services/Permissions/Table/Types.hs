module Harness.Services.Permissions.Table.Types
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
import Harness.Schema.Name
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
  { selectPermissionSource :: (Text, Text),
    selectPermissionTable :: (SchemaName, Text),
    selectPermissionRole :: Text,
    selectPermissionColumns :: [Text],
    selectPermissionRows :: Value,
    selectPermissionAllowAggregations :: Bool,
    selectPermissionLimit :: Value
  }
  deriving (Eq, Show)

data UpdatePermissionDetails = UpdatePermissionDetails
  { updatePermissionSource :: (Text, Text),
    updatePermissionTable :: (SchemaName, Text),
    updatePermissionRole :: Text,
    updatePermissionColumns :: [Text],
    updatePermissionRows :: Value
  }
  deriving (Eq, Show)

data InsertPermissionDetails = InsertPermissionDetails
  { insertPermissionSource :: (Text, Text),
    insertPermissionTable :: (SchemaName, Text),
    insertPermissionRole :: Text,
    insertPermissionColumns :: [Text],
    insertPermissionRows :: Value
  }
  deriving (Eq, Show)

selectPermission :: (Text, Text) -> SchemaName -> Text -> SelectPermissionDetails
selectPermission (backendPrefix, sourceName) schemaName tableName =
  SelectPermissionDetails
    { selectPermissionSource = (backendPrefix, sourceName),
      selectPermissionTable = (schemaName, tableName),
      selectPermissionRole = "test-role",
      selectPermissionColumns = mempty,
      selectPermissionRows = object [],
      selectPermissionAllowAggregations = False,
      selectPermissionLimit = Null
    }

updatePermission :: (Text, Text) -> SchemaName -> Text -> UpdatePermissionDetails
updatePermission (backendPrefix, sourceName) schemaName tableName =
  UpdatePermissionDetails
    { updatePermissionSource = (backendPrefix, sourceName),
      updatePermissionTable = (schemaName, tableName),
      updatePermissionRole = "test-role",
      updatePermissionColumns = mempty,
      updatePermissionRows = object []
    }

insertPermission :: (Text, Text) -> SchemaName -> Text -> InsertPermissionDetails
insertPermission (backendPrefix, sourceName) schemaName tableName =
  InsertPermissionDetails
    { insertPermissionSource = (backendPrefix, sourceName),
      insertPermissionTable = (schemaName, tableName),
      insertPermissionRole = "test-role",
      insertPermissionColumns = mempty,
      insertPermissionRows = object []
    }
