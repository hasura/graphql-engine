{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

-- | This module captures what different backends happen to have in common with
-- regard to permission metadata handling.
--
-- Tests should never use the setup function in this module directly but instead
-- rely those exposed in specific backend harness modules.
module Harness.Test.Permissions
  ( Permission (..),
    SelectPermissionDetails (..),
    UpdatePermissionDetails (..),
    InsertPermissionDetails (..),
    createPermissionCommand,
    dropPermissionCommand,
    selectPermission,
    updatePermission,
    insertPermission,
  )
where

import Data.Aeson qualified as Aeson
import Data.Text qualified as Text
import Harness.Quoter.Yaml (yaml)
import Harness.Test.BackendType qualified as BackendType
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment
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
    selectPermissionRows :: Aeson.Value,
    selectPermissionAllowAggregations :: Bool,
    selectPermissionLimit :: Aeson.Value
  }
  deriving (Eq, Show)

data UpdatePermissionDetails = UpdatePermissionDetails
  { updatePermissionSource :: Maybe Text,
    updatePermissionTable :: Text,
    updatePermissionRole :: Text,
    updatePermissionColumns :: [Text],
    updatePermissionRows :: Aeson.Value
  }
  deriving (Eq, Show)

data InsertPermissionDetails = InsertPermissionDetails
  { insertPermissionSource :: Maybe Text,
    insertPermissionTable :: Text,
    insertPermissionRole :: Text,
    insertPermissionColumns :: [Text],
    insertPermissionRows :: Aeson.Value
  }
  deriving (Eq, Show)

selectPermission :: SelectPermissionDetails
selectPermission =
  SelectPermissionDetails
    { selectPermissionSource = Nothing,
      selectPermissionTable = mempty,
      selectPermissionRole = mempty,
      selectPermissionColumns = mempty,
      selectPermissionRows = [yaml|{}|],
      selectPermissionAllowAggregations = False,
      selectPermissionLimit = Aeson.Null
    }

updatePermission :: UpdatePermissionDetails
updatePermission =
  UpdatePermissionDetails
    { updatePermissionSource = Nothing,
      updatePermissionTable = mempty,
      updatePermissionRole = mempty,
      updatePermissionColumns = mempty,
      updatePermissionRows = [yaml|{}|]
    }

insertPermission :: InsertPermissionDetails
insertPermission =
  InsertPermissionDetails
    { insertPermissionSource = Nothing,
      insertPermissionTable = mempty,
      insertPermissionRole = mempty,
      insertPermissionColumns = mempty,
      insertPermissionRows = [yaml|{}|]
    }

-- | Send a JSON payload of the common `*_create_*_permission` form.
-- Backends where the format of this api call deviates significantly from this
-- should implement their own variation in its harness module.
createPermissionCommand :: TestEnvironment -> Permission -> Aeson.Value
createPermissionCommand testEnvironment (InsertPermission InsertPermissionDetails {..}) = do
  let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
      schemaName = Schema.getSchemaName testEnvironment
      backendType = BackendType.backendTypeString backendTypeMetadata
      sourceName =
        maybe
          (BackendType.backendSourceName backendTypeMetadata)
          Text.unpack
          insertPermissionSource
      requestType = backendType <> "_create_insert_permission"
      qualifiedTable = Schema.mkTableField backendTypeMetadata schemaName insertPermissionTable
  [yaml|
    type: *requestType
    args:
      table: *qualifiedTable
      source: *sourceName
      role:  *insertPermissionRole
      permission:
        columns: *insertPermissionColumns
        filter: *insertPermissionRows
        check: {}
        set: {}
  |]
createPermissionCommand testEnvironment (UpdatePermission UpdatePermissionDetails {..}) = do
  let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
      schemaName = Schema.getSchemaName testEnvironment
      backendType = BackendType.backendTypeString backendTypeMetadata
      sourceName =
        maybe
          (BackendType.backendSourceName backendTypeMetadata)
          Text.unpack
          updatePermissionSource
      requestType = backendType <> "_create_update_permission"
      qualifiedTable = Schema.mkTableField backendTypeMetadata schemaName updatePermissionTable
  [yaml|
    type: *requestType
    args:
      table: *qualifiedTable
      source: *sourceName
      role:  *updatePermissionRole
      permission:
        columns: *updatePermissionColumns
        filter: *updatePermissionRows
        check: {}
        set: {}
  |]
createPermissionCommand testEnvironment (SelectPermission SelectPermissionDetails {..}) = do
  let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
      schemaName = Schema.getSchemaName testEnvironment
      backendType = BackendType.backendTypeString backendTypeMetadata
      sourceName =
        maybe
          (BackendType.backendSourceName backendTypeMetadata)
          Text.unpack
          selectPermissionSource
      requestType = backendType <> "_create_select_permission"
      qualifiedTable = Schema.mkTableField backendTypeMetadata schemaName selectPermissionTable
  [yaml|
    type: *requestType
    args:
      table: *qualifiedTable
      source: *sourceName
      role:  *selectPermissionRole
      permission:
        columns: *selectPermissionColumns
        filter: *selectPermissionRows
        allow_aggregations: *selectPermissionAllowAggregations
        limit: *selectPermissionLimit
  |]

dropPermissionCommand :: TestEnvironment -> Permission -> Aeson.Value
dropPermissionCommand env (InsertPermission InsertPermissionDetails {..}) = do
  let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig env
      schemaName = Schema.getSchemaName env
      backendType = BackendType.backendTypeString backendTypeMetadata
      requestType = backendType <> "_drop_insert_permission"
      sourceName = BackendType.backendSourceName backendTypeMetadata
      qualifiedTable = Schema.mkTableField backendTypeMetadata schemaName insertPermissionTable
  [yaml|
    type: *requestType
    args:
      table: *qualifiedTable
      source: *sourceName
      role:  *insertPermissionRole
  |]
dropPermissionCommand env (SelectPermission SelectPermissionDetails {..}) = do
  let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig env
      schemaName = Schema.getSchemaName env
      backendType = BackendType.backendTypeString backendTypeMetadata
      sourceName = BackendType.backendSourceName backendTypeMetadata
      requestType = backendType <> "_drop_select_permission"
      qualifiedTable = Schema.mkTableField backendTypeMetadata schemaName selectPermissionTable
  [yaml|
    type: *requestType
    args:
      table: *qualifiedTable
      source: *sourceName
      role:  *selectPermissionRole
  |]
dropPermissionCommand env (UpdatePermission UpdatePermissionDetails {..}) = do
  let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig env
      schemaName = Schema.getSchemaName env
      backendType = BackendType.backendTypeString backendTypeMetadata
      sourceName = BackendType.backendSourceName backendTypeMetadata
      requestType = backendType <> "_drop_update_permission"
      qualifiedTable = Schema.mkTableField backendTypeMetadata schemaName updatePermissionTable
  [yaml|
    type: *requestType
    args:
      table: *qualifiedTable
      source: *sourceName
      role:  *updatePermissionRole
  |]
