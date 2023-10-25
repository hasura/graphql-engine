{-# LANGUAGE QuasiQuotes #-}

module Harness.Permissions.Metadata
  ( createPermissionMetadata,
    dropPermissionMetadata,
  )
where

import Data.Aeson
import Data.Text qualified as Text
import Harness.Permissions.Types qualified as Types
import Harness.Quoter.Yaml (yaml)
import Harness.Schema qualified as Schema
import Harness.Test.BackendType qualified as BackendType
import Harness.TestEnvironment (TestEnvironment (..), getBackendTypeConfig)
import Hasura.Prelude

-- | Produce a JSON payload of the common `*_create_*_permission` form.
createPermissionMetadata :: TestEnvironment -> Types.Permission -> Value
createPermissionMetadata testEnvironment (Types.InsertPermission Types.InsertPermissionDetails {..}) = do
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
      validateInput =
        insertPermissionValidationWebhook <&> \url -> object ["type" .= ("http" :: String), "definition" .= (object ["url" .= url])]
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
        validate_input: *validateInput
  |]
createPermissionMetadata testEnvironment (Types.UpdatePermission Types.UpdatePermissionDetails {..}) = do
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
      validateInput =
        updatePermissionValidationWebhook <&> \url -> object ["type" .= ("http" :: String), "definition" .= (object ["url" .= url])]
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
        validate_input: *validateInput
  |]
createPermissionMetadata testEnvironment (Types.SelectPermission Types.SelectPermissionDetails {..}) = do
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
        computed_fields: *selectPermissionComputedFields
        filter: *selectPermissionRows
        allow_aggregations: *selectPermissionAllowAggregations
        limit: *selectPermissionLimit
  |]
createPermissionMetadata testEnvironment (Types.LogicalModelSelectPermission Types.LogicalModelSelectPermissionDetails {..}) = do
  let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
      backendType = BackendType.backendTypeString backendTypeMetadata
      sourceName =
        maybe
          (BackendType.backendSourceName backendTypeMetadata)
          Text.unpack
          lmSelectPermissionSource
      requestType = backendType <> "_create_logical_model_select_permission"
  [yaml|
    type: *requestType
    args:
      source: *sourceName
      name: *lmSelectPermissionName
      role:  *lmSelectPermissionRole
      permission:
        columns: *lmSelectPermissionColumns
        filter: *lmSelectPermissionFilter
  |]
createPermissionMetadata testEnvironment (Types.DeletePermission Types.DeletePermissionDetails {..}) = do
  let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
      schemaName = Schema.getSchemaName testEnvironment
      backendType = BackendType.backendTypeString backendTypeMetadata
      sourceName =
        maybe
          (BackendType.backendSourceName backendTypeMetadata)
          Text.unpack
          deletePermissionSource
      requestType = backendType <> "_create_delete_permission"
      qualifiedTable = Schema.mkTableField backendTypeMetadata schemaName deletePermissionTable
      validateInput =
        deletePermissionValidationWebhook <&> \url -> object ["type" .= ("http" :: String), "definition" .= (object ["url" .= url])]
  [yaml|
    type: *requestType
    args:
      table: *qualifiedTable
      source: *sourceName
      role:  *deletePermissionRole
      permission:
        filter: *deletePermissionRows
        validate_input: *validateInput
  |]
createPermissionMetadata _testEnvironment (Types.InheritedRole Types.InheritedRoleDetails {..}) = do
  [yaml|
    type: add_inherited_role
    args:
      role_name: *inheritedRoleName
      role_set: *inheritedRoleRoleSet
  |]

-- | Produce a JSON payload of the common `*_drop_*_permission` form.
dropPermissionMetadata :: TestEnvironment -> Types.Permission -> Value
dropPermissionMetadata env (Types.InsertPermission Types.InsertPermissionDetails {..}) = do
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
dropPermissionMetadata env (Types.SelectPermission Types.SelectPermissionDetails {..}) = do
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
dropPermissionMetadata env (Types.LogicalModelSelectPermission Types.LogicalModelSelectPermissionDetails {..}) = do
  let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig env
      backendType = BackendType.backendTypeString backendTypeMetadata
      sourceName =
        maybe
          (BackendType.backendSourceName backendTypeMetadata)
          Text.unpack
          lmSelectPermissionSource
      requestType = backendType <> "_drop_logical_model_select_permission"
  [yaml|
    type: *requestType
    args:
      source: *sourceName
      name: *lmSelectPermissionName
      role: *lmSelectPermissionRole
  |]
dropPermissionMetadata env (Types.UpdatePermission Types.UpdatePermissionDetails {..}) = do
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
dropPermissionMetadata env (Types.DeletePermission Types.DeletePermissionDetails {..}) = do
  let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig env
      schemaName = Schema.getSchemaName env
      backendType = BackendType.backendTypeString backendTypeMetadata
      sourceName = BackendType.backendSourceName backendTypeMetadata
      requestType = backendType <> "_drop_delete_permission"
      qualifiedTable = Schema.mkTableField backendTypeMetadata schemaName deletePermissionTable
  [yaml|
    type: *requestType
    args:
      table: *qualifiedTable
      source: *sourceName
      role:  *deletePermissionRole
  |]
dropPermissionMetadata _env (Types.InheritedRole Types.InheritedRoleDetails {..}) = do
  [yaml|
    type: drop_inherited_role
    args:
      role_name: *inheritedRoleName
  |]
