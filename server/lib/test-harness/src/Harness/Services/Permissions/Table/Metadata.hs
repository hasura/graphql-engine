{-# LANGUAGE QuasiQuotes #-}

module Harness.Services.Permissions.Table.Metadata
  ( -- Intended interface
    withTablePermissions,
    -- Low-level interface
    createPermissionMetadata,
    dropPermissionMetadata,
  )
where

import Data.Aeson qualified as J
import Data.Aeson.Types
import Data.Has
import Harness.Logging
import Harness.Quoter.Yaml (yaml)
import Harness.Schema
import Harness.Services.GraphqlEngine
import Harness.Services.Permissions.Table.Types qualified as Types
import Hasura.Prelude
import Test.Hspec

-- | Produce a JSON payload of the common `*_create_*_permission` form.
createPermissionMetadata :: Types.Permission -> Value
createPermissionMetadata (Types.InsertPermission Types.InsertPermissionDetails {..}) = do
  let (backendTypePrefix, sourceName) = insertPermissionSource
      requestType = backendTypePrefix <> "_create_insert_permission"
      (schemaName, tableName) = insertPermissionTable
      qualifiedTable = J.object ["schema" .= J.String (unSchemaName schemaName), "name" .= J.String tableName]
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
createPermissionMetadata (Types.UpdatePermission Types.UpdatePermissionDetails {..}) = do
  let (backendTypePrefix, sourceName) = updatePermissionSource
      requestType = backendTypePrefix <> "_create_update_permission"
      (schemaName, tableName) = updatePermissionTable
      qualifiedTable = J.object ["schema" .= J.String (unSchemaName schemaName), "name" .= J.String tableName]
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
createPermissionMetadata (Types.SelectPermission Types.SelectPermissionDetails {..}) = do
  let (backendTypePrefix, sourceName) = selectPermissionSource
      requestType = backendTypePrefix <> "_create_select_permission"
      (schemaName, tableName) = selectPermissionTable
      qualifiedTable = J.object ["schema" .= J.String (unSchemaName schemaName), "name" .= J.String tableName]
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

-- | Produce a JSON payload of the common `*_drop_*_permission` form.
dropPermissionMetadata :: Types.Permission -> Value
dropPermissionMetadata (Types.InsertPermission Types.InsertPermissionDetails {..}) = do
  let (backendTypePrefix, sourceName) = insertPermissionSource
      requestType = backendTypePrefix <> "_drop_insert_permission"
      (schemaName, tableName) = insertPermissionTable
      qualifiedTable = J.object ["schema" .= J.String (unSchemaName schemaName), "name" .= J.String tableName]
  [yaml|
    type: *requestType
    args:
      table: *qualifiedTable
      source: *sourceName
      role:  *insertPermissionRole
  |]
dropPermissionMetadata (Types.SelectPermission Types.SelectPermissionDetails {..}) = do
  let (backendTypePrefix, sourceName) = selectPermissionSource
      requestType = backendTypePrefix <> "_drop_insert_permission"
      (schemaName, tableName) = selectPermissionTable
      qualifiedTable = J.object ["schema" .= J.String (unSchemaName schemaName), "name" .= J.String tableName]
  [yaml|
    type: *requestType
    args:
      table: *qualifiedTable
      source: *sourceName
      role:  *selectPermissionRole
  |]
dropPermissionMetadata (Types.UpdatePermission Types.UpdatePermissionDetails {..}) = do
  let (backendTypePrefix, sourceName) = updatePermissionSource
      requestType = backendTypePrefix <> "_drop_insert_permission"
      (schemaName, tableName) = updatePermissionTable
      qualifiedTable = J.object ["schema" .= J.String (unSchemaName schemaName), "name" .= J.String tableName]
  [yaml|
    type: *requestType
    args:
      table: *qualifiedTable
      source: *sourceName
      role:  *updatePermissionRole
  |]

-- TODO: Also test inadequate permissions
withTablePermissions ::
  (Has HgeServerInstance env, Has Logger env) =>
  (env -> [Types.Permission]) ->
  SpecWith env ->
  SpecWith env
withTablePermissions perms =
  beforeWith
    ( \env -> do
        for_ (perms env)
          $ hgePostMetadata env
          . createPermissionMetadata

        return env
    )
