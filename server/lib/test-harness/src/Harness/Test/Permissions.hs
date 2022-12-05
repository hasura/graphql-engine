{-# LANGUAGE QuasiQuotes #-}

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
    createPermission,
    dropPermission,
    setup,
    teardown,
    selectPermission,
    updatePermission,
    insertPermission,
  )
where

import Data.Aeson qualified as Aeson
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml (yaml)
import Harness.Test.BackendType (BackendType)
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

data SelectPermissionDetails = SelectPermissionDetails
  { selectPermissionTable :: Text,
    selectPermissionSource :: Text,
    selectPermissionRole :: Text,
    selectPermissionColumns :: [Text],
    selectPermissionRows :: Aeson.Value,
    selectPermissionAllowAggregations :: Bool,
    selectPermissionLimit :: Aeson.Value
  }

data UpdatePermissionDetails = UpdatePermissionDetails
  { updatePermissionTable :: Text,
    updatePermissionSource :: Text,
    updatePermissionRole :: Text,
    updatePermissionColumns :: [Text],
    updatePermissionRows :: Aeson.Value
  }

data InsertPermissionDetails = InsertPermissionDetails
  { insertPermissionTable :: Text,
    insertPermissionSource :: Text,
    insertPermissionRole :: Text,
    insertPermissionColumns :: [Text],
    insertPermissionRows :: Aeson.Value
  }

selectPermission :: SelectPermissionDetails
selectPermission =
  SelectPermissionDetails
    { selectPermissionTable = mempty,
      selectPermissionSource = mempty,
      selectPermissionRole = mempty,
      selectPermissionColumns = mempty,
      selectPermissionRows = [yaml|{}|],
      selectPermissionAllowAggregations = False,
      selectPermissionLimit = Aeson.Null
    }

updatePermission :: UpdatePermissionDetails
updatePermission =
  UpdatePermissionDetails
    { updatePermissionTable = mempty,
      updatePermissionSource = mempty,
      updatePermissionRole = mempty,
      updatePermissionColumns = mempty,
      updatePermissionRows = [yaml|{}|]
    }

insertPermission :: InsertPermissionDetails
insertPermission =
  InsertPermissionDetails
    { insertPermissionTable = mempty,
      insertPermissionSource = mempty,
      insertPermissionRole = mempty,
      insertPermissionColumns = mempty,
      insertPermissionRows = [yaml|{}|]
    }

-- | Send a JSON payload of the common `*_create_*_permission` form.
-- Backends where the format of this api call deviates significantly from this
-- should implement their own variation in its harness module.
createPermission :: BackendType -> TestEnvironment -> Permission -> IO ()
createPermission backend env (InsertPermission InsertPermissionDetails {..}) = do
  let schemaName = Schema.getSchemaName env
      backendType = BackendType.defaultBackendTypeString backend
      requestType = backendType <> "_create_insert_permission"
      qualifiedTable = Schema.mkTableField backend schemaName insertPermissionTable
  GraphqlEngine.postMetadata_
    env
    [yaml|
      type: *requestType
      args:
        table: *qualifiedTable
        source: *insertPermissionSource
        role:  *insertPermissionRole
        permission:
          columns: *insertPermissionColumns
          filter: *insertPermissionRows
          check: {}
          set: {}
    |]
createPermission backend env (UpdatePermission UpdatePermissionDetails {..}) = do
  let schemaName = Schema.getSchemaName env
      backendType = BackendType.defaultBackendTypeString backend
      requestType = backendType <> "_create_update_permission"
      qualifiedTable = Schema.mkTableField backend schemaName updatePermissionTable
  GraphqlEngine.postMetadata_
    env
    [yaml|
      type: *requestType
      args:
        table: *qualifiedTable
        source: *updatePermissionSource
        role:  *updatePermissionRole
        permission:
          columns: *updatePermissionColumns
          filter: *updatePermissionRows
          check: {}
          set: {}
    |]
createPermission backend env (SelectPermission SelectPermissionDetails {..}) = do
  let schemaName = Schema.getSchemaName env
      backendType = BackendType.defaultBackendTypeString backend
      requestType = backendType <> "_create_select_permission"
      qualifiedTable = Schema.mkTableField backend schemaName selectPermissionTable
  GraphqlEngine.postMetadata_
    env
    [yaml|
      type: *requestType
      args:
        table: *qualifiedTable
        source: *selectPermissionSource
        role:  *selectPermissionRole
        permission:
          columns: *selectPermissionColumns
          filter: *selectPermissionRows
          allow_aggregations: *selectPermissionAllowAggregations
          limit: *selectPermissionLimit
    |]

dropPermission :: BackendType -> TestEnvironment -> Permission -> IO ()
dropPermission backend env (InsertPermission InsertPermissionDetails {..}) = do
  let schemaName = Schema.getSchemaName env
      backendType = BackendType.defaultBackendTypeString backend
      requestType = backendType <> "_drop_insert_permission"
      qualifiedTable = Schema.mkTableField backend schemaName insertPermissionTable
  GraphqlEngine.postMetadata_
    env
    [yaml|
      type: *requestType
      args:
        table: *qualifiedTable
        source: *insertPermissionSource
        role:  *insertPermissionRole
    |]
dropPermission backend env (SelectPermission SelectPermissionDetails {..}) = do
  let schemaName = Schema.getSchemaName env
      backendType = BackendType.defaultBackendTypeString backend
      requestType = backendType <> "_drop_select_permission"
      qualifiedTable = Schema.mkTableField backend schemaName selectPermissionTable
  GraphqlEngine.postMetadata_
    env
    [yaml|
      type: *requestType
      args:
        table: *qualifiedTable
        source: *selectPermissionSource
        role:  *selectPermissionRole
    |]
dropPermission backend env (UpdatePermission UpdatePermissionDetails {..}) = do
  let schemaName = Schema.getSchemaName env
      backendType = BackendType.defaultBackendTypeString backend
      requestType = backendType <> "_drop_update_permission"
      qualifiedTable = Schema.mkTableField backend schemaName updatePermissionTable
  GraphqlEngine.postMetadata_
    env
    [yaml|
      type: *requestType
      args:
        table: *qualifiedTable
        source: *updatePermissionSource
        role:  *updatePermissionRole
    |]

-- | Setup the given permissions to the graphql engine in a TestEnvironment.
setup :: BackendType -> [Permission] -> TestEnvironment -> IO ()
setup backend permissions testEnvironment =
  mapM_ (createPermission backend testEnvironment) permissions

-- | Remove the given permissions from the graphql engine in a TestEnvironment.
teardown :: BackendType -> [Permission] -> TestEnvironment -> IO ()
teardown backend permissions testEnvironment =
  mapM_ (dropPermission backend testEnvironment) permissions
