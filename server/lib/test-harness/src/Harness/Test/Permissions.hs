{-# LANGUAGE QuasiQuotes #-}

-- | This module captures what different backends happen to have in common with
-- regard to permission metadata handling.
--
-- Tests should never use the setup function in this module directly but instead
-- rely those exposed in specific backend harness modules.
module Harness.Test.Permissions
  ( Permission (..),
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
  = SelectPermission
      { permissionTable :: Text,
        permissionSource :: Text,
        permissionRole :: Text,
        permissionColumns :: [Text],
        permissionRows :: Aeson.Value,
        permissionAllowAggregations :: Bool
      }
  | UpdatePermission
      { permissionTable :: Text,
        permissionSource :: Text,
        permissionRole :: Text,
        permissionColumns :: [Text],
        permissionRows :: Aeson.Value
      }
  | InsertPermission
      { permissionTable :: Text,
        permissionSource :: Text,
        permissionRole :: Text,
        permissionColumns :: [Text],
        permissionRows :: Aeson.Value
      }

selectPermission :: Permission
selectPermission =
  SelectPermission
    { permissionTable = mempty,
      permissionSource = mempty,
      permissionRole = mempty,
      permissionColumns = mempty,
      permissionRows = [yaml|{}|],
      permissionAllowAggregations = False
    }

updatePermission :: Permission
updatePermission =
  UpdatePermission
    { permissionTable = mempty,
      permissionSource = mempty,
      permissionRole = mempty,
      permissionColumns = mempty,
      permissionRows = [yaml|{}|]
    }

insertPermission :: Permission
insertPermission =
  InsertPermission
    { permissionTable = mempty,
      permissionSource = mempty,
      permissionRole = mempty,
      permissionColumns = mempty,
      permissionRows = [yaml|{}|]
    }

-- | Send a JSON payload of the common `*_create_*_permission` form.
-- Backends where the format of this api call deviates significantly from this
-- should implement their own variation in its harness module.
createPermission :: BackendType -> TestEnvironment -> Permission -> IO ()
createPermission backend env InsertPermission {..} = do
  let schemaName = Schema.getSchemaName env
      backendType = BackendType.defaultBackendTypeString backend
      requestType = backendType <> "_create_insert_permission"
      qualifiedTable = Schema.mkTableField backend schemaName permissionTable
  GraphqlEngine.postMetadata_
    env
    [yaml|
      type: *requestType
      args:
        table: *qualifiedTable
        source: *permissionSource
        role:  *permissionRole
        permission:
          columns: *permissionColumns
          filter: *permissionRows
          check: {}
          set: {}
    |]
createPermission backend env UpdatePermission {..} = do
  let schemaName = Schema.getSchemaName env
      backendType = BackendType.defaultBackendTypeString backend
      requestType = backendType <> "_create_update_permission"
      qualifiedTable = Schema.mkTableField backend schemaName permissionTable
  GraphqlEngine.postMetadata_
    env
    [yaml|
      type: *requestType
      args:
        table: *qualifiedTable
        source: *permissionSource
        role:  *permissionRole
        permission:
          columns: *permissionColumns
          filter: *permissionRows
          check: {}
          set: {}
    |]
createPermission backend env SelectPermission {..} = do
  let schemaName = Schema.getSchemaName env
      backendType = BackendType.defaultBackendTypeString backend
      requestType = backendType <> "_create_select_permission"
      qualifiedTable = Schema.mkTableField backend schemaName permissionTable
  GraphqlEngine.postMetadata_
    env
    [yaml|
      type: *requestType
      args:
        table: *qualifiedTable
        source: *permissionSource
        role:  *permissionRole
        permission:
          columns: *permissionColumns
          filter: *permissionRows
          allow_aggregations: *permissionAllowAggregations
    |]

dropPermission :: BackendType -> TestEnvironment -> Permission -> IO ()
dropPermission backend env InsertPermission {..} = do
  let schemaName = Schema.getSchemaName env
      backendType = BackendType.defaultBackendTypeString backend
      requestType = backendType <> "_drop_insert_permission"
      qualifiedTable = Schema.mkTableField backend schemaName permissionTable
  GraphqlEngine.postMetadata_
    env
    [yaml|
      type: *requestType
      args:
        table: *qualifiedTable
        source: *permissionSource
        role:  *permissionRole
    |]
dropPermission backend env SelectPermission {..} = do
  let schemaName = Schema.getSchemaName env
      backendType = BackendType.defaultBackendTypeString backend
      requestType = backendType <> "_drop_select_permission"
      qualifiedTable = Schema.mkTableField backend schemaName permissionTable
  GraphqlEngine.postMetadata_
    env
    [yaml|
      type: *requestType
      args:
        table: *qualifiedTable
        source: *permissionSource
        role:  *permissionRole
    |]
dropPermission backend env UpdatePermission {..} = do
  let schemaName = Schema.getSchemaName env
      backendType = BackendType.defaultBackendTypeString backend
      requestType = backendType <> "_drop_update_permission"
      qualifiedTable = Schema.mkTableField backend schemaName permissionTable
  GraphqlEngine.postMetadata_
    env
    [yaml|
      type: *requestType
      args:
        table: *qualifiedTable
        source: *permissionSource
        role:  *permissionRole
    |]

-- | Setup the given permissions to the graphql engine in a TestEnvironment.
setup :: BackendType -> [Permission] -> TestEnvironment -> IO ()
setup backend permissions testEnvironment =
  mapM_ (createPermission backend testEnvironment) permissions

-- | Remove the given permissions from the graphql engine in a TestEnvironment.
teardown :: BackendType -> [Permission] -> TestEnvironment -> IO ()
teardown backend permissions testEnvironment =
  mapM_ (dropPermission backend testEnvironment) permissions
