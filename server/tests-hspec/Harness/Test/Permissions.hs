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
  )
where

import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml (yaml)
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
        permissionColumns :: [Text]
      }
  | UpdatePermission
      { permissionTable :: Text,
        permissionSource :: Text,
        permissionRole :: Text,
        permissionColumns :: [Text]
      }
  | InsertPermission
      { permissionTable :: Text,
        permissionSource :: Text,
        permissionRole :: Text,
        permissionColumns :: [Text]
      }

-- | Send a JSON payload of the common `*_create_*_permission` form.
-- Backends where the format of this api call deviates significantly from this
-- should implement their own variation in its harness module.
createPermission :: Text -> TestEnvironment -> Permission -> IO ()
createPermission backendPrefix env InsertPermission {..} = do
  let requestType = backendPrefix <> "_create_insert_permission"
      qualifiedTable =
        [yaml|
          schema: "hasura"
          name: *permissionTable
        |]
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
          check: {}
          set: {}
    |]
createPermission backendPrefix env UpdatePermission {..} = do
  let requestType = backendPrefix <> "_create_update_permission"
      qualifiedTable =
        [yaml|
          schema: "hasura"
          name: *permissionTable
        |]
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
          filter: {}
          check: {}
          set: {}
    |]
createPermission backendPrefix env SelectPermission {..} = do
  let requestType = backendPrefix <> "_create_select_permission"
      qualifiedTable =
        [yaml|
          schema: "hasura"
          name: *permissionTable
        |]
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
          filter: {}
    |]

dropPermission :: Text -> TestEnvironment -> Permission -> IO ()
dropPermission backendPrefix env InsertPermission {..} = do
  let requestType = backendPrefix <> "_drop_insert_permission"
      qualifiedTable =
        [yaml|
          schema: "hasura"
          name: *permissionTable
        |]
  GraphqlEngine.postMetadata_
    env
    [yaml|
      type: *requestType
      args:
        table: *qualifiedTable
        source: *permissionSource
        role:  *permissionRole
    |]
dropPermission backendPrefix env SelectPermission {..} = do
  let requestType = backendPrefix <> "_drop_select_permission"
      qualifiedTable =
        [yaml|
          schema: "hasura"
          name: *permissionTable
        |]
  GraphqlEngine.postMetadata_
    env
    [yaml|
      type: *requestType
      args:
        table: *qualifiedTable
        source: *permissionSource
        role:  *permissionRole
    |]
dropPermission backendPrefix env UpdatePermission {..} = do
  let requestType = backendPrefix <> "_drop_update_permission"
      qualifiedTable =
        [yaml|
          schema: "hasura"
          name: *permissionTable
        |]
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
setup :: Text -> [Permission] -> TestEnvironment -> IO ()
setup backendPrefix permissions testEnvironment =
  mapM_ (createPermission backendPrefix testEnvironment) permissions

-- | Remove the given permissions from the graphql engine in a TestEnvironment.
teardown :: Text -> [Permission] -> TestEnvironment -> IO ()
teardown backendPrefix permissions testEnvironment =
  mapM_ (dropPermission backendPrefix testEnvironment) permissions
