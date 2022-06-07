{-# LANGUAGE QuasiQuotes #-}

-- | Common interface for setup/teardown of permissions metadata.
--
-- It's preferable to use the backend-specific versions of 'setup' and 'teardown'.
module Harness.Test.Permissions
  ( Permission (..),
    createPermission,
    dropPermission,
    setup,
    teardown,
  )
where

import Data.Text (Text)
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml (yaml)
import Harness.TestEnvironment
import Prelude

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
