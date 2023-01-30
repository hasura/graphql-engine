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
    createPermission,
    dropPermission,
    setup,
    teardown,
    selectPermission,
    updatePermission,
    insertPermission,
    withPermissions,
    withRole,
  )
where

import Control.Exception (SomeException, finally, try)
import Data.Aeson qualified as Aeson
import Data.List (subsequences)
import Data.Text qualified as Text
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml (yaml)
import Harness.Test.BackendType (BackendTypeConfig)
import Harness.Test.BackendType qualified as BackendType
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment
import Hasura.Prelude
import Test.Hspec (expectationFailure)
import Test.Hspec.Core.Spec
import Text.Show.Pretty (ppShow)

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

-- | Assert that a given test block requires precisely the given permissions.
--
-- This function modifies the given Hspec test "forest" to replace each test
-- with two separate tests:
--
-- * The original test, but all requests will be made under a Hasura role with
--   the given permissions. This test should therefore only pass if the given
--   permissions are sufficient.
--
-- * The opposite test: all requests will be made under a Hasura role with
--   every (proper) subset of the given permissions. If the original test can
--   run successfully with missing permissions, then this test will __fail__.
--
-- The responsibility is on the test writer to make the permissions
-- requirements as granular as possible. If two tests in the same block require
-- differing levels of permissions, those tests should be separated into
-- distinct blocks.
--
-- Note that we don't check anything about /extra/ permissions.
withPermissions :: NonEmpty Permission -> SpecWith TestEnvironment -> SpecWith TestEnvironment
withPermissions (toList -> permissions) = mapSpecForest (map go)
  where
    go :: SpecTree TestEnvironment -> SpecTree TestEnvironment
    go = \case
      Node name forest ->
        Node name (map go forest)
      NodeWithCleanup cleanup c forest ->
        NodeWithCleanup cleanup c (map go forest)
      Leaf item ->
        Node
          (itemRequirement item)
          [ Leaf
              item
                { itemRequirement = "Passes with sufficient permissions",
                  itemExample = \params -> itemExample item params . succeeding
                },
            Leaf
              item
                { itemRequirement = "Rejects insufficient permissions",
                  itemExample = \params -> itemExample item params . failing
                }
          ]

    succeeding :: (ActionWith TestEnvironment -> IO ()) -> ActionWith TestEnvironment -> IO ()
    succeeding k test = k \testEnvironment -> do
      let permissions' :: [Permission]
          permissions' = fmap (withRole "success") permissions

      traverse_ (createPermission testEnvironment) permissions'

      test testEnvironment {testingRole = Just "success"}
        `finally` for_ (getBackendTypeConfig testEnvironment) \config ->
          traverse_ (dropPermission config testEnvironment) permissions'

    failing :: (ActionWith TestEnvironment -> IO ()) -> ActionWith TestEnvironment -> IO ()
    failing k test = k \testEnvironment -> do
      -- Test every possible (strict) subset of the permissions to ensure that
      -- they lead to test failures.
      for_ (subsequences permissions) \subsequence ->
        unless (subsequence == permissions) do
          let permissions' = map (withRole "failure") subsequence

          traverse_ (createPermission testEnvironment) permissions'

          let attempt :: IO () -> IO ()
              attempt x =
                try x >>= \case
                  Right _ ->
                    expectationFailure $
                      mconcat
                        [ "Unexpectedly adequate permissions:\n",
                          ppShow permissions'
                        ]
                  Left (_ :: SomeException) ->
                    pure ()

          attempt (test testEnvironment {testingRole = Just "failure"})
            `finally` for_ (getBackendTypeConfig testEnvironment) \config ->
              traverse_ (dropPermission config testEnvironment) permissions'

-- | Update the role on a given permission.
withRole :: Text -> Permission -> Permission
withRole role = \case
  SelectPermission p -> SelectPermission p {selectPermissionRole = role}
  UpdatePermission p -> UpdatePermission p {updatePermissionRole = role}
  InsertPermission p -> InsertPermission p {insertPermissionRole = role}

-- | Send a JSON payload of the common `*_create_*_permission` form.
-- Backends where the format of this api call deviates significantly from this
-- should implement their own variation in its harness module.
createPermission :: TestEnvironment -> Permission -> IO ()
createPermission testEnvironment (InsertPermission InsertPermissionDetails {..}) = do
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
  GraphqlEngine.postMetadata_
    testEnvironment
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
createPermission testEnvironment (UpdatePermission UpdatePermissionDetails {..}) = do
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
  GraphqlEngine.postMetadata_
    testEnvironment
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
createPermission testEnvironment (SelectPermission SelectPermissionDetails {..}) = do
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
  GraphqlEngine.postMetadata_
    testEnvironment
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

dropPermission :: BackendTypeConfig -> TestEnvironment -> Permission -> IO ()
dropPermission backendTypeMetadata env (InsertPermission InsertPermissionDetails {..}) = do
  let schemaName = Schema.getSchemaName env
      backendType = BackendType.backendTypeString backendTypeMetadata
      requestType = backendType <> "_drop_insert_permission"
      sourceName = BackendType.backendSourceName backendTypeMetadata
      qualifiedTable = Schema.mkTableField backendTypeMetadata schemaName insertPermissionTable
  GraphqlEngine.postMetadata_
    env
    [yaml|
      type: *requestType
      args:
        table: *qualifiedTable
        source: *sourceName
        role:  *insertPermissionRole
    |]
dropPermission backendTypeMetadata env (SelectPermission SelectPermissionDetails {..}) = do
  let schemaName = Schema.getSchemaName env
      backendType = BackendType.backendTypeString backendTypeMetadata
      sourceName = BackendType.backendSourceName backendTypeMetadata
      requestType = backendType <> "_drop_select_permission"
      qualifiedTable = Schema.mkTableField backendTypeMetadata schemaName selectPermissionTable
  GraphqlEngine.postMetadata_
    env
    [yaml|
      type: *requestType
      args:
        table: *qualifiedTable
        source: *sourceName
        role:  *selectPermissionRole
    |]
dropPermission backendTypeMetadata env (UpdatePermission UpdatePermissionDetails {..}) = do
  let schemaName = Schema.getSchemaName env
      backendType = BackendType.backendTypeString backendTypeMetadata
      sourceName = BackendType.backendSourceName backendTypeMetadata
      requestType = backendType <> "_drop_update_permission"
      qualifiedTable = Schema.mkTableField backendTypeMetadata schemaName updatePermissionTable
  GraphqlEngine.postMetadata_
    env
    [yaml|
      type: *requestType
      args:
        table: *qualifiedTable
        source: *sourceName
        role:  *updatePermissionRole
    |]

-- | Setup the given permissions to the graphql engine in a TestEnvironment.
setup :: [Permission] -> TestEnvironment -> IO ()
setup permissions testEnvironment =
  mapM_ (createPermission testEnvironment) permissions

-- | Remove the given permissions from the graphql engine in a TestEnvironment.
teardown :: BackendTypeConfig -> [Permission] -> TestEnvironment -> IO ()
teardown backendTypeMetadata permissions testEnvironment =
  mapM_ (dropPermission backendTypeMetadata testEnvironment) permissions
