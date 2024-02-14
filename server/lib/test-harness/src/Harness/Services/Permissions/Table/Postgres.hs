module Harness.Services.Permissions.Table.Postgres
  ( I.Permission (..),
    I.SelectPermissionDetails (..),
    I.UpdatePermissionDetails (..),
    I.InsertPermissionDetails (..),
    selectPermission,
    updatePermission,
    insertPermission,
    withTablePermissions,
  )
where

import Data.Has
import Harness.Schema.Name
import Harness.Services.Permissions.Table.Metadata
import Harness.Services.Permissions.Table.Types qualified as I
import Harness.Services.Source.Postgres
import Hasura.Prelude

selectPermission ::
  ( Has PostgresSource env,
    Has SchemaName env
  ) =>
  env ->
  Text ->
  I.SelectPermissionDetails
selectPermission env tableName =
  I.selectPermission ("pg", postgresSourceName $ getter env) (getter env) tableName

updatePermission ::
  ( Has PostgresSource env,
    Has SchemaName env
  ) =>
  env ->
  Text ->
  I.UpdatePermissionDetails
updatePermission env tableName =
  I.updatePermission ("pg", postgresSourceName $ getter env) (getter env) tableName

insertPermission ::
  ( Has PostgresSource env,
    Has SchemaName env
  ) =>
  env ->
  Text ->
  I.InsertPermissionDetails
insertPermission env tableName =
  I.insertPermission ("pg", postgresSourceName $ getter env) (getter env) tableName
