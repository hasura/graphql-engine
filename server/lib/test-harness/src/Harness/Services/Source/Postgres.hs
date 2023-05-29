{-# LANGUAGE QuasiQuotes #-}

-- | This module houses low-level functions and types to help access and work
-- with Postgres sources.
module Harness.Services.Source.Postgres
  ( withPostgresSource,
    PostgresSource (..),
    withPostgresSchema,
  )
where

import Data.Aeson qualified as J
import Data.Aeson.Key qualified as K
import Data.Aeson.Types qualified as J
import Data.Has
import Harness.Logging
import Harness.Quoter.Yaml (yaml)
import Harness.Schema.Name
import Harness.Schema.Table qualified as Schema
import Harness.Services.Database.Postgres
import Harness.Services.GraphqlEngine
import Hasura.Prelude
import Test.Hspec

data PostgresSource = PostgresSource
  { postgresSourceName :: Text,
    postgresSourceDbName :: FreshPostgresDb,
    postgresSourceBaseUrl :: PostgresServerUrl
  }

withPostgresSource ::
  ( Has Logger testEnvironment,
    Has PostgresServerUrl testEnvironment,
    Has HgeServerInstance testEnvironment
  ) =>
  Text ->
  SpecWith (PostgresSource, testEnvironment) ->
  SpecWith testEnvironment
withPostgresSource sourceName =
  describe "Postgres"
    . withFreshPostgresDb
    . ( aroundWith \action (freshDb, env) -> do
          liftIO $ testLogMessage env (logHarness ("adding source.." :: Text))
          let pgUrl = getter env
          pg_add_source (freshDb, env) sourceName
          -- TODO assert that res is a success result
          -- TODO: use 'managed'?
          action (PostgresSource sourceName freshDb pgUrl, env)
      )

withPostgresSchema ::
  ( Has PostgresSource testEnvironment,
    Has Logger testEnvironment,
    Has SchemaName testEnvironment,
    Has HgeServerInstance testEnvironment
  ) =>
  [Schema.Table] ->
  SpecWith testEnvironment ->
  SpecWith testEnvironment
withPostgresSchema tables spec = flip aroundWith spec \action testEnvironment' -> do
  let PostgresSource {..} = getter testEnvironment'
      pgUrl = mkFreshDbConnectionString postgresSourceBaseUrl postgresSourceDbName

      testEnvironment = (pgUrl, testEnvironment')

  -- Setup and track tables
  for_ tables $ \table -> do
    createTable testEnvironment table
    insertTable testEnvironment table
    pg_track_table testEnvironment table

  {- TODO:

  -- Setup relationships
  for_ tables $ \table -> do
    Schema.trackObjectRelationships table testEnvironment
    Schema.trackArrayRelationships table testEnvironment
    -}

  action testEnvironment'

pg_add_source ::
  ( Has Logger env,
    Has PostgresServerUrl env,
    Has FreshPostgresDb env,
    Has HgeServerInstance env
  ) =>
  env ->
  Text ->
  IO ()
pg_add_source env sourceName = do
  let pgServerUrl = getter env
      pgDb = getter env
      pgUrl = mkFreshDbConnectionString pgServerUrl pgDb

  _res <-
    hgePost
      env
      200
      "/v1/metadata"
      []
      [yaml|
          args:
            configuration:
              connection_info:
                database_url: *pgUrl
            name: *sourceName
          type: pg_add_source
        |]

  -- TODO assert that res is a success result
  return ()

pg_track_table ::
  ( HasCallStack,
    Has PostgresSource testEnvironment,
    Has HgeServerInstance testEnvironment,
    Has SchemaName testEnvironment,
    Has Logger testEnvironment
  ) =>
  testEnvironment ->
  Schema.Table ->
  IO ()
pg_track_table testEnvironment (Schema.Table {tableName, tableColumns}) = do
  let PostgresSource {..} = getter testEnvironment
  let schemaName = getter @SchemaName testEnvironment
  let column_config = columnsConfig tableColumns
  _ <-
    hgePost
      testEnvironment
      200
      "/v1/metadata"
      []
      [yaml|
            type: pg_track_table
            args:
              source: *postgresSourceName
              table:
                schema: *schemaName
                name: *tableName
              configuration:
                column_config: *column_config
          |]

  return ()
  where
    columnsConfig :: [Schema.Column] -> J.Value
    columnsConfig = J.object . mapMaybe columnConfig

    columnConfig :: Schema.Column -> Maybe J.Pair
    columnConfig col = do
      alias <- Schema.columnGqlAlias col
      return
        $ ( K.fromText $ Schema.columnName col,
            [yaml|
        custom_name: *alias
        |]
          )
