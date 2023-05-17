{-# LANGUAGE QuasiQuotes #-}

-- | This module houses the componentized fixture functions and types for hspec
-- tests that use the postgres data connector.
module Harness.Services.Source.DCPostgres
  ( DcPostgresSource (..),
    withDcPostgresSource,
    withDcPostgresSchema,
  )
where

import Control.Lens
import Control.Monad.Managed
import Data.Aeson qualified as J
import Data.Aeson.Key qualified as K
import Data.Aeson.Lens qualified as JL
import Data.Aeson.Types qualified as J
import Data.Has
import Harness.Logging
import Harness.Quoter.Yaml (interpolateYaml, yaml)
import Harness.Schema.Name
import Harness.Schema.Table qualified as Schema
import Harness.Services.Database.Postgres
import Harness.Services.ExternalProcess.DCPostgresAgent
import Harness.Services.GraphqlEngine.API
import Hasura.Prelude
import Test.Hspec

data DcPostgresSource = DcPostgresSource
  { dcPostgresSourceName :: Text,
    dcPostgresSourceAgentKind :: DcPgAgentKind,
    dcPostgresSourceDbName :: FreshPostgresDb,
    dcPostgresSourceBaseUrl :: PostgresServerUrl
  }

newtype DcPgAgentKind = DcPgAgentKind {getDcPgAgentKind :: Text}

-- | Setup a source that uses a fresh instance of the postgres agent (drawn from
-- a pool) and pointing to a fresh postgres database.
--
-- If you need a more specific setup in order to test something, feel free to make
-- a new version of this function or export some of its building blocks.
withDcPostgresSource ::
  ( Has DcPgBinPath testEnvironment,
    Has DcPgPool testEnvironment,
    Has HgeServerInstance testEnvironment,
    Has Logger testEnvironment,
    Has PostgresServerUrl testEnvironment
  ) =>
  Text ->
  SpecWith (DcPostgresSource, testEnvironment) ->
  SpecWith testEnvironment
withDcPostgresSource sourceName =
  describe "Dc Postgres"
    . withFreshPostgresDb
    . withDefaultDcPgAgent
    . ( aroundWith \action (agentKind, (freshDb, env)) -> do
          let pgUrl = getter env
          dc_pg_add_source (agentKind, (freshDb, env)) sourceName
          action (DcPostgresSource sourceName agentKind freshDb pgUrl, env)
      )

setupDcPostgresAgent ::
  ( Has Logger env,
    Has HgeServerInstance env,
    Has DcPgAgentInstance env
  ) =>
  env ->
  IO DcPgAgentKind
setupDcPostgresAgent env = do
  let agentKind = DcPgAgentKind "dcpg"
  setupDcPostgresAgentNamed agentKind env
  return agentKind

setupDcPostgresAgentNamed ::
  ( Has Logger env,
    Has HgeServerInstance env,
    Has DcPgAgentInstance env
  ) =>
  DcPgAgentKind ->
  env ->
  IO ()
setupDcPostgresAgentNamed agentKind env = do
  metadata <- export_metadata env
  let agentUrl = getDcPgAgentInstanceUrl $ getter env

  let agentConfig =
        [interpolateYaml|
        #{getDcPgAgentKind agentKind}:
          uri: #{agentUrl}
          |]

  let metadata' =
        ( metadata
            & JL.atKey "backend_configs" %~ \case
              Nothing -> Just (J.Object mempty)
              Just x -> Just x
        )
          & JL.key "backend_configs" . JL.atKey "dataconnector" .~ Just agentConfig

  _ <- replace_metadata env metadata'

  return ()

-- | Draw an agent instance and attach it to an hge instance, with only the
-- default configuration.
withDefaultDcPgAgent ::
  ( Has Logger testEnvironment,
    Has DcPgBinPath testEnvironment,
    Has DcPgPool testEnvironment,
    Has HgeServerInstance testEnvironment
  ) =>
  SpecWith (DcPgAgentKind, testEnvironment) ->
  SpecWith testEnvironment
withDefaultDcPgAgent = aroundWith \action env -> runManaged $ do
  agentInstance <- drawFromPool env emptyDcPgConfig
  agentKind <- liftIO $ setupDcPostgresAgent (agentInstance, env)
  liftIO $ action (agentKind, env)

withDcPostgresSchema ::
  ( Has DcPostgresSource testEnvironment,
    Has Logger testEnvironment,
    Has SchemaName testEnvironment,
    Has HgeServerInstance testEnvironment
  ) =>
  [Schema.Table] ->
  SpecWith testEnvironment ->
  SpecWith testEnvironment
withDcPostgresSchema tables spec = flip aroundWith spec \action testEnvironment' -> do
  let DcPostgresSource {..} = getter testEnvironment'
      pgUrl = mkFreshDbConnectionString dcPostgresSourceBaseUrl dcPostgresSourceDbName

      testEnvironment = (pgUrl, testEnvironment')

  -- Setup and track tables
  for_ tables $ \table -> do
    createTable testEnvironment table
    insertTable testEnvironment table
    dc_pg_track_table testEnvironment table

  {- TODO:

  -- Setup relationships
  for_ tables $ \table -> do
    Schema.trackObjectRelationships table testEnvironment
    Schema.trackArrayRelationships table testEnvironment
    -}

  action testEnvironment'

dc_pg_add_source ::
  ( Has Logger env,
    Has PostgresServerUrl env,
    Has FreshPostgresDb env,
    Has DcPgAgentKind env,
    Has HgeServerInstance env
  ) =>
  env ->
  Text ->
  IO ()
dc_pg_add_source env sourceName = do
  let pgServerUrl = getter env
      pgDb = getter env
      pgUrl = mkFreshDbConnectionString pgServerUrl pgDb
      agentKind = getDcPgAgentKind $ getter env

  _res <-
    hgePost
      env
      200
      "/v1/metadata"
      []
      [interpolateYaml|
          args:
            configuration:
              connection: #{getPostgresServerUrl pgUrl}
            name: #{sourceName}
            kind: #{agentKind}
          type: #{agentKind}_add_source
        |]

  -- TODO assert that res is a success result
  return ()

dc_pg_track_table ::
  ( HasCallStack,
    Has DcPostgresSource testEnvironment,
    Has HgeServerInstance testEnvironment,
    Has SchemaName testEnvironment,
    Has Logger testEnvironment
  ) =>
  testEnvironment ->
  Schema.Table ->
  IO ()
dc_pg_track_table testEnvironment (Schema.Table {tableName, tableColumns}) = do
  let DcPostgresSource {..} = getter testEnvironment
      schemaName = getter @SchemaName testEnvironment
      column_config = columnsConfig tableColumns
      agentKind = getDcPgAgentKind $ dcPostgresSourceAgentKind
  _ <-
    hgePost
      testEnvironment
      200
      "/v1/metadata"
      []
      [interpolateYaml|
            type: #{agentKind}_track_table
            args:
              source: #{dcPostgresSourceName}
              table:
                - #{schemaName}
                - #{tableName}
              configuration:
                column_config: #{column_config}
          |]

  return ()
  where
    columnsConfig :: [Schema.Column] -> J.Value
    columnsConfig = J.object . mapMaybe columnConfig

    columnConfig :: Schema.Column -> Maybe J.Pair
    columnConfig col = do
      alias <- Schema.columnGqlAlias col
      return $
        ( K.fromText $ Schema.columnName col,
          [yaml|
        custom_name: *alias
        |]
        )
