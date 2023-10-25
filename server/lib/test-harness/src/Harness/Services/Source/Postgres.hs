{-# LANGUAGE QuasiQuotes #-}

-- | This module houses low-level functions and types to help access and work
-- with Postgres sources.
module Harness.Services.Source.Postgres
  ( withPostgresSource,
    withPostgresSourceCustomized,
    PostgresSource (..),
    PostgresSourceCustomization (..),
    NamingConvention (..),
    withPostgresSchema,
  )
where

import Control.Lens
import Data.Aeson qualified as J
import Data.Aeson.Key qualified as K
import Data.Aeson.Lens
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
          pg_add_source (freshDb, env) sourceName Nothing
          -- TODO assert that res is a success result
          -- TODO: use 'managed'?
          action (PostgresSource sourceName freshDb pgUrl, env)
      )

data PostgresSourceCustomization = PostgresSourceCustomization
  { pscNamingConvention :: Maybe NamingConvention
  }

data NamingConvention = HasuraDefault | GraphQLDefault
  deriving (Eq, Ord, Enum, Show)

withPostgresSourceCustomized ::
  ( Has Logger testEnvironment,
    Has PostgresServerUrl testEnvironment,
    Has HgeServerInstance testEnvironment
  ) =>
  Text ->
  PostgresSourceCustomization ->
  SpecWith (PostgresSource, testEnvironment) ->
  SpecWith testEnvironment
withPostgresSourceCustomized sourceName customization =
  describe "Postgres"
    . withFreshPostgresDb
    . ( aroundWith \action (freshDb, env) -> do
          liftIO $ testLogMessage env (logHarness ("adding source.." :: Text))
          let pgUrl = getter env
          pg_add_source (freshDb, env) sourceName (Just customization)
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

  -- Setup relationships
  for_ tables $ \table -> do
    trackObjectRelationships table testEnvironment
    trackArrayRelationships table testEnvironment

  action testEnvironment'

trackObjectRelationships ::
  ( Has PostgresSource env,
    Has Logger env,
    Has SchemaName env,
    HasCallStack,
    Has HgeServerInstance env
  ) =>
  Schema.Table ->
  env ->
  IO ()
trackObjectRelationships (Schema.Table {tableName, tableReferences {-, tableManualRelationships-}}) env = do
  let localSchema = getter @SchemaName env
      source = postgresSourceName $ getter @PostgresSource env
      tableField = J.object ["schema" J..= J.String (unSchemaName localSchema), "name" J..= J.String tableName]

  for_ tableReferences $ \ref@Schema.Reference {referenceLocalColumn} -> do
    let relationshipName = mkObjectRelationshipName ref
    hgePost
      env
      200
      "/v1/metadata"
      []
      [yaml|
        type: "pg_create_object_relationship"
        args:
          source: *source
          table: *tableField
          name: *relationshipName
          using:
            foreign_key_constraint_on: *referenceLocalColumn
      |]

-- | Helper to create the object relationship name
mkObjectRelationshipName :: Schema.Reference -> Text
mkObjectRelationshipName Schema.Reference {referenceLocalColumn, referenceTargetTable, referenceTargetColumn, referenceTargetQualifiers} =
  let columnName = case resolveReferenceSchema referenceTargetQualifiers of
        Just (SchemaName targetSchema) -> targetSchema <> "_" <> referenceTargetColumn
        Nothing -> referenceTargetColumn
   in referenceTargetTable <> "_by_" <> referenceLocalColumn <> "_to_" <> columnName

-- | Unified track array relationships
trackArrayRelationships ::
  ( HasCallStack,
    Has HgeServerInstance env,
    Has SchemaName env,
    Has PostgresSource env,
    Has Logger env
  ) =>
  Schema.Table ->
  env ->
  IO ()
trackArrayRelationships (Schema.Table {tableName, tableReferences}) env = do
  let localSchema = getter @SchemaName env
      source = postgresSourceName $ getter @PostgresSource env
      tableField = J.object ["schema" J..= J.String (unSchemaName localSchema), "name" J..= J.String tableName]

  for_ tableReferences
    $ \Schema.Reference
         { referenceLocalColumn,
           referenceTargetTable,
           referenceTargetColumn,
           referenceTargetQualifiers
         } -> do
        let targetSchema = localSchema
            relationshipName = mkArrayRelationshipName tableName referenceTargetColumn referenceLocalColumn referenceTargetQualifiers
            targetTableField =
              J.object
                [ "schema" J..= J.String (unSchemaName targetSchema),
                  "name" J..= J.String referenceTargetTable
                ]

        hgePost
          env
          200
          "/v1/metadata"
          []
          [yaml|
        type: pg_create_array_relationship
        args:
          source: *source
          table: *targetTableField
          name: *relationshipName
          using:
            foreign_key_constraint_on:
              table: *tableField
              column: *referenceLocalColumn
      |]

-- | Helper to create the array relationship name
mkArrayRelationshipName :: Text -> Text -> Text -> [Text] -> Text
mkArrayRelationshipName tableName referenceLocalColumn referenceTargetColumn referenceTargetQualifiers =
  let columnName = case resolveReferenceSchema referenceTargetQualifiers of
        Just (SchemaName targetSchema) -> targetSchema <> "_" <> referenceTargetColumn
        Nothing -> referenceTargetColumn
   in tableName <> "s_by_" <> referenceLocalColumn <> "_to_" <> columnName

pg_add_source ::
  ( Has Logger env,
    Has PostgresServerUrl env,
    Has FreshPostgresDb env,
    Has HgeServerInstance env
  ) =>
  env ->
  Text ->
  Maybe PostgresSourceCustomization ->
  IO ()
pg_add_source env sourceName maybeCustomization = do
  let payload = pg_add_source_data env sourceName maybeCustomization

  _res <-
    hgePost
      env
      200
      "/v1/metadata"
      []
      payload

  -- TODO assert that res is a success result
  return ()

pg_add_source_data ::
  ( Has Logger env,
    Has PostgresServerUrl env,
    Has FreshPostgresDb env,
    Has HgeServerInstance env
  ) =>
  env ->
  Text ->
  Maybe PostgresSourceCustomization ->
  J.Value
pg_add_source_data env sourceName maybeCustomization =
  let pgServerUrl = getter env
      pgDb = getter env
      pgUrl = mkFreshDbConnectionString pgServerUrl pgDb

      configuration =
        [yaml|
              connection_info:
                database_url: *pgUrl
                |]

      customizationObject :: PostgresSourceCustomization -> J.Value
      customizationObject
        PostgresSourceCustomization
          { pscNamingConvention = maybeNamingConvention
          } =
          J.Object mempty
            -- Set naming conventions if any is given.
            & atKey "naming_convention" .~ (maybeNamingConvention <&> namingConventionObject)

      namingConventionObject :: NamingConvention -> J.Value
      namingConventionObject HasuraDefault = "hasura-default"
      namingConventionObject GraphQLDefault = "graphql-default"

      args =
        [yaml|
            configuration: *configuration
            name: *sourceName
            |]
          -- Set the "customization" field if any is given.
          & atKey "customization" .~ (maybeCustomization <&> customizationObject)
   in [yaml|
          args: *args
          type: pg_add_source
        |]

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
    hgePostMetadata
      testEnvironment
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
