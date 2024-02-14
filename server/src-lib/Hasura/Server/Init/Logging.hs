-- | Helpful functions and types for generating log statements and URIs during
-- Options fetching and merging.
module Hasura.Server.Init.Logging
  ( -- * URI/QueryParam Manipulation
    censorQuery,
    updateQuery,
    censorURI,

    -- * Log Construction
    mkGenericLog,
    connInfoToLog,
    serveOptsToLog,
    StartupTimeInfo (..),
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON, (.=))
import Data.Aeson qualified as J
import Data.ByteString.Char8 qualified as Char8
import Data.HashSet qualified as HashSet
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import Database.PG.Query qualified as Query
import Hasura.Logging qualified as Logging
import Hasura.Prelude
import Hasura.RQL.Types.Schema.Options qualified as Options
import Hasura.Server.Auth qualified as Auth
import Hasura.Server.Init.Config qualified as Config
import Hasura.Server.Logging qualified as Server.Logging
import Network.HTTP.Types.URI qualified as URI
import Network.URI qualified as URI
import Network.WebSockets qualified as WebSockets

--------------------------------------------------------------------------------

censorQueryItem :: Text -> URI.QueryItem -> URI.QueryItem
censorQueryItem sensitive (key, Just _) | key == Text.Encoding.encodeUtf8 sensitive = (key, Just "...")
censorQueryItem _ qi = qi

censorQuery :: Text -> URI.Query -> URI.Query
censorQuery sensitive = fmap (censorQueryItem sensitive)

updateQuery :: (URI.Query -> URI.Query) -> URI.URI -> URI.URI
updateQuery f uri =
  let queries = URI.parseQuery $ Char8.pack $ URI.uriQuery uri
   in uri {URI.uriQuery = Char8.unpack (URI.renderQuery True $ f queries)}

censorURI :: Text -> URI.URI -> URI.URI
censorURI sensitive uri = updateQuery (censorQuery sensitive) uri

-- | Generate a 'StartupLog' from the Postgres 'ConnInfo'.
connInfoToLog :: Query.ConnInfo -> Server.Logging.StartupLog
connInfoToLog connInfo =
  Server.Logging.StartupLog Logging.LevelInfo "postgres_connection" infoVal
  where
    Query.ConnInfo retries details = connInfo
    infoVal = case details of
      -- create a valid URI referencing the 'dynamic_from_file' metadata
      -- configuration the user must have used to get here:
      Query.CDDynamicDatabaseURI path -> mkDBUriLog $ "dynamic-from-file://" <> path
      Query.CDDatabaseURI uri -> mkDBUriLog $ Text.unpack $ bsToTxt uri
      Query.CDOptions co ->
        J.object
          [ "host" .= Query.connHost co,
            "port" .= Query.connPort co,
            "user" .= Query.connUser co,
            "database" .= Query.connDatabase co,
            "retries" .= retries
          ]

    mkDBUriLog uri =
      case show . censorURI "sslpassword" <$> URI.parseURI uri of
        Nothing ->
          J.object
            ["error" .= ("parsing database url failed" :: String)]
        Just s ->
          J.object
            [ "retries" .= retries,
              "database_url" .= s
            ]

-- | Generate a 'StartupLog' from the final 'ServeOptions'.
serveOptsToLog :: (ToJSON (Logging.EngineLogType impl)) => Config.ServeOptions impl -> Server.Logging.StartupLog
serveOptsToLog so =
  Server.Logging.StartupLog Logging.LevelInfo "server_configuration" infoVal
  where
    infoVal =
      J.object
        [ "port" .= Config.soPort so,
          "server_host" .= show (Config.soHost so),
          "transaction_isolation" .= show (Config.soTxIso so),
          "admin_secret_set" .= not (HashSet.null (Config.soAdminSecret so)),
          "auth_hook" .= (Auth.ahUrl <$> Config.soAuthHook so),
          "auth_hook_mode" .= (show . Auth.ahType <$> Config.soAuthHook so),
          "jwt_secret" .= (J.toJSON <$> Config.soJwtSecret so),
          "unauth_role" .= Config.soUnAuthRole so,
          "cors_config" .= Config.soCorsConfig so,
          "enable_console" .= Config.soConsoleStatus so,
          "console_assets_dir" .= Config.soConsoleAssetsDir so,
          "console_sentry_dsn" .= Config.soConsoleSentryDsn so,
          "enable_telemetry" .= Config.soEnableTelemetry so,
          "use_prepared_statements" .= (Query.cpAllowPrepare . Config.soConnParams) so,
          "stringify_numeric_types" .= case Config.soStringifyNum so of
            Options.StringifyNumbers -> True
            Options.Don'tStringifyNumbers -> False,
          "v1-boolean-null-collapse" .= Config.soDangerousBooleanCollapse so,
          "enabled_apis" .= Config.soEnabledAPIs so,
          "live_query_options" .= Config.soLiveQueryOpts so,
          "enable_allowlist" .= Config.isAllowListEnabled (Config.soEnableAllowList so),
          "enabled_log_types" .= Config.soEnabledLogTypes so,
          "log_level" .= Config.soLogLevel so,
          "remote_schema_permissions" .= Config.soEnableRemoteSchemaPermissions so,
          "websocket_compression_options" .= show (WebSockets.connectionCompressionOptions . Config.soConnectionOptions $ so),
          "websocket_keep_alive" .= show (Config.soWebSocketKeepAlive so),
          "infer_function_permissions" .= Config.soInferFunctionPermissions so,
          "enable_maintenance_mode" .= Config.soEnableMaintenanceMode so,
          "experimental_features" .= Config.soExperimentalFeatures so,
          "events_fetch_batch_size" .= Config.soEventsFetchBatchSize so,
          "graceful_shutdown_timeout" .= Config.soGracefulShutdownTimeout so,
          "websocket_connection_init_timeout" .= show (Config.soWebSocketConnectionInitTimeout so),
          "enable_metadata_query_logging" .= Config.soEnableMetadataQueryLogging so
        ]

mkGenericLog :: (ToJSON a) => Logging.LogLevel -> Text -> a -> Server.Logging.StartupLog
mkGenericLog logLevel k msg =
  Server.Logging.StartupLog logLevel k $ J.toJSON msg

data StartupTimeInfo = StartupTimeInfo
  { _stiMessage :: !Text,
    _stiTimeTaken :: !Double
  }

instance FromJSON StartupTimeInfo where
  parseJSON = J.withObject "StartupTimeInfo" \obj -> do
    _stiMessage <- obj J..: "message"
    _stiTimeTaken <- obj J..: "time_taken"
    pure StartupTimeInfo {..}

instance ToJSON StartupTimeInfo where
  toJSON StartupTimeInfo {..} =
    J.object ["message" J..= _stiMessage, "time_taken" J..= _stiTimeTaken]
