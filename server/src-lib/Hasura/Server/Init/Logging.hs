module Hasura.Server.Init.Logging
  ( censorQuery,
    updateQuery,
    censorURI,
    mkGenericLog,
    mkGenericStrLog,
    connInfoToLog,
    serveOptsToLog,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson qualified as J
import Data.ByteString.Char8 qualified as Char8
import Data.HashSet qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Database.PG.Query qualified as Q
import Hasura.GraphQL.Schema.Options qualified as Options
import Hasura.Logging qualified as L
import Hasura.Prelude
import Hasura.Server.Auth qualified as Auth
import Hasura.Server.Init.Config qualified as Config
import Hasura.Server.Logging qualified as Logging
import Network.HTTP.Types.URI qualified as URI
import Network.URI qualified as URI
import Network.WebSockets qualified as WS

--------------------------------------------------------------------------------

censorQueryItem :: T.Text -> URI.QueryItem -> URI.QueryItem
censorQueryItem sensitive (key, Just _) | key == TE.encodeUtf8 sensitive = (key, Just "...")
censorQueryItem _ qi = qi

censorQuery :: Text -> URI.Query -> URI.Query
censorQuery sensitive = fmap (censorQueryItem sensitive)

updateQuery :: (URI.Query -> URI.Query) -> URI.URI -> URI.URI
updateQuery f uri =
  let queries = URI.parseQuery $ Char8.pack $ URI.uriQuery uri
   in uri {URI.uriQuery = Char8.unpack (URI.renderQuery True $ f queries)}

censorURI :: Text -> URI.URI -> URI.URI
censorURI sensitive uri = updateQuery (censorQuery sensitive) uri

-- | TODO(SOLOMON): Haddock
-- Init logging related
connInfoToLog :: Q.ConnInfo -> Logging.StartupLog
connInfoToLog connInfo =
  Logging.StartupLog L.LevelInfo "postgres_connection" infoVal
  where
    Q.ConnInfo retries details = connInfo
    infoVal = case details of
      Q.CDDatabaseURI uri -> mkDBUriLog $ T.unpack $ bsToTxt uri
      Q.CDOptions co ->
        J.object
          [ "host" J..= Q.connHost co,
            "port" J..= Q.connPort co,
            "user" J..= Q.connUser co,
            "database" J..= Q.connDatabase co,
            "retries" J..= retries
          ]

    mkDBUriLog uri =
      case show . censorURI "sslpassword" <$> URI.parseURI uri of
        Nothing ->
          J.object
            ["error" J..= ("parsing database url failed" :: String)]
        Just s ->
          J.object
            [ "retries" J..= retries,
              "database_url" J..= s
            ]

-- | TODO(SOLOMON): Haddock
serveOptsToLog :: J.ToJSON (L.EngineLogType impl) => Config.ServeOptions impl -> Logging.StartupLog
serveOptsToLog so =
  Logging.StartupLog L.LevelInfo "server_configuration" infoVal
  where
    infoVal =
      J.object
        [ "port" J..= Config.soPort so,
          "server_host" J..= show (Config.soHost so),
          "transaction_isolation" J..= show (Config.soTxIso so),
          "admin_secret_set" J..= not (Set.null (Config.soAdminSecret so)),
          "auth_hook" J..= (Auth.ahUrl <$> Config.soAuthHook so),
          "auth_hook_mode" J..= (show . Auth.ahType <$> Config.soAuthHook so),
          "jwt_secret" J..= (J.toJSON <$> Config.soJwtSecret so),
          "unauth_role" J..= Config.soUnAuthRole so,
          "cors_config" J..= Config.soCorsConfig so,
          "enable_console" J..= Config.soEnableConsole so,
          "console_assets_dir" J..= Config.soConsoleAssetsDir so,
          "enable_telemetry" J..= Config.soEnableTelemetry so,
          "use_prepared_statements" J..= (Q.cpAllowPrepare . Config.soConnParams) so,
          "stringify_numeric_types" J..= case Config.soStringifyNum so of
            Options.StringifyNumbers -> True
            Options.Don'tStringifyNumbers -> False,
          "v1-boolean-null-collapse" J..= Config.soDangerousBooleanCollapse so,
          "enabled_apis" J..= Config.soEnabledAPIs so,
          "live_query_options" J..= Config.soLiveQueryOpts so,
          "enable_allowlist" J..= Config.soEnableAllowlist so,
          "enabled_log_types" J..= Config.soEnabledLogTypes so,
          "log_level" J..= Config.soLogLevel so,
          "remote_schema_permissions" J..= Config.soEnableRemoteSchemaPermissions so,
          "websocket_compression_options" J..= show (WS.connectionCompressionOptions . Config.soConnectionOptions $ so),
          "websocket_keep_alive" J..= show (Config.soWebSocketKeepAlive so),
          "infer_function_permissions" J..= Config.soInferFunctionPermissions so,
          "enable_maintenance_mode" J..= Config.soEnableMaintenanceMode so,
          "experimental_features" J..= Config.soExperimentalFeatures so,
          "events_fetch_batch_size" J..= Config.soEventsFetchBatchSize so,
          "graceful_shutdown_timeout" J..= Config.soGracefulShutdownTimeout so,
          "websocket_connection_init_timeout" J..= show (Config.soWebSocketConnectionInitTimeout so),
          "enable_metadata_query_logging" J..= Config.soEnableMetadataQueryLogging so
        ]

mkGenericStrLog :: L.LogLevel -> Text -> String -> Logging.StartupLog
mkGenericStrLog logLevel k msg =
  Logging.StartupLog logLevel k $ J.toJSON msg

mkGenericLog :: (J.ToJSON a) => L.LogLevel -> Text -> a -> Logging.StartupLog
mkGenericLog logLevel k msg =
  Logging.StartupLog logLevel k $ J.toJSON msg
