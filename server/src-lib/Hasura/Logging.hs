{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Hasura.Logging
  ( LoggerSettings (..),
    defaultLoggerSettings,
    EngineLogType (..),
    Hasura,
    InternalLogTypes (..),
    EngineLog (..),
    userAllowedLogTypes,
    ToEngineLog (..),
    debugT,
    debugBS,
    debugLBS,
    UnstructuredLog (..),
    Logger (..),
    LogLevel (..),
    mkLogger,
    nullLogger,
    LoggerCtx (..),
    mkLoggerCtx,
    cleanLoggerCtx,
    eventTriggerLogType,
    scheduledTriggerLogType,
    sourceCatalogMigrationLogType,
    EnabledLogTypes (..),
    defaultEnabledEngineLogTypes,
    isEngineLogTypeEnabled,
    readLogTypes,
    getFormattedTime,
  )
where

import Control.AutoUpdate qualified as Auto
import Control.Monad.Trans.Control
import Control.Monad.Trans.Managed (ManagedT (..), allocate)
import Data.Aeson qualified as J
import Data.Aeson.TH qualified as J
import Data.Aeson.Types qualified as J
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BLC
import Data.HashSet qualified as Set
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.SerializableBlob qualified as SB
import Data.Text qualified as T
import Data.Time.Clock qualified as Time
import Data.Time.Format qualified as Format
import Data.Time.LocalTime qualified as Time
import Hasura.Prelude
import System.Log.FastLogger qualified as FL
import Witch qualified

newtype FormattedTime = FormattedTime {_unFormattedTime :: Text}
  deriving (Show, Eq, J.ToJSON)

-- | Typeclass representing any type which can be parsed into a list of enabled log types, and has a @Set@
-- of default enabled log types, and can find out if a log type is enabled
class (Eq (EngineLogType impl), Hashable (EngineLogType impl)) => EnabledLogTypes impl where
  parseEnabledLogTypes :: String -> Either String [EngineLogType impl]
  defaultEnabledLogTypes :: Set.HashSet (EngineLogType impl)
  isLogTypeEnabled :: Set.HashSet (EngineLogType impl) -> EngineLogType impl -> Bool

-- | A family of EngineLogType types
data family EngineLogType impl

data Hasura

data instance EngineLogType Hasura
  = ELTHttpLog
  | ELTWebsocketLog
  | ELTWebhookLog
  | ELTQueryLog
  | ELTStartup
  | ELTLivequeryPollerLog
  | ELTActionHandler
  | ELTDataConnectorLog
  | ELTJwkRefreshLog
  | -- internal log types
    ELTInternal !InternalLogTypes
  deriving (Show, Eq, Generic)

instance Hashable (EngineLogType Hasura)

instance Witch.From (EngineLogType Hasura) Text where
  from = \case
    ELTHttpLog -> "http-log"
    ELTWebsocketLog -> "websocket-log"
    ELTWebhookLog -> "webhook-log"
    ELTQueryLog -> "query-log"
    ELTStartup -> "startup"
    ELTLivequeryPollerLog -> "livequery-poller-log"
    ELTActionHandler -> "action-handler-log"
    ELTDataConnectorLog -> "data-connector-log"
    ELTJwkRefreshLog -> "jwk-refresh-log"
    ELTInternal t -> Witch.from t

instance J.ToJSON (EngineLogType Hasura) where
  toJSON = J.String . Witch.into @Text

instance J.FromJSON (EngineLogType Hasura) where
  parseJSON = J.withText "log-type" $ \s ->
    let logTypeText = T.toLower $ T.strip s
        logTypeMaybe = Map.lookup logTypeText allowedLogTypeMapping
     in logTypeMaybe `onNothing` failure
    where
      allowedLogTypeMapping :: Map Text (EngineLogType Hasura)
      allowedLogTypeMapping =
        Map.fromList $ (\lt -> (Witch.into @Text lt, lt)) <$> userAllowedLogTypes

      failure :: J.Parser (EngineLogType Hasura)
      failure =
        fail $ "Valid list of comma-separated log types: " <> BLC.unpack (J.encode userAllowedLogTypes)

data InternalLogTypes
  = -- | mostly for debug logs - see @debugT@, @debugBS@ and @debugLBS@ functions
    ILTUnstructured
  | ILTEventTrigger
  | ILTScheduledTrigger
  | -- | internal logs for the websocket server
    ILTWsServer
  | ILTPgClient
  | -- | log type for logging metadata related actions; currently used in logging inconsistent metadata
    ILTMetadata
  | ILTTelemetry
  | ILTSchemaSync
  | ILTSourceCatalogMigration
  deriving (Show, Eq, Generic)

instance Hashable InternalLogTypes

instance Witch.From InternalLogTypes Text where
  from = \case
    ILTUnstructured -> "unstructured"
    ILTEventTrigger -> "event-trigger"
    ILTScheduledTrigger -> "scheduled-trigger"
    ILTWsServer -> "ws-server"
    ILTPgClient -> "pg-client"
    ILTMetadata -> "metadata"
    ILTTelemetry -> "telemetry-log"
    ILTSchemaSync -> "schema-sync"
    ILTSourceCatalogMigration -> "source-catalog-migration"

instance J.ToJSON InternalLogTypes where
  toJSON = J.String . Witch.into @Text

-- the default enabled log-types
defaultEnabledEngineLogTypes :: Set.HashSet (EngineLogType Hasura)
defaultEnabledEngineLogTypes =
  Set.fromList [ELTStartup, ELTHttpLog, ELTWebhookLog, ELTWebsocketLog, ELTJwkRefreshLog]

isEngineLogTypeEnabled :: Set.HashSet (EngineLogType Hasura) -> EngineLogType Hasura -> Bool
isEngineLogTypeEnabled enabledTypes logTy = case logTy of
  ELTInternal _ -> True
  _ -> logTy `Set.member` enabledTypes

readLogTypes :: String -> Either String [EngineLogType Hasura]
readLogTypes = mapM (J.eitherDecodeStrict' . quote . txtToBs) . T.splitOn "," . T.pack
  where
    quote x = "\"" <> x <> "\""

instance EnabledLogTypes Hasura where
  parseEnabledLogTypes = readLogTypes
  defaultEnabledLogTypes = defaultEnabledEngineLogTypes
  isLogTypeEnabled = isEngineLogTypeEnabled

-- log types that can be set by the user
userAllowedLogTypes :: [EngineLogType Hasura]
userAllowedLogTypes =
  [ ELTStartup,
    ELTHttpLog,
    ELTWebhookLog,
    ELTWebsocketLog,
    ELTQueryLog,
    ELTLivequeryPollerLog,
    ELTActionHandler,
    ELTDataConnectorLog,
    ELTJwkRefreshLog
  ]

data LogLevel
  = LevelDebug
  | LevelInfo
  | LevelWarn
  | LevelError
  | LevelOther Text
  deriving (Show, Eq, Ord)

instance J.ToJSON LogLevel where
  toJSON =
    J.toJSON . \case
      LevelDebug -> "debug"
      LevelInfo -> "info"
      LevelWarn -> "warn"
      LevelError -> "error"
      LevelOther t -> t

data EngineLog impl = EngineLog
  { _elTimestamp :: !FormattedTime,
    _elLevel :: !LogLevel,
    _elType :: !(EngineLogType impl),
    _elDetail :: !J.Value
  }

deriving instance Show (EngineLogType impl) => Show (EngineLog impl)

deriving instance Eq (EngineLogType impl) => Eq (EngineLog impl)

-- Empty splice to bring all the above definitions in scope.
--
-- TODO: Restructure the code so that we can avoid this.
$(pure [])

instance J.ToJSON (EngineLogType impl) => J.ToJSON (EngineLog impl) where
  toJSON = $(J.mkToJSON hasuraJSON ''EngineLog)

-- | Typeclass representing any data type that can be converted to @EngineLog@ for the purpose of
-- logging
class EnabledLogTypes impl => ToEngineLog a impl where
  toEngineLog :: a -> (LogLevel, EngineLogType impl, J.Value)

data UnstructuredLog = UnstructuredLog {_ulLevel :: !LogLevel, _ulPayload :: !SB.SerializableBlob}
  deriving (Show)

debugT :: Text -> UnstructuredLog
debugT = UnstructuredLog LevelDebug . SB.fromText

debugBS :: B.ByteString -> UnstructuredLog
debugBS = UnstructuredLog LevelDebug . SB.fromBS

debugLBS :: BL.ByteString -> UnstructuredLog
debugLBS = UnstructuredLog LevelDebug . SB.fromLBS

instance ToEngineLog UnstructuredLog Hasura where
  toEngineLog (UnstructuredLog level t) =
    (level, ELTInternal ILTUnstructured, J.toJSON t)

data LoggerCtx impl = LoggerCtx
  { _lcLoggerSet :: !FL.LoggerSet,
    _lcLogLevel :: !LogLevel,
    _lcTimeGetter :: !(IO FormattedTime),
    _lcEnabledLogTypes :: !(Set.HashSet (EngineLogType impl))
  }

data LoggerSettings = LoggerSettings
  { -- | should current time be cached (refreshed every sec)
    _lsCachedTimestamp :: !Bool,
    _lsTimeZone :: !(Maybe Time.TimeZone),
    _lsLevel :: !LogLevel
  }
  deriving (Show, Eq)

defaultLoggerSettings :: Bool -> LogLevel -> LoggerSettings
defaultLoggerSettings isCached =
  LoggerSettings isCached Nothing

getFormattedTime :: Maybe Time.TimeZone -> IO FormattedTime
getFormattedTime tzM = do
  tz <- onNothing tzM Time.getCurrentTimeZone
  t <- Time.getCurrentTime
  let zt = Time.utcToZonedTime tz t
  return $ FormattedTime $ T.pack $ formatTime zt
  where
    formatTime = Format.formatTime Format.defaultTimeLocale format
    format = "%FT%H:%M:%S%3Q%z"

-- format = Format.iso8601DateFormat (Just "%H:%M:%S")

mkLoggerCtx ::
  (MonadIO io, MonadBaseControl IO io) =>
  LoggerSettings ->
  Set.HashSet (EngineLogType impl) ->
  ManagedT io (LoggerCtx impl)
mkLoggerCtx (LoggerSettings cacheTime tzM logLevel) enabledLogs = do
  loggerSet <-
    allocate
      (liftIO $ FL.newStdoutLoggerSet FL.defaultBufSize)
      (liftIO . FL.rmLoggerSet)
  timeGetter <- liftIO $ bool (return $ getFormattedTime tzM) cachedTimeGetter cacheTime
  return $ LoggerCtx loggerSet logLevel timeGetter enabledLogs
  where
    cachedTimeGetter =
      Auto.mkAutoUpdate
        Auto.defaultUpdateSettings
          { Auto.updateAction = getFormattedTime tzM
          }

cleanLoggerCtx :: LoggerCtx a -> IO ()
cleanLoggerCtx =
  FL.rmLoggerSet . _lcLoggerSet

-- See Note [Existentially Quantified Types]
newtype Logger impl = Logger {unLogger :: forall a m. (ToEngineLog a impl, MonadIO m) => a -> m ()}

mkLogger :: (J.ToJSON (EngineLogType impl)) => LoggerCtx impl -> Logger impl
mkLogger (LoggerCtx loggerSet serverLogLevel timeGetter enabledLogTypes) = Logger $ \l -> do
  localTime <- liftIO timeGetter
  let (logLevel, logTy, logDet) = toEngineLog l
  when (logLevel >= serverLogLevel && isLogTypeEnabled enabledLogTypes logTy) $
    liftIO $
      FL.pushLogStrLn loggerSet $
        FL.toLogStr (J.encode $ EngineLog localTime logLevel logTy logDet)

nullLogger :: Logger Hasura
nullLogger = Logger \_ -> pure ()

eventTriggerLogType :: EngineLogType Hasura
eventTriggerLogType = ELTInternal ILTEventTrigger

scheduledTriggerLogType :: EngineLogType Hasura
scheduledTriggerLogType = ELTInternal ILTScheduledTrigger

sourceCatalogMigrationLogType :: EngineLogType Hasura
sourceCatalogMigrationLogType = ELTInternal ILTSourceCatalogMigration
