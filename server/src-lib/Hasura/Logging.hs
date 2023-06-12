{-# LANGUAGE NumericUnderscores #-}
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
    UnhandledInternalErrorLog (..),
    mkLogger,
    nullLogger,
    LoggerCtx (..),
    mkLoggerCtx,
    cleanLoggerCtx,
    eventTriggerLogType,
    eventTriggerProcessLogType,
    scheduledTriggerLogType,
    scheduledTriggerProcessLogType,
    cronEventGeneratorProcessType,
    sourceCatalogMigrationLogType,
    EnabledLogTypes (..),
    defaultEnabledEngineLogTypes,
    isEngineLogTypeEnabled,
    readLogTypes,
    getFormattedTime,

    -- * Debounced stats logger
    createStatsLogger,
    closeStatsLogger,
    logStats,

    -- * Other internal logs
    StoredIntrospectionLog (..),
    StoredIntrospectionStorageLog (..),
  )
where

import Control.AutoUpdate qualified as Auto
import Control.Exception (ErrorCall (ErrorCallWithLocation), catch)
import Control.FoldDebounce qualified as FDebounce
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
import Data.String (fromString)
import Data.Text qualified as T
import Data.Time.Clock qualified as Time
import Data.Time.Format qualified as Format
import Data.Time.LocalTime qualified as Time
import Hasura.Base.Error (QErr)
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
  | ELTExecutionLog
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
    ELTExecutionLog -> "execution-log"
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
  | ILTUnhandledInternalError
  | ILTEventTrigger
  | ILTEventTriggerProcess
  | ILTScheduledTrigger
  | ILTScheduledTriggerProcess
  | ILTCronEventGeneratorProcess
  | -- | internal logs for the websocket server
    ILTWsServer
  | ILTPgClient
  | -- | log type for logging metadata related actions; currently used in logging inconsistent metadata
    ILTMetadata
  | ILTTelemetry
  | ILTSchemaSync
  | ILTSourceCatalogMigration
  | ILTStoredIntrospection
  | ILTStoredIntrospectionStorage
  deriving (Show, Eq, Generic)

instance Hashable InternalLogTypes

instance Witch.From InternalLogTypes Text where
  from = \case
    ILTUnstructured -> "unstructured"
    ILTUnhandledInternalError -> "unhandled-internal-error"
    ILTEventTrigger -> "event-trigger"
    ILTEventTriggerProcess -> "event-trigger-process"
    ILTScheduledTrigger -> "scheduled-trigger"
    ILTScheduledTriggerProcess -> "scheduled-trigger-process"
    ILTCronEventGeneratorProcess -> "cron-event-generator-process"
    ILTWsServer -> "ws-server"
    ILTPgClient -> "pg-client"
    ILTMetadata -> "metadata"
    ILTTelemetry -> "telemetry-log"
    ILTSchemaSync -> "schema-sync"
    ILTSourceCatalogMigration -> "source-catalog-migration"
    ILTStoredIntrospection -> "stored-introspection"
    ILTStoredIntrospectionStorage -> "stored-introspection-storage"

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
    ELTExecutionLog,
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

deriving instance (Show (EngineLogType impl)) => Show (EngineLog impl)

deriving instance (Eq (EngineLogType impl)) => Eq (EngineLog impl)

-- Empty splice to bring all the above definitions in scope.
--
-- TODO: Restructure the code so that we can avoid this.
$(pure [])

instance (J.ToJSON (EngineLogType impl)) => J.ToJSON (EngineLog impl) where
  toJSON = $(J.mkToJSON hasuraJSON ''EngineLog)

-- | Typeclass representing any data type that can be converted to @EngineLog@ for the purpose of
-- logging
class (EnabledLogTypes impl) => ToEngineLog a impl where
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

-- * Unhandled Internal Errors

-- | We expect situations where there are code paths that should not occur and we throw
--   an 'error' on this code paths. If our assumptions are incorrect and infact
--   these errors do occur, we want to log them.
newtype UnhandledInternalErrorLog = UnhandledInternalErrorLog ErrorCall

instance ToEngineLog UnhandledInternalErrorLog Hasura where
  toEngineLog (UnhandledInternalErrorLog (ErrorCallWithLocation err loc)) =
    ( LevelError,
      ELTInternal ILTUnhandledInternalError,
      J.object [("error", fromString err), ("location", fromString loc)]
    )

-- * LoggerSettings

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

-- | Creates a new 'LoggerCtx'.
--
-- The underlying 'LoggerSet' is bound to the 'ManagedT' context: when it exits,
-- the log will be flushed and cleared regardless of whether it was exited
-- properly or not ('ManagedT' uses 'bracket' underneath). This guarantees that
-- the logs will always be flushed, even in case of error, avoiding a repeat of
-- https://github.com/hasura/graphql-engine/issues/4772.
mkLoggerCtx ::
  (MonadIO io, MonadBaseControl IO io) =>
  LoggerSettings ->
  Set.HashSet (EngineLogType impl) ->
  ManagedT io (LoggerCtx impl)
mkLoggerCtx (LoggerSettings cacheTime tzM logLevel) enabledLogs = do
  loggerSet <- allocate acquire release
  timeGetter <- liftIO $ bool (pure $ getFormattedTime tzM) cachedTimeGetter cacheTime
  pure $ LoggerCtx loggerSet logLevel timeGetter enabledLogs
  where
    acquire = liftIO do
      FL.newStdoutLoggerSet FL.defaultBufSize
    release loggerSet = liftIO do
      FL.flushLogStr loggerSet
      FL.rmLoggerSet loggerSet
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
  when (logLevel >= serverLogLevel && isLogTypeEnabled enabledLogTypes logTy)
    $ liftIO
    $ FL.pushLogStrLn loggerSet
    $ FL.toLogStr (J.encode $ EngineLog localTime logLevel logTy logDet)

nullLogger :: Logger Hasura
nullLogger = Logger \_ -> pure ()

eventTriggerLogType :: EngineLogType Hasura
eventTriggerLogType = ELTInternal ILTEventTrigger

eventTriggerProcessLogType :: EngineLogType Hasura
eventTriggerProcessLogType = ELTInternal ILTEventTriggerProcess

scheduledTriggerLogType :: EngineLogType Hasura
scheduledTriggerLogType = ELTInternal ILTScheduledTrigger

scheduledTriggerProcessLogType :: EngineLogType Hasura
scheduledTriggerProcessLogType = ELTInternal ILTScheduledTriggerProcess

cronEventGeneratorProcessType :: EngineLogType Hasura
cronEventGeneratorProcessType = ELTInternal ILTCronEventGeneratorProcess

sourceCatalogMigrationLogType :: EngineLogType Hasura
sourceCatalogMigrationLogType = ELTInternal ILTSourceCatalogMigration

-- | Emit when stored introspection is used
data StoredIntrospectionLog = StoredIntrospectionLog
  { silMessage :: Text,
    -- | upstream data source errors
    silSourceError :: QErr
  }
  deriving stock (Generic)

instance J.ToJSON StoredIntrospectionLog where
  toJSON = J.genericToJSON hasuraJSON

instance ToEngineLog StoredIntrospectionLog Hasura where
  toEngineLog siLog =
    (LevelInfo, ELTInternal ILTStoredIntrospection, J.toJSON siLog)

-- | Logs related to errors while interacting with the stored introspection
-- storage
data StoredIntrospectionStorageLog = StoredIntrospectionStorageLog
  { sislMessage :: Text,
    sislError :: QErr
  }
  deriving stock (Generic)

instance J.ToJSON StoredIntrospectionStorageLog where
  toJSON = J.genericToJSON hasuraJSON

instance ToEngineLog StoredIntrospectionStorageLog Hasura where
  toEngineLog sisLog =
    (LevelInfo, ELTInternal ILTStoredIntrospectionStorage, J.toJSON sisLog)

-- | A logger useful for accumulating  and logging stats, in tight polling loops. It also
-- debounces to not flood with excessive logs. Use @'logStats' to record statistics for logging.
createStatsLogger ::
  forall m stats impl.
  ( MonadIO m,
    ToEngineLog stats impl,
    Monoid stats
  ) =>
  Logger impl ->
  m (FDebounce.Trigger stats stats)
createStatsLogger hasuraLogger =
  liftIO $ FDebounce.new debounceArgs debounceOpts
  where
    logDelay :: Int
    logDelay =
      -- Accumulate stats occurred within 10 minutes and log once.
      10 * 60 * 1000_000 -- 10 minutes
    debounceArgs :: FDebounce.Args stats stats
    debounceArgs =
      FDebounce.Args
        { FDebounce.cb = unLogger hasuraLogger, -- Log using the Hasura logger
          FDebounce.fold = (<>),
          FDebounce.init = mempty
        }

    debounceOpts :: FDebounce.Opts stats stats
    debounceOpts = FDebounce.def {FDebounce.delay = logDelay}

-- Orphan instance. Required for @'closeStatsLogger'.
instance (EnabledLogTypes impl) => ToEngineLog (FDebounce.OpException, EngineLogType impl) impl where
  toEngineLog (opException, logType) =
    let errorMessage :: Text
        errorMessage = case opException of
          FDebounce.AlreadyClosedException -> "already closed"
          FDebounce.UnexpectedClosedException _someException -> "closed unexpectedly"
     in (LevelWarn, logType, J.object ["message" J..= ("cannot close fetched events stats logger: " <> errorMessage)])

-- | Safely close the statistics logger. When occurred, exception is logged.
closeStatsLogger :: (MonadIO m, EnabledLogTypes impl) => EngineLogType impl -> Logger impl -> FDebounce.Trigger stats stats -> m ()
closeStatsLogger logType (Logger hasuraLogger) debounceLogger =
  liftIO $ catch (FDebounce.close debounceLogger) $ \(e :: FDebounce.OpException) -> hasuraLogger (e, logType)

-- | This won't log the given stats immediately.
-- The stats are accumulated over the specific timeframe and logged only once.
-- See @'createStatsLogger' for more details.
logStats :: (MonadIO m) => FDebounce.Trigger stats stats -> stats -> m ()
logStats debounceTrigger = liftIO . FDebounce.send debounceTrigger
