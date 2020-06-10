{-# LANGUAGE UndecidableInstances #-}

module Hasura.Logging
  ( LoggerSettings(..)
  , defaultLoggerSettings
  , EngineLogType(..)
  , Hasura
  , InternalLogTypes(..)
  , EngineLog(..)
  , userAllowedLogTypes
  , ToEngineLog(..)
  , debugT
  , debugBS
  , debugLBS
  , UnstructuredLog(..)
  , Logger (..)
  , LogLevel(..)
  , mkLogger
  , LoggerCtx(..)
  , mkLoggerCtx
  , cleanLoggerCtx
  , eventTriggerLogType
  , scheduledTriggerLogType
  , EnabledLogTypes (..)
  , defaultEnabledEngineLogTypes
  , isEngineLogTypeEnabled
  , readLogTypes
  ) where

import           Hasura.Prelude

import qualified Control.AutoUpdate         as Auto
import qualified Data.Aeson                 as J
import qualified Data.Aeson.Casing          as J
import qualified Data.Aeson.TH              as J
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.HashSet               as Set
import qualified Data.TByteString           as TBS
import qualified Data.Text                  as T
import qualified Data.Time.Clock            as Time
import qualified Data.Time.Format           as Format
import qualified Data.Time.LocalTime        as Time
import qualified System.Log.FastLogger      as FL


newtype FormattedTime
  = FormattedTime { _unFormattedTime :: Text }
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
  -- internal log types
  | ELTInternal !InternalLogTypes
  deriving (Show, Eq, Generic)

instance Hashable (EngineLogType Hasura)

instance J.ToJSON (EngineLogType Hasura) where
  toJSON = \case
    ELTHttpLog -> "http-log"
    ELTWebsocketLog -> "websocket-log"
    ELTWebhookLog -> "webhook-log"
    ELTQueryLog -> "query-log"
    ELTStartup -> "startup"
    ELTLivequeryPollerLog -> "livequery-poller-log"
    ELTInternal t -> J.toJSON t

instance J.FromJSON (EngineLogType Hasura) where
  parseJSON = J.withText "log-type" $ \s -> case T.toLower $ T.strip s of
    "startup" -> return ELTStartup
    "http-log" -> return ELTHttpLog
    "webhook-log" -> return ELTWebhookLog
    "websocket-log" -> return ELTWebsocketLog
    "query-log" -> return ELTQueryLog
    "livequery-poller-log" -> return ELTLivequeryPollerLog
    _ -> fail $ "Valid list of comma-separated log types: "
         <> BLC.unpack (J.encode userAllowedLogTypes)

data InternalLogTypes
 = ILTUnstructured
 -- ^ mostly for debug logs - see @debugT@, @debugBS@ and @debugLBS@ functions
 | ILTEventTrigger
 | ILTScheduledTrigger
 | ILTWsServer
 -- ^ internal logs for the websocket server
 | ILTPgClient
 | ILTMetadata
 -- ^ log type for logging metadata related actions; currently used in logging inconsistent metadata
 | ILTJwkRefreshLog
 | ILTTelemetry
 | ILTSchemaSyncThread
 deriving (Show, Eq, Generic)

instance Hashable InternalLogTypes

instance J.ToJSON InternalLogTypes where
  toJSON = \case
    ILTUnstructured -> "unstructured"
    ILTEventTrigger -> "event-trigger"
    ILTScheduledTrigger -> "scheduled-trigger"
    ILTWsServer -> "ws-server"
    ILTPgClient -> "pg-client"
    ILTMetadata -> "metadata"
    ILTJwkRefreshLog -> "jwk-refresh-log"
    ILTTelemetry -> "telemetry-log"
    ILTSchemaSyncThread -> "schema-sync-thread"

-- the default enabled log-types
defaultEnabledEngineLogTypes :: Set.HashSet (EngineLogType Hasura)
defaultEnabledEngineLogTypes =
  Set.fromList [ELTStartup, ELTHttpLog, ELTWebhookLog, ELTWebsocketLog]

isEngineLogTypeEnabled :: Set.HashSet (EngineLogType Hasura) -> EngineLogType Hasura -> Bool
isEngineLogTypeEnabled enabledTypes logTy = case logTy of
  ELTInternal _ -> True
  _             -> logTy `Set.member` enabledTypes


readLogTypes :: String -> Either String [EngineLogType Hasura]
readLogTypes = mapM (J.eitherDecodeStrict' . quote . txtToBs) . T.splitOn "," . T.pack
  where quote x = "\"" <> x <> "\""

instance EnabledLogTypes Hasura where
  parseEnabledLogTypes = readLogTypes
  defaultEnabledLogTypes = defaultEnabledEngineLogTypes
  isLogTypeEnabled = isEngineLogTypeEnabled

-- log types that can be set by the user
userAllowedLogTypes :: [EngineLogType Hasura]
userAllowedLogTypes =
  [ ELTStartup
  , ELTHttpLog
  , ELTWebhookLog
  , ELTWebsocketLog
  , ELTQueryLog
  , ELTLivequeryPollerLog
  ]

data LogLevel
  = LevelDebug
  | LevelInfo
  | LevelWarn
  | LevelError
  | LevelOther Text
  deriving (Show, Eq, Ord)

instance J.ToJSON LogLevel where
  toJSON = J.toJSON . \case
    LevelDebug   -> "debug"
    LevelInfo    -> "info"
    LevelWarn    -> "warn"
    LevelError   -> "error"
    LevelOther t -> t

data EngineLog impl
  = EngineLog
  { _elTimestamp :: !FormattedTime
  , _elLevel     :: !LogLevel
  , _elType      :: !(EngineLogType impl)
  , _elDetail    :: !J.Value
  }

deriving instance Show (EngineLogType impl) => Show (EngineLog impl)
deriving instance Eq (EngineLogType impl) => Eq (EngineLog impl)

-- empty splice to bring all the above definitions in scope
$(pure [])

instance J.ToJSON (EngineLogType impl) => J.ToJSON (EngineLog impl) where
  toJSON = $(J.mkToJSON (J.aesonDrop 3 J.snakeCase) ''EngineLog)

-- | Typeclass representing any data type that can be converted to @EngineLog@ for the purpose of
-- logging
class EnabledLogTypes impl => ToEngineLog a impl where
  toEngineLog :: a -> (LogLevel, EngineLogType impl, J.Value)


data UnstructuredLog
  = UnstructuredLog { _ulLevel :: !LogLevel, _ulPayload :: !TBS.TByteString }
  deriving (Show, Eq)

debugT :: Text -> UnstructuredLog
debugT = UnstructuredLog LevelDebug . TBS.fromText

debugBS :: B.ByteString -> UnstructuredLog
debugBS = UnstructuredLog LevelDebug . TBS.fromBS

debugLBS :: BL.ByteString -> UnstructuredLog
debugLBS = UnstructuredLog LevelDebug . TBS.fromLBS

instance ToEngineLog UnstructuredLog Hasura where
  toEngineLog (UnstructuredLog level t) =
    (level, ELTInternal ILTUnstructured, J.toJSON t)

data LoggerCtx impl
  = LoggerCtx
  { _lcLoggerSet       :: !FL.LoggerSet
  , _lcLogLevel        :: !LogLevel
  , _lcTimeGetter      :: !(IO FormattedTime)
  , _lcEnabledLogTypes :: !(Set.HashSet (EngineLogType impl))
  }

data LoggerSettings
  = LoggerSettings
  { _lsCachedTimestamp :: !Bool
  -- ^ should current time be cached (refreshed every sec)
  , _lsTimeZone        :: !(Maybe Time.TimeZone)
  , _lsLevel           :: !LogLevel
  } deriving (Show, Eq)


defaultLoggerSettings :: Bool -> LogLevel -> LoggerSettings
defaultLoggerSettings isCached =
  LoggerSettings isCached Nothing

getFormattedTime :: Maybe Time.TimeZone -> IO FormattedTime
getFormattedTime tzM = do
  tz <- maybe Time.getCurrentTimeZone return tzM
  t  <- Time.getCurrentTime
  let zt = Time.utcToZonedTime tz t
  return $ FormattedTime $ T.pack $ formatTime zt
  where
    formatTime = Format.formatTime Format.defaultTimeLocale format
    format = "%FT%H:%M:%S%3Q%z"
    -- format = Format.iso8601DateFormat (Just "%H:%M:%S")

mkLoggerCtx
  :: LoggerSettings
  -> Set.HashSet (EngineLogType impl)
  -> IO (LoggerCtx impl)
mkLoggerCtx (LoggerSettings cacheTime tzM logLevel) enabledLogs = do
  loggerSet <- FL.newStdoutLoggerSet FL.defaultBufSize
  timeGetter <- bool (return $ getFormattedTime tzM) cachedTimeGetter cacheTime
  return $ LoggerCtx loggerSet logLevel timeGetter enabledLogs
  where
    cachedTimeGetter =
      Auto.mkAutoUpdate Auto.defaultUpdateSettings {
        Auto.updateAction = getFormattedTime tzM
      }

cleanLoggerCtx :: LoggerCtx a -> IO ()
cleanLoggerCtx =
  FL.rmLoggerSet . _lcLoggerSet


newtype Logger impl
  = Logger { unLogger :: forall a m. (ToEngineLog a impl, MonadIO m) => a -> m () }

mkLogger :: LoggerCtx Hasura -> Logger Hasura
mkLogger (LoggerCtx loggerSet serverLogLevel timeGetter enabledLogTypes) = Logger $ \l -> do
  localTime <- liftIO timeGetter
  let (logLevel, logTy, logDet) = toEngineLog l
  when (logLevel >= serverLogLevel && isLogTypeEnabled enabledLogTypes logTy) $
    liftIO $ FL.pushLogStrLn loggerSet $ FL.toLogStr (J.encode $ EngineLog localTime logLevel logTy logDet)

eventTriggerLogType :: EngineLogType Hasura
eventTriggerLogType = ELTInternal ILTEventTrigger

scheduledTriggerLogType :: EngineLogType Hasura
scheduledTriggerLogType = ELTInternal ILTScheduledTrigger
