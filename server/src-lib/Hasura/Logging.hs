{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE UndecidableInstances #-}

module Hasura.Logging
  ( LoggerSettings(..)
  , defaultLoggerSettings
  , EngineLogType(..)
  , HasuraEngine
  , EngineLog(..)
  , userAllowedLogTypes
  , ToEngineLog(..)
  , debugT
  , debugBS
  , debugLBS
  , Logger (..)
  , LogLevel(..)
  , mkLogger
  , LoggerCtx(..)
  , mkLoggerCtx
  , cleanLoggerCtx
  , eventTriggerLogType
  , EnabledLogTypes (..)
  , defaultEnabledEngineLogTypes
  , isEngineLogTypeEnabled
  , readLogTypes
  ) where

import           Hasura.Prelude

import qualified Control.AutoUpdate         as Auto
import qualified Data.Aeson                 as J
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
class (Eq a, Hashable a) => EnabledLogTypes a where
  parseEnabledLogTypes :: String -> Either String [a]
  defaultEnabledLogTypes :: Set.HashSet a
  isLogTypeEnabled :: Set.HashSet a -> a -> Bool

-- | A family of EngineLogType types
data family EngineLogType impl

data HasuraEngine

data instance EngineLogType HasuraEngine
  = ELTHttpLog
  | ELTWebsocketLog
  | ELTWebhookLog
  | ELTQueryLog
  | ELTStartup
  -- internal log types
  | ELTInternal !Text
  deriving (Show, Eq, Generic)

instance Hashable (EngineLogType HasuraEngine)

instance J.ToJSON (EngineLogType HasuraEngine) where
  toJSON = \case
    ELTHttpLog      -> "http-log"
    ELTWebsocketLog -> "websocket-log"
    ELTWebhookLog   -> "webhook-log"
    ELTQueryLog     -> "query-log"
    ELTStartup      -> "startup"
    ELTInternal t   -> J.String t

instance J.FromJSON (EngineLogType HasuraEngine) where
  parseJSON = J.withText "log-type" $ \s -> case T.toLower $ T.strip s of
    "startup"       -> return ELTStartup
    "http-log"      -> return ELTHttpLog
    "webhook-log"   -> return ELTWebhookLog
    "websocket-log" -> return ELTWebsocketLog
    "query-log"     -> return ELTQueryLog
    _               -> fail $ "Valid list of comma-separated log types: "
                       <> BLC.unpack (J.encode userAllowedLogTypes)

-- the default enabled log-types
defaultEnabledEngineLogTypes :: Set.HashSet (EngineLogType HasuraEngine)
defaultEnabledEngineLogTypes =
  Set.fromList [ELTStartup, ELTHttpLog, ELTWebhookLog, ELTWebsocketLog]

isEngineLogTypeEnabled :: Set.HashSet (EngineLogType HasuraEngine) -> EngineLogType HasuraEngine -> Bool
isEngineLogTypeEnabled enabledTypes logTy = case logTy of
  ELTInternal _ -> True
  _             -> logTy `Set.member` enabledTypes


readLogTypes :: String -> Either String [EngineLogType HasuraEngine]
readLogTypes = mapM (J.eitherDecodeStrict' . quote . txtToBs) . T.splitOn "," . T.pack
  where quote x = "\"" <> x <> "\""

instance EnabledLogTypes (EngineLogType HasuraEngine) where
  parseEnabledLogTypes = readLogTypes
  defaultEnabledLogTypes = defaultEnabledEngineLogTypes
  isLogTypeEnabled = isEngineLogTypeEnabled

-- log types that can be set by the user
userAllowedLogTypes :: [EngineLogType HasuraEngine]
userAllowedLogTypes =
  [ ELTStartup
  , ELTHttpLog
  , ELTWebhookLog
  , ELTWebsocketLog
  , ELTQueryLog
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
  } --deriving (Show, Eq)

-- $(J.deriveToJSON (J.aesonDrop 3 J.snakeCase) ''EngineLog)

instance J.ToJSON (EngineLogType impl) => J.ToJSON (EngineLog impl) where
  toJSON (EngineLog ts level ty detail) =
    J.object [ "timestamp" J..= ts
             , "level" J..= level
             , "type" J..= ty
             , "detail" J..= detail
             ]

-- instance Show impl => Show (EngineLog impl)
-- instance Eq impl => Eq (EngineLog impl)


class EnabledLogTypes (EngineLogType logtype) => ToEngineLog a logtype where
  toEngineLog :: a -> (LogLevel, EngineLogType logtype, J.Value)


newtype UnstructuredLog
  = UnstructuredLog { _unUnstructuredLog :: TBS.TByteString }
  deriving (Show, Eq)

debugT :: Text -> UnstructuredLog
debugT = UnstructuredLog . TBS.fromText

debugBS :: B.ByteString -> UnstructuredLog
debugBS = UnstructuredLog . TBS.fromBS

debugLBS :: BL.ByteString -> UnstructuredLog
debugLBS = UnstructuredLog . TBS.fromLBS

instance ToEngineLog UnstructuredLog HasuraEngine where
  toEngineLog (UnstructuredLog t) =
    (LevelDebug, ELTInternal "unstructured", J.toJSON t)

data LoggerCtx impl
  = LoggerCtx
  { _lcLoggerSet       :: !FL.LoggerSet
  , _lcLogLevel        :: !LogLevel
  , _lcTimeGetter      :: !(IO FormattedTime)
  , _lcEnabledLogTypes :: !(Set.HashSet (EngineLogType impl))
  }

data LoggerSettings
  = LoggerSettings
  -- should current time be cached (refreshed every sec)
  { _lsCachedTimestamp :: !Bool
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
  -> Set.HashSet (EngineLogType a)
  -> IO (LoggerCtx a)
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


newtype Logger logtype
  = Logger { unLogger :: forall a m. (ToEngineLog a logtype, MonadIO m) => a -> m () }

mkLogger :: LoggerCtx HasuraEngine -> Logger HasuraEngine
mkLogger (LoggerCtx loggerSet serverLogLevel timeGetter enabledLogTypes) = Logger $ \l -> do
  localTime <- liftIO timeGetter
  let (logLevel, logTy, logDet) = toEngineLog l
  when (logLevel >= serverLogLevel && isLogTypeEnabled enabledLogTypes logTy) $
    liftIO $ FL.pushLogStrLn loggerSet $ FL.toLogStr (J.encode $ EngineLog localTime logLevel logTy logDet)

eventTriggerLogType :: EngineLogType HasuraEngine
eventTriggerLogType = ELTInternal "event-trigger"
