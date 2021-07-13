-- This is taken from wai-logger and customised for our use

module Hasura.Server.Logging
  ( StartupLog(..)
  , PGLog(..)
  , RequestBatching(..)
  , mkInconsMetadataLog
  , mkHttpAccessLogContext
  , mkHttpErrorLogContext
  , mkHttpLog
  , HttpInfoLog(..)
  , OperationLog(..)
  , HttpLogContext(..)
  , WebHookLog(..)
  , HttpException
  , HttpLog (..)
  , EnvVarsMovedToMetadata(..)
  , DeprecatedEnvVars(..)
  , logDeprecatedEnvVars
  , CommonHttpLogMetadata(..)
  , HttpLogMetadata
  , buildHttpLogMetadata
  , emptyHttpLogMetadata
  ) where

import           Hasura.Prelude

import qualified Data.ByteString.Lazy                  as BL
import qualified Data.Environment                      as Env
import qualified Data.HashMap.Strict                   as HM
import qualified Data.HashSet                          as Set
import qualified Data.TByteString                      as TBS
import qualified Data.Text                             as T
import qualified Network.HTTP.Types                    as HTTP
import qualified Network.Wai.Extended                  as Wai

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Int                              (Int64)
import           Data.Text.Extended

import           Hasura.Base.Error
import           Hasura.GraphQL.ParameterizedQueryHash
import           Hasura.HTTP
import           Hasura.Logging
import           Hasura.Metadata.Class
import           Hasura.RQL.Types
import           Hasura.Server.Compression
import           Hasura.Server.Types
import           Hasura.Server.Utils                   (DeprecatedEnvVars (..),
                                                        EnvVarsMovedToMetadata (..),
                                                        deprecatedEnvVars, envVarsMovedToMetadata)
import           Hasura.Session
import           Hasura.Tracing                        (TraceT)


data StartupLog
  = StartupLog
  { slLogLevel :: !LogLevel
  , slKind     :: !Text
  , slInfo     :: !Value
  } deriving (Show, Eq)

instance ToJSON StartupLog where
  toJSON (StartupLog _ k info) =
    object [ "kind" .= k
           , "info" .= info
           ]

instance ToEngineLog StartupLog Hasura where
  toEngineLog startupLog =
    (slLogLevel startupLog, ELTStartup, toJSON startupLog)

data PGLog
  = PGLog
  { plLogLevel :: !LogLevel
  , plMessage  :: !Text
  } deriving (Show, Eq)

instance ToJSON PGLog where
  toJSON (PGLog _ msg) =
    object ["message" .= msg]

instance ToEngineLog PGLog Hasura where
  toEngineLog pgLog =
    (plLogLevel pgLog, ELTInternal ILTPgClient, toJSON pgLog)

data MetadataLog
  = MetadataLog
  { mlLogLevel :: !LogLevel
  , mlMessage  :: !Text
  , mlInfo     :: !Value
  } deriving (Show, Eq)

instance ToJSON MetadataLog where
  toJSON (MetadataLog _ msg infoVal) =
    object [ "message" .= msg
           , "info" .= infoVal
           ]

instance ToEngineLog MetadataLog Hasura where
  toEngineLog ml =
    (mlLogLevel ml, ELTInternal ILTMetadata, toJSON ml)

mkInconsMetadataLog :: [InconsistentMetadata] -> MetadataLog
mkInconsMetadataLog objs =
  MetadataLog LevelWarn "Inconsistent Metadata!" $
    object [ "objects" .= objs]

data WebHookLog
  = WebHookLog
  { whlLogLevel   :: !LogLevel
  , whlStatusCode :: !(Maybe HTTP.Status)
  , whlUrl        :: !Text
  , whlMethod     :: !HTTP.StdMethod
  , whlError      :: !(Maybe HttpException)
  , whlResponse   :: !(Maybe Text)
  , whlMessage    :: !(Maybe Text)
  } deriving (Show)

instance ToEngineLog WebHookLog Hasura where
  toEngineLog webHookLog =
    (whlLogLevel webHookLog, ELTWebhookLog, toJSON webHookLog)

instance ToJSON WebHookLog where
  toJSON whl =
    object [ "status_code" .= (HTTP.statusCode <$> whlStatusCode whl)
           , "url" .= whlUrl whl
           , "method" .= show (whlMethod whl)
           , "http_error" .= whlError whl
           , "response" .= whlResponse whl
           , "message" .= whlMessage whl
           ]

data RequestBatching = RequestBatched | RequestSingle
  deriving (Show, Eq)

instance ToJSON RequestBatching where
  toJSON = \case
    RequestBatched -> "batched"
    RequestSingle  -> "single"

newtype CommonHttpLogMetadata = CommonHttpLogMetadata {_chlmRequestType :: Maybe RequestBatching}
  deriving (Show, Eq)

-- | The http-log metadata attached to HTTP requests running in the monad 'm', split into a
-- common portion that is present regardless of 'm', and a monad-specific one defined in the
-- 'HttpLog' instance.
--
-- This allows us to not have to duplicate the code that generates the common part of the metadata
-- across OSS and Pro, so that instances only have to implement the part of it unique to them.
type HttpLogMetadata m = (CommonHttpLogMetadata, ExtraHttpLogMetadata m)

buildHttpLogMetadata
  :: forall m
   . HttpLog m
  => [ParameterizedQueryHash]
  -> Maybe RequestBatching
  -> HttpLogMetadata m
buildHttpLogMetadata xs rb = (CommonHttpLogMetadata rb, buildExtraHttpLogMetadata @m xs)

-- | synonym for clarity, writing `emptyHttpLogMetadata @m` instead of `def @(HttpLogMetadata m)`
emptyHttpLogMetadata :: forall m. HttpLog m => HttpLogMetadata m
emptyHttpLogMetadata = (CommonHttpLogMetadata Nothing, emptyExtraHttpLogMetadata @m)


{- Note [Disable query printing when query-log is disabled]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
As a temporary hack (as per https://github.com/hasura/graphql-engine-mono/issues/1770),
we want to print the graphql query string in `http-log` or `websocket-log` only
when `query-log` is enabled.
-}

class Monad m => HttpLog m where

  -- | Extra http-log metadata that we attach when operating in 'm'.
  type ExtraHttpLogMetadata m

  emptyExtraHttpLogMetadata :: ExtraHttpLogMetadata m

  buildExtraHttpLogMetadata :: [ParameterizedQueryHash] -> ExtraHttpLogMetadata m

  logHttpError
    :: Logger Hasura
    -- ^ the logger
    -> HashSet (EngineLogType Hasura)
    -- ^ this is only required for the short-term fix in https://github.com/hasura/graphql-engine-mono/issues/1770
    -> Maybe UserInfo
    -- ^ user info may or may not be present (error can happen during user resolution)
    -> RequestId
    -- ^ request id of the request
    -> Wai.Request
    -- ^ the Wai.Request object
    -> (BL.ByteString, Maybe Value)
    -- ^ the request body and parsed request
    -> QErr
    -- ^ the error
    -> [HTTP.Header]
    -- ^ list of request headers
    -> m ()

  logHttpSuccess
    :: Logger Hasura
    -- ^ the logger
    -> HashSet (EngineLogType Hasura)
    -- ^ this is only required for the short-term fix in https://github.com/hasura/graphql-engine-mono/issues/1770
    -> Maybe UserInfo
    -- ^ user info may or may not be present (error can happen during user resolution)
    -> RequestId
    -- ^ request id of the request
    -> Wai.Request
    -- ^ the Wai.Request object
    -> (BL.ByteString, Maybe Value)
    -- ^ the request body and parsed request
    -> BL.ByteString
    -- ^ the response bytes
    -> BL.ByteString
    -- ^ the compressed response bytes
    -- ^ TODO (from master): make the above two type represented
    -> Maybe (DiffTime, DiffTime)
    -- ^ IO/network wait time and service time (respectively) for this request, if available.
    -> Maybe CompressionType
    -- ^ possible compression type
    -> [HTTP.Header]
    -- ^ list of request headers
    -> HttpLogMetadata m
    -> m ()

instance HttpLog m => HttpLog (TraceT m) where

  type ExtraHttpLogMetadata (TraceT m) = ExtraHttpLogMetadata m

  buildExtraHttpLogMetadata a = buildExtraHttpLogMetadata @m a
  emptyExtraHttpLogMetadata = emptyExtraHttpLogMetadata @m

  logHttpError a b c d e f g h = lift $ logHttpError a b c d e f g h

  logHttpSuccess a b c d e f g h i j k l = lift $ logHttpSuccess a b c d e f g h i j k l

instance HttpLog m => HttpLog (ReaderT r m) where

  type ExtraHttpLogMetadata (ReaderT r m) = ExtraHttpLogMetadata m

  buildExtraHttpLogMetadata a = buildExtraHttpLogMetadata @m a
  emptyExtraHttpLogMetadata = emptyExtraHttpLogMetadata @m

  logHttpError a b c d e f g h = lift $ logHttpError a b c d e f g h

  logHttpSuccess a b c d e f g h i j k l = lift $ logHttpSuccess a b c d e f g h i j k l

instance HttpLog m => HttpLog (MetadataStorageT m) where

  type ExtraHttpLogMetadata (MetadataStorageT m) = ExtraHttpLogMetadata m

  buildExtraHttpLogMetadata a = buildExtraHttpLogMetadata @m a
  emptyExtraHttpLogMetadata = emptyExtraHttpLogMetadata @m

  logHttpError a b c d e f g h = lift $ logHttpError a b c d e f g h

  logHttpSuccess a b c d e f g h i j k l = lift $ logHttpSuccess a b c d e f g h i j k l

-- | Log information about the HTTP request
data HttpInfoLog
  = HttpInfoLog
  { hlStatus      :: !HTTP.Status
  , hlMethod      :: !Text
  , hlSource      :: !Wai.IpAddress
  , hlPath        :: !Text
  , hlHttpVersion :: !HTTP.HttpVersion
  , hlCompression :: !(Maybe CompressionType)
  , hlHeaders     :: ![HTTP.Header]
  -- ^ all the request headers
  } deriving (Show, Eq)

instance ToJSON HttpInfoLog where
  toJSON (HttpInfoLog st met src path hv compressTypeM _) =
    object [ "status" .= HTTP.statusCode st
           , "method" .= met
           , "ip" .= Wai.showIPAddress src
           , "url" .= path
           , "http_version" .= show hv
           , "content_encoding" .= (compressionTypeToTxt <$> compressTypeM)
           ]

-- | Information about a GraphQL/Hasura metadata operation over HTTP
data OperationLog
  = OperationLog
  { olRequestId          :: !RequestId
  , olUserVars           :: !(Maybe SessionVariables)
  , olResponseSize       :: !(Maybe Int64)
  , olRequestReadTime    :: !(Maybe Seconds)
  -- ^ Request IO wait time, i.e. time spent reading the full request from the socket.
  , olQueryExecutionTime :: !(Maybe Seconds)
  -- ^ Service time, not including request IO wait time.
  , olQuery              :: !(Maybe Value)
  , olRawQuery           :: !(Maybe Text)
  , olError              :: !(Maybe QErr)
  , olRequestMode        :: !(Maybe RequestBatching)
  } deriving (Show, Eq)

$(deriveToJSON hasuraJSON{omitNothingFields = True} ''OperationLog)

data HttpLogContext
  = HttpLogContext
  { hlcHttpInfo  :: !HttpInfoLog
  , hlcOperation :: !OperationLog
  , hlcRequestId :: !RequestId
  } deriving (Show, Eq)
$(deriveToJSON hasuraJSON ''HttpLogContext)

mkHttpAccessLogContext
  :: Maybe UserInfo
  -- ^ Maybe because it may not have been resolved
  -> HashSet (EngineLogType Hasura)
  -> RequestId
  -> Wai.Request
  -> (BL.ByteString, Maybe Value)
  -> BL.ByteString
  -> Maybe (DiffTime, DiffTime)
  -> Maybe CompressionType
  -> [HTTP.Header]
  -> Maybe RequestBatching
  -> HttpLogContext
mkHttpAccessLogContext userInfoM enabledLogTypes reqId req (_, parsedReq) res mTiming compressTypeM headers batching =
  let http = HttpInfoLog
             { hlStatus      = status
             , hlMethod      = bsToTxt $ Wai.requestMethod req
             , hlSource      = Wai.getSourceFromFallback req
             , hlPath        = bsToTxt $ Wai.rawPathInfo req
             , hlHttpVersion = Wai.httpVersion req
             , hlCompression = compressTypeM
             , hlHeaders     = headers
             }
      op = OperationLog
           { olRequestId    = reqId
           , olUserVars     = _uiSession <$> userInfoM
           , olResponseSize = respSize
           , olRequestReadTime    = Seconds . fst <$> mTiming
           , olQueryExecutionTime = Seconds . snd <$> mTiming
           , olRequestMode = batching
           -- See Note [Disable query printing when query-log is disabled]
           , olQuery = bool Nothing parsedReq $ Set.member ELTQueryLog enabledLogTypes
           , olRawQuery = Nothing
           , olError = Nothing
           }
  in HttpLogContext http op reqId
  where
    status = HTTP.status200
    respSize = Just $ BL.length res

mkHttpErrorLogContext
  :: Maybe UserInfo
  -- ^ Maybe because it may not have been resolved
  -> HashSet (EngineLogType Hasura)
  -> RequestId
  -> Wai.Request
  -> (BL.ByteString, Maybe Value)
  -> QErr
  -> Maybe (DiffTime, DiffTime)
  -> Maybe CompressionType
  -> [HTTP.Header]
  -> HttpLogContext
mkHttpErrorLogContext userInfoM enabledLogTypes reqId waiReq (reqBody, parsedReq) err mTiming compressTypeM headers =
  let http = HttpInfoLog
             { hlStatus      = qeStatus err
             , hlMethod      = bsToTxt $ Wai.requestMethod waiReq
             , hlSource      = Wai.getSourceFromFallback waiReq
             , hlPath        = bsToTxt $ Wai.rawPathInfo waiReq
             , hlHttpVersion = Wai.httpVersion waiReq
             , hlCompression = compressTypeM
             , hlHeaders     = headers
             }
      op = OperationLog
           { olRequestId          = reqId
           , olUserVars           = _uiSession <$> userInfoM
           , olResponseSize       = Just $ BL.length $ encode err
           , olRequestReadTime    = Seconds . fst <$> mTiming
           , olQueryExecutionTime = Seconds . snd <$> mTiming
           , olQuery              = reqToLog parsedReq
           -- if parsedReq is Nothing, add the raw query
           , olRawQuery           = maybe (reqToLog $ Just $ bsToTxt $ BL.toStrict reqBody) (const Nothing) parsedReq
           , olError              = Just err
           , olRequestMode        = Nothing
           }

      -- See Note [Disable query printing when query-log is disabled]
      reqToLog :: Maybe a -> Maybe a
      reqToLog req = bool Nothing req $ Set.member ELTQueryLog enabledLogTypes
  in HttpLogContext http op reqId

data HttpLogLine
  = HttpLogLine
  { _hlLogLevel :: !LogLevel
  , _hlLogLine  :: !HttpLogContext
  }

instance ToEngineLog HttpLogLine Hasura where
  toEngineLog (HttpLogLine logLevel logLine) =
    (logLevel, ELTHttpLog, toJSON logLine)

mkHttpLog :: HttpLogContext -> HttpLogLine
mkHttpLog httpLogCtx =
  let isError = isJust $ olError $ hlcOperation httpLogCtx
      logLevel = bool LevelInfo LevelError isError
  in HttpLogLine logLevel httpLogCtx

-- | Log warning messages for deprecated environment variables
logDeprecatedEnvVars
  :: Logger Hasura
  -> Env.Environment
  -> SourceCache
  -> IO ()
logDeprecatedEnvVars logger env sources = do
  let toText envVars = commaSeparated envVars
      -- The environment variables that have been initialized by user
      envVarsInitialized = fmap fst (Env.toList env)
      checkDeprecatedEnvVars envs = T.pack <$> envVarsInitialized `intersect` envs

  -- When a source named 'default' is present, it means that it is a migrated v2
  -- hasura project. In such cases log those environment variables that are moved
  -- to the metadata
  onJust (HM.lookup SNDefault sources) $ \_defSource -> do
    let deprecated = checkDeprecatedEnvVars (unEnvVarsMovedToMetadata envVarsMovedToMetadata)
    unless (null deprecated) $
      unLogger logger $ UnstructuredLog LevelWarn $ TBS.fromText $
        "The following environment variables are deprecated and moved to metadata: " <>
        toText deprecated

  -- Log when completely deprecated environment variables are present
  let deprecated = checkDeprecatedEnvVars (unDeprecatedEnvVars deprecatedEnvVars)
  unless (null deprecated) $
    unLogger logger $ UnstructuredLog LevelWarn $ TBS.fromText $
      "The following environment variables are deprecated: " <>
      toText deprecated
