-- | This is taken from wai-logger and customised for our use
module Hasura.Server.Logging
  ( StartupLog (..),
    PGLog (..),
    RequestMode (..),
    mkInconsMetadataLog,
    mkHttpAccessLogContext,
    mkHttpErrorLogContext,
    mkHttpLog,
    HttpInfoLog (..),
    OperationLog (..),
    HttpLogContext (..),
    WebHookLog (..),
    HttpException,
    HttpLog (..),
    GQLBatchQueryOperationLog (..),
    GQLQueryOperationSuccessLog (..),
    GQLQueryOperationErrorLog (..),
    MetadataLog (..),
    EnvVarsMovedToMetadata (..),
    DeprecatedEnvVars (..),
    logDeprecatedEnvVars,
    CommonHttpLogMetadata (..),
    HttpLogMetadata,
    buildHttpLogMetadata,
    emptyHttpLogMetadata,
    MetadataQueryLoggingMode (..),
    LoggingSettings (..),
    SchemaSyncThreadType (..),
    SchemaSyncLog (..),
    HttpLogGraphQLInfo,
    emptyHttpLogGraphQLInfo,
    ModelInfo (..),
    ModelInfoLog (..),
  )
where

import Control.Lens ((^?))
import Data.Aeson qualified as J
import Data.Aeson.Lens (key, _String)
import Data.ByteString.Lazy qualified as BL
import Data.Environment qualified as Env
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as Set
import Data.Int (Int64)
import Data.List.NonEmpty qualified as NE
import Data.SerializableBlob qualified as SB
import Data.Text qualified as T
import Data.Text.Extended
import Hasura.Base.Error
import Hasura.GraphQL.ParameterizedQueryHash
import Hasura.GraphQL.Transport.HTTP.Protocol qualified as GH
import Hasura.HTTP
import Hasura.Logging
import Hasura.Prelude
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Metadata.Object
import Hasura.RQL.Types.Source
import Hasura.Server.Compression
import Hasura.Server.Types
import Hasura.Server.Utils
  ( DeprecatedEnvVars (..),
    EnvVarsMovedToMetadata (..),
    deprecatedEnvVars,
    envVarsMovedToMetadata,
  )
import Hasura.Session
import Hasura.Tracing (TraceT)
import Network.HTTP.Types qualified as HTTP
import Network.Wai.Extended qualified as Wai

data StartupLog = StartupLog
  { slLogLevel :: !LogLevel,
    slKind :: !Text,
    slInfo :: !J.Value
  }
  deriving (Eq)

instance J.ToJSON StartupLog where
  toJSON (StartupLog _ k info) =
    J.object
      [ "kind" J..= k,
        "info" J..= info
      ]

instance ToEngineLog StartupLog Hasura where
  toEngineLog startupLog =
    (slLogLevel startupLog, ELTStartup, J.toJSON startupLog)

data PGLog = PGLog
  { plLogLevel :: !LogLevel,
    plMessage :: !J.Value
  }
  deriving (Eq)

instance J.ToJSON PGLog where
  toJSON (PGLog _ msg) =
    J.object ["message" J..= msg]

instance ToEngineLog PGLog Hasura where
  toEngineLog pgLog =
    (plLogLevel pgLog, ELTInternal ILTPgClient, J.toJSON pgLog)

data MetadataLog = MetadataLog
  { mlLogLevel :: !LogLevel,
    mlMessage :: !Text,
    mlInfo :: !J.Value
  }
  deriving (Eq)

instance J.ToJSON MetadataLog where
  toJSON (MetadataLog _ msg infoVal) =
    J.object
      [ "message" J..= msg,
        "info" J..= infoVal
      ]

instance ToEngineLog MetadataLog Hasura where
  toEngineLog ml =
    (mlLogLevel ml, ELTInternal ILTMetadata, J.toJSON ml)

mkInconsMetadataLog :: [InconsistentMetadata] -> MetadataLog
mkInconsMetadataLog objs =
  MetadataLog LevelWarn "Inconsistent Metadata!"
    $ J.object ["objects" J..= objs]

data WebHookLog = WebHookLog
  { whlLogLevel :: !LogLevel,
    whlStatusCode :: !(Maybe HTTP.Status),
    whlUrl :: !Text,
    whlMethod :: !HTTP.StdMethod,
    whlError :: !(Maybe HttpException),
    whlResponse :: !(Maybe Text),
    whlMessage :: !(Maybe Text)
  }

instance ToEngineLog WebHookLog Hasura where
  toEngineLog webHookLog =
    (whlLogLevel webHookLog, ELTWebhookLog, J.toJSON webHookLog)

instance J.ToJSON WebHookLog where
  toJSON whl =
    J.object
      [ "status_code" J..= (HTTP.statusCode <$> whlStatusCode whl),
        "url" J..= whlUrl whl,
        "method" J..= show (whlMethod whl),
        "http_error" J..= whlError whl,
        "response" J..= whlResponse whl,
        "message" J..= whlMessage whl
      ]

-- | GQLQueryOperationSuccessLog captures all the data required to construct
--   an HTTP success log.
data GQLQueryOperationSuccessLog = GQLQueryOperationSuccessLog
  { gqolQuery :: !GH.GQLReqUnparsed,
    gqolQueryExecutionTime :: !DiffTime,
    gqolResponseSize :: !Int64,
    gqolRequestSize :: !Int64,
    gqolParameterizedQueryHash :: !ParameterizedQueryHash
  }
  deriving (Eq, Generic)

instance J.ToJSON GQLQueryOperationSuccessLog where
  toJSON = J.genericToJSON hasuraJSON {J.omitNothingFields = True}
  toEncoding = J.genericToEncoding hasuraJSON {J.omitNothingFields = True}

-- | GQLQueryOperationErrorLog captures the request along with the error message
data GQLQueryOperationErrorLog = GQLQueryOperationErrorLog
  { gqelQuery :: !GH.GQLReqUnparsed,
    gqelError :: !QErr
  }
  deriving (Eq, Generic)

instance J.ToJSON GQLQueryOperationErrorLog where
  toJSON = J.genericToJSON hasuraJSON
  toEncoding = J.genericToEncoding hasuraJSON

data GQLBatchQueryOperationLog
  = GQLQueryOperationSuccess !GQLQueryOperationSuccessLog
  | GQLQueryOperationError !GQLQueryOperationErrorLog
  deriving (Eq)

instance J.ToJSON GQLBatchQueryOperationLog where
  toJSON = \case
    GQLQueryOperationSuccess successLog -> J.toJSON successLog
    GQLQueryOperationError errorLog -> J.toJSON errorLog

-- | whether a request is executed in batched mode or not
data RequestMode
  = -- | this request is batched
    RequestModeBatched
  | -- | this is a single request
    RequestModeSingle
  | -- | this request is of a kind for which batching is not done or does not make sense
    RequestModeNonBatchable
  | -- | the execution of this request failed
    RequestModeError
  deriving (Eq)

instance J.ToJSON RequestMode where
  toJSON = \case
    RequestModeBatched -> "batched"
    RequestModeSingle -> "single"
    RequestModeNonBatchable -> "non-graphql"
    RequestModeError -> "error"

data CommonHttpLogMetadata = CommonHttpLogMetadata
  { _chlmRequestMode :: !RequestMode,
    _chlmBatchOperationLog :: !(Maybe (GH.GQLBatchedReqs GQLBatchQueryOperationLog))
  }
  deriving (Eq)

-- The information from the GraphQL layer that needs to be included in the http-log.
-- This info is used to construct 'HttpLogMetadata m'
type HttpLogGraphQLInfo = (CommonHttpLogMetadata, ParameterizedQueryHashList)

emptyHttpLogGraphQLInfo :: HttpLogGraphQLInfo
emptyHttpLogGraphQLInfo = (CommonHttpLogMetadata RequestModeNonBatchable Nothing, PQHSetEmpty)

-- | The http-log metadata attached to HTTP requests running in the monad 'm', split into a
-- common portion that is present regardless of 'm', and a monad-specific one defined in the
-- 'HttpLog' instance.
--
-- This allows us to not have to duplicate the code that generates the common part of the metadata
-- across OSS and Pro, so that instances only have to implement the part of it unique to them.
type HttpLogMetadata m = (CommonHttpLogMetadata, ExtraHttpLogMetadata m)

buildHttpLogMetadata ::
  forall m.
  (HttpLog m) =>
  HttpLogGraphQLInfo ->
  ExtraUserInfo ->
  HttpLogMetadata m
buildHttpLogMetadata (commonHttpLogMetadata, paramQueryHashList) extraUserInfo =
  (commonHttpLogMetadata, buildExtraHttpLogMetadata @m paramQueryHashList extraUserInfo)

-- | synonym for clarity, writing `emptyHttpLogMetadata @m` instead of `def @(HttpLogMetadata m)`
emptyHttpLogMetadata :: forall m. (HttpLog m) => HttpLogMetadata m
emptyHttpLogMetadata = (CommonHttpLogMetadata RequestModeNonBatchable Nothing, emptyExtraHttpLogMetadata @m)

-- See Note [Disable query printing for metadata queries]
data MetadataQueryLoggingMode = MetadataQueryLoggingEnabled | MetadataQueryLoggingDisabled
  deriving (Show, Eq)

instance J.FromJSON MetadataQueryLoggingMode where
  parseJSON =
    J.withBool "MetadataQueryLoggingMode"
      $ pure
      . bool MetadataQueryLoggingDisabled MetadataQueryLoggingEnabled

instance J.ToJSON MetadataQueryLoggingMode where
  toJSON = \case
    MetadataQueryLoggingEnabled -> J.Bool True
    MetadataQueryLoggingDisabled -> J.Bool False

-- | Setting used to control the information in logs
data LoggingSettings = LoggingSettings
  { -- | this is only required for the short-term fix in https://github.com/hasura/graphql-engine-mono/issues/1770
    -- See Note [Disable query printing when query-log is disabled]
    _lsEnabledLogTypes :: HashSet (EngineLogType Hasura),
    -- See Note [Disable query printing for metadata queries]
    _lsMetadataQueryLoggingMode :: MetadataQueryLoggingMode
  }
  deriving (Eq)

{- Note [Disable query printing when query-log is disabled]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
As a temporary hack (as per https://github.com/hasura/graphql-engine-mono/issues/1770),
we want to print the graphql query string in `http-log` or `websocket-log` only
when `query-log` is enabled.
-}

{- Note [Disable query printing for metadata queries]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The 'olQuery' in the 'OperationLog' logs the actual query that is sent over HTTP.
This can lead to security issues, since the request sent in metadata queries may
include sensitive information such as DB URLS. Thus it is important that we hide
these sensitive information for the metadata URL.

As a temporary hotfix (ref: https://github.com/hasura/graphql-engine-mono/issues/3937),
If the URL path of HTTP requests is for a metadata operation and the
HASURA_GRAPHQL_ENABLE_METADATA_QUERY_LOGGING envirnoment variables is not set, then
we disable the 'query' field in HTTP logs.
-}

class (Monad m) => HttpLog m where
  -- | Extra http-log metadata that we attach when operating in 'm'.
  type ExtraHttpLogMetadata m

  emptyExtraHttpLogMetadata :: ExtraHttpLogMetadata m

  buildExtraHttpLogMetadata :: ParameterizedQueryHashList -> ExtraUserInfo -> ExtraHttpLogMetadata m

  logHttpError ::
    -- | the logger
    Logger Hasura ->
    -- | setting used to control the information in logs
    LoggingSettings ->
    -- | user info may or may not be present (error can happen during user resolution)
    Maybe UserInfo ->
    -- | request id of the request
    RequestId ->
    -- | the Wai.Request object
    Wai.Request ->
    -- | the request body and parsed request
    (BL.ByteString, Maybe J.Value) ->
    -- | the error
    QErr ->
    -- | list of request headers
    [HTTP.Header] ->
    HttpLogMetadata m ->
    -- | flag to indicate if the request/response size should be added to the Prometheus Counter
    Bool ->
    m ()

  logHttpSuccess ::
    -- | the logger
    Logger Hasura ->
    -- | setting used to control the information in logs
    LoggingSettings ->
    -- | user info may or may not be present (error can happen during user resolution)
    Maybe UserInfo ->
    -- | request id of the request
    RequestId ->
    -- | the Wai.Request object
    Wai.Request ->
    -- | the request body and parsed request
    (BL.ByteString, Maybe J.Value) ->
    -- | the response bytes
    BL.ByteString ->
    -- | the compressed response bytes
    -- ^ TODO (from master): make the above two type represented
    BL.ByteString ->
    -- | IO/network wait time and service time (respectively) for this request, if available.
    Maybe (DiffTime, DiffTime) ->
    -- | possible compression type
    Maybe CompressionType ->
    -- | list of request headers
    [HTTP.Header] ->
    HttpLogMetadata m ->
    -- | flag to indicate if the request/response size should be added to the Prometheus Counter
    Bool ->
    m ()

instance (HttpLog m) => HttpLog (TraceT m) where
  type ExtraHttpLogMetadata (TraceT m) = ExtraHttpLogMetadata m

  buildExtraHttpLogMetadata a = buildExtraHttpLogMetadata @m a
  emptyExtraHttpLogMetadata = emptyExtraHttpLogMetadata @m

  logHttpError a b c d e f g h i j = lift $ logHttpError a b c d e f g h i j

  logHttpSuccess a b c d e f g h i j k l m = lift $ logHttpSuccess a b c d e f g h i j k l m

instance (HttpLog m) => HttpLog (ReaderT r m) where
  type ExtraHttpLogMetadata (ReaderT r m) = ExtraHttpLogMetadata m

  buildExtraHttpLogMetadata a = buildExtraHttpLogMetadata @m a
  emptyExtraHttpLogMetadata = emptyExtraHttpLogMetadata @m

  logHttpError a b c d e f g h i j = lift $ logHttpError a b c d e f g h i j

  logHttpSuccess a b c d e f g h i j k l m = lift $ logHttpSuccess a b c d e f g h i j k l m

instance (HttpLog m) => HttpLog (ExceptT e m) where
  type ExtraHttpLogMetadata (ExceptT e m) = ExtraHttpLogMetadata m

  buildExtraHttpLogMetadata a = buildExtraHttpLogMetadata @m a
  emptyExtraHttpLogMetadata = emptyExtraHttpLogMetadata @m

  logHttpError a b c d e f g h i j = lift $ logHttpError a b c d e f g h i j

  logHttpSuccess a b c d e f g h i j k l m = lift $ logHttpSuccess a b c d e f g h i j k l m

-- | Log information about the HTTP request
data HttpInfoLog = HttpInfoLog
  { hlStatus :: !HTTP.Status,
    hlMethod :: !Text,
    hlSource :: !Wai.IpAddress,
    hlPath :: !Text,
    hlHttpVersion :: !HTTP.HttpVersion,
    hlCompression :: !(Maybe CompressionType),
    -- | all the request headers
    hlHeaders :: ![HTTP.Header]
  }
  deriving (Eq)

instance J.ToJSON HttpInfoLog where
  toJSON (HttpInfoLog st met src path hv compressTypeM _) =
    J.object
      [ "status" J..= HTTP.statusCode st,
        "method" J..= met,
        "ip" J..= Wai.showIPAddress src,
        "url" J..= path,
        "http_version" J..= show hv,
        "content_encoding" J..= (compressionTypeToTxt <$> compressTypeM)
      ]

-- | Information about a GraphQL/Hasura metadata operation over HTTP
data OperationLog = OperationLog
  { olRequestId :: !RequestId,
    olUserVars :: !(Maybe SessionVariables),
    olResponseSize :: !(Maybe Int64),
    -- | Response size before compression
    olUncompressedResponseSize :: !Int64,
    -- | Request IO wait time, i.e. time spent reading the full request from the socket.
    olRequestReadTime :: !(Maybe Seconds),
    -- | Service time, not including request IO wait time.
    olQueryExecutionTime :: !(Maybe Seconds),
    olQuery :: !(Maybe J.Value),
    olRawQuery :: !(Maybe Text),
    olError :: !(Maybe QErr),
    olRequestMode :: !RequestMode
  }
  deriving (Eq, Generic)

instance J.ToJSON OperationLog where
  toJSON = J.genericToJSON hasuraJSON {J.omitNothingFields = True}
  toEncoding = J.genericToEncoding hasuraJSON {J.omitNothingFields = True}

-- | @BatchOperationSuccessLog@ contains the information required for a single
--   successful operation in a batch request for OSS. This type is a subset of the @GQLQueryOperationSuccessLog@
data BatchOperationSuccessLog = BatchOperationSuccessLog
  { _bolQuery :: !(Maybe J.Value),
    _bolResponseSize :: !Int64,
    _bolQueryExecutionTime :: !Seconds
  }
  deriving (Eq, Generic)

instance J.ToJSON BatchOperationSuccessLog where
  toJSON = J.genericToJSON hasuraJSON {J.omitNothingFields = True}
  toEncoding = J.genericToEncoding hasuraJSON {J.omitNothingFields = True}

-- | @BatchOperationSuccessLog@ contains the information required for a single
--   erroneous operation in a batch request for OSS. This type is a subset of the @GQLQueryOperationErrorLog@
data BatchOperationErrorLog = BatchOperationErrorLog
  { _belQuery :: !(Maybe J.Value),
    _belError :: !QErr
  }
  deriving (Eq, Generic)

instance J.ToJSON BatchOperationErrorLog where
  toJSON = J.genericToJSON hasuraJSON {J.omitNothingFields = True}
  toEncoding = J.genericToEncoding hasuraJSON {J.omitNothingFields = True}

data BatchOperationLog
  = BatchOperationSuccess !BatchOperationSuccessLog
  | BatchOperationError !BatchOperationErrorLog
  deriving (Eq)

instance J.ToJSON BatchOperationLog where
  toJSON = \case
    BatchOperationSuccess successLog -> J.toJSON successLog
    BatchOperationError errorLog -> J.toJSON errorLog

data HttpLogContext = HttpLogContext
  { hlcHttpInfo :: !HttpInfoLog,
    hlcOperation :: !OperationLog,
    hlcRequestId :: !RequestId,
    hlcBatchedOperations :: !(Maybe (NE.NonEmpty BatchOperationLog))
  }
  deriving (Eq, Generic)

instance J.ToJSON HttpLogContext where
  toJSON = J.genericToJSON hasuraJSON {J.omitNothingFields = True}
  toEncoding = J.genericToEncoding hasuraJSON {J.omitNothingFields = True}

-- | Check if the 'query' field should be included in the http-log
isQueryIncludedInLogs :: Text -> LoggingSettings -> Bool
isQueryIncludedInLogs urlPath LoggingSettings {..}
  -- See Note [Disable query printing for metadata queries]
  | isQueryLogEnabled && isMetadataRequest = _lsMetadataQueryLoggingMode == MetadataQueryLoggingEnabled
  -- See Note [Disable query printing when query-log is disabled]
  | isQueryLogEnabled = True
  | otherwise = False
  where
    isQueryLogEnabled = Set.member ELTQueryLog _lsEnabledLogTypes
    metadataUrlPaths = ["/v1/metadata", "/v1/query"]
    isMetadataRequest = urlPath `elem` metadataUrlPaths

-- | Add the 'query' field to the http-log if `MetadataQueryLoggingMode`
-- is set to `MetadataQueryLoggingEnabled` else only adds the `query.type` field.
addQuery :: Maybe J.Value -> Text -> LoggingSettings -> Maybe J.Value
addQuery parsedReq path loggingSettings =
  if isQueryIncludedInLogs path loggingSettings
    then parsedReq
    else Just $ J.object ["type" J..= (fmap (^? key "type" . _String)) parsedReq]

mkHttpAccessLogContext ::
  -- | Maybe because it may not have been resolved
  Maybe UserInfo ->
  LoggingSettings ->
  RequestId ->
  Wai.Request ->
  (BL.ByteString, Maybe J.Value) ->
  -- | Size of response body, before compression
  Int64 ->
  BL.ByteString ->
  Maybe (DiffTime, DiffTime) ->
  Maybe CompressionType ->
  [HTTP.Header] ->
  RequestMode ->
  Maybe (GH.GQLBatchedReqs GQLBatchQueryOperationLog) ->
  HttpLogContext
mkHttpAccessLogContext userInfoM loggingSettings reqId req (_, parsedReq) uncompressedResponseSize res mTiming compressTypeM headers batching queryLogMetadata =
  let http =
        HttpInfoLog
          { hlStatus = status,
            hlMethod = bsToTxt $ Wai.requestMethod req,
            hlSource = Wai.getSourceFromFallback req,
            hlPath = bsToTxt $ Wai.rawPathInfo req,
            hlHttpVersion = Wai.httpVersion req,
            hlCompression = compressTypeM,
            hlHeaders = headers
          }
      op =
        OperationLog
          { olRequestId = reqId,
            olUserVars = _uiSession <$> userInfoM,
            olResponseSize = respSize,
            olUncompressedResponseSize = uncompressedResponseSize,
            olRequestReadTime = Seconds . fst <$> mTiming,
            olQueryExecutionTime = Seconds . snd <$> mTiming,
            olRequestMode = batching,
            olQuery = addQuery parsedReq (hlPath http) loggingSettings,
            olRawQuery = Nothing,
            olError = Nothing
          }
      batchOpLog =
        queryLogMetadata
          >>= ( \case
                  GH.GQLSingleRequest _ -> Nothing -- This case is aleady handled in the `OperationLog`
                  GH.GQLBatchedReqs opLogs ->
                    NE.nonEmpty
                      $ map
                        ( \case
                            GQLQueryOperationSuccess (GQLQueryOperationSuccessLog {..}) ->
                              BatchOperationSuccess
                                $ BatchOperationSuccessLog
                                  (addQuery parsedReq (hlPath http) loggingSettings)
                                  gqolResponseSize
                                  (convertDuration gqolQueryExecutionTime)
                            GQLQueryOperationError (GQLQueryOperationErrorLog {..}) ->
                              BatchOperationError
                                $ BatchOperationErrorLog
                                  (addQuery parsedReq (hlPath http) loggingSettings)
                                  gqelError
                        )
                        opLogs
              )
   in HttpLogContext http op reqId batchOpLog
  where
    status = HTTP.status200
    respSize = Just $ BL.length res

mkHttpErrorLogContext ::
  -- | Maybe because it may not have been resolved
  Maybe UserInfo ->
  LoggingSettings ->
  RequestId ->
  Wai.Request ->
  (BL.ByteString, Maybe J.Value) ->
  QErr ->
  Maybe (DiffTime, DiffTime) ->
  Maybe CompressionType ->
  [HTTP.Header] ->
  HttpLogContext
mkHttpErrorLogContext userInfoM loggingSettings reqId waiReq (reqBody, parsedReq) err mTiming compressTypeM headers =
  let http =
        HttpInfoLog
          { hlStatus = qeStatus err,
            hlMethod = bsToTxt $ Wai.requestMethod waiReq,
            hlSource = Wai.getSourceFromFallback waiReq,
            hlPath = bsToTxt $ Wai.rawPathInfo waiReq,
            hlHttpVersion = Wai.httpVersion waiReq,
            hlCompression = compressTypeM,
            hlHeaders = headers
          }
      responseSize = BL.length $ J.encode err
      op =
        OperationLog
          { olRequestId = reqId,
            olUserVars = _uiSession <$> userInfoM,
            olResponseSize = Just responseSize,
            olUncompressedResponseSize = responseSize,
            olRequestReadTime = Seconds . fst <$> mTiming,
            olQueryExecutionTime = Seconds . snd <$> mTiming,
            olQuery = addQuery parsedReq (hlPath http) loggingSettings,
            -- if parsedReq is Nothing, add the raw query
            olRawQuery = maybe (reqToLog $ Just $ bsToTxt $ BL.toStrict reqBody) (const Nothing) parsedReq,
            olError = Just err,
            olRequestMode = RequestModeError
          }

      reqToLog :: Maybe a -> Maybe a
      reqToLog req = if (isQueryIncludedInLogs (hlPath http) loggingSettings) then req else Nothing
   in HttpLogContext http op reqId Nothing -- Batched operation logs are always reported in logHttpSuccess even if there are errors

data HttpLogLine = HttpLogLine
  { _hlLogLevel :: !LogLevel,
    _hlLogLine :: !HttpLogContext
  }

instance ToEngineLog HttpLogLine Hasura where
  toEngineLog (HttpLogLine logLevel logLine) =
    (logLevel, ELTHttpLog, J.toJSON logLine)

mkHttpLog :: HttpLogContext -> HttpLogLine
mkHttpLog httpLogCtx =
  let isError = isJust $ olError $ hlcOperation httpLogCtx
      logLevel = bool LevelInfo LevelError isError
   in HttpLogLine logLevel httpLogCtx

-- | Log warning messages for deprecated environment variables
logDeprecatedEnvVars ::
  Logger Hasura ->
  Env.Environment ->
  SourceCache ->
  IO ()
logDeprecatedEnvVars logger env sources = do
  let toText envVars = commaSeparated envVars
      -- The environment variables that have been initialized by user
      envVarsInitialized = fmap fst (Env.toList env)
      checkDeprecatedEnvVars envs = T.pack <$> envVarsInitialized `intersect` envs

  -- When a source named 'default' is present, it means that it is a migrated v2
  -- hasura project. In such cases log those environment variables that are moved
  -- to the metadata
  for_ (HashMap.lookup SNDefault sources) $ \_defSource -> do
    let deprecated = checkDeprecatedEnvVars (unEnvVarsMovedToMetadata envVarsMovedToMetadata)
    unless (null deprecated)
      $ unLogger logger
      $ UnstructuredLog LevelWarn
      $ SB.fromText
      $ "The following environment variables are deprecated and moved to metadata: "
      <> toText deprecated

  -- Log when completely deprecated environment variables are present
  let deprecated = checkDeprecatedEnvVars (unDeprecatedEnvVars deprecatedEnvVars)
  unless (null deprecated)
    $ unLogger logger
    $ UnstructuredLog LevelWarn
    $ SB.fromText
    $ "The following environment variables are deprecated: "
    <> toText deprecated

data SchemaSyncThreadType
  = TTListener
  | TTProcessor
  | TTMetadataApi
  deriving (Eq)

instance Show SchemaSyncThreadType where
  show TTListener = "listener"
  show TTProcessor = "processor"
  show TTMetadataApi = "metadata-api"

data SchemaSyncLog = SchemaSyncLog
  { sslLogLevel :: !LogLevel,
    sslThreadType :: !SchemaSyncThreadType,
    sslInfo :: !J.Value
  }
  deriving (Show, Eq)

instance J.ToJSON SchemaSyncLog where
  toJSON (SchemaSyncLog _ t info) =
    J.object
      [ "thread_type" J..= show t,
        "info" J..= info
      ]

instance ToEngineLog SchemaSyncLog Hasura where
  toEngineLog threadLog =
    (sslLogLevel threadLog, ELTInternal ILTSchemaSync, J.toJSON threadLog)

data ModelInfo = ModelInfo
  { miModelName :: !Text,
    miModelType :: !Text,
    miSourceName :: !(Maybe Text),
    miSourceType :: !(Maybe Text),
    miQueryType :: !Text,
    miIsCached :: !Bool
  }
  deriving stock (Generic)

instance J.ToJSON ModelInfo where
  toJSON = J.genericToJSON hasuraJSON {J.omitNothingFields = True}

data ModelInfoLog = ModelInfoLog
  { milLogType :: !LogLevel,
    milModelInfo :: !ModelInfo
  }
  deriving stock (Generic)

instance ToEngineLog ModelInfoLog Hasura where
  toEngineLog (ModelInfoLog level t) =
    (level, ELTInternal ILTModelInfo, J.toJSON t)
