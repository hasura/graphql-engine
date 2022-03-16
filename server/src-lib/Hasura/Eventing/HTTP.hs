{-# LANGUAGE TemplateHaskell #-}

-- |
--  = Hasura.Eventing.HTTP
--
--  This module is an utility module providing HTTP utilities for
--  "Hasura.Eventing.EventTriggers" and "Hasura.Eventing.ScheduledTriggers".
--
--  The event triggers and scheduled triggers share the event delivery
--  mechanism using the 'tryWebhook' function defined in this module.
module Hasura.Eventing.HTTP
  ( HTTPErr (..),
    HTTPResp (..),
    runHTTP,
    isNetworkError,
    isNetworkErrorHC,
    logHTTPForET,
    logHTTPForST,
    ExtraLogContext (..),
    RequestDetails (..),
    extractRequest,
    EventId,
    InvocationVersion,
    Response (..),
    WebhookRequest (..),
    WebhookResponse (..),
    ClientError (..),
    isClientError,
    mkClientErr,
    mkWebhookReq,
    mkResp,
    mkInvocationResp,
    LogBehavior (..),
    ResponseLogBehavior (..),
    HeaderLogBehavior (..),
    prepareHeaders,
    getRetryAfterHeaderFromHTTPErr,
    getRetryAfterHeaderFromResp,
    parseRetryHeaderValue,
    invocationVersionET,
    invocationVersionST,
    mkRequest,
    invokeRequest,
    TransformableRequestError (..),
  )
where

import Control.Exception (try)
import Control.Lens (preview, set, view)
import Data.Aeson qualified as J
import Data.Aeson.Lens
import Data.Aeson.TH
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.CaseInsensitive qualified as CI
import Data.Either
import Data.Has
import Data.HashMap.Lazy qualified as HML
import Data.Int (Int64)
import Data.TByteString qualified as TBS
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TE
import Hasura.HTTP (HttpException (..), addDefaultHeaders)
import Hasura.Logging
import Hasura.Prelude
import Hasura.RQL.DDL.Headers
import Hasura.RQL.DDL.Webhook.Transform
import Hasura.RQL.DDL.Webhook.Transform.Class (mkReqTransformCtx)
import Hasura.RQL.Types.Common (ResolvedWebhook (..))
import Hasura.RQL.Types.EventTrigger
import Hasura.RQL.Types.Eventing
import Hasura.Session (SessionVariables)
import Hasura.Tracing
import Network.HTTP.Client.Transformable qualified as HTTP

data LogBehavior = LogBehavior
  { _lbHeader :: !HeaderLogBehavior,
    _lbResponse :: !ResponseLogBehavior
  }

data HeaderLogBehavior = LogEnvValue | LogEnvVarname
  deriving (Show, Eq)

data ResponseLogBehavior = LogSanitisedResponse | LogEntireResponse
  deriving (Show, Eq)

retryAfterHeader :: CI.CI Text
retryAfterHeader = "Retry-After"

data ExtraLogContext = ExtraLogContext
  { elEventId :: !EventId,
    elEventName :: !(Maybe TriggerName)
  }
  deriving (Show, Eq)

data HTTPResp (a :: TriggerTypes) = HTTPResp
  { hrsStatus :: !Int,
    hrsHeaders :: ![HeaderConf],
    hrsBody :: !TBS.TByteString,
    hrsSize :: !Int64
  }
  deriving (Show, Eq)

$(deriveToJSON hasuraJSON {omitNothingFields = True} ''HTTPResp)

instance ToEngineLog (HTTPResp 'EventType) Hasura where
  toEngineLog resp = (LevelInfo, eventTriggerLogType, J.toJSON resp)

instance ToEngineLog (HTTPResp 'ScheduledType) Hasura where
  toEngineLog resp = (LevelInfo, scheduledTriggerLogType, J.toJSON resp)

data HTTPErr (a :: TriggerTypes)
  = HClient !HttpException
  | HStatus !(HTTPResp a)
  | HOther !String
  deriving (Show)

instance J.ToJSON (HTTPErr a) where
  toJSON err = toObj $ case err of
    (HClient httpException) ->
      ("client", J.toJSON httpException)
    (HStatus resp) ->
      ("status", J.toJSON resp)
    (HOther e) -> ("internal", J.toJSON e)
    where
      toObj :: (Text, J.Value) -> J.Value
      toObj (k, v) =
        J.object
          [ "type" J..= k,
            "detail" J..= v
          ]

instance ToEngineLog (HTTPErr 'EventType) Hasura where
  toEngineLog err = (LevelError, eventTriggerLogType, J.toJSON err)

instance ToEngineLog (HTTPErr 'ScheduledType) Hasura where
  toEngineLog err = (LevelError, scheduledTriggerLogType, J.toJSON err)

mkHTTPResp :: HTTP.Response LBS.ByteString -> HTTPResp a
mkHTTPResp resp =
  HTTPResp
    { hrsStatus = HTTP.statusCode $ HTTP.responseStatus resp,
      hrsHeaders = map decodeHeader $ HTTP.responseHeaders resp,
      hrsBody = TBS.fromLBS respBody,
      hrsSize = LBS.length respBody
    }
  where
    respBody = HTTP.responseBody resp
    decodeBS = TE.decodeUtf8With TE.lenientDecode
    decodeHeader (hdrName, hdrVal) =
      HeaderConf (decodeBS $ CI.original hdrName) (HVValue (decodeBS hdrVal))

data RequestDetails = RequestDetails
  { _rdOriginalRequest :: HTTP.Request,
    _rdOriginalSize :: Int64,
    _rdTransformedRequest :: Maybe HTTP.Request,
    _rdTransformedSize :: Maybe Int64,
    _rdReqTransformCtx :: Maybe RequestTransformCtx,
    _rdSessionVars :: Maybe SessionVariables
  }

extractRequest :: RequestDetails -> HTTP.Request
extractRequest RequestDetails {..} = fromMaybe _rdOriginalRequest _rdTransformedRequest

$(deriveToJSON hasuraJSON ''RequestDetails)

data HTTPRespExtra (a :: TriggerTypes) = HTTPRespExtra
  { _hreResponse :: !(Either (HTTPErr a) (HTTPResp a)),
    _hreContext :: !ExtraLogContext,
    _hreRequest :: !RequestDetails,
    -- | Whether to log the entire response, including the body and the headers,
    -- which may contain sensitive information.
    _hreLogResponse :: !ResponseLogBehavior
  }

instance J.ToJSON (HTTPRespExtra a) where
  toJSON (HTTPRespExtra resp ctxt req logResp) =
    case resp of
      Left errResp ->
        J.object $
          [ "response" J..= J.toJSON errResp,
            "request" J..= J.toJSON req,
            "event_id" J..= elEventId ctxt
          ]
            ++ eventName
      Right okResp ->
        J.object $
          [ "response" J..= case logResp of
              LogEntireResponse -> J.toJSON okResp
              LogSanitisedResponse -> sanitisedRespJSON okResp,
            "request" J..= J.toJSON req,
            "event_id" J..= elEventId ctxt
          ]
            ++ eventName
    where
      eventName = case elEventName ctxt of
        Just name -> ["event_name" J..= name]
        Nothing -> []
      sanitisedRespJSON v =
        J.Object $
          HML.fromList
            [ "size" J..= hrsSize v,
              "status" J..= hrsStatus v
            ]

instance ToEngineLog (HTTPRespExtra 'EventType) Hasura where
  toEngineLog resp = (LevelInfo, eventTriggerLogType, J.toJSON resp)

instance ToEngineLog (HTTPRespExtra 'ScheduledType) Hasura where
  toEngineLog resp = (LevelInfo, scheduledTriggerLogType, J.toJSON resp)

isNetworkError :: HTTPErr a -> Bool
isNetworkError = \case
  HClient he -> isNetworkErrorHC he
  _ -> False

isNetworkErrorHC :: HttpException -> Bool
isNetworkErrorHC (HttpException exception) =
  case exception of
    HTTP.HttpExceptionRequest _ (HTTP.ConnectionFailure _) -> True
    HTTP.HttpExceptionRequest _ HTTP.ConnectionTimeout -> True
    HTTP.HttpExceptionRequest _ HTTP.ResponseTimeout -> True
    _ -> False

anyBodyParser :: HTTP.Response LBS.ByteString -> Either (HTTPErr a) (HTTPResp a)
anyBodyParser resp = do
  let httpResp = mkHTTPResp resp
  if respCode >= HTTP.status200 && respCode < HTTP.status300
    then return httpResp
    else throwError $ HStatus httpResp
  where
    respCode = HTTP.responseStatus resp

data HTTPReq = HTTPReq
  { _hrqMethod :: !String,
    _hrqUrl :: !String,
    _hrqPayload :: !(Maybe J.Value),
    _hrqTry :: !Int,
    _hrqDelay :: !(Maybe Int)
  }
  deriving (Show, Eq)

$(deriveJSON hasuraJSON {omitNothingFields = True} ''HTTPReq)

instance ToEngineLog HTTPReq Hasura where
  toEngineLog req = (LevelInfo, eventTriggerLogType, J.toJSON req)

logHTTPForET ::
  ( MonadReader r m,
    Has (Logger Hasura) r,
    MonadIO m
  ) =>
  Either (HTTPErr 'EventType) (HTTPResp 'EventType) ->
  ExtraLogContext ->
  RequestDetails ->
  LogBehavior ->
  m ()
logHTTPForET eitherResp extraLogCtx reqDetails logBehavior = do
  logger :: Logger Hasura <- asks getter
  unLogger logger $ HTTPRespExtra eitherResp extraLogCtx reqDetails (_lbResponse logBehavior)

logHTTPForST ::
  ( MonadReader r m,
    Has (Logger Hasura) r,
    MonadIO m
  ) =>
  Either (HTTPErr 'ScheduledType) (HTTPResp 'ScheduledType) ->
  ExtraLogContext ->
  RequestDetails ->
  LogBehavior ->
  m ()
logHTTPForST eitherResp extraLogCtx reqDetails logBehavior = do
  logger :: Logger Hasura <- asks getter
  unLogger logger $ HTTPRespExtra eitherResp extraLogCtx reqDetails (_lbResponse logBehavior)

runHTTP :: (MonadIO m) => HTTP.Manager -> HTTP.Request -> m (Either (HTTPErr a) (HTTPResp a))
runHTTP manager req = do
  res <- liftIO $ try $ HTTP.performRequest req manager
  return $ either (Left . HClient . HttpException) anyBodyParser res

data TransformableRequestError a
  = HTTPError J.Value (HTTPErr a)
  | TransformationError J.Value TransformErrorBundle
  deriving (Show)

mkRequest ::
  MonadError (TransformableRequestError a) m =>
  [HTTP.Header] ->
  HTTP.ResponseTimeout ->
  -- | the request body. It is passed as a 'BL.Bytestring' because we need to
  -- log the request size. As the logging happens outside the function, we pass
  -- it the final request body, instead of 'Value'
  LBS.ByteString ->
  Maybe RequestTransform ->
  ResolvedWebhook ->
  m RequestDetails
mkRequest headers timeout payload mRequestTransform (ResolvedWebhook webhook) =
  let body = fromMaybe J.Null $ J.decode @J.Value payload
   in case HTTP.mkRequestEither webhook of
        Left excp -> throwError $ HTTPError body (HClient $ HttpException excp)
        Right initReq ->
          let req =
                initReq & set HTTP.method "POST"
                  & set HTTP.headers headers
                  & set HTTP.body (Just payload)
                  & set HTTP.timeout timeout
              sessionVars = do
                val <- J.decode @J.Value payload
                varVal <- preview (key "event" . key "session_variables") val
                case J.fromJSON @SessionVariables varVal of
                  J.Success sessionVars' -> pure sessionVars'
                  _ -> Nothing
           in case mRequestTransform of
                Nothing ->
                  pure $ RequestDetails req (LBS.length payload) Nothing Nothing Nothing sessionVars
                Just RequestTransform {..} ->
                  let reqTransformCtx = mkReqTransformCtx webhook sessionVars templateEngine
                   in case applyRequestTransform reqTransformCtx requestFields req of
                        Left err -> throwError $ TransformationError body err
                        Right transformedReq ->
                          let transformedReqSize = HTTP.getReqSize transformedReq
                           in pure $ RequestDetails req (LBS.length payload) (Just transformedReq) (Just transformedReqSize) (Just $ reqTransformCtx req) sessionVars

invokeRequest ::
  ( MonadReader r m,
    MonadError (TransformableRequestError a) m,
    Has HTTP.Manager r,
    Has (Logger Hasura) r,
    MonadIO m,
    MonadTrace m
  ) =>
  RequestDetails ->
  Maybe ResponseTransform ->
  Maybe SessionVariables ->
  ((Either (HTTPErr a) (HTTPResp a)) -> RequestDetails -> m ()) ->
  m (HTTPResp a)
invokeRequest reqDetails@RequestDetails {..} respTransform' sessionVars logger = do
  let finalReq = fromMaybe _rdOriginalRequest _rdTransformedRequest
      reqBody = fromMaybe J.Null $ view HTTP.body finalReq >>= J.decode @J.Value
  manager <- asks getter
  -- Perform the HTTP Request
  eitherResp <- tracedHttpRequest finalReq $ runHTTP manager
  -- Log the result along with the pre/post transformation Request data
  logger eitherResp reqDetails

  resp <- eitherResp `onLeft` (throwError . HTTPError reqBody)
  case respTransform' of
    Nothing -> pure resp
    Just respTransform -> do
      case TBS.toLBS $ hrsBody resp of
        -- If we fail to decode the body then carry on without performing a transformation
        Nothing -> pure resp
        Just respBody ->
          let engine = respTransformTemplateEngine respTransform
              respTransformCtx = buildRespTransformCtx _rdReqTransformCtx sessionVars engine respBody
           in case applyResponseTransform respTransform respTransformCtx of
                Left err -> do
                  -- Log The Response Transformation Error
                  logger' :: Logger Hasura <- asks getter
                  unLogger logger' $ UnstructuredLog LevelError (TBS.fromLBS $ J.encode err)
                  -- Throw an exception with the Transformation Error
                  throwError $ HTTPError reqBody $ HOther $ T.unpack $ TE.decodeUtf8 $ LBS.toStrict $ J.encode $ J.toJSON err
                Right transformedBody -> pure $ resp {hrsBody = TBS.fromLBS transformedBody}

mkResp :: Int -> TBS.TByteString -> [HeaderConf] -> Response a
mkResp status payload headers =
  let wr = WebhookResponse payload headers status
   in ResponseHTTP wr

mkClientErr :: TBS.TByteString -> Response a
mkClientErr message =
  let cerr = ClientError message
   in ResponseError cerr

mkWebhookReq :: J.Value -> [HeaderConf] -> InvocationVersion -> WebhookRequest
mkWebhookReq payload headers = WebhookRequest payload headers

mkInvocationResp :: Maybe Int -> TBS.TByteString -> [HeaderConf] -> Response a
mkInvocationResp statusMaybe responseBody responseHeaders =
  case statusMaybe of
    Nothing -> mkClientErr responseBody
    Just status ->
      if isClientError status
        then mkClientErr responseBody
        else mkResp status responseBody responseHeaders

isClientError :: Int -> Bool
isClientError status = status >= 300

encodeHeader :: EventHeaderInfo -> HTTP.Header
encodeHeader (EventHeaderInfo hconf cache) =
  let (HeaderConf name _) = hconf
      ciname = CI.mk $ TE.encodeUtf8 name
      value = TE.encodeUtf8 cache
   in (ciname, value)

decodeHeader ::
  LogBehavior ->
  [EventHeaderInfo] ->
  (HTTP.HeaderName, BS.ByteString) ->
  HeaderConf
decodeHeader logBehavior headerInfos (hdrName, hdrVal) =
  let name = decodeBS $ CI.original hdrName
      getName ehi =
        let (HeaderConf name' _) = ehiHeaderConf ehi
         in name'
      mehi = find (\hi -> getName hi == name) headerInfos
   in case mehi of
        Nothing -> HeaderConf name (HVValue (decodeBS hdrVal))
        Just ehi -> case _lbHeader logBehavior of
          LogEnvValue -> HeaderConf name (HVValue (ehiCachedValue ehi))
          LogEnvVarname -> ehiHeaderConf ehi
  where
    decodeBS = TE.decodeUtf8With TE.lenientDecode

-- | Encodes given request headers along with our 'defaultHeaders' and returns
-- them along with the re-decoded set of headers (for logging purposes).
prepareHeaders ::
  LogBehavior ->
  [EventHeaderInfo] ->
  ([HTTP.Header], [HeaderConf])
prepareHeaders logBehavior headerInfos = (headers, logHeaders)
  where
    encodedHeaders = map encodeHeader headerInfos
    headers = addDefaultHeaders encodedHeaders
    logHeaders = map (decodeHeader logBehavior headerInfos) headers

getRetryAfterHeaderFromHTTPErr :: HTTPErr a -> Maybe Text
getRetryAfterHeaderFromHTTPErr (HStatus resp) = getRetryAfterHeaderFromResp resp
getRetryAfterHeaderFromHTTPErr _ = Nothing

getRetryAfterHeaderFromResp :: HTTPResp a -> Maybe Text
getRetryAfterHeaderFromResp resp =
  let mHeader =
        find
          (\(HeaderConf name _) -> CI.mk name == retryAfterHeader)
          (hrsHeaders resp)
   in case mHeader of
        Just (HeaderConf _ (HVValue value)) -> Just value
        _ -> Nothing

parseRetryHeaderValue :: Text -> Maybe Int
parseRetryHeaderValue hValue =
  let seconds = readMaybe $ T.unpack hValue
   in case seconds of
        Nothing -> Nothing
        Just sec ->
          if sec > 0
            then Just sec
            else Nothing
