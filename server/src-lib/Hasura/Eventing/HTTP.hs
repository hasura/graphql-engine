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
    httpExceptionErrorEncoding,
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
import Control.Lens (preview, set, (.~))
import Data.Aeson qualified as J
import Data.Aeson.Encoding qualified as JE
import Data.Aeson.Key qualified as J
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Lens
import Data.ByteString qualified as BS
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.CaseInsensitive qualified as CI
import Data.Either
import Data.Has
import Data.Int (Int64)
import Data.SerializableBlob qualified as SB
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TE
import Data.URL.Template (mkPlainTemplate, printTemplate)
import Hasura.HTTP
import Hasura.Logging
import Hasura.Prelude
import Hasura.RQL.DDL.Webhook.Transform qualified as Transform
import Hasura.RQL.Types.Common (ResolvedWebhook (..))
import Hasura.RQL.Types.EventTrigger
import Hasura.RQL.Types.Eventing
import Hasura.RQL.Types.Headers
import Hasura.Server.Types (TriggersErrorLogLevelStatus, isTriggersErrorLogLevelEnabled)
import Hasura.Session (SessionVariables)
import Hasura.Tracing
import Network.HTTP.Client.Transformable qualified as HTTP

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
    hrsBody :: !SB.SerializableBlob,
    hrsSize :: !Int64
  }
  deriving (Generic, Show)

instance J.ToJSON (HTTPResp a) where
  toJSON = J.genericToJSON hasuraJSON {J.omitNothingFields = True}
  toEncoding = J.genericToEncoding hasuraJSON {J.omitNothingFields = True}

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
      ("client", getHttpExceptionJson (ShowErrorInfo True) httpException)
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

-- similar to Aeson.encode function which uses `getHttpExceptionJson` function instead of ToJSON instance of
-- HttpException
httpExceptionErrorEncoding :: HttpException -> ByteString
httpExceptionErrorEncoding = JE.encodingToLazyByteString . JE.value . (getHttpExceptionJson (ShowErrorInfo True))

instance ToEngineLog (HTTPErr 'EventType) Hasura where
  toEngineLog err = (LevelError, eventTriggerLogType, J.toJSON err)

instance ToEngineLog (HTTPErr 'ScheduledType) Hasura where
  toEngineLog err = (LevelError, scheduledTriggerLogType, J.toJSON err)

mkHTTPResp :: HTTP.Response LBS.ByteString -> HTTPResp a
mkHTTPResp resp =
  HTTPResp
    { hrsStatus = HTTP.statusCode $ HTTP.responseStatus resp,
      hrsHeaders = map decodeHeader' $ HTTP.responseHeaders resp,
      hrsBody = SB.fromLBS respBody,
      hrsSize = LBS.length respBody
    }
  where
    respBody = HTTP.responseBody resp
    decodeBS = TE.decodeUtf8With TE.lenientDecode
    decodeHeader' (hdrName, hdrVal) =
      HeaderConf (decodeBS $ CI.original hdrName) (HVValue $ mkPlainTemplate (decodeBS hdrVal))

data RequestDetails = RequestDetails
  { _rdOriginalRequest :: HTTP.Request,
    _rdOriginalSize :: Int64,
    _rdTransformedRequest :: Maybe HTTP.Request,
    _rdTransformedSize :: Maybe Int64,
    _rdReqTransformCtx :: Maybe Transform.RequestContext,
    _rdSessionVars :: Maybe SessionVariables
  }
  deriving (Generic)

extractRequest :: RequestDetails -> HTTP.Request
extractRequest RequestDetails {..} = fromMaybe _rdOriginalRequest _rdTransformedRequest

instance J.ToJSON RequestDetails where
  toJSON = J.genericToJSON hasuraJSON
  toEncoding = J.genericToEncoding hasuraJSON

data HTTPRespExtra (a :: TriggerTypes) = HTTPRespExtra
  { _hreResponse :: !(Either (HTTPErr a) (HTTPResp a)),
    _hreContext :: !ExtraLogContext,
    _hreRequest :: !RequestDetails,
    _hreWebhookVarName :: !Text,
    _hreLogHeaders :: ![HeaderConf]
  }

instance J.ToJSON (HTTPRespExtra a) where
  toJSON (HTTPRespExtra resp ctxt req webhookVarName logHeaders) =
    case resp of
      Left errResp ->
        J.object
          $ [ "response" J..= J.toJSON errResp,
              "request" J..= sanitiseReqJSON req,
              "event_id" J..= elEventId ctxt
            ]
          ++ eventName
      Right okResp ->
        J.object
          $ [ "response" J..= J.toJSON okResp,
              "request" J..= J.toJSON req,
              "event_id" J..= elEventId ctxt
            ]
          ++ eventName
    where
      eventName = case elEventName ctxt of
        Just name -> ["event_name" J..= name]
        Nothing -> []
      getValue val = case val of
        HVValue txt -> J.String (printTemplate txt)
        HVEnv txt -> J.String txt
      getRedactedHeaders =
        J.Object
          $ foldr (\(HeaderConf name val) -> KM.insert (J.fromText name) (getValue val)) mempty logHeaders
      updateReqDetail v reqType =
        let webhookRedactedReq = J.toJSON v & key reqType . key "url" .~ J.String webhookVarName
            redactedReq = webhookRedactedReq & key reqType . key "headers" .~ getRedactedHeaders
         in redactedReq
      -- redact the resolved webhook and headers value, this helps in not logging
      -- sensitive info
      sanitiseReqJSON v = case _rdTransformedRequest v of
        Nothing -> updateReqDetail v "original_request"
        Just _ -> updateReqDetail v "transformed_request"

data HTTPRespExtraLog a = HTTPRespExtraLog {_hrelLevel :: !LogLevel, _hrelpayload :: HTTPRespExtra a}

instance ToEngineLog (HTTPRespExtraLog 'EventType) Hasura where
  toEngineLog (HTTPRespExtraLog level resp) = (level, eventTriggerLogType, J.toJSON resp)

instance ToEngineLog (HTTPRespExtraLog 'ScheduledType) Hasura where
  toEngineLog (HTTPRespExtraLog level resp) = (level, scheduledTriggerLogType, J.toJSON resp)

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
  deriving (Show, Generic, Eq)

instance J.ToJSON HTTPReq where
  toJSON = J.genericToJSON hasuraJSON {J.omitNothingFields = True}
  toEncoding = J.genericToEncoding hasuraJSON {J.omitNothingFields = True}

instance ToEngineLog HTTPReq Hasura where
  toEngineLog req = (LevelInfo, eventTriggerLogType, J.toJSON req)

logHTTPForTriggers ::
  ( MonadReader r m,
    Has (Logger Hasura) r,
    MonadIO m,
    MonadTraceContext m,
    ToEngineLog (HTTPRespExtraLog a) Hasura
  ) =>
  Either (HTTPErr a) (HTTPResp a) ->
  ExtraLogContext ->
  RequestDetails ->
  Text ->
  [HeaderConf] ->
  TriggersErrorLogLevelStatus ->
  m ()
logHTTPForTriggers eitherResp extraLogCtx reqDetails webhookVarName logHeaders triggersErrorLogLevelStatus = do
  logger :: Logger Hasura <- asks getter
  case (eitherResp, isTriggersErrorLogLevelEnabled triggersErrorLogLevelStatus) of
    (Left _, True) -> unLoggerTracing logger $ HTTPRespExtraLog LevelError $ HTTPRespExtra eitherResp extraLogCtx reqDetails webhookVarName logHeaders
    (_, _) -> unLoggerTracing logger $ HTTPRespExtraLog LevelInfo $ HTTPRespExtra eitherResp extraLogCtx reqDetails webhookVarName logHeaders

logHTTPForET ::
  ( MonadReader r m,
    MonadTraceContext m,
    Has (Logger Hasura) r,
    MonadIO m
  ) =>
  Either (HTTPErr 'EventType) (HTTPResp 'EventType) ->
  ExtraLogContext ->
  RequestDetails ->
  Text ->
  [HeaderConf] ->
  TriggersErrorLogLevelStatus ->
  m ()
logHTTPForET = logHTTPForTriggers

logHTTPForST ::
  ( MonadReader r m,
    MonadTraceContext m,
    Has (Logger Hasura) r,
    MonadIO m
  ) =>
  Either (HTTPErr 'ScheduledType) (HTTPResp 'ScheduledType) ->
  ExtraLogContext ->
  RequestDetails ->
  Text ->
  [HeaderConf] ->
  TriggersErrorLogLevelStatus ->
  m ()
logHTTPForST = logHTTPForTriggers

runHTTP :: (MonadIO m) => HTTP.Manager -> HTTP.Request -> m (Either (HTTPErr a) (HTTPResp a))
runHTTP manager req = do
  res <- liftIO $ try $ HTTP.httpLbs req manager
  return $ either (Left . HClient . HttpException) anyBodyParser res

data TransformableRequestError a
  = HTTPError J.Value (HTTPErr a)
  | TransformationError J.Value Transform.TransformErrorBundle
  deriving (Show)

mkRequest ::
  (MonadError (TransformableRequestError a) m) =>
  [HTTP.Header] ->
  HTTP.ResponseTimeout ->
  -- | the request body. It is passed as a 'BL.Bytestring' because we need to
  -- log the request size. As the logging happens outside the function, we pass
  -- it the final request body, instead of 'Value'
  LBS.ByteString ->
  Maybe Transform.RequestTransform ->
  ResolvedWebhook ->
  m RequestDetails
mkRequest headers timeout payload mRequestTransform (ResolvedWebhook webhook) =
  let body = fromMaybe J.Null $ J.decode @J.Value payload
   in case HTTP.mkRequestEither webhook of
        Left excp -> throwError $ HTTPError body (HClient $ HttpException excp)
        Right initReq ->
          let req =
                initReq
                  & set HTTP.method "POST"
                  & set HTTP.headers headers
                  & set HTTP.body (HTTP.RequestBodyLBS payload)
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
                Just Transform.RequestTransform {..} ->
                  let reqTransformCtx = Transform.mkReqTransformCtx webhook sessionVars templateEngine
                      requestContext = fmap Transform.mkRequestContext reqTransformCtx
                   in case Transform.applyRequestTransform requestContext requestFields req of
                        Left err -> throwError $ TransformationError body err
                        Right transformedReq ->
                          let transformedReqSize = HTTP.getReqSize transformedReq
                           in pure $ RequestDetails req (LBS.length payload) (Just transformedReq) (Just transformedReqSize) (Just $ requestContext req) sessionVars

invokeRequest ::
  ( MonadReader r m,
    MonadError (TransformableRequestError a) m,
    Has HTTP.Manager r,
    Has (Logger Hasura) r,
    MonadIO m,
    MonadTrace m
  ) =>
  RequestDetails ->
  Maybe Transform.ResponseTransform ->
  Maybe SessionVariables ->
  ((Either (HTTPErr a) (HTTPResp a)) -> RequestDetails -> m ()) ->
  HttpPropagator ->
  m (HTTPResp a)
invokeRequest reqDetails@RequestDetails {..} respTransform' sessionVars logger tracesPropagator = do
  let finalReq = fromMaybe _rdOriginalRequest _rdTransformedRequest
      reqBody = fromMaybe J.Null $ preview (HTTP.body . HTTP._RequestBodyLBS) finalReq >>= J.decode @J.Value
  manager <- asks getter
  -- Perform the HTTP Request
  eitherResp <- traceHTTPRequest tracesPropagator finalReq $ runHTTP manager
  -- Log the result along with the pre/post transformation Request data
  logger eitherResp reqDetails
  resp <- eitherResp `onLeft` (throwError . HTTPError reqBody)
  case respTransform' of
    Nothing -> pure resp
    Just respTransform -> do
      let respBody = SB.toLBS $ hrsBody resp
          engine = Transform.respTransformTemplateEngine respTransform
          respTransformCtx = Transform.buildRespTransformCtx _rdReqTransformCtx sessionVars engine respBody (hrsStatus resp)
       in case Transform.applyResponseTransform respTransform respTransformCtx of
            Left err -> do
              -- Log The Response Transformation Error
              logger' :: Logger Hasura <- asks getter
              unLoggerTracing logger' $ UnstructuredLog LevelError (SB.fromLBS $ J.encode err)
              -- Throw an exception with the Transformation Error
              throwError $ HTTPError reqBody $ HOther $ T.unpack $ TE.decodeUtf8 $ LBS.toStrict $ J.encode $ J.toJSON err
            Right transformedBody -> pure $ resp {hrsBody = SB.fromLBS transformedBody}

mkResp :: Int -> SB.SerializableBlob -> [HeaderConf] -> Response a
mkResp status payload headers =
  let wr = WebhookResponse payload headers status
   in ResponseHTTP wr

mkClientErr :: SB.SerializableBlob -> Response a
mkClientErr message =
  let cerr = ClientError message
   in ResponseError cerr

mkWebhookReq :: J.Value -> [HeaderConf] -> InvocationVersion -> WebhookRequest
mkWebhookReq payload headers = WebhookRequest payload headers

mkInvocationResp :: Maybe Int -> SB.SerializableBlob -> [HeaderConf] -> Response a
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
  [EventHeaderInfo] ->
  (HTTP.HeaderName, BS.ByteString) ->
  HeaderConf
decodeHeader headerInfos (hdrName, hdrVal) =
  let name = decodeBS $ CI.original hdrName
      getName ehi =
        let (HeaderConf name' _) = ehiHeaderConf ehi
         in name'
      mehi = find (\hi -> getName hi == name) headerInfos
   in case mehi of
        Nothing -> HeaderConf name (HVValue $ mkPlainTemplate (decodeBS hdrVal))
        Just ehi -> ehiHeaderConf ehi
  where
    decodeBS = TE.decodeUtf8With TE.lenientDecode

-- | Encodes given request headers along with our 'defaultHeaders' and returns
-- them along with the re-decoded set of headers (for logging purposes).
prepareHeaders ::
  [EventHeaderInfo] ->
  ([HTTP.Header], [HeaderConf])
prepareHeaders headerInfos = (headers, logHeaders)
  where
    encodedHeaders = map encodeHeader headerInfos
    headers = addDefaultHeaders encodedHeaders
    logHeaders = map (decodeHeader headerInfos) headers

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
        Just (HeaderConf _ (HVValue value)) -> Just $ printTemplate value
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
