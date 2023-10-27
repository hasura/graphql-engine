module Hasura.Backends.DataConnector.Logging
  ( logAgentRequest,
    logClientError,
  )
where

import Control.Lens ((^.), (^?))
import Data.Aeson (object, (.=))
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.CaseInsensitive qualified as CI
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Encoding.Error (lenientDecode)
import Hasura.HTTP qualified
import Hasura.Logging (EngineLogType (..), Hasura, LogLevel (..), Logger (..), ToEngineLog (..))
import Hasura.Prelude
import Hasura.Tracing (MonadTrace)
import Hasura.Tracing qualified as Tracing
import Hasura.Tracing.TraceId (spanIdToHex, traceIdToHex)
import Network.HTTP.Client.Transformable qualified as HTTP
import Servant.Client (ClientError (..), responseStatusCode, showBaseUrl)
import Servant.Client.Core (RequestF (..))

data RequestLogInfo = RequestLogInfo
  { _rliRequestMethod :: Text,
    _rliRequestUri :: Text,
    _rliRequestHeaders :: KeyMap Text,
    _rliRequestBody :: Maybe Text
  }
  deriving stock (Show, Eq)

data AgentCommunicationLog = AgentCommunicationLog
  { _aclRequest :: Maybe RequestLogInfo,
    _aclResponseStatusCode :: Maybe Int,
    _aclError :: Maybe Text,
    _aclTraceId :: Text,
    _aclSpanId :: Text
  }
  deriving stock (Show, Eq)

instance ToEngineLog AgentCommunicationLog Hasura where
  toEngineLog AgentCommunicationLog {..} =
    (LevelDebug, ELTDataConnectorLog, logJson)
    where
      logJson =
        object
          $ catMaybes
            [ ("requestMethod" .=) . _rliRequestMethod <$> _aclRequest,
              ("requestUri" .=) . _rliRequestUri <$> _aclRequest,
              ("requestHeaders" .=) . _rliRequestHeaders <$> _aclRequest,
              ("requestBody" .=) <$> (_rliRequestBody =<< _aclRequest),
              ("responseStatusCode" .=) <$> _aclResponseStatusCode,
              ("error" .=) <$> _aclError,
              Just $ "traceId" .= _aclTraceId,
              Just $ "spanId" .= _aclSpanId
            ]

logAgentRequest :: (MonadIO m, MonadTrace m) => Logger Hasura -> HTTP.Request -> Either HTTP.HttpException (HTTP.Response BSL.ByteString) -> m ()
logAgentRequest (LoggerTracing writeLog) req responseOrError = do
  traceCtx <- Tracing.currentContext
  let _aclRequest = Just $ extractRequestLogInfoFromClientRequest req
      _aclResponseStatusCode = case responseOrError of
        Right response -> Just . HTTP.statusCode $ HTTP.responseStatus response
        Left httpExn -> Hasura.HTTP.getHTTPExceptionStatus $ Hasura.HTTP.HttpException httpExn
      _aclError = either (Just . Hasura.HTTP.serializeHTTPExceptionMessageForDebugging) (const Nothing) responseOrError
      _aclTraceId = maybe "" (bsToTxt . traceIdToHex . Tracing.tcCurrentTrace) traceCtx
      _aclSpanId = maybe "" (bsToTxt . spanIdToHex . Tracing.tcCurrentSpan) traceCtx
  writeLog AgentCommunicationLog {..}

extractRequestLogInfoFromClientRequest :: HTTP.Request -> RequestLogInfo
extractRequestLogInfoFromClientRequest req =
  let _rliRequestMethod = req ^. HTTP.method & fromUtf8
      _rliRequestUri = req ^. HTTP.url
      _rliRequestPath = req ^. HTTP.path & fromUtf8
      _rliRequestHeaders = req ^. HTTP.headers & headersToKeyMap
      -- NOTE: We cannot decode IO based body types.
      _rliRequestBody = req ^? (HTTP.body . HTTP._RequestBodyLBS) <&> (BSL.toStrict >>> fromUtf8)
   in RequestLogInfo {..}

logClientError :: (MonadIO m, MonadTrace m) => Logger Hasura -> ClientError -> m ()
logClientError (LoggerTracing writeLog) clientError = do
  traceCtx <- Tracing.currentContext
  let _aclResponseStatusCode = case clientError of
        FailureResponse _ response -> Just . HTTP.statusCode $ responseStatusCode response
        _ -> Nothing
      _aclRequest = extractRequestLogInfoFromClientInfo clientError
      _aclError = Just $ Hasura.HTTP.serializeServantClientErrorMessageForDebugging clientError
      _aclTraceId = maybe "" (bsToTxt . traceIdToHex . Tracing.tcCurrentTrace) traceCtx
      _aclSpanId = maybe "" (bsToTxt . spanIdToHex . Tracing.tcCurrentSpan) traceCtx
  writeLog AgentCommunicationLog {..}

extractRequestLogInfoFromClientInfo :: ClientError -> Maybe RequestLogInfo
extractRequestLogInfoFromClientInfo = \case
  FailureResponse request _ ->
    let _rliRequestMethod = requestMethod request & fromUtf8
        (baseUrl, path') = requestPath request
        _rliRequestUri = Text.pack (showBaseUrl baseUrl) <> fromUtf8 path'
        _rliRequestHeaders = headersToKeyMap . toList $ requestHeaders request
        _rliRequestBody = Nothing
     in Just RequestLogInfo {..}
  _ -> Nothing

headersToKeyMap :: [HTTP.Header] -> KeyMap Text
headersToKeyMap headers' =
  headers'
    <&> (\(name, value) -> (K.fromText . fromUtf8 $ CI.original name, fromUtf8 value))
    & KM.fromList

fromUtf8 :: BS.ByteString -> Text
fromUtf8 = Text.decodeUtf8With lenientDecode
