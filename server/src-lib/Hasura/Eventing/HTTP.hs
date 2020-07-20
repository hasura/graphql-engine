{-|
  = Hasura.Eventing.HTTP

  This module is an utility module providing HTTP utilities for
  "Hasura.Eventing.EventTriggers" and "Hasura.Eventing.ScheduledTriggers".

  The event triggers and scheduled triggers share the event delivery
  mechanism using the 'tryWebhook' function defined in this module.
-}
module Hasura.Eventing.HTTP
  ( HTTPErr(..)
  , HTTPResp(..)
  , tryWebhook
  , runHTTP
  , isNetworkError
  , isNetworkErrorHC
  , logHTTPForET
  , logHTTPForST
  , ExtraLogContext(..)
  , EventId
  , Invocation(..)
  , InvocationVersion
  , Response(..)
  , WebhookRequest(..)
  , WebhookResponse(..)
  , ClientError(..)
  , isClientError
  , mkClientErr
  , mkWebhookReq
  , mkResp
  , LogEnvHeaders
  , encodeHeader
  , decodeHeader
  , getRetryAfterHeaderFromHTTPErr
  , getRetryAfterHeaderFromResp
  , parseRetryHeaderValue
  , TriggerTypes(..)
  , invocationVersionET
  , invocationVersionST
  ) where

import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.CaseInsensitive          as CI
import qualified Data.TByteString              as TBS
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE
import qualified Data.Text.Encoding.Error      as TE
import qualified Network.HTTP.Client           as HTTP
import qualified Network.HTTP.Types            as HTTP
import qualified Data.Time.Clock               as Time

import           Control.Exception             (try)
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.Either
import           Data.Has
import           Hasura.Logging
import           Hasura.Prelude
import           Hasura.RQL.DDL.Headers
import           Hasura.RQL.Types.EventTrigger
import           Hasura.Tracing

type LogEnvHeaders = Bool

retryAfterHeader :: CI.CI T.Text
retryAfterHeader = "Retry-After"

data WebhookRequest
  = WebhookRequest
  { _rqPayload :: Value
  , _rqHeaders :: [HeaderConf]
  , _rqVersion :: T.Text
  }
$(deriveToJSON (aesonDrop 3 snakeCase){omitNothingFields=True} ''WebhookRequest)

data WebhookResponse
  = WebhookResponse
  { _wrsBody    :: TBS.TByteString
  , _wrsHeaders :: [HeaderConf]
  , _wrsStatus  :: Int
  }
$(deriveToJSON (aesonDrop 4 snakeCase){omitNothingFields=True} ''WebhookResponse)

newtype ClientError =  ClientError { _ceMessage :: TBS.TByteString}
$(deriveToJSON (aesonDrop 3 snakeCase){omitNothingFields=True} ''ClientError)

type InvocationVersion = T.Text

invocationVersionET :: InvocationVersion
invocationVersionET = "2"

invocationVersionST :: InvocationVersion
invocationVersionST = "1"

-- | There are two types of events: EventType (for event triggers) and ScheduledType (for scheduled triggers)
data TriggerTypes = EventType | ScheduledType

data Response (a :: TriggerTypes) =
  ResponseHTTP WebhookResponse | ResponseError ClientError

instance ToJSON (Response 'EventType) where
  toJSON (ResponseHTTP resp) = object
    [ "type" .= String "webhook_response"
    , "data" .= toJSON resp
    , "version" .= invocationVersionET
    ]
  toJSON (ResponseError err) = object
    [ "type" .= String "client_error"
    , "data" .= toJSON err
    , "version" .= invocationVersionET
    ]

instance ToJSON (Response 'ScheduledType) where
  toJSON (ResponseHTTP resp) = object
    [ "type" .= String "webhook_response"
    , "data" .= toJSON resp
    , "version" .= invocationVersionST
    ]
  toJSON (ResponseError err) = object
    [ "type" .= String "client_error"
    , "data" .= toJSON err
    , "version" .= invocationVersionST
    ]


data Invocation (a :: TriggerTypes)
  = Invocation
  { iEventId  :: EventId
  , iStatus   :: Int
  , iRequest  :: WebhookRequest
  , iResponse :: (Response a)
  }

data ExtraLogContext
  = ExtraLogContext
  { elEventCreatedAt :: Maybe Time.UTCTime
  , elEventId        :: EventId
  } deriving (Show, Eq)

$(deriveJSON (aesonDrop 2 snakeCase){omitNothingFields=True} ''ExtraLogContext)

data HTTPResp (a :: TriggerTypes)
   = HTTPResp
   { hrsStatus  :: !Int
   , hrsHeaders :: ![HeaderConf]
   , hrsBody    :: !TBS.TByteString
   } deriving (Show, Eq)

$(deriveToJSON (aesonDrop 3 snakeCase){omitNothingFields=True} ''HTTPResp)

instance ToEngineLog (HTTPResp 'EventType) Hasura where
  toEngineLog resp = (LevelInfo, eventTriggerLogType, toJSON resp)

instance ToEngineLog (HTTPResp 'ScheduledType) Hasura where
  toEngineLog resp = (LevelInfo, scheduledTriggerLogType, toJSON resp)

data HTTPErr (a :: TriggerTypes)
  = HClient !HTTP.HttpException
  | HParse !HTTP.Status !String
  | HStatus !(HTTPResp a)
  | HOther !String
  deriving (Show)

instance ToJSON (HTTPErr a) where
  toJSON err = toObj $ case err of
    (HClient e) -> ("client", toJSON $ show e)
    (HParse st e) ->
      ( "parse"
      , toJSON (HTTP.statusCode st,  show e)
      )
    (HStatus resp) ->
      ("status", toJSON resp)
    (HOther e) -> ("internal", toJSON $ show e)
    where
      toObj :: (T.Text, Value) -> Value
      toObj (k, v) = object [ "type" .= k
                            , "detail" .= v]

instance ToEngineLog (HTTPErr 'EventType) Hasura where
  toEngineLog err = (LevelError, eventTriggerLogType, toJSON err)

instance ToEngineLog (HTTPErr 'ScheduledType) Hasura where
  toEngineLog err = (LevelError, scheduledTriggerLogType, toJSON err)

mkHTTPResp :: HTTP.Response LBS.ByteString -> HTTPResp a
mkHTTPResp resp =
  HTTPResp
  { hrsStatus = HTTP.statusCode $ HTTP.responseStatus resp
  , hrsHeaders = map decodeHeader $ HTTP.responseHeaders resp
  , hrsBody = TBS.fromLBS $ HTTP.responseBody resp
  }
  where
    decodeBS = TE.decodeUtf8With TE.lenientDecode
    decodeHeader (hdrName, hdrVal)
      = HeaderConf (decodeBS $ CI.original hdrName) (HVValue (decodeBS hdrVal))

data HTTPRespExtra (a :: TriggerTypes)
  = HTTPRespExtra
  { _hreResponse :: Either (HTTPErr a) (HTTPResp a)
  , _hreContext  :: ExtraLogContext
  }

instance ToJSON (HTTPRespExtra a) where
  toJSON (HTTPRespExtra resp ctxt) = do
    case resp of
      Left errResp ->
        object [ "response" .= toJSON errResp
               , "context" .= toJSON ctxt
               ]
      Right rsp ->
        object [ "response" .= toJSON rsp
               , "context" .= toJSON ctxt
               ]

instance ToEngineLog (HTTPRespExtra 'EventType) Hasura where
  toEngineLog resp = (LevelInfo, eventTriggerLogType, toJSON resp)

instance ToEngineLog (HTTPRespExtra 'ScheduledType) Hasura where
  toEngineLog resp = (LevelInfo, scheduledTriggerLogType, toJSON resp)

isNetworkError :: HTTPErr a -> Bool
isNetworkError = \case
  HClient he -> isNetworkErrorHC he
  _          -> False

isNetworkErrorHC :: HTTP.HttpException -> Bool
isNetworkErrorHC = \case
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

data HTTPReq
  = HTTPReq
  { _hrqMethod  :: !String
  , _hrqUrl     :: !String
  , _hrqPayload :: !(Maybe Value)
  , _hrqTry     :: !Int
  , _hrqDelay   :: !(Maybe Int)
  } deriving (Show, Eq)

$(deriveJSON (aesonDrop 4 snakeCase){omitNothingFields=True} ''HTTPReq)

instance ToEngineLog HTTPReq Hasura where
  toEngineLog req = (LevelInfo, eventTriggerLogType, toJSON req)

logHTTPForET
  :: ( MonadReader r m
     , Has (Logger Hasura) r
     , MonadIO m
     )
  => Either (HTTPErr 'EventType) (HTTPResp 'EventType) -> ExtraLogContext -> m ()
logHTTPForET eitherResp extraLogCtx = do
  logger :: Logger Hasura <- asks getter
  unLogger logger $ HTTPRespExtra eitherResp extraLogCtx

logHTTPForST
  :: ( MonadReader r m
     , Has (Logger Hasura) r
     , MonadIO m
     )
  => Either (HTTPErr 'ScheduledType) (HTTPResp 'ScheduledType) -> ExtraLogContext -> m ()
logHTTPForST eitherResp extraLogCtx = do
  logger :: Logger Hasura <- asks getter
  unLogger logger $ HTTPRespExtra eitherResp extraLogCtx

runHTTP :: (MonadIO m) => HTTP.Manager -> HTTP.Request -> m (Either (HTTPErr a) (HTTPResp a))
runHTTP manager req = do
  res <- liftIO $ try $ HTTP.httpLbs req manager
  return $ either (Left . HClient) anyBodyParser res

tryWebhook ::
  ( MonadReader r m
  , Has HTTP.Manager r
  , MonadIO m
  , MonadError (HTTPErr a) m
  , MonadTrace m
  )
  => [HTTP.Header]
  -> HTTP.ResponseTimeout
  -> Value
  -> String
  -> m (HTTPResp a)
tryWebhook headers timeout payload webhook = traceHttpRequest (T.pack webhook) do
  initReqE <- liftIO $ try $ HTTP.parseRequest webhook
  manager <- asks getter
  case initReqE of
    Left excp -> throwError $ HClient excp
    Right initReq -> do
      let req =
            initReq
              { HTTP.method = "POST"
              , HTTP.requestHeaders = headers
              , HTTP.requestBody = HTTP.RequestBodyLBS (encode payload)
              , HTTP.responseTimeout = timeout
              }
      pure $ SuspendedRequest req \req' -> do
        eitherResp <- runHTTP manager req'
        onLeft eitherResp throwError

mkResp :: Int -> TBS.TByteString -> [HeaderConf] -> Response a
mkResp status payload headers =
  let wr = WebhookResponse payload headers status
  in ResponseHTTP wr

mkClientErr :: TBS.TByteString -> Response a
mkClientErr message =
  let cerr = ClientError message
  in ResponseError cerr

mkWebhookReq :: Value -> [HeaderConf] -> InvocationVersion -> WebhookRequest
mkWebhookReq payload headers = WebhookRequest payload headers

isClientError :: Int -> Bool
isClientError status = status >= 1000

encodeHeader :: EventHeaderInfo -> HTTP.Header
encodeHeader (EventHeaderInfo hconf cache) =
  let (HeaderConf name _) = hconf
      ciname = CI.mk $ TE.encodeUtf8 name
      value = TE.encodeUtf8 cache
   in (ciname, value)

decodeHeader
  :: LogEnvHeaders -> [EventHeaderInfo] -> (HTTP.HeaderName, BS.ByteString)
  -> HeaderConf
decodeHeader logenv headerInfos (hdrName, hdrVal)
  = let name = decodeBS $ CI.original hdrName
        getName ehi = let (HeaderConf name' _) = ehiHeaderConf ehi
                      in name'
        mehi = find (\hi -> getName hi == name) headerInfos
    in case mehi of
         Nothing -> HeaderConf name (HVValue (decodeBS hdrVal))
         Just ehi -> if logenv
                     then HeaderConf name (HVValue (ehiCachedValue ehi))
                     else ehiHeaderConf ehi
   where
     decodeBS = TE.decodeUtf8With TE.lenientDecode

getRetryAfterHeaderFromHTTPErr :: HTTPErr a -> Maybe Text
getRetryAfterHeaderFromHTTPErr (HStatus resp) = getRetryAfterHeaderFromResp resp
getRetryAfterHeaderFromHTTPErr _              = Nothing

getRetryAfterHeaderFromResp :: HTTPResp a -> Maybe Text
getRetryAfterHeaderFromResp resp =
  let mHeader =
        find
          (\(HeaderConf name _) -> CI.mk name == retryAfterHeader)
          (hrsHeaders resp)
   in case mHeader of
        Just (HeaderConf _ (HVValue value)) -> Just value
        _                                   -> Nothing

parseRetryHeaderValue :: T.Text -> Maybe Int
parseRetryHeaderValue hValue =
  let seconds = readMaybe $ T.unpack hValue
   in case seconds of
        Nothing -> Nothing
        Just sec ->
          if sec > 0
            then Just sec
            else Nothing
