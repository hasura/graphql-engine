module Hasura.Eventing.HTTP
  ( HTTPErr(..)
  , HTTPResp(..)
  , tryWebhook
  , runHTTP
  , isNetworkError
  , isNetworkErrorHC
  , ExtraLogContext(..)
  , EventId
  , Invocation(..)
  , Version
  , Response(..)
  , WebhookRequest(..)
  , WebhookResponse(..)
  , ClientError(..)
  , isClientError
  , mkClientErr
  , TriggerMetadata(..)
  , DeliveryInfo(..)
  , logHTTPErr
  , mkWebhookReq
  , mkResp
  , toInt64
  , LogEnvHeaders
  , encodeHeader
  , decodeHeader
  , getRetryAfterHeaderFromHTTPErr
  , getRetryAfterHeaderFromResp
  , parseRetryHeaderValue
  ) where

import qualified Data.Aeson                    as J
import qualified Data.Aeson.Casing             as J
import qualified Data.Aeson.TH                 as J
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.CaseInsensitive          as CI
import qualified Data.TByteString              as TBS
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE
import qualified Data.Text.Encoding.Error      as TE
import qualified Hasura.Logging                as L
import qualified Network.HTTP.Client           as HTTP
import qualified Network.HTTP.Types            as HTTP

import           Control.Exception             (try)
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Control.Monad.Reader          (MonadReader)
import           Data.Either
import           Data.Has
import           Data.Int                      (Int64)
import           Hasura.Logging
import           Hasura.Prelude
import           Hasura.RQL.DDL.Headers
import           Hasura.RQL.Types.EventTrigger

type LogEnvHeaders = Bool

retryAfterHeader :: CI.CI T.Text
retryAfterHeader = "Retry-After"

data WebhookRequest
  = WebhookRequest
  { _rqPayload :: J.Value
  , _rqHeaders :: Maybe [HeaderConf]
  , _rqVersion :: T.Text
  }
$(J.deriveToJSON (J.aesonDrop 3 J.snakeCase){J.omitNothingFields=True} ''WebhookRequest)

data WebhookResponse
  = WebhookResponse
  { _wrsBody    :: TBS.TByteString
  , _wrsHeaders :: Maybe [HeaderConf]
  , _wrsStatus  :: Int
  }
$(J.deriveToJSON (J.aesonDrop 4 J.snakeCase){J.omitNothingFields=True} ''WebhookResponse)

newtype ClientError =  ClientError { _ceMessage :: TBS.TByteString}
$(J.deriveToJSON (J.aesonDrop 3 J.snakeCase){J.omitNothingFields=True} ''ClientError)

type Version = T.Text

data Response = ResponseType1 WebhookResponse | ResponseType2 ClientError

instance J.ToJSON Response where
  toJSON (ResponseType1 resp) = J.object
    [ "type" J..= J.String "webhook_response"
    , "data" J..= J.toJSON resp
    ]
  toJSON (ResponseType2 err ) = J.object
    [ "type" J..= J.String "client_error"
    , "data" J..= J.toJSON err
    ]

data Invocation
  = Invocation
  { iEventId  :: EventId
  , iStatus   :: Int
  , iRequest  :: WebhookRequest
  , iResponse :: Response
  }

data ExtraLogContext
  = ExtraLogContext
  { elcEventId        :: EventId
  } deriving (Show, Eq)

$(J.deriveJSON (J.aesonDrop 3 J.snakeCase){J.omitNothingFields=True} ''ExtraLogContext)

data HTTPResp
   = HTTPResp
   { hrsStatus  :: !Int
   , hrsHeaders :: ![HeaderConf]
   , hrsBody    :: !TBS.TByteString
   } deriving (Show, Eq)

$(J.deriveToJSON (J.aesonDrop 3 J.snakeCase){J.omitNothingFields=True} ''HTTPResp)

instance ToEngineLog HTTPResp Hasura where
  toEngineLog resp = (LevelInfo, eventTriggerLogType, J.toJSON resp)

data HTTPErr
  = HClient !HTTP.HttpException
  | HParse !HTTP.Status !String
  | HStatus !HTTPResp
  | HOther !String
  deriving (Show)

instance J.ToJSON HTTPErr where
  toJSON err = toObj $ case err of
    (HClient e) -> ("client", J.toJSON $ show e)
    (HParse st e) ->
      ( "parse"
      , J.toJSON (HTTP.statusCode st,  show e)
      )
    (HStatus resp) ->
      ("status", J.toJSON resp)
    (HOther e) -> ("internal", J.toJSON $ show e)
    where
      toObj :: (T.Text, J.Value) -> J.Value
      toObj (k, v) = J.object [ "type" J..= k
                              , "detail" J..= v]
-- encapsulates a http operation
instance ToEngineLog HTTPErr Hasura where
  toEngineLog err = (LevelError, eventTriggerLogType, J.toJSON err)

mkHTTPResp :: HTTP.Response LBS.ByteString -> HTTPResp
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

data HTTPRespExtra
  = HTTPRespExtra
  { _hreResponse :: HTTPResp
  , _hreContext  :: Maybe ExtraLogContext
  }

$(J.deriveToJSON (J.aesonDrop 4 J.snakeCase){J.omitNothingFields=True} ''HTTPRespExtra)

instance ToEngineLog HTTPRespExtra Hasura where
  toEngineLog resp = (LevelInfo, eventTriggerLogType, J.toJSON resp)

isNetworkError :: HTTPErr -> Bool
isNetworkError = \case
  HClient he -> isNetworkErrorHC he
  _          -> False

isNetworkErrorHC :: HTTP.HttpException -> Bool
isNetworkErrorHC = \case
  HTTP.HttpExceptionRequest _ (HTTP.ConnectionFailure _) -> True
  HTTP.HttpExceptionRequest _ HTTP.ConnectionTimeout -> True
  HTTP.HttpExceptionRequest _ HTTP.ResponseTimeout -> True
  _ -> False

anyBodyParser :: HTTP.Response LBS.ByteString -> Either HTTPErr HTTPResp
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
  , _hrqPayload :: !(Maybe J.Value)
  , _hrqTry     :: !Int
  , _hrqDelay   :: !(Maybe Int)
  } deriving (Show, Eq)

$(J.deriveJSON (J.aesonDrop 4 J.snakeCase){J.omitNothingFields=True} ''HTTPReq)

instance ToEngineLog HTTPReq Hasura where
  toEngineLog req = (LevelInfo, eventTriggerLogType, J.toJSON req)

runHTTP
  :: ( MonadReader r m
     , Has (Logger Hasura) r
     , MonadIO m
     )
  => HTTP.Manager -> HTTP.Request -> Maybe ExtraLogContext -> m (Either HTTPErr HTTPResp)
runHTTP manager req exLog = do
  logger :: Logger Hasura <- asks getter
  res <- liftIO $ try $ HTTP.httpLbs req manager
  case res of
    Left e     -> unLogger logger $ HClient e
    Right resp -> unLogger logger $ HTTPRespExtra (mkHTTPResp resp) exLog
  return $ either (Left . HClient) anyBodyParser res

tryWebhook ::
  ( MonadReader r m
  , Has HTTP.Manager r
  , Has (L.Logger L.Hasura) r
  , MonadIO m
  , MonadError HTTPErr m
  )
  => [HTTP.Header]
  -> HTTP.ResponseTimeout
  -> J.Value
  -> String
  -> Maybe ExtraLogContext
  -> m HTTPResp
tryWebhook headers timeout payload webhook extraLogCtx = do
  initReqE <- liftIO $ try $ HTTP.parseRequest webhook
  manager <- asks getter
  case initReqE of
    Left excp -> throwError $ HClient excp
    Right initReq -> do
      let req =
            initReq
              { HTTP.method = "POST"
              , HTTP.requestHeaders = headers
              , HTTP.requestBody = HTTP.RequestBodyLBS (J.encode payload)
              , HTTP.responseTimeout = timeout
              }
      eitherResp <- runHTTP manager req extraLogCtx
      onLeft eitherResp throwError

data TriggerMetadata
  = TriggerMetadata { tmName :: TriggerName }
  deriving (Show, Eq)

$(J.deriveJSON (J.aesonDrop 2 J.snakeCase){J.omitNothingFields=True} ''TriggerMetadata)

data DeliveryInfo
  = DeliveryInfo
  { diCurrentRetry :: Int
  , diMaxRetries   :: Int
  } deriving (Show, Eq)

$(J.deriveJSON (J.aesonDrop 2 J.snakeCase){J.omitNothingFields=True} ''DeliveryInfo)

mkResp :: Int -> TBS.TByteString -> [HeaderConf] -> Response
mkResp status payload headers =
  let wr = WebhookResponse payload (mkMaybe headers) status
  in ResponseType1 wr

mkClientErr :: TBS.TByteString -> Response
mkClientErr message =
  let cerr = ClientError message
  in ResponseType2 cerr

mkWebhookReq :: J.Value -> [HeaderConf] -> Version -> WebhookRequest
mkWebhookReq payload headers = WebhookRequest payload (mkMaybe headers)

isClientError :: Int -> Bool
isClientError status = status >= 1000

mkMaybe :: [a] -> Maybe [a]
mkMaybe [] = Nothing
mkMaybe x  = Just x

logHTTPErr
  :: ( MonadReader r m
     , Has (L.Logger L.Hasura) r
     , MonadIO m
     )
  => HTTPErr -> m ()
logHTTPErr err = do
  logger :: L.Logger L.Hasura <- asks getter
  L.unLogger logger err

toInt64 :: (Integral a) => a -> Int64
toInt64 = fromIntegral

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

getRetryAfterHeaderFromHTTPErr :: HTTPErr -> Maybe Text
getRetryAfterHeaderFromHTTPErr (HStatus resp) = getRetryAfterHeaderFromResp resp
getRetryAfterHeaderFromHTTPErr _              = Nothing

getRetryAfterHeaderFromResp :: HTTPResp -> Maybe Text
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
