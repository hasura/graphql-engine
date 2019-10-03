module Hasura.Events.HTTP
  ( HTTPErr(..)
  , HTTPResp(..)
  , runHTTP
  , isNetworkError
  , isNetworkErrorHC
  , HLogger
  , mkHLogger
  , ExtraContext(..)
  ) where

import qualified Data.Aeson                    as J
import qualified Data.Aeson.Casing             as J
import qualified Data.Aeson.TH                 as J
import qualified Data.ByteString.Lazy          as B
import qualified Data.CaseInsensitive          as CI
import           Data.Either
import qualified Data.HashSet                  as Set
import qualified Data.TByteString              as TBS
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE
import qualified Data.Text.Encoding.Error      as TE
import qualified Data.Time.Clock               as Time
import qualified Network.HTTP.Client           as HTTP
import qualified Network.HTTP.Types            as HTTP
import qualified System.Log.FastLogger         as FL

import           Control.Exception             (try)
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Control.Monad.Reader          (MonadReader)
import           Data.Has
import           Hasura.Logging
import           Hasura.Prelude
import           Hasura.RQL.DDL.Headers
import           Hasura.RQL.Types.EventTrigger

type HLogger = (LogLevel, EngineLogType, J.Value) -> IO ()

data ExtraContext
  = ExtraContext
  { elEventCreatedAt :: Time.UTCTime
  , elEventId        :: EventId
  } deriving (Show, Eq)

$(J.deriveJSON (J.aesonDrop 2 J.snakeCase){J.omitNothingFields=True} ''ExtraContext)

data HTTPResp
   = HTTPResp
   { hrsStatus  :: !Int
   , hrsHeaders :: ![HeaderConf]
   , hrsBody    :: !TBS.TByteString
   } deriving (Show, Eq)

$(J.deriveToJSON (J.aesonDrop 3 J.snakeCase){J.omitNothingFields=True} ''HTTPResp)

instance ToEngineLog HTTPResp where
  toEngineLog resp = (LevelInfo, ELTEventTrigger, J.toJSON resp )

mkHTTPResp :: HTTP.Response B.ByteString -> HTTPResp
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
  , _hreContext  :: Maybe ExtraContext
  }

$(J.deriveToJSON (J.aesonDrop 4 J.snakeCase){J.omitNothingFields=True} ''HTTPRespExtra)

instance ToEngineLog HTTPRespExtra where
  toEngineLog resp = (LevelInfo, ELTEventTrigger, J.toJSON resp )

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
instance ToEngineLog HTTPErr where
  toEngineLog err = (LevelError, ELTEventTrigger, J.toJSON err )

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

anyBodyParser :: HTTP.Response B.ByteString -> Either HTTPErr HTTPResp
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

instance ToEngineLog  HTTPReq where
  toEngineLog req = (LevelInfo, ELTEventTrigger, J.toJSON req )

runHTTP
  :: ( MonadReader r m
     , MonadIO m
     , Has HLogger r
     , Has HTTP.Manager r
     )
  => HTTP.Request -> Maybe ExtraContext -> m (Either HTTPErr HTTPResp)
runHTTP req exLog = do
  (logF:: HLogger) <- asks getter
  manager <- asks getter
  res <- liftIO $ try $ HTTP.httpLbs req manager
  case res of
    Left e -> liftIO $ logF $ toEngineLog $ HClient e
    Right resp -> liftIO $ logF $ toEngineLog $ HTTPRespExtra (mkHTTPResp resp) exLog
  return $ either (Left . HClient) anyBodyParser res

mkHLogger :: LoggerCtx -> HLogger
mkHLogger (LoggerCtx loggerSet serverLogLevel timeGetter enabledLogs) (logLevel, logTy, logDet) = do
  localTime <- timeGetter
  when (logLevel >= serverLogLevel && logTy `Set.member` enabledLogs) $
    FL.pushLogStrLn loggerSet $ FL.toLogStr $
    J.encode $ EngineLog localTime logLevel logTy logDet
