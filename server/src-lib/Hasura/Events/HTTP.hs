{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Hasura.Events.HTTP
  ( HTTP(..)
  , mkHTTP
  , mkAnyHTTPPost
  , mkHTTPMaybe
  , HTTPErr(..)
  , runHTTP
  , default2xxParser
  , noBody2xxParser
  , defaultRetryPolicy
  , defaultRetryFn
  , defaultParser
  , defaultParserMaybe
  , isNetworkError
  , isNetworkErrorHC
  , HLogger
  , mkHLogger
  , ExtraContext(..)
  ) where

import qualified Control.Retry              as R
import qualified Data.Aeson                 as J
import qualified Data.Aeson.Casing          as J
import qualified Data.Aeson.TH              as J
import qualified Data.ByteString.Lazy       as B
import qualified Data.CaseInsensitive       as CI
import qualified Data.TByteString           as TBS
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import qualified Data.Text.Encoding.Error   as TE
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Encoding    as TLE
import qualified Data.Time.Clock            as Time
import qualified Network.HTTP.Client        as H
import qualified Network.HTTP.Types         as N
import qualified Network.Wreq               as W
import qualified Network.Wreq.Session       as WS
import qualified System.Log.FastLogger      as FL

import           Control.Exception          (try)
import           Control.Lens
import           Control.Monad.Except       (MonadError, throwError)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader       (MonadReader)
import           Data.Has
import           Hasura.Logging
-- import           Data.Monoid
import           Hasura.Prelude
import           Hasura.RQL.Types.Subscribe

-- import           Context                  (HTTPSessionMgr (..))
-- import           Log

type HLogger = (LogLevel, EngineLogType, J.Value) -> IO ()

data ExtraContext
  = ExtraContext
  { elEventCreatedAt :: Time.UTCTime
  , elEventId        :: TriggerId
  } deriving (Show, Eq)

$(J.deriveJSON (J.aesonDrop 2 J.snakeCase){J.omitNothingFields=True} ''ExtraContext)

data HTTPErr
  = HClient !H.HttpException
  | HParse !N.Status !String
  | HStatus !N.Status TBS.TByteString
  | HOther !String
  deriving (Show)

instance J.ToJSON HTTPErr where
  toJSON err = toObj $ case err of
    (HClient e) -> ("client", J.toJSON $ show e)
    (HParse st e) ->
      ( "parse"
      , J.toJSON (N.statusCode st,  show e)
      )
    (HStatus st resp) ->
      ("status", J.toJSON (N.statusCode st, resp))
    (HOther e) -> ("internal", J.toJSON $ show e)
    where
      toObj :: (T.Text, J.Value) -> J.Value
      toObj (k, v) = J.object [ "type" J..= k
                              , "detail" J..= v]

-- encapsulates a http operation
instance ToEngineLog HTTPErr where
  toEngineLog err = (LevelError, "event-trigger", J.toJSON err )

data HTTP a
  = HTTP
  { _hMethod      :: !String
  , _hUrl         :: !String
  , _hPayload     :: !(Maybe J.Value)
  , _hFormData    :: !(Maybe [W.FormParam])
  -- options modifier
  , _hOptions     :: W.Options -> W.Options
  -- the response parser
  , _hParser      :: W.Response B.ByteString -> Either HTTPErr a
  -- should the operation be retried
  , _hRetryFn     :: Either HTTPErr a -> Bool
  -- the retry policy
  , _hRetryPolicy :: R.RetryPolicyM IO
  }

-- TODO. Why this istance?
-- instance Show (HTTP a) where
--   show (HTTP m u p _ _ _ _) = show m ++ " " ++ show u ++ " : " ++ show p

isNetworkError :: HTTPErr -> Bool
isNetworkError = \case
  HClient he -> isNetworkErrorHC he
  _          -> False

isNetworkErrorHC :: H.HttpException -> Bool
isNetworkErrorHC = \case
  H.HttpExceptionRequest _ (H.ConnectionFailure _) -> True
  H.HttpExceptionRequest _ H.ConnectionTimeout -> True
  H.HttpExceptionRequest _ H.ResponseTimeout -> True
  _ -> False

-- retries on the typical network errors
defaultRetryFn :: Either HTTPErr a -> Bool
defaultRetryFn _ = False

-- full jitter backoff
defaultRetryPolicy :: (MonadIO m) => R.RetryPolicyM m
defaultRetryPolicy =
  R.capDelay (120 * 1000 * 1000) (R.fullJitterBackoff (2 * 1000 * 1000))
  <> R.limitRetries 15

-- a helper function
respJson :: (J.FromJSON a) => W.Response B.ByteString -> Either HTTPErr a
respJson resp =
  either (Left . HParse respCode) return $
  J.eitherDecode respBody
  where
    respCode = resp ^. W.responseStatus
    respBody = resp ^. W.responseBody

defaultParser :: (J.FromJSON a) => W.Response B.ByteString -> Either HTTPErr a
defaultParser resp = if
  | respCode == N.status200 -> respJson resp
  | otherwise -> do
      let val = TBS.fromLBS $ resp ^. W.responseBody
      throwError $ HStatus respCode val
  where
    respCode = resp ^. W.responseStatus

-- like default parser but turns 404 into maybe
defaultParserMaybe
  :: (J.FromJSON a) => W.Response B.ByteString -> Either HTTPErr (Maybe a)
defaultParserMaybe resp = if
  | respCode == N.status200 -> Just <$> respJson resp
  | respCode == N.status404 -> return Nothing
  | otherwise -> do
      let val = TBS.fromLBS $ resp ^. W.responseBody
      throwError $ HStatus respCode val
  where
    respCode = resp ^. W.responseStatus

-- default parser which allows all 2xx responses
default2xxParser :: (J.FromJSON a) => W.Response B.ByteString -> Either HTTPErr a
default2xxParser resp = if
  | respCode >= N.status200 && respCode < N.status300 -> respJson resp
  | otherwise -> do
      let val = TBS.fromLBS $ resp ^. W.responseBody
      throwError $ HStatus respCode val
  where
    respCode = resp ^. W.responseStatus

noBody2xxParser :: W.Response B.ByteString -> Either HTTPErr ()
noBody2xxParser resp = if
  | respCode >= N.status200 && respCode < N.status300 -> return ()
  | otherwise -> do
      let val = TBS.fromLBS $ resp ^. W.responseBody
      throwError $ HStatus respCode val
  where
    respCode = resp ^. W.responseStatus

anyBodyParser :: W.Response B.ByteString -> Either HTTPErr B.ByteString
anyBodyParser resp = if
  | respCode >= N.status200 && respCode < N.status300 -> return $ resp ^. W.responseBody
  | otherwise -> do
      let val = TBS.fromLBS $ resp ^. W.responseBody
      throwError $ HStatus respCode val
  where
    respCode = resp ^. W.responseStatus

mkHTTP :: (J.FromJSON a) => String -> String -> HTTP a
mkHTTP method url =
  HTTP method url Nothing Nothing id defaultParser
  defaultRetryFn defaultRetryPolicy

mkAnyHTTPPost :: String -> Maybe J.Value -> HTTP B.ByteString
mkAnyHTTPPost url payload =
  HTTP "POST" url payload Nothing id anyBodyParser
  defaultRetryFn defaultRetryPolicy

mkHTTPMaybe :: (J.FromJSON a) => String -> String -> HTTP (Maybe a)
mkHTTPMaybe method url =
  HTTP method url Nothing Nothing id defaultParserMaybe
  defaultRetryFn defaultRetryPolicy

-- internal logging related types
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
  toEngineLog req = (LevelInfo, "event-trigger", J.toJSON req )

instance ToEngineLog HTTPResp where
  toEngineLog resp = (LevelInfo, "event-trigger", J.toJSON resp )

data HTTPResp
   = HTTPResp
   { _hrsStatus  :: !Int
   , _hrsHeaders :: ![T.Text]
   , _hrsBody    :: !TL.Text
   } deriving (Show, Eq)

$(J.deriveJSON (J.aesonDrop 4 J.snakeCase){J.omitNothingFields=True} ''HTTPResp)


data HTTPRespExtra
  = HTTPRespExtra
  { _hreResponse :: HTTPResp
  , _hreContext  :: Maybe ExtraContext
  }

$(J.deriveJSON (J.aesonDrop 4 J.snakeCase){J.omitNothingFields=True} ''HTTPRespExtra)

instance ToEngineLog HTTPRespExtra where
  toEngineLog resp = (LevelInfo, "event-trigger", J.toJSON resp )

mkHTTPResp :: W.Response B.ByteString -> HTTPResp
mkHTTPResp resp =
  HTTPResp
  (resp ^. W.responseStatus.W.statusCode)
  (map decodeHeader $ resp ^. W.responseHeaders)
  (decodeLBS $ resp ^. W.responseBody)
  where
    decodeBS = TE.decodeUtf8With TE.lenientDecode
    decodeLBS = TLE.decodeUtf8With TE.lenientDecode
    decodeHeader (hdrName, hdrVal)
      = decodeBS (CI.original hdrName) <> " : " <> decodeBS hdrVal


runHTTP
  :: ( MonadReader r m
     , MonadError HTTPErr m
     , MonadIO m
     , Has WS.Session r
     , Has HLogger r
     )
  => W.Options -> HTTP a -> Maybe ExtraContext -> m a
runHTTP opts http exLog = do
  -- try the http request
  res <- R.retrying retryPol' retryFn' $ httpWithLogging opts http exLog

  -- process the result
  either throwError return res

  where
    retryPol'  = R.RetryPolicyM $ liftIO . R.getRetryPolicyM (_hRetryPolicy http)
    retryFn' _ = return . _hRetryFn http

httpWithLogging
  :: ( MonadReader r m
     , MonadIO m
     , Has WS.Session r
     , Has HLogger r
     )
  => W.Options -> HTTP a -> Maybe ExtraContext -> R.RetryStatus -> m (Either HTTPErr a)
-- the actual http action
httpWithLogging opts (HTTP method url mPayload mFormParams optsMod bodyParser _ _) exLog retryStatus = do
  (logF:: HLogger) <- asks getter
  -- log the request
  liftIO $ logF $ toEngineLog $ HTTPReq method url mPayload
    (R.rsIterNumber retryStatus) (R.rsPreviousDelay retryStatus)

  session <- asks getter

  res <- finallyRunHTTPPlz session
  case res of
    Left e     -> liftIO $ logF $ toEngineLog $ HClient e
    Right resp ->
      --liftIO $ print "=======================>"
      liftIO $ logF $ toEngineLog $ HTTPRespExtra (mkHTTPResp resp) exLog
      --liftIO $ print "<======================="

  -- return the processed response
  return $ either (Left . HClient) bodyParser res

  where
    -- set wreq options to ignore status code exceptions
    ignoreStatusCodeExceptions _ _ = return ()
    finalOpts = optsMod opts
                & W.checkResponse ?~ ignoreStatusCodeExceptions

    -- the actual function which makes the relevant Wreq calls
    finallyRunHTTPPlz sessMgr =
      liftIO $ try $
      case (mPayload, mFormParams) of
        (Just payload, _)   -> WS.customPayloadMethodWith method finalOpts sessMgr url payload
        (Nothing, Just fps) -> WS.customPayloadMethodWith method finalOpts sessMgr url fps
        (Nothing, Nothing)  -> WS.customMethodWith method finalOpts sessMgr url

mkHLogger :: LoggerCtx -> HLogger
mkHLogger (LoggerCtx loggerSet serverLogLevel timeGetter) (logLevel, logTy, logDet) = do
  localTime <- timeGetter
  when (logLevel >= serverLogLevel) $
    FL.pushLogStrLn loggerSet $ FL.toLogStr $
    J.encode $ EngineLog localTime logLevel logTy logDet

