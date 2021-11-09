module Hasura.Server.Auth.WebHook
  ( AuthHookType (..),
    AuthHookG (..),
    AuthHook,
    userInfoFromAuthHook,
  )
where

import Control.Exception.Lifted (try)
import Control.Lens
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson
import Data.Aeson qualified as J
import Data.ByteString.Lazy qualified as BL
import Data.HashMap.Strict qualified as Map
import Data.Parser.CacheControl (parseMaxAge)
import Data.Parser.Expires
import Data.Text qualified as T
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import Hasura.Base.Error
import Hasura.GraphQL.Transport.HTTP.Protocol qualified as GH
import Hasura.HTTP
import Hasura.Logging
import Hasura.Prelude
import Hasura.Server.Logging
import Hasura.Server.Utils
import Hasura.Session
import Hasura.Tracing qualified as Tracing
import Network.HTTP.Client.Transformable qualified as H
import Network.HTTP.Types qualified as N
import Network.Wreq qualified as Wreq

data AuthHookType
  = AHTGet
  | AHTPost
  deriving (Eq)

instance Show AuthHookType where
  show AHTGet = "GET"
  show AHTPost = "POST"

data AuthHookG a b = AuthHookG
  { ahUrl :: !a,
    ahType :: !b
  }
  deriving (Show, Eq)

type AuthHook = AuthHookG Text AuthHookType

hookMethod :: AuthHook -> N.StdMethod
hookMethod authHook = case ahType authHook of
  AHTGet -> N.GET
  AHTPost -> N.POST

-- | Makes an authentication request to the given AuthHook and returns
--   UserInfo parsed from the response, plus an expiration time if one
--   was returned. Optionally passes a batch of raw GraphQL requests
--   for finer-grained auth. (#2666)
userInfoFromAuthHook ::
  forall m.
  (MonadIO m, MonadBaseControl IO m, MonadError QErr m, Tracing.MonadTrace m) =>
  Logger Hasura ->
  H.Manager ->
  AuthHook ->
  [N.Header] ->
  Maybe GH.ReqsText ->
  m (UserInfo, Maybe UTCTime, [N.Header])
userInfoFromAuthHook logger manager hook reqHeaders reqs = do
  resp <- (`onLeft` logAndThrow) =<< try performHTTPRequest
  let status = resp ^. Wreq.responseStatus
      respBody = resp ^. Wreq.responseBody
      cookieHeaders = filter (\(headerName, _) -> headerName == "Set-Cookie") (resp ^. Wreq.responseHeaders)

  mkUserInfoFromResp logger (ahUrl hook) (hookMethod hook) status respBody cookieHeaders
  where
    performHTTPRequest :: m (Wreq.Response BL.ByteString)
    performHTTPRequest = do
      let url = T.unpack $ ahUrl hook
      req <- liftIO $ H.mkRequestThrow $ T.pack url
      Tracing.tracedHttpRequest req \req' -> liftIO do
        case ahType hook of
          AHTGet -> do
            let isCommonHeader = (`elem` commonClientHeadersIgnored)
                filteredHeaders = filter (not . isCommonHeader . fst) reqHeaders
                req'' = req' & set H.headers (addDefaultHeaders filteredHeaders)
            H.performRequest req'' manager
          AHTPost -> do
            let contentType = ("Content-Type", "application/json")
                headersPayload = J.toJSON $ Map.fromList $ hdrsToText reqHeaders
                req'' =
                  req & set H.method "POST"
                    & set H.headers (addDefaultHeaders [contentType])
                    & set H.body (Just $ J.encode $ object ["headers" J..= headersPayload, "request" J..= reqs])
            H.performRequest req'' manager

    logAndThrow :: H.HttpException -> m a
    logAndThrow err = do
      unLogger logger $
        WebHookLog
          LevelError
          Nothing
          (ahUrl hook)
          (hookMethod hook)
          (Just $ HttpException err)
          Nothing
          Nothing
      throw500 "webhook authentication request failed"

mkUserInfoFromResp ::
  (MonadIO m, MonadError QErr m) =>
  Logger Hasura ->
  Text ->
  N.StdMethod ->
  N.Status ->
  BL.ByteString ->
  [N.Header] ->
  m (UserInfo, Maybe UTCTime, [N.Header])
mkUserInfoFromResp (Logger logger) url method statusCode respBody respHdrs
  | statusCode == N.status200 =
    case eitherDecode respBody of
      Left e -> do
        logError
        throw500 $ "Invalid response from authorization hook: " <> T.pack e
      Right rawHeaders -> getUserInfoFromHdrs rawHeaders respHdrs
  | statusCode == N.status401 = do
    logError
    throw401 "Authentication hook unauthorized this request"
  | otherwise = do
    logError
    throw500 "Invalid response from authorization hook"
  where
    getUserInfoFromHdrs rawHeaders responseHdrs = do
      userInfo <-
        mkUserInfo URBFromSessionVariables UAdminSecretNotSent $
          mkSessionVariablesText rawHeaders
      logWebHookResp LevelInfo Nothing Nothing
      expiration <- runMaybeT $ timeFromCacheControl rawHeaders <|> timeFromExpires rawHeaders
      pure (userInfo, expiration, responseHdrs)

    logWebHookResp :: MonadIO m => LogLevel -> Maybe BL.ByteString -> Maybe Text -> m ()
    logWebHookResp logLevel mResp message =
      logger $
        WebHookLog
          logLevel
          (Just statusCode)
          url
          method
          Nothing
          (bsToTxt . BL.toStrict <$> mResp)
          message
    logWarn message = logWebHookResp LevelWarn (Just respBody) (Just message)
    logError = logWebHookResp LevelError (Just respBody) Nothing

    timeFromCacheControl headers = do
      header <- afold $ Map.lookup "Cache-Control" headers
      duration <- parseMaxAge header `onLeft` \err -> logWarn (T.pack err) *> empty
      addUTCTime (fromInteger duration) <$> liftIO getCurrentTime
    timeFromExpires headers = do
      header <- afold $ Map.lookup "Expires" headers
      parseExpirationTime header `onLeft` \err -> logWarn (T.pack err) *> empty
