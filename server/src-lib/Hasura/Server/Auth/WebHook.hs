module Hasura.Server.Auth.WebHook
  ( AuthHookType (..),
    AuthHook (..),
    userInfoFromAuthHook,
  )
where

import Control.Exception.Lifted (try)
import Control.Lens
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson
import Data.Aeson qualified as J
import Data.ByteString.Lazy qualified as BL
import Data.HashMap.Strict qualified as HashMap
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
import Network.HTTP.Client.Transformable qualified as HTTP
import Network.Wreq qualified as Wreq

data AuthHookType
  = AHTGet
  | AHTPost
  deriving (Eq)

instance Show AuthHookType where
  show AHTGet = "GET"
  show AHTPost = "POST"

data AuthHook = AuthHook
  { ahUrl :: Text,
    ahType :: AuthHookType,
    -- | Whether to send the request body to the auth hook
    ahSendRequestBody :: Bool
  }
  deriving (Show, Eq)

hookMethod :: AuthHook -> HTTP.StdMethod
hookMethod authHook = case ahType authHook of
  AHTGet -> HTTP.GET
  AHTPost -> HTTP.POST

-- | Makes an authentication request to the given AuthHook and returns
--   UserInfo parsed from the response, plus an expiration time if one
--   was returned. Optionally passes a batch of raw GraphQL requests
--   for finer-grained auth. (#2666)
userInfoFromAuthHook ::
  forall m.
  (MonadIO m, MonadBaseControl IO m, MonadError QErr m) =>
  Logger Hasura ->
  HTTP.Manager ->
  AuthHook ->
  [HTTP.Header] ->
  Maybe GH.ReqsText ->
  m (UserInfo, Maybe UTCTime, [HTTP.Header])
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
      req <- liftIO $ HTTP.mkRequestThrow $ T.pack url
      liftIO do
        case ahType hook of
          AHTGet -> do
            let isCommonHeader = (`elem` commonClientHeadersIgnored)
                filteredHeaders = filter (not . isCommonHeader . fst) reqHeaders
                req' = req & set HTTP.headers (addDefaultHeaders filteredHeaders)
            HTTP.httpLbs req' manager
          AHTPost -> do
            let contentType = ("Content-Type", "application/json")
                headersPayload = J.toJSON $ HashMap.fromList $ hdrsToText reqHeaders
                req' =
                  req
                    & set HTTP.method "POST"
                    & set HTTP.headers (addDefaultHeaders [contentType])
                    & set
                      HTTP.body
                      ( HTTP.RequestBodyLBS
                          $ J.encode
                          $ object
                            ( ["headers" J..= headersPayload]
                                -- We will only send the request if `ahSendRequestBody` is set to true
                                <> ["request" J..= reqs | ahSendRequestBody hook]
                            )
                      )
            HTTP.httpLbs req' manager

    logAndThrow :: HTTP.HttpException -> m a
    logAndThrow err = do
      unLogger logger
        $ WebHookLog
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
  HTTP.StdMethod ->
  HTTP.Status ->
  BL.ByteString ->
  [HTTP.Header] ->
  m (UserInfo, Maybe UTCTime, [HTTP.Header])
mkUserInfoFromResp (Logger logger) url method statusCode respBody respHdrs
  | statusCode == HTTP.status200 =
      case eitherDecode respBody of
        Left e -> do
          logError
          throw500 $ "Invalid response from authorization hook: " <> T.pack e
        Right rawHeaders -> getUserInfoFromHdrs rawHeaders respHdrs
  | statusCode == HTTP.status401 = do
      logError
      throw401 "Authentication hook unauthorized this request"
  | otherwise = do
      logError
      throw500 "Invalid response from authorization hook"
  where
    getUserInfoFromHdrs rawHeaders responseHdrs = do
      userInfo <-
        mkUserInfo URBFromSessionVariables UAdminSecretNotSent
          $ mkSessionVariablesText rawHeaders
      logWebHookResp LevelInfo Nothing Nothing
      expiration <- runMaybeT $ timeFromCacheControl rawHeaders <|> timeFromExpires rawHeaders
      pure (userInfo, expiration, responseHdrs)

    logWebHookResp :: (MonadIO m) => LogLevel -> Maybe BL.ByteString -> Maybe Text -> m ()
    logWebHookResp logLevel mResp message =
      logger
        $ WebHookLog
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
      header <- afold $ HashMap.lookup "Cache-Control" headers
      duration <- parseMaxAge header `onLeft` \err -> logWarn (T.pack err) *> empty
      addUTCTime (fromInteger duration) <$> liftIO getCurrentTime
    timeFromExpires headers = do
      header <- afold $ HashMap.lookup "Expires" headers
      parseExpirationTime header `onLeft` \err -> logWarn (T.pack err) *> empty
