module Hasura.Server.Auth.WebHook
  ( AuthHookType(..)
  , AuthHookG (..)
  , AuthHook
  , userInfoFromAuthHook
  ) where

import           Control.Exception.Lifted    (try)
import           Control.Lens
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import           Data.Time.Clock             (UTCTime, addUTCTime, getCurrentTime)
import           Hasura.Server.Version       (HasVersion)

import qualified Data.Aeson                  as J
import qualified Data.ByteString.Lazy        as BL
import qualified Data.HashMap.Strict         as Map
import qualified Data.Text                   as T
import qualified Network.HTTP.Client         as H
import qualified Network.HTTP.Types          as N
import qualified Network.Wreq                as Wreq

import           Data.Parser.CacheControl
import           Data.Parser.Expires
import           Hasura.HTTP
import           Hasura.Logging
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.Server.Logging
import           Hasura.Server.Utils
import           Hasura.Session


data AuthHookType
  = AHTGet
  | AHTPost
  deriving (Eq)

instance Show AuthHookType where
  show AHTGet  = "GET"
  show AHTPost = "POST"


data AuthHookG a b
  = AuthHookG
  { ahUrl  :: !a
  , ahType :: !b
  } deriving (Show, Eq)

type AuthHook = AuthHookG T.Text AuthHookType

hookMethod :: AuthHook -> N.StdMethod
hookMethod authHook = case ahType authHook of
  AHTGet  -> N.GET
  AHTPost -> N.POST


-- | Makes an authentication request to the given AuthHook and returns
--   UserInfo parsed from the response, plus an expiration time if one
--   was returned.
userInfoFromAuthHook
  :: forall m
   . (HasVersion, MonadIO m, MonadBaseControl IO m, MonadError QErr m)
  => Logger Hasura
  -> H.Manager
  -> AuthHook
  -> [N.Header]
  -> m (UserInfo, Maybe UTCTime)
userInfoFromAuthHook logger manager hook reqHeaders = do
  resp <- (`onLeft` logAndThrow) =<< try performHTTPRequest
  let status   = resp ^. Wreq.responseStatus
      respBody = resp ^. Wreq.responseBody
  mkUserInfoFromResp logger (ahUrl hook) (hookMethod hook) status respBody
  where
    performHTTPRequest :: m (Wreq.Response BL.ByteString)
    performHTTPRequest = do
      let url = T.unpack $ ahUrl hook
      req <- liftIO $ H.parseRequest url
      liftIO do
        case ahType hook of
          AHTGet  -> do
            let isCommonHeader  = (`elem` commonClientHeadersIgnored)
                filteredHeaders = filter (not . isCommonHeader . fst) reqHeaders
            H.httpLbs (req { H.requestHeaders = addDefaultHeaders filteredHeaders }) manager
          AHTPost -> do
            let contentType = ("Content-Type", "application/json")
                headersPayload = J.toJSON $ Map.fromList $ hdrsToText reqHeaders
            H.httpLbs (req { H.method         = "POST"
                           , H.requestHeaders = addDefaultHeaders [contentType] 
                           , H.requestBody    = H.RequestBodyLBS . J.encode $ object ["headers" J..= headersPayload]
                           }) manager

    logAndThrow :: H.HttpException -> m a
    logAndThrow err = do
      unLogger logger $
        WebHookLog LevelError Nothing (ahUrl hook) (hookMethod hook)
        (Just $ HttpException err) Nothing Nothing
      throw500 "webhook authentication request failed"


mkUserInfoFromResp
  :: (MonadIO m, MonadError QErr m)
  => Logger Hasura
  -> T.Text
  -> N.StdMethod
  -> N.Status
  -> BL.ByteString
  -> m (UserInfo, Maybe UTCTime)
mkUserInfoFromResp (Logger logger) url method statusCode respBody
  | statusCode == N.status200 =
    case eitherDecode respBody of
      Left e -> do
        logError
        throw500 $ "Invalid response from authorization hook: " <> T.pack e
      Right rawHeaders -> getUserInfoFromHdrs rawHeaders

  | statusCode == N.status401 = do
    logError
    throw401 "Authentication hook unauthorized this request"

  | otherwise = do
    logError
    throw500 "Invalid response from authorization hook"
  where
    getUserInfoFromHdrs rawHeaders = do
      userInfo <- mkUserInfo URBFromSessionVariables UAdminSecretNotSent $
                  mkSessionVariablesText $ Map.toList rawHeaders
      logWebHookResp LevelInfo Nothing Nothing
      expiration <- runMaybeT $ timeFromCacheControl rawHeaders <|> timeFromExpires rawHeaders
      pure (userInfo, expiration)

    logWebHookResp :: MonadIO m => LogLevel -> Maybe BL.ByteString -> Maybe T.Text -> m ()
    logWebHookResp logLevel mResp message =
      logger $ WebHookLog logLevel (Just statusCode)
        url method Nothing (bsToTxt . BL.toStrict <$> mResp) message
    logWarn message = logWebHookResp LevelWarn (Just respBody) (Just message)
    logError = logWebHookResp LevelError (Just respBody) Nothing

    timeFromCacheControl headers = do
      header <- afold $ Map.lookup "Cache-Control" headers
      duration <- parseMaxAge header `onLeft` \err -> logWarn (T.pack err) *> empty
      addUTCTime (fromInteger duration) <$> liftIO getCurrentTime
    timeFromExpires headers = do
      header <- afold $ Map.lookup "Expires" headers
      parseExpirationTime header `onLeft` \err -> logWarn (T.pack err) *> empty
