module Hasura.Server.Auth.WebHook
  ( AuthHookType(..)
  , AuthHookG (..)
  , AuthHook
  , userInfoFromAuthHook
  ) where

import           Control.Exception         (try)
import           Control.Lens
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import           Data.Time.Clock           (UTCTime, addUTCTime, getCurrentTime)
import           Hasura.Server.Version     (HasVersion)

import qualified Data.Aeson                as J
import qualified Data.ByteString.Lazy      as BL
import qualified Data.HashMap.Strict       as Map
import qualified Data.Text                 as T
import qualified Network.HTTP.Client       as H
import qualified Network.HTTP.Types        as N
import qualified Network.Wreq              as Wreq

import           Data.Parser.CacheControl
import           Data.Parser.Expires
import           Hasura.HTTP
import           Hasura.Logging
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.Server.Logging
import           Hasura.Server.Utils


data AuthHookType
  = AHTGet
  | AHTPost
  deriving (Eq)

instance Show AuthHookType where
  show AHTGet  = "GET"
  show AHTPost = "POST"

isPost :: AuthHookType -> Bool
isPost AHTGet  = False
isPost AHTPost = True


data AuthHookG a b
  = AuthHookG
  { ahUrl  :: !a
  , ahType :: !b
  } deriving (Show, Eq)

type AuthHook = AuthHookG T.Text AuthHookType

withAuthHook :: AuthHook -> (T.Text -> a) -> (T.Text -> a) -> a
withAuthHook (AuthHookG u t) ifGet ifPost =
  bool (ifGet u) (ifPost u) $ isPost t

hookMethod :: AuthHook -> N.StdMethod
hookMethod = bool N.GET N.POST . isPost . ahType


-- | Extracts 'UserInfo' from the webhook answer.
--   This uses the provided manager to make the call, extracts
--   information such as response body and status, and delegates to a
--   local 'mkUserInfoFromResp'.
userInfoFromAuthHook
  :: (HasVersion, MonadIO m, MonadError QErr m)
  => Logger Hasura
  -> H.Manager
  -> AuthHook
  -> [N.Header]
  -> m (UserInfo, Maybe UTCTime)
userInfoFromAuthHook logger manager hook reqHeaders = do
  res  <- liftIO $ try $ withAuthHook hook withGET withPOST
  resp <- either logAndThrow return res
  let status   = resp ^. Wreq.responseStatus
      respBody = resp ^. Wreq.responseBody
  mkUserInfoFromResp logger (ahUrl hook) (hookMethod hook) status respBody

  where
    withGET  url = Wreq.getWith  (mkOptions filteredHeaders) $ T.unpack url
    withPOST url = Wreq.postWith (mkOptions [contentType]) (T.unpack url) $
      object ["headers" J..= postHdrsPayload]
    mkOptions = wreqOptions manager
    contentType = ("Content-Type", "application/json")
    postHdrsPayload = J.toJSON $ Map.fromList $ hdrsToText reqHeaders

    logAndThrow err = do
      unLogger logger $
        WebHookLog LevelError Nothing (ahUrl hook) (hookMethod hook)
        (Just $ HttpException err) Nothing Nothing
      throw500 "Internal Server Error"

    filteredHeaders = flip filter reqHeaders $ \(n, _) ->
      n `notElem` commonClientHeadersIgnored


-- | Processes the result of the wehook call, and tries to construct a
--   'UserInfo'. The expiration time is retrieved from either the
--   CacheControl or Expires header.
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
      let usrVars = mkUserVars $ Map.toList rawHeaders
      case roleFromVars usrVars of
        Nothing -> do
          logError
          throw500 "missing x-hasura-role key in webhook response"
        Just rn -> do
          logWebHookResp LevelInfo Nothing Nothing
          expiration <- runMaybeT $ timeFromCacheControl rawHeaders <|> timeFromExpires rawHeaders
          return (mkUserInfo rn usrVars, expiration)

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
