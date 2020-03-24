module Hasura.Server.Auth.WebHook
  ( AuthHookType(..)
  , AuthHookG (..)
  , AuthHook
  , userInfoFromAuthHook
  ) where

import           Control.Exception        (try)
import           Control.Lens
import           Data.Aeson
import           Data.Time.Clock          (UTCTime)
import           Hasura.Server.Version    (HasVersion)

import qualified Data.Aeson               as J
import qualified Data.ByteString.Lazy     as BL
import qualified Data.HashMap.Strict      as Map
import qualified Data.Text                as T
import qualified Network.HTTP.Client      as H
import qualified Network.HTTP.Types       as N
import qualified Network.Wreq             as Wreq

import           Hasura.HTTP
import           Hasura.Logging
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.Server.Auth.Utils
import           Hasura.Server.Logging
import           Hasura.Server.Utils


-- hook type

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


-- auth hook

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



-- | Extract @UserInfo@ from the webhook answer.
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


-- | Form the 'UserInfo' from the response from webhook
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
          let ccHeader = Map.lookup "Cache-Control" rawHeaders
              eHeader  = Map.lookup "Expires"       rawHeaders
          expTime <- getExpiryTime ccHeader eHeader
          return (mkUserInfo rn usrVars, expTime)

    logWarn =
      logWebHookResp LevelWarn $ Just respBody
    logError =
      logWebHookResp LevelError (Just respBody) Nothing
    logWebHookResp logLevel mResp message =
      logger $ WebHookLog logLevel (Just statusCode)
        url method Nothing (bsToTxt . BL.toStrict <$> mResp) message

    getExpiryTime ccHeader eHeader = do
      cct <- timeFromCacheControlHeader ccHeader $ logWarn . Just
      expTime <- case cct of
        Just t  -> return $ Just t
        Nothing -> timeFromExpiresHeader eHeader $ logWarn . Just
      onMaybe expTime $ fmap Just . toUTCTime

