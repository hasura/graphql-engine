{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE RankNTypes #-}

module Hasura.Server.Auth
  ( getUserInfo
  , AuthMode(..)
  , mkAuthMode
  , AccessKey (..)
  , AuthHookType(..)
  , AuthHookG (..)
  , AuthHook
  -- JWT related
  , RawJWT
  , JWTConfig (..)
  , JWTCtx (..)
  , JWKSet (..)
  , processJwt
  , updateJwkRef
  , jwkRefreshCtrl
  ) where

import           Control.Exception       (try)
import           Control.Lens
import           Data.Aeson
import           Data.CaseInsensitive    (CI (..), original)
import           Data.IORef              (newIORef)

import qualified Data.Aeson              as J
import qualified Data.ByteString.Lazy    as BL
import qualified Data.HashMap.Strict     as Map
import qualified Data.String.Conversions as CS
import qualified Data.Text               as T
import qualified Network.HTTP.Client     as H
import qualified Network.HTTP.Types      as N
import qualified Network.Wreq            as Wreq

import           Hasura.HTTP
import           Hasura.Logging
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.Server.Auth.JWT
import           Hasura.Server.Logging
import           Hasura.Server.Utils

import qualified Hasura.Logging          as L


newtype AccessKey
  = AccessKey { getAccessKey :: T.Text }
  deriving (Show, Eq)

data AuthHookType
  = AHTGet
  | AHTPost
  deriving (Show, Eq)

data AuthHookG a b
  = AuthHookG
  { ahUrl  :: !a
  , ahType :: !b
  } deriving (Show, Eq)

type AuthHook = AuthHookG T.Text AuthHookType

data AuthMode
  = AMNoAuth
  | AMAccessKey !AccessKey !(Maybe RoleName)
  | AMAccessKeyAndHook !AccessKey !AuthHook
  | AMAccessKeyAndJWT !AccessKey !JWTCtx !(Maybe RoleName)
  deriving (Show, Eq)

hdrsToText :: [N.Header] -> [(T.Text, T.Text)]
hdrsToText hdrs =
  [ (bsToTxt $ original hdrName, bsToTxt hdrVal)
  | (hdrName, hdrVal) <- hdrs
  ]

mkAuthMode
  :: ( MonadIO m
     , MonadError T.Text m
     )
  => Maybe AccessKey
  -> Maybe AuthHook
  -> Maybe T.Text
  -> Maybe RoleName
  -> H.Manager
  -> LoggerCtx
  -> m AuthMode
mkAuthMode mAccessKey mWebHook mJwtSecret mUnAuthRole httpManager lCtx =
  case (mAccessKey, mWebHook, mJwtSecret) of
    (Nothing,  Nothing,   Nothing)      -> return AMNoAuth
    (Just key, Nothing,   Nothing)      -> return $ AMAccessKey key mUnAuthRole
    (Just key, Just hook, Nothing)      -> unAuthRoleNotReqForWebHook >>
                                           return (AMAccessKeyAndHook key hook)
    (Just key, Nothing,   Just jwtConf) -> do
      jwtCtx <- mkJwtCtx jwtConf httpManager lCtx
      return $ AMAccessKeyAndJWT key jwtCtx mUnAuthRole

    (Nothing, Just _, Nothing)     -> throwError $
      "Fatal Error : --auth-hook (HASURA_GRAPHQL_AUTH_HOOK)"
      <> " requires --access-key (HASURA_GRAPHQL_ACCESS_KEY) to be set"
    (Nothing, Nothing, Just _)     -> throwError $
      "Fatal Error : --jwt-secret (HASURA_GRAPHQL_JWT_SECRET)"
      <> " requires --access-key (HASURA_GRAPHQL_ACCESS_KEY) to be set"
    (Nothing, Just _, Just _)     -> throwError
      "Fatal Error: Both webhook and JWT mode cannot be enabled at the same time"
    (Just _, Just _, Just _)     -> throwError
      "Fatal Error: Both webhook and JWT mode cannot be enabled at the same time"
  where
    unAuthRoleNotReqForWebHook =
      when (isJust mUnAuthRole) $
        throwError $ "Fatal Error: --unauthorized-role (HASURA_GRAPHQL_UNAUTHORIZED_ROLE) is not allowed"
                     <> " when --auth-hook (HASURA_GRAPHQL_AUTH_HOOK) is set"

mkJwtCtx
  :: ( MonadIO m
     , MonadError T.Text m
     )
  => T.Text
  -> H.Manager
  -> LoggerCtx
  -> m JWTCtx
mkJwtCtx jwtConf httpManager loggerCtx = do
  -- the JWT Conf as JSON string; try to parse it
  conf   <- either decodeErr return $ eitherDecodeStrict $ CS.cs jwtConf
  jwkRef <- case jcKeyOrUrl conf of
    Left jwk  -> liftIO $ newIORef (JWKSet [jwk])
    Right url -> do
      ref <- liftIO $ newIORef $ JWKSet []
      let logger = mkLogger loggerCtx
      mTime <- updateJwkRef logger httpManager url ref
      case mTime of
        Nothing -> return ref
        Just t -> do
          jwkRefreshCtrl logger httpManager url ref t
          return ref
  return $ JWTCtx jwkRef (jcClaimNs conf) (jcAudience conf)
  where
    decodeErr e = throwError . T.pack $ "Fatal Error: JWT conf: " <> e

mkUserInfoFromResp
  :: (MonadIO m, MonadError QErr m)
  => L.Logger
  -> T.Text
  -> N.StdMethod
  -> N.Status
  -> BL.ByteString
  -> m UserInfo
mkUserInfoFromResp logger url method statusCode respBody
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
          logWebHookResp L.LevelInfo Nothing
          return $ mkUserInfo rn usrVars

    logError =
      logWebHookResp L.LevelError $ Just respBody

    logWebHookResp logLevel mResp =
      liftIO $ L.unLogger logger $ WebHookLog logLevel (Just statusCode)
        url method Nothing $ fmap (bsToTxt . BL.toStrict) mResp

userInfoFromAuthHook
  :: (MonadIO m, MonadError QErr m)
  => L.Logger
  -> H.Manager
  -> AuthHook
  -> [N.Header]
  -> m UserInfo
userInfoFromAuthHook logger manager hook reqHeaders = do
  res <- liftIO $ try $ bool withGET withPOST isPost
  resp <- either logAndThrow return res
  let status = resp ^. Wreq.responseStatus
      respBody = resp ^. Wreq.responseBody

  mkUserInfoFromResp logger urlT method status respBody
  where
    mkOptions = wreqOptions manager
    AuthHookG urlT ty = hook
    isPost = case ty of
      AHTPost -> True
      AHTGet  -> False
    method = bool N.GET N.POST isPost

    withGET = Wreq.getWith (mkOptions filteredHeaders) $
              T.unpack urlT

    contentType = ("Content-Type", "application/json")
    postHdrsPayload = J.toJSON $ Map.fromList $ hdrsToText reqHeaders
    withPOST = Wreq.postWith (mkOptions [contentType]) (T.unpack urlT) $
               object ["headers" J..= postHdrsPayload]

    logAndThrow err = do
      liftIO $ L.unLogger logger $
        WebHookLog L.LevelError Nothing urlT method
        (Just $ HttpException err) Nothing
      throw500 "Internal Server Error"

    filteredHeaders = flip filter reqHeaders $ \(n, _) ->
      n `notElem` [ "Content-Length", "Content-MD5", "User-Agent", "Host"
                  , "Origin", "Referer" , "Accept", "Accept-Encoding"
                  , "Accept-Language", "Accept-Datetime"
                  , "Cache-Control", "Connection", "DNT"
                  ]


getUserInfo
  :: (MonadIO m, MonadError QErr m)
  => L.Logger
  -> H.Manager
  -> [N.Header]
  -> AuthMode
  -> m UserInfo
getUserInfo logger manager rawHeaders = \case

  AMNoAuth -> return userInfoFromHeaders

  AMAccessKey accKey unAuthRole ->
    case getVarVal accessKeyHeader usrVars of
      Just givenAccKey -> userInfoWhenAccessKey accKey givenAccKey
      Nothing          -> userInfoWhenNoAccessKey unAuthRole

  AMAccessKeyAndHook accKey hook ->
    whenAccessKeyAbsent accKey (userInfoFromAuthHook logger manager hook rawHeaders)

  AMAccessKeyAndJWT accKey jwtSecret unAuthRole ->
    whenAccessKeyAbsent accKey (processJwt jwtSecret rawHeaders unAuthRole)

  where
    -- when access key is absent, run the action to retrieve UserInfo, otherwise
    -- accesskey override
    whenAccessKeyAbsent ak action =
      maybe action (userInfoWhenAccessKey ak) $ getVarVal accessKeyHeader usrVars

    usrVars = mkUserVars $ hdrsToText rawHeaders

    userInfoFromHeaders =
      case roleFromVars usrVars of
        Just rn -> mkUserInfo rn usrVars
        Nothing -> mkUserInfo adminRole usrVars

    userInfoWhenAccessKey key reqKey = do
      when (reqKey /= getAccessKey key) $ throw401 $ "invalid " <> accessKeyHeader
      return userInfoFromHeaders

    userInfoWhenNoAccessKey = \case
      Nothing -> throw401 $ accessKeyHeader <> " required, but not found"
      Just role -> return $ mkUserInfo role usrVars
