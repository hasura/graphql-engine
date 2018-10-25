{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Hasura.Server.Auth
  ( getUserInfo
  , AuthMode(..)
  , mkAuthMode
  , AccessKey (..)
  , Webhook (..)
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

import qualified Data.ByteString.Lazy    as BL
import qualified Data.HashMap.Strict     as M
import qualified Data.String.Conversions as CS
import qualified Data.Text               as T
import qualified Network.HTTP.Client     as H
import qualified Network.HTTP.Types      as N
import qualified Network.Wreq            as Wreq

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

newtype Webhook
  = Webhook { getWebhook :: T.Text }
  deriving (Show, Eq)

data AuthMode
  = AMNoAuth
  | AMAccessKey !AccessKey !(Maybe RoleName)
  | AMAccessKeyAndHook !AccessKey !Webhook
  | AMAccessKeyAndJWT !AccessKey !JWTCtx !(Maybe RoleName)
  deriving (Show, Eq)

mkAuthMode
  :: ( MonadIO m
     , MonadError T.Text m
     )
  => Maybe AccessKey
  -> Maybe Webhook
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
  -> N.Status
  -> BL.ByteString
  -> m UserInfo
mkUserInfoFromResp logger url statusCode respBody
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
      let headers = M.fromList [(T.toLower k, v) | (k, v) <- M.toList rawHeaders]
      case M.lookup userRoleHeader headers of
        Nothing -> do
          logError
          throw500 "missing x-hasura-role key in webhook response"
        Just v  -> do
          logWebHookResp L.LevelInfo Nothing
          return $ UserInfo (RoleName v) headers

    logError =
      logWebHookResp L.LevelError $ Just respBody

    logWebHookResp logLevel mResp =
      liftIO $ L.unLogger logger $ WebHookLog logLevel (Just statusCode)
        url Nothing $ fmap (bsToTxt . BL.toStrict) mResp

userInfoFromWebhook
  :: (MonadIO m, MonadError QErr m)
  => L.Logger
  -> H.Manager
  -> Webhook
  -> [N.Header]
  -> m UserInfo
userInfoFromWebhook logger manager hook reqHeaders = do
  let options =
        Wreq.defaults
        & Wreq.headers .~ filteredHeaders
        & Wreq.checkResponse ?~ (\_ _ -> return ())
        & Wreq.manager .~ Right manager

      urlT = getWebhook hook
  res <- liftIO $ try $ Wreq.getWith options $ T.unpack urlT
  resp <- either logAndThrow return res
  let status = resp ^. Wreq.responseStatus
      respBody = resp ^. Wreq.responseBody

  mkUserInfoFromResp logger urlT status respBody
  where
    logAndThrow err = do
      let urlT = getWebhook hook
      liftIO $ L.unLogger logger $
        WebHookLog L.LevelError Nothing urlT (Just err) Nothing
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
    case getHeader accessKeyHeader of
      Just givenAccKey -> userInfoWhenAccessKey accKey givenAccKey
      Nothing          -> userInfoWhenNoAccessKey unAuthRole

  AMAccessKeyAndHook accKey hook ->
    whenAccessKeyAbsent accKey (userInfoFromWebhook logger manager hook rawHeaders)

  AMAccessKeyAndJWT accKey jwtSecret unAuthRole ->
    whenAccessKeyAbsent accKey (processJwt jwtSecret rawHeaders unAuthRole)

  where
    -- when access key is absent, run the action to retrieve UserInfo, otherwise
    -- accesskey override
    whenAccessKeyAbsent ak action =
      maybe action (userInfoWhenAccessKey ak) $ getHeader accessKeyHeader

    headers =
      M.fromList $ filter (T.isPrefixOf "x-hasura-" . fst) $
      flip map rawHeaders $
      \(hdrName, hdrVal) ->
        (T.toLower $ bsToTxt $ original hdrName, bsToTxt hdrVal)

    getHeader h = M.lookup h headers

    userInfoFromHeaders =
      case M.lookup userRoleHeader headers of
        Just v  -> UserInfo (RoleName v) headers
        Nothing -> adminUserInfo

    userInfoWhenAccessKey key reqKey = do
      when (reqKey /= getAccessKey key) $ throw401 $ "invalid " <> accessKeyHeader
      return userInfoFromHeaders

    userInfoWhenNoAccessKey = \case
      Nothing -> throw401 $ accessKeyHeader <> " required, but not found"
      Just role -> return $ UserInfo role $
        M.insertWith const userRoleHeader (getRoleTxt role) headers
