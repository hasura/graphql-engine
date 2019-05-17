module Hasura.Server.Auth
  ( getUserInfo
  , getUserInfoWithExpTime
  , AuthMode(..)
  , mkAuthMode
  , AdminSecret (..)
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
import           Data.IORef              (newIORef)
import           Data.Time.Clock         (UTCTime)

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


newtype AdminSecret
  = AdminSecret { getAdminSecret :: T.Text }
  deriving (Show, Eq)

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

data AuthMode
  = AMNoAuth
  | AMAdminSecret !AdminSecret !(Maybe RoleName)
  | AMAdminSecretAndHook !AdminSecret !AuthHook
  | AMAdminSecretAndJWT !AdminSecret !JWTCtx !(Maybe RoleName)
  deriving (Show, Eq)

mkAuthMode
  :: ( MonadIO m
     , MonadError T.Text m
     )
  => Maybe AdminSecret
  -> Maybe AuthHook
  -> Maybe T.Text
  -> Maybe RoleName
  -> H.Manager
  -> LoggerCtx
  -> m AuthMode
mkAuthMode mAdminSecret mWebHook mJwtSecret mUnAuthRole httpManager lCtx =
  case (mAdminSecret, mWebHook, mJwtSecret) of
    (Nothing,  Nothing,   Nothing)      -> return AMNoAuth
    (Just key, Nothing,   Nothing)      -> return $ AMAdminSecret key mUnAuthRole
    (Just key, Just hook, Nothing)      -> unAuthRoleNotReqForWebHook >>
                                           return (AMAdminSecretAndHook key hook)
    (Just key, Nothing,   Just jwtConf) -> do
      jwtCtx <- mkJwtCtx jwtConf httpManager lCtx
      return $ AMAdminSecretAndJWT key jwtCtx mUnAuthRole

    (Nothing, Just _, Nothing)     -> throwError $
      "Fatal Error : --auth-hook (HASURA_GRAPHQL_AUTH_HOOK)" <> requiresAdminScrtMsg
    (Nothing, Nothing, Just _)     -> throwError $
      "Fatal Error : --jwt-secret (HASURA_GRAPHQL_JWT_SECRET)" <> requiresAdminScrtMsg
    (Nothing, Just _, Just _)     -> throwError
      "Fatal Error: Both webhook and JWT mode cannot be enabled at the same time"
    (Just _, Just _, Just _)     -> throwError
      "Fatal Error: Both webhook and JWT mode cannot be enabled at the same time"
  where
    requiresAdminScrtMsg =
      " requires --admin-secret (HASURA_GRAPHQL_ADMIN_SECRET) or "
      <> " --access-key (HASURA_GRAPHQL_ACCESS_KEY) to be set"
    unAuthRoleNotReqForWebHook =
      when (isJust mUnAuthRole) $ throwError $
        "Fatal Error: --unauthorized-role (HASURA_GRAPHQL_UNAUTHORIZED_ROLE) is not allowed"
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
  let claimsFmt = fromMaybe JCFJson (jcClaimsFormat conf)
  return $ JWTCtx jwkRef (jcClaimNs conf) (jcAudience conf) claimsFmt
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
      n `notElem` commonClientHeadersIgnored

getUserInfo
  :: (MonadIO m, MonadError QErr m)
  => L.Logger
  -> H.Manager
  -> [N.Header]
  -> AuthMode
  -> m UserInfo
getUserInfo l m r a = fst <$> getUserInfoWithExpTime l m r a

getUserInfoWithExpTime
  :: (MonadIO m, MonadError QErr m)
  => L.Logger
  -> H.Manager
  -> [N.Header]
  -> AuthMode
  -> m (UserInfo, Maybe UTCTime)
getUserInfoWithExpTime logger manager rawHeaders = \case

  AMNoAuth -> return (userInfoFromHeaders, Nothing)

  AMAdminSecret adminScrt unAuthRole ->
    case adminSecretM of
      Just givenAdminScrt ->
        withNoExpTime $ userInfoWhenAdminSecret adminScrt givenAdminScrt
      Nothing             ->
        withNoExpTime $ userInfoWhenNoAdminSecret unAuthRole

  AMAdminSecretAndHook accKey hook ->
    whenAdminSecretAbsent accKey $
      withNoExpTime $ userInfoFromAuthHook logger manager hook rawHeaders

  AMAdminSecretAndJWT accKey jwtSecret unAuthRole ->
    whenAdminSecretAbsent accKey (processJwt jwtSecret rawHeaders unAuthRole)

  where
    -- when admin secret is absent, run the action to retrieve UserInfo, otherwise
    -- adminsecret override
    whenAdminSecretAbsent ak action =
      maybe action (withNoExpTime . userInfoWhenAdminSecret ak) adminSecretM

    adminSecretM= foldl1 (<|>) $
      map (`getVarVal` usrVars) [adminSecretHeader, deprecatedAccessKeyHeader]

    usrVars = mkUserVars $ hdrsToText rawHeaders

    userInfoFromHeaders =
      case roleFromVars usrVars of
        Just rn -> mkUserInfo rn usrVars
        Nothing -> mkUserInfo adminRole usrVars

    userInfoWhenAdminSecret key reqKey = do
      when (reqKey /= getAdminSecret key) $ throw401 $
        "invalid " <> adminSecretHeader <> "/" <> deprecatedAccessKeyHeader
      return userInfoFromHeaders

    userInfoWhenNoAdminSecret = \case
      Nothing -> throw401 $ adminSecretHeader <> "/"
                 <>  deprecatedAccessKeyHeader <> " required, but not found"
      Just role -> return $ mkUserInfo role usrVars

    withNoExpTime a = (, Nothing) <$> a
