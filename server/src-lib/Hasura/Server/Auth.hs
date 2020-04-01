{-# LANGUAGE RecordWildCards #-}
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
  , UserAuthentication (..)
  ) where

import           Control.Concurrent.Extended (forkImmortal)
import           Control.Exception           (try)
import           Control.Lens
import           Data.Aeson
import           Data.IORef                  (newIORef)
import           Data.Time.Clock             (UTCTime)
import           Hasura.Server.Version       (HasVersion)

import qualified Data.Aeson                  as J
import qualified Data.ByteString.Lazy        as BL
import qualified Data.HashMap.Strict         as Map
import qualified Data.Text                   as T
import qualified Network.HTTP.Client         as H
import qualified Network.HTTP.Types          as N
import qualified Network.Wreq                as Wreq

import           Hasura.HTTP
import           Hasura.Logging
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.Server.Auth.JWT
import           Hasura.Server.Logging
import           Hasura.Server.Utils
import           Hasura.User

-- | Typeclass representing the @UserInfo@ authorization and resolving effect
class (Monad m) => UserAuthentication m where
  resolveUserInfo
    :: HasVersion
    => Logger Hasura
    -> H.Manager
    -> [N.Header]
    -- ^ request headers
    -> AuthMode
    -> m (Either QErr (UserInfo, Maybe UTCTime))

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
  :: ( HasVersion
     , MonadIO m
     , MonadError T.Text m
     )
  => Maybe AdminSecret
  -> Maybe AuthHook
  -> Maybe JWTConfig
  -> Maybe RoleName
  -> H.Manager
  -> Logger Hasura
  -> m AuthMode
mkAuthMode mAdminSecret mWebHook mJwtSecret mUnAuthRole httpManager logger =
  case (mAdminSecret, mWebHook, mJwtSecret) of
    (Nothing,  Nothing,   Nothing)      -> return AMNoAuth
    (Just key, Nothing,   Nothing)      -> return $ AMAdminSecret key mUnAuthRole
    (Just key, Just hook, Nothing)      -> unAuthRoleNotReqForWebHook >>
                                           return (AMAdminSecretAndHook key hook)
    (Just key, Nothing,   Just jwtConf) -> do
      jwtCtx <- mkJwtCtx jwtConf httpManager logger
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

-- | Given the 'JWTConfig' (the user input of JWT configuration), create the 'JWTCtx' (the runtime JWT config used)
mkJwtCtx
  :: ( HasVersion
     , MonadIO m
     , MonadError T.Text m
     )
  => JWTConfig
  -> H.Manager
  -> Logger Hasura
  -> m JWTCtx
mkJwtCtx JWTConfig{..} httpManager logger = do
  jwkRef <- case jcKeyOrUrl of
    Left jwk  -> liftIO $ newIORef (JWKSet [jwk])
    Right url -> getJwkFromUrl url
  let claimsFmt = fromMaybe JCFJson jcClaimsFormat
  return $ JWTCtx jwkRef jcClaimNs jcAudience claimsFmt jcIssuer
  where
    -- if we can't find any expiry time for the JWK (either in @Expires@ header or @Cache-Control@
    -- header), do not start a background thread for refreshing the JWK
    getJwkFromUrl url = do
      ref <- liftIO $ newIORef $ JWKSet []
      maybeExpiry <- withJwkError $ updateJwkRef logger httpManager url ref
      case maybeExpiry of
        Nothing   -> return ref
        Just time -> do
          void $ liftIO $ forkImmortal "jwkRefreshCtrl" logger $
            jwkRefreshCtrl logger httpManager url ref (fromUnits time)
          return ref

    withJwkError act = do
      res <- runExceptT act
      case res of
        Right r -> return r
        Left err  -> case err of
          -- when fetching JWK initially, except expiry parsing error, all errors are critical
          JFEHttpException _ msg  -> throwError msg
          JFEHttpError _ _ _ e    -> throwError e
          JFEJwkParseError _ e    -> throwError e
          JFEExpiryParseError _ _ -> return Nothing


-- | Form the 'UserInfo' from the response from webhook
mkUserInfoFromResp
  :: (MonadIO m, MonadError QErr m)
  => Logger Hasura
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
      let sessionVariables = mkSessionVariablesText $ Map.toList rawHeaders
      case roleFromSession sessionVariables of
        Nothing -> do
          logError
          throw500 "missing x-hasura-role key in webhook response"
        Just rn -> do
          logWebHookResp LevelInfo Nothing
          return $ mkUserInfo rn sessionVariables UAdminSecretAbsent

    logError =
      logWebHookResp LevelError $ Just respBody

    logWebHookResp logLevel mResp =
      unLogger logger $ WebHookLog logLevel (Just statusCode)
        url method Nothing $ fmap (bsToTxt . BL.toStrict) mResp

userInfoFromAuthHook
  :: (HasVersion, MonadIO m, MonadError QErr m)
  => Logger Hasura
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
      unLogger logger $
        WebHookLog LevelError Nothing urlT method
        (Just $ HttpException err) Nothing
      throw500 "Internal Server Error"

    filteredHeaders = flip filter reqHeaders $ \(n, _) ->
      n `notElem` commonClientHeadersIgnored

getUserInfo
  :: (HasVersion, MonadIO m, MonadError QErr m)
  => Logger Hasura
  -> H.Manager
  -> [N.Header]
  -> AuthMode
  -> m UserInfo
getUserInfo l m r a = fst <$> getUserInfoWithExpTime l m r a

getUserInfoWithExpTime
  :: (HasVersion, MonadIO m, MonadError QErr m)
  => Logger Hasura
  -> H.Manager
  -> [N.Header]
  -> AuthMode
  -> m (UserInfo, Maybe UTCTime)
getUserInfoWithExpTime logger manager rawHeaders = \case

  AMNoAuth -> pure (userInfoFromHeaders UNoAuthSet, Nothing)

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
      map (`getSessionVariableValue` sessionVariables) [adminSecretHeader, deprecatedAccessKeyHeader]

    sessionVariables = mkSessionVariables rawHeaders

    userInfoWhenAdminSecret key reqKey = do
      when (reqKey /= getAdminSecret key) $ throw401 $
        "invalid " <> adminSecretHeader <> "/" <> deprecatedAccessKeyHeader
      pure $ userInfoFromHeaders UAdminSecretPresent

    userInfoWhenNoAdminSecret = \case
      Nothing -> throw401 $ adminSecretHeader <> "/"
                 <> deprecatedAccessKeyHeader <> " required, but not found"
      Just roleName -> pure $ mkUserInfo roleName sessionVariables UAdminSecretAbsent

    withNoExpTime a = (, Nothing) <$> a

    userInfoFromHeaders userAdminSecret =
      case roleFromSession sessionVariables of
        Just rn -> mkUserInfo rn sessionVariables userAdminSecret
        Nothing -> mkUserInfo adminRoleName sessionVariables userAdminSecret
