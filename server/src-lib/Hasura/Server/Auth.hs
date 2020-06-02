{-# LANGUAGE RecordWildCards #-}

module Hasura.Server.Auth
  ( getUserInfo
  , getUserInfoWithExpTime
  , AuthMode (..)
  , mkAuthMode
  , AdminSecret (..)
  -- WebHook related
  , AuthHookType (..)
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
import           Data.IORef                  (newIORef)
import           Data.Time.Clock             (UTCTime)
import           Hasura.Server.Version       (HasVersion)

import qualified Data.Text                   as T
import qualified Network.HTTP.Client         as H
import qualified Network.HTTP.Types          as N

import           Hasura.Logging
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.Server.Auth.JWT
import           Hasura.Server.Auth.WebHook
import           Hasura.Server.Utils
import           Hasura.Session

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

    (Nothing, Just _,  Nothing) -> throwError $
      "Fatal Error : --auth-hook (HASURA_GRAPHQL_AUTH_HOOK)" <> requiresAdminScrtMsg
    (Nothing, Nothing, Just _)  -> throwError $
      "Fatal Error : --jwt-secret (HASURA_GRAPHQL_JWT_SECRET)" <> requiresAdminScrtMsg
    (Nothing, Just _,  Just _)  -> throwError
      "Fatal Error: Both webhook and JWT mode cannot be enabled at the same time"
    (Just _,  Just _,  Just _)  -> throwError
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
  return $ JWTCtx jwkRef jcAudience jcIssuer jcClaims
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
            jwkRefreshCtrl logger httpManager url ref (convertDuration time)
          return ref
--   return $ JWTCtx jwkRef (jcAudience conf) (jcIssuer conf) (jcClaims conf)

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

getUserInfo
  :: (HasVersion, MonadIO m, MonadError QErr m)
  => Logger Hasura
  -> H.Manager
  -> [N.Header]
  -> AuthMode
  -> m UserInfo
getUserInfo l m r a = fst <$> getUserInfoWithExpTime l m r a

getUserInfoWithExpTime
  :: forall m. (HasVersion, MonadIO m, MonadError QErr m)
  => Logger Hasura
  -> H.Manager
  -> [N.Header]
  -> AuthMode
  -> m (UserInfo, Maybe UTCTime)
getUserInfoWithExpTime logger manager rawHeaders = \case

  AMNoAuth -> withNoExpTime $ mkUserInfoFallbackAdminRole UAuthNotSet

  AMAdminSecret adminSecretSet maybeUnauthRole ->
    withAuthorization adminSecretSet $ withNoExpTime $
      -- Consider unauthorized role, if not found raise admin secret header required exception
      case maybeUnauthRole of
        Nothing -> throw401 $ adminSecretHeader <> "/"
                   <> deprecatedAccessKeyHeader <> " required, but not found"
        Just unAuthRole ->
          mkUserInfo (URBPreDetermined unAuthRole) UAdminSecretNotSent sessionVariables

  AMAdminSecretAndHook adminSecretSet hook ->
    withAuthorization adminSecretSet $ userInfoFromAuthHook logger manager hook rawHeaders

  AMAdminSecretAndJWT adminSecretSet jwtSecret unAuthRole ->
    withAuthorization adminSecretSet $ processJwt jwtSecret rawHeaders unAuthRole

  where
    mkUserInfoFallbackAdminRole adminSecretState =
      mkUserInfo (URBFromSessionVariablesFallback adminRoleName)
      adminSecretState sessionVariables

    sessionVariables = mkSessionVariables rawHeaders

    withAuthorization
      :: AdminSecret -> m (UserInfo, Maybe UTCTime) -> m (UserInfo, Maybe UTCTime)
    withAuthorization adminSecretSet actionIfNoAdminSecret = do
      let maybeRequestAdminSecret =
            foldl1 (<|>) $ map (`getSessionVariableValue` sessionVariables)
            [adminSecretHeader, deprecatedAccessKeyHeader]

      -- when admin secret is absent, run the action to retrieve UserInfo
      case maybeRequestAdminSecret of
        Nothing                 -> actionIfNoAdminSecret
        Just requestAdminSecret -> do
          when (requestAdminSecret /= getAdminSecret adminSecretSet) $ throw401 $
            "invalid " <> adminSecretHeader <> "/" <> deprecatedAccessKeyHeader
          withNoExpTime $ mkUserInfoFallbackAdminRole UAdminSecretSent

    withNoExpTime a = (, Nothing) <$> a
