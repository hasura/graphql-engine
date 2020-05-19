{-# LANGUAGE RecordWildCards #-}

module Hasura.Server.Auth
  ( getUserInfo
  , getUserInfoWithExpTime
  , AuthMode (..)
  , setupAuthMode
  , AdminSecretHash
  , hashAdminSecret
  -- * WebHook related
  , AuthHookType (..)
  , AuthHookG (..)
  , AuthHook
  -- * JWT related
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

import qualified Crypto.Hash                 as Crypto
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
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

-- | The hashed admin password. 'hashAdminSecret' is our public interface for
-- constructing the secret. 
--
-- To prevent misuse and leaking we keep this opaque and don't provide
-- instances that could leak information. Likewise for 'AuthMode'.
--
-- Although this exists only in memory we store only a hash of the admin secret
-- primarily in order to:
--     
--     - prevent theoretical timing attacks from a naive `==` 
--     - prevent misuse or inadvertent leaking of the secret
--
-- NOTE: if we could scrub memory of admin secret (from argv and envp) somehow,
-- this would additionally harden against attacks that could read arbitrary
-- memory, so long as the secret was strong. I'm not sure that's attainable.
newtype AdminSecretHash = AdminSecretHash (Crypto.Digest Crypto.SHA512)
  deriving (Ord, Eq)

hashAdminSecret :: T.Text -> AdminSecretHash
hashAdminSecret = AdminSecretHash . Crypto.hash . T.encodeUtf8

-- | The methods we'll use to derive roles for authenticating requests.
--
-- @Maybe RoleName@ below is the optionally-defined role for the
-- unauthenticated (anonymous) user. 
--
-- See: https://hasura.io/docs/1.0/graphql/manual/auth/authentication/unauthenticated-access.html 
data AuthMode
  = AMNoAuth
  | AMAdminSecret !AdminSecretHash !(Maybe RoleName)
  | AMAdminSecretAndHook !AdminSecretHash !AuthHook
  | AMAdminSecretAndJWT !AdminSecretHash !JWTCtx !(Maybe RoleName)

-- | Validate the user's requested authentication configuration, launching any
-- required maintenance threads for JWT etc.
--
-- This must only be run once, on launch.
setupAuthMode
  :: ( HasVersion
     , MonadIO m
     , MonadError T.Text m
     )
  => Maybe AdminSecretHash
  -> Maybe AuthHook
  -> Maybe JWTConfig
  -> Maybe RoleName
  -> H.Manager
  -> Logger Hasura
  -> m AuthMode
setupAuthMode mAdminSecretHash mWebHook mJwtSecret mUnAuthRole httpManager logger =
  case (mAdminSecretHash, mWebHook, mJwtSecret) of
    (Nothing,  Nothing,   Nothing)      -> return AMNoAuth
    (Just hash, Nothing,   Nothing)      -> return $ AMAdminSecret hash mUnAuthRole
    (Just hash, Just hook, Nothing)      -> unAuthRoleNotReqForWebHook >>
                                           return (AMAdminSecretAndHook hash hook)
    (Just hash, Nothing,   Just jwtConf) -> do
      jwtCtx <- mkJwtCtx jwtConf
      return $ AMAdminSecretAndJWT hash jwtCtx mUnAuthRole

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

    -- | Given the 'JWTConfig' (the user input of JWT configuration), create
    -- the 'JWTCtx' (the runtime JWT config used)
    mkJwtCtx :: (HasVersion, MonadIO m, MonadError T.Text m) => JWTConfig -> m JWTCtx
    mkJwtCtx JWTConfig{..} = do
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
                jwkRefreshCtrl logger httpManager url ref (convertDuration time)
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

getUserInfo
  :: (HasVersion, MonadIO m, MonadError QErr m)
  => Logger Hasura
  -> H.Manager
  -> [N.Header]
  -> AuthMode
  -> m UserInfo
getUserInfo l m r a = fst <$> getUserInfoWithExpTime l m r a

-- | Authenticate the request using the headers and the configured 'AuthMode'.
getUserInfoWithExpTime
  :: forall m. (HasVersion, MonadIO m, MonadError QErr m)
  => Logger Hasura
  -> H.Manager
  -> [N.Header]
  -> AuthMode
  -> m (UserInfo, Maybe UTCTime)
getUserInfoWithExpTime logger manager rawHeaders = \case

  AMNoAuth -> withNoExpTime $ mkUserInfoFallbackAdminRole UAuthNotSet

  AMAdminSecret realAdminSecretHash maybeUnauthRole ->
    withAuthorization realAdminSecretHash $ withNoExpTime $
      -- Consider unauthorized role, if not found raise admin secret header required exception
      case maybeUnauthRole of
        Nothing -> throw401 $ adminSecretHeader <> "/"
                   <> deprecatedAccessKeyHeader <> " required, but not found"
        Just unAuthRole ->
          mkUserInfo (URBPreDetermined unAuthRole) UAdminSecretNotSent sessionVariables

  AMAdminSecretAndHook realAdminSecretHash hook ->
    withAuthorization realAdminSecretHash $ userInfoFromAuthHook logger manager hook rawHeaders

  AMAdminSecretAndJWT realAdminSecretHash jwtSecret unAuthRole ->
    withAuthorization realAdminSecretHash $ processJwt jwtSecret rawHeaders unAuthRole

  where
    mkUserInfoFallbackAdminRole adminSecretState =
      mkUserInfo (URBFromSessionVariablesFallback adminRoleName)
      adminSecretState sessionVariables

    sessionVariables = mkSessionVariables rawHeaders

    withAuthorization
      :: AdminSecretHash -> m (UserInfo, Maybe UTCTime) -> m (UserInfo, Maybe UTCTime)
    withAuthorization realAdminSecretHash actionIfNoAdminSecret = do
      let maybeRequestAdminSecret =
            foldl1 (<|>) $ map (`getSessionVariableValue` sessionVariables)
            [adminSecretHeader, deprecatedAccessKeyHeader]

      -- when admin secret is absent, run the action to retrieve UserInfo
      case maybeRequestAdminSecret of
        Nothing                 -> actionIfNoAdminSecret
        Just requestAdminSecret -> do
          when (hashAdminSecret requestAdminSecret /= realAdminSecretHash) $ throw401 $
            "invalid " <> adminSecretHeader <> "/" <> deprecatedAccessKeyHeader
          withNoExpTime $ mkUserInfoFallbackAdminRole UAdminSecretSent

    withNoExpTime a = (, Nothing) <$> a
