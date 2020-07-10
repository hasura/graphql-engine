{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}

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

  -- * Exposed for testing
  , getUserInfoWithExpTime_
  ) where

import qualified Control.Concurrent.Async.Lifted.Safe as LA
import           Control.Concurrent.Extended          (forkImmortal)
import           Control.Monad.Trans.Control          (MonadBaseControl)
import           Data.IORef                           (newIORef)
import           Data.Time.Clock                      (UTCTime)
import           Hasura.Server.Version                (HasVersion)

import qualified Crypto.Hash                          as Crypto
import qualified Data.Text                            as T
import qualified Data.Text.Encoding                   as T
import qualified Network.HTTP.Client                  as H
import qualified Network.HTTP.Types                   as N

import           Hasura.Logging
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.Server.Auth.JWT               hiding (processJwt_)
import           Hasura.Server.Auth.WebHook
import           Hasura.Server.Utils
import           Hasura.Session
import qualified Hasura.Tracing                       as Tracing

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
--     - prevent theoretical timing attacks from a naive `==` check
--     - prevent misuse or inadvertent leaking of the secret
--
newtype AdminSecretHash = AdminSecretHash (Crypto.Digest Crypto.SHA512)
  deriving (Ord, Eq)

-- We don't want to be able to leak the secret hash. This is a dummy instance
-- to support 'Show AuthMode' which we want for testing.
instance Show AdminSecretHash where
  show _ = "(error \"AdminSecretHash hidden\")"

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
  deriving (Show, Eq)

-- | Validate the user's requested authentication configuration, launching any
-- required maintenance threads for JWT etc.
--
-- This must only be run once, on launch.
setupAuthMode
  :: ( HasVersion
     , MonadIO m
     , MonadBaseControl IO m
     , LA.Forall (LA.Pure m)
     , Tracing.HasReporter m
     )
  => Maybe AdminSecretHash
  -> Maybe AuthHook
  -> Maybe JWTConfig
  -> Maybe RoleName
  -> H.Manager
  -> Logger Hasura
  -> ExceptT Text m AuthMode
setupAuthMode mAdminSecretHash mWebHook mJwtSecret mUnAuthRole httpManager logger =
  case (mAdminSecretHash, mWebHook, mJwtSecret) of
    (Just hash, Nothing,   Nothing)      -> return $ AMAdminSecret hash mUnAuthRole
    (Just hash, Nothing,   Just jwtConf) -> do
      jwtCtx <- mkJwtCtx jwtConf
      return $ AMAdminSecretAndJWT hash jwtCtx mUnAuthRole
    -- Nothing below this case uses unauth role. Throw a fatal error if we would otherwise ignore
    -- that parameter, lest users misunderstand their auth configuration:
    _ | isJust mUnAuthRole -> throwError $
        "Fatal Error: --unauthorized-role (HASURA_GRAPHQL_UNAUTHORIZED_ROLE)"
          <> requiresAdminScrtMsg
          <> " and is not allowed when --auth-hook (HASURA_GRAPHQL_AUTH_HOOK) is set"

    (Nothing,  Nothing,   Nothing)  -> return AMNoAuth
    (Just hash, Just hook, Nothing) -> return $ AMAdminSecretAndHook hash hook

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

    -- | Given the 'JWTConfig' (the user input of JWT configuration), create
    -- the 'JWTCtx' (the runtime JWT config used)
    -- mkJwtCtx :: HasVersion => JWTConfig -> m JWTCtx
    mkJwtCtx
      :: ( HasVersion
         , MonadIO m
         , MonadBaseControl IO m
         , LA.Forall (LA.Pure m)
         , Tracing.HasReporter m
         )
      => JWTConfig
      -> ExceptT T.Text m JWTCtx
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
          maybeExpiry <- withJwkError $ Tracing.runTraceT "jwk init" $ updateJwkRef logger httpManager url ref
          case maybeExpiry of
            Nothing   -> return ref
            Just time -> do
              void . lift $ forkImmortal "jwkRefreshCtrl" logger $
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
  :: (HasVersion, MonadIO m, MonadBaseControl IO m, MonadError QErr m, Tracing.MonadTrace m)
  => Logger Hasura
  -> H.Manager
  -> [N.Header]
  -> AuthMode
  -> m UserInfo
getUserInfo l m r a = fst <$> getUserInfoWithExpTime l m r a

-- | Authenticate the request using the headers and the configured 'AuthMode'.
getUserInfoWithExpTime
  :: forall m. (HasVersion, MonadIO m, MonadBaseControl IO m, MonadError QErr m, Tracing.MonadTrace m)
  => Logger Hasura
  -> H.Manager
  -> [N.Header]
  -> AuthMode
  -> m (UserInfo, Maybe UTCTime)
getUserInfoWithExpTime = getUserInfoWithExpTime_ userInfoFromAuthHook processJwt

-- Broken out for testing with mocks:
getUserInfoWithExpTime_
  :: forall m _Manager _Logger_Hasura. (MonadIO m, MonadError QErr m)
  => (_Logger_Hasura -> _Manager -> AuthHook -> [N.Header] -> m (UserInfo, Maybe UTCTime))
  -- ^ mock 'userInfoFromAuthHook'
  -> (JWTCtx -> [N.Header] -> Maybe RoleName -> m (UserInfo, Maybe UTCTime))
  -- ^ mock 'processJwt'
  -> _Logger_Hasura
  -> _Manager
  -> [N.Header]
  -> AuthMode
  -> m (UserInfo, Maybe UTCTime)
getUserInfoWithExpTime_ userInfoFromAuthHook_ processJwt_ logger manager rawHeaders = \case

  AMNoAuth -> withNoExpTime $ mkUserInfoFallbackAdminRole UAuthNotSet

  -- If hasura was started with an admin secret we:
  --   - check if a secret was sent in the request
  --     - if so, check it and authorize as admin else fail
  --   - if not proceed with either webhook or JWT auth if configured
  AMAdminSecret realAdminSecretHash maybeUnauthRole ->
    checkingSecretIfSent realAdminSecretHash $ withNoExpTime $
      -- Consider unauthorized role, if not found raise admin secret header required exception
      case maybeUnauthRole of
        Nothing -> throw401 $ adminSecretHeader <> "/"
                   <> deprecatedAccessKeyHeader <> " required, but not found"
        Just unAuthRole ->
          mkUserInfo (URBPreDetermined unAuthRole) UAdminSecretNotSent sessionVariables

  AMAdminSecretAndHook realAdminSecretHash hook ->
    checkingSecretIfSent realAdminSecretHash $ userInfoFromAuthHook_ logger manager hook rawHeaders

  AMAdminSecretAndJWT realAdminSecretHash jwtSecret unAuthRole ->
    checkingSecretIfSent realAdminSecretHash $ processJwt_ jwtSecret rawHeaders unAuthRole

  where
    -- CAREFUL!:
    mkUserInfoFallbackAdminRole adminSecretState =
      mkUserInfo (URBFromSessionVariablesFallback adminRoleName)
      adminSecretState sessionVariables

    sessionVariables = mkSessionVariables rawHeaders

    checkingSecretIfSent
      :: AdminSecretHash -> m (UserInfo, Maybe UTCTime) -> m (UserInfo, Maybe UTCTime)
    checkingSecretIfSent realAdminSecretHash actionIfNoAdminSecret = do
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
