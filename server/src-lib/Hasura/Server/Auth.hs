{-# LANGUAGE DerivingStrategies #-}

module Hasura.Server.Auth
  ( getUserInfoWithExpTime,
    AuthMode (..),
    setupAuthMode,
    AdminSecretHash,
    hashAdminSecret,

    -- * WebHook related
    AuthHookType (..),
    AuthHookG (..),
    AuthHook,

    -- * JWT related
    RawJWT,
    JWTConfig (..),
    JWTCtx (..),
    JWKSet (..),
    processJwt,
    updateJwkRef,
    UserAuthentication (..),

    -- * Exposed for testing
    getUserInfoWithExpTime_,
  )
where

import Control.Concurrent.Extended (ForkableMonadIO, forkManagedT)
import Control.Monad.Morph (hoist)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Managed (ManagedT)
import Crypto.Hash qualified as Crypto
import Data.ByteArray qualified as BA
import Data.ByteString (ByteString)
import Data.HashSet qualified as Set
import Data.Hashable qualified as Hash
import Data.IORef (newIORef)
import Data.Text.Encoding qualified as T
import Data.Time.Clock (UTCTime)
import Hasura.Base.Error
import Hasura.GraphQL.Transport.HTTP.Protocol (ReqsText)
import Hasura.Logging
import Hasura.Prelude
import Hasura.Server.Auth.JWT hiding (processJwt_)
import Hasura.Server.Auth.WebHook
import Hasura.Server.Utils
import Hasura.Session
import Hasura.Tracing qualified as Tracing
import Network.HTTP.Client qualified as H
import Network.HTTP.Types qualified as N

-- | Typeclass representing the @UserInfo@ authorization and resolving effect
class (Monad m) => UserAuthentication m where
  resolveUserInfo ::
    Logger Hasura ->
    H.Manager ->
    -- | request headers
    [N.Header] ->
    AuthMode ->
    Maybe ReqsText ->
    m (Either QErr (UserInfo, Maybe UTCTime, [N.Header]))

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
newtype AdminSecretHash = AdminSecretHash (Crypto.Digest Crypto.SHA512)
  deriving (Ord, Eq)

instance Hash.Hashable AdminSecretHash where
  hashWithSalt salt (AdminSecretHash h) = Hash.hashWithSalt @ByteString salt $ BA.convert h

-- We don't want to be able to leak the secret hash. This is a dummy instance
-- to support 'Show AuthMode' which we want for testing.
instance Show AdminSecretHash where
  show _ = "(error \"AdminSecretHash hidden\")"

hashAdminSecret :: Text -> AdminSecretHash
hashAdminSecret = AdminSecretHash . Crypto.hash . T.encodeUtf8

-- | The methods we'll use to derive roles for authenticating requests.
--
-- @Maybe RoleName@ below is the optionally-defined role for the
-- unauthenticated (anonymous) user.
--
-- See: https://hasura.io/docs/latest/graphql/core/auth/authentication/unauthenticated-access.html
data AuthMode
  = AMNoAuth
  | AMAdminSecret !(Set.HashSet AdminSecretHash) !(Maybe RoleName)
  | AMAdminSecretAndHook !(Set.HashSet AdminSecretHash) !AuthHook
  | AMAdminSecretAndJWT !(Set.HashSet AdminSecretHash) !JWTCtx !(Maybe RoleName)
  deriving (Show, Eq)

-- | Validate the user's requested authentication configuration, launching any
-- required maintenance threads for JWT etc.
--
-- This must only be run once, on launch.
setupAuthMode ::
  ( ForkableMonadIO m,
    Tracing.HasReporter m
  ) =>
  Set.HashSet AdminSecretHash ->
  Maybe AuthHook ->
  Maybe JWTConfig ->
  Maybe RoleName ->
  H.Manager ->
  Logger Hasura ->
  ExceptT Text (ManagedT m) AuthMode
setupAuthMode adminSecretHashSet mWebHook mJwtSecret mUnAuthRole httpManager logger =
  case (not (Set.null adminSecretHashSet), mWebHook, mJwtSecret) of
    (True, Nothing, Nothing) -> return $ AMAdminSecret adminSecretHashSet mUnAuthRole
    (True, Nothing, Just jwtConf) -> do
      jwtCtx <- mkJwtCtx jwtConf
      return $ AMAdminSecretAndJWT adminSecretHashSet jwtCtx mUnAuthRole
    -- Nothing below this case uses unauth role. Throw a fatal error if we would otherwise ignore
    -- that parameter, lest users misunderstand their auth configuration:
    _
      | isJust mUnAuthRole ->
        throwError $
          "Fatal Error: --unauthorized-role (HASURA_GRAPHQL_UNAUTHORIZED_ROLE)"
            <> requiresAdminScrtMsg
            <> " and is not allowed when --auth-hook (HASURA_GRAPHQL_AUTH_HOOK) is set"
    (False, Nothing, Nothing) -> return AMNoAuth
    (True, Just hook, Nothing) -> return $ AMAdminSecretAndHook adminSecretHashSet hook
    (False, Just _, Nothing) ->
      throwError $
        "Fatal Error : --auth-hook (HASURA_GRAPHQL_AUTH_HOOK)" <> requiresAdminScrtMsg
    (False, Nothing, Just _) ->
      throwError $
        "Fatal Error : --jwt-secret (HASURA_GRAPHQL_JWT_SECRET)" <> requiresAdminScrtMsg
    (_, Just _, Just _) ->
      throwError
        "Fatal Error: Both webhook and JWT mode cannot be enabled at the same time"
  where
    requiresAdminScrtMsg =
      " requires --admin-secret (HASURA_GRAPHQL_ADMIN_SECRET) or "
        <> " --access-key (HASURA_GRAPHQL_ACCESS_KEY) to be set"

    mkJwtCtx ::
      ( ForkableMonadIO m,
        Tracing.HasReporter m
      ) =>
      JWTConfig ->
      ExceptT Text (ManagedT m) JWTCtx
    mkJwtCtx JWTConfig {..} = do
      jwkRef <- case jcKeyOrUrl of
        Left jwk -> liftIO $ newIORef (JWKSet [jwk])
        Right url -> getJwkFromUrl url
      let jwtHeader = fromMaybe JHAuthorization jcHeader
      return $ JWTCtx jwkRef jcAudience jcIssuer jcClaims jcAllowedSkew jwtHeader
      where
        -- if we can't find any expiry time for the JWK (either in @Expires@ header or @Cache-Control@
        -- header), do not start a background thread for refreshing the JWK
        getJwkFromUrl url = do
          ref <- liftIO $ newIORef $ JWKSet []
          maybeExpiry <- hoist lift $ withJwkError $ Tracing.runTraceT "jwk init" $ updateJwkRef logger httpManager url ref
          case maybeExpiry of
            Nothing -> return ref
            Just time -> do
              void . lift $
                forkManagedT "jwkRefreshCtrl" logger $
                  jwkRefreshCtrl logger httpManager url ref (convertDuration time)
              return ref

        withJwkError act = do
          res <- runExceptT act
          onLeft res $ \case
            -- when fetching JWK initially, except expiry parsing error, all errors are critical
            JFEHttpException _ msg -> throwError msg
            JFEHttpError _ _ _ e -> throwError e
            JFEJwkParseError _ e -> throwError e
            JFEExpiryParseError _ _ -> return Nothing

-- | Authenticate the request using the headers and the configured 'AuthMode'.
getUserInfoWithExpTime ::
  forall m.
  (MonadIO m, MonadBaseControl IO m, MonadError QErr m, Tracing.MonadTrace m) =>
  Logger Hasura ->
  H.Manager ->
  [N.Header] ->
  AuthMode ->
  Maybe ReqsText ->
  m (UserInfo, Maybe UTCTime, [N.Header])
getUserInfoWithExpTime = getUserInfoWithExpTime_ userInfoFromAuthHook processJwt

-- Broken out for testing with mocks:
getUserInfoWithExpTime_ ::
  forall m _Manager _Logger_Hasura.
  (MonadIO m, MonadError QErr m) =>
  -- | mock 'userInfoFromAuthHook'
  ( _Logger_Hasura ->
    _Manager ->
    AuthHook ->
    [N.Header] ->
    Maybe ReqsText ->
    m (UserInfo, Maybe UTCTime, [N.Header])
  ) ->
  -- | mock 'processJwt'
  (JWTCtx -> [N.Header] -> Maybe RoleName -> m (UserInfo, Maybe UTCTime, [N.Header])) ->
  _Logger_Hasura ->
  _Manager ->
  [N.Header] ->
  AuthMode ->
  Maybe ReqsText ->
  m (UserInfo, Maybe UTCTime, [N.Header])
getUserInfoWithExpTime_ userInfoFromAuthHook_ processJwt_ logger manager rawHeaders authMode reqs = case authMode of
  AMNoAuth -> withNoExpTime $ mkUserInfoFallbackAdminRole UAuthNotSet
  -- If hasura was started with an admin secret we:
  --   - check if a secret was sent in the request
  --     - if so, check it and authorize as admin else fail
  --   - if not proceed with either webhook or JWT auth if configured
  AMAdminSecret adminSecretHashSet maybeUnauthRole ->
    checkingSecretIfSent adminSecretHashSet $
      withNoExpTime
        -- Consider unauthorized role, if not found raise admin secret header required exception
        case maybeUnauthRole of
          Nothing ->
            throw401 $
              adminSecretHeader <> "/"
                <> deprecatedAccessKeyHeader
                <> " required, but not found"
          Just unAuthRole ->
            mkUserInfo (URBPreDetermined unAuthRole) UAdminSecretNotSent sessionVariables
  -- this is the case that actually ends up consuming the request AST
  AMAdminSecretAndHook adminSecretHashSet hook ->
    checkingSecretIfSent adminSecretHashSet $ userInfoFromAuthHook_ logger manager hook rawHeaders reqs
  AMAdminSecretAndJWT adminSecretHashSet jwtSecret unAuthRole ->
    checkingSecretIfSent adminSecretHashSet $ processJwt_ jwtSecret rawHeaders unAuthRole
  where
    -- CAREFUL!:
    mkUserInfoFallbackAdminRole adminSecretState =
      mkUserInfo
        (URBFromSessionVariablesFallback adminRoleName)
        adminSecretState
        sessionVariables

    sessionVariables = mkSessionVariablesHeaders rawHeaders

    checkingSecretIfSent ::
      Set.HashSet AdminSecretHash -> m (UserInfo, Maybe UTCTime, [N.Header]) -> m (UserInfo, Maybe UTCTime, [N.Header])
    checkingSecretIfSent adminSecretHashSet actionIfNoAdminSecret = do
      let maybeRequestAdminSecret =
            foldl1 (<|>) $
              map
                (`getSessionVariableValue` sessionVariables)
                [adminSecretHeader, deprecatedAccessKeyHeader]

      -- when admin secret is absent, run the action to retrieve UserInfo
      case maybeRequestAdminSecret of
        Nothing -> actionIfNoAdminSecret
        Just requestAdminSecret -> do
          unless (Set.member (hashAdminSecret requestAdminSecret) adminSecretHashSet) $
            throw401 $
              "invalid " <> adminSecretHeader <> "/" <> deprecatedAccessKeyHeader
          withNoExpTime $ mkUserInfoFallbackAdminRole UAdminSecretSent

    withNoExpTime a = (,Nothing,[]) <$> a
