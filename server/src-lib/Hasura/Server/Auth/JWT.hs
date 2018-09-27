{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Hasura.Server.Auth.JWT
  ( processJwt
  , RawJWT
  , JWTConfig (..)
  , JWTCtx (..)
  , JWKSet (..)
  , updateJwkRef
  , jwkRefreshCtrl
  ) where

import           Control.Exception               (try)
import           Control.Lens
import           Control.Monad                   (when)
import           Crypto.JWT
import           Data.IORef                      (IORef, modifyIORef, readIORef)

import           Data.List                       (find)
import           Data.Time.Clock                 (NominalDiffTime, diffUTCTime,
                                                  getCurrentTime)
import           Data.Time.Format                (defaultTimeLocale, parseTimeM)

import           Hasura.Logging                  (Logger (..))
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.Server.Auth.JWT.Internal (parseHmacKey, parseRsaKey)
import           Hasura.Server.Auth.JWT.Logging
import           Hasura.Server.Utils             (accessKeyHeader, bsToTxt,
                                                  userRoleHeader)

import qualified Control.Concurrent              as C
import qualified Data.Aeson                      as A
import qualified Data.Aeson.Casing               as A
import qualified Data.Aeson.TH                   as A
import qualified Data.ByteString.Lazy            as BL
import qualified Data.ByteString.Lazy.Char8      as BLC
import qualified Data.CaseInsensitive            as CI
import qualified Data.HashMap.Strict             as Map
import qualified Data.String.Conversions         as CS
import qualified Data.Text                       as T
import qualified Network.HTTP.Client             as HTTP
import qualified Network.HTTP.Types              as HTTP
import qualified Network.URI                     as N
import qualified Network.Wreq                    as Wreq


newtype RawJWT = RawJWT BL.ByteString

data JWTConfig
  = JWTConfig
  { jcType     :: !T.Text
  , jcKeyOrUrl :: !(Either JWK N.URI)
  , jcClaimNs  :: !(Maybe T.Text)
  , jcAudience :: !(Maybe T.Text)
  -- , jcIssuer   :: !(Maybe T.Text)
  } deriving (Show, Eq)

data JWTCtx
  = JWTCtx
  { jcxKey      :: !(IORef JWKSet)
  , jcxClaimNs  :: !(Maybe T.Text)
  , jcxAudience :: !(Maybe T.Text)
  } deriving (Show, Eq)

instance Show (IORef JWKSet) where
  show _ = "<IORef JWKRef>"

data HasuraClaims
  = HasuraClaims
  { _cmAllowedRoles :: ![RoleName]
  , _cmDefaultRole  :: !RoleName
  } deriving (Show, Eq)
$(A.deriveJSON (A.aesonDrop 3 A.snakeCase) ''HasuraClaims)

allowedRolesClaim :: T.Text
allowedRolesClaim = "x-hasura-allowed-roles"

defaultRoleClaim :: T.Text
defaultRoleClaim = "x-hasura-default-role"

defaultClaimNs :: T.Text
defaultClaimNs = "https://hasura.io/jwt/claims"

-- | create a background thread to refresh the JWK
jwkRefreshCtrl
  :: (MonadIO m)
  => Logger
  -> HTTP.Manager
  -> N.URI
  -> IORef JWKSet
  -> NominalDiffTime
  -> m ()
jwkRefreshCtrl lggr mngr url ref time =
  void $ liftIO $ C.forkIO $ do
    C.threadDelay $ delay time
    forever $ do
      res <- runExceptT $ updateJwkRef lggr mngr url ref
      mTime <- either (const $ return Nothing) return res
      C.threadDelay $ maybe (60 * aSecond) delay mTime
  where
    delay t = (floor (realToFrac t :: Double) - 10) * aSecond
    aSecond = 1000 * 1000


-- | Given a JWK url, fetch JWK from it and update the IORef
updateJwkRef
  :: ( MonadIO m
     , MonadError T.Text m)
  => Logger
  -> HTTP.Manager
  -> N.URI
  -> IORef JWKSet
  -> m (Maybe NominalDiffTime)
updateJwkRef (Logger logger) manager url jwkRef = do
  let options = Wreq.defaults
              & Wreq.checkResponse ?~ (\_ _ -> return ())
              & Wreq.manager .~ Right manager

  res  <- liftIO $ try $ Wreq.getWith options $ show url
  resp <- either logAndThrowHttp return res
  let status = resp ^. Wreq.responseStatus
      respBody = resp ^. Wreq.responseBody

  when (status ^. Wreq.statusCode /= 200) $ do
    let urlT = T.pack $ show url
        respBodyT = Just $ CS.cs respBody
        errMsg = "non-200 response on fetching JWK from: " <> urlT
        httpErr = Just (JwkRefreshHttpError (Just status) urlT Nothing respBodyT)
    logAndThrow errMsg httpErr

  jwkset <- either (\e -> logAndThrow (T.pack e) Nothing) return . A.eitherDecode $ respBody
  liftIO $ modifyIORef jwkRef (const jwkset)

  let mExpiresT = resp ^? Wreq.responseHeader "Expires"
  forM mExpiresT $ \expiresT -> do
    let expiresE = parseTimeM True defaultTimeLocale timeFmt $ CS.cs expiresT
    expires  <- either (`logAndThrow` Nothing) return expiresE
    currTime <- liftIO getCurrentTime
    return $ diffUTCTime expires currTime

  where
    logAndThrow :: (MonadIO m, MonadError T.Text m) => T.Text -> Maybe JwkRefreshHttpError -> m a
    logAndThrow err httpErr = do
      liftIO $ logger $ mkJwkRefreshLog err httpErr
      throwError err

    logAndThrowHttp :: (MonadIO m, MonadError T.Text m) => HTTP.HttpException -> m a
    logAndThrowHttp err = do
      let httpErr = JwkRefreshHttpError Nothing (T.pack $ show url) (Just err) Nothing
          errMsg = "error fetching JWK: " <> T.pack (show err)
      logAndThrow errMsg (Just httpErr)

    timeFmt = "%a, %d %b %Y %T GMT"


-- | Process the request headers to verify the JWT and extract UserInfo from it
processJwt
  :: ( MonadIO m
     , MonadError QErr m)
  => JWTCtx
  -> HTTP.RequestHeaders
  -> m UserInfo
processJwt jwtCtx headers = do
  -- try to parse JWT token from Authorization header
  jwt <- parseAuthzHeader

  -- verify the JWT
  claims <- liftJWTError invalidJWTError $ verifyJwt jwtCtx $ RawJWT jwt

  let claimsNs = fromMaybe defaultClaimNs $ jcxClaimNs jwtCtx

  -- see if the hasura claims key exist in the claims map
  let mHasuraClaims = Map.lookup claimsNs $ claims ^. unregisteredClaims
  hasuraClaimsV <- maybe claimsNotFound return mHasuraClaims
  -- the value of hasura claims key has to be an object
  hasuraClaims <- validateIsObject hasuraClaimsV

  -- filter only x-hasura claims and convert to lower-case
  let claimsMap = Map.filterWithKey (\k _ -> T.isPrefixOf "x-hasura-" k)
                $ Map.fromList $ map (\(k, v) -> (T.toLower k, v))
                $ Map.toList hasuraClaims

  HasuraClaims allowedRoles defaultRole <- parseHasuraClaims claimsMap
  let role = getCurrentRole defaultRole

  when (role `notElem` allowedRoles) currRoleNotAllowed
  let finalClaims =
        Map.delete defaultRoleClaim . Map.delete allowedRolesClaim $ claimsMap

  -- transform the map of text:aeson-value -> text:text
  metadata <- decodeJSON $ A.Object finalClaims

  -- delete the x-hasura-access-key from this map, and insert x-hasura-role
  let hasuraMd = Map.insert userRoleHeader (getRoleTxt role) $
        Map.delete accessKeyHeader metadata

  return $ UserInfo role hasuraMd

  where
    parseAuthzHeader = do
      let mAuthzHeader = find (\h -> fst h == CI.mk "Authorization") headers
      (_, authzHeader) <- maybe missingAuthzHeader return mAuthzHeader
      let tokenParts = BLC.words $ BL.fromStrict authzHeader
      case tokenParts of
        ["Bearer", jwt] -> return jwt
        _               -> malformedAuthzHeader

    validateIsObject jVal =
      case jVal of
        A.Object x -> return x
        _          -> throw400 JWTInvalidClaims "hasura claims should be an object"

    -- see if there is a x-hasura-role header, or else pick the default role
    getCurrentRole defaultRole =
      let userRoleHeaderB = CS.cs userRoleHeader
          mUserRole = snd <$> find (\h -> fst h == CI.mk userRoleHeaderB) headers
      in maybe defaultRole (RoleName . bsToTxt) mUserRole

    decodeJSON val = case A.fromJSON val of
      A.Error e   -> throw400 JWTInvalidClaims ("x-hasura-* claims: " <> T.pack e)
      A.Success a -> return a

    liftJWTError :: (MonadError e' m) => (e -> e') -> ExceptT e m a -> m a
    liftJWTError ef action = do
      res <- runExceptT action
      either (throwError . ef) return res

    invalidJWTError e =
      err400 JWTInvalid $ "Could not verify JWT: " <> T.pack (show e)

    malformedAuthzHeader =
      throw400 InvalidHeaders "Malformed Authorization header"
    missingAuthzHeader =
      throw400 InvalidHeaders "Missing Authorization header in JWT authentication mode"
    currRoleNotAllowed =
      throw400 AccessDenied "Your current role is not in allowed roles"
    claimsNotFound = do
      let claimsNs = fromMaybe defaultClaimNs $ jcxClaimNs jwtCtx
      throw400 JWTInvalidClaims $ "claims key: '" <> claimsNs <> "' not found"


-- parse x-hasura-allowed-roles, x-hasura-default-role from JWT claims
parseHasuraClaims
  :: (MonadError QErr m)
  => A.Object -> m HasuraClaims
parseHasuraClaims claimsMap = do
  let mAllowedRolesV = Map.lookup allowedRolesClaim claimsMap
  allowedRolesV <- maybe missingAllowedRolesClaim return mAllowedRolesV
  allowedRoles <- parseJwtClaim (A.fromJSON allowedRolesV) errMsg

  let mDefaultRoleV = Map.lookup defaultRoleClaim claimsMap
  defaultRoleV <- maybe missingDefaultRoleClaim return mDefaultRoleV
  defaultRole <- parseJwtClaim (A.fromJSON defaultRoleV) errMsg

  return $ HasuraClaims allowedRoles defaultRole

  where
    missingAllowedRolesClaim =
      let msg = "JWT claim does not contain " <> allowedRolesClaim
      in throw400 JWTRoleClaimMissing msg

    missingDefaultRoleClaim =
      let msg = "JWT claim does not contain " <> defaultRoleClaim
      in throw400 JWTRoleClaimMissing msg

    errMsg _ = "invalid " <> allowedRolesClaim <> "; should be a list of roles"

    parseJwtClaim :: (MonadError QErr m) => A.Result a -> (String -> Text) -> m a
    parseJwtClaim res errFn =
      case res of
        A.Success val -> return val
        A.Error e     -> throw400 JWTInvalidClaims $ errFn e


-- | Verify the JWT against given JWK
verifyJwt
  :: ( MonadError JWTError m
     , MonadIO m
     )
  => JWTCtx
  -> RawJWT
  -> m ClaimsSet
verifyJwt ctx (RawJWT rawJWT) = do
  key <- liftIO $ readIORef $ jcxKey ctx
  jwt <- decodeCompact rawJWT
  t   <- liftIO getCurrentTime
  verifyClaimsAt config key t jwt
  where
    audCheck aud = maybe True (== (T.pack . show) aud) $ jcxAudience ctx
    config = defaultJWTValidationSettings audCheck


-- | Parse from a json string like:
-- | `{"type": "RS256", "key": "<PEM-encoded-public-key-or-X509-cert>"}`
-- | to JWTConfig
instance A.FromJSON JWTConfig where

  parseJSON = A.withObject "JWTConfig" $ \o -> do
    keyType <- o A..: "type"
    mRawKey <- o A..:? "key"
    claimNs <- o A..:? "claims_namespace"
    aud     <- o A..:? "audience"
    jwkUrl  <- o A..:? "jwk_url"

    case (mRawKey, jwkUrl) of
      (Nothing, Nothing) -> fail "key and jwk_url both cannot be empty"
      (Just _, Just _)   -> fail "key, jwk_url both cannot be present"
      (Just rawKey, Nothing) -> do
        key <- parseKey keyType rawKey
        return $ JWTConfig keyType (Left key) claimNs aud
      (Nothing, Just url) ->
        return $ JWTConfig keyType (Right url) claimNs aud

    where
      parseKey keyType rawKey =
       case keyType of
          "HS256" -> runEither $ parseHmacKey rawKey 256
          "HS384" -> runEither $ parseHmacKey rawKey 384
          "HS512" -> runEither $ parseHmacKey rawKey 512
          "RS256" -> runEither $ parseRsaKey rawKey
          "RS384" -> runEither $ parseRsaKey rawKey
          "RS512" -> runEither $ parseRsaKey rawKey
          -- TODO: support ES256, ES384, ES512, PS256, PS384
          _       -> invalidJwk ("Key type: " <> T.unpack keyType <> " is not supported")

      runEither = either (invalidJwk . T.unpack) return
      invalidJwk msg = fail ("Invalid JWK: " <> msg)
