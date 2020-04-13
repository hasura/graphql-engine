module Hasura.Server.Auth.JWT
  ( processJwt
  , RawJWT
  , JWTConfig (..)
  , JWTCtx (..)
  , Jose.JWKSet (..)
  , JWTClaimsFormat (..)
  , JwkFetchError (..)
  , updateJwkRef
  , jwkRefreshCtrl
  , defaultClaimNs
  ) where

import           Control.Exception               (try)
import           Control.Lens
import           Control.Monad                   (when)
import           Control.Monad.Trans.Maybe
import           Data.IORef                      (IORef, readIORef, writeIORef)
import           Data.List                       (find)
import           Data.Time.Clock                 (NominalDiffTime, UTCTime, diffUTCTime,
                                                  getCurrentTime)
import           GHC.AssertNF
import           Network.URI                     (URI)

import           Data.Parser.CacheControl
import           Data.Parser.Expires
import           Hasura.HTTP
import           Hasura.Logging                  (Hasura, LogLevel (..), Logger (..))
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.Server.Auth.JWT.Internal (parseHmacKey, parseRsaKey)
import           Hasura.Server.Auth.JWT.Logging
import           Hasura.Server.Utils             (getRequestHeader, isSessionVariable,
                                                  userRoleHeader)
import           Hasura.Server.Version           (HasVersion)
import           Hasura.Session

import qualified Control.Concurrent.Extended     as C
import qualified Crypto.JWT                      as Jose
import qualified Data.Aeson                      as J
import qualified Data.Aeson.Casing               as J
import qualified Data.Aeson.TH                   as J
import qualified Data.ByteString.Lazy            as BL
import qualified Data.ByteString.Lazy.Char8      as BLC
import qualified Data.CaseInsensitive            as CI
import qualified Data.HashMap.Strict             as Map
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as T
import qualified Network.HTTP.Client             as HTTP
import qualified Network.HTTP.Types              as HTTP
import qualified Network.Wreq                    as Wreq


newtype RawJWT = RawJWT BL.ByteString

data JWTClaimsFormat
  = JCFJson
  | JCFStringifiedJson
  deriving (Show, Eq)

$(J.deriveJSON J.defaultOptions { J.sumEncoding = J.ObjectWithSingleField
                                , J.constructorTagModifier = J.snakeCase . drop 3 } ''JWTClaimsFormat)

data JWTConfig
  = JWTConfig
  { jcType         :: !T.Text
  , jcKeyOrUrl     :: !(Either Jose.JWK URI)
  , jcClaimNs      :: !(Maybe T.Text)
  , jcAudience     :: !(Maybe Jose.Audience)
  , jcClaimsFormat :: !(Maybe JWTClaimsFormat)
  , jcIssuer       :: !(Maybe Jose.StringOrURI)
  } deriving (Show, Eq)

data JWTCtx
  = JWTCtx
  { jcxKey          :: !(IORef Jose.JWKSet)
  , jcxClaimNs      :: !(Maybe T.Text)
  , jcxAudience     :: !(Maybe Jose.Audience)
  , jcxClaimsFormat :: !JWTClaimsFormat
  , jcxIssuer       :: !(Maybe Jose.StringOrURI)
  } deriving (Eq)

instance Show JWTCtx where
  show (JWTCtx _ nsM audM cf iss) =
    show ["<IORef JWKSet>", show nsM, show audM, show cf, show iss]

data HasuraClaims
  = HasuraClaims
  { _cmAllowedRoles :: ![RoleName]
  , _cmDefaultRole  :: !RoleName
  } deriving (Show, Eq)
$(J.deriveJSON (J.aesonDrop 3 J.snakeCase) ''HasuraClaims)

allowedRolesClaim :: T.Text
allowedRolesClaim = "x-hasura-allowed-roles"

defaultRoleClaim :: T.Text
defaultRoleClaim = "x-hasura-default-role"

defaultClaimNs :: T.Text
defaultClaimNs = "https://hasura.io/jwt/claims"


-- | An action that refreshes the JWK at intervals in an infinite loop.
jwkRefreshCtrl
  :: HasVersion
  => Logger Hasura
  -> HTTP.Manager
  -> URI
  -> IORef Jose.JWKSet
  -> DiffTime
  -> IO void
jwkRefreshCtrl logger manager url ref time = liftIO $ do
    C.sleep time
    forever $ do
      res <- runExceptT $ updateJwkRef logger manager url ref
      mTime <- either (const $ logNotice >> return Nothing) return res
      -- if can't parse time from header, defaults to 1 min
      let delay = maybe (minutes 1) fromUnits mTime
      C.sleep delay
  where
    logNotice = do
      let err = JwkRefreshLog LevelInfo (Just "retrying again in 60 secs") Nothing
      liftIO $ unLogger logger err

-- | Given a JWK url, fetch JWK from it and update the IORef
updateJwkRef
  :: ( HasVersion
     , MonadIO m
     , MonadError JwkFetchError m
     )
  => Logger Hasura
  -> HTTP.Manager
  -> URI
  -> IORef Jose.JWKSet
  -> m (Maybe NominalDiffTime)
updateJwkRef (Logger logger) manager url jwkRef = do
  let options = wreqOptions manager []
      urlT    = T.pack $ show url
      infoMsg = "refreshing JWK from endpoint: " <> urlT
  liftIO $ logger $ JwkRefreshLog LevelInfo (Just infoMsg) Nothing
  res  <- liftIO $ try $ Wreq.getWith options $ show url
  resp <- either logAndThrowHttp return res
  let status = resp ^. Wreq.responseStatus
      respBody = resp ^. Wreq.responseBody
      statusCode = status ^. Wreq.statusCode

  unless (statusCode >= 200 && statusCode < 300) $ do
    let errMsg = "Non-2xx response on fetching JWK from: " <> urlT
        err = JFEHttpError url status respBody errMsg
    logAndThrow err

  let parseErr e = JFEJwkParseError (T.pack e) $ "Error parsing JWK from url: " <> urlT
  !jwkset <- either (logAndThrow . parseErr) return $ J.eitherDecode' respBody
  liftIO $ do
    $assertNFHere jwkset  -- so we don't write thunks to mutable vars
    writeIORef jwkRef jwkset

  -- first check for Cache-Control header to get max-age, if not found, look for Expires header
  runMaybeT $ timeFromCacheControl resp <|> timeFromExpires resp

  where
    parseCacheControlErr e =
      JFEExpiryParseError (Just e)
      "Failed parsing Cache-Control header from JWK response. Could not find max-age or s-maxage"
    parseTimeErr =
      JFEExpiryParseError Nothing
      "Failed parsing Expires header from JWK response. Value of header is not a valid timestamp"

    timeFromCacheControl resp = do
      header <- afold $ bsToTxt <$> resp ^? Wreq.responseHeader "Cache-Control"
      fromInteger <$> parseMaxAge header `onLeft` \err -> logAndThrowInfo $ parseCacheControlErr $ T.pack err
    timeFromExpires resp = do
      header <- afold $ bsToTxt <$> resp ^? Wreq.responseHeader "Expires"
      expiry <- parseExpirationTime header `onLeft` const (logAndThrowInfo parseTimeErr)
      diffUTCTime expiry <$> liftIO getCurrentTime

    logAndThrowInfo :: (MonadIO m, MonadError JwkFetchError m) => JwkFetchError -> m a
    logAndThrowInfo err = do
      liftIO $ logger $ JwkRefreshLog LevelInfo Nothing (Just err)
      throwError err

    logAndThrow :: (MonadIO m, MonadError JwkFetchError m) => JwkFetchError -> m a
    logAndThrow err = do
      liftIO $ logger $ JwkRefreshLog (LevelOther "critical") Nothing (Just err)
      throwError err

    logAndThrowHttp :: (MonadIO m, MonadError JwkFetchError m) => HTTP.HttpException -> m a
    logAndThrowHttp httpEx = do
      let errMsg = "Error fetching JWK: " <> T.pack (getHttpExceptionMsg httpEx)
          err = JFEHttpException (HttpException httpEx) errMsg
      logAndThrow err

    getHttpExceptionMsg = \case
      HTTP.HttpExceptionRequest _ reason -> show reason
      HTTP.InvalidUrlException _ reason -> show reason


-- | Process the request headers to verify the JWT and extract UserInfo from it
processJwt
  :: ( MonadIO m
     , MonadError QErr m)
  => JWTCtx
  -> HTTP.RequestHeaders
  -> Maybe RoleName
  -> m (UserInfo, Maybe UTCTime)
processJwt jwtCtx headers mUnAuthRole =
  maybe withoutAuthZHeader withAuthZHeader mAuthZHeader
  where
    mAuthZHeader = find (\h -> fst h == CI.mk "Authorization") headers

    withAuthZHeader (_, authzHeader) =
      processAuthZHeader jwtCtx headers $ BL.fromStrict authzHeader

    withoutAuthZHeader = do
      unAuthRole <- maybe missingAuthzHeader return mUnAuthRole
      userInfo <- mkUserInfo UAdminSecretNotSent (mkSessionVariables headers) $ Just unAuthRole
      pure (userInfo, Nothing)

    missingAuthzHeader =
      throw400 InvalidHeaders "Missing Authorization header in JWT authentication mode"

processAuthZHeader
  :: ( MonadIO m
     , MonadError QErr m)
  => JWTCtx
  -> HTTP.RequestHeaders
  -> BLC.ByteString
  -> m (UserInfo, Maybe UTCTime)
processAuthZHeader jwtCtx headers authzHeader = do
  -- try to parse JWT token from Authorization header
  jwt <- parseAuthzHeader

  -- verify the JWT
  claims <- liftJWTError invalidJWTError $ verifyJwt jwtCtx $ RawJWT jwt

  let claimsNs  = fromMaybe defaultClaimNs $ jcxClaimNs jwtCtx
      claimsFmt = jcxClaimsFormat jwtCtx
      expTimeM = fmap (\(Jose.NumericDate t) -> t) $ claims ^. Jose.claimExp

  -- see if the hasura claims key exist in the claims map
  let mHasuraClaims = Map.lookup claimsNs $ claims ^. Jose.unregisteredClaims
  hasuraClaimsV <- maybe claimsNotFound return mHasuraClaims

  -- get hasura claims value as an object. parse from string possibly
  hasuraClaims <- parseObjectFromString claimsFmt hasuraClaimsV

  -- filter only x-hasura claims and convert to lower-case
  let claimsMap = Map.filterWithKey (\k _ -> isSessionVariable k)
                $ Map.fromList $ map (first T.toLower)
                $ Map.toList hasuraClaims

  HasuraClaims allowedRoles defaultRole <- parseHasuraClaims claimsMap
  let roleName = getCurrentRole defaultRole

  when (roleName `notElem` allowedRoles) currRoleNotAllowed
  let finalClaims =
        Map.delete defaultRoleClaim . Map.delete allowedRolesClaim $ claimsMap

  -- transform the map of text:aeson-value -> text:text
  metadata <- decodeJSON $ J.Object finalClaims
  userInfo <- mkUserInfo UAdminSecretNotSent
              (mkSessionVariablesText $ Map.toList metadata) $ Just roleName
  pure (userInfo, expTimeM)
  where
    parseAuthzHeader = do
      let tokenParts = BLC.words authzHeader
      case tokenParts of
        ["Bearer", jwt] -> return jwt
        _               -> malformedAuthzHeader

    parseObjectFromString claimsFmt jVal =
      case (claimsFmt, jVal) of
        (JCFStringifiedJson, J.String v) ->
          either (const $ claimsErr $ strngfyErr v) return
          $ J.eitherDecodeStrict $ T.encodeUtf8 v
        (JCFStringifiedJson, _) ->
          claimsErr "expecting a string when claims_format is stringified_json"
        (JCFJson, J.Object o) -> return o
        (JCFJson, _) ->
          claimsErr "expecting a json object when claims_format is json"

    strngfyErr v = "expecting stringified json at: '"
                   <> fromMaybe defaultClaimNs (jcxClaimNs jwtCtx)
                   <> "', but found: " <> v

    claimsErr = throw400 JWTInvalidClaims

    -- see if there is a x-hasura-role header, or else pick the default role
    getCurrentRole defaultRole =
      let mUserRole = getRequestHeader userRoleHeader headers
      in fromMaybe defaultRole $ mUserRole >>= mkRoleName . bsToTxt

    decodeJSON val = case J.fromJSON val of
      J.Error e   -> throw400 JWTInvalidClaims ("x-hasura-* claims: " <> T.pack e)
      J.Success a -> return a

    liftJWTError :: (MonadError e' m) => (e -> e') -> ExceptT e m a -> m a
    liftJWTError ef action = do
      res <- runExceptT action
      either (throwError . ef) return res

    invalidJWTError e =
      err400 JWTInvalid $ "Could not verify JWT: " <> T.pack (show e)

    malformedAuthzHeader =
      throw400 InvalidHeaders "Malformed Authorization header"
    currRoleNotAllowed =
      throw400 AccessDenied "Your current role is not in allowed roles"
    claimsNotFound = do
      let claimsNs = fromMaybe defaultClaimNs $ jcxClaimNs jwtCtx
      throw400 JWTInvalidClaims $ "claims key: '" <> claimsNs <> "' not found"


-- parse x-hasura-allowed-roles, x-hasura-default-role from JWT claims
parseHasuraClaims
  :: (MonadError QErr m)
  => J.Object -> m HasuraClaims
parseHasuraClaims claimsMap = do
  let mAllowedRolesV = Map.lookup allowedRolesClaim claimsMap
  allowedRolesV <- maybe missingAllowedRolesClaim return mAllowedRolesV
  allowedRoles <- parseJwtClaim (J.fromJSON allowedRolesV) errMsg

  let mDefaultRoleV = Map.lookup defaultRoleClaim claimsMap
  defaultRoleV <- maybe missingDefaultRoleClaim return mDefaultRoleV
  defaultRole <- parseJwtClaim (J.fromJSON defaultRoleV) errMsg

  return $ HasuraClaims allowedRoles defaultRole

  where
    missingAllowedRolesClaim =
      let msg = "JWT claim does not contain " <> allowedRolesClaim
      in throw400 JWTRoleClaimMissing msg

    missingDefaultRoleClaim =
      let msg = "JWT claim does not contain " <> defaultRoleClaim
      in throw400 JWTRoleClaimMissing msg

    errMsg _ = "invalid " <> allowedRolesClaim <> "; should be a list of roles"

    parseJwtClaim :: (MonadError QErr m) => J.Result a -> (String -> Text) -> m a
    parseJwtClaim res errFn =
      case res of
        J.Success val -> return val
        J.Error e     -> throw400 JWTInvalidClaims $ errFn e


-- | Verify the JWT against given JWK
verifyJwt
  :: ( MonadError Jose.JWTError m
     , MonadIO m
     )
  => JWTCtx
  -> RawJWT
  -> m Jose.ClaimsSet
verifyJwt ctx (RawJWT rawJWT) = do
  key <- liftIO $ readIORef $ jcxKey ctx
  jwt <- Jose.decodeCompact rawJWT
  t   <- liftIO getCurrentTime
  Jose.verifyClaimsAt config key t jwt
  where
    config = case jcxIssuer ctx of
      Nothing  -> Jose.defaultJWTValidationSettings audCheck
      Just iss -> Jose.defaultJWTValidationSettings audCheck
                  & set Jose.issuerPredicate (== iss)
    audCheck audience =
      -- dont perform the check if there are no audiences in the conf
      case jcxAudience ctx of
        Nothing                        -> True
        Just (Jose.Audience audiences) -> audience `elem` audiences


instance J.ToJSON JWTConfig where
  toJSON (JWTConfig ty keyOrUrl claimNs aud claimsFmt iss) =
    case keyOrUrl of
         Left _    -> mkObj ("key" J..= J.String "<JWK REDACTED>")
         Right url -> mkObj ("jwk_url" J..= url)
    where
      mkObj item = J.object [ "type" J..= ty
                            , "claims_namespace" J..= claimNs
                            , "claims_format" J..= claimsFmt
                            , "audience" J..= aud
                            , "issuer" J..= iss
                            , item
                            ]

-- | Parse from a json string like:
-- | `{"type": "RS256", "key": "<PEM-encoded-public-key-or-X509-cert>"}`
-- | to JWTConfig
instance J.FromJSON JWTConfig where

  parseJSON = J.withObject "JWTConfig" $ \o -> do
    keyType <- o J..: "type"
    mRawKey <- o J..:? "key"
    claimNs <- o J..:? "claims_namespace"
    aud     <- o J..:? "audience"
    iss     <- o J..:? "issuer"
    jwkUrl  <- o J..:? "jwk_url"
    isStrngfd <- o J..:? "claims_format"

    case (mRawKey, jwkUrl) of
      (Nothing, Nothing) -> fail "key and jwk_url both cannot be empty"
      (Just _, Just _)   -> fail "key, jwk_url both cannot be present"
      (Just rawKey, Nothing) -> do
        key <- parseKey keyType rawKey
        return $ JWTConfig keyType (Left key) claimNs aud isStrngfd iss
      (Nothing, Just url) ->
        return $ JWTConfig keyType (Right url) claimNs aud isStrngfd iss

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
