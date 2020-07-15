module Hasura.Server.Auth.JWT
  ( processJwt
  , RawJWT
  , JWTConfig (..)
  , JWTCtx (..)
  , Jose.JWKSet (..)
  , JWTClaimsFormat (..)
  , JwkFetchError (..)
  , JWTConfigClaims (..)
  , updateJwkRef
  , jwkRefreshCtrl
  , defaultClaimNs

  -- * Exposed for testing
  , processJwt_
  , allowedRolesClaim
  , defaultRoleClaim
  ) where

import           Control.Exception.Lifted        (try)
import           Control.Lens
import           Control.Monad.Trans.Control     (MonadBaseControl)
import           Control.Monad.Trans.Maybe
import           Data.IORef                      (IORef, readIORef, writeIORef)
import           Data.Time.Clock                 (NominalDiffTime, UTCTime, diffUTCTime,
                                                  getCurrentTime)
import           GHC.AssertNF
import           Network.URI                     (URI)

import           Data.Aeson.Internal             (JSONPath)
import           Data.Parser.CacheControl
import           Data.Parser.Expires
import           Hasura.HTTP
import           Hasura.Logging                  (Hasura, LogLevel (..), Logger (..))
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.Server.Auth.JWT.Internal (parseHmacKey, parseRsaKey)
import           Hasura.Server.Auth.JWT.Logging
import           Hasura.Server.Utils             (executeJSONPath, getRequestHeader,
                                                  isSessionVariable, userRoleHeader)
import           Hasura.Server.Version           (HasVersion)
import           Hasura.Session
import qualified Hasura.Tracing                  as Tracing

import qualified Control.Concurrent.Extended     as C
import qualified Crypto.JWT                      as Jose
import qualified Data.Aeson                      as J
import qualified Data.Aeson.Casing               as J
import qualified Data.Aeson.Internal             as J
import qualified Data.Aeson.TH                   as J
import qualified Data.ByteString.Lazy            as BL
import qualified Data.ByteString.Lazy.Char8      as BLC
import qualified Data.CaseInsensitive            as CI
import qualified Data.HashMap.Strict             as Map
import qualified Data.Parser.JSONPath            as JSONPath
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

data JWTConfigClaims
  = ClaimNsPath JSONPath
  | ClaimNs T.Text
  deriving (Show, Eq)

instance J.ToJSON JWTConfigClaims where
  toJSON (ClaimNsPath nsPath) = J.String . T.pack $ encodeJSONPath nsPath
  toJSON (ClaimNs ns)         = J.String ns

-- | The JWT configuration we got from the user.
data JWTConfig
  = JWTConfig
  { jcKeyOrUrl     :: !(Either Jose.JWK URI)
  , jcClaimNs      :: !JWTConfigClaims
  , jcAudience     :: !(Maybe Jose.Audience)
  , jcClaimsFormat :: !(Maybe JWTClaimsFormat)
  , jcIssuer       :: !(Maybe Jose.StringOrURI)
  } deriving (Show, Eq)

-- | The validated runtime JWT configuration returned by 'mkJwtCtx' in 'setupAuthMode'.
--
-- This is also evidence that the 'jwkRefreshCtrl' thread is running, if an
-- expiration schedule could be determined.
data JWTCtx
  = JWTCtx
  { jcxKey          :: !(IORef Jose.JWKSet)
    -- ^ This needs to be a mutable variable for 'updateJwkRef'.
  , jcxClaimNs      :: !JWTConfigClaims
  , jcxAudience     :: !(Maybe Jose.Audience)
  , jcxClaimsFormat :: !JWTClaimsFormat
  , jcxIssuer       :: !(Maybe Jose.StringOrURI)
  } deriving (Eq)

instance Show JWTCtx where
  show (JWTCtx _ nsM audM cf iss) =
    show ["<IORef JWKSet>", show nsM,show audM, show cf, show iss]

data HasuraClaims
  = HasuraClaims
  { _cmAllowedRoles :: ![RoleName]
  , _cmDefaultRole  :: !RoleName
  } deriving (Show, Eq)
$(J.deriveJSON (J.aesonDrop 3 J.snakeCase) ''HasuraClaims)


-- NOTE: these must stay lowercase; TODO consider using "Data.CaseInsensitive"
allowedRolesClaim :: T.Text
allowedRolesClaim = "x-hasura-allowed-roles"

defaultRoleClaim :: T.Text
defaultRoleClaim = "x-hasura-default-role"

defaultClaimNs :: T.Text
defaultClaimNs = "https://hasura.io/jwt/claims"


-- | An action that refreshes the JWK at intervals in an infinite loop.
jwkRefreshCtrl
  :: (HasVersion, MonadIO m, MonadBaseControl IO m, Tracing.HasReporter m)
  => Logger Hasura
  -> HTTP.Manager
  -> URI
  -> IORef Jose.JWKSet
  -> DiffTime
  -> m void
jwkRefreshCtrl logger manager url ref time = do
    liftIO $ C.sleep time
    forever $ Tracing.runTraceT "jwk refresh" do
      res <- runExceptT $ updateJwkRef logger manager url ref
      mTime <- either (const $ logNotice >> return Nothing) return res
      -- if can't parse time from header, defaults to 1 min
      -- let delay = maybe (minutes 1) fromUnits mTime
      let delay = maybe (minutes 1) (convertDuration) mTime
      liftIO $ C.sleep delay
  where
    logNotice = do
      let err = JwkRefreshLog LevelInfo (Just "retrying again in 60 secs") Nothing
      liftIO $ unLogger logger err

-- | Given a JWK url, fetch JWK from it and update the IORef
updateJwkRef
  :: ( HasVersion
     , MonadIO m
     , MonadBaseControl IO m
     , MonadError JwkFetchError m
     , Tracing.MonadTrace m
     )
  => Logger Hasura
  -> HTTP.Manager
  -> URI
  -> IORef Jose.JWKSet
  -> m (Maybe NominalDiffTime)
updateJwkRef (Logger logger) manager url jwkRef = do
  let urlT    = T.pack $ show url
      infoMsg = "refreshing JWK from endpoint: " <> urlT
  liftIO $ logger $ JwkRefreshLog LevelInfo (Just infoMsg) Nothing
  res <- try $ Tracing.traceHttpRequest urlT do
    initReq <- liftIO $ HTTP.parseRequest $ show url
    let req = initReq { HTTP.requestHeaders = addDefaultHeaders (HTTP.requestHeaders initReq) }
    pure $ Tracing.SuspendedRequest req \req' -> do
      liftIO $ HTTP.httpLbs req' manager
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
--
-- Iff no "Authorization" header was passed, we will fall back to the
-- unauthenticated user role [1], if one was configured at server start.
--
-- When no 'x-hasura-user-role' is specified in the request, the mandatory
-- 'x-hasura-default-role' [2] from the JWT claims will be used.

-- [1]: https://hasura.io/docs/1.0/graphql/manual/auth/authentication/unauthenticated-access.html 
-- [2]: https://hasura.io/docs/1.0/graphql/manual/auth/authentication/jwt.html#the-spec
processJwt
  :: ( MonadIO m
     , MonadError QErr m)
  => JWTCtx
  -> HTTP.RequestHeaders
  -> Maybe RoleName
  -> m (UserInfo, Maybe UTCTime)
processJwt = processJwt_ processAuthZHeader

-- Broken out for testing with mocks:
processJwt_
  :: (MonadError QErr m)
  => (_JWTCtx -> BLC.ByteString -> m (J.Object, Maybe UTCTime))
  -- ^ mock 'processAuthZHeader'
  -> _JWTCtx
  -> HTTP.RequestHeaders
  -> Maybe RoleName
  -> m (UserInfo, Maybe UTCTime)
processJwt_ processAuthZHeader_ jwtCtx headers mUnAuthRole =
  maybe withoutAuthZHeader withAuthZHeader mAuthZHeader
  where
    mAuthZHeader = find (\h -> fst h == CI.mk "Authorization") headers

    withAuthZHeader (_, authzHeader) = do
      (hasuraClaims, expTimeM) <- processAuthZHeader_ jwtCtx $ BL.fromStrict authzHeader

      -- filter only x-hasura claims and convert to lower-case
      let claimsMap = Map.filterWithKey (\k _ -> isSessionVariable k)
                    $ Map.fromList $ map (first T.toLower)
                    $ Map.toList hasuraClaims

      HasuraClaims allowedRoles defaultRole <- parseHasuraClaims claimsMap
      -- see if there is a x-hasura-role header, or else pick the default role.
      -- The role returned is unauthenticated at this point:
      let requestedRole = fromMaybe defaultRole $
            getRequestHeader userRoleHeader headers >>= mkRoleName . bsToTxt

      when (requestedRole `notElem` allowedRoles) $
        throw400 AccessDenied "Your requested role is not in allowed roles"
      let finalClaims =
            Map.delete defaultRoleClaim . Map.delete allowedRolesClaim $ claimsMap

      -- transform the map of text:aeson-value -> text:text
      metadata <- parseJwtClaim (J.Object finalClaims) "x-hasura-* claims"
      userInfo <- mkUserInfo (URBPreDetermined requestedRole) UAdminSecretNotSent $
                  mkSessionVariablesText $ Map.toList metadata
      pure (userInfo, expTimeM)

    withoutAuthZHeader = do
      unAuthRole <- maybe missingAuthzHeader return mUnAuthRole
      userInfo <- mkUserInfo (URBPreDetermined unAuthRole) UAdminSecretNotSent $
        mkSessionVariables headers
      pure (userInfo, Nothing)

      where
        missingAuthzHeader =
          throw400 InvalidHeaders "Missing Authorization header in JWT authentication mode"

-- Parse and verify the 'Authorization' header, returning the raw claims
-- object, and the expiration, if any.
processAuthZHeader
  :: ( MonadIO m
     , MonadError QErr m)
  => JWTCtx
  -> BLC.ByteString
  -> m (J.Object, Maybe UTCTime)
processAuthZHeader jwtCtx@JWTCtx{jcxClaimNs, jcxClaimsFormat} authzHeader = do
  -- try to parse JWT token from Authorization header
  jwt <- parseAuthzHeader

  -- verify the JWT
  claims <- liftJWTError invalidJWTError $ verifyJwt jwtCtx $ RawJWT jwt

  let expTimeM = fmap (\(Jose.NumericDate t) -> t) $ claims ^. Jose.claimExp

  -- see if the hasura claims key exists in the claims map
  let mHasuraClaims =
        case jcxClaimNs of
          ClaimNs k -> Map.lookup k $ claims ^. Jose.unregisteredClaims
          ClaimNsPath path -> parseIValueJsonValue $ executeJSONPath path (J.toJSON $ claims ^. Jose.unregisteredClaims)

  hasuraClaimsV <- maybe claimsNotFound return mHasuraClaims
  -- return hasura claims value as an object. parse from string possibly
  (, expTimeM) <$> parseObjectFromString hasuraClaimsV

  where
    parseAuthzHeader = do
      let tokenParts = BLC.words authzHeader
      case tokenParts of
        ["Bearer", jwt] -> return jwt
        _               -> malformedAuthzHeader

    parseObjectFromString jVal =
      case (jcxClaimsFormat, jVal) of
        (JCFStringifiedJson, J.String v) ->
          either (const $ claimsErr $ strngfyErr v) return
          $ J.eitherDecodeStrict $ T.encodeUtf8 v
        (JCFStringifiedJson, _) ->
          claimsErr "expecting a string when claims_format is stringified_json"
        (JCFJson, J.Object o) -> return o
        (JCFJson, _) ->
          claimsErr "expecting a json object when claims_format is json"

    strngfyErr v =
      "expecting stringified json at: '"
      <> claimsLocation
      <> "', but found: " <> v
      where
        claimsLocation :: Text
        claimsLocation =
          case jcxClaimNs of
            ClaimNsPath path -> T.pack $ "claims_namespace_path " <> encodeJSONPath path
            ClaimNs ns       -> "claims_namespace " <> ns

    claimsErr = throw400 JWTInvalidClaims

    parseIValueJsonValue (J.IError _ _) = Nothing
    parseIValueJsonValue (J.ISuccess v) = Just v

    liftJWTError :: (MonadError e' m) => (e -> e') -> ExceptT e m a -> m a
    liftJWTError ef action = do
      res <- runExceptT action
      either (throwError . ef) return res

    invalidJWTError e =
      err400 JWTInvalid $ "Could not verify JWT: " <> T.pack (show e)

    malformedAuthzHeader =
      throw400 InvalidHeaders "Malformed Authorization header"
    claimsNotFound = do
      let claimsNsError = case jcxClaimNs of
                            ClaimNsPath path -> T.pack $ "claims not found at claims_namespace_path: '"
                                                <> encodeJSONPath path <> "'"
                            ClaimNs ns -> "claims key: '" <> ns <> "' not found"
      throw400 JWTInvalidClaims claimsNsError


-- parse x-hasura-allowed-roles, x-hasura-default-role from JWT claims
parseHasuraClaims :: forall m. (MonadError QErr m) => J.Object -> m HasuraClaims
parseHasuraClaims claimsMap = do
  HasuraClaims <$> 
    parseClaim allowedRolesClaim "should be a list of roles" <*> 
    parseClaim defaultRoleClaim  "should be a single role name"

  where
    parseClaim :: J.FromJSON a => Text -> Text -> m a
    parseClaim claim hint = do
      claimV <- maybe missingClaim return $ Map.lookup claim claimsMap
      parseJwtClaim claimV $ "invalid " <> claim <> "; " <> hint
      where
        missingClaim = throw400 JWTRoleClaimMissing $ "JWT claim does not contain " <> claim

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
  toJSON (JWTConfig keyOrUrl claimNs aud claimsFmt iss) =
    J.object (jwkFields ++ sharedFields ++ claimsNsFields)
    where
      jwkFields = case keyOrUrl of
        Left _    -> [ "type" J..= J.String "<TYPE REDACTED>"
                     , "key" J..= J.String "<JWK REDACTED>" ]
        Right url -> [ "jwk_url" J..= url ]

      claimsNsFields = case claimNs of
        ClaimNsPath nsPath ->
          ["claims_namespace_path" J..= encodeJSONPath nsPath]
        ClaimNs ns -> ["claims_namespace" J..= J.String ns]

      sharedFields = [ "claims_format" J..= claimsFmt
                     , "audience" J..= aud
                     , "issuer" J..= iss
                     ]

-- | Parse from a json string like:
-- | `{"type": "RS256", "key": "<PEM-encoded-public-key-or-X509-cert>"}`
-- | to JWTConfig
instance J.FromJSON JWTConfig where

  parseJSON = J.withObject "JWTConfig" $ \o -> do
    mRawKey <- o J..:? "key"
    claimsNs <- o J..:? "claims_namespace"
    claimsNsPath <- o J..:? "claims_namespace_path"
    aud     <- o J..:? "audience"
    iss     <- o J..:? "issuer"
    jwkUrl  <- o J..:? "jwk_url"
    isStrngfd <- o J..:? "claims_format"


    hasuraClaimsNs <-
      case (claimsNsPath,claimsNs) of
        (Nothing, Nothing) -> return $ ClaimNs defaultClaimNs
        (Just nsPath, Nothing) -> either failJSONPathParsing (return . ClaimNsPath) . JSONPath.parseJSONPath $ nsPath
        (Nothing, Just ns) -> return $ ClaimNs ns
        (Just _, Just _) -> fail "claims_namespace and claims_namespace_path both cannot be set"

    case (mRawKey, jwkUrl) of
      (Nothing, Nothing) -> fail "key and jwk_url both cannot be empty"
      (Just _, Just _)   -> fail "key, jwk_url both cannot be present"
      (Just rawKey, Nothing) -> do
        keyType <- o J..: "type"
        key <- parseKey keyType rawKey
        return $ JWTConfig (Left key) hasuraClaimsNs aud isStrngfd iss
      (Nothing, Just url) ->
        return $ JWTConfig (Right url) hasuraClaimsNs aud isStrngfd iss

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

      failJSONPathParsing err = fail $ "invalid JSON path claims_namespace_path error: " ++ err


-- Utility:
parseJwtClaim :: (J.FromJSON a, MonadError QErr m) => J.Value -> Text -> m a
parseJwtClaim v errMsg =
  case J.fromJSON v of
    J.Success val -> return val
    J.Error e     -> throw400 JWTInvalidClaims $ errMsg <> ": " <> T.pack e

