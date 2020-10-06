{-# LANGUAGE CPP #-}
module Hasura.Server.Auth.JWT
  ( processJwt
  , RawJWT
  , JWTConfig (..)
  , JWTCtx (..)
  , Jose.JWKSet (..)
  , JWTClaimsFormat (..)
  , JWTClaims(..)
  , JwkFetchError (..)
  , JWTNamespace (..)
  , JWTCustomClaimsMapDefaultRole
  , JWTCustomClaimsMapAllowedRoles
  , JWTCustomClaimsMapValue
  , ClaimsMap
  , updateJwkRef
  , jwkRefreshCtrl
  , defaultClaimsFormat
  , defaultClaimsNamespace

  -- * Exposed for testing
  , processJwt_
  , allowedRolesClaim
  , defaultRoleClaim
  , parseClaimsMap
  , JWTCustomClaimsMapValueG(..)
  , JWTCustomClaimsMap(..)
  ) where

import           Control.Exception.Lifted        (try)
import           Control.Lens
import           Control.Monad.Trans.Control     (MonadBaseControl)
import           Control.Monad.Trans.Maybe
import           Data.IORef                      (IORef, readIORef, writeIORef)
import           Data.Parser.JSONPath            (parseJSONPath)
import           Data.Time.Clock                 (NominalDiffTime, UTCTime, diffUTCTime,
                                                  getCurrentTime)
#ifndef PROFILING
import           GHC.AssertNF
#endif
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
import           Hasura.Server.Utils             (executeJSONPath, getRequestHeader, userRoleHeader, isSessionVariable)
import           Hasura.Server.Version           (HasVersion)
import           Hasura.Session
import qualified Hasura.Tracing                  as Tracing

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

defaultClaimsFormat :: JWTClaimsFormat
defaultClaimsFormat = JCFJson

allowedRolesClaim :: SessionVariable
allowedRolesClaim = mkSessionVariable "x-hasura-allowed-roles"

defaultRoleClaim :: SessionVariable
defaultRoleClaim = mkSessionVariable "x-hasura-default-role"

defaultClaimsNamespace :: Text
defaultClaimsNamespace = "https://hasura.io/jwt/claims"

-- | 'JWTCustomClaimsMapValueG' is used to represent a single value of
-- the 'JWTCustomClaimsMap'. A 'JWTCustomClaimsMapValueG' can either be
-- an JSON object or the literal value of the claim. If the value is an
-- JSON object, then it should contain a key `path`, which is the JSON path
-- to the claim value in the JWT token. There's also an option to specify a
-- default value in the map via the 'default' key, which will be used
-- when a peek at the JWT token using the JSON path fails (key does not exist).
data JWTCustomClaimsMapValueG v
  = JWTCustomClaimsMapJSONPath !J.JSONPath !(Maybe v)
  -- ^ JSONPath to the key in the claims map, in case
  -- the key doesn't exist in the claims map then the default
  -- value will be used (if provided)
  | JWTCustomClaimsMapStatic !v
  deriving (Show, Eq, Functor, Foldable, Traversable)

instance (J.FromJSON v) => J.FromJSON (JWTCustomClaimsMapValueG v) where
  parseJSON (J.Object obj) = do
    path <- obj J..: "path" >>= (either fail pure . parseJSONPath)
    defaultVal <- obj J..:? "default" >>= traverse pure
    pure $ JWTCustomClaimsMapJSONPath path defaultVal
  parseJSON v = JWTCustomClaimsMapStatic <$> J.parseJSON v

instance (J.ToJSON v) => J.ToJSON (JWTCustomClaimsMapValueG v) where
  toJSON (JWTCustomClaimsMapJSONPath jsonPath mDefVal) =
    J.object $
       [ "path"    J..= encodeJSONPath jsonPath ]
    <> [ "default" J..= defVal | Just defVal <- [mDefVal]]
  toJSON (JWTCustomClaimsMapStatic v)          = J.toJSON v

type JWTCustomClaimsMapDefaultRole = JWTCustomClaimsMapValueG RoleName
type JWTCustomClaimsMapAllowedRoles = JWTCustomClaimsMapValueG [RoleName]

-- Used to store other session variables like `x-hasura-user-id`
type JWTCustomClaimsMapValue = JWTCustomClaimsMapValueG SessionVariableValue

type CustomClaimsMap = Map.HashMap SessionVariable JWTCustomClaimsMapValue

-- | JWTClaimsMap is an option to provide a custom JWT claims map.
-- The JWTClaimsMap should be specified in the `HASURA_GRAPHQL_JWT_SECRET`
-- in the `claims_map`. The JWTClaimsMap, if specified, requires two
-- mandatory fields, namely, `x-hasura-allowed-roles` and the
-- `x-hasura-default-role`, other claims may also be provided in the claims map.
data JWTCustomClaimsMap
  = JWTCustomClaimsMap
  { jcmDefaultRole  :: !JWTCustomClaimsMapDefaultRole
  , jcmAllowedRoles :: !JWTCustomClaimsMapAllowedRoles
  , jcmCustomClaims :: !CustomClaimsMap
  } deriving (Show,Eq)

instance J.ToJSON JWTCustomClaimsMap where
  toJSON (JWTCustomClaimsMap defaultRole allowedRoles customClaims) =
    J.Object $
    Map.fromList $ [ (sessionVariableToText defaultRoleClaim, J.toJSON defaultRole)
                   , (sessionVariableToText allowedRolesClaim, J.toJSON allowedRoles)
                   ]
    <> map (sessionVariableToText *** J.toJSON) (Map.toList customClaims)

instance J.FromJSON JWTCustomClaimsMap where
  parseJSON = J.withObject "JWTClaimsMap" $ \obj -> do
    let withNotFoundError sessionVariable =
          let errorMsg = T.unpack $
                sessionVariableToText sessionVariable <> " is expected but not found"
          in maybe (fail errorMsg) pure $ Map.lookup (sessionVariableToText sessionVariable) obj

    allowedRoles <- withNotFoundError allowedRolesClaim  >>= J.parseJSON
    defaultRole <- withNotFoundError defaultRoleClaim  >>= J.parseJSON
    let filteredClaims = Map.delete allowedRolesClaim $ Map.delete defaultRoleClaim
                          $ Map.fromList $ map (first mkSessionVariable) $ Map.toList obj
    customClaims <- flip Map.traverseWithKey filteredClaims $ const $ J.parseJSON
    pure $ JWTCustomClaimsMap defaultRole allowedRoles customClaims

-- | JWTNamespace is used to locate the claims map within the JWT token.
-- The location can be either provided via a JSON path or the name of the
-- key in the JWT token.
data JWTNamespace
  = ClaimNsPath JSONPath
  | ClaimNs T.Text
  deriving (Show, Eq)

instance J.ToJSON JWTNamespace where
  toJSON (ClaimNsPath nsPath) = J.String . T.pack $ encodeJSONPath nsPath
  toJSON (ClaimNs ns)         = J.String ns

data JWTClaims
  = JCNamespace !JWTNamespace !JWTClaimsFormat
  | JCMap !JWTCustomClaimsMap
  deriving (Show, Eq)

-- | The JWT configuration we got from the user.
data JWTConfig
  = JWTConfig
  { jcKeyOrUrl :: !(Either Jose.JWK URI)
  , jcAudience :: !(Maybe Jose.Audience)
  , jcIssuer   :: !(Maybe Jose.StringOrURI)
  , jcClaims   :: !JWTClaims
  } deriving (Show, Eq)

-- | The validated runtime JWT configuration returned by 'mkJwtCtx' in 'setupAuthMode'.
--
-- This is also evidence that the 'jwkRefreshCtrl' thread is running, if an
-- expiration schedule could be determined.
data JWTCtx
  = JWTCtx
  { jcxKey      :: !(IORef Jose.JWKSet)
    -- ^ This needs to be a mutable variable for 'updateJwkRef'.
  , jcxAudience :: !(Maybe Jose.Audience)
  , jcxIssuer   :: !(Maybe Jose.StringOrURI)
  , jcxClaims   :: !JWTClaims
  } deriving (Eq)

instance Show JWTCtx where
  show (JWTCtx _ audM iss claims) =
    show ["<IORef JWKSet>", show audM, show iss, show claims]

data HasuraClaims
  = HasuraClaims
  { _cmAllowedRoles :: ![RoleName]
  , _cmDefaultRole  :: !RoleName
  } deriving (Show, Eq)
$(J.deriveJSON (J.aesonDrop 3 J.snakeCase) ''HasuraClaims)

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
      let delay = maybe (minutes 1) convertDuration mTime
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
  res <- try $ do
    initReq <- liftIO $ HTTP.parseRequest $ show url
    let req = initReq { HTTP.requestHeaders = addDefaultHeaders (HTTP.requestHeaders initReq) }
    Tracing.tracedHttpRequest req \req' -> do
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
#ifndef PROFILING
    $assertNFHere jwkset  -- so we don't write thunks to mutable vars
#endif
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

type ClaimsMap = Map.HashMap SessionVariable J.Value

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
  => (_JWTCtx -> BLC.ByteString -> m (ClaimsMap, Maybe UTCTime))
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
      (claimsMap, expTimeM) <- processAuthZHeader_ jwtCtx $ BL.fromStrict authzHeader

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
      let finalClaimsObject = Map.fromList . map (first sessionVariableToText) . Map.toList $ finalClaims
      metadata <- parseJwtClaim (J.Object $ finalClaimsObject) "x-hasura-* claims"
      userInfo <- mkUserInfo (URBPreDetermined requestedRole) UAdminSecretNotSent $
                  mkSessionVariablesText $ Map.toList metadata
      pure (userInfo, expTimeM)

    withoutAuthZHeader = do
      unAuthRole <- maybe missingAuthzHeader return mUnAuthRole
      userInfo <- mkUserInfo (URBPreDetermined unAuthRole) UAdminSecretNotSent $
        mkSessionVariablesHeaders headers
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
  -> m (ClaimsMap, Maybe UTCTime)
processAuthZHeader jwtCtx authzHeader = do
  -- try to parse JWT token from Authorization header
  jwt <- parseAuthzHeader

  -- verify the JWT
  claims <- liftJWTError invalidJWTError $ verifyJwt jwtCtx $ RawJWT jwt

  let expTimeM = fmap (\(Jose.NumericDate t) -> t) $ claims ^. Jose.claimExp
      unregisteredClaims = claims ^. Jose.unregisteredClaims

  claimsObject <- parseClaimsMap unregisteredClaims claimsConfig

  pure $ (claimsObject, expTimeM)

  where
    claimsConfig = jcxClaims jwtCtx
    parseAuthzHeader = do
      let tokenParts = BLC.words authzHeader
      case tokenParts of
        ["Bearer", jwt] -> return jwt
        _               -> malformedAuthzHeader

    liftJWTError :: (MonadError e' m) => (e -> e') -> ExceptT e m a -> m a
    liftJWTError ef action = do
      res <- runExceptT action
      either (throwError . ef) return res

    invalidJWTError e =
      err400 JWTInvalid $ "Could not verify JWT: " <> T.pack (show e)

    malformedAuthzHeader =
      throw400 InvalidHeaders "Malformed Authorization header"

-- | parse the claims map from the JWT token or custom claims from the JWT config
parseClaimsMap
  :: (MonadError QErr m)
  => J.Object -- ^ Unregistered JWT claims
  -> JWTClaims -- ^ Claims config
  -> m ClaimsMap -- ^ Hasura claims and other claims
parseClaimsMap unregisteredClaims jcxClaims =
  case jcxClaims of
    JCNamespace namespace claimsFormat -> do
      claimsV <- maybe (claimsNotFound namespace) pure $ case namespace of
            ClaimNs k -> Map.lookup k unregisteredClaims
            ClaimNsPath path -> iResultToMaybe $ executeJSONPath path (J.toJSON unregisteredClaims)
      -- get hasura claims value as an object. parse from string possibly
      claimsObject <- parseObjectFromString namespace claimsFormat claimsV

      -- filter only x-hasura claims
      let claimsMap = Map.fromList
                      $ map (first mkSessionVariable)
                      $ filter (\(k, _) -> isSessionVariable k)
                      $ Map.toList claimsObject

      pure claimsMap

    JCMap claimsConfig -> do
      let JWTCustomClaimsMap defaultRoleClaimsMap allowedRolesClaimsMap otherClaimsMap = claimsConfig
          claimsObjValue = J.Object unregisteredClaims

      allowedRoles <- case allowedRolesClaimsMap of
        JWTCustomClaimsMapJSONPath allowedRolesJsonPath defaultVal ->
          parseAllowedRolesClaim defaultVal $ iResultToMaybe $ executeJSONPath allowedRolesJsonPath claimsObjValue
        JWTCustomClaimsMapStatic staticAllowedRoles -> pure staticAllowedRoles

      defaultRole <- case defaultRoleClaimsMap of
        JWTCustomClaimsMapJSONPath defaultRoleJsonPath defaultVal ->
          parseDefaultRoleClaim defaultVal $ iResultToMaybe $
          executeJSONPath defaultRoleJsonPath claimsObjValue
        JWTCustomClaimsMapStatic staticDefaultRole -> pure staticDefaultRole

      otherClaims <- flip Map.traverseWithKey otherClaimsMap $ \k claimObj -> do
        let throwClaimErr = throw400 JWTInvalidClaims $ "JWT claim from claims_map, "
                            <> sessionVariableToText k <> " not found"
        case claimObj of
          JWTCustomClaimsMapJSONPath path defaultVal ->
            maybe (onNothing (J.String <$> defaultVal) throwClaimErr) pure
              $ iResultToMaybe $ executeJSONPath path claimsObjValue
          JWTCustomClaimsMapStatic claimStaticValue -> pure $ J.String claimStaticValue

      pure $  Map.fromList [
            (allowedRolesClaim, J.toJSON allowedRoles),
            (defaultRoleClaim, J.toJSON defaultRole)
            ] <> otherClaims

  where
    parseAllowedRolesClaim defaultVal = \case
      Nothing ->
        onNothing defaultVal $
        throw400 JWTRoleClaimMissing $ "JWT claim does not contain " <> sessionVariableToText allowedRolesClaim
      Just v -> parseJwtClaim v $ "invalid " <> sessionVariableToText allowedRolesClaim <>
                "; should be a list of roles"

    parseDefaultRoleClaim defaultVal = \case
      Nothing ->
        onNothing defaultVal $
        throw400 JWTRoleClaimMissing $ "JWT claim does not contain " <> sessionVariableToText defaultRoleClaim
      Just v -> parseJwtClaim v $ "invalid " <> sessionVariableToText defaultRoleClaim <>
                "; should be a role"

    claimsNotFound namespace =
      throw400 JWTInvalidClaims $ case namespace of
      ClaimNsPath path -> T.pack $ "claims not found at claims_namespace_path: '"
                          <> encodeJSONPath path <> "'"
      ClaimNs ns -> "claims key: '" <> ns <> "' not found"

    parseObjectFromString namespace claimsFmt jVal =
      case (claimsFmt, jVal) of
        (JCFStringifiedJson, J.String v) ->
          either (const $ claimsErr $ strngfyErr v) return
          $ J.eitherDecodeStrict $ T.encodeUtf8 v
        (JCFStringifiedJson, _) ->
          claimsErr "expecting a string when claims_format is stringified_json"
        (JCFJson, J.Object o) -> return o
        (JCFJson, _) ->
          claimsErr "expecting a json object when claims_format is json"
      where
        strngfyErr v =
          let claimsLocation = case namespace of
                ClaimNsPath path -> T.pack $ "claims_namespace_path " <> encodeJSONPath path
                ClaimNs ns       -> "claims_namespace " <> ns
          in "expecting stringified json at: '"
             <> claimsLocation
             <> "', but found: " <> v

        claimsErr = throw400 JWTInvalidClaims

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
  toJSON (JWTConfig keyOrUrl aud iss claims) =
    let keyOrUrlPairs = case keyOrUrl of
          Left _    -> [ "type" J..= J.String "<TYPE REDACTED>"
                       , "key" J..= J.String "<JWK REDACTED>"
                       ]
          Right url -> ["jwk_url" J..= url]
        claimsPairs = case claims of
          JCNamespace namespace claimsFormat ->
            let namespacePairs = case namespace of
                  ClaimNsPath nsPath ->
                    ["claims_namespace_path" J..= encodeJSONPath nsPath]
                  ClaimNs ns -> ["claims_namespace" J..= J.String ns]
            in namespacePairs <> ["claims_format" J..= claimsFormat]
          JCMap claimsMap -> ["claims_map" J..= claimsMap]
    in J.object $ keyOrUrlPairs <>
                  [ "audience" J..= aud
                  , "issuer" J..= iss
                  ] <> claimsPairs

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
    claimsFormat <- o J..:? "claims_format" J..!= defaultClaimsFormat
    claimsMap <- o J..:? "claims_map"

    hasuraClaimsNs <-
      case (claimsNsPath,claimsNs) of
        (Nothing, Nothing) -> pure $ ClaimNs defaultClaimsNamespace
        (Just nsPath, Nothing) -> either failJSONPathParsing (return . ClaimNsPath) . parseJSONPath $ nsPath
        (Nothing, Just ns) -> return $ ClaimNs ns
        (Just _, Just _) -> fail "claims_namespace and claims_namespace_path both cannot be set"

    keyOrUrl <- case (mRawKey, jwkUrl) of
      (Nothing, Nothing) -> fail "key and jwk_url both cannot be empty"
      (Just _, Just _)   -> fail "key, jwk_url both cannot be present"
      (Just rawKey, Nothing) -> do
        keyType <- o J..: "type"
        key <- parseKey keyType rawKey
        pure $ Left key
      (Nothing, Just url) -> pure $ Right url

    pure $ JWTConfig keyOrUrl aud iss $
           maybe (JCNamespace hasuraClaimsNs claimsFormat) JCMap claimsMap

    where
      parseKey keyType rawKey =
       case keyType of
          "HS256" -> runEither $ parseHmacKey rawKey 256
          "HS384" -> runEither $ parseHmacKey rawKey 384
          "HS512" -> runEither $ parseHmacKey rawKey 512
          "RS256" -> runEither $ parseRsaKey rawKey
          "RS384" -> runEither $ parseRsaKey rawKey
          "RS512" -> runEither $ parseRsaKey rawKey
          -- TODO(from master): support ES256, ES384, ES512, PS256, PS384
          _       -> invalidJwk ("Key type: " <> T.unpack keyType <> " is not supported")

      runEither = either (invalidJwk . T.unpack) return

      invalidJwk msg = fail ("Invalid JWK: " <> msg)

      failJSONPathParsing err = fail $ "invalid JSON path claims_namespace_path error: " ++ err

-- parse x-hasura-allowed-roles, x-hasura-default-role from JWT claims
parseHasuraClaims :: forall m. (MonadError QErr m) => ClaimsMap -> m HasuraClaims
parseHasuraClaims claimsMap = do
  HasuraClaims <$>
    parseClaim allowedRolesClaim "should be a list of roles" <*>
    parseClaim defaultRoleClaim  "should be a single role name"

  where
    parseClaim :: J.FromJSON a => SessionVariable -> Text -> m a
    parseClaim claim hint = do
      claimV <- maybe missingClaim return $ Map.lookup claim claimsMap
      parseJwtClaim claimV $ "invalid " <> claimText <> "; " <> hint
      where
        missingClaim = throw400 JWTRoleClaimMissing $ "JWT claim does not contain " <> claimText
        claimText = sessionVariableToText claim

-- Utility:
parseJwtClaim :: (J.FromJSON a, MonadError QErr m) => J.Value -> Text -> m a
parseJwtClaim v errMsg =
  case J.fromJSON v of
    J.Success val -> return val
    J.Error e     -> throw400 JWTInvalidClaims $ errMsg <> ": " <> T.pack e
