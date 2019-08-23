module Hasura.Server.Auth.JWT
  ( processJwt
  , RawJWT
  , JWTConfig (..)
  , JWTCtx (..)
  , Jose.JWKSet (..)
  , JWTClaimsFormat (..)
  , updateJwkRef
  , jwkRefreshCtrl
  , defaultClaimNs
  ) where

import           Control.Arrow                   (first)
import           Control.Exception               (try)
import           Control.Lens
import           Control.Monad                   (when)
import           Data.IORef                      (IORef, modifyIORef, readIORef)

import           Data.List                       (find)
import           Data.Time.Clock                 (NominalDiffTime, UTCTime,
                                                  diffUTCTime, getCurrentTime)
import           Data.Time.Format                (defaultTimeLocale, parseTimeM)
import           Network.URI                     (URI)

import           Hasura.HTTP
import           Hasura.Logging                  (LogLevel (..), Logger (..))
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.Server.Auth.JWT.Internal (parseHmacKey, parseRsaKey)
import           Hasura.Server.Auth.JWT.Logging
import           Hasura.Server.Utils             (diffTimeToMicro,
                                                  userRoleHeader)

import qualified Control.Concurrent              as C
import qualified Crypto.JWT                      as Jose
import qualified Data.Aeson                      as A
import qualified Data.Aeson.Casing               as A
import qualified Data.Aeson.TH                   as A
import qualified Data.ByteString.Lazy            as BL
import qualified Data.ByteString.Lazy.Char8      as BLC
import qualified Data.CaseInsensitive            as CI
import qualified Data.HashMap.Strict             as Map
import qualified Data.String.Conversions         as CS
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

$(A.deriveJSON A.defaultOptions { A.sumEncoding = A.ObjectWithSingleField
                                , A.constructorTagModifier = A.snakeCase . drop 3
                                } ''JWTClaimsFormat)

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
$(A.deriveJSON (A.aesonDrop 3 A.snakeCase) ''HasuraClaims)

allowedRolesClaim :: T.Text
allowedRolesClaim = "x-hasura-allowed-roles"

defaultRoleClaim :: T.Text
defaultRoleClaim = "x-hasura-default-role"

defaultClaimNs :: T.Text
defaultClaimNs = "https://hasura.io/jwt/claims"

-- | if the time is greater than 100 seconds, should refresh the JWK 10 seonds
-- before the expiry, else refresh at given seconds
computeDiffTime :: NominalDiffTime -> Int
computeDiffTime t =
  let intTime = diffTimeToMicro t
  in if intTime > 100 then intTime - 10 else intTime

-- | create a background thread to refresh the JWK
jwkRefreshCtrl
  :: (MonadIO m)
  => Logger
  -> HTTP.Manager
  -> URI
  -> IORef Jose.JWKSet
  -> NominalDiffTime
  -> m ()
jwkRefreshCtrl lggr mngr url ref time =
  void $ liftIO $ C.forkIO $ do
    C.threadDelay $ diffTimeToMicro time
    forever $ do
      res <- runExceptT $ updateJwkRef lggr mngr url ref
      mTime <- either (const $ return Nothing) return res
      -- if can't parse time from header, defaults to 1 min
      let delay = maybe (60 * aSecond) computeDiffTime mTime
      C.threadDelay delay
  where
    aSecond = 1000 * 1000


-- | Given a JWK url, fetch JWK from it and update the IORef
updateJwkRef
  :: ( MonadIO m
     , MonadError T.Text m)
  => Logger
  -> HTTP.Manager
  -> URI
  -> IORef Jose.JWKSet
  -> m (Maybe NominalDiffTime)
updateJwkRef (Logger logger) manager url jwkRef = do
  let options = wreqOptions manager []
      urlT    = T.pack $ show url
      infoMsg = JLNInfo $ "refreshing JWK from endpoint: " <> urlT
  liftIO $ logger $ JwkRefreshLog LevelInfo infoMsg Nothing
  res  <- liftIO $ try $ Wreq.getWith options $ show url
  resp <- either logAndThrowHttp return res
  let status = resp ^. Wreq.responseStatus
      respBody = resp ^. Wreq.responseBody

  when (status ^. Wreq.statusCode /= 200) $ do
    let respBodyT = Just $ CS.cs respBody
        errMsg = "Non-200 response on fetching JWK from: " <> urlT
        httpErr = Just (JwkRefreshHttpError (Just status) urlT Nothing respBodyT)
    logAndThrow errMsg httpErr

  let parseErr e = "Error parsing JWK from url (" <> urlT <> "): " <> T.pack e
  jwkset <- either (\e -> logAndThrow (parseErr e) Nothing) return $
    A.eitherDecode respBody
  liftIO $ modifyIORef jwkRef (const jwkset)

  let mExpiresT = resp ^? Wreq.responseHeader "Expires"
  forM mExpiresT $ \expiresT -> do
    let expiresE = parseTimeM True defaultTimeLocale timeFmt $ CS.cs expiresT
    expires  <- either (`logAndThrow` Nothing) return expiresE
    currTime <- liftIO getCurrentTime
    return $ diffUTCTime expires currTime

  where
    logAndThrow
      :: (MonadIO m, MonadError T.Text m)
      => T.Text -> Maybe JwkRefreshHttpError -> m a
    logAndThrow err httpErr = do
      liftIO $ logger $ JwkRefreshLog (LevelOther "critical") (JLNError err) httpErr
      throwError err

    logAndThrowHttp :: (MonadIO m, MonadError T.Text m) => HTTP.HttpException -> m a
    logAndThrowHttp err = do
      let httpErr = JwkRefreshHttpError Nothing (T.pack $ show url) (Just $ HttpException err) Nothing
          errMsg = "Error fetching JWK: " <> T.pack (getHttpExceptionMsg err)
      logAndThrow errMsg (Just httpErr)

    getHttpExceptionMsg = \case
      HTTP.HttpExceptionRequest _ reason -> show reason
      HTTP.InvalidUrlException _ reason -> show reason

    timeFmt = "%a, %d %b %Y %T GMT"


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
      return $ (, Nothing) $
        mkUserInfo unAuthRole $ mkUserVars $ hdrsToText headers

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
  let claimsMap = Map.filterWithKey (\k _ -> T.isPrefixOf "x-hasura-" k)
                $ Map.fromList $ map (first T.toLower)
                $ Map.toList hasuraClaims

  HasuraClaims allowedRoles defaultRole <- parseHasuraClaims claimsMap
  let role = getCurrentRole defaultRole

  when (role `notElem` allowedRoles) currRoleNotAllowed
  let finalClaims =
        Map.delete defaultRoleClaim . Map.delete allowedRolesClaim $ claimsMap

  -- transform the map of text:aeson-value -> text:text
  metadata <- decodeJSON $ A.Object finalClaims

  return $ (, expTimeM) $ mkUserInfo role $ mkUserVars $ Map.toList metadata

  where
    parseAuthzHeader = do
      let tokenParts = BLC.words authzHeader
      case tokenParts of
        ["Bearer", jwt] -> return jwt
        _               -> malformedAuthzHeader

    parseObjectFromString claimsFmt jVal =
      case (claimsFmt, jVal) of
        (JCFStringifiedJson, A.String v) ->
          either (const $ claimsErr $ strngfyErr v) return
          $ A.eitherDecodeStrict $ T.encodeUtf8 v
        (JCFStringifiedJson, _) ->
          claimsErr "expecting a string when claims_format is stringified_json"
        (JCFJson, A.Object o) -> return o
        (JCFJson, _) ->
          claimsErr "expecting a json object when claims_format is json"

    strngfyErr v = "expecting stringified json at: '"
                   <> fromMaybe defaultClaimNs (jcxClaimNs jwtCtx)
                   <> "', but found: " <> v

    claimsErr = throw400 JWTInvalidClaims

    -- see if there is a x-hasura-role header, or else pick the default role
    getCurrentRole defaultRole =
      let userRoleHeaderB = CS.cs userRoleHeader
          mUserRole = snd <$> find (\h -> fst h == CI.mk userRoleHeaderB) headers
      in maybe defaultRole RoleName $ mUserRole >>= mkNonEmptyText . bsToTxt

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


instance A.ToJSON JWTConfig where
  toJSON (JWTConfig ty keyOrUrl claimNs aud claimsFmt iss) =
    case keyOrUrl of
         Left _    -> mkObj ("key" A..= A.String "<JWK REDACTED>")
         Right url -> mkObj ("jwk_url" A..= url)
    where
      mkObj item = A.object [ "type" A..= ty
                            , "claims_namespace" A..= claimNs
                            , "claims_format" A..= claimsFmt
                            , "audience" A..= aud
                            , "issuer" A..= iss
                            , item
                            ]

-- | Parse from a json string like:
-- | `{"type": "RS256", "key": "<PEM-encoded-public-key-or-X509-cert>"}`
-- | to JWTConfig
instance A.FromJSON JWTConfig where

  parseJSON = A.withObject "JWTConfig" $ \o -> do
    keyType <- o A..: "type"
    mRawKey <- o A..:? "key"
    claimNs <- o A..:? "claims_namespace"
    aud     <- o A..:? "audience"
    iss     <- o A..:? "issuer"
    jwkUrl  <- o A..:? "jwk_url"
    isStrngfd <- o A..:? "claims_format"

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
