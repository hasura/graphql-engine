{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Hasura.Server.Auth.JWT
-- Description : Implements JWT Configuration and Validation Logic.
-- Copyright   : Hasura
--
-- This module implements the bulk of Hasura's JWT capabilities and interactions.
-- Its main point of non-testing invocation is `Hasura.Server.Auth`.
--
-- It exports both `processJwt` and `processJwt_` with `processJwt_` being the
-- majority of the implementation with the JWT Token processing function
-- passed in as an argument in order to enable mocking in test-code.
--
-- In `processJwt_`, prior to validation of the token, first the token locations
-- and issuers are reconciled. Locations are either specified as auth or
-- cookie (with cookie name) or assumed to be auth. Issuers can be omitted or
-- specified, where an omitted configured issuer can match any issuer specified by
-- a request.
--
-- If none match, then this is considered an no-auth request, if one matches,
-- then normal token auth is performed, and if multiple match, then this is
-- considered an ambiguity error.
module Hasura.Server.Auth.JWT
  ( processJwt,
    RawJWT,
    StringOrURI (..),
    JWTConfig (..),
    JWTCtx (..),
    Jose.JWKSet (..),
    JWTClaimsFormat (..),
    JWTClaims (..),
    JwkFetchError (..),
    JWTHeader (..),
    JWTNamespace (..),
    JWTCustomClaimsMapDefaultRole,
    JWTCustomClaimsMapAllowedRoles,
    JWTCustomClaimsMapValue,
    ClaimsMap,
    fetchAndUpdateJWKs,
    fetchJwk,
    defaultClaimsFormat,
    defaultClaimsNamespace,

    -- * Exposed for testing
    processJwt_,
    tokenIssuer,
    allowedRolesClaim,
    defaultRoleClaim,
    parseClaimsMap,
    JWTCustomClaimsMapValueG (..),
    JWTCustomClaimsMap (..),
    determineJwkExpiryLifetime,
  )
where

import Control.Exception.Lifted (try)
import Control.Lens
import Control.Monad.Trans.Control (MonadBaseControl)
import Crypto.JWT qualified as Jose
import Data.Aeson (JSONPath)
import Data.Aeson qualified as J
import Data.Aeson.Casing qualified as J
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.ByteArray.Encoding qualified as BAE
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Internal qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BLC
import Data.CaseInsensitive qualified as CI
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable
import Data.IORef (IORef, modifyIORef, readIORef, writeIORef)
import Data.Map.Strict qualified as M
import Data.Parser.CacheControl
import Data.Parser.Expires
import Data.Parser.JSONPath (encodeJSONPath, parseJSONPath)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Time.Clock
  ( NominalDiffTime,
    UTCTime,
    addUTCTime,
    getCurrentTime,
  )
import GHC.AssertNF.CPP
import Hasura.Base.Error
import Hasura.HTTP
import Hasura.Logging (Hasura, LogLevel (..), Logger (..))
import Hasura.Prelude
import Hasura.RQL.Types.Roles (RoleName, mkRoleName)
import Hasura.Server.Auth.JWT.Internal (parseEdDSAKey, parseEsKey, parseHmacKey, parseRsaKey)
import Hasura.Server.Auth.JWT.Logging
import Hasura.Server.Utils
  ( executeJSONPath,
    getRequestHeader,
    isSessionVariable,
    userRoleHeader,
  )
import Hasura.Session (SessionVariable, SessionVariableValue, UserAdminSecret (..), UserInfo, UserRoleBuild (..), mkSessionVariable, mkSessionVariablesHeaders, mkSessionVariablesText, mkUserInfo, sessionVariableToText)
import Network.HTTP.Client.Transformable qualified as HTTP
import Network.HTTP.Types as N
import Network.URI (URI)
import Network.Wreq qualified as Wreq
import Web.Spock.Internal.Cookies qualified as Spock

newtype RawJWT = RawJWT BL.ByteString

data JWTClaimsFormat
  = JCFJson
  | JCFStringifiedJson
  deriving (Show, Eq, Generic)

instance J.FromJSON JWTClaimsFormat where
  parseJSON =
    J.genericParseJSON
      J.defaultOptions
        { J.sumEncoding = J.ObjectWithSingleField,
          J.constructorTagModifier = J.snakeCase . drop 3
        }

instance J.ToJSON JWTClaimsFormat where
  toJSON =
    J.genericToJSON
      J.defaultOptions
        { J.sumEncoding = J.ObjectWithSingleField,
          J.constructorTagModifier = J.snakeCase . drop 3
        }

  toEncoding =
    J.genericToEncoding
      J.defaultOptions
        { J.sumEncoding = J.ObjectWithSingleField,
          J.constructorTagModifier = J.snakeCase . drop 3
        }

data JWTHeader
  = JHAuthorization
  | JHCookie Text -- cookie name
  | JHCustomHeader Text -- header name
  deriving (Show, Eq, Generic)

instance Hashable JWTHeader

instance J.FromJSON JWTHeader where
  parseJSON = J.withObject "JWTHeader" $ \o -> do
    hdrType <- o J..: "type" <&> CI.mk @Text
    if
      | hdrType == "Authorization" -> pure JHAuthorization
      | hdrType == "Cookie" -> JHCookie <$> o J..: "name"
      | hdrType == "CustomHeader" -> JHCustomHeader <$> o J..: "name"
      | otherwise -> fail "expected 'type' is 'Authorization' or 'Cookie' or 'CustomHeader'"

instance J.ToJSON JWTHeader where
  toJSON JHAuthorization = J.object ["type" J..= ("Authorization" :: String)]
  toJSON (JHCookie name) =
    J.object
      [ "type" J..= ("Cookie" :: String),
        "name" J..= name
      ]
  toJSON (JHCustomHeader name) =
    J.object
      [ "type" J..= ("CustomHeader" :: String),
        "name" J..= name
      ]

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
  = -- | JSONPath to the key in the claims map, in case
    -- the key doesn't exist in the claims map then the default
    -- value will be used (if provided)
    JWTCustomClaimsMapJSONPath !J.JSONPath !(Maybe v)
  | JWTCustomClaimsMapStatic !v
  deriving (Show, Eq, Functor, Foldable, Traversable)

instance (J.FromJSON v) => J.FromJSON (JWTCustomClaimsMapValueG v) where
  parseJSON (J.Object obj) = do
    path <- obj J..: "path" >>= (either (fail . T.unpack) pure . parseJSONPath)
    defaultVal <- obj J..:? "default"
    pure $ JWTCustomClaimsMapJSONPath path defaultVal
  parseJSON v = JWTCustomClaimsMapStatic <$> J.parseJSON v

instance (J.ToJSON v) => J.ToJSON (JWTCustomClaimsMapValueG v) where
  toJSON (JWTCustomClaimsMapJSONPath jsonPath mDefVal) =
    J.object
      $ ["path" J..= encodeJSONPath jsonPath]
      <> ["default" J..= defVal | Just defVal <- [mDefVal]]
  toJSON (JWTCustomClaimsMapStatic v) = J.toJSON v

type JWTCustomClaimsMapDefaultRole = JWTCustomClaimsMapValueG RoleName

type JWTCustomClaimsMapAllowedRoles = JWTCustomClaimsMapValueG [RoleName]

-- Used to store other session variables like `x-hasura-user-id`
type JWTCustomClaimsMapValue = JWTCustomClaimsMapValueG SessionVariableValue

type CustomClaimsMap = HashMap.HashMap SessionVariable JWTCustomClaimsMapValue

-- | JWTClaimsMap is an option to provide a custom JWT claims map.
-- The JWTClaimsMap should be specified in the `HASURA_GRAPHQL_JWT_SECRET`
-- in the `claims_map`. The JWTClaimsMap, if specified, requires two
-- mandatory fields, namely, `x-hasura-allowed-roles` and the
-- `x-hasura-default-role`, other claims may also be provided in the claims map.
data JWTCustomClaimsMap = JWTCustomClaimsMap
  { jcmDefaultRole :: !JWTCustomClaimsMapDefaultRole,
    jcmAllowedRoles :: !JWTCustomClaimsMapAllowedRoles,
    jcmCustomClaims :: !CustomClaimsMap
  }
  deriving (Show, Eq)

instance J.ToJSON JWTCustomClaimsMap where
  toJSON (JWTCustomClaimsMap defaultRole allowedRoles customClaims) =
    J.Object
      $ KM.fromList
      $ map (first (K.fromText . sessionVariableToText))
      $ [ (defaultRoleClaim, J.toJSON defaultRole),
          (allowedRolesClaim, J.toJSON allowedRoles)
        ]
      <> map (second J.toJSON) (HashMap.toList customClaims)

instance J.FromJSON JWTCustomClaimsMap where
  parseJSON = J.withObject "JWTClaimsMap" $ \obj -> do
    let withNotFoundError sessionVariable =
          let sessionVarText = sessionVariableToText sessionVariable
              errorMsg =
                T.unpack
                  $ sessionVarText
                  <> " is expected but not found"
           in KM.lookup (K.fromText sessionVarText) obj
                `onNothing` fail errorMsg

    allowedRoles <- withNotFoundError allowedRolesClaim >>= J.parseJSON
    defaultRole <- withNotFoundError defaultRoleClaim >>= J.parseJSON
    let filteredClaims =
          HashMap.delete allowedRolesClaim
            $ HashMap.delete defaultRoleClaim
            $ HashMap.fromList
            $ map (first (mkSessionVariable . K.toText))
            $ KM.toList obj
    customClaims <- flip HashMap.traverseWithKey filteredClaims $ const $ J.parseJSON
    pure $ JWTCustomClaimsMap defaultRole allowedRoles customClaims

-- | JWTNamespace is used to locate the claims map within the JWT token.
-- The location can be either provided via a JSON path or the name of the
-- key in the JWT token.
data JWTNamespace
  = ClaimNsPath JSONPath
  | ClaimNs Text
  deriving (Show, Eq)

instance J.ToJSON JWTNamespace where
  toJSON (ClaimNsPath nsPath) = J.String $ encodeJSONPath nsPath
  toJSON (ClaimNs ns) = J.String ns

data JWTClaims
  = JCNamespace !JWTNamespace !JWTClaimsFormat
  | JCMap !JWTCustomClaimsMap
  deriving (Show, Eq)

-- | Hashable Wrapper for constructing a HashMap of JWTConfigs
newtype StringOrURI = StringOrURI {unStringOrURI :: Jose.StringOrURI}
  deriving newtype (Show, Eq, J.ToJSON, J.FromJSON)

instance J.ToJSONKey StringOrURI

instance J.FromJSONKey StringOrURI

instance J.ToJSONKey (Maybe StringOrURI)

instance J.FromJSONKey (Maybe StringOrURI)

instance Hashable StringOrURI where
  hashWithSalt i = hashWithSalt i . J.encode

-- | The JWT configuration we got from the user.
data JWTConfig = JWTConfig
  { jcKeyOrUrl :: !(Either Jose.JWK URI),
    jcAudience :: !(Maybe Jose.Audience),
    jcIssuer :: !(Maybe Jose.StringOrURI),
    jcClaims :: !JWTClaims,
    jcAllowedSkew :: !(Maybe NominalDiffTime),
    jcHeader :: !(Maybe JWTHeader)
  }
  deriving (Show, Eq)

-- | The validated runtime JWT configuration returned by 'mkJwtCtx' in 'setupAuthMode'.
data JWTCtx = JWTCtx
  { jcxUrl :: !(Maybe URI),
    -- | This needs to be a mutable variable for 'fetchJwk'.
    -- | We add the expiry time of the JWK to the IORef, to determine
    -- | if the JWK has expired and needs to be refreshed.
    jcxKeyConfig :: !(IORef (Jose.JWKSet, Maybe UTCTime)),
    jcxAudience :: !(Maybe Jose.Audience),
    jcxIssuer :: !(Maybe Jose.StringOrURI),
    jcxClaims :: !JWTClaims,
    jcxAllowedSkew :: !(Maybe NominalDiffTime),
    jcxHeader :: !JWTHeader
  }
  deriving (Eq)

instance Show JWTCtx where
  show (JWTCtx url _ audM iss claims allowedSkew headers) =
    show [show url, "<IORef JWKSet, Expiry>", show audM, show iss, show claims, show allowedSkew, show headers]

data HasuraClaims = HasuraClaims
  { _cmAllowedRoles :: ![RoleName],
    _cmDefaultRole :: !RoleName
  }
  deriving (Show, Eq, Generic)

instance J.FromJSON HasuraClaims where
  parseJSON = J.genericParseJSON hasuraJSON

instance J.ToJSON HasuraClaims where
  toJSON = J.genericToJSON hasuraJSON

-- | An action that fetches the JWKs and updates the expiry time and JWKs in the
-- IORef
fetchAndUpdateJWKs ::
  (MonadIO m, MonadBaseControl IO m) =>
  Logger Hasura ->
  HTTP.Manager ->
  URI ->
  IORef (Jose.JWKSet, Maybe UTCTime) ->
  m ()
fetchAndUpdateJWKs logger httpManager url jwkRef = do
  res <- runExceptT $ fetchJwk logger httpManager url
  case res of
    -- As this 'fetchJwk' is going to happen always in background thread, we are
    -- not going to throw fatal error(s). If there is any error fetching JWK -
    -- don't do anything; this will get retried again in 1 second
    -- TODO: we need to do a 'fetchJwk' check in 'setupAuthMode' and throw any
    -- fatal error(s) there
    Left _e -> pure ()
    Right (jwkSet, responseHeaders) -> do
      expiryRes <-
        runExceptT
          $ determineJwkExpiryLifetime (liftIO getCurrentTime) logger responseHeaders
      maybeExpiry <- onLeft expiryRes (const $ pure Nothing)
      case maybeExpiry of
        Nothing -> liftIO $ do
          $assertNFHere jwkSet -- so we don't write thunks to mutable vars
          -- If there is an error in parsing the expiry time, then we
          -- keep the previous expiry time in the jwkRef,so that we can fetch the JWK
          -- in the next iteration
          modifyIORef jwkRef (\(_, previousExpiry) -> (jwkSet, previousExpiry))
          logNotice
        Just expiryTime -> liftIO $ writeIORef jwkRef (jwkSet, Just expiryTime)
  where
    logNotice = do
      let err = JwkRefreshLog LevelInfo (Just "Either the expiry is not present or cannot be parsed (retrying again after 1 second)") Nothing
      liftIO $ unLogger logger err

-- | Given a JWK url, fetch JWK from it
fetchJwk ::
  ( MonadIO m,
    MonadBaseControl IO m,
    MonadError JwkFetchError m
  ) =>
  Logger Hasura ->
  HTTP.Manager ->
  URI ->
  m (Jose.JWKSet, ResponseHeaders)
fetchJwk (Logger logger) manager url = do
  let urlT = tshow url
      infoMsg = "refreshing JWK from endpoint: " <> urlT
  liftIO $ logger $ JwkRefreshLog LevelInfo (Just infoMsg) Nothing
  res <- try $ do
    req <- liftIO $ HTTP.mkRequestThrow $ tshow url
    let req' = req & over HTTP.headers addDefaultHeaders
    liftIO $ HTTP.httpLbs req' manager
  resp <- onLeft res logAndThrowHttp
  let status = resp ^. Wreq.responseStatus
      respBody = resp ^. Wreq.responseBody
      statusCode = status ^. Wreq.statusCode

  unless (statusCode >= 200 && statusCode < 300) $ do
    let errMsg = "Non-2xx response on fetching JWK from: " <> urlT
        err = JFEHttpError url status respBody errMsg
    logAndThrow err

  let parseErr e = JFEJwkParseError (T.pack e) $ "Error parsing JWK from url: " <> urlT
  !jwkset <- onLeft (J.eitherDecode' respBody) (logAndThrow . parseErr)
  return (jwkset, resp ^. Wreq.responseHeaders)
  where
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

-- | First check for Cache-Control header, if not found, look for Expires header
determineJwkExpiryLifetime ::
  forall m.
  (MonadIO m, MonadError JwkFetchError m) =>
  m UTCTime ->
  Logger Hasura ->
  ResponseHeaders ->
  m (Maybe UTCTime)
determineJwkExpiryLifetime getCurrentTime' (Logger logger) responseHeaders =
  runMaybeT $ timeFromCacheControl <|> timeFromExpires
  where
    parseCacheControlErr :: Text -> JwkFetchError
    parseCacheControlErr e =
      JFEExpiryParseError
        (Just e)
        "Failed parsing Cache-Control header from JWK response"

    parseTimeErr :: JwkFetchError
    parseTimeErr =
      JFEExpiryParseError
        Nothing
        "Failed parsing Expires header from JWK response. Value of header is not a valid timestamp"

    timeFromCacheControl :: MaybeT m UTCTime
    timeFromCacheControl = do
      header <- afold $ bsToTxt <$> lookup "Cache-Control" responseHeaders
      cacheControl <- parseCacheControl header `onLeft` \err -> logAndThrowInfo $ parseCacheControlErr $ T.pack err
      maxAgeMaybe <- fmap fromInteger <$> findMaxAge cacheControl `onLeft` \err -> logAndThrowInfo $ parseCacheControlErr $ T.pack err
      currTime <- lift getCurrentTime'
      maxExpiryMaybe <- case maxAgeMaybe of
        Just maxAge -> return $ Just $ addUTCTime maxAge currTime
        Nothing -> return Nothing

      if
        -- If a max-age is specified with a must-revalidate we use it, but if not we use an immediate expiry time
        | mustRevalidateExists cacheControl -> pure $ fromMaybe currTime maxExpiryMaybe
        -- In these cases we want don't want to cache the JWK, so we use an immediate expiry time
        | noCacheExists cacheControl || noStoreExists cacheControl -> pure currTime
        -- Use max-age, if it exists
        | otherwise -> hoistMaybe maxExpiryMaybe

    timeFromExpires :: MaybeT m UTCTime
    timeFromExpires = do
      header <- afold $ bsToTxt <$> lookup "Expires" responseHeaders
      parseExpirationTime header `onLeft` const (logAndThrowInfo parseTimeErr)

    logAndThrowInfo :: (MonadIO m1, MonadError JwkFetchError m1) => JwkFetchError -> m1 a
    logAndThrowInfo err = do
      liftIO $ logger $ JwkRefreshLog LevelInfo Nothing (Just err)
      throwError err

type ClaimsMap = HashMap.HashMap SessionVariable J.Value

-- | Decode a Jose ClaimsSet without verifying the signature
decodeClaimsSet :: RawJWT -> Maybe Jose.ClaimsSet
decodeClaimsSet (RawJWT jwt) = do
  (_, c, _) <- extractElems $ BL.splitWith (== B.c2w '.') jwt
  case BAE.convertFromBase BAE.Base64URLUnpadded $ BL.toStrict c of
    Left _ -> Nothing
    Right s -> J.decode $ BL.fromStrict s
  where
    extractElems (h : c : s : _) = Just (h, c, s)
    extractElems _ = Nothing

-- | Extract the issuer from a bearer tokena _without_ verifying it.
tokenIssuer :: RawJWT -> Maybe StringOrURI
tokenIssuer = coerce <$> (decodeClaimsSet >=> view Jose.claimIss)

-- | Process the request headers to verify the JWT and extract UserInfo from it
-- From the JWT config, we check which header to expect, it can be the "Authorization"
-- or "Cookie" header
--
-- If no "Authorization"/"Cookie" header was passed, we will fall back to the
-- unauthenticated user role [1], if one was configured at server start.
--
-- When no 'x-hasura-user-role' is specified in the request, the mandatory
-- 'x-hasura-default-role' [2] from the JWT claims will be used.

-- [1]: https://hasura.io/docs/latest/graphql/core/auth/authentication/unauthenticated-access.html
-- [2]: https://hasura.io/docs/latest/graphql/core/auth/authentication/jwt.html#the-spec
processJwt ::
  ( MonadIO m,
    MonadError QErr m
  ) =>
  [JWTCtx] ->
  HTTP.RequestHeaders ->
  Maybe RoleName ->
  m (UserInfo, Maybe UTCTime, [N.Header], Maybe JWTCtx)
processJwt = processJwt_ processHeaderSimple tokenIssuer jcxHeader

type AuthTokenLocation = JWTHeader

-- Broken out for testing with mocks:
processJwt_ ::
  (MonadError QErr m) =>
  -- | mock 'processAuthZOrCookieHeader'
  (JWTCtx -> BLC.ByteString -> m (ClaimsMap, Maybe UTCTime)) ->
  (RawJWT -> Maybe StringOrURI) ->
  (JWTCtx -> JWTHeader) ->
  [JWTCtx] ->
  HTTP.RequestHeaders ->
  Maybe RoleName ->
  m (UserInfo, Maybe UTCTime, [N.Header], Maybe JWTCtx)
processJwt_ processJwtBytes decodeIssuer fGetHeaderType jwtCtxs headers mUnAuthRole = do
  -- Here we use `intersectKeys` to match up the correct locations of JWTs to those specified in JWTCtxs
  -- Then we match up issuers, where no-issuer specified in a JWTCtx can match any issuer in a JWT
  -- Then there should either be zero matches - Perform no auth
  -- Or one match - Perform normal auth
  -- Otherwise there is an ambiguous situation which we currently treat as an error.
  issuerMatches <- traverse issuerMatch $ intersectKeys (keyCtxOnAuthTypes jwtCtxs) (keyTokensOnAuthTypes headers)

  case (lefts issuerMatches, rights issuerMatches) of
    ([], []) -> withoutAuthZ
    (_ : _, []) -> jwtNotIssuerError
    (_, [(ctx, val)]) -> withAuthZ val ctx
    _ -> throw400 InvalidHeaders "Could not verify JWT: Multiple JWTs found"
  where
    intersectKeys :: (Hashable a) => HashMap.HashMap a [b] -> HashMap.HashMap a [c] -> [(b, c)]
    intersectKeys m n = concatMap (uncurry cartesianProduct) $ HashMap.elems $ HashMap.intersectionWith (,) m n

    issuerMatch (j, b) = do
      b'' <- case b of
        (JHCookie _, b') -> pure b'
        (JHAuthorization, b') ->
          case BC.words b' of
            ["Bearer", jwt] -> pure jwt
            _ -> throw400 InvalidHeaders "Malformed Authorization header"
        (JHCustomHeader _, b') -> pure b'

      case (StringOrURI <$> jcxIssuer j, decodeIssuer $ RawJWT $ BLC.fromStrict b'') of
        (Nothing, _) -> pure $ Right (j, b'')
        (_, Nothing) -> pure $ Right (j, b'')
        (ci, ji)
          | ci == ji -> pure $ Right (j, b'')
          | otherwise -> pure $ Left (ci, ji, j, b'')

    cartesianProduct :: [a] -> [b] -> [(a, b)]
    cartesianProduct as bs = [(a, b) | a <- as, b <- bs]

    keyCtxOnAuthTypes :: [JWTCtx] -> HashMap.HashMap AuthTokenLocation [JWTCtx]
    keyCtxOnAuthTypes = HashMap.fromListWith (++) . fmap (expectedHeader &&& pure)

    getCustomHeaderName :: JWTHeader -> Maybe Text
    getCustomHeaderName = \case
      JHCustomHeader headerName -> Just headerName
      _ -> Nothing

    getCustomHeaderNameList = mapMaybe (getCustomHeaderName . fGetHeaderType) jwtCtxs

    keyTokensOnAuthTypes :: [HTTP.Header] -> HashMap.HashMap AuthTokenLocation [(AuthTokenLocation, B.ByteString)]
    keyTokensOnAuthTypes = HashMap.fromListWith (++) . map (fst &&& pure) . concatMap (findTokensInHeader getCustomHeaderNameList)

    findTokensInHeader :: [Text] -> Header -> [(AuthTokenLocation, B.ByteString)]
    findTokensInHeader jwtCustomHeaderList (key, val)
      | key == CI.mk "Authorization" = [(JHAuthorization, val)]
      | key == CI.mk "Cookie" = bimap JHCookie T.encodeUtf8 <$> Spock.parseCookies val
      | key `elem` (map (CI.mk . T.encodeUtf8) jwtCustomHeaderList) =
          [(JHCustomHeader (T.toLower $ T.decodeUtf8 $ CI.original key), val)]
      | otherwise = []

    expectedHeader :: JWTCtx -> AuthTokenLocation
    expectedHeader jwtCtx =
      case fGetHeaderType jwtCtx of
        JHAuthorization -> JHAuthorization
        JHCookie name -> JHCookie name
        JHCustomHeader name -> JHCustomHeader (T.toLower name)

    withAuthZ authzHeader jwtCtx = do
      authMode <- processJwtBytes jwtCtx $ BL.fromStrict authzHeader

      let (claimsMap, expTimeM) = authMode
       in do
            HasuraClaims allowedRoles defaultRole <- parseHasuraClaims claimsMap
            -- see if there is a x-hasura-role header, or else pick the default role.
            -- The role returned is unauthenticated at this point:
            let requestedRole =
                  fromMaybe defaultRole
                    $ getRequestHeader userRoleHeader headers
                    >>= mkRoleName
                    . bsToTxt

            when (requestedRole `notElem` allowedRoles)
              $ throw400 AccessDenied "Your requested role is not in allowed roles"
            let finalClaims =
                  HashMap.delete defaultRoleClaim . HashMap.delete allowedRolesClaim $ claimsMap

            let finalClaimsObject =
                  KM.fromList
                    $ map (first (K.fromText . sessionVariableToText))
                    $ HashMap.toList finalClaims
            metadata <- parseJwtClaim (J.Object finalClaimsObject) "x-hasura-* claims"
            userInfo <-
              mkUserInfo (URBPreDetermined requestedRole) UAdminSecretNotSent
                $ mkSessionVariablesText metadata
            pure (userInfo, expTimeM, [], Just jwtCtx)

    withoutAuthZ = do
      unAuthRole <- onNothing mUnAuthRole (throw400 InvalidHeaders "Missing 'Authorization' or 'Cookie' header in JWT authentication mode")
      userInfo <-
        mkUserInfo (URBPreDetermined unAuthRole) UAdminSecretNotSent
          $ mkSessionVariablesHeaders headers
      pure (userInfo, Nothing, [], Nothing)

    jwtNotIssuerError = throw400 JWTInvalid "Could not verify JWT: JWTNotInIssuer"

-- | Processes a token payload (excluding the `Bearer ` prefix in the context of a JWTCtx)
processHeaderSimple ::
  ( MonadIO m,
    MonadError QErr m
  ) =>
  JWTCtx ->
  BLC.ByteString ->
  -- The "Maybe" in "m (Maybe (...))" covers the case where the
  -- requested Cookie name is not present (returns "m Nothing")
  m (ClaimsMap, Maybe UTCTime)
processHeaderSimple jwtCtx jwt = do
  -- iss <- _ <$> Jose.decodeCompact (BL.fromStrict token)
  -- let ctx = M.lookup iss jwtCtx

  -- try to parse JWT token from Authorization or Cookie header
  -- verify the JWT
  claims <- liftJWTError invalidJWTError $ verifyJwt jwtCtx $ RawJWT jwt

  let expTimeM = fmap (\(Jose.NumericDate t) -> t) $ claims ^. Jose.claimExp

  claimsObject <- parseClaimsMap claims claimsConfig

  pure (claimsObject, expTimeM)
  where
    claimsConfig = jcxClaims jwtCtx

    liftJWTError :: (MonadError e' m) => (e -> e') -> ExceptT e m a -> m a
    liftJWTError ef action = do
      res <- runExceptT action
      onLeft res (throwError . ef)

    invalidJWTError e = err400 JWTInvalid $ "Could not verify JWT: " <> tshow e

-- | parse the claims map from the JWT token or custom claims from the JWT config
parseClaimsMap ::
  (MonadError QErr m) =>
  -- | Unregistered JWT claims
  Jose.ClaimsSet ->
  -- | Claims config
  JWTClaims ->
  -- | Hasura claims and other claims
  m ClaimsMap
parseClaimsMap claimsSet jcxClaims = do
  let claimsJSON = J.toJSON claimsSet
      unregisteredClaims = claimsSet ^. Jose.unregisteredClaims
  case jcxClaims of
    -- when the user specifies the namespace of the hasura claims map,
    -- the hasura claims map *must* be specified in the unregistered claims
    JCNamespace namespace claimsFormat -> do
      claimsV <- flip onNothing (claimsNotFound namespace) $ case namespace of
        ClaimNs k -> M.lookup k unregisteredClaims
        ClaimNsPath path -> iResultToMaybe $ executeJSONPath path (J.toJSON unregisteredClaims)
      -- get hasura claims value as an object. parse from string possibly
      claimsObject <- parseObjectFromString namespace claimsFormat claimsV

      -- filter only x-hasura claims
      let claimsMap =
            HashMap.fromList
              $ map (first mkSessionVariable)
              $ filter (isSessionVariable . fst)
              $ map (first K.toText)
              $ KM.toList claimsObject

      pure claimsMap
    JCMap claimsConfig -> do
      let JWTCustomClaimsMap defaultRoleClaimsMap allowedRolesClaimsMap otherClaimsMap = claimsConfig

      allowedRoles <- case allowedRolesClaimsMap of
        JWTCustomClaimsMapJSONPath allowedRolesJsonPath defaultVal ->
          parseAllowedRolesClaim defaultVal $ iResultToMaybe $ executeJSONPath allowedRolesJsonPath claimsJSON
        JWTCustomClaimsMapStatic staticAllowedRoles -> pure staticAllowedRoles

      defaultRole <- case defaultRoleClaimsMap of
        JWTCustomClaimsMapJSONPath defaultRoleJsonPath defaultVal ->
          parseDefaultRoleClaim defaultVal
            $ iResultToMaybe
            $ executeJSONPath defaultRoleJsonPath claimsJSON
        JWTCustomClaimsMapStatic staticDefaultRole -> pure staticDefaultRole

      otherClaims <- flip HashMap.traverseWithKey otherClaimsMap $ \k claimObj -> do
        let throwClaimErr =
              throw400 JWTInvalidClaims
                $ "JWT claim from claims_map, "
                <> sessionVariableToText k
                <> " not found"
        case claimObj of
          JWTCustomClaimsMapJSONPath path defaultVal ->
            iResultToMaybe (executeJSONPath path claimsJSON)
              `onNothing` (J.String <$> defaultVal)
              `onNothing` throwClaimErr
          JWTCustomClaimsMapStatic claimStaticValue -> pure $ J.String claimStaticValue

      pure
        $ HashMap.fromList
          [ (allowedRolesClaim, J.toJSON allowedRoles),
            (defaultRoleClaim, J.toJSON defaultRole)
          ]
        <> otherClaims
  where
    parseAllowedRolesClaim defaultVal = \case
      Nothing ->
        onNothing defaultVal
          $ throw400 JWTRoleClaimMissing
          $ "JWT claim does not contain "
          <> sessionVariableToText allowedRolesClaim
      Just v ->
        parseJwtClaim v
          $ "invalid "
          <> sessionVariableToText allowedRolesClaim
          <> "; should be a list of roles"

    parseDefaultRoleClaim defaultVal = \case
      Nothing ->
        onNothing defaultVal
          $ throw400 JWTRoleClaimMissing
          $ "JWT claim does not contain "
          <> sessionVariableToText defaultRoleClaim
      Just v ->
        parseJwtClaim v
          $ "invalid "
          <> sessionVariableToText defaultRoleClaim
          <> "; should be a role"

    claimsNotFound namespace =
      throw400 JWTInvalidClaims $ case namespace of
        ClaimNsPath path ->
          "claims not found at claims_namespace_path: '"
            <> encodeJSONPath path
            <> "'"
        ClaimNs ns -> "claims key: '" <> ns <> "' not found"

    parseObjectFromString namespace claimsFmt jVal =
      case (claimsFmt, jVal) of
        (JCFStringifiedJson, J.String v) ->
          onLeft (J.eitherDecodeStrict $ T.encodeUtf8 v) (const $ claimsErr $ strngfyErr v)
        (JCFStringifiedJson, _) ->
          claimsErr "expecting a string when claims_format is stringified_json"
        (JCFJson, J.Object o) -> return o
        (JCFJson, _) ->
          claimsErr "expecting a json object when claims_format is json"
      where
        strngfyErr v =
          let claimsLocation = case namespace of
                ClaimNsPath path -> "claims_namespace_path " <> encodeJSONPath path
                ClaimNs ns -> "claims_namespace " <> ns
           in "expecting stringified json at: '"
                <> claimsLocation
                <> "', but found: "
                <> v

        claimsErr = throw400 JWTInvalidClaims

-- | Verify the JWT against given JWK
verifyJwt ::
  ( MonadError Jose.JWTError m,
    MonadIO m
  ) =>
  JWTCtx ->
  RawJWT ->
  m Jose.ClaimsSet
verifyJwt ctx (RawJWT rawJWT) = do
  keyConfig <- liftIO $ readIORef $ jcxKeyConfig ctx
  jwt <- Jose.decodeCompact rawJWT
  t <- liftIO getCurrentTime
  Jose.verifyClaimsAt config (fst keyConfig) t jwt
  where
    validationSettingsWithSkew =
      case jcxAllowedSkew ctx of
        Just allowedSkew -> Jose.defaultJWTValidationSettings audCheck & set Jose.allowedSkew allowedSkew
        -- In `Jose.defaultJWTValidationSettings`, the `allowedSkew` is 0
        Nothing -> Jose.defaultJWTValidationSettings audCheck

    config = case jcxIssuer ctx of
      Nothing -> validationSettingsWithSkew
      Just iss -> validationSettingsWithSkew & set Jose.issuerPredicate (== iss)
    audCheck audience =
      -- dont perform the check if there are no audiences in the conf
      case jcxAudience ctx of
        Nothing -> True
        Just (Jose.Audience audiences) -> audience `elem` audiences

instance J.ToJSON JWTConfig where
  toJSON (JWTConfig keyOrUrl aud iss claims allowedSkew jwtHeader) =
    let keyOrUrlPairs = case keyOrUrl of
          Left _ ->
            [ "type" J..= J.String "<TYPE REDACTED>",
              "key" J..= J.String "<JWK REDACTED>"
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
     in J.object
          $ keyOrUrlPairs
          <> [ "audience" J..= aud,
               "issuer" J..= iss,
               "header" J..= jwtHeader
             ]
          <> claimsPairs
          <> (maybe [] (\skew -> ["allowed_skew" J..= skew]) allowedSkew)

-- | Parse from a json string like:
-- | `{"type": "RS256", "key": "<PEM-encoded-public-key-or-X509-cert>"}`
-- | to JWTConfig
instance J.FromJSON JWTConfig where
  parseJSON = J.withObject "JWTConfig" $ \o -> do
    mRawKey <- o J..:? "key"
    claimsNs <- o J..:? "claims_namespace"
    claimsNsPath <- o J..:? "claims_namespace_path"
    aud <- o J..:? "audience"
    iss <- o J..:? "issuer"
    jwkUrl <- o J..:? "jwk_url"
    claimsFormat <- o J..:? "claims_format" J..!= defaultClaimsFormat
    claimsMap <- o J..:? "claims_map"
    allowedSkew <- o J..:? "allowed_skew"
    jwtHeader <- o J..:? "header"

    hasuraClaimsNs <-
      case (claimsNsPath, claimsNs) of
        (Nothing, Nothing) -> pure $ ClaimNs defaultClaimsNamespace
        (Just nsPath, Nothing) -> either failJSONPathParsing (return . ClaimNsPath) . parseJSONPath $ nsPath
        (Nothing, Just ns) -> return $ ClaimNs ns
        (Just _, Just _) -> fail "claims_namespace and claims_namespace_path both cannot be set"

    keyOrUrl <- case (mRawKey, jwkUrl) of
      (Nothing, Nothing) -> fail "key and jwk_url both cannot be empty"
      (Just _, Just _) -> fail "key, jwk_url both cannot be present"
      (Just rawKey, Nothing) -> do
        keyType <- o J..: "type"
        key <- parseKey keyType rawKey
        pure $ Left key
      (Nothing, Just url) -> pure $ Right url

    let jwtClaims = maybe (JCNamespace hasuraClaimsNs claimsFormat) JCMap claimsMap

    pure $ JWTConfig keyOrUrl aud iss jwtClaims allowedSkew jwtHeader
    where
      parseKey keyType rawKey =
        case keyType of
          "HS256" -> runEither $ parseHmacKey rawKey 256
          "HS384" -> runEither $ parseHmacKey rawKey 384
          "HS512" -> runEither $ parseHmacKey rawKey 512
          "RS256" -> runEither $ parseRsaKey rawKey
          "RS384" -> runEither $ parseRsaKey rawKey
          "RS512" -> runEither $ parseRsaKey rawKey
          "Ed25519" -> runEither $ parseEdDSAKey rawKey
          "ES256" -> runEither $ parseEsKey rawKey
          "ES384" -> runEither $ parseEsKey rawKey
          "ES512" -> runEither $ parseEsKey rawKey
          -- TODO(from master): support PS256, PS384, Ed448 (JOSE doesn't support it as of now)
          _ -> invalidJwk ("Key type: " <> T.unpack keyType <> " is not supported")

      runEither = either (invalidJwk . T.unpack) return

      invalidJwk msg = fail ("Invalid JWK: " <> msg)

      failJSONPathParsing err = fail . T.unpack $ "invalid JSON path claims_namespace_path error: " <> err

-- parse x-hasura-allowed-roles, x-hasura-default-role from JWT claims
parseHasuraClaims :: forall m. (MonadError QErr m) => ClaimsMap -> m HasuraClaims
parseHasuraClaims claimsMap = do
  HasuraClaims
    <$> parseClaim allowedRolesClaim "should be a list of roles"
    <*> parseClaim defaultRoleClaim "should be a single role name"
  where
    parseClaim :: (J.FromJSON a) => SessionVariable -> Text -> m a
    parseClaim claim hint = do
      claimV <- onNothing (HashMap.lookup claim claimsMap) missingClaim
      parseJwtClaim claimV $ "invalid " <> claimText <> "; " <> hint
      where
        missingClaim = throw400 JWTRoleClaimMissing $ "JWT claim does not contain " <> claimText
        claimText = sessionVariableToText claim

-- Utility:
parseJwtClaim :: (J.FromJSON a, MonadError QErr m) => J.Value -> Text -> m a
parseJwtClaim v errMsg =
  case J.fromJSON v of
    J.Success val -> return val
    J.Error e -> throw400 JWTInvalidClaims $ errMsg <> ": " <> T.pack e
