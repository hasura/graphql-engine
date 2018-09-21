{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Hasura.Server.Auth.JWT
  ( processJwt
  , RawJWT
  , JWTConfig (..)
  , JWTCtx (..)
  , JWKSet (..)
  , updateJwkRef
  ) where

import           Control.Exception          (try)
import           Control.Lens
import           Control.Monad              (when)
import           Crypto.JOSE.Types          (Base64Integer (..))
import           Crypto.JWT
import           Crypto.PubKey.RSA          (PublicKey (..))
import           Data.ASN1.BinaryEncoding   (DER (..))
import           Data.ASN1.Encoding         (decodeASN1')
import           Data.ASN1.Types            (ASN1 (End, IntVal, Start),
                                             ASN1ConstructionType (Sequence),
                                             fromASN1)
import           Data.Int                   (Int64)
import           Data.IORef                 (IORef, modifyIORef, readIORef)

import           Data.List                  (find)
import           Data.Time.Clock            (diffUTCTime, getCurrentTime)
import           Data.Time.Format           (defaultTimeLocale, parseTimeM)

import           Hasura.Logging             (LogLevel (..), Logger)
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.Server.Logging      (JwkRefreshHttpError (..),
                                             JwkRefreshLog (..))
import           Hasura.Server.Utils        (accessKeyHeader, bsToTxt,
                                             userRoleHeader)

import qualified Control.Concurrent         as C
import qualified Data.Aeson                 as A
import qualified Data.Aeson.Casing          as A
import qualified Data.Aeson.TH              as A
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.CaseInsensitive       as CI
import qualified Data.HashMap.Strict        as Map
import qualified Data.PEM                   as PEM
import qualified Data.String.Conversions    as CS
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import qualified Data.X509                  as X509
import qualified Network.HTTP.Client        as HTTP
import qualified Network.HTTP.Types         as HTTP
import qualified Network.URI                as N
import qualified Network.Wreq               as Wreq


newtype RawJWT = RawJWT BL.ByteString

data JWTConfig
  = JWTConfig
  { jcType     :: !T.Text
  , jcKey      :: !(Maybe JWK)
  , jcJwkUrl   :: !(Maybe N.URI)
  , jcClaimNs  :: !(Maybe T.Text)
  , jcAudience :: !(Maybe T.Text)
  -- , jcIssuer   :: !(Maybe T.Text)
  } deriving (Show, Eq)

data JWTCtx
  = JWTCtx
  { jcxKey      :: IORef JWKSet -- should it have strictness anno?
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


mkJwkRefreshLog :: T.Text -> Maybe JwkRefreshHttpError -> JwkRefreshLog
mkJwkRefreshLog = JwkRefreshLog (LevelOther "critical")

-- | Given a JWK url, fetch JWK from it and create an IORef; also create
-- | a background thread, if the JWK expires, to refresh it
updateJwkRef
  :: ( MonadIO m
     , MonadError T.Text m)
  => Logger
  -> HTTP.Manager
  -> N.URI
  -> IORef JWKSet
  -> m ()
updateJwkRef logger manager url jwkRef = do
  let options = Wreq.defaults
              & Wreq.checkResponse ?~ (\_ _ -> return ())
              & Wreq.manager .~ Right manager

  res  <- liftIO $ try $ Wreq.getWith options $ show url
  resp <- either logAndThrow return res
  let status = resp ^. Wreq.responseStatus
      respBody = resp ^. Wreq.responseBody

  when (status ^. Wreq.statusCode /= 200) $ do
    let urlT = T.pack $ show url
        respBodyT = Just $ CS.cs respBody
        errMsg = "non-200 response on fetching JWK from: " <> urlT
        httpErr = Just (JwkRefreshHttpError (Just status) urlT Nothing respBodyT)
    throwRetry errMsg httpErr

  jwkset <- either (\e -> throwRetry (T.pack e) Nothing) return . A.eitherDecode $ respBody
  liftIO $ modifyIORef jwkRef (const jwkset)
  let mExpiresT = resp ^? Wreq.responseHeader "Expires"
  case mExpiresT of
    Nothing -> return ()
    Just expiresT -> do
      let expiresE = parseTimeM True defaultTimeLocale timeFmt $ CS.cs expiresT
      expires  <- either (`throwRetry` Nothing) return expiresE
      currTime <- liftIO getCurrentTime
      let expiryTtl = diffUTCTime expires currTime
      void $ liftIO $ C.forkIO (waitAndRefreshToken $ realToFrac expiryTtl)

  where
    logAndThrow :: (MonadIO m, MonadError T.Text m) => HTTP.HttpException -> m a
    logAndThrow err = do
      let httpErr = JwkRefreshHttpError Nothing (T.pack $ show url) (Just err) Nothing
          errMsg = "Critical: error fetching JWK: " <> T.pack (show err)
      throwRetry errMsg (Just httpErr)

    timeFmt = "%a, %d %b %Y %T GMT"

    waitAndRefreshToken :: Double -> IO ()
    waitAndRefreshToken seconds = do
      let delay = (floor seconds - 10) * 1000 * 1000
      C.threadDelay delay
      res <- runExceptT $ updateJwkRef logger manager url jwkRef
      either (putStrLn . T.unpack) return res

    throwRetry :: (MonadIO m, MonadError T.Text m) => T.Text -> Maybe JwkRefreshHttpError -> m a
    throwRetry err httpErr = do
      void $ liftIO $ C.forkIO (waitAndRefreshToken 30)
      liftIO $ logger $ mkJwkRefreshLog err httpErr
      throwError err

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
      let userRoleHeaderB = TE.encodeUtf8 userRoleHeader
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

    case mRawKey of
      Nothing -> return $ JWTConfig keyType Nothing jwkUrl claimNs aud
      Just rawKey -> do
        key <- case keyType of
          "HS256" -> runEither $ parseHmacKey rawKey 256
          "HS384" -> runEither $ parseHmacKey rawKey 384
          "HS512" -> runEither $ parseHmacKey rawKey 512
          "RS256" -> runEither $ parseRsaKey rawKey
          "RS384" -> runEither $ parseRsaKey rawKey
          "RS512" -> runEither $ parseRsaKey rawKey
          -- TODO: support ES256, ES384, ES512, PS256, PS384
          _       -> invalidJwk ("Key type: " <> T.unpack keyType <> " is not supported")

        return $ JWTConfig keyType (Just key) jwkUrl claimNs aud
    where
      runEither = either (invalidJwk . T.unpack) return
      invalidJwk msg = fail ("Invalid JWK: " <> msg)


-- | Helper functions to decode Text to JWK

parseHmacKey :: T.Text -> Int64 -> Either T.Text JWK
parseHmacKey key size = do
  let secret = BL.fromStrict $ TE.encodeUtf8 key
      err s = "Key size too small; should be atleast " <> show (s `div` 8) <> " characters"
  if BL.length secret < size `div` 8
    then Left . T.pack $ err size
    else pure $ fromOctets secret

parseRsaKey :: T.Text -> Either T.Text JWK
parseRsaKey key = do
  let res = fromRawPem (BL.fromStrict $ TE.encodeUtf8 key)
      err e = "Could not decode PEM: " <> e
  either (Left . err) pure res


-- | Helper functions to decode PEM bytestring to RSA public key

-- try PKCS first, then x509
fromRawPem :: BL.ByteString -> Either Text JWK
fromRawPem bs = -- pubKeyToJwk <=< fromPkcsPem
  case fromPkcsPem bs of
    Right pk -> pubKeyToJwk pk
    Left e ->
      case fromX509Pem bs of
        Right pk1 -> pubKeyToJwk pk1
        Left e1   -> Left (e <> " " <> e1)

-- decode a PKCS1 or PKCS8 PEM to obtain the public key
fromPkcsPem :: BL.ByteString -> Either Text X509.PubKey
fromPkcsPem bs = do
  pems <- fmapL T.pack $ PEM.pemParseLBS bs
  pem  <- getAtleastOne "No pem found" pems
  res  <- fmapL asn1ErrToText $ decodeASN1' DER $ PEM.pemContent pem
  case res of
    -- PKCS#1 format
    [Start Sequence, IntVal n, IntVal e, End Sequence] ->
      return $ X509.PubKeyRSA $ PublicKey (calculateSize n) n e
    -- try and see if its a PKCS#8 format
    asn1 -> do
      (pub, xs) <- fmapL T.pack $ fromASN1 asn1
      unless (null xs) (Left "Could not decode public key")
      return pub
  where
    asn1ErrToText = T.pack . show


-- decode a x509 certificate containing the RSA public key
fromX509Pem :: BL.ByteString -> Either Text X509.PubKey
fromX509Pem s = do
  -- try to parse bytestring to a [PEM]
  pems <- fmapL T.pack $ PEM.pemParseLBS s
  -- fail if [PEM] is empty
  pem <- getAtleastOne "No pem found" pems
  -- decode the bytestring to a certificate
  signedExactCert <- fmapL T.pack $ X509.decodeSignedCertificate $
                     PEM.pemContent pem
  let cert = X509.signedObject $ X509.getSigned signedExactCert
      pubKey = X509.certPubKey cert
  case pubKey of
    X509.PubKeyRSA pk -> return $ X509.PubKeyRSA pk
    _ -> Left "Could not decode RSA public key from x509 cert"


pubKeyToJwk :: X509.PubKey -> Either Text JWK
pubKeyToJwk pubKey = do
  jwk' <- mkJwk
  return $ jwk' & jwkKeyOps .~ Just [Verify]
  where
    mkJwk = case pubKey of
      X509.PubKeyRSA (PublicKey _ n e) ->
        return $ fromKeyMaterial $ RSAKeyMaterial (rsaKeyParams n e)
      _ -> Left "This key type is not supported"
    rsaKeyParams n e =
      RSAKeyParameters (Base64Integer n) (Base64Integer e) Nothing


fmapL :: (a -> a') -> Either a b -> Either a' b
fmapL fn (Left e) = Left (fn e)
fmapL _ (Right x) = pure x

getAtleastOne :: Text -> [a] -> Either Text a
getAtleastOne err []  = Left err
getAtleastOne _ (x:_) = Right x

calculateSize :: Integer -> Int
calculateSize = go 1
  where
    go i n | 2 ^ (i * 8) > n = i
           | otherwise       = go (i + 1) n
