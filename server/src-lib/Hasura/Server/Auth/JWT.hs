{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

module Hasura.Server.Auth.JWT
  ( processJwt
  , RawJWT
  , JWTConfig (..)
  ) where

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
import           Data.List                  (find)
import           Data.Time.Clock            (getCurrentTime)
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.Server.Utils        (accessKeyHeader, bsToTxt,
                                             userRoleHeader)

import qualified Data.Aeson                 as A
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.CaseInsensitive       as CI
import qualified Data.HashMap.Strict        as Map
import qualified Data.PEM                   as PEM
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import qualified Data.X509                  as X509
import qualified Network.HTTP.Types         as HTTP


type RawJWT = BL.ByteString

newtype AllowedRoles
  = AllowedRoles { getAllowedRoles :: [RoleName] }
  deriving (Show, Eq, A.FromJSON)


-- | Process the request headers to verify the JWT and extract UserInfo from it
processJwt
  :: ( MonadIO m
     , MonadError QErr m)
  => JWTConfig
  -> HTTP.RequestHeaders
  -> m UserInfo
processJwt conf headers = do
  -- try to parse JWT token from Authorization header
  jwt <- parseAuthzHeader

  -- verify the JWT
  claims <- liftJWTError invalidJWTError $ verifyJwt (jcKey conf) jwt

  -- filter only x-hasura claims
  let claimsMap = Map.filterWithKey (\k _ -> T.isPrefixOf "x-hasura-" k) $
        claims ^. unregisteredClaims

  allowedRoles <- parseAllowedRoles claimsMap
  (role, metadata) <- parseRoleAndMetadata conf headers allowedRoles claimsMap

  -- delete the x-hasura-access-key from this map
  let finalMetadata = Map.delete accessKeyHeader metadata
  return $ UserInfo role finalMetadata

  where
    parseAuthzHeader = do
      let mAuthzHeader = find (\h -> fst h == CI.mk "Authorization") headers
      (_, authzHeader) <- maybe missingAuthzHeader return mAuthzHeader
      let tokenParts = BLC.words $ BL.fromStrict authzHeader
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
    missingAuthzHeader =
      throw400 InvalidHeaders "Missing Authorization header in JWT authentication mode"

-- parse x-hasura-allowed-roles from JWT claims
parseAllowedRoles
  :: (MonadError QErr m)
  => A.Object -> m (Maybe [RoleName])
parseAllowedRoles claimsMap = do
  let allowedRoles = Map.lookup "x-hasura-allowed-roles" claimsMap
  case A.fromJSON <$> allowedRoles of
    Nothing -> return Nothing
    Just ar -> parseRes ar
  where
    parseRes r =
      case r of
        A.Success val -> return val
        A.Error _ -> throw400 JWTInvalidClaims "invalid x-hasura-allowed-roles; should be a list of roles"


-- | Meat of the logic of JWT authz. Determine if x-hasura-allowed-roles is
-- | there, then parse that and assert if user's current role is in that list. User's
-- | current role comes from request header x-hasura-role if found or defaults to
-- | one mentioned in JWTConfig.
-- | If x-hasura-allowed-roles is not present, then the JWT claim should contain
-- | x-hasura-role
-- | Finally, return the deduced role and the claim converted to metadata
parseRoleAndMetadata
  :: (MonadError QErr m)
  => JWTConfig
  -> HTTP.RequestHeaders
  -> Maybe [RoleName] -- allowed roles
  -> A.Object         -- the JWT claims
  -> m (RoleName, Map.HashMap T.Text T.Text)
parseRoleAndMetadata conf headers mAllowedRoles claimsMap =
  case mAllowedRoles of
    Just allowedRoles -> do
      -- if allowed roles present, check if current role is part of that.
      -- current role: check if role is present in header, else pick default
      -- role from jwt config
      when (getCurrentRole `notElem` allowedRoles) currRoleNotAllowed
      let finalClaims = Map.delete "x-hasura-allowed-roles" claimsMap
      -- transform the map of text:aeson-value -> text:text
      metadata <- decodeJSON $ A.Object finalClaims
      let md = Map.insert "x-hasura-role" (getRoleTxt getCurrentRole) metadata
      return (getCurrentRole, md)

    -- no allowed roles present, convert the claims and try to assert user role
    -- is present
    Nothing -> do
      -- transform the map of text:aeson-value -> text:text
      metadata <- decodeJSON $ A.Object claimsMap
      let mRole = Map.lookup userRoleHeader metadata
      -- throw error if role is not in claims
      role <- maybe missingRoleClaim return mRole
      return (RoleName role, metadata)

  where
    -- see if there is a x-hasura-role header, or else pick the default role
    -- from conf
    getCurrentRole =
      let userRoleHeaderB = TE.encodeUtf8 userRoleHeader
          mUserRole = snd <$> find (\h -> fst h == CI.mk userRoleHeaderB) headers
      in RoleName $ maybe (jcDefaultRole conf) bsToTxt mUserRole

    decodeJSON val = case A.fromJSON val of
      A.Error e   -> throw400 JWTInvalidClaims ("x-hasura-* claims: " <> T.pack e)
      A.Success a -> return a

    currRoleNotAllowed =
      throw400 AccessDenied "Your current role is not in allowed roles"
    missingRoleClaim =
      let msg = "JWT claim does not contain " <> userRoleHeader <> " or x-hasura-allowed-roles"
      in throw400 JWTRoleClaimMissing msg


-- | Verify the JWT against given JWK
verifyJwt
  :: ( MonadError JWTError m
     , MonadIO m
     )
  => JWK
  -> RawJWT
  -> m ClaimsSet
verifyJwt key rawJWT = do
  jwt <- decodeCompact rawJWT -- decode JWT
  t   <- liftIO getCurrentTime
  verifyClaimsAt config key t jwt
  where
    audCheck = const True -- we ignore the audience check?
    config = defaultJWTValidationSettings audCheck


-- | HGE's own representation of various JWKs
data JWTConfig
  = JWTConfig
  { jcType        :: !T.Text
  , jcKey         :: !JWK
  , jcDefaultRole :: !T.Text
  } deriving (Show, Eq)

-- | Parse from a json string like:
-- | `{"type": "RS256", "key": "<PEM-encoded-public-key-or-X509-cert>", "default_role": "user"}`
-- | to JWTConfig
instance A.FromJSON JWTConfig where

  parseJSON = A.withObject "JWTConfig" $ \o -> do
    keyType <- o A..: "type"
    rawKey  <- o A..: "key"
    defaultRole <- o A..: "default_role"
    case keyType of
      "HS256" -> parseHmacKey rawKey 256 keyType defaultRole
      "HS384" -> parseHmacKey rawKey 384 keyType defaultRole
      "HS512" -> parseHmacKey rawKey 512 keyType defaultRole
      "RS256" -> parseRsaKey rawKey keyType defaultRole
      "RS384" -> parseRsaKey rawKey keyType defaultRole
      "RS512" -> parseRsaKey rawKey keyType defaultRole
      -- TODO: support ES256, ES384, ES512, PS256, PS384
      _       -> invalidJwk ("Key type: " <> T.unpack keyType <> " not supported")
    where
      parseHmacKey key size ktype role = do
        let secret = BL.fromStrict $ TE.encodeUtf8 key
        when (BL.length secret < size `div` 8) $
          invalidJwk "Key size too small"
        return $ JWTConfig ktype (fromOctets secret) role

      parseRsaKey key ktype role = do
        let res = fromRawPem (BL.fromStrict $ TE.encodeUtf8 key)
            err e = "Could not decode PEM: " <> T.unpack e
        either (invalidJwk . err) (\k -> return $ JWTConfig ktype k role) res

      invalidJwk msg = fail ("Invalid JWK: " <> msg)


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
