{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Hasura.Server.Auth.JWT
  ( processJwt
  , RawJWT
  , JWTConfig (..)
  ) where

import           Control.Lens
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
--import           Debug.Trace
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.Server.Utils        (userRoleHeader)

import qualified Data.Aeson                 as A
-- import qualified Data.ByteString.Base64     as B64
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

-- | Process the request headers to verify the JWT and extract UserInfo from it
processJwt
  :: ( MonadIO m
     , MonadError QErr m)
  => JWK
  -> HTTP.RequestHeaders
  -> m UserInfo
processJwt key headers = do

  -- try to parse JWT token from Authorization header
  let mAuthzHeader = find (\h -> fst h == CI.mk "Authorization") headers
  (_, authzHeader) <- maybe missingAuthzHeader return mAuthzHeader
  let tokenParts = BLC.words $ BL.fromStrict authzHeader
  when (length tokenParts /= 2) malformedAuthzHeader

  -- verify the JWT
  let jwt = tokenParts !! 1
  claims <- liftJWTError invalidJWTError $ verifyJwt key jwt

  -- filter only x-hasura claims
  let claimsMap = Map.filterWithKey (\k _ -> T.isPrefixOf "x-hasura-" k) $
        claims ^. unregisteredClaims

  -- transform the map of text:aeson-value -> text:text
  metadataWithRole <- decodeJSON $ A.Object claimsMap

  -- throw error if role is not in claims
  let mRole = Map.lookup userRoleHeader metadataWithRole
  role <- maybe missingRoleClaim return mRole

  -- delete the x-hasura-role key from this map
  let metadata = Map.delete userRoleHeader metadataWithRole

  return $ UserInfo (RoleName role) metadata

  where
    liftJWTError :: (MonadError e' m) => (e -> e') -> ExceptT e m a -> m a
    liftJWTError ef action = do
      res <- runExceptT action
      either (throwError . ef) return res

    decodeJSON val = case A.fromJSON val of
      A.Error e   -> throw400 JWTInvalidClaims ("x-hasura-* claims: " <> T.pack e)
      A.Success a -> return a

    invalidJWTError e =
      err400 JWTInvalid $ "Could not verify JWT: " <> T.pack (show e)

    missingRoleClaim =
      throw400 JWTRoleClaimMissing "Your JWT claim should contain x-hasura-role"
    malformedAuthzHeader =
      throw400 InvalidHeaders "Malformed Authorization header"
    missingAuthzHeader =
      throw400 InvalidHeaders "Missing Authorization header in JWT authentication mode"


verifyJwt
  :: ( MonadError JWTError m
     , MonadIO m
     )
  => JWK
  -> RawJWT
  -> m ClaimsSet
verifyJwt key rawJWT = do
  jwt <- decodeCompact rawJWT -- decode JWT
  t <- liftIO getCurrentTime
  verifyClaimsAt config key t jwt
  where
    audCheck = const True -- we ignore the audience check?
    config = defaultJWTValidationSettings audCheck


-- HGE's own representation of various JWKs
data JWTConfig
  = JWTConfig
  { jcType :: !T.Text
  , jcKey  :: !JWK
  } deriving (Show, Eq)

-- | Parse from a json string like `{"type": "RS256", "key": <bytestring>}` to
-- | JWTConfig
instance A.FromJSON JWTConfig where

  parseJSON = A.withObject "foo" $ \o -> do
    keyType <- o A..: "type"
    rawKey  <- o A..: "key"
    case keyType of
      "HS256" -> parseHmacKey keyType rawKey 256
      "HS384" -> parseHmacKey keyType rawKey 384
      "HS512" -> parseHmacKey keyType rawKey 512
      "RS256" -> parseRsaKey keyType rawKey
      "RS384" -> parseRsaKey keyType rawKey
      "RS512" -> parseRsaKey keyType rawKey
      -- TODO: support ES256, ES384, ES512, PS256, PS384
      _       -> invalidJwk ("Key type: " <> T.unpack keyType <> " not supported")
    where
      parseHmacKey ktype key size = do
        let secret = BL.fromStrict $ TE.encodeUtf8 key
        when (BL.length secret < size `div` 8) $
          invalidJwk "Key size too small"
        return $ JWTConfig ktype $ fromOctets secret

      parseRsaKey ktype key = do
        let res = fromRawPem (BL.fromStrict $ TE.encodeUtf8 key)
            err e = "Could not decode PEM: " <> T.unpack e
        either (invalidJwk . err) (return . JWTConfig ktype) res

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
fromPkcsPem :: BL.ByteString -> Either Text PublicKey
fromPkcsPem bs = do
  pems <- fmapL T.pack $ PEM.pemParseLBS bs
  pem  <- getAtleastOnePem pems
  res  <- fmapL asn1ErrToText $ decodeASN1' DER $ PEM.pemContent pem
  -- trace ("ASN1 decodes: " ++ show res) (return ())
  case res of
    -- PKCS#1 format
    [Start Sequence, IntVal n, IntVal e, End Sequence] ->
      return $ PublicKey (calculateSize n) n e
    -- try and see if its a PKCS#8 format
    asn1 -> do
      (pub, []) <- fmapL T.pack $ fromASN1 asn1
      case pub of
        X509.PubKeyRSA pk -> return pk
        _                 -> Left "Could not decode RSA public key"
  where
    asn1ErrToText = T.pack . show

-- decode a x509 certificate containing the RSA public key
fromX509Pem :: BL.ByteString -> Either Text PublicKey
fromX509Pem s = do
  -- try to parse bytestring to a [PEM]
  pems <- fmapL T.pack $ PEM.pemParseLBS s
  -- fail if [PEM] is empty
  pem <- getAtleastOnePem pems

  -- decode the bytestring to a certificate
  signedExactCert <- fmapL T.pack $ X509.decodeSignedCertificate $
                     PEM.pemContent pem
  let cert = X509.signedObject $ X509.getSigned signedExactCert
      pubKey = X509.certPubKey cert

  case pubKey of
    X509.PubKeyRSA pk -> return pk
    _ -> Left "Could not decode RSA public key from x509 cert"


pubKeyToJwk :: PublicKey -> Either Text JWK
pubKeyToJwk (PublicKey _ n e) = do
  let jwk' = fromKeyMaterial $ RSAKeyMaterial rsaKeyParams
  return $ jwk' & jwkKeyOps .~ Just [Verify]
  where
    rsaKeyParams = RSAKeyParameters (Base64Integer n) (Base64Integer e) Nothing


fmapL :: (a -> a') -> Either a b -> Either a' b
fmapL fn (Left e) = Left (fn e)
fmapL _ (Right x) = pure x

getAtleastOnePem :: [PEM.PEM] -> Either Text PEM.PEM
getAtleastOnePem []    = Left "No pem found"
getAtleastOnePem (x:_) = Right x

calculateSize :: Integer -> Int
calculateSize = go 1
  where
    go i n | 2 ^ (i * 8) > n = i
           | otherwise       = go (i + 1) n
