module Hasura.Server.Auth.JWT.Internal where

import           Control.Lens
import           Crypto.JOSE.Types        (Base64Integer (..))
import           Crypto.JWT
import           Crypto.PubKey.RSA        (PublicKey (..))
import           Data.ASN1.BinaryEncoding (DER (..))
import           Data.ASN1.Encoding       (decodeASN1')
import           Data.ASN1.Types          (ASN1 (End, IntVal, Start),
                                           ASN1ConstructionType (Sequence),
                                           fromASN1)
import           Data.Int                 (Int64)
import           Data.Text.Conversions

import           Hasura.Prelude
import           Hasura.Server.Utils      (fmapL)

import qualified Data.ByteString.Lazy     as BL
import qualified Data.PEM                 as PEM
import qualified Data.Text                as T
import qualified Data.X509                as X509

-- | Helper functions to decode Text to JWK

parseHmacKey :: T.Text -> Int64 -> Either T.Text JWK
parseHmacKey key size = do
  let secret = unUTF8 $ fromText key
      err s = "Key size too small; should be atleast " <> show (s `div` 8) <> " characters"
  if BL.length secret < size `div` 8
    then Left . T.pack $ err size
    else pure $ fromOctets secret

parseRsaKey :: T.Text -> Either T.Text JWK
parseRsaKey key = do
  let res = fromRawPem (unUTF8 $ fromText key)
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


getAtleastOne :: Text -> [a] -> Either Text a
getAtleastOne err []  = Left err
getAtleastOne _ (x:_) = Right x

calculateSize :: Integer -> Int
calculateSize = go 1
  where
    go i n | 2 ^ (i * 8) > n = i
           | otherwise       = go (i + 1) n
