module Hasura.Server.Auth.JWT.Internal
  ( parseEdDSAKey,
    parseHmacKey,
    parseRsaKey,
    parseEsKey,
  )
where

import Control.Lens
import Crypto.JOSE.Types (Base64Integer (..))
import Crypto.JWT
import Crypto.PubKey.RSA (PublicKey (..))
import Data.ASN1.BinaryEncoding (DER (..))
import Data.ASN1.Encoding (decodeASN1')
import Data.ASN1.Types
  ( ASN1 (End, IntVal, Start),
    ASN1ConstructionType (Sequence),
    fromASN1,
  )
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int64)
import Data.PEM qualified as PEM
import Data.Text qualified as T
import Data.Text.Conversions
import Data.X509 qualified as X509
import Hasura.Prelude
import Hasura.Server.Utils (fmapL)

-- | Helper functions to decode Text to JWK
parseHmacKey :: Text -> Int64 -> Either Text JWK
parseHmacKey key size = do
  let secret = unUTF8 $ fromText key
      err s = "Key size too small; should be atleast " <> show (s `div` 8) <> " characters"
  if BL.length secret < size `div` 8
    then Left . T.pack $ err size
    else pure $ fromOctets secret

parseRsaKey :: Text -> Either Text JWK
parseRsaKey key = do
  let res = fromRawPem (unUTF8 $ fromText key)
      err e = "Could not decode PEM: " <> e
  onLeft res (Left . err)

parseEdDSAKey :: Text -> Either Text JWK
parseEdDSAKey key = do
  let res = fromRawPem (unUTF8 $ fromText key)
      err e = "Could not decode PEM: " <> e
  onLeft res (Left . err)

parseEsKey :: Text -> Either Text JWK
parseEsKey key = do
  let res = fromRawPem (unUTF8 $ fromText key)
      err e = "Could not decode PEM: " <> e
  onLeft res (Left . err)

-- | Helper functions to decode PEM bytestring to RSA public key

-- try PKCS first, then x509
fromRawPem :: BL.ByteString -> Either Text JWK
fromRawPem bs =
  -- pubKeyToJwk <=< fromPkcsPem
  case fromPkcsPem bs of
    Right pk -> pubKeyToJwk pk
    Left e ->
      case fromX509Pem bs of
        Right pk1 -> pubKeyToJwk pk1
        Left e1 -> Left (e <> " " <> e1)

-- decode a PKCS1 or PKCS8 PEM to obtain the public key
fromPkcsPem :: BL.ByteString -> Either Text X509.PubKey
fromPkcsPem bs = do
  pems <- fmapL T.pack $ PEM.pemParseLBS bs
  pem <- getAtleastOne "No pem found" pems
  res <- fmapL tshow $ decodeASN1' DER $ PEM.pemContent pem
  case res of
    -- PKCS#1 format
    [Start Sequence, IntVal n, IntVal e, End Sequence] ->
      return $ X509.PubKeyRSA $ PublicKey (calculateSize n) n e
    -- try and see if its a PKCS#8 format
    asn1 -> do
      (pub, xs) <- fmapL T.pack $ fromASN1 asn1
      unless (null xs) (Left "Could not decode public key")
      return pub

-- decode a x509 certificate containing the RSA public key or EdDSA (ed25519) public key
fromX509Pem :: BL.ByteString -> Either Text X509.PubKey
fromX509Pem s = do
  -- try to parse bytestring to a [PEM]
  pems <- fmapL T.pack $ PEM.pemParseLBS s
  -- fail if [PEM] is empty
  pem <- getAtleastOne "No pem found" pems
  -- decode the bytestring to a certificate
  signedExactCert <-
    fmapL T.pack
      $ X509.decodeSignedCertificate
      $ PEM.pemContent pem
  let cert = X509.signedObject $ X509.getSigned signedExactCert
      pubKey = X509.certPubKey cert
  case pubKey of
    X509.PubKeyRSA pk -> return $ X509.PubKeyRSA pk
    X509.PubKeyEd25519 pk -> return $ X509.PubKeyEd25519 pk
    X509.PubKeyEC pk -> return $ X509.PubKeyEC pk
    _ -> Left "Could not decode RSA, EdDSA or EC public key from x509 cert"

pubKeyToJwk :: X509.PubKey -> Either Text JWK
pubKeyToJwk pubKey = do
  jwk' <- mkJwk
  return $ jwk' & jwkKeyOps .~ Just [Verify]
  where
    mkJwk = case pubKey of
      X509.PubKeyRSA (PublicKey _ n e) ->
        return $ fromKeyMaterial $ RSAKeyMaterial (rsaKeyParams n e)
      X509.PubKeyEd25519 pubKeyEd ->
        return $ fromKeyMaterial $ OKPKeyMaterial (Ed25519Key pubKeyEd Nothing)
      X509.PubKeyEC pubKeyEc ->
        case ecParametersFromX509 pubKeyEc of
          Nothing -> Left "Error getting EC parameters from the public key"
          Just ecKeyParameters ->
            return $ fromKeyMaterial $ ECKeyMaterial ecKeyParameters
      _ -> Left "This key type is not supported"
    rsaKeyParams n e =
      RSAKeyParameters (Base64Integer n) (Base64Integer e) Nothing

getAtleastOne :: Text -> [a] -> Either Text a
getAtleastOne err [] = Left err
getAtleastOne _ (x : _) = Right x

calculateSize :: Integer -> Int
calculateSize = go 1
  where
    go i n
      | 2 ^ (i * 8) > n = i
      | otherwise = go (i + 1) n
