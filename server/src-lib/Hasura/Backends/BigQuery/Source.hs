{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoGeneralisedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.BigQuery.Source
  ( BigQueryConnSourceConfig (..),
    RetryOptions (..),
    BigQueryConnection (..),
    BigQuerySourceConfig (..),
    ConfigurationInput (..),
    ConfigurationInputs (..),
    ConfigurationJSON (..),
    GoogleAccessToken (GoogleAccessToken),
    PKey (unPKey),
    ServiceAccount (..),
    TokenResp (..),
  )
where

import Autodocodec
import Control.Concurrent.MVar
import Crypto.PubKey.RSA.Types qualified as Cry
import Data.Aeson qualified as J
import Data.Aeson.Casing qualified as J
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.TH qualified as J
import Data.ByteString.Lazy qualified as BL
import Data.Int qualified as Int
import Data.Scientific (Scientific)
import Data.Text.Encoding qualified as TE
import Data.X509 qualified as X509
import Data.X509.Memory qualified as X509
import Hasura.Incremental (Cacheable (..))
import Hasura.Metadata.DTO.Utils (fromEnvCodec)
import Hasura.Prelude

data PKey = PKey
  { unPKey :: Cry.PrivateKey,
    originalBS :: Text
  }
  deriving (Show, Eq, Data, Generic, NFData, Hashable)

deriving instance Generic Cry.PrivateKey -- orphan

deriving instance Generic Cry.PublicKey -- orphan

deriving instance J.ToJSON Cry.PrivateKey -- orphan

deriving instance J.ToJSON Cry.PublicKey -- orphan

deriving instance Hashable Cry.PrivateKey -- orphan

deriving instance Hashable Cry.PublicKey -- orphan

instance HasCodec PKey where
  codec = bimapCodec dec originalBS codec
    where
      dec k = case X509.readKeyFileFromMemory $ TE.encodeUtf8 k of
        [X509.PrivKeyRSA k'] -> Right $ PKey k' k
        _ -> Left "unable to parse private key"

instance J.FromJSON PKey where
  parseJSON = J.withText "private_key" $ \k ->
    case X509.readKeyFileFromMemory $ TE.encodeUtf8 k of
      [X509.PrivKeyRSA k'] -> return $ PKey k' k
      _ -> fail "unable to parse private key"

instance J.ToJSON PKey where
  toJSON PKey {..} = J.toJSON originalBS

newtype GoogleAccessToken
  = GoogleAccessToken Text
  deriving (Show, Eq, J.FromJSON, J.ToJSON, Hashable, Generic, Data, NFData)

data TokenResp = TokenResp
  { _trAccessToken :: GoogleAccessToken,
    _trExpiresAt :: Integer -- Number of seconds until expiry from `now`, but we add `now` seconds to this for easy tracking
  }
  deriving (Eq, Show, Data, NFData, Generic, Hashable)

instance J.FromJSON TokenResp where
  parseJSON = J.withObject "TokenResp" $ \o ->
    TokenResp
      <$> o J..: "access_token"
      <*> o J..: "expires_in"

data ServiceAccount = ServiceAccount
  { _saClientEmail :: Text,
    _saPrivateKey :: PKey,
    _saProjectId :: Text
  }
  deriving (Eq, Show, Data, NFData, Generic, Hashable)

instance HasCodec ServiceAccount where
  codec =
    object "BigQueryServiceAccount" $
      ServiceAccount
        <$> requiredField' "client_email" .= _saClientEmail
        <*> requiredField' "private_key" .= _saPrivateKey
        <*> requiredField' "project_id" .= _saProjectId

$(J.deriveJSON (J.aesonDrop 3 J.snakeCase) {J.omitNothingFields = False} ''ServiceAccount)

data ConfigurationJSON a
  = FromEnvJSON Text
  | FromYamlJSON a
  deriving stock (Show, Eq, Generic)
  deriving (NFData, Hashable)

-- This codec has straightforward encoding, but on decoding there is
-- a possibility of receiving a string that contains JSON that is recursively
-- handled by this codec. There is also the issue that decoding the
-- @FromYamlJSON@ case should be attempted last because there is a possibility
-- that the decoding for @a@ is not disjoint from the other decoding cases. This
-- presents some asymmetry that is a little tricky to capture in a codec.
instance HasCodec a => HasCodec (ConfigurationJSON a) where
  codec = parseAlternative (parseAlternative mainCodec fromEnvEncodedAsNestedJSON) yamlJSONCodec
    where
      -- This is the only codec in this implementation that is used for
      -- encoding. It must cover both the @FromEnvJSON@ and @FromYamlJSON@ cases
      -- because Autodocodec does not support codecs that are partial in
      -- encoding.
      mainCodec :: JSONCodec (ConfigurationJSON a)
      mainCodec =
        dimapCodec dec enc $
          eitherCodec
            fromEnvCodec
            ( bimapCodec
                -- Fail parsing at this point because @codec \@a@ should only be
                -- used for parsing after trying @fromEnvEncodedAsNestedJSON@.
                (const $ Left "not used for parsing")
                id
                $ codec @a
            )
        where
          dec (Left text) = FromEnvJSON text
          dec (Right a) = FromYamlJSON a

          enc (FromEnvJSON i) = Left i
          enc (FromYamlJSON j) = Right j

      -- The JSON-encoded string case is used as an alternative in
      -- a 'parseAlternative' because we can implement the decoding direction,
      -- but not the encoding direction. (There isn't a good way to implement
      -- @ConfigurationJSON a -> Text@.) Fortunately an alternative in
      -- a 'parseAlternative' is only used for decoding so we don't need to
      -- implement encoding logic here.
      fromEnvEncodedAsNestedJSON :: ValueCodec Text (ConfigurationJSON a)
      fromEnvEncodedAsNestedJSON =
        bimapCodec
          (eitherDecodeJSONViaCodec . BL.fromStrict . TE.encodeUtf8)
          id
          $ codec @Text <?> "JSON-encoded string"

      yamlJSONCodec :: ValueCodec a (ConfigurationJSON a)
      yamlJSONCodec = FromYamlJSON <$> codec @a

instance J.FromJSON a => J.FromJSON (ConfigurationJSON a) where
  parseJSON = \case
    J.Object o | Just (J.String text) <- KM.lookup "from_env" o -> pure (FromEnvJSON text)
    J.String s -> case J.eitherDecode . BL.fromStrict . TE.encodeUtf8 $ s of
      Left {} -> fail "error parsing configuration json"
      Right sa -> pure sa
    j -> fmap FromYamlJSON (J.parseJSON j)

instance J.ToJSON a => J.ToJSON (ConfigurationJSON a) where
  toJSON = \case
    FromEnvJSON i -> J.object ["from_env" J..= i]
    FromYamlJSON j -> J.toJSON j

-- | Configuration inputs when they are a YAML array or an Env var whose value is
-- a comma-separated string
data ConfigurationInputs
  = FromYamls [Text]
  | FromEnvs Text
  deriving stock (Show, Eq, Generic)
  deriving (NFData, Hashable)

instance HasCodec ConfigurationInputs where
  codec =
    dimapCodec
      (either FromYamls FromEnvs)
      (\case FromYamls i -> Left i; FromEnvs i -> Right i)
      $ disjointEitherCodec
        (codec @[Text])
        fromEnvCodec

instance J.ToJSON ConfigurationInputs where
  toJSON = \case
    FromYamls i -> J.toJSON i
    FromEnvs i -> J.object ["from_env" J..= i]

instance J.FromJSON ConfigurationInputs where
  parseJSON = \case
    J.Object o -> FromEnvs <$> o J..: "from_env"
    s@(J.Array _) -> FromYamls <$> J.parseJSON s
    _ -> fail "one of array or object must be provided"

-- | Configuration input when the YAML value as well as the Env var have
-- singular values
data ConfigurationInput
  = FromYaml Text
  | FromEnv Text
  deriving stock (Show, Eq, Generic)
  deriving (NFData, Hashable)

instance HasCodec ConfigurationInput where
  codec =
    dimapCodec
      (either FromYaml FromEnv)
      (\case FromYaml i -> Left i; FromEnv i -> Right i)
      $ disjointEitherCodec fromYamls fromEnvCodec
    where
      fromYamls =
        parseAlternative
          (codec @Text)
          (tshow <$> codec @Scientific)

instance J.ToJSON ConfigurationInput where
  toJSON = \case
    FromYaml i -> J.toJSON i
    FromEnv i -> J.object ["from_env" J..= i]

instance J.FromJSON ConfigurationInput where
  parseJSON = \case
    J.Object o -> FromEnv <$> o J..: "from_env"
    s@(J.String _) -> FromYaml <$> J.parseJSON s
    (J.Number n) -> FromYaml <$> J.parseJSON (J.String (tshow n))
    _ -> fail "one of string or number or object must be provided"

data BigQueryConnSourceConfig = BigQueryConnSourceConfig
  { _cscServiceAccount :: ConfigurationJSON ServiceAccount,
    _cscDatasets :: ConfigurationInputs,
    _cscProjectId :: ConfigurationInput, -- this is part of service-account.json, but we put it here on purpose
    _cscGlobalSelectLimit :: Maybe ConfigurationInput,
    _cscRetryBaseDelay :: Maybe ConfigurationInput,
    _cscRetryLimit :: Maybe ConfigurationInput
  }
  deriving (Eq, Generic, NFData)

$(J.deriveJSON (J.aesonDrop 4 J.snakeCase) {J.omitNothingFields = True} ''BigQueryConnSourceConfig)

-- TODO: Write a proper codec, and use it to derive FromJSON and ToJSON
-- instances.
instance HasCodec BigQueryConnSourceConfig where
  codec =
    object "BigQueryConnSourceConfig" $
      BigQueryConnSourceConfig
        <$> requiredField' "service_account" .= _cscServiceAccount
        <*> requiredField' "datasets" .= _cscDatasets
        <*> requiredField' "project_id" .= _cscProjectId
        <*> optionalFieldOrNull' "global_select_limit" .= _cscGlobalSelectLimit
        <*> optionalFieldOrNull' "retry_base_delay" .= _cscRetryBaseDelay
        <*> optionalFieldOrNull' "retry_limit" .= _cscRetryLimit

deriving instance Show BigQueryConnSourceConfig

deriving instance Hashable BigQueryConnSourceConfig

instance Cacheable BigQueryConnSourceConfig where
  unchanged _ = (==)

data RetryOptions = RetryOptions
  { _retryBaseDelay :: Microseconds,
    _retryNumRetries :: Int
  }
  deriving (Eq)

data BigQueryConnection = BigQueryConnection
  { _bqServiceAccount :: ServiceAccount,
    _bqProjectId :: Text, -- this is part of service-account.json, but we put it here on purpose
    _bqRetryOptions :: Maybe RetryOptions,
    _bqAccessTokenMVar :: MVar (Maybe TokenResp)
  }
  deriving (Eq)

data BigQuerySourceConfig = BigQuerySourceConfig
  { _scConnection :: BigQueryConnection,
    _scDatasets :: [Text],
    _scGlobalSelectLimit :: Int.Int64
  }
  deriving (Eq)

instance Cacheable BigQuerySourceConfig where
  unchanged _ = (==)

instance J.ToJSON BigQuerySourceConfig where
  toJSON BigQuerySourceConfig {..} =
    J.object $
      [ "service_account" J..= _bqServiceAccount _scConnection,
        "datasets" J..= _scDatasets,
        "project_id" J..= _bqProjectId _scConnection,
        "global_select_limit" J..= _scGlobalSelectLimit
      ]
        <> case _bqRetryOptions _scConnection of
          Just RetryOptions {..} ->
            [ "base_delay" J..= diffTimeToMicroSeconds (microseconds _retryBaseDelay),
              "retry_limit" J..= _retryNumRetries
            ]
          Nothing -> []
