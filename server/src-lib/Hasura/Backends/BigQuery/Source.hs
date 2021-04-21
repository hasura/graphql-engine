{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoGeneralisedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.BigQuery.Source where


import           Control.Concurrent.MVar
import           Control.DeepSeq
import qualified Crypto.PubKey.RSA.Types as Cry
import qualified Data.Aeson as J
import qualified Data.Aeson.Casing as J
import qualified Data.Aeson.TH as J
import qualified Data.ByteString.Lazy as BL
import           Data.Hashable (hashWithSalt)
import qualified Data.HashMap.Strict as HM
import           Data.Text (pack)
import qualified Data.Text.Encoding as TE
import qualified Data.X509 as X509
import qualified Data.X509.Memory as X509
import           Hasura.Incremental (Cacheable (..))
import           Hasura.Prelude
import qualified System.Environment as SE (getEnv)
import           System.FilePath (isRelative)
import           System.IO.Unsafe (unsafePerformIO)


data PKey = PKey
  { unPKey :: Cry.PrivateKey
  , originalBS :: Text
  }
  deriving (Show, Eq, Data, Generic, NFData, Hashable)
deriving instance Generic Cry.PrivateKey -- orphan
deriving instance Generic Cry.PublicKey -- orphan
deriving instance J.ToJSON Cry.PrivateKey -- orphan
deriving instance J.ToJSON Cry.PublicKey -- orphan
deriving instance Hashable Cry.PrivateKey -- orphan
deriving instance Hashable Cry.PublicKey -- orphan
instance Arbitrary Cry.PrivateKey where -- orphan
  arbitrary = genericArbitrary
instance Arbitrary Cry.PublicKey where -- orphan
  arbitrary = genericArbitrary
instance Arbitrary PKey where
  arbitrary = genericArbitrary
instance J.FromJSON PKey where
  parseJSON = J.withText "private_key" $ \k ->
    case X509.readKeyFileFromMemory $ TE.encodeUtf8 k of
      [X509.PrivKeyRSA k'] -> return $ PKey k' k
      _                    -> fail "unable to parse private key"
instance J.ToJSON PKey where
  toJSON PKey{..} = J.toJSON originalBS


newtype GoogleAccessToken
  = GoogleAccessToken Text
  deriving (Show, Eq, J.FromJSON, J.ToJSON, Hashable, Generic, Data, NFData)
instance Arbitrary GoogleAccessToken where
  arbitrary = genericArbitrary


data TokenResp
  = TokenResp
  { _trAccessToken :: !GoogleAccessToken
  , _trExpiresAt   :: !Integer -- Number of seconds until expiry from `now`, but we add `now` seconds to this for easy tracking
  } deriving (Eq, Show, Data, NFData, Generic, Hashable)
instance J.FromJSON TokenResp where
  parseJSON = J.withObject "TokenResp" $ \o -> TokenResp
    <$> o J..: "access_token"
    <*> o J..: "expires_in"
instance Arbitrary TokenResp where
  arbitrary = genericArbitrary


dummyTokenResp :: TokenResp
dummyTokenResp = TokenResp (GoogleAccessToken "DUMMY") 0


data ServiceAccount
  = ServiceAccount
  { _saClientEmail :: !Text
  , _saPrivateKey  :: !PKey
  , _saProjectId :: !Text
  } deriving (Eq, Show, Data, NFData, Generic, Hashable)
$(J.deriveJSON (J.aesonDrop 3 J.snakeCase){J.omitNothingFields=False} ''ServiceAccount)
instance Arbitrary ServiceAccount where
  arbitrary = genericArbitrary


data ConfigurationJSON a
  = FromEnvJSON Text
  | FromYamlJSON a
  deriving stock (Show, Eq, Generic)
  deriving (NFData, Hashable)
instance Arbitrary a => Arbitrary (ConfigurationJSON a) where
  arbitrary = genericArbitrary
instance J.FromJSON a => J.FromJSON (ConfigurationJSON a) where
  parseJSON = \case
    J.Object o | Just (J.String text) <- HM.lookup "from_env" o -> pure (FromEnvJSON text)
    J.String s -> case J.eitherDecode . BL.fromStrict . TE.encodeUtf8 $ s of
      Left {} -> fail "error parsing configuration json"
      Right sa -> pure sa
    j -> fmap FromYamlJSON (J.parseJSON j)
instance J.ToJSON a => J.ToJSON (ConfigurationJSON a) where
  toJSON = \case
    FromEnvJSON i -> J.object ["from_env" J..= i]
    FromYamlJSON j -> J.toJSON j


-- | Configuration inputs when they are a YAML array or an Env var whos value is
-- a comma-separated string
data ConfigurationInputs
  = FromYamls ![Text]
  | FromEnvs !Text
  deriving stock (Show, Eq, Generic)
  deriving (NFData, Hashable)
instance Arbitrary ConfigurationInputs where
  arbitrary = genericArbitrary
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
-- singlular values
data ConfigurationInput
  = FromYaml !Text
  | FromEnv !Text
  deriving stock (Show, Eq, Generic)
  deriving (NFData, Hashable)
instance Arbitrary ConfigurationInput where
  arbitrary = genericArbitrary
instance J.ToJSON ConfigurationInput where
  toJSON = \case
    FromYaml i -> J.toJSON i
    FromEnv i -> J.object ["from_env" J..= i]
instance J.FromJSON ConfigurationInput where
  parseJSON = \case
    J.Object o -> FromEnv <$> o J..: "from_env"
    s@(J.String _) -> FromYaml <$> J.parseJSON s
    _ -> fail "one of string or object must be provided"


data BigQueryConnSourceConfig
  = BigQueryConnSourceConfig
  { _cscServiceAccount :: !(ConfigurationJSON ServiceAccount)
  , _cscDatasets :: !ConfigurationInputs
  , _cscProjectId :: !ConfigurationInput -- this is part of service-account.json, but we put it here on purpose
  } deriving (Eq, Generic, NFData)
$(J.deriveJSON (J.aesonDrop 4 J.snakeCase){J.omitNothingFields=True} ''BigQueryConnSourceConfig)
deriving instance Show BigQueryConnSourceConfig
deriving instance Hashable BigQueryConnSourceConfig
instance Arbitrary BigQueryConnSourceConfig where
  arbitrary = genericArbitrary
instance Cacheable BigQueryConnSourceConfig where
  unchanged _ = (==)


data BigQuerySourceConfig
  = BigQuerySourceConfig
  { _scServiceAccount :: !ServiceAccount
  , _scDatasets :: ![Text]
  , _scProjectId :: !Text -- this is part of service-account.json, but we put it here on purpose
  , _scAccessTokenMVar :: !(MVar (Maybe TokenResp))
  } deriving (Eq, Generic, NFData)
$(J.deriveJSON (J.aesonDrop 3 J.snakeCase){J.omitNothingFields=True} ''BigQuerySourceConfig)
deriving instance Show BigQuerySourceConfig
deriving instance Hashable BigQuerySourceConfig
instance Arbitrary BigQuerySourceConfig where
  arbitrary = genericArbitrary
instance Cacheable BigQuerySourceConfig where
  unchanged _ = (==)

instance Show (MVar (Maybe TokenResp)) where
  -- show = maybe "NOTHING" (const "_REDACTED_") . unsafePerformIO . readIORef
  show = (const "_REDACTED_")
instance J.FromJSON (MVar (Maybe TokenResp)) where
  parseJSON _ = pure $ unsafePerformIO $ newEmptyMVar
instance J.ToJSON (MVar (Maybe TokenResp)) where
  toJSON _ = J.String "_REDACTED_"
instance Hashable (MVar (Maybe TokenResp)) where
  hashWithSalt i r = hashWithSalt i (unsafePerformIO $ readMVar r)
instance Arbitrary (MVar (Maybe TokenResp)) where
  arbitrary = genericArbitrary @(Maybe TokenResp) <&> (unsafePerformIO . newMVar)


-- | for testing
getBigQuerySourceConfigEnv :: IO BigQuerySourceConfig
getBigQuerySourceConfigEnv = do
  _scServiceAccountFilePath <- getEnvUnline _safpEnvKey
  if isRelative _scServiceAccountFilePath
    then error $ _safpEnvKey <> " needs to be an absolute file-path"
    else do
      _scDatasets <- pure . pack <$> getEnvUnline "HASURA_BIGQUERY_DATASET"
      _scProjectId <- pack <$> getEnvUnline "HASURA_BIGQUERY_PROJECT_ID"
      _scServiceAccount :: ServiceAccount <- either error id . J.eitherDecode' <$> BL.readFile _scServiceAccountFilePath
      _scAccessTokenMVar <- newMVar Nothing
      pure BigQuerySourceConfig {..}
  where
    _safpEnvKey = "HASURA_BIGQUERY_SERVICE_ACCOUNT_FILE_PATH"
    getEnvUnline key = fmap (concat . take 1 . lines) (SE.getEnv key)
