{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Postgres Connection Settings
--
-- This module contains types and combinators related to postgres connection,
-- pool, and replica related settings.
module Hasura.Backends.Postgres.Connection.Settings
  ( PostgresPoolSettings (..),
    PostgresSourceConnInfo (..),
    PostgresConnConfiguration (..),
    PGClientCerts (..),
    CertVar (..),
    CertData (..),
    SSLMode (..),
    DefaultPostgresPoolSettings (..),
    getDefaultPGPoolSettingIfNotExists,
    defaultPostgresPoolSettings,
    defaultPostgresExtensionsSchema,
    setPostgresPoolSettings,
    pccConnectionInfo,
    pccReadReplicas,
    pccExtensionsSchema,
    psciDatabaseUrl,
    psciPoolSettings,
    psciUsePreparedStatements,
    psciIsolationLevel,
    psciSslConfiguration,
  )
where

import Control.Lens (makeLenses)
import Data.Aeson
import Data.Aeson.Casing (aesonDrop)
import Data.Aeson.TH
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Char (toLower)
import Data.Semigroup (Max (..))
import Data.Text (unpack)
import Data.Text qualified as T
import Data.Time
import Data.Time.Clock.Compat ()
import Database.PG.Query qualified as Q
import Hasura.Base.Instances ()
import Hasura.Incremental (Cacheable (..))
import Hasura.Prelude
import Hasura.RQL.Types.Common (UrlConf (..))
import Hasura.SQL.Types (ExtensionsSchema (..))
import Hasura.Server.Utils (parseConnLifeTime, readIsoLevel)
import Test.QuickCheck.Instances.Semigroup ()
import Test.QuickCheck.Instances.Time ()

data PostgresPoolSettings = PostgresPoolSettings
  { _ppsMaxConnections :: Maybe Int,
    _ppsIdleTimeout :: Maybe Int,
    _ppsRetries :: Maybe Int,
    _ppsPoolTimeout :: Maybe NominalDiffTime,
    _ppsConnectionLifetime :: Maybe NominalDiffTime
  }
  deriving (Show, Eq, Generic)

instance Cacheable PostgresPoolSettings

instance Hashable PostgresPoolSettings

instance NFData PostgresPoolSettings

$(deriveToJSON hasuraJSON {omitNothingFields = True} ''PostgresPoolSettings)

instance FromJSON PostgresPoolSettings where
  parseJSON = withObject "PostgresPoolSettings" $ \o ->
    PostgresPoolSettings
      <$> o .:? "max_connections"
      <*> o .:? "idle_timeout"
      <*> o .:? "retries"
      <*> o .:? "pool_timeout"
      <*> ((o .:? "connection_lifetime") <&> parseConnLifeTime)

data DefaultPostgresPoolSettings = DefaultPostgresPoolSettings
  { _dppsMaxConnections :: Int,
    _dppsIdleTimeout :: Int,
    _dppsRetries :: Int,
    _dppsConnectionLifetime :: Maybe NominalDiffTime
  }
  deriving (Show, Eq)

defaultPostgresPoolSettings :: DefaultPostgresPoolSettings
defaultPostgresPoolSettings = DefaultPostgresPoolSettings 50 180 1 (Just 600)

-- Use this when you want to set only few of the PG Pool settings.
-- The values which are not set will use the default values.
setPostgresPoolSettings :: PostgresPoolSettings
setPostgresPoolSettings =
  PostgresPoolSettings
    { _ppsMaxConnections = (Just $ _dppsMaxConnections defaultPostgresPoolSettings),
      _ppsIdleTimeout = (Just $ _dppsIdleTimeout defaultPostgresPoolSettings),
      _ppsRetries = (Just $ _dppsRetries defaultPostgresPoolSettings),
      _ppsPoolTimeout = Nothing, -- @Nothing@ is the default value of the pool timeout
      _ppsConnectionLifetime = _dppsConnectionLifetime defaultPostgresPoolSettings
    }

-- PG Pool Settings are not given by the user, set defaults
getDefaultPGPoolSettingIfNotExists :: Maybe PostgresPoolSettings -> DefaultPostgresPoolSettings -> (Int, Int, Int)
getDefaultPGPoolSettingIfNotExists connSettings defaultPgPoolSettings =
  case connSettings of
    -- Atleast one of the postgres pool settings is set, then set default values to other settings
    Just connSettings' ->
      (maxConnections connSettings', idleTimeout connSettings', retries connSettings')
    -- No PG Pool settings provided by user, set default values for all
    Nothing -> (defMaxConnections, defIdleTimeout, defRetries)
  where
    defMaxConnections = _dppsMaxConnections defaultPgPoolSettings
    defIdleTimeout = _dppsIdleTimeout defaultPgPoolSettings
    defRetries = _dppsRetries defaultPgPoolSettings

    maxConnections = fromMaybe defMaxConnections . _ppsMaxConnections
    idleTimeout = fromMaybe defIdleTimeout . _ppsIdleTimeout
    retries = fromMaybe defRetries . _ppsRetries

data SSLMode
  = Disable
  | Allow
  | Prefer
  | Require
  | VerifyCA
  | VerifyFull
  deriving (Eq, Ord, Generic, Enum, Bounded)

instance Cacheable SSLMode

instance Hashable SSLMode

instance NFData SSLMode

instance Show SSLMode where
  show = \case
    Disable -> "disable"
    Allow -> "allow"
    Prefer -> "prefer"
    Require -> "require"
    VerifyCA -> "verify-ca"
    VerifyFull -> "verify-full"

deriving via (Max SSLMode) instance Semigroup SSLMode

instance FromJSON SSLMode where
  parseJSON = withText "SSLMode" $ \case
    "disable" -> pure Disable
    "allow" -> pure Allow
    "prefer" -> pure Prefer
    "require" -> pure Require
    "verify-ca" -> pure VerifyCA
    "verify-full" -> pure VerifyFull
    err -> fail $ "Invalid SSL Mode " <> unpack err

newtype CertVar
  = CertVar String
  deriving (Show, Eq, Generic)

instance Cacheable CertVar

instance Hashable CertVar

instance NFData CertVar

instance ToJSON CertVar where
  toJSON (CertVar var) = (object ["from_env" .= var])

instance FromJSON CertVar where
  parseJSON = withObject "CertVar" (\o -> CertVar <$> o .: "from_env")

newtype CertData = CertData {unCert :: Text}
  deriving (Show, Eq, Generic)

instance ToJSON CertData where
  toJSON = String . unCert

data PGClientCerts p a = PGClientCerts
  { pgcSslCert :: Maybe a,
    pgcSslKey :: Maybe a,
    pgcSslRootCert :: Maybe a,
    pgcSslMode :: SSLMode,
    pgcSslPassword :: Maybe p
  }
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

$(deriveFromJSON (aesonDrop 3 (fmap toLower)) ''PGClientCerts)
$(deriveToJSON (aesonDrop 3 (fmap toLower)) {omitNothingFields = True} ''PGClientCerts)

instance Bifunctor PGClientCerts where
  bimap f g oldCerts@(PGClientCerts {pgcSslPassword}) =
    let certs = oldCerts {pgcSslPassword = f <$> pgcSslPassword}
     in g <$> certs

instance Bifoldable PGClientCerts where
  bifoldMap f g PGClientCerts {..} =
    let gs = foldMap (foldMap g) [pgcSslCert, pgcSslKey, pgcSslRootCert]
        fs = foldMap f pgcSslPassword
     in gs <> fs

instance Bitraversable PGClientCerts where
  bitraverse f g PGClientCerts {..} =
    PGClientCerts
      <$> traverse g pgcSslCert
      <*> traverse g pgcSslKey
      <*> traverse g pgcSslRootCert
      <*> pure pgcSslMode
      <*> traverse f pgcSslPassword

instance (Cacheable p, Cacheable a) => Cacheable (PGClientCerts p a)

instance (Hashable p, Hashable a) => Hashable (PGClientCerts p a)

instance (NFData p, NFData a) => NFData (PGClientCerts p a)

instance ToJSON SSLMode where
  toJSON = String . tshow

deriving instance Generic Q.TxIsolation

instance Cacheable Q.TxIsolation

instance NFData Q.TxIsolation

instance Hashable Q.TxIsolation

instance FromJSON Q.TxIsolation where
  parseJSON = withText "Q.TxIsolation" $ \t ->
    onLeft (readIsoLevel $ T.unpack t) fail

instance ToJSON Q.TxIsolation where
  toJSON Q.ReadCommitted = "read-committed"
  toJSON Q.RepeatableRead = "repeatable-read"
  toJSON Q.Serializable = "serializable"

data PostgresSourceConnInfo = PostgresSourceConnInfo
  { _psciDatabaseUrl :: UrlConf,
    _psciPoolSettings :: Maybe PostgresPoolSettings,
    _psciUsePreparedStatements :: Bool,
    _psciIsolationLevel :: Q.TxIsolation,
    _psciSslConfiguration :: Maybe (PGClientCerts CertVar CertVar)
  }
  deriving (Show, Eq, Generic)

instance Cacheable PostgresSourceConnInfo

instance Hashable PostgresSourceConnInfo

instance NFData PostgresSourceConnInfo

$(deriveToJSON hasuraJSON {omitNothingFields = True} ''PostgresSourceConnInfo)
$(makeLenses ''PostgresSourceConnInfo)

instance FromJSON PostgresSourceConnInfo where
  parseJSON = withObject "PostgresSourceConnInfo" $ \o ->
    PostgresSourceConnInfo
      <$> o .: "database_url"
      <*> o .:? "pool_settings"
      <*> o .:? "use_prepared_statements" .!= False -- By default, preparing statements is OFF for postgres source
      <*> o .:? "isolation_level" .!= Q.ReadCommitted
      <*> o .:? "ssl_configuration"

defaultPostgresExtensionsSchema :: ExtensionsSchema
defaultPostgresExtensionsSchema = ExtensionsSchema "public"

data PostgresConnConfiguration = PostgresConnConfiguration
  { _pccConnectionInfo :: PostgresSourceConnInfo,
    _pccReadReplicas :: Maybe (NonEmpty PostgresSourceConnInfo),
    _pccExtensionsSchema :: ExtensionsSchema
  }
  deriving (Show, Eq, Generic)

instance Cacheable PostgresConnConfiguration

instance Hashable PostgresConnConfiguration

instance NFData PostgresConnConfiguration

instance FromJSON PostgresConnConfiguration where
  parseJSON = withObject "PostgresConnConfiguration" $ \o ->
    PostgresConnConfiguration
      <$> o .: "connection_info"
      <*> o .:? "read_replicas"
      <*> o .:? "extensions_schema" .!= defaultPostgresExtensionsSchema

instance ToJSON PostgresConnConfiguration where
  toJSON PostgresConnConfiguration {..} =
    object $
      ["connection_info" .= _pccConnectionInfo]
        <> maybe mempty (\readReplicas -> ["read_replicas" .= readReplicas]) _pccReadReplicas
        <> bool mempty (["extensions_schema" .= _pccExtensionsSchema]) (_pccExtensionsSchema /= defaultPostgresExtensionsSchema)

$(makeLenses ''PostgresConnConfiguration)
