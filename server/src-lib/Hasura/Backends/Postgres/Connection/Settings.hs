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

import Autodocodec hiding (object, (.=))
import Autodocodec qualified as AC
import Control.Lens (makeLenses)
import Data.Aeson
import Data.Aeson.Casing (aesonDrop)
import Data.Aeson.TH
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Char (toLower)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Semigroup (Max (..))
import Data.Text (unpack)
import Data.Text qualified as T
import Data.Time
import Data.Time.Clock.Compat ()
import Database.PG.Query qualified as PG
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
    _ppsTotalMaxConnections :: Maybe Int,
    _ppsIdleTimeout :: Maybe Int,
    _ppsRetries :: Maybe Int,
    _ppsPoolTimeout :: Maybe NominalDiffTime,
    _ppsConnectionLifetime :: Maybe NominalDiffTime
  }
  deriving (Show, Eq, Generic)

instance Cacheable PostgresPoolSettings

instance Hashable PostgresPoolSettings

instance NFData PostgresPoolSettings

instance HasCodec PostgresPoolSettings where
  codec =
    CommentCodec "https://hasura.io/docs/latest/graphql/core/api-reference/syntax-defs.html#pgpoolsettings" $
      AC.object "PostgresPoolSettings" $
        PostgresPoolSettings
          <$> optionalFieldOrNull "max_connections" maxConnectionsDoc .== _ppsMaxConnections
          <*> optionalFieldOrNull "total_max_connections" totalMaxConnectionsDoc .== _ppsTotalMaxConnections
          <*> optionalFieldOrNull "idle_timeout" idleTimeoutDoc .== _ppsIdleTimeout
          <*> optionalFieldOrNull "retries" retriesDoc .== _ppsRetries
          <*> optionalFieldOrNull "pool_timeout" poolTimeoutDoc .== _ppsPoolTimeout
          <*> parseConnLifeTime `rmapCodec` optionalFieldOrNull "connection_lifetime" connectionLifetimeDoc .== _ppsConnectionLifetime
    where
      maxConnectionsDoc = "Maximum number of connections to be kept in the pool (default: 50)"
      totalMaxConnectionsDoc = "Total maximum number of connections across all instances (cloud only, default: null)"
      idleTimeoutDoc = "The idle timeout (in seconds) per connection (default: 180)"
      retriesDoc = "Number of retries to perform (default: 1)"
      poolTimeoutDoc = "Maximum time to wait while acquiring a Postgres connection from the pool, in seconds (default: forever)"
      connectionLifetimeDoc =
        T.unwords
          [ "Time from connection creation after which the connection should be",
            "destroyed and a new one created. A value of 0 indicates we should",
            "never destroy an active connection. If 0 is passed, memory from large",
            "query results may not be reclaimed. (default: 600 sec)"
          ]
      (.==) = (AC..=)

$(deriveToJSON hasuraJSON {omitNothingFields = True} ''PostgresPoolSettings)

instance FromJSON PostgresPoolSettings where
  parseJSON = withObject "PostgresPoolSettings" $ \o ->
    PostgresPoolSettings
      <$> o .:? "max_connections"
      <*> o .:? "total_max_connections"
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
      _ppsTotalMaxConnections = Nothing,
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

instance HasCodec SSLMode where
  codec =
    named "SSLMode" $
      stringConstCodec $
        NonEmpty.fromList $
          (\m -> (m, tshow m)) <$> [minBound ..]

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

instance HasCodec CertVar where
  codec =
    AC.object "CertVar" $ CertVar <$> requiredField' "from_env" .== unCertVar
    where
      unCertVar (CertVar t) = t
      (.==) = (AC..=)

instance ToJSON CertVar where
  toJSON (CertVar var) = (object ["from_env" .= var])

instance FromJSON CertVar where
  parseJSON = withObject "CertVar" (\o -> CertVar <$> o .: "from_env")

newtype CertData = CertData {unCert :: Text}
  deriving (Show, Eq, Generic)

instance HasCodec CertData where
  codec = dimapCodec CertData unCert textCodec

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

instance (HasCodec p, HasCodec a) => HasCodec (PGClientCerts p a) where
  codec =
    CommentCodec "https://hasura.io/docs/latest/graphql/core/api-reference/syntax-defs.html#pgcertsettings" $
      AC.object "PGClientCerts" $
        PGClientCerts
          <$> optionalFieldOrNull "sslcert" sslcertDoc .== pgcSslCert
          <*> optionalFieldOrNull "sslkey" sslkeyDoc .== pgcSslKey
          <*> optionalFieldOrNull "sslrootcert" sslrootcertDoc .== pgcSslRootCert
          <*> requiredField "sslmode" sslmodeDoc .== pgcSslMode
          <*> optionalFieldOrNull "sslpassword" sslpasswordDoc .== pgcSslPassword
    where
      sslcertDoc = "Environment variable which stores the client certificate."
      sslkeyDoc = "Environment variable which stores the client private key."
      sslrootcertDoc = "Environment variable which stores trusted certificate authorities."
      sslmodeDoc = "The SSL connection mode. See the libpq ssl support docs <https://www.postgresql.org/docs/9.1/libpq-ssl.html> for more details."
      sslpasswordDoc = "Password in the case where the sslkey is encrypted."
      (.==) = (AC..=)

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

deriving instance Generic PG.TxIsolation

instance Cacheable PG.TxIsolation

instance NFData PG.TxIsolation

instance Hashable PG.TxIsolation

instance HasCodec PG.TxIsolation where
  codec =
    named "TxIsolation" $
      stringConstCodec $
        NonEmpty.fromList $
          [ (PG.ReadCommitted, "read-committed"),
            (PG.RepeatableRead, "repeatable-read"),
            (PG.Serializable, "serializable")
          ]

instance FromJSON PG.TxIsolation where
  parseJSON = withText "Q.TxIsolation" $ \t ->
    onLeft (readIsoLevel $ T.unpack t) fail

instance ToJSON PG.TxIsolation where
  toJSON PG.ReadCommitted = "read-committed"
  toJSON PG.RepeatableRead = "repeatable-read"
  toJSON PG.Serializable = "serializable"

data PostgresSourceConnInfo = PostgresSourceConnInfo
  { _psciDatabaseUrl :: UrlConf,
    _psciPoolSettings :: Maybe PostgresPoolSettings,
    _psciUsePreparedStatements :: Bool,
    _psciIsolationLevel :: PG.TxIsolation,
    _psciSslConfiguration :: Maybe (PGClientCerts CertVar CertVar)
  }
  deriving (Show, Eq, Generic)

instance Cacheable PostgresSourceConnInfo

instance Hashable PostgresSourceConnInfo

instance NFData PostgresSourceConnInfo

instance HasCodec PostgresSourceConnInfo where
  codec =
    CommentCodec "https://hasura.io/docs/latest/graphql/core/api-reference/syntax-defs.html#pgsourceconnectioninfo" $
      AC.object "PostgresSourceConnInfo" $
        PostgresSourceConnInfo
          <$> requiredField "database_url" databaseUrlDoc .== _psciDatabaseUrl
          <*> optionalFieldOrNull "pool_settings" poolSettingsDoc .== _psciPoolSettings
          <*> optionalFieldWithDefault "use_prepared_statements" False usePreparedStatementsDoc .== _psciUsePreparedStatements
          <*> optionalFieldWithDefault "isolation_level" PG.ReadCommitted isolationLevelDoc .== _psciIsolationLevel
          <*> optionalFieldOrNull "ssl_configuration" sslConfigurationDoc .== _psciSslConfiguration
    where
      databaseUrlDoc = "The database connection URL as a string, as an environment variable, or as connection parameters."
      poolSettingsDoc = "Connection pool settings"
      usePreparedStatementsDoc =
        T.unwords
          [ "If set to true the server prepares statement before executing on the",
            "source database (default: false). For more details, refer to the",
            "Postgres docs"
          ]
      isolationLevelDoc =
        T.unwords
          [ "The transaction isolation level in which the queries made to the",
            "source will be run with (default: read-committed)."
          ]
      sslConfigurationDoc = "The client SSL certificate settings for the database (Only available in Cloud)."
      (.==) = (AC..=)

$(deriveToJSON hasuraJSON {omitNothingFields = True} ''PostgresSourceConnInfo)
$(makeLenses ''PostgresSourceConnInfo)

instance FromJSON PostgresSourceConnInfo where
  parseJSON = withObject "PostgresSourceConnInfo" $ \o ->
    PostgresSourceConnInfo
      <$> o .: "database_url"
      <*> o .:? "pool_settings"
      <*> o .:? "use_prepared_statements" .!= False -- By default, preparing statements is OFF for postgres source
      <*> o .:? "isolation_level" .!= PG.ReadCommitted
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

instance HasCodec PostgresConnConfiguration where
  codec =
    CommentCodec "https://hasura.io/docs/latest/graphql/core/api-reference/syntax-defs.html#pgconfiguration" $
      AC.object "PostgresConnConfiguration" $
        PostgresConnConfiguration
          <$> requiredField "connection_info" connectionInfoDoc .== _pccConnectionInfo
          <*> optionalFieldOrNull "read_replicas" readReplicasDoc .== _pccReadReplicas
          <*> optionalFieldWithOmittedDefault "extensions_schema" defaultPostgresExtensionsSchema extensionsSchemaDoc .== _pccExtensionsSchema
    where
      connectionInfoDoc = "Connection parameters for the source"
      readReplicasDoc = "Optional list of read replica configuration (supported only in cloud/enterprise versions)"
      extensionsSchemaDoc = "Name of the schema where the graphql-engine will install database extensions (default: public)"
      (.==) = (AC..=)

$(makeLenses ''PostgresConnConfiguration)
