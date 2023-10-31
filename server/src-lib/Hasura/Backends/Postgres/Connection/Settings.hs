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
    ConnectionTemplate (..),
    PostgresConnectionSetMemberName (..),
    PostgresConnectionSet (..),
    PostgresConnectionSetMember (..),
    KritiTemplate (..),
    getDefaultPGPoolSettingIfNotExists,
    defaultPostgresPoolSettings,
    defaultPostgresExtensionsSchema,
    setPostgresPoolSettings,
  )
where

import Autodocodec hiding (object, (.=))
import Autodocodec qualified as AC
import Data.Aeson
import Data.Aeson.Casing (aesonDrop)
import Data.Aeson.Extended (mapWithJSONPath)
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Char (toLower)
import Data.HashMap.Strict.NonEmpty qualified as NEMap
import Data.Hashable (hashWithSalt)
import Data.List.Extended qualified as L (uniques)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Semigroup (Max (..))
import Data.Text (unpack)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Extended (ToTxt (toTxt), dquote, dquoteList)
import Data.Text.NonEmpty
import Data.Time
import Data.Time.Clock.Compat ()
import Database.PG.Query qualified as PG
import Hasura.Base.Instances ()
import Hasura.Prelude
import Hasura.RQL.Types.Common (UrlConf (..))
import Hasura.SQL.Types (ExtensionsSchema (..))
import Hasura.Server.Utils (parseConnLifeTime, readIsoLevel)
import Kriti qualified
import Kriti.Error qualified as Kriti
import Kriti.Parser qualified as Kriti
import Prettyprinter qualified as PP
import Prettyprinter.Render.Text qualified as PP
import Test.QuickCheck.Instances.Semigroup ()
import Test.QuickCheck.Instances.Time ()

data PostgresPoolSettings = PostgresPoolSettings
  { ppsMaxConnections :: Maybe Int,
    ppsTotalMaxConnections :: Maybe Int,
    ppsIdleTimeout :: Maybe Int,
    ppsRetries :: Maybe Int,
    ppsPoolTimeout :: Maybe NominalDiffTime,
    ppsConnectionLifetime :: Maybe NominalDiffTime
  }
  deriving (Show, Eq, Generic)

instance Hashable PostgresPoolSettings

instance NFData PostgresPoolSettings

instance HasCodec PostgresPoolSettings where
  codec =
    CommentCodec "https://hasura.io/docs/latest/graphql/core/api-reference/syntax-defs.html#pgpoolsettings"
      $ AC.object "PostgresPoolSettings"
      $ PostgresPoolSettings
      <$> optionalFieldOrNull "max_connections" maxConnectionsDoc
      .== ppsMaxConnections
        <*> optionalFieldOrNull "total_max_connections" totalMaxConnectionsDoc
      .== ppsTotalMaxConnections
        <*> optionalFieldOrNull "idle_timeout" idleTimeoutDoc
      .== ppsIdleTimeout
        <*> optionalFieldOrNull "retries" retriesDoc
      .== ppsRetries
        <*> optionalFieldOrNull "pool_timeout" poolTimeoutDoc
      .== ppsPoolTimeout
        <*> (parseConnLifeTime `rmapCodec` optionalFieldOrNull "connection_lifetime" connectionLifetimeDoc)
      .== ppsConnectionLifetime
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
      infix 8 .==
      (.==) = (AC..=)

instance ToJSON PostgresPoolSettings where
  toJSON = genericToJSON hasuraJSON {omitNothingFields = True}
  toEncoding = genericToEncoding hasuraJSON {omitNothingFields = True}

instance FromJSON PostgresPoolSettings where
  parseJSON = withObject "PostgresPoolSettings" $ \o ->
    PostgresPoolSettings
      <$> o
      .:? "max_connections"
      <*> o
      .:? "total_max_connections"
      <*> o
      .:? "idle_timeout"
      <*> o
      .:? "retries"
      <*> o
      .:? "pool_timeout"
      <*> ((o .:? "connection_lifetime") <&> parseConnLifeTime)

data DefaultPostgresPoolSettings = DefaultPostgresPoolSettings
  { dppsMaxConnections :: Int,
    dppsIdleTimeout :: Int,
    dppsRetries :: Int,
    dppsConnectionLifetime :: Maybe NominalDiffTime
  }
  deriving (Show, Eq)

defaultPostgresPoolSettings :: DefaultPostgresPoolSettings
defaultPostgresPoolSettings = DefaultPostgresPoolSettings 50 180 1 (Just 600)

-- Use this when you want to set only few of the PG Pool settings.
-- The values which are not set will use the default values.
setPostgresPoolSettings :: PostgresPoolSettings
setPostgresPoolSettings =
  PostgresPoolSettings
    { ppsMaxConnections = (Just $ dppsMaxConnections defaultPostgresPoolSettings),
      ppsTotalMaxConnections = Nothing,
      ppsIdleTimeout = (Just $ dppsIdleTimeout defaultPostgresPoolSettings),
      ppsRetries = (Just $ dppsRetries defaultPostgresPoolSettings),
      ppsPoolTimeout = Nothing, -- @Nothing@ is the default value of the pool timeout
      ppsConnectionLifetime = dppsConnectionLifetime defaultPostgresPoolSettings
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
    defMaxConnections = dppsMaxConnections defaultPgPoolSettings
    defIdleTimeout = dppsIdleTimeout defaultPgPoolSettings
    defRetries = dppsRetries defaultPgPoolSettings

    maxConnections = fromMaybe defMaxConnections . ppsMaxConnections
    idleTimeout = fromMaybe defIdleTimeout . ppsIdleTimeout
    retries = fromMaybe defRetries . ppsRetries

data SSLMode
  = Disable
  | Allow
  | Prefer
  | Require
  | VerifyCA
  | VerifyFull
  deriving (Eq, Ord, Generic, Enum, Bounded)

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
    named "SSLMode"
      $ stringConstCodec
      $ NonEmpty.fromList
      $ (\m -> (m, tshow m))
      <$> [minBound ..]

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

instance Hashable CertVar

instance NFData CertVar

instance HasCodec CertVar where
  codec =
    AC.object "CertVar" $ CertVar <$> requiredField' "from_env" .== unCertVar
    where
      unCertVar (CertVar t) = t
      infix 8 .==
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
    CommentCodec "https://hasura.io/docs/latest/graphql/core/api-reference/syntax-defs.html#pgcertsettings"
      $ AC.object "PGClientCerts"
      $ PGClientCerts
      <$> optionalFieldOrNull "sslcert" sslcertDoc
      .== pgcSslCert
        <*> optionalFieldOrNull "sslkey" sslkeyDoc
      .== pgcSslKey
        <*> optionalFieldOrNull "sslrootcert" sslrootcertDoc
      .== pgcSslRootCert
        <*> requiredField "sslmode" sslmodeDoc
      .== pgcSslMode
        <*> optionalFieldOrNull "sslpassword" sslpasswordDoc
      .== pgcSslPassword
    where
      sslcertDoc = "Environment variable which stores the client certificate."
      sslkeyDoc = "Environment variable which stores the client private key."
      sslrootcertDoc = "Environment variable which stores trusted certificate authorities."
      sslmodeDoc = "The SSL connection mode. See the libpq ssl support docs <https://www.postgresql.org/docs/9.1/libpq-ssl.html> for more details."
      sslpasswordDoc = "Password in the case where the sslkey is encrypted."
      infix 8 .==
      (.==) = (AC..=)

instance (FromJSON p, FromJSON a) => FromJSON (PGClientCerts p a) where
  parseJSON = genericParseJSON (aesonDrop 3 (fmap toLower))

instance (ToJSON p, ToJSON a) => ToJSON (PGClientCerts p a) where
  toJSON = genericToJSON (aesonDrop 3 (fmap toLower)) {omitNothingFields = True}
  toEncoding = genericToEncoding (aesonDrop 3 (fmap toLower)) {omitNothingFields = True}

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

instance (Hashable p, Hashable a) => Hashable (PGClientCerts p a)

instance (NFData p, NFData a) => NFData (PGClientCerts p a)

instance ToJSON SSLMode where
  toJSON = String . tshow

deriving instance Generic PG.TxIsolation

instance NFData PG.TxIsolation

instance Hashable PG.TxIsolation

instance HasCodec PG.TxIsolation where
  codec =
    named "TxIsolation"
      $ stringConstCodec
      $ NonEmpty.fromList
      $ [ (PG.ReadCommitted, "read-committed"),
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
  { psciDatabaseUrl :: UrlConf,
    psciPoolSettings :: Maybe PostgresPoolSettings,
    psciUsePreparedStatements :: Bool,
    psciIsolationLevel :: PG.TxIsolation,
    psciSslConfiguration :: Maybe (PGClientCerts CertVar CertVar)
  }
  deriving (Show, Eq, Generic)

instance Hashable PostgresSourceConnInfo

instance NFData PostgresSourceConnInfo

instance HasCodec PostgresSourceConnInfo where
  codec =
    CommentCodec "https://hasura.io/docs/latest/graphql/core/api-reference/syntax-defs.html#pgsourceconnectioninfo"
      $ AC.object "PostgresSourceConnInfo"
      $ PostgresSourceConnInfo
      <$> requiredField "database_url" databaseUrlDoc
      .== psciDatabaseUrl
        <*> optionalFieldOrNull "pool_settings" poolSettingsDoc
      .== psciPoolSettings
        <*> optionalFieldWithDefault "use_prepared_statements" False usePreparedStatementsDoc
      .== psciUsePreparedStatements
        <*> optionalFieldWithDefault "isolation_level" PG.ReadCommitted isolationLevelDoc
      .== psciIsolationLevel
        <*> optionalFieldOrNull "ssl_configuration" sslConfigurationDoc
      .== psciSslConfiguration
    where
      databaseUrlDoc = "The database connection URL as a string, from an environment variable, as connection parameters, or dynamically read from a file at connect time."
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
      infix 8 .==
      (.==) = (AC..=)

instance ToJSON PostgresSourceConnInfo where
  toJSON = genericToJSON hasuraJSON {omitNothingFields = True}
  toEncoding = genericToEncoding hasuraJSON {omitNothingFields = True}

instance FromJSON PostgresSourceConnInfo where
  parseJSON = withObject "PostgresSourceConnInfo" $ \o ->
    PostgresSourceConnInfo
      <$> o
      .: "database_url"
      <*> o
      .:? "pool_settings"
      <*> o
      .:? "use_prepared_statements"
      .!= False -- By default, preparing statements is OFF for postgres source
      <*> o
      .:? "isolation_level"
      .!= PG.ReadCommitted
      <*> o
      .:? "ssl_configuration"

defaultPostgresExtensionsSchema :: ExtensionsSchema
defaultPostgresExtensionsSchema = ExtensionsSchema "public"

-- | `kriti-lang` template.
data KritiTemplate = KritiTemplate
  { -- TODO: There is redundency here, we should remove the templateSrc field once the renderPretty bug is resolved
    -- (https://github.com/hasura/kriti-lang/issues/77)

    -- | Raw kriti template
    ktSource :: Text,
    -- | Parsed kriti template
    ktParsedAST :: Kriti.ValueExt
  }
  deriving (Show, Eq, Generic)

instance Hashable KritiTemplate where
  hashWithSalt salt (KritiTemplate templateSrc _) = hashWithSalt salt templateSrc

instance NFData KritiTemplate

instance ToJSON KritiTemplate where
  toJSON (KritiTemplate templateSrc _) = String templateSrc

instance FromJSON KritiTemplate where
  parseJSON = withText "KritiTemplate" $ \templateSrc ->
    KritiTemplate templateSrc
      <$> Kriti.parser (T.encodeUtf8 templateSrc)
      `onLeft` \err ->
        fail $ "Kriti template parsing failed - " <> (T.unpack . serializedErrorToString . Kriti.serialize $ err)

serializedErrorToString :: Kriti.SerializedError -> Text
serializedErrorToString (Kriti.SerializedError code msg errSpan) =
  let prettyText = PP.renderStrict . PP.layoutPretty PP.defaultLayoutOptions . PP.pretty
      spanToText (Kriti.Span start end) = "from " <> prettySpan start <> " to " <> prettySpan end
      prettySpan (Kriti.AlexSourcePos line col) = "line " <> tshow line <> ", column " <> tshow col
   in msg <> " Occured " <> spanToText errSpan <> " with error code " <> dquote (prettyText code) <> "."

instance HasCodec KritiTemplate where
  codec = codecViaAeson "KritiTemplate"

-- | Connection template for the dynamic DB connection.
data ConnectionTemplate = ConnectionTemplate
  { -- | Version for the connection template. Please read more about this in the dynamic DB connection RFC (Metadata API > Versioning).
    ctVersion :: Int,
    -- | `kriti-lang` template for the dynamic DB connection.
    ctTemplate :: KritiTemplate
  }
  deriving (Show, Eq, Generic)

instance Hashable ConnectionTemplate

instance NFData ConnectionTemplate

-- | All the supported versions for the dynamic DB connection template.
supportedConnectionTemplateVersions :: [Int]
supportedConnectionTemplateVersions = [1]

instance FromJSON ConnectionTemplate where
  parseJSON = withObject "ConnectionTemplate" $ \o -> do
    version <- o .:? "version" .!= 1
    when (version `notElem` supportedConnectionTemplateVersions)
      $ fail
      $ "Supported versions are "
      <> show supportedConnectionTemplateVersions
    ConnectionTemplate version
      <$> o
      .: "template"

instance ToJSON ConnectionTemplate where
  toJSON ConnectionTemplate {..} =
    object
      [ "version" .= ctVersion,
        "template" .= ctTemplate
      ]

instance HasCodec ConnectionTemplate where
  codec =
    CommentCodec "https://hasura.io/docs/latest/graphql/core/api-reference/syntax-defs.html#pgconnectiontemplate"
      $ AC.object "ConnectionTemplate"
      $ ConnectionTemplate
      <$> optionalFieldWithOmittedDefault "version" 1 ctVersionInfoDoc
      AC..= ctVersion
        <*> requiredField "template" ctTemplateInfoDoc
      AC..= ctTemplate
    where
      ctVersionInfoDoc = "Optional connection template version (supported versions: [1], default: 1)"
      ctTemplateInfoDoc = "Connection kriti template (read more in the docs)"

-- | Name of the member of a connection set.
newtype PostgresConnectionSetMemberName = PostgresConnectionSetMemberName {getPostgresConnectionSetMemberName :: NonEmptyText}
  deriving (Show, Eq, Generic, Ord, ToTxt)

instance Hashable PostgresConnectionSetMemberName

instance NFData PostgresConnectionSetMemberName

instance ToJSON PostgresConnectionSetMemberName where
  toJSON (PostgresConnectionSetMemberName sName) = toJSON sName

instance FromJSON PostgresConnectionSetMemberName where
  parseJSON val = PostgresConnectionSetMemberName <$> parseJSON val

data PostgresConnectionSetMember = PostgresConnectionSetMember
  { pscmName :: PostgresConnectionSetMemberName,
    pscmConnectionInfo :: PostgresSourceConnInfo
  }
  deriving (Show, Eq, Generic)

instance FromJSON PostgresConnectionSetMember where
  parseJSON = genericParseJSON hasuraJSON {omitNothingFields = True}

instance ToJSON PostgresConnectionSetMember where
  toJSON = genericToJSON hasuraJSON {omitNothingFields = True}
  toEncoding = genericToEncoding hasuraJSON {omitNothingFields = True}

instance Hashable PostgresConnectionSetMember

instance NFData PostgresConnectionSetMember

-- | HashMap of the connection set. This is used for the dynamic DB connection feature.
newtype PostgresConnectionSet = PostgresConnectionSet {getPostgresConnectionSet :: NEMap.NEHashMap PostgresConnectionSetMemberName PostgresConnectionSetMember}
  deriving (Show, Eq, Generic, Semigroup)

instance Hashable PostgresConnectionSet

instance NFData PostgresConnectionSet

instance FromJSON PostgresConnectionSet where
  parseJSON = withArray "PostgresConnectionSet" \arr -> do
    connectionSet <- mapWithJSONPath parseJSON (toList arr)
    let connectionSetMemberNames = map pscmName connectionSet
        duplicateConnSetMemberNames = connectionSetMemberNames \\ (L.uniques connectionSetMemberNames)
    -- check if members with same name are present in connection set
    unless (null duplicateConnSetMemberNames) $ do
      fail $ "connection set members with duplicate names are not allowed: " ++ unpack (dquoteList (map toTxt duplicateConnSetMemberNames))
    let connectionSetTuples = map (pscmName &&& id) connectionSet
    connectionSetHashMap <- NEMap.fromList connectionSetTuples `onNothing` fail "connection set cannot be empty"
    pure $ PostgresConnectionSet connectionSetHashMap

instance ToJSON PostgresConnectionSet where
  toJSON (PostgresConnectionSet connSet) = toJSON $ NEMap.elems connSet

instance HasCodec PostgresConnectionSet where
  codec = codecViaAeson "PostgresConnectionSet"

data PostgresConnConfiguration = PostgresConnConfiguration
  { pccConnectionInfo :: PostgresSourceConnInfo,
    pccReadReplicas :: Maybe (NonEmpty PostgresSourceConnInfo),
    pccExtensionsSchema :: ExtensionsSchema,
    pccConnectionTemplate :: Maybe ConnectionTemplate,
    pccConnectionSet :: Maybe PostgresConnectionSet
  }
  deriving (Show, Eq, Generic)

instance Hashable PostgresConnConfiguration

instance NFData PostgresConnConfiguration

instance FromJSON PostgresConnConfiguration where
  parseJSON = withObject "PostgresConnConfiguration" $ \o -> do
    PostgresConnConfiguration
      <$> o
      .: "connection_info"
      <*> o
      .:? "read_replicas"
      <*> o
      .:? "extensions_schema"
      .!= defaultPostgresExtensionsSchema
      <*> o
      .:? "connection_template"
      <*> o
      .:? "connection_set"

instance ToJSON PostgresConnConfiguration where
  toJSON PostgresConnConfiguration {..} =
    object
      $ ["connection_info" .= pccConnectionInfo]
      <> maybe mempty (\readReplicas -> ["read_replicas" .= readReplicas]) pccReadReplicas
      <> bool mempty (["extensions_schema" .= pccExtensionsSchema]) (pccExtensionsSchema /= defaultPostgresExtensionsSchema)
      <> maybe mempty (\connTemplate -> ["connection_template" .= connTemplate]) pccConnectionTemplate
      <> maybe mempty (\connSet -> ["connection_set" .= NEMap.elems (getPostgresConnectionSet connSet)]) pccConnectionSet

instance HasCodec PostgresConnConfiguration where
  codec =
    CommentCodec "https://hasura.io/docs/latest/graphql/core/api-reference/syntax-defs.html#pgconfiguration"
      $ AC.object "PostgresConnConfiguration"
      $ PostgresConnConfiguration
      <$> requiredField "connection_info" connectionInfoDoc
      .== pccConnectionInfo
        <*> optionalFieldOrNull "read_replicas" readReplicasDoc
      .== pccReadReplicas
        <*> optionalFieldWithOmittedDefault "extensions_schema" defaultPostgresExtensionsSchema extensionsSchemaDoc
      .== pccExtensionsSchema
        <*> optionalFieldOrNull "connection_template" connectionTemplateDoc
      .== pccConnectionTemplate
        <*> optionalFieldOrNull "connection_set" connectionSetDoc
      .== pccConnectionSet
    where
      connectionInfoDoc = "Connection parameters for the source"
      readReplicasDoc = "Optional list of read replica configuration (supported only in cloud/enterprise versions)"
      extensionsSchemaDoc = "Name of the schema where the graphql-engine will install database extensions (default: public)"
      connectionTemplateDoc = "Optional connection template (supported only for cloud/enterprise edition)"
      connectionSetDoc = "connection set used for connection template (supported only for cloud/enterprise edition)"
      infix 8 .==
      (.==) = (AC..=)
