{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes #-}

module Hasura.SQL.Backend
  ( PostgresKind (..),
    BackendType (..),
    BackendSourceKind (..),
    backendShortName,
    supportedBackends,
    backendTextNames,
    backendTypeFromText,
    parseBackendTypeFromText,
    backendTypeFromBackendSourceKind,
  )
where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Proxy
import Data.Text (unpack)
import Data.Text.Extended
import Data.Text.NonEmpty (NonEmptyText, mkNonEmptyText, nonEmptyTextQQ)
import Hasura.Backends.DataWrapper.Adapter.Types (DataConnectorName (..))
import Hasura.Incremental
import Hasura.Prelude
import Witch qualified

-- | Argument to Postgres; we represent backends which are variations on Postgres as sub-types of
-- Postgres. This value indicates which "flavour" of Postgres a backend is.
data PostgresKind
  = Vanilla
  | Citus
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable, Cacheable)

-- | An enum that represents each backend we support.
data BackendType
  = Postgres PostgresKind
  | MSSQL
  | BigQuery
  | MySQL
  | DataWrapper
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable, Cacheable)

-- | The name of the backend, as we expect it to appear in our metadata and API.
instance Witch.From BackendType NonEmptyText where
  from (Postgres Vanilla) = [nonEmptyTextQQ|postgres|]
  from (Postgres Citus) = [nonEmptyTextQQ|citus|]
  from MSSQL = [nonEmptyTextQQ|mssql|]
  from BigQuery = [nonEmptyTextQQ|bigquery|]
  from MySQL = [nonEmptyTextQQ|mysql|]
  from DataWrapper = [nonEmptyTextQQ|dataconnector|]

instance ToTxt BackendType where
  toTxt = toTxt . Witch.into @NonEmptyText

instance FromJSON BackendType where
  parseJSON = withText "backend type" parseBackendTypeFromText

instance ToJSON BackendType where
  toJSON = String . toTxt

instance Cacheable (Proxy (b :: BackendType))

-- | Similar to 'BackendType', however, in the case of 'DataWrapperKind' we need to be able
-- capture the name of the data connector that should be used by the DataWrapper backend.
-- This type correlates to the kind property of 'SourceMetadata', which is usually just
-- postgres, mssql, etc for static backends, but can be a configurable value for DataWrapper
-- hence requiring 'DataConnectorName' for 'DataWrapperKind'
--
-- This type cannot entirely replace 'BackendType' because 'BackendType' has a fixed number of
-- possible values which can be enumerated over at compile time, but 'BackendSourceKind' does
-- not because DataWrapper fundamentally is configured at runtime with 'DataConnectorName'.
data BackendSourceKind (b :: BackendType) where
  PostgresVanillaKind :: BackendSourceKind ('Postgres 'Vanilla)
  PostgresCitusKind :: BackendSourceKind ('Postgres 'Citus)
  MSSQLKind :: BackendSourceKind 'MSSQL
  BigQueryKind :: BackendSourceKind 'BigQuery
  MySQLKind :: BackendSourceKind 'MySQL
  DataWrapperKind :: DataConnectorName -> BackendSourceKind 'DataWrapper

deriving instance Show (BackendSourceKind b)

deriving instance Eq (BackendSourceKind b)

deriving instance Ord (BackendSourceKind b)

instance Cacheable (BackendSourceKind b) where
  unchanged _ = (==)

instance Witch.From (BackendSourceKind b) NonEmptyText where
  -- All cases are specified explicitly here to ensure compiler warnings highlight
  -- this area for consideration and update if another BackendType is added
  from k@PostgresVanillaKind = Witch.into @NonEmptyText $ backendTypeFromBackendSourceKind k
  from k@PostgresCitusKind = Witch.into @NonEmptyText $ backendTypeFromBackendSourceKind k
  from k@MSSQLKind = Witch.into @NonEmptyText $ backendTypeFromBackendSourceKind k
  from k@BigQueryKind = Witch.into @NonEmptyText $ backendTypeFromBackendSourceKind k
  from k@MySQLKind = Witch.into @NonEmptyText $ backendTypeFromBackendSourceKind k
  from (DataWrapperKind dataConnectorName) = Witch.into @NonEmptyText dataConnectorName

instance ToTxt (BackendSourceKind b) where
  toTxt = toTxt . Witch.into @NonEmptyText

-- If you need to parse an arbitrary string into a BackendSourceKind, you can't because of the
-- b type parameter. You actually want to parse into 'AnyBackend BackendSourceKind'.
-- See 'backendSourceKindFromText' from the AnyBackend module for that.

instance ToJSON (BackendSourceKind b) where
  toJSON = String . toTxt

instance FromJSON (BackendSourceKind ('Postgres 'Vanilla)) where
  parseJSON = mkParseStaticBackendSourceKind PostgresVanillaKind

instance FromJSON (BackendSourceKind ('Postgres 'Citus)) where
  parseJSON = mkParseStaticBackendSourceKind PostgresCitusKind

instance FromJSON (BackendSourceKind ('MSSQL)) where
  parseJSON = mkParseStaticBackendSourceKind MSSQLKind

instance FromJSON (BackendSourceKind ('BigQuery)) where
  parseJSON = mkParseStaticBackendSourceKind BigQueryKind

instance FromJSON (BackendSourceKind ('MySQL)) where
  parseJSON = mkParseStaticBackendSourceKind MySQLKind

instance FromJSON (BackendSourceKind ('DataWrapper)) where
  parseJSON = withText "BackendSourceKind" $ \text ->
    DataWrapperKind . DataConnectorName <$> mkNonEmptyText text
      `onNothing` fail "Cannot be empty string"

mkParseStaticBackendSourceKind :: BackendSourceKind b -> (Value -> Parser (BackendSourceKind b))
mkParseStaticBackendSourceKind backendSourceKind =
  withText "BackendSourceKind" $ \text ->
    if text `elem` validValues
      then pure backendSourceKind
      else fail ("got: " <> unpack text <> ", expected one of: " <> unpack (commaSeparated validValues))
  where
    validValues = backendTextNames $ backendTypeFromBackendSourceKind backendSourceKind

-- | Some generated APIs use a shortened version of the backend's name rather than its full
-- name. This function returns the "short form" of a backend, if any.
backendShortName :: BackendType -> Maybe Text
backendShortName = \case
  Postgres Vanilla -> Just "pg"
  _ -> Nothing

supportedBackends :: [BackendType]
supportedBackends =
  [ Postgres Vanilla,
    Postgres Citus,
    MSSQL,
    BigQuery,
    MySQL,
    DataWrapper
  ]

backendTextNames :: BackendType -> [Text]
backendTextNames b =
  catMaybes
    [ Just (toTxt b), -- long form
      backendShortName b -- short form
    ]

backendTextNameLookup :: [(Text, BackendType)]
backendTextNameLookup =
  supportedBackends >>= (\b -> (,b) <$> backendTextNames b)

-- | This uses this lookup mechanism to avoid having to duplicate and hardcode the
-- backend string. We accept both the short form and the long form of the backend's name.
backendTypeFromText :: Text -> Maybe BackendType
backendTypeFromText txt =
  lookup txt backendTextNameLookup

parseBackendTypeFromText :: Text -> Parser BackendType
parseBackendTypeFromText txt =
  let uniqueBackends = commaSeparated $ fst <$> backendTextNameLookup
   in backendTypeFromText txt
        `onNothing` fail ("got: " <> unpack txt <> ", expected one of: " <> unpack uniqueBackends)

backendTypeFromBackendSourceKind :: BackendSourceKind b -> BackendType
backendTypeFromBackendSourceKind = \case
  PostgresVanillaKind -> Postgres Vanilla
  PostgresCitusKind -> Postgres Citus
  MSSQLKind -> MSSQL
  BigQueryKind -> BigQuery
  MySQLKind -> MySQL
  DataWrapperKind _ -> DataWrapper
