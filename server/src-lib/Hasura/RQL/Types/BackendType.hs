{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes #-}

module Hasura.RQL.Types.BackendType
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

import Autodocodec (Codec (StringCodec), HasCodec (codec), JSONCodec, bimapCodec, literalTextCodec, parseAlternatives, (<?>))
import Data.Aeson hiding ((<?>))
import Data.Aeson.Types (Parser)
import Data.Text (unpack)
import Data.Text.Extended
import Data.Text.NonEmpty (NonEmptyText, nonEmptyTextQQ)
import Hasura.Prelude
import Hasura.RQL.Types.DataConnector (DataConnectorName (..), mkDataConnectorName)
import Language.GraphQL.Draft.Syntax qualified as GQL
import Witch qualified

-- | Argument to Postgres; we represent backends which are variations on Postgres as sub-types of
-- Postgres. This value indicates which "flavour" of Postgres a backend is.
data PostgresKind
  = Vanilla
  | Citus
  | Cockroach
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable)

-- | An enum that represents each backend we support.
data BackendType
  = Postgres PostgresKind
  | MSSQL
  | BigQuery
  | DataConnector
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable)

-- | The name of the backend, as we expect it to appear in our metadata and API.
instance Witch.From BackendType NonEmptyText where
  from (Postgres Vanilla) = [nonEmptyTextQQ|postgres|]
  from (Postgres Citus) = [nonEmptyTextQQ|citus|]
  from (Postgres Cockroach) = [nonEmptyTextQQ|cockroach|]
  from MSSQL = [nonEmptyTextQQ|mssql|]
  from BigQuery = [nonEmptyTextQQ|bigquery|]
  from DataConnector = [nonEmptyTextQQ|dataconnector|]

instance ToTxt BackendType where
  toTxt = toTxt . Witch.into @NonEmptyText

instance FromJSON BackendType where
  parseJSON = withText "backend type" parseBackendTypeFromText

instance ToJSON BackendType where
  toJSON = String . toTxt

-- | Similar to 'BackendType', however, in the case of 'DataConnectorKind' we need to be able
-- capture the name of the data connector that should be used by the DataConnector backend.
-- This type correlates to the kind property of 'SourceMetadata', which is usually just
-- postgres, mssql, etc for static backends, but can be a configurable value for DataConnector
-- hence requiring 'DataConnectorName' for 'DataConnectorKind'
--
-- This type cannot entirely replace 'BackendType' because 'BackendType' has a fixed number of
-- possible values which can be enumerated over at compile time, but 'BackendSourceKind' does
-- not because DataConnector fundamentally is configured at runtime with 'DataConnectorName'.
data BackendSourceKind (b :: BackendType) where
  PostgresVanillaKind :: BackendSourceKind ('Postgres 'Vanilla)
  PostgresCitusKind :: BackendSourceKind ('Postgres 'Citus)
  PostgresCockroachKind :: BackendSourceKind ('Postgres 'Cockroach)
  MSSQLKind :: BackendSourceKind 'MSSQL
  BigQueryKind :: BackendSourceKind 'BigQuery
  DataConnectorKind :: DataConnectorName -> BackendSourceKind 'DataConnector

deriving instance Show (BackendSourceKind b)

deriving instance Eq (BackendSourceKind b)

deriving instance Ord (BackendSourceKind b)

instance Witch.From (BackendSourceKind b) NonEmptyText where
  -- All cases are specified explicitly here to ensure compiler warnings highlight
  -- this area for consideration and update if another BackendType is added
  from k@PostgresVanillaKind = Witch.into @NonEmptyText $ backendTypeFromBackendSourceKind k
  from k@PostgresCitusKind = Witch.into @NonEmptyText $ backendTypeFromBackendSourceKind k
  from k@PostgresCockroachKind = Witch.into @NonEmptyText $ backendTypeFromBackendSourceKind k
  from k@MSSQLKind = Witch.into @NonEmptyText $ backendTypeFromBackendSourceKind k
  from k@BigQueryKind = Witch.into @NonEmptyText $ backendTypeFromBackendSourceKind k
  from (DataConnectorKind dataConnectorName) = Witch.into @NonEmptyText dataConnectorName

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

instance FromJSON (BackendSourceKind ('Postgres 'Cockroach)) where
  parseJSON = mkParseStaticBackendSourceKind PostgresCockroachKind

instance FromJSON (BackendSourceKind ('MSSQL)) where
  parseJSON = mkParseStaticBackendSourceKind MSSQLKind

instance FromJSON (BackendSourceKind ('BigQuery)) where
  parseJSON = mkParseStaticBackendSourceKind BigQueryKind

instance FromJSON (BackendSourceKind ('DataConnector)) where
  parseJSON v = DataConnectorKind <$> parseJSON v

mkParseStaticBackendSourceKind :: BackendSourceKind b -> (Value -> Parser (BackendSourceKind b))
mkParseStaticBackendSourceKind backendSourceKind =
  withText "BackendSourceKind" $ \text ->
    if text `elem` validValues
      then pure backendSourceKind
      else fail ("got: " <> unpack text <> ", expected one of: " <> unpack (commaSeparated validValues))
  where
    validValues = backendTextNames $ backendTypeFromBackendSourceKind backendSourceKind

instance HasCodec (BackendSourceKind ('Postgres 'Vanilla)) where
  codec = mkCodecStaticBackendSourceKind PostgresVanillaKind

instance HasCodec (BackendSourceKind ('Postgres 'Citus)) where
  codec = mkCodecStaticBackendSourceKind PostgresCitusKind

instance HasCodec (BackendSourceKind ('Postgres 'Cockroach)) where
  codec = mkCodecStaticBackendSourceKind PostgresCockroachKind

instance HasCodec (BackendSourceKind ('MSSQL)) where
  codec = mkCodecStaticBackendSourceKind MSSQLKind

instance HasCodec (BackendSourceKind ('BigQuery)) where
  codec = mkCodecStaticBackendSourceKind BigQueryKind

instance HasCodec (BackendSourceKind ('DataConnector)) where
  codec = bimapCodec dec enc gqlNameCodec
    where
      dec :: GQL.Name -> Either String (BackendSourceKind 'DataConnector)
      dec n = DataConnectorKind <$> mkDataConnectorName n

      enc :: BackendSourceKind ('DataConnector) -> GQL.Name
      enc (DataConnectorKind dcName) = unDataConnectorName dcName

      gqlNameCodec :: JSONCodec GQL.Name
      gqlNameCodec =
        bimapCodec
          parseName
          GQL.unName
          (StringCodec (Just "GraphQLName"))
          <?> "A valid GraphQL name"

      parseName text = GQL.mkName text `onNothing` Left (unpack text <> " is not a valid GraphQL name")

mkCodecStaticBackendSourceKind :: BackendSourceKind b -> JSONCodec (BackendSourceKind b)
mkCodecStaticBackendSourceKind backendSourceKind =
  bimapCodec dec enc
    $ parseAlternatives (literalTextCodec longName) (literalTextCodec <$> aliases)
  where
    dec text =
      if text `elem` validValues
        then Right backendSourceKind
        else Left ("got: " <> unpack text <> ", expected one of: " <> unpack (commaSeparated validValues))

    enc = toTxt

    validValues = backendTextNames $ backendTypeFromBackendSourceKind backendSourceKind
    longName = head validValues
    aliases = tail validValues

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
    Postgres Cockroach,
    MSSQL,
    BigQuery,
    DataConnector
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
  PostgresCockroachKind -> Postgres Cockroach
  MSSQLKind -> MSSQL
  BigQueryKind -> BigQuery
  DataConnectorKind _ -> DataConnector
