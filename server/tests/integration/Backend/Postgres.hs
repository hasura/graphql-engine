-- | A module for obtaining a connection to a new, temporary Postgresql
-- database. This module is a thin wrapper over the "tmp-postgres" library.
module Backend.Postgres
  ( withPostgres,
  )
where

import Data.ByteString.Char8 qualified as BS
import Data.Text qualified as T
import Data.Text.NonEmpty (mkNonEmptyTextUnsafe)
import Data.URL.Template (mkPlainURLTemplate)
import Database.PG.Query qualified as Q
import Database.Postgres.Temp qualified as TempPG
import Hasura.Backends.Postgres.Connection
  ( PostgresConnConfiguration (..),
    PostgresPoolSettings (..),
    PostgresSourceConnInfo (..),
  )
import Hasura.Prelude
import Hasura.RQL.Types.Common
  ( InputWebhook (..),
    SourceName (..),
    UrlConf (UrlValue),
  )
import Hasura.RQL.Types.Metadata (SourceMetadata (..))
import Hasura.RQL.Types.SourceCustomization (emptySourceCustomization)
import Hasura.SQL.Backend (BackendType (..), PostgresKind (..))

--------------------------------------------------------------------------------

-- | Use "tmp-postgres" to create a new, temporary Postgresql database.
-- Redundantly supplies a connection string and an embedding of it into a
-- 'SourceMetadata' value.
withPostgres ::
  (String -> SourceMetadata ('Postgres 'Vanilla) -> IO a) ->
  IO (Either TempPG.StartError a)
withPostgres action =
  TempPG.with $ \db -> do
    let connStr = BS.unpack $ TempPG.toConnectionString db
        dbUrlConf = connStringToUrlConf connStr
        sourceMetadata = initialSourceMetadata dbUrlConf
    action connStr sourceMetadata

-- Helper
connStringToUrlConf :: String -> UrlConf
connStringToUrlConf =
  UrlValue . InputWebhook . mkPlainURLTemplate . T.pack

initialSourceMetadata :: UrlConf -> SourceMetadata ('Postgres 'Vanilla)
initialSourceMetadata dbUrl =
  SourceMetadata
    { _smName = SNName $ mkNonEmptyTextUnsafe "default",
      _smTables = mempty,
      _smFunctions = mempty,
      _smConfiguration = defaultPostgresConfig dbUrl,
      _smQueryTags = Nothing,
      _smCustomization = emptySourceCustomization
    }

-- Arbitrary
defaultPostgresConfig :: UrlConf -> PostgresConnConfiguration
defaultPostgresConfig dbUrl =
  PostgresConnConfiguration
    { _pccConnectionInfo =
        PostgresSourceConnInfo
          { _psciDatabaseUrl = dbUrl,
            _psciPoolSettings =
              Just
                PostgresPoolSettings
                  { _ppsMaxConnections = Just 50,
                    _ppsIdleTimeout = Just 180,
                    _ppsRetries = Just 1,
                    _ppsPoolTimeout = Nothing,
                    _ppsConnectionLifetime = Just 600
                  },
            _psciUsePreparedStatements = True,
            _psciIsolationLevel = Q.ReadCommitted,
            _psciSslConfiguration = Nothing
          },
      _pccReadReplicas = Nothing
    }
