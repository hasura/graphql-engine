-- | This module houses low-level functions and types to help access and work
-- with Postgres servers. For functions and types to help test the Postgres HGE
-- backend, see 'Harness.Backend.Postgres'.
module Harness.Services.Postgres
  ( FreshPostgresDb (..),
    PostgresServerUrl (..),
    run,
    mkFreshPostgresDb,
    mkFreshDbConnectionString,
  )
where

import Control.Monad.Managed
import Data.ByteString.Char8 qualified as S8
import Data.Char qualified
import Data.String
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.UUID.V4 qualified as UUID
import Database.PostgreSQL.Simple qualified as Postgres
import Harness.Exceptions
import Harness.Logging
import Hasura.Prelude

newtype PostgresServerUrl = PostgresServerUrl {getPostgresServerUrl :: Text}

newtype FreshPostgresDb = FreshPostgresDb {freshDbName :: Text}

--- | Run a plain SQL query.
-- On error, print something useful for debugging.
run :: HasCallStack => Logger -> PostgresServerUrl -> Text -> IO ()
run logger (PostgresServerUrl connectionString) query = do
  runLogger logger $ LogDBQuery connectionString query
  catch
    ( bracket
        ( Postgres.connectPostgreSQL
            (encodeUtf8 connectionString)
        )
        Postgres.close
        (\conn -> void (Postgres.execute_ conn (fromString $ T.unpack query)))
    )
    ( \(e :: Postgres.SqlError) ->
        error
          ( unlines
              [ "PostgreSQL query error:",
                S8.unpack (Postgres.sqlErrorMsg e),
                "SQL was:",
                T.unpack query
              ]
          )
    )

mkFreshPostgresDb :: Logger -> PostgresServerUrl -> Managed FreshPostgresDb
mkFreshPostgresDb logger pgServerUrl = do
  freshDbName <- liftIO $ drawFreshDbName
  managed $
    bracket
      (run logger pgServerUrl ("CREATE DATABASE " <> freshDbName <> ""))
      (\_ -> run logger pgServerUrl ("DROP DATABASE " <> freshDbName <> ""))
  return $ FreshPostgresDb freshDbName
  where
    drawFreshDbName :: IO Text
    drawFreshDbName = do
      uuid <- tshow <$> liftIO UUID.nextRandom
      return $
        "freshdb_"
          <> T.map
            ( \a ->
                if Data.Char.isAlphaNum a
                  then a
                  else '_'
            )
            uuid

mkFreshDbConnectionString :: PostgresServerUrl -> FreshPostgresDb -> PostgresServerUrl
mkFreshDbConnectionString (PostgresServerUrl pgUrl) (FreshPostgresDb db) =
  PostgresServerUrl $ T.dropWhileEnd ((/= '/')) pgUrl <> db
