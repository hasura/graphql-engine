{-# OPTIONS -Wno-redundant-constraints #-}

-- | PostgreSQL helpers.
module Harness.Backend.Postgres
  ( livenessCheck,
    run_,
    defaultSourceMetadata,
    defaultSourceConfiguration,
  )
where

import Control.Concurrent
import Control.Exception
import Control.Monad.Reader
import Data.Aeson (Value)
import Data.ByteString.Char8 qualified as S8
import Data.String
import Database.PostgreSQL.Simple qualified as Postgres
import GHC.Stack
import Harness.Constants as Constants
import Harness.Quoter.Yaml (yaml)
import System.Process.Typed
import Prelude

-- | Check the postgres server is live and ready to accept connections.
livenessCheck :: HasCallStack => IO ()
livenessCheck = loop Constants.postgresLivenessCheckAttempts
  where
    loop 0 = error ("Liveness check failed for PostgreSQL.")
    loop attempts =
      catch
        ( bracket
            ( Postgres.connectPostgreSQL
                (fromString Constants.postgresqlConnectionString)
            )
            Postgres.close
            (const (pure ()))
        )
        ( \(_failure :: ExitCodeException) -> do
            threadDelay
              Constants.postgresLivenessCheckIntervalMicroseconds
            loop (attempts - 1)
        )

-- | Run a plain SQL query. On error, print something useful for
-- debugging.
run_ :: HasCallStack => String -> IO ()
run_ q =
  catch
    ( bracket
        ( Postgres.connectPostgreSQL
            (fromString Constants.postgresqlConnectionString)
        )
        Postgres.close
        (\conn -> void (Postgres.execute_ conn (fromString q)))
    )
    ( \(e :: Postgres.SqlError) ->
        error
          ( unlines
              [ "PostgreSQL query error:",
                S8.unpack (Postgres.sqlErrorMsg e),
                "SQL was:",
                q
              ]
          )
    )

-- | Metadata source information for the default Postgres instance.
defaultSourceMetadata :: Value
defaultSourceMetadata =
  [yaml|
name: postgres
kind: postgres
tables: []
configuration: *defaultSourceConfiguration
|]

defaultSourceConfiguration :: Value
defaultSourceConfiguration =
  [yaml|
connection_info:
  database_url: *postgresqlConnectionString
  pool_settings: {}
|]
