{-# OPTIONS -Wno-redundant-constraints #-}

-- | PostgreSQL helpers.
module Harness.Postgres
  ( livenessCheck,
    run_,
    runPersistent,
  )
where

import Control.Concurrent
import Control.Exception
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader
import Data.ByteString.Char8 qualified as S8
import Data.String
import Database.Persist.Postgresql qualified as Postgresql
import Database.Persist.Sql
import Database.PostgreSQL.LibPQ qualified as PQ
import Database.PostgreSQL.Simple qualified as Postgres
import GHC.Stack
import Harness.Constants as Constants
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
            (PQ.connectdb (fromString Constants.postgresqlConnectionString))
            PQ.finish
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

-- | Run persistent for postgres.
runPersistent ::
  (MonadUnliftIO m, HasCallStack) =>
  ReaderT SqlBackend (NoLoggingT m) a ->
  m a
runPersistent actions =
  runNoLoggingT
    ( Postgresql.withPostgresqlConn
        (fromString Constants.postgresqlConnectionString)
        (runReaderT actions)
    )
