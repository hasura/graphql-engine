{-# OPTIONS -Wno-redundant-constraints #-}

-- | MySQL helpers.
module Harness.Mysql
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
import Data.String
import Database.MySQL.Simple as Mysql
import Database.Persist.MySQL qualified as Mysql
import Database.Persist.Sql
import GHC.Stack
import Harness.Constants as Constants
import System.Process.Typed
import Prelude

-- | Check that the MySQL service is live and ready to accept connections.
livenessCheck :: HasCallStack => IO ()
livenessCheck = loop Constants.mysqlLivenessCheckAttempts
  where
    loop 0 = error ("Liveness check failed for MySQL.")
    loop attempts =
      catch
        ( bracket
            (Mysql.connect Constants.mysqlConnectInfo)
            Mysql.close
            (const (pure ()))
        )
        ( \(_failure :: ExitCodeException) -> do
            threadDelay
              Constants.mysqlLivenessCheckIntervalMicroseconds
            loop (attempts - 1)
        )

-- | Run a plain SQL string against the server, ignore the
-- result. Just checks for errors.
run_ :: HasCallStack => String -> IO ()
run_ query' =
  catch
    ( bracket
        (Mysql.connect Constants.mysqlConnectInfo)
        Mysql.close
        (\conn -> void (Mysql.execute_ conn (fromString query')))
    )
    ( \(e :: SomeException) ->
        error
          ( unlines
              [ "MySQL query error:",
                show e,
                "SQL was:",
                query'
              ]
          )
    )

-- | Run a persistent action against mysql.
runPersistent ::
  (MonadUnliftIO m, HasCallStack) =>
  ReaderT SqlBackend (NoLoggingT m) a ->
  m a
runPersistent actions =
  runNoLoggingT
    ( Mysql.withMySQLConn
        Constants.mysqlConnectInfo
        ( \conn -> do
            a <- runReaderT actions conn
            liftIO $ connCommit conn (error "Invalid use of connCommit.")
            pure a
        )
    )
