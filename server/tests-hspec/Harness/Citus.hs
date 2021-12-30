{-# OPTIONS -Wno-redundant-constraints #-}

-- | CitusQL helpers. Pretty much the same as postgres. Could refactor
-- if we add more things here.
module Harness.Citus
  ( livenessCheck,
    run_,
  )
where

import Control.Concurrent
import Control.Exception
import Control.Monad.Reader
import Data.ByteString.Char8 qualified as S8
import Data.String
import Database.PostgreSQL.Simple qualified as Postgres
import GHC.Stack
import Harness.Constants as Constants
import System.Process.Typed
import Prelude

-- | Check the citus server is live and ready to accept connections.
livenessCheck :: HasCallStack => IO ()
livenessCheck = loop Constants.postgresLivenessCheckAttempts
  where
    loop 0 = error ("Liveness check failed for Citus.")
    loop attempts =
      catch
        ( bracket
            ( Postgres.connectPostgreSQL
                (fromString Constants.citusConnectionString)
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
            (fromString Constants.citusConnectionString)
        )
        Postgres.close
        (\conn -> void (Postgres.execute_ conn (fromString q)))
    )
    ( \(e :: Postgres.SqlError) ->
        error
          ( unlines
              [ "Citus query error:",
                S8.unpack (Postgres.sqlErrorMsg e),
                "SQL was:",
                q
              ]
          )
    )
