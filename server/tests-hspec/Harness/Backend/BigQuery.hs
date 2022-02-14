{-# OPTIONS -Wno-redundant-constraints #-}

-- | BigQuery helpers.
module Harness.Backend.BigQuery
  ( run_,
    getServiceAccount,
    getProjectId,
  )
where

import Control.Exception
import Data.String
import GHC.Stack
import Harness.Constants as Constants
import Harness.Env
import Hasura.Backends.BigQuery.Connection (initConnection)
import Hasura.Backends.BigQuery.Execute (BigQuery (..), executeBigQuery)
import Hasura.Backends.BigQuery.Source (ServiceAccount)
import Hasura.Prelude

getServiceAccount :: (HasCallStack) => IO ServiceAccount
getServiceAccount = getEnvJSON Constants.bigqueryServiceAccountVar

getProjectId :: (HasCallStack) => IO Text
getProjectId = getEnvString Constants.bigqueryProjectIdVar

-- | Run a plain Standard SQL string against the server, ignore the
-- result. Just checks for errors.
run_ :: (HasCallStack) => ServiceAccount -> Text -> String -> IO ()
run_ serviceAccount projectId query =
  handle (\(e :: SomeException) -> bigQueryError e query) $ do
    conn <- initConnection serviceAccount projectId Nothing
    res <- executeBigQuery conn BigQuery {query = fromString query, parameters = mempty}
    case res of
      Left err -> bigQueryError err query
      Right () -> pure ()

bigQueryError :: (Show e, HasCallStack) => e -> String -> IO ()
bigQueryError e query =
  error
    ( unlines
        [ "BigQuery query error:",
          show e,
          "SQL was:",
          query
        ]
    )
