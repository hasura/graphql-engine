module Hasura.Backends.Postgres.Instances.PingSource
  ( runCockroachDBPing,
  )
where

import Data.Text qualified as T
import Database.PG.Query qualified as PG
import Hasura.Backends.Postgres.Connection qualified as PG
import Hasura.Backends.Postgres.Connection.Connect (withPostgresDB)
import Hasura.Prelude
import Hasura.RQL.Types.Common (SourceName, sourceNameToText)
import Hasura.Server.Version

runCockroachDBPing ::
  (String -> IO ()) ->
  SourceName ->
  PG.PostgresConnConfiguration ->
  IO ()
runCockroachDBPing pingLog sourceName sourceConnection = do
  let versionMessage = "hasura-graphql-engine-version=" <> tshow currentVersion
      query = PG.fromText ("select 1 /* " <> versionMessage <> " */")
  result <- withPostgresDB sourceConnection $ do
    PG.discardQE PG.dmlTxErrorHandler query () False
  case result of
    Left _ ->
      pingLog ("Ping for " <> T.unpack (sourceNameToText sourceName) <> " failed")
    Right _ ->
      pingLog ("Ping for " <> T.unpack (sourceNameToText sourceName) <> " succeeded")
