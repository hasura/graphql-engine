module Hasura.Backends.Postgres.Instances.PingSource
  ( runCockroachDBPing,
  )
where

import Data.Environment qualified as Env
import Data.Text qualified as T
import Database.PG.Query qualified as PG
import Hasura.Backends.Postgres.Connection qualified as PG
import Hasura.Backends.Postgres.Connection.Connect (withPostgresDB)
import Hasura.Prelude
import Hasura.RQL.Types.Common (SourceName, sourceNameToText)
import Hasura.Server.Version

runCockroachDBPing ::
  Env.Environment ->
  (String -> IO ()) ->
  SourceName ->
  PG.PostgresConnConfiguration ->
  IO ()
runCockroachDBPing env pingLog sourceName sourceConnection = do
  let versionMessage = "hasura-graphql-engine-version=" <> tshow currentVersion
      query = PG.fromText ("select 1 /* " <> versionMessage <> " */")
  result <- withPostgresDB env sourceName sourceConnection $ do
    PG.discardQE PG.dmlTxErrorHandler query () False
  case result of
    Left _ ->
      pingLog ("Ping for " <> T.unpack (sourceNameToText sourceName) <> " failed")
    Right _ ->
      pingLog ("Ping for " <> T.unpack (sourceNameToText sourceName) <> " succeeded")
