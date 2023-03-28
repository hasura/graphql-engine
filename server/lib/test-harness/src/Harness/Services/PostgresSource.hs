{-# LANGUAGE QuasiQuotes #-}

-- | This module houses low-level functions and types to help access and work
-- with Postgres sources.
module Harness.Services.PostgresSource (withPostgresSource, PostgresSource (getPostgresSource)) where

import Data.Has
import Harness.Logging
import Harness.Quoter.Yaml (yaml)
import Harness.Services.GraphqlEngine
import Harness.Services.PostgresDb
import Hasura.Prelude
import Test.Hspec

newtype PostgresSource = PostgresSource {getPostgresSource :: Text}

withPostgresSource ::
  ( Has Logger a,
    Has PostgresServerUrl a,
    Has HgeServerInstance a
  ) =>
  Text ->
  SpecWith (PostgresSource, a) ->
  SpecWith a
withPostgresSource sourceName specs =
  -- We mark "Postgres" to interoperate with HASURA_TEST_BACKEND_TYPE.
  describe "Postgres" $
    flip aroundWith specs \action env -> do
      pg_add_source env sourceName
      -- TODO assert that res is a success result
      -- TODO: use 'managed'?
      action (PostgresSource sourceName, env)

pg_add_source ::
  ( Has Logger env,
    Has PostgresServerUrl env,
    Has HgeServerInstance env
  ) =>
  env ->
  Text ->
  IO ()
pg_add_source env sourceName = do
  let pgUrl = getPostgresServerUrl $ getter env
  _res <-
    hgePost
      env
      200
      "/v1/metadata"
      []
      [yaml|
          args:
            configuration:
              connection_info:
                database_url: *pgUrl
            name: *sourceName
          type: pg_add_source
        |]

  -- TODO assert that res is a success result
  return ()
