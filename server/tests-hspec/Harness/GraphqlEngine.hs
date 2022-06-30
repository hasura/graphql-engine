{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

-- | Helpers for talking to graphql engine.
module Harness.GraphqlEngine
  ( -- * HTTP Calls

    -- ** POST
    post,
    post_,
    postWithHeaders,
    postWithHeaders_,
    postMetadata_,
    postMetadata,
    postMetadataWithStatus,
    exportMetadata,
    postGraphqlYaml,
    postGraphqlYamlWithHeaders,
    postGraphql,
    postGraphqlWithPair,
    postGraphqlWithHeaders,
    postWithHeadersStatus,
    clearMetadata,
    postV2Query,
    postV2Query_,

    -- ** Misc.
    setSource,
    setSources,

    -- * Server Setup
    startServerThread,

    -- * Re-exports
    serverUrl,
    Server,
  )
where

-------------------------------------------------------------------------------

import Control.Concurrent (forkIO)
import Control.Concurrent.Extended (sleep)
import Control.Monad.Trans.Managed (ManagedT (..), lowerManagedT)
import Data.Aeson (Value, object, (.=))
import Data.Aeson.Types (Pair)
import Data.Environment qualified as Env
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import Harness.Constants qualified as Constants
import Harness.Exceptions (HasCallStack, bracket, withFrozenCallStack)
import Harness.Http qualified as Http
import Harness.Quoter.Yaml (yaml)
import Harness.TestEnvironment (Server (..), TestEnvironment, getServer, serverUrl)
import Hasura.App (Loggers (..), ServeCtx (..))
import Hasura.App qualified as App
import Hasura.Logging (Hasura)
import Hasura.Prelude
import Hasura.RQL.Types.Common (PGConnectionParams (..), UrlConf (..))
import Hasura.Server.Init (PostgresConnInfo (..), ServeOptions (..))
import Hasura.Server.Metrics (ServerMetricsSpec, createServerMetrics)
import Network.Socket qualified as Socket
import Network.Wai.Handler.Warp qualified as Warp
import System.Metrics qualified as EKG

-------------------------------------------------------------------------------

-- HTTP Calls - POST

-- | Post some JSON to graphql-engine, getting back more JSON.
--
-- Optimistically assumes success; use another function if you want to test for
-- failure.
--
-- See 'postWithHeaders' to issue a request with 'Http.RequestHeaders'.
--
-- Note: We add 'withFrozenCallStack' to reduce stack trace clutter.
post :: HasCallStack => TestEnvironment -> String -> Value -> IO Value
post testEnvironment path v = withFrozenCallStack $ postWithHeaders testEnvironment path mempty v

-- | Same as 'post', but ignores the value.
--
-- See 'postWithHeaders_' to issue a request with 'Http.RequestHeaders'.
--
-- Note: We add 'withFrozenCallStack' to reduce stack trace clutter.
post_ :: HasCallStack => TestEnvironment -> String -> Value -> IO ()
post_ testEnvironment path v = void $ withFrozenCallStack $ postWithHeaders_ testEnvironment path mempty v

-- | Post some JSON to graphql-engine, getting back more JSON.
--
-- Optimistically assumes success; use another function if you want to test for
-- failure.
--
-- Note: We add 'withFrozenCallStack' to reduce stack trace clutter.
postWithHeaders ::
  HasCallStack => TestEnvironment -> String -> Http.RequestHeaders -> Value -> IO Value
postWithHeaders v = withFrozenCallStack $ postWithHeadersStatus 200 v

-- | Post some JSON to graphql-engine, getting back more JSON.
--
-- Expecting non-200 status code; use @'postWithHeaders' if you want to test for
-- success reponse.
--
-- Note: We add 'withFrozenCallStack' to reduce stack trace clutter.
postWithHeadersStatus ::
  HasCallStack => Int -> TestEnvironment -> String -> Http.RequestHeaders -> Value -> IO Value
postWithHeadersStatus statusCode (getServer -> Server {urlPrefix, port}) path headers v =
  withFrozenCallStack $ Http.postValueWithStatus statusCode (urlPrefix ++ ":" ++ show port ++ path) headers v

-- | Post some JSON to graphql-engine, getting back more JSON.
--
-- Optimistically assumes success; use another function if you want to test for
-- failure.
--
-- Note: We add 'withFrozenCallStack' to reduce stack trace clutter.
postWithHeaders_ ::
  HasCallStack => TestEnvironment -> String -> Http.RequestHeaders -> Value -> IO ()
postWithHeaders_ testEnvironment path headers v =
  void $ withFrozenCallStack $ postWithHeaders testEnvironment path headers v

-- | Same as 'post', but defaults to the graphql end-point.
--
-- Note: We add 'withFrozenCallStack' to reduce stack trace clutter.
postGraphqlYaml ::
  HasCallStack => TestEnvironment -> Value -> IO Value
postGraphqlYaml testEnvironment v = withFrozenCallStack $ postGraphqlYamlWithHeaders testEnvironment mempty v

-- | Same as 'postWithHeaders', but defaults to the graphql end-point.
--
-- Note: We add 'withFrozenCallStack' to reduce stack trace clutter.
postGraphqlYamlWithHeaders ::
  HasCallStack => TestEnvironment -> Http.RequestHeaders -> Value -> IO Value
postGraphqlYamlWithHeaders testEnvironment headers =
  withFrozenCallStack $ postWithHeaders testEnvironment "/v1/graphql" headers

-- | Same as 'postGraphqlYaml', but adds the @{query:..}@ wrapper.
--
-- Note: We add 'withFrozenCallStack' to reduce stack trace clutter.
postGraphql :: HasCallStack => TestEnvironment -> Value -> IO Value
postGraphql testEnvironment value =
  withFrozenCallStack $ postGraphqlYaml testEnvironment (object ["query" .= value])

-- | Same as postGraphql but accepts a list of 'Pair' to pass
-- additional parameters to the endpoint.
postGraphqlWithPair :: HasCallStack => TestEnvironment -> Value -> [Pair] -> IO Value
postGraphqlWithPair testEnvironment value pair =
  withFrozenCallStack $ postGraphqlYaml testEnvironment (object $ ["query" .= value] <> pair)

-- | Same as 'postGraphqlYamlWithHeaders', but adds the @{query:..}@ wrapper.
--
-- Note: We add 'withFrozenCallStack' to reduce stack trace clutter.
postGraphqlWithHeaders ::
  HasCallStack => TestEnvironment -> Http.RequestHeaders -> Value -> IO Value
postGraphqlWithHeaders testEnvironment headers value =
  withFrozenCallStack $ postGraphqlYamlWithHeaders testEnvironment headers (object ["query" .= value])

-- | Same as 'post_', but defaults to the @"v1/metadata"@ endpoint.
--
-- @headers@ are mostly irrelevant for the admin endpoint @v1/metadata@.
--
-- Note: We add 'withFrozenCallStack' to reduce stack trace clutter.
postMetadata_ :: HasCallStack => TestEnvironment -> Value -> IO ()
postMetadata_ testEnvironment = withFrozenCallStack $ post_ testEnvironment "/v1/metadata"

postMetadata :: HasCallStack => TestEnvironment -> Value -> IO Value
postMetadata testEnvironment = withFrozenCallStack $ post testEnvironment "/v1/metadata"

postMetadataWithStatus :: HasCallStack => Int -> TestEnvironment -> Value -> IO Value
postMetadataWithStatus statusCode testEnvironment v =
  withFrozenCallStack $ postWithHeadersStatus statusCode testEnvironment "/v1/metadata" mempty v

-- | Resets metadata, removing all sources or remote schemas.
--
-- Note: We add 'withFrozenCallStack' to reduce stack trace clutter.
clearMetadata :: HasCallStack => TestEnvironment -> IO ()
clearMetadata s = withFrozenCallStack $ postMetadata_ s [yaml|{type: clear_metadata, args: {}}|]

exportMetadata :: HasCallStack => TestEnvironment -> IO Value
exportMetadata s = withFrozenCallStack $ postMetadata s [yaml|{type: export_metadata, args: {}}|]

-- | Same as 'postWithHeadersStatus', but defaults to the @"/v2/query"@ endpoint
--
-- @headers@ are mostly irrelevant for the admin endpoint @v2/query@.
--
-- Note: We add 'withFrozenCallStack' to reduce stack trace clutter.
postV2Query :: HasCallStack => Int -> TestEnvironment -> Value -> IO Value
postV2Query statusCode testEnvironment =
  withFrozenCallStack $ postWithHeadersStatus statusCode testEnvironment "/v2/query" mempty

postV2Query_ :: HasCallStack => TestEnvironment -> Value -> IO ()
postV2Query_ testEnvironment =
  withFrozenCallStack $ post_ testEnvironment "/v2/query"

-------------------------------------------------------------------------------

-- HTTP Calls - Misc.

-- | Replace the engine's metadata to use the given source as the only source.
setSource :: TestEnvironment -> Value -> Maybe Value -> IO ()
setSource testEnvironment sourceMetadata backendConfig = setSources testEnvironment [sourceMetadata] backendConfig

-- | Replace the engine's metadata to use the given sources.
setSources :: TestEnvironment -> [Value] -> Maybe Value -> IO ()
setSources testEnvironment sourcesMetadata backendConfig =
  postMetadata_
    testEnvironment
    [yaml|
type: replace_metadata
args:
  metadata:
    version: 3
    sources: *sourcesMetadata
    backend_configs: *backendConfig
  |]

-------------------------------------------------------------------------------

-- | Choose a random port and start a graphql-engine server on that
-- port accessible from localhost. It waits until the server is
-- available before returning.
--
-- The port availability is subject to races.
startServerThread :: Maybe (String, Int) -> IO Server
startServerThread murlPrefixport = do
  (urlPrefix, port, threadId) <-
    case murlPrefixport of
      Just (urlPrefix, port) -> do
        threadId <- forkIO (forever (sleep 1)) -- Just wait.
        pure (urlPrefix, port, threadId)
      Nothing -> do
        port <- bracket (Warp.openFreePort) (Socket.close . snd) (pure . fst)
        let urlPrefix = "http://127.0.0.1"
        threadId <-
          forkIO (runApp Constants.serveOptions {soPort = fromIntegral port})
        pure (urlPrefix, port, threadId)
  let server = Server {port = fromIntegral port, urlPrefix, threadId}
  Http.healthCheck (serverUrl server)
  pure server

-------------------------------------------------------------------------------

-- | Run the graphql-engine server.
runApp :: ServeOptions Hasura.Logging.Hasura -> IO ()
runApp serveOptions = do
  let rci =
        PostgresConnInfo
          { _pciDatabaseConn =
              Just
                ( UrlFromParams
                    PGConnectionParams
                      { _pgcpHost = T.pack Constants.postgresHost,
                        _pgcpUsername = T.pack Constants.postgresUser,
                        _pgcpPassword = Just (T.pack Constants.postgresPassword),
                        _pgcpPort = fromIntegral Constants.postgresPort,
                        _pgcpDatabase = T.pack Constants.postgresDb
                      }
                ),
            _pciRetries = Nothing
          }
      metadataDbUrl = Just Constants.postgresqlConnectionString
  env <- Env.getEnvironment
  initTime <- liftIO getCurrentTime
  globalCtx <- App.initGlobalCtx env metadataDbUrl rci
  do
    (ekgStore, serverMetrics) <-
      liftIO $ do
        store <- EKG.newStore @TestMetricsSpec
        serverMetrics <-
          liftIO $ createServerMetrics $ EKG.subset ServerSubset store
        pure (EKG.subset EKG.emptyOf store, serverMetrics)
    runManagedT (App.initialiseServeCtx env globalCtx serveOptions serverMetrics) $ \serveCtx ->
      do
        let Loggers _ _logger pgLogger = _scLoggers serveCtx
        flip App.runPGMetadataStorageAppT (_scMetadataDbPool serveCtx, pgLogger)
          . lowerManagedT
          $ do
            App.runHGEServer
              (const $ pure ())
              env
              serveOptions
              serveCtx
              initTime
              Nothing
              serverMetrics
              ekgStore

-- | Used only for 'runApp' above.
data TestMetricsSpec name metricType tags
  = ServerSubset (ServerMetricsSpec name metricType tags)
