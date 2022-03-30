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
    postGraphqlYaml,
    postGraphqlYamlWithHeaders,
    postGraphql,
    postGraphqlWithHeaders,
    clearMetadata,
    postV2Query,

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

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad.Trans.Managed (ManagedT (..), lowerManagedT)
import Data.Aeson (Value, object, (.=))
import Data.Environment qualified as Env
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import Harness.Constants qualified as Constants
import Harness.Exceptions (HasCallStack, bracket, withFrozenCallStack)
import Harness.Http qualified as Http
import Harness.Quoter.Yaml (yaml)
import Harness.State (Server (..), State, getServer, serverUrl)
import Hasura.App (Loggers (..), ServeCtx (..))
import Hasura.App qualified as App
import Hasura.Logging (Hasura)
import Hasura.Prelude hiding (State, state)
import Hasura.RQL.Types (PGConnectionParams (..), UrlConf (..))
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
post :: HasCallStack => State -> String -> Value -> IO Value
post state path = withFrozenCallStack . postWithHeaders state path mempty

-- | Same as 'post', but ignores the value.
--
-- See 'postWithHeaders_' to issue a request with 'Http.RequestHeaders'.
--
-- Note: We add 'withFrozenCallStack' to reduce stack trace clutter.
post_ :: HasCallStack => State -> String -> Value -> IO ()
post_ state path = void . withFrozenCallStack . postWithHeaders_ state path mempty

-- | Post some JSON to graphql-engine, getting back more JSON.
--
-- Optimistically assumes success; use another function if you want to test for
-- failure.
--
-- Note: We add 'withFrozenCallStack' to reduce stack trace clutter.
postWithHeaders ::
  HasCallStack => State -> String -> Http.RequestHeaders -> Value -> IO Value
postWithHeaders = withFrozenCallStack . postWithHeadersStatus 200

-- | Post some JSON to graphql-engine, getting back more JSON.
--
-- Expecting non-200 status code; use @'postWithHeaders' if you want to test for
-- success reponse.
--
-- Note: We add 'withFrozenCallStack' to reduce stack trace clutter.
postWithHeadersStatus ::
  HasCallStack => Int -> State -> String -> Http.RequestHeaders -> Value -> IO Value
postWithHeadersStatus statusCode (getServer -> Server {urlPrefix, port}) path headers =
  withFrozenCallStack . Http.postValueWithStatus statusCode (urlPrefix ++ ":" ++ show port ++ path) headers

-- | Post some JSON to graphql-engine, getting back more JSON.
--
-- Optimistically assumes success; use another function if you want to test for
-- failure.
--
-- Note: We add 'withFrozenCallStack' to reduce stack trace clutter.
postWithHeaders_ ::
  HasCallStack => State -> String -> Http.RequestHeaders -> Value -> IO ()
postWithHeaders_ state path headers =
  void . withFrozenCallStack . postWithHeaders state path headers

-- | Same as 'post', but defaults to the graphql end-point.
--
-- Note: We add 'withFrozenCallStack' to reduce stack trace clutter.
postGraphqlYaml ::
  HasCallStack => State -> Value -> IO Value
postGraphqlYaml state = withFrozenCallStack . postGraphqlYamlWithHeaders state mempty

-- | Same as 'postWithHeaders', but defaults to the graphql end-point.
--
-- Note: We add 'withFrozenCallStack' to reduce stack trace clutter.
postGraphqlYamlWithHeaders ::
  HasCallStack => State -> Http.RequestHeaders -> Value -> IO Value
postGraphqlYamlWithHeaders state headers =
  withFrozenCallStack $ postWithHeaders state "/v1/graphql" headers

-- | Same as 'postGraphqlYaml', but adds the @{query:..}@ wrapper.
--
-- Note: We add 'withFrozenCallStack' to reduce stack trace clutter.
postGraphql :: HasCallStack => State -> Value -> IO Value
postGraphql state value =
  withFrozenCallStack $ postGraphqlYaml state (object ["query" .= value])

-- | Same as 'postGraphqlYamlWithHeaders', but adds the @{query:..}@ wrapper.
--
-- Note: We add 'withFrozenCallStack' to reduce stack trace clutter.
postGraphqlWithHeaders ::
  HasCallStack => State -> Http.RequestHeaders -> Value -> IO Value
postGraphqlWithHeaders state headers value =
  withFrozenCallStack $ postGraphqlYamlWithHeaders state headers (object ["query" .= value])

-- | Same as 'post_', but defaults to the @"v1/metadata"@ endpoint.
--
-- @headers@ are mostly irrelevant for the admin endpoint @v1/metadata@.
--
-- Note: We add 'withFrozenCallStack' to reduce stack trace clutter.
postMetadata_ :: HasCallStack => State -> Value -> IO ()
postMetadata_ state = withFrozenCallStack $ post_ state "/v1/metadata"

-- | Resets metadata, removing all sources or remote schemas.
--
-- Note: We add 'withFrozenCallStack' to reduce stack trace clutter.
clearMetadata :: HasCallStack => State -> IO ()
clearMetadata s = withFrozenCallStack $ postMetadata_ s [yaml|{type: clear_metadata, args: {}}|]

-- | Same as 'postWithHeadersStatus', but defaults to the @"/v2/query"@ endpoint
--
-- @headers@ are mostly irrelevant for the admin endpoint @v2/query@.
--
-- Note: We add 'withFrozenCallStack' to reduce stack trace clutter.
postV2Query :: HasCallStack => Int -> State -> Value -> IO Value
postV2Query statusCode state =
  withFrozenCallStack $ postWithHeadersStatus statusCode state "/v2/query" mempty

-------------------------------------------------------------------------------

-- HTTP Calls - Misc.

-- | Replace the engine's metadata to use the given source as the only source.
setSource :: State -> Value -> IO ()
setSource state sourceMetadata = setSources state [sourceMetadata]

-- | Replace the engine's metadata to use the given sources.
setSources :: State -> [Value] -> IO ()
setSources state sourcesMetadata =
  postMetadata_
    state
    [yaml|
type: replace_metadata
args:
  version: 3
  sources: *sourcesMetadata
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
        threadId <- forkIO (forever (threadDelay 1000000)) -- Just wait.
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
