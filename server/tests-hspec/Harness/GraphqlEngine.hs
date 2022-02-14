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
    postGraphql,
    postGraphqlYaml,

    -- ** Misc.
    setSource,
    setSources,

    -- * Misc. Helpers
    graphqlEndpoint,

    -- * Server Setup & Teardown
    startServerThread,
    stopServer,

    -- * Re-exports
    serverUrl,
    Server,
  )
where

-------------------------------------------------------------------------------

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Exception.Safe (bracket)
import Control.Monad.Trans.Managed (ManagedT (..), lowerManagedT)
import Data.Aeson (Value, object, (.=))
import Data.Environment qualified as Env
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import GHC.Stack (HasCallStack)
import Harness.Constants qualified as Constants
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
post :: HasCallStack => State -> String -> Value -> IO Value
post state path = postWithHeaders state path mempty

-- | Same as 'post', but ignores the value.
--
-- See 'postWithHeaders_' to issue a request with 'Http.RequestHeaders'.
post_ :: HasCallStack => State -> String -> Value -> IO ()
post_ state path = void . postWithHeaders_ state path mempty

-- | Post some JSON to graphql-engine, getting back more JSON.
--
-- Optimistically assumes success; use another function if you want to test for
-- failure.
postWithHeaders ::
  HasCallStack => State -> String -> Http.RequestHeaders -> Value -> IO Value
postWithHeaders (getServer -> Server {urlPrefix, port}) path =
  Http.postValue_ (urlPrefix ++ ":" ++ show port ++ path)

-- | Post some JSON to graphql-engine, getting back more JSON.
--
-- Optimistically assumes success; use another function if you want to test for
-- failure.
postWithHeaders_ ::
  HasCallStack => State -> String -> Http.RequestHeaders -> Value -> IO ()
postWithHeaders_ state path headers =
  void . postWithHeaders state path headers

-- | Same as 'postWithHeaders', but defaults to the graphql end-point.
postGraphqlYaml :: HasCallStack => State -> Value -> IO Value
postGraphqlYaml state = post state "/v1/graphql"

-- | Same as 'postGraphqlYaml', but adds the @{query:..}@ wrapper.
postGraphql :: HasCallStack => State -> Value -> IO Value
postGraphql state value = postGraphqlYaml state (object ["query" .= value])

-- | Same as 'post_', but defaults to the @"v1/metadata"@ endpoint.
--
-- @headers@ are mostly irrelevant for the admin endpoint @v1/metadata@.
postMetadata_ :: HasCallStack => State -> Value -> IO ()
postMetadata_ state = post_ state "/v1/metadata"

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

-- | Extracts the full GraphQL endpoint URL from a given 'Server'.
--
-- @
--   > graphqlEndpoint (Server 8080 "http://localhost" someThreadId)
--   "http://localhost:8080/graphql"
-- @
graphqlEndpoint :: Server -> String
graphqlEndpoint server = serverUrl server ++ "/graphql"

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

-- | Forcibly stop a given 'Server'.
stopServer :: Server -> IO ()
stopServer Server {threadId} = killThread threadId

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
    runManagedT (App.initialiseServeCtx env globalCtx serveOptions) $ \serveCtx ->
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
