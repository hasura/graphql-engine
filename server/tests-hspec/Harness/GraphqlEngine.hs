{-# LANGUAGE ViewPatterns #-}

-- | Helpers for talking to graphql engine.
module Harness.GraphqlEngine
  ( post,
    post_,
    postGraphql,
    postGraphqlYaml,
    startServerThread,
    stopServer,
    serverUrl,
    Server,
  )
where

import Control.Concurrent
import Control.Exception
import Control.Monad.Trans.Managed (ManagedT (..), lowerManagedT)
import Data.Aeson
import Data.Environment qualified as Env
import Data.Functor
import Data.Text qualified as T
import Data.Time
import GHC.Stack
import Harness.Constants qualified as Constants
import Harness.Http qualified as Http
import Harness.State
import Hasura.App
import Hasura.Logging (Hasura)
import Hasura.Prelude hiding (State)
import Hasura.RQL.Types
import Hasura.Server.Init
import Hasura.Server.Metrics (ServerMetricsSpec, createServerMetrics)
import Network.Socket qualified as Socket
import Network.Wai.Handler.Warp qualified as Warp
import System.Metrics qualified as EKG

-- | Used only for 'runApp' below.
data TestMetricsSpec name metricType tags
  = ServerSubset (ServerMetricsSpec name metricType tags)

-- | Post some JSON to graphql-engine, getting back more
-- JSON. Optimistically assumes success. Use another function if you
-- want to test for failure.
post :: HasCallStack => State -> String -> Value -> IO Value
post (getServer -> Server {urlPrefix, port}) path =
  Http.postValue_ (urlPrefix ++ ":" ++ show port ++ path)

-- | Same as post, but ignores the value.
post_ :: HasCallStack => State -> String -> Value -> IO ()
post_ server path = void . post server path

-- | Same as post, but defaults to the graphql end-point.
postGraphqlYaml :: HasCallStack => State -> Value -> IO Value
postGraphqlYaml server = post server "/v1/graphql"

-- | Same as 'postGraphqlYaml', but adds the {query:..} wrapper.
postGraphql :: HasCallStack => State -> Value -> IO Value
postGraphql server value = postGraphqlYaml server (object ["query" .= value])

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

-- | Stop the server.
stopServer :: Server -> IO ()
stopServer Server {threadId} = killThread threadId

-- | Get the server URL, for sanity checking.
serverUrl :: Server -> String
serverUrl Server {urlPrefix, port} = urlPrefix ++ ":" ++ show port

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
  globalCtx <- initGlobalCtx env metadataDbUrl rci
  do
    (ekgStore, serverMetrics) <-
      liftIO $ do
        store <- EKG.newStore @TestMetricsSpec
        serverMetrics <-
          liftIO $ createServerMetrics $ EKG.subset ServerSubset store
        pure (EKG.subset EKG.emptyOf store, serverMetrics)
    runManagedT (initialiseServeCtx env globalCtx serveOptions) $ \serveCtx ->
      do
        let Loggers _ _logger pgLogger = _scLoggers serveCtx
        flip runPGMetadataStorageAppT (_scMetadataDbPool serveCtx, pgLogger)
          . lowerManagedT
          $ do
            runHGEServer
              (const $ pure ())
              env
              serveOptions
              serveCtx
              initTime
              Nothing
              serverMetrics
              ekgStore
