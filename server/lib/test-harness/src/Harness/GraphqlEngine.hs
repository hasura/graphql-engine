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
    postMetadataWithStatusAndHeaders,
    postExplain,
    postExplainWithStatus,
    exportMetadata,
    reloadMetadata,
    postGraphqlYaml,
    postGraphqlYamlWithHeaders,
    postGraphql,
    postGraphqlWithReqHeaders,
    postMetadataWithStatusAndReqHeaders,
    postGraphqlInternal,
    postMetadataInternal,
    postGraphqlWithVariables,
    postGraphqlWithPair,
    postGraphqlWithHeaders,
    postWithHeadersStatus,
    clearMetadata,
    postV1Query,
    postV2Query,
    postV2Query_,

    -- ** Misc.
    setSource,
    setSources,
    addDataConnectorAgent,
    deleteDataConnectorAgent,

    -- * Server Setup
    startServerThread,
    runApp,

    -- * Re-exports
    serverUrl,
    Server,
  )
where

-------------------------------------------------------------------------------

import Control.Concurrent (myThreadId)
import Control.Concurrent.Async qualified as Async
import Control.Monad.Trans.Managed (ManagedT (..), lowerManagedT)
import Data.Aeson (Value (String), encode, fromJSON, object, (.=))
import Data.Aeson.Encode.Pretty as AP
import Data.Aeson.Types (Pair)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy.Char8 qualified as Char8
import Data.CaseInsensitive (original)
import Data.Environment qualified as Env
import Data.Has
import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Time (getCurrentTime)
import Harness.Constants qualified as Constants
import Harness.Exceptions (SomeException (..), bracket, catch, throwTo, withFrozenCallStack)
import Harness.Http qualified as Http
import Harness.Logging
import Harness.Quoter.Yaml (fromYaml, yaml)
import Harness.Services.GraphqlEngine
import Harness.TestEnvironment (Protocol (..), Server (..), TestEnvironment (..), TestingRole (..), getServer, requestProtocol, server, serverUrl, traceIf)
import Harness.WebSockets (responseListener, sendMessages)
import Hasura.App qualified as App
import Hasura.Logging (Hasura)
import Hasura.Prelude
import Hasura.Server.App (CEConsoleType (OSSConsole))
import Hasura.Server.Init (PostgresConnInfo (..), ServeOptions (..), unsafePort)
import Hasura.Server.Metrics (ServerMetricsSpec, createServerMetrics)
import Hasura.Server.Prometheus (makeDummyPrometheusMetrics)
import Hasura.Tracing (sampleAlways)
import Network.Socket qualified as Socket
import Network.Wai.Handler.Warp qualified as Warp
import Network.WebSockets qualified as WS
import System.Metrics qualified as EKG
import Test.Hspec

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
post :: (HasCallStack) => TestEnvironment -> String -> Value -> IO Value
post testEnvironment path v = withFrozenCallStack $ postWithHeaders testEnvironment path mempty v

-- | Same as 'post', but ignores the value.
--
-- See 'postWithHeaders_' to issue a request with 'Http.RequestHeaders'.
--
-- Note: We add 'withFrozenCallStack' to reduce stack trace clutter.
post_ :: (HasCallStack) => TestEnvironment -> String -> Value -> IO ()
post_ testEnvironment path v = void $ withFrozenCallStack $ postWithHeaders_ testEnvironment path mempty v

-- | Post some JSON to graphql-engine, getting back more JSON.
--
-- Optimistically assumes success; use another function if you want to test for
-- failure.
--
-- Note: We add 'withFrozenCallStack' to reduce stack trace clutter.
postWithHeaders ::
  (HasCallStack) => TestEnvironment -> String -> Http.RequestHeaders -> Value -> IO Value
postWithHeaders =
  withFrozenCallStack $ postWithHeadersStatus 200

-- | Post some JSON to graphql-engine, getting back more JSON.
--
-- Expecting non-200 status code; use @'postWithHeaders' if you want to test for
-- success reponse.
--
-- Note: We add 'withFrozenCallStack' to reduce stack trace clutter.
postWithHeadersStatus ::
  (HasCallStack) => Int -> TestEnvironment -> String -> Http.RequestHeaders -> Value -> IO Value
postWithHeadersStatus statusCode testEnv@(getServer -> Server {urlPrefix, port}) path headers requestBody =
  withFrozenCallStack $ do
    testLogMessage testEnv $ LogHGERequest (T.pack path) $ traceIf testEnv requestBody
    responseBody <- Http.postValueWithStatus statusCode (urlPrefix ++ ":" ++ show port ++ path) (addAuthzHeaders testEnv headers) requestBody
    testLogMessage testEnv $ LogHGEResponse (T.pack path) responseBody
    pure responseBody

addAuthzHeaders :: TestEnvironment -> Http.RequestHeaders -> Http.RequestHeaders
addAuthzHeaders testEnv headers =
  let role :: ByteString
      role = case permissions testEnv of
        Admin -> "admin"
        NonAdmin _ -> "test-role"
   in ("X-Hasura-Admin-Secret", "top-secret") : ("X-Hasura-Role", role) : headers

-- | Post some JSON to graphql-engine's GraphQL endpoint, getting back more JSON,
-- either via HTTP or websockets depending on what is configured in the 'TestEnvironment'
--
-- Note: We add 'withFrozenCallStack' to reduce stack trace clutter.
postGraphqlViaHttpOrWebSocketWithHeadersStatus ::
  (HasCallStack) => Int -> TestEnvironment -> Http.RequestHeaders -> Value -> IO Value
postGraphqlViaHttpOrWebSocketWithHeadersStatus statusCode testEnv headers requestBody = do
  withFrozenCallStack $ case requestProtocol (globalEnvironment testEnv) of
    WebSocket -> postWithHeadersStatusViaWebSocket testEnv (addAuthzHeaders testEnv headers) requestBody
    HTTP -> postWithHeadersStatus statusCode testEnv "/v1/graphql" headers requestBody

-- | Post some JSON to graphql-engine, getting back more JSON, via websockets.
--
-- This will be used by 'postWithHeadersStatus' if the 'TestEnvironment' sets
-- the 'requestProtocol' to 'WebSocket'.
postWithHeadersStatusViaWebSocket :: TestEnvironment -> Http.RequestHeaders -> Value -> IO Value
postWithHeadersStatusViaWebSocket testEnv headers requestBody = do
  let preparedHeaders :: HashMap Text ByteString
      preparedHeaders =
        HashMap.fromList
          [ (decodeUtf8 (original key), value)
            | (key, value) <- headers
          ]
      server' = server (globalEnvironment testEnv)
      port' = port server'
      host = T.unpack (T.drop 7 (T.pack (urlPrefix server')))
      path = "/v1/graphql"

  -- paritosh: We are initiating a websocket connection for each request and sending the request instead of initiating a
  -- websocket connection for the test and reusing the connection for requests. This is done to avoid managing the
  -- connection (as we do have metadata changes as part of test and HGE closes websockets on metadata changes).
  WS.runClient host (fromIntegral port') path \connection -> do
    sendMessages
      testEnv
      connection
      [ object
          [ "type" .= String "connection_init",
            "payload" .= object ["headers" .= preparedHeaders]
          ],
        object
          [ "id" .= String "some-request-id",
            "type" .= String "start",
            "payload" .= requestBody
          ]
      ]
    responseListener testEnv connection \_ type' payload -> do
      when (type' `notElem` ["data", "error"]) $ fail ("Websocket message type " <> T.unpack type' <> " received. Payload: " <> Char8.unpack (encode payload))
      pure payload

-- | Post some JSON to graphql-engine, getting back more JSON.
--
-- Optimistically assumes success; use another function if you want to test for
-- failure.
--
-- Note: We add 'withFrozenCallStack' to reduce stack trace clutter.
postWithHeaders_ ::
  (HasCallStack) => TestEnvironment -> String -> Http.RequestHeaders -> Value -> IO ()
postWithHeaders_ testEnvironment path headers v =
  void $ withFrozenCallStack $ postWithHeaders testEnvironment path headers v

-- | Same as 'post', but defaults to the graphql end-point.
--
-- Note: We add 'withFrozenCallStack' to reduce stack trace clutter.
postGraphqlYaml ::
  (HasCallStack) => TestEnvironment -> Value -> IO Value
postGraphqlYaml testEnvironment v = withFrozenCallStack $ postGraphqlYamlWithHeaders testEnvironment mempty v

-- | Same as 'postWithHeaders', but defaults to the graphql end-point.
--
-- Note: We add 'withFrozenCallStack' to reduce stack trace clutter.
postGraphqlYamlWithHeaders ::
  (HasCallStack) => TestEnvironment -> Http.RequestHeaders -> Value -> IO Value
postGraphqlYamlWithHeaders testEnvironment headers =
  withFrozenCallStack $ postGraphqlViaHttpOrWebSocketWithHeadersStatus 200 testEnvironment headers

postGraphql :: (Has PostGraphql testEnvironment) => testEnvironment -> Value -> IO Value
postGraphql testEnv = (getPostGraphql $ getter testEnv) []

postGraphqlWithReqHeaders :: (Has PostGraphql testEnvironment) => testEnvironment -> Http.RequestHeaders -> Value -> IO Value
postGraphqlWithReqHeaders = getPostGraphql . getter

postMetadataWithStatusAndReqHeaders :: (Has PostMetadata testEnvironment) => testEnvironment -> Int -> Http.RequestHeaders -> Value -> IO Value
postMetadataWithStatusAndReqHeaders = getPostMetadata . getter

-- | Same as 'postGraphqlYaml', but adds the @{query:..}@ wrapper.
--
-- Note: We add 'withFrozenCallStack' to reduce stack trace clutter.
postGraphqlInternal :: (HasCallStack) => TestEnvironment -> Http.RequestHeaders -> Value -> IO Value
postGraphqlInternal testEnvironment reqHeaders value =
  withFrozenCallStack $ postGraphqlYamlWithHeaders testEnvironment reqHeaders (object ["query" .= value])

postMetadataInternal :: (HasCallStack) => TestEnvironment -> Int -> Http.RequestHeaders -> Value -> IO Value
postMetadataInternal testEnvironment status reqHeaders value =
  withFrozenCallStack $ postMetadataWithStatusAndHeaders status testEnvironment reqHeaders value

-- | Same as 'postGraphql', but accepts variables to the GraphQL query as well.
postGraphqlWithVariables :: (HasCallStack) => TestEnvironment -> Value -> Value -> IO Value
postGraphqlWithVariables testEnvironment query variables =
  withFrozenCallStack
    $ postGraphqlYaml
      testEnvironment
      ( object
          [ "query" .= query,
            "variables" .= variables
          ]
      )

-- | Same as postGraphql but accepts a list of 'Pair' to pass
-- additional parameters to the endpoint.
postGraphqlWithPair :: (HasCallStack) => TestEnvironment -> Value -> [Pair] -> IO Value
postGraphqlWithPair testEnvironment value pair =
  withFrozenCallStack $ postGraphqlYaml testEnvironment (object $ ["query" .= value] <> pair)

-- | Same as 'postGraphqlYamlWithHeaders', but adds the @{query:..}@ wrapper.
--
-- Note: We add 'withFrozenCallStack' to reduce stack trace clutter.
postGraphqlWithHeaders ::
  (HasCallStack) => TestEnvironment -> Http.RequestHeaders -> Value -> IO Value
postGraphqlWithHeaders testEnvironment headers value =
  withFrozenCallStack $ postGraphqlYamlWithHeaders testEnvironment headers (object ["query" .= value])

-- | post to /v1/graphql/explain endpoint
postExplain :: (HasCallStack) => TestEnvironment -> Value -> IO Value
postExplain = postExplainWithStatus 200

-- | post to /v1/graphql/explain endpoint and expect a specific status
postExplainWithStatus :: (HasCallStack) => Int -> TestEnvironment -> Value -> IO Value
postExplainWithStatus status testEnvironment value =
  withFrozenCallStack
    $ postWithHeadersStatus
      status
      testEnvironment
      "/v1/graphql/explain"
      mempty
      [yaml|
          query:
            query: *value
        |]

-- | Metadata requests don't work over websockets. This overrides the HGE
-- request protocol.
withHTTP :: TestEnvironment -> TestEnvironment
withHTTP testEnvironment =
  testEnvironment
    { globalEnvironment =
        (globalEnvironment testEnvironment)
          { requestProtocol = HTTP
          }
    }

-- | Same as 'post_', but defaults to the @"v1/metadata"@ endpoint.
--
-- @headers@ are mostly irrelevant for the admin endpoint @v1/metadata@.
--
-- Note: We add 'withFrozenCallStack' to reduce stack trace clutter.
postMetadata_ :: (HasCallStack) => TestEnvironment -> Value -> IO ()
postMetadata_ testEnvironment = withFrozenCallStack $ post_ (withHTTP testEnvironment) "/v1/metadata"

postMetadata :: (HasCallStack) => TestEnvironment -> Value -> IO Value
postMetadata testEnvironment = withFrozenCallStack $ post (withHTTP testEnvironment) "/v1/metadata"

postMetadataWithStatus :: (HasCallStack) => Int -> TestEnvironment -> Value -> IO Value
postMetadataWithStatus statusCode testEnvironment v =
  withFrozenCallStack $ postWithHeadersStatus statusCode (withHTTP testEnvironment) "/v1/metadata" mempty v

postMetadataWithStatusAndHeaders :: (HasCallStack) => Int -> TestEnvironment -> Http.RequestHeaders -> Value -> IO Value
postMetadataWithStatusAndHeaders statusCode testEnvironment =
  withFrozenCallStack $ postWithHeadersStatus statusCode (withHTTP testEnvironment) "/v1/metadata"

-- | Resets metadata, removing all sources or remote schemas.
--
-- Note: We add 'withFrozenCallStack' to reduce stack trace clutter.
clearMetadata :: (HasCallStack) => TestEnvironment -> IO ()
clearMetadata s = withFrozenCallStack $ postMetadata_ s [yaml|{type: clear_metadata, args: {}}|]

exportMetadata :: (HasCallStack) => TestEnvironment -> IO Value
exportMetadata s = withFrozenCallStack $ postMetadata s [yaml|{type: export_metadata, args: {}}|]

-- | Reload metadata
reloadMetadata :: TestEnvironment -> IO ()
reloadMetadata testEnvironment =
  postMetadata_
    testEnvironment
    [yaml|
type: reload_metadata
args: {}
  |]

-- | Same as 'postWithHeadersStatus', but defaults to the @"/v2/query"@ endpoint
--
-- @headers@ are mostly irrelevant for the admin endpoint @v2/query@.
--
-- Note: We add 'withFrozenCallStack' to reduce stack trace clutter.
postV2Query :: (HasCallStack) => Int -> TestEnvironment -> Value -> IO Value
postV2Query statusCode testEnvironment =
  withFrozenCallStack $ postWithHeadersStatus statusCode testEnvironment "/v2/query" mempty

postV2Query_ :: (HasCallStack) => TestEnvironment -> Value -> IO ()
postV2Query_ testEnvironment =
  withFrozenCallStack $ post_ testEnvironment "/v2/query"

postV1Query :: (HasCallStack) => Int -> TestEnvironment -> Value -> IO Value
postV1Query statusCode testEnvironment =
  withFrozenCallStack $ postWithHeadersStatus statusCode testEnvironment "/v1/query" mempty

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

-- | Adds a data connector agent to the data connector backend config
addDataConnectorAgent :: TestEnvironment -> String -> String -> IO ()
addDataConnectorAgent testEnvironment name url =
  postMetadata_
    testEnvironment
    [yaml|
      type: dc_add_agent
      args:
        name: *name
        url: *url
    |]

-- | Removes a data connector agent from the data connector backend config
deleteDataConnectorAgent :: TestEnvironment -> String -> IO ()
deleteDataConnectorAgent testEnvironment name =
  postMetadata_
    testEnvironment
    [yaml|
      type: dc_delete_agent
      args:
        name: *name
    |]

-------------------------------------------------------------------------------

-- | Choose a random port and start a graphql-engine server on that
-- port accessible from localhost. It waits until the server is
-- available before returning.
--
-- The port availability is subject to races.
startServerThread :: IO Server
startServerThread = do
  mainThreadId <- myThreadId
  port <- bracket (Warp.openFreePort) (Socket.close . snd) (pure . fst)
  let urlPrefix = "http://127.0.0.1"
      backendConfigs =
        [fromYaml|
        backend_configs:
          dataconnector:
            foobar:
              display_name: FOOBARDB
              uri: "http://localhost:65005" |]
  thread <-
    Async.async
      ( runApp
          Constants.postgresqlMetadataConnectionString
          Constants.serveOptions
            { soPort = unsafePort port,
              soMetadataDefaults = backendConfigs
            }
          `catch` \(SomeException e) -> throwTo mainThreadId e
      )
  let server = Server {port = fromIntegral port, urlPrefix, thread}
  Http.healthCheck (serverUrl server <> "/healthz")
  pure server

-------------------------------------------------------------------------------

-- | Run the graphql-engine server.
runApp :: String -> ServeOptions Hasura.Logging.Hasura -> IO ()
runApp metadataDbUrl serveOptions = do
  let rci =
        PostgresConnInfo
          { _pciDatabaseConn = Nothing,
            _pciRetries = Nothing
          }
  env <- Env.getEnvironment
  initTime <- liftIO getCurrentTime
  metadataConnectionInfo <- App.initMetadataConnectionInfo env (Just metadataDbUrl) rci
  let defaultConnInfo = App.BasicConnectionInfo metadataConnectionInfo Nothing
  (ekgStore, serverMetrics) <-
    liftIO $ do
      store <- EKG.newStore @TestMetricsSpec
      serverMetrics <-
        liftIO $ createServerMetrics $ EKG.subset ServerSubset store
      pure (EKG.subset EKG.emptyOf store, serverMetrics)
  prometheusMetrics <- makeDummyPrometheusMetrics
  let managedServerCtx = App.initialiseAppEnv env defaultConnInfo serveOptions Nothing serverMetrics prometheusMetrics sampleAlways
  runManagedT managedServerCtx \(appInit, appEnv) ->
    App.runAppM appEnv do
      appCtx <- App.initialiseAppContext env serveOptions appInit
      lowerManagedT
        $ App.runHGEServer
          (const $ pure ())
          appCtx
          initTime
          Nothing
          OSSConsole
          ekgStore

-- | Used only for 'runApp' above.
data TestMetricsSpec name metricType tags
  = ServerSubset (ServerMetricsSpec name metricType tags)
