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
    postExplain,
    exportMetadata,
    reloadMetadata,
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

    -- * Subscriptions
    SubscriptionHandle,
    withSubscriptions,
    getNextResponse,
  )
where

-------------------------------------------------------------------------------

import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.Extended (sleep)
import Control.Concurrent.STM (atomically, newTVarIO, readTVar, writeTVar)
import Control.Lens (preview)
import Control.Monad.Trans.Managed (ManagedT (..), lowerManagedT)
import Data.Aeson
import Data.Aeson.Encode.Pretty as AP
import Data.Aeson.Lens (key, _String)
import Data.Aeson.QQ.Simple
import Data.Aeson.Types (Pair)
import Data.Environment qualified as Env
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import Harness.Constants qualified as Constants
import Harness.Exceptions (bracket, throw, withFrozenCallStack)
import Harness.Http qualified as Http
import Harness.Quoter.Yaml (yaml)
import Harness.TestEnvironment (BackendSettings (..), Server (..), TestEnvironment (..), getServer, serverUrl, testLog, testLogBytestring)
import Hasura.App (Loggers (..), ServeCtx (..))
import Hasura.App qualified as App
import Hasura.Logging (Hasura)
import Hasura.Prelude
import Hasura.RQL.Types.Common (PGConnectionParams (..), UrlConf (..))
import Hasura.Server.Init (PostgresConnInfo (..), ServeOptions (..), unsafePort)
import Hasura.Server.Metrics (ServerMetricsSpec, createServerMetrics)
import Hasura.Server.Prometheus (makeDummyPrometheusMetrics)
import Network.Socket qualified as Socket
import Network.Wai.Handler.Warp qualified as Warp
import Network.WebSockets qualified as WS
import System.Metrics qualified as EKG
import System.Timeout (timeout)
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
postWithHeaders =
  withFrozenCallStack $ postWithHeadersStatus 200

-- | Post some JSON to graphql-engine, getting back more JSON.
--
-- Expecting non-200 status code; use @'postWithHeaders' if you want to test for
-- success reponse.
--
-- Note: We add 'withFrozenCallStack' to reduce stack trace clutter.
postWithHeadersStatus ::
  HasCallStack => Int -> TestEnvironment -> String -> Http.RequestHeaders -> Value -> IO Value
postWithHeadersStatus statusCode testEnv@(getServer -> Server {urlPrefix, port}) path headers requestBody = do
  testLog testEnv $ "Posting to " <> path
  testLogBytestring testEnv $ "Request body: " <> AP.encodePretty requestBody
  responseBody <- withFrozenCallStack $ Http.postValueWithStatus statusCode (urlPrefix ++ ":" ++ show port ++ path) headers requestBody
  testLogBytestring testEnv $ "Response body: " <> AP.encodePretty responseBody
  pure responseBody

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

-- | post to /v1/graphql/explain endpoint
postExplain :: HasCallStack => TestEnvironment -> Value -> IO Value
postExplain testEnvironment value =
  withFrozenCallStack $
    post
      testEnvironment
      "/v1/graphql/explain"
      [yaml|
          query:
            query: *value
        |]

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
startServerThread :: BackendSettings -> Maybe (String, Int) -> IO Server
startServerThread backendSettings murlPrefixport = do
  (urlPrefix, port, thread) <-
    case murlPrefixport of
      Just (urlPrefix, port) -> do
        thread <- Async.async (forever (sleep 1)) -- Just wait.
        pure (urlPrefix, port, thread)
      Nothing -> do
        port <- bracket (Warp.openFreePort) (Socket.close . snd) (pure . fst)
        let urlPrefix = "http://127.0.0.1"
        thread <-
          Async.async (runApp backendSettings Constants.serveOptions {soPort = unsafePort port})
        pure (urlPrefix, port, thread)
  let server = Server {port = fromIntegral port, urlPrefix, thread}
  Http.healthCheck (serverUrl server)
  pure server

-------------------------------------------------------------------------------

-- | Run the graphql-engine server.
runApp :: BackendSettings -> ServeOptions Hasura.Logging.Hasura -> IO ()
runApp backendSettings serveOptions = do
  let rci =
        PostgresConnInfo
          { _pciDatabaseConn =
              Just
                ( UrlFromParams
                    PGConnectionParams
                      { _pgcpHost = T.pack Constants.postgresHost,
                        _pgcpUsername = T.pack Constants.postgresUser,
                        _pgcpPassword = Just (T.pack Constants.postgresPassword),
                        _pgcpPort = fromIntegral (Constants.postgresPort backendSettings),
                        _pgcpDatabase = T.pack Constants.postgresDb
                      }
                ),
            _pciRetries = Nothing
          }
      metadataDbUrl = Just Constants.postgresqlMetadataConnectionString
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
    prometheusMetrics <- makeDummyPrometheusMetrics
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
              Nothing
              prometheusMetrics

-- | Used only for 'runApp' above.
data TestMetricsSpec name metricType tags
  = ServerSubset (ServerMetricsSpec name metricType tags)

-- * Subscriptions

--

-- $ subscriptions
-- A subscription is a live query where the result is received each time the data
-- changes.
--
-- Creating a subscription query is done in two parts. First, we initiate a connection
-- with the server via websockets, then we send our query along with an id number.
--
-- The server will send back multiple messages, each with a @"type"@ field which indicate
-- their purpose. Some are "acknowledge connection" (@connection_ack@),
-- or "keep alive" (@ka@), others are actual data.

-- | A subscription's connection initiation message.
initMessage :: Value
initMessage =
  [aesonQQ|
  {
    "type": "connection_init",
    "payload": {
      "headers": {
        "content-type": "application/json"
      },
      "lazy": true
    }
  }
  |]

-- | A subscription's start query message.
startQueryMessage :: Int -> Value -> [Pair] -> Value
startQueryMessage subId query extras =
  object
    [ "id" .= String (tshow subId),
      "type" .= String "start",
      "payload" .= object (["query" .= query] <> extras)
    ]

-- | A handle to an active subscription. Can be queried for the next message received.
newtype SubscriptionHandle = SubscriptionHandle {unSubscriptionHandle :: MVar (Either String Value)}

-- | A Spec transformer that sets up the ability to run subscriptions against a HGE instance.
-- Example usage:
--
-- > spec :: SpecWith (TestEnvironment)
-- > spec = do
-- >   describe "subscriptions" $
-- >     withSubscriptions $ subscriptionsSpec
--
-- > subscriptionsSpec :: SpecWith (Value -> IO SubscriptionHandle, TestEnvironment)
-- > subscriptionsSpec = do
-- >   it "works" $ \(mkSubscription, _te) -> do
-- >     let schemaName :: Schema.SchemaName
-- >         schemaName = Schema.getSchemaName testEnvironment
-- >     query <- mkSubscription "[graphql| subscription { #{schemaName}_example { id, name }} |]"
-- >     let expected :: Value
-- >         expected =
-- >           [yaml|
-- >             data:
-- >               hasura_example: []
-- >           |]
-- >         actual :: IO Value
-- >         actual = getNextResponse query
-- >     actual `shouldBe` expected
withSubscriptions :: SpecWith (Value -> [Pair] -> IO SubscriptionHandle, TestEnvironment) -> SpecWith TestEnvironment
withSubscriptions = aroundAllWith \actionWithSubAndTest testEnvironment -> do
  WS.runClient "127.0.0.1" (fromIntegral $ port $ server testEnvironment) "/v1/graphql" \conn -> do
    -- CAVE: loads of stuff still outstanding:
    --  * trimming threads, NDAT-228
    --  * multiplexing handles, NDAT-229
    --  * timeouts on blocking operations, NDAT-230

    -- send initialization message
    WS.sendTextData conn (encode initMessage)

    -- Open communication channel with responses.
    --
    -- TODO: this currently doesn't capture any notion of ordering across
    -- subscriptions (only within a single subscription). This might be
    -- something we want to change in future. For now, we can tell that
    -- response @S@ comes before response @T@ with a single identifier.
    handlers <- newIORef Map.empty

    -- counter for subscriptions
    subNextId <- newTVarIO 1

    let -- Shorthand for an 'atomicModifyIORef'' call that returns no information.
        atomicModify :: IORef x -> (x -> x) -> IO ()
        atomicModify ref f = atomicModifyIORef' ref \x -> (f x, ())

        -- Convenience function for converting JSON values to strings.
        jsonToString :: Value -> String
        jsonToString = T.unpack . WS.fromLazyByteString . encode

        -- Is this an actual message or client/server busywork?
        isInteresting :: Value -> Bool
        isInteresting res =
          preview (key "type") res
            `notElem` [ Just "ka", -- keep alive
                        Just "connection_ack" -- connection acknowledged
                      ]

        -- listens for server responses and populates @handlers@ with the new
        -- response. It will only read one message at a time because it is
        -- blocked by reading/writing to the MVar. Will throw an exception to
        -- the other thread if it encounters an error.
        responseListener :: IO ()
        responseListener = do
          msgBytes <- WS.receiveData conn
          case eitherDecode msgBytes of
            Left err -> do
              testLog testEnvironment $ "Subscription decode failed: " ++ err
              testLogBytestring testEnvironment $ "Payload was: " <> msgBytes
              throw $ userError (unlines ["Subscription decode failed: " <> err, "Payload: " <> show msgBytes])
            Right msg -> do
              when (isInteresting msg) do
                testLog testEnvironment $ "subscriptions message: " ++ jsonToString msg

                let maybePayload :: Maybe Value
                    maybePayload = preview (key "payload") msg

                    maybeIdentifier :: Maybe Text
                    maybeIdentifier = preview (key "id" . _String) msg

                case liftA2 (,) maybePayload maybeIdentifier of
                  Nothing -> do
                    testLog testEnvironment "Unable to parse message"
                    throw $ userError ("Unable to parse message: " ++ show msg)
                  Just (payload, identifier) ->
                    readIORef handlers >>= \mvars ->
                      case Map.lookup identifier mvars of
                        Just mvar -> putMVar mvar (Right payload)
                        Nothing -> throw (userError "Unexpected handler identifier")

              responseListener

        -- Create a subscription over this websocket connection. Will be used
        -- by the user to create new subscriptions. The handled can be used to
        -- request the next response.
        mkSub :: Value -> [Pair] -> IO SubscriptionHandle
        mkSub query extras = do
          -- each subscription has an id, this manages a new id for each subscription.
          subId <- atomically do
            currSubId <- readTVar subNextId
            let !next = currSubId + 1

            writeTVar subNextId next
            pure currSubId

          messageBox <- newEmptyMVar
          atomicModify handlers (Map.insert (tshow subId) messageBox)

          -- initialize a connection.
          testLog testEnvironment ("Initialising websocket connection")
          testLogBytestring testEnvironment (encode query)
          WS.sendTextData conn (encode $ startQueryMessage subId query extras)
          pure $ SubscriptionHandle messageBox

        handleExceptionsAndTimeout action = do
          let time = seconds subscriptionsTimeoutTime

          timeout (fromIntegral $ diffTimeToMicroSeconds time) action >>= \case
            Nothing -> throw $ userError do
              "Subscription exceeded the allotted time of: " <> show time
            Just _ -> pure ()

    -- @withAsync@ will take care of cancelling the 'responseListener' thread
    -- for us once the test has been executed.
    Async.withAsync (handleExceptionsAndTimeout responseListener) \_ -> do
      actionWithSubAndTest (mkSub, testEnvironment)

-- | Get the next response received on a subscription.
-- Blocks until data is available.
getNextResponse :: HasCallStack => SubscriptionHandle -> IO Value
getNextResponse handle = do
  let time = seconds subscriptionsTimeoutTime
  res <- timeout (fromIntegral $ diffTimeToMicroSeconds time) $ takeMVar (unSubscriptionHandle handle)
  case res of
    Nothing -> withFrozenCallStack $ error ("Waiting for a response exceeded the allotted time of: " <> show time)
    Just (Left err) -> withFrozenCallStack $ error err
    Just (Right val) -> pure val

subscriptionsTimeoutTime :: Seconds
subscriptionsTimeoutTime = 20
