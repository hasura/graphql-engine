{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

-- | Helpers building tests of subscriptions.
--
-- A subscription is a live query where the result is received each time the data
-- changes.
--
-- Creating a subscription query is done in two parts. First, we initiate a connection
-- with the server via websockets, then we send our query along with an id number.
--
-- The server will send back multiple messages, each with a @"type"@ field which indicate
-- their purpose. Some are "acknowledge connection" (@connection_ack@),
-- or "keep alive" (@ka@), others are actual data.
module Harness.Subscriptions
  ( -- * Subscriptions
    SubscriptionHandle,
    withSubscriptions,
    withSubscriptionsHeaders,
    getNextResponse,
  )
where

-------------------------------------------------------------------------------

import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM (atomically, newTVarIO, readTVar, writeTVar)
import Data.Aeson
import Data.Aeson.QQ
import Data.Aeson.Types (Pair)
import Data.ByteString.Lazy.Char8 qualified as Char8
import Data.Has
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Harness.Exceptions (throw, withFrozenCallStack)
import Harness.Logging.Messages
import Harness.Services.GraphqlEngine
import Harness.TestEnvironment
  ( GlobalTestEnvironment (..),
    Server (..),
  )
import Harness.WebSockets (responseListener, sendMessages)
import Hasura.Prelude
import Network.WebSockets qualified as WS
import System.Timeout (timeout)
import Test.Hspec

-- | A subscription's connection initiation message.
initMessage :: HgeServerInstance -> [(T.Text, T.Text)] -> Value
initMessage hgeInstance headers =
  [aesonQQ|
  {
    "type": "connection_init",
    "payload": {
      "headers": #{hdrs},
      "lazy": true
    }
  }
  |]
  where
    hdrs = mkInitMessageHeaders hgeInstance headers

-- | A subscription's start query message.
startQueryMessage :: Int -> Value -> [Pair] -> Value
startQueryMessage subId query extras =
  object
    [ "id" .= String (tshow subId),
      "type" .= String "start",
      "payload" .= object (["query" .= query] <> extras)
    ]

-- | A handle to an active subscription. Can be queried for the next message received.
newtype SubscriptionHandle = SubscriptionHandle {unSubscriptionHandle :: MVar Value}

-- withSubscriptions :: (a -> TestEnvironment) -> SpecWith (Value -> [Pair] -> IO SubscriptionHandle, a) -> SpecWith a

-- | A Spec transformer that sets up the ability to run subscriptions against a HGE instance.
-- Example usage:
--
-- > spec :: SpecWith (TestEnvironment)
-- > spec = do
-- >   describe "subscriptions" $
-- >     withSubscriptions subscriptionsSpec
--
-- > subscriptionsSpec :: SpecWith (Value -> [Pair] -> IO SubscriptionHandle, TestEnvironment)
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
withSubscriptions ::
  ( Has HgeServerInstance env,
    Has GlobalTestEnvironment env,
    Has Logger env
  ) =>
  SpecWith (Value -> [Pair] -> IO SubscriptionHandle, env) ->
  SpecWith env
withSubscriptions = withSubscriptionsHeaders []

withSubscriptionsHeaders ::
  ( Has HgeServerInstance env,
    Has GlobalTestEnvironment env,
    Has Logger env
  ) =>
  [(T.Text, T.Text)] ->
  SpecWith (Value -> [Pair] -> IO SubscriptionHandle, env) ->
  SpecWith env
withSubscriptionsHeaders headers = aroundAllWith \actionWithSubAndTest testEnv -> do
  let hgeInstance = getter @HgeServerInstance testEnv
  let globalEnv = getter @GlobalTestEnvironment testEnv
  WS.runClient "127.0.0.1" (fromIntegral $ port $ server globalEnv) "/v1/graphql" \conn -> do
    -- CAVE: loads of stuff still outstanding:
    --  * trimming threads, NDAT-228
    --  * multiplexing handles, NDAT-229
    --  * timeouts on blocking operations, NDAT-230

    -- send initialization message
    sendMessages testEnv conn [initMessage hgeInstance headers]

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

        -- listens for server responses and populates @handlers@ with the new
        -- response. It will only read one message at a time because it is
        -- blocked by reading/writing to the MVar. Will throw an exception to
        -- the other thread if it encounters an error.
        listener :: IO ()
        listener = responseListener testEnv conn \identifier type' payload -> do
          when (type' == "connection_error") $ fail ("Connection error message received. Payload: " <> Char8.unpack (encode payload))
          identifier' <- identifier `onNothing` fail "Missing handler identifier"
          readIORef handlers >>= \mvars ->
            case Map.lookup identifier' mvars of
              Just mvar -> putMVar mvar payload
              Nothing -> fail "Unexpected handler identifier"

          listener

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
          testLogMessage testEnv $ LogSubscriptionInit query
          sendMessages testEnv conn [startQueryMessage subId query extras]
          pure $ SubscriptionHandle messageBox

        handleExceptionsAndTimeout action = do
          let time = seconds subscriptionsTimeoutTime

          timeout (fromIntegral $ diffTimeToMicroSeconds time) action >>= \case
            Nothing -> throw $ userError do
              "Subscription exceeded the allotted time of: " <> show time
            Just _ -> pure ()

    -- @withAsync@ will take care of cancelling the 'listener' thread
    -- for us once the test has been executed.
    Async.withAsync (handleExceptionsAndTimeout listener) \_ -> do
      actionWithSubAndTest (mkSub, testEnv)

-- | Get the next response received on a subscription.
-- Blocks until data is available.
getNextResponse :: (HasCallStack) => SubscriptionHandle -> IO Value
getNextResponse handle = do
  let time = seconds subscriptionsTimeoutTime
  res <- timeout (fromIntegral $ diffTimeToMicroSeconds time) $ takeMVar (unSubscriptionHandle handle)
  onNothing res $ withFrozenCallStack $ error ("Waiting for a response exceeded the allotted time of: " <> show time)

subscriptionsTimeoutTime :: Seconds
subscriptionsTimeoutTime = 20

mkInitMessageHeaders :: HgeServerInstance -> [(T.Text, T.Text)] -> Value
mkInitMessageHeaders hgeInstance hdrs =
  ( toJSON
      $ Map.fromList
      $ [ ("content-type", "application/json"),
          ("X-Hasura-Admin-Secret", hgeAdminSecret hgeInstance)
        ]
      <> hdrs
  )
