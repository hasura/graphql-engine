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
    getNextResponse,
  )
where

-------------------------------------------------------------------------------

import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM (atomically, newTVarIO, readTVar, writeTVar)
import Control.Lens (preview)
import Data.Aeson
import Data.Aeson.Lens (key, _String)
import Data.Aeson.QQ.Simple
import Data.Aeson.Types (Pair)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Map.Strict qualified as Map
import Harness.Exceptions (throw, withFrozenCallStack)
import Harness.Logging.Messages
import Harness.TestEnvironment
  ( GlobalTestEnvironment (..),
    Server (..),
    TestEnvironment (..),
    testLogMessage,
  )
import Hasura.Prelude
import Network.WebSockets qualified as WS
import System.Timeout (timeout)
import Test.Hspec

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
newtype SubscriptionHandle = SubscriptionHandle {unSubscriptionHandle :: MVar Value}

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
  WS.runClient "127.0.0.1" (fromIntegral $ port $ server $ globalEnvironment testEnvironment) "/v1/graphql" \conn -> do
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
              throw $ userError (unlines ["Subscription decode failed: " <> err, "Payload: " <> show msgBytes])
            Right msg -> do
              when (isInteresting msg) do
                testLogMessage testEnvironment $ LogSubscriptionResponse msg

                let maybePayload :: Maybe Value
                    maybePayload = preview (key "payload") msg

                    maybeIdentifier :: Maybe Text
                    maybeIdentifier = preview (key "id" . _String) msg

                case liftA2 (,) maybePayload maybeIdentifier of
                  Nothing -> do
                    throw $ userError ("Unable to parse message: " ++ show msg)
                  Just (payload, identifier) ->
                    readIORef handlers >>= \mvars ->
                      case Map.lookup identifier mvars of
                        Just mvar -> putMVar mvar payload
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
          testLogMessage testEnvironment $ LogSubscriptionInit query
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
  onNothing res $ withFrozenCallStack $ error ("Waiting for a response exceeded the allotted time of: " <> show time)

subscriptionsTimeoutTime :: Seconds
subscriptionsTimeoutTime = 20
