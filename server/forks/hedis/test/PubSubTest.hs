{-# LANGUAGE CPP, OverloadedStrings, DeriveDataTypeable #-}
module PubSubTest (testPubSubThreaded) where

import Control.Concurrent
import Control.Monad
import Control.Concurrent.Async
import Control.Exception
import Data.Typeable
import qualified Data.List
import Data.Text
import Data.ByteString
import Control.Concurrent.STM
import qualified Test.Framework as Test
import qualified Test.Framework.Providers.HUnit as Test (testCase)
import qualified Test.HUnit as HUnit

import Database.Redis

testPubSubThreaded :: [Connection -> Test.Test]
testPubSubThreaded = [removeAllTest, callbackErrorTest, removeFromUnregister]

-- | A handler label to be able to distinguish the handlers from one another
-- to help make sure we unregister the correct handler.
type HandlerLabel = Text

data TestMsg = MsgFromChannel HandlerLabel ByteString
             | MsgFromPChannel HandlerLabel RedisChannel ByteString
  deriving (Show, Eq)

type MsgVar = TVar [TestMsg]

-- | A handler that just writes the message into the TVar
handler :: HandlerLabel -> MsgVar -> MessageCallback
handler label ref msg = atomically $
  modifyTVar ref $ \x -> x ++ [MsgFromChannel label msg]

-- | A pattern handler that just writes the message into the TVar
phandler :: HandlerLabel -> MsgVar -> PMessageCallback
phandler label ref chan msg = atomically $
  modifyTVar ref $ \x -> x ++ [MsgFromPChannel label chan msg]

-- | Wait for a given message to be received
waitForMessage :: MsgVar -> HandlerLabel -> ByteString -> IO ()
waitForMessage ref label msg = atomically $ do
  let expected = MsgFromChannel label msg
  lst <- readTVar ref
  unless (expected `Prelude.elem` lst) retry
  writeTVar ref $ Prelude.filter (/= expected) lst

-- | Wait for a given pattern message to be received
waitForPMessage :: MsgVar -> HandlerLabel -> RedisChannel -> ByteString -> IO ()
waitForPMessage ref label chan msg = atomically $ do
  let expected = MsgFromPChannel label chan msg
  lst <- readTVar ref
  unless (expected `Prelude.elem` lst) retry
  writeTVar ref $ Prelude.filter (/= expected) lst

expectRedisChannels :: Connection -> [RedisChannel] -> IO ()
expectRedisChannels conn expected = do
  actual <- runRedis conn $ sendRequest ["PUBSUB", "CHANNELS"]
  case actual of
    Left err -> HUnit.assertFailure $ "Error geting channels: " ++ show err
    Right s -> HUnit.assertEqual "redis channels" (Data.List.sort s) (Data.List.sort expected)

-- | Test basic messages, plus using removeChannels
removeAllTest :: Connection -> Test.Test
removeAllTest conn = Test.testCase "Multithreaded Pub/Sub - basic" $ do
  msgVar <- newTVarIO []
  initialComplete <- newTVarIO False
  ctrl <- newPubSubController [("foo1", handler "InitialFoo1" msgVar), ("foo2", handler "InitialFoo2" msgVar)]
                              [("bar1:*", phandler "InitialBar1" msgVar), ("bar2:*", phandler "InitialBar2" msgVar)]
  withAsync (pubSubForever conn ctrl (atomically $ writeTVar initialComplete True)) $ \_ -> do
    -- wait for initial
    atomically $ readTVar initialComplete >>= \b -> if b then return () else retry
    expectRedisChannels conn ["foo1", "foo2"]

    runRedis conn $ publish "foo1" "Hello"
    waitForMessage msgVar "InitialFoo1" "Hello"

    runRedis conn $ publish "bar2:zzz" "World"
    waitForPMessage msgVar "InitialBar2" "bar2:zzz" "World"

    -- subscribe to foo1 and bar1 again
    addChannelsAndWait ctrl [("foo1", handler "NewFoo1" msgVar)] [("bar1:*", phandler "NewBar1" msgVar)]
    expectRedisChannels conn ["foo1", "foo2"]

    runRedis conn $ publish "foo1" "abcdef"
    waitForMessage msgVar "InitialFoo1" "abcdef"
    waitForMessage msgVar "NewFoo1" "abcdef"

    -- unsubscribe from foo1 and bar1
    removeChannelsAndWait ctrl ["foo1", "unusued"] ["bar1:*", "unused:*"]
    expectRedisChannels conn ["foo2"]

    -- foo2 and bar2 are still subscribed
    runRedis conn $ publish "foo2" "12345"
    waitForMessage msgVar "InitialFoo2" "12345"

    runRedis conn $ publish "bar2:aaa" "0987"
    waitForPMessage msgVar "InitialBar2" "bar2:aaa" "0987"

data TestError = TestError ByteString
  deriving (Eq, Show, Typeable)
instance Exception TestError

-- | Test an error thrown from a message handler
callbackErrorTest :: Connection -> Test.Test
callbackErrorTest conn = Test.testCase "Multithreaded Pub/Sub - error in handler" $ do
  initialComplete <- newTVarIO False
  ctrl <- newPubSubController [("foo", throwIO . TestError)] []

  thread <- async (pubSubForever conn ctrl (atomically $ writeTVar initialComplete True))
  atomically $ readTVar initialComplete >>= \b -> if b then return () else retry

  runRedis conn $ publish "foo" "Hello"

  ret <- waitCatch thread
  case ret of
    Left (SomeException e) | cast e == Just (TestError "Hello") -> return ()
    _ -> HUnit.assertFailure $ "Did not properly throw error from message thread " ++ show ret

-- | Test removing channels by using the return value of 'addHandlersAndWait'.
removeFromUnregister :: Connection -> Test.Test
removeFromUnregister conn = Test.testCase "Multithreaded Pub/Sub - unregister handlers" $ do
  msgVar <- newTVarIO []
  initialComplete <- newTVarIO False
  ctrl <- newPubSubController [] []
  withAsync (pubSubForever conn ctrl (atomically $ writeTVar initialComplete True)) $ \_ -> do
    atomically $ readTVar initialComplete >>= \b -> if b then return () else retry

    -- register to some channels
    void $ addChannelsAndWait ctrl
        [("abc", handler "InitialAbc" msgVar), ("xyz", handler "InitialXyz" msgVar)]
        [("def:*", phandler "InitialDef" msgVar), ("uvw", phandler "InitialUvw" msgVar)]
    expectRedisChannels conn ["abc", "xyz"]

    runRedis conn $ publish "abc" "Hello"
    waitForMessage msgVar "InitialAbc" "Hello"

    -- register to some more channels
    unreg <- addChannelsAndWait ctrl
        [("abc", handler "SecondAbc"  msgVar), ("123", handler "Second123" msgVar)]
        [("def:*", phandler "SecondDef" msgVar), ("890:*", phandler "Second890" msgVar)]
    expectRedisChannels conn ["abc", "xyz", "123"]

    -- check messages on all channels
    runRedis conn $ publish "abc" "World"
    waitForMessage msgVar "InitialAbc" "World"
    waitForMessage msgVar "SecondAbc" "World"

    runRedis conn $ publish "123" "World2"
    waitForMessage msgVar "Second123" "World2"

    runRedis conn $ publish "def:bbbb" "World3"
    waitForPMessage msgVar "InitialDef" "def:bbbb" "World3"
    waitForPMessage msgVar "SecondDef" "def:bbbb" "World3"

    runRedis conn $ publish "890:tttt" "World4"
    waitForPMessage msgVar "Second890" "890:tttt" "World4"

    -- unregister
    unreg

    -- we have no way of waiting until unregister actually happened, so just delay and hope
    threadDelay $ 1000*1000 -- 1 second
    expectRedisChannels conn ["abc", "xyz"]

    -- now only initial should be around. In particular, abc should still be subscribed
    runRedis conn $ publish "abc" "World5"
    waitForMessage msgVar "InitialAbc" "World5"

    runRedis conn $ publish "def:cccc" "World6"
    waitForPMessage msgVar "InitialDef" "def:cccc" "World6"
