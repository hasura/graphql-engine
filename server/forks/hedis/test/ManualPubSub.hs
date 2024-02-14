{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module ManualPubSub (main) where

-- A test for PubSub which must be run manually to be able to kill and restart the redis-server.
-- I execute this with `stack runghc ManualPubSub.hs`

import Database.Redis
import Data.Monoid ((<>))
import Control.Monad
import Control.Exception
import Control.Monad.Trans (liftIO)
import Control.Concurrent
import Control.Concurrent.Async
import Data.Text
import Data.ByteString (ByteString)
import Data.Text.Encoding
import System.IO

-- | publish messages every 2 seconds to several channels
publishThread :: Connection -> IO ()
publishThread c = runRedis c $ loop (0 :: Int)
  where
    loop i = do
      let msg = encodeUtf8 $ pack $ "Publish iteration " ++ show i
      void $ publish "foo" ("foo" <> msg)
      void $ publish "bar" ("bar" <> msg)
      void $ publish "baz:1" ("baz1" <> msg)
      void $ publish "baz:2" ("baz2" <> msg)
      liftIO $ threadDelay $ 2*1000*1000 -- 2 seconds
      loop (i+1)

onInitialComplete :: IO ()
onInitialComplete = hPutStrLn stderr "Initial subscr complete"

handlerThread :: Connection -> PubSubController -> IO ()
handlerThread conn ctrl = forever $
       pubSubForever conn ctrl onInitialComplete
         `catch` (\(e :: SomeException) -> do
           hPutStrLn stderr $ "Got error: " ++ show e
           threadDelay $ 50*1000)

msgHandler :: ByteString -> IO ()
msgHandler msg = hPutStrLn stderr $ "Saw msg: " ++ unpack (decodeUtf8 msg)

pmsgHandler :: RedisChannel -> ByteString -> IO ()
pmsgHandler channel msg = hPutStrLn stderr $ "Saw pmsg: " ++ unpack (decodeUtf8 channel) ++ unpack (decodeUtf8 msg)

showChannels :: Connection -> IO ()
showChannels c = do
  resp :: Either Reply [ByteString] <- runRedis c $ sendRequest ["PUBSUB", "CHANNELS"]
  liftIO $ hPutStrLn stderr $ "Current redis channels: " ++ show resp

main :: IO ()
main = do
  ctrl <- newPubSubController [("foo", msgHandler)] []
  conn <- connect defaultConnectInfo

  withAsync (publishThread conn) $ \_pubT -> do
  withAsync (handlerThread conn ctrl) $ \_handlerT -> do

  void $ hPutStrLn stderr "Press enter to subscribe to bar" >> getLine
  void $ addChannels ctrl [("bar", msgHandler)] []

  void $ hPutStrLn stderr "Press enter to subscribe to baz:*" >> getLine
  void $ addChannels ctrl [] [("baz:*", pmsgHandler)]

  void $ hPutStrLn stderr "Press enter to unsub from foo" >> getLine
  removeChannels ctrl ["foo"] []

  void $ hPutStrLn stderr "Try killing and restarting the redis server" >> getLine
  withAsync (publishThread conn) $ \_pubT -> do

  void $ hPutStrLn stderr "Press enter to unsub from baz:*" >> getLine
  removeChannels ctrl [] ["baz:*"]

  void $ hPutStrLn stderr "Press enter to sub to foo and baz:*" >> getLine
  unsub1 <- addChannelsAndWait ctrl [("foo", msgHandler)] [("baz:*", pmsgHandler)]
  showChannels conn

  void $ hPutStrLn stderr "Press enter to sub to foo again and baz:1" >> getLine
  unsub2 <- addChannelsAndWait ctrl [("foo", msgHandler), ("baz:1", msgHandler)] []
  showChannels conn

  void $ hPutStrLn stderr "Press enter to unsub to foo and baz:1" >> getLine
  unsub2

  void $ hPutStrLn stderr "Press enter to unsub to foo and baz:*" >> getLine
  showChannels conn
  unsub1

  void $ hPutStrLn stderr "Press enter to exit" >> getLine
  showChannels conn
