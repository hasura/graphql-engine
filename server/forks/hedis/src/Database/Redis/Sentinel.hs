{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StrictData         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | "Database.Redis" like interface with connection through Redis Sentinel.
--
-- More details here: <https://redis.io/topics/sentinel>.
--
-- Example:
--
-- @
-- conn <- 'connect' 'SentinelConnectionInfo' (("localhost", PortNumber 26379) :| []) "mymaster" 'defaultConnectInfo'
--
-- 'runRedis' conn $ do
--   'set' "hello" "world"
-- @
--
-- When connection is opened, the Sentinels will be queried to get current master. Subsequent 'runRedis'
-- calls will talk to that master.
--
-- If 'runRedis' call fails, the next call will choose a new master to talk to.
--
-- This implementation is based on Gist by Emanuel Borsboom
-- at <https://gist.github.com/borsboom/681d37d273d5c4168723>
module Database.Redis.Sentinel
  (
    -- * Connection
    SentinelConnectInfo(..)
  , SentinelConnection
  , connect
    -- * runRedis with Sentinel support
  , runRedis
  , RedisSentinelException(..)

    -- * Re-export Database.Redis
  , module Database.Redis
  ) where

import           Control.Concurrent
import           Control.Exception     (Exception, IOException, evaluate, throwIO)
import           Control.Monad
import           Control.Monad.Catch   (Handler (..), MonadCatch, catches, throwM)
import           Control.Monad.Except
import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BS8
import           Data.Foldable         (toList)
import           Data.List             (delete)
import           Data.List.NonEmpty    (NonEmpty (..))
import           Data.Typeable         (Typeable)
import           Data.Unique
import           Network.Socket        (HostName)

import           Database.Redis hiding (Connection, connect, runRedis)
import qualified Database.Redis as Redis

-- | Interact with a Redis datastore.  See 'Database.Redis.runRedis' for details.
runRedis :: SentinelConnection
         -> Redis (Either Reply a)
         -> IO (Either Reply a)
runRedis (SentinelConnection connMVar) action = do
  (baseConn, preToken) <- modifyMVar connMVar $ \oldConnection@SentinelConnection'
          { rcCheckFailover
          , rcToken = oldToken
          , rcSentinelConnectInfo = oldConnectInfo
          , rcMasterConnectInfo = oldMasterConnectInfo
          , rcBaseConnection = oldBaseConnection } ->
      if rcCheckFailover
        then do
          (newConnectInfo, newMasterConnectInfo) <- updateMaster oldConnectInfo
          newToken <- newUnique
          (connInfo, conn) <-
            if sameHost newMasterConnectInfo oldMasterConnectInfo
              then return (oldMasterConnectInfo, oldBaseConnection)
              else do
                newConn <- Redis.connect newMasterConnectInfo
                return (newMasterConnectInfo, newConn)

          return
            ( SentinelConnection'
              { rcCheckFailover = False
              , rcToken = newToken
              , rcSentinelConnectInfo = newConnectInfo
              , rcMasterConnectInfo = connInfo
              , rcBaseConnection = conn
              }
            , (conn, newToken)
            )
        else return (oldConnection, (oldBaseConnection, oldToken))

  -- Use evaluate to make sure we catch exceptions from 'runRedis'.
  reply <- (Redis.runRedis baseConn action >>= evaluate)
    `catchRedisRethrow` (\_ -> setCheckSentinel preToken)
  case reply of
    Left (Error e) | "READONLY " `BS.isPrefixOf` e ->
        -- This means our connection has turned into a slave
        setCheckSentinel preToken
    _ -> return ()
  return reply

  where
    sameHost :: Redis.ConnectInfo -> Redis.ConnectInfo -> Bool
    sameHost l r = connectHost l == connectHost r && connectPort l == connectPort r

    setCheckSentinel preToken = modifyMVar_ connMVar $ \conn@SentinelConnection'{rcToken} ->
      if preToken == rcToken
        then do
          newToken <- newUnique
          return (conn{rcToken = newToken, rcCheckFailover = True})
        else return conn


connect :: SentinelConnectInfo -> IO SentinelConnection
connect origConnectInfo = do
  (connectInfo, masterConnectInfo) <- updateMaster origConnectInfo
  conn <- Redis.connect masterConnectInfo
  token <- newUnique

  SentinelConnection <$> newMVar SentinelConnection'
    { rcCheckFailover = False
    , rcToken = token
    , rcSentinelConnectInfo = connectInfo
    , rcMasterConnectInfo = masterConnectInfo
    , rcBaseConnection = conn
    }

updateMaster :: SentinelConnectInfo
             -> IO (SentinelConnectInfo, Redis.ConnectInfo)
updateMaster sci@SentinelConnectInfo{..} = do
    -- This is using the Either monad "backwards" -- Left means stop because we've made a connection,
    -- Right means try again.
    resultEither <- runExceptT $ forM_ connectSentinels $ \(host, port) -> do
      trySentinel host port `catchRedis` (\_ -> return ())


    case resultEither of
        Left (conn, sentinelPair) -> return
          ( sci
            { connectSentinels = sentinelPair :| delete sentinelPair (toList connectSentinels)
            }
          , conn
          )
        Right () -> throwIO $ NoSentinels connectSentinels
  where
    trySentinel :: HostName -> PortID -> ExceptT (Redis.ConnectInfo, (HostName, PortID)) IO ()
    trySentinel sentinelHost sentinelPort = do
      -- bang to ensure exceptions from runRedis get thrown immediately.
      !replyE <- liftIO $ do
        !sentinelConn <- Redis.connect $ Redis.defaultConnectInfo
            { connectHost = sentinelHost
            , connectPort = sentinelPort
            , connectMaxConnections = 1
            }
        Redis.runRedis sentinelConn $ sendRequest
          ["SENTINEL", "get-master-addr-by-name", connectMasterName]

      case replyE of
        Right [host, port] ->
          throwError
            ( connectBaseInfo
              { connectHost = BS8.unpack host
              , connectPort =
                  maybe
                    (PortNumber 26379)
                    (PortNumber . fromIntegral . fst)
                    $ BS8.readInt port
              }
            , (sentinelHost, sentinelPort)
            )
        _ -> return ()

catchRedisRethrow :: MonadCatch m => m a -> (String -> m ()) -> m a
catchRedisRethrow action handler =
  action `catches`
    [ Handler $ \ex -> handler (show @IOException ex) >> throwM ex
    , Handler $ \ex -> handler (show @ConnectionLostException ex) >> throwM ex
    ]

catchRedis :: MonadCatch m => m a -> (String -> m a) -> m a
catchRedis action handler =
  action `catches`
    [ Handler $ \ex -> handler (show @IOException ex)
    , Handler $ \ex -> handler (show @ConnectionLostException ex)
    ]

newtype SentinelConnection = SentinelConnection (MVar SentinelConnection')

data SentinelConnection'
  = SentinelConnection'
      { rcCheckFailover       :: Bool
      , rcToken               :: Unique
      , rcSentinelConnectInfo :: SentinelConnectInfo
      , rcMasterConnectInfo   :: Redis.ConnectInfo
      , rcBaseConnection      :: Redis.Connection
      }

-- | Configuration of Sentinel hosts.
data SentinelConnectInfo
  = SentinelConnectInfo
      { connectSentinels  :: NonEmpty (HostName, PortID)
        -- ^ List of sentinels.
      , connectMasterName :: ByteString
        -- ^ Name of master to connect to.
      , connectBaseInfo   :: Redis.ConnectInfo
        -- ^ This is used to configure auth and other parameters for Redis connection,
        -- but 'Redis.connectHost' and 'Redis.connectPort' are ignored.
      }
  deriving (Show)

-- | Exception thrown by "Database.Redis.Sentinel".
data RedisSentinelException
  = NoSentinels (NonEmpty (HostName, PortID))
    -- ^ Thrown if no sentinel can be reached.
  deriving (Show, Typeable)

deriving instance Exception RedisSentinelException
