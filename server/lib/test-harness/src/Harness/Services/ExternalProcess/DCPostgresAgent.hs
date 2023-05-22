{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module houses low-level functions and types to help access and work
-- with the Postgres Data Connector Agent
module Harness.Services.ExternalProcess.DCPostgresAgent
  ( -- * Types
    DcPgBinPath (..),
    DcPgAgentInstance (..),
    getDcPgAgentInstanceUrl,
    DcPgConfig (..),
    emptyDcPgConfig,

    -- * Direct spawning
    spawnAgent,

    -- * Pooled access
    DcPgPool (dcPgPoolDestroy),
    mkDcPgInstancePool,
    drawFromPool,
  )
where

import Control.Concurrent (forkIO)
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.MVar
import Control.Lens
import Control.Monad.Managed
import Data.Aeson qualified as J
import Data.ByteString qualified as BS
import Data.Has
import Data.HashMap.Strict qualified as HashMap
import Data.Pool
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Vector (fromList)
import Harness.Exceptions
import Harness.Http qualified as Http
import Harness.Logging
import Hasura.Prelude
import Network.HTTP.Simple qualified as Http
import Network.Socket qualified as Socket
import Network.Wai.Handler.Warp qualified as Warp
import System.IO
import System.Monitor.Heartbeat (emitHeartbeatHandle, heartbeatThread)
import System.Process

-- | The path to the 'postgres-agent' executable.
newtype DcPgBinPath = DcPgBinPath FilePath
  deriving (Eq, Generic)

instance Hashable DcPgBinPath

data DcPgConfig = DcPgConfig
  { dcPgConfigEnvironmentVars :: [(String, String)]
  }
  deriving (Eq, Generic)

instance Hashable DcPgConfig

data DcPgAgentInstance = DcPgAgentInstance
  { dcPgServerHost :: Text,
    dcPgServerPort :: Int
  }

getDcPgAgentInstanceUrl :: DcPgAgentInstance -> Text
getDcPgAgentInstanceUrl (DcPgAgentInstance {dcPgServerHost, dcPgServerPort}) =
  "http://" <> dcPgServerHost <> ":" <> tshow dcPgServerPort <> "/"

emptyDcPgConfig :: DcPgConfig
emptyDcPgConfig = DcPgConfig []

data DcPgPoolArguments = DcPgPoolArguments
  { _paConfig :: DcPgConfig,
    _paBinPath :: DcPgBinPath
  }
  deriving (Eq, Generic)

instance Hashable DcPgPoolArguments

$(makeLenses ''DcPgPoolArguments)

instance Has DcPgConfig DcPgPoolArguments where
  hasLens = paConfig

instance Has DcPgBinPath DcPgPoolArguments where
  hasLens = paBinPath

data DcPgAgentHandle = DcPgAgentHandle
  { dpahInstance :: DcPgAgentInstance,
    dpahDestroy :: IO ()
  }

data DcPgPool = DcPgPool
  { dcPgPoolGet :: DcPgPoolArguments -> IO (Pool DcPgAgentHandle),
    dcPgPoolDestroy :: IO ()
  }

mkDcPgInstancePool :: Logger -> IO DcPgPool
mkDcPgInstancePool logger = do
  poolsMVar <- newMVar (mempty :: HashMap.HashMap DcPgPoolArguments (Pool DcPgAgentHandle))
  let dcPgPoolGet = \args -> do
        pools <- takeMVar poolsMVar
        case HashMap.lookup args pools of
          Nothing -> do
            pool <- createPool (serverHandle args) dpahDestroy 1 60.0 5
            putMVar poolsMVar (HashMap.insert args pool pools)
            return pool
          Just pool -> do
            putMVar poolsMVar pools
            return pool

      dcPgPoolDestroy = do
        pools <- takeMVar poolsMVar
        traverse_ destroyAllResources pools

  return DcPgPool {..}
  where
    serverHandle :: DcPgPoolArguments -> IO DcPgAgentHandle
    serverHandle args = do
      (dcPg, cleanup) <- spawnAgent (logger, args) (_paConfig args)
      return $ DcPgAgentHandle dcPg cleanup

withPool :: DcPgPool -> DcPgPoolArguments -> (DcPgAgentInstance -> IO a) -> IO a
withPool mkPool args k = do
  pool <- dcPgPoolGet mkPool args
  withResource pool (k . dpahInstance)

drawFromPool ::
  ( Has DcPgBinPath testEnvironment,
    Has DcPgPool testEnvironment
  ) =>
  testEnvironment ->
  DcPgConfig ->
  Managed DcPgAgentInstance
drawFromPool env config =
  managed
    ( withPool
        (getter env)
        DcPgPoolArguments
          { _paConfig = config,
            _paBinPath = getter env
          }
    )

-- | spin up a Postgres-agent instance and check it is healthy
spawnAgent ::
  ( Has Logger testEnvironment,
    Has DcPgBinPath testEnvironment
  ) =>
  testEnvironment ->
  DcPgConfig ->
  IO (DcPgAgentInstance, IO ())
spawnAgent testEnv (DcPgConfig {dcPgConfigEnvironmentVars}) = do
  let (DcPgBinPath dcPgBinPath) = getter testEnv
      allEnv = dcPgConfigEnvironmentVars
      logger = getter @Logger testEnv
  port <- bracket (Warp.openFreePort) (Socket.close . snd) (pure . fst)
  testLogMessage testEnv $ DcPgInstanceStartMessage port

  process@(Just dcPgStdIn, Just dcPgStdOut, Just dcPgStdErr, _) <-
    createProcess
      ( proc
          dcPgBinPath
          [ "+Heartbeat",
            "--enable-monitoring",
            "-Heartbeat",
            "--port",
            show port,
            "--use-colors",
            show False
          ]
      )
        { std_in = CreatePipe,
          std_out = CreatePipe,
          std_err = CreatePipe,
          env = Just allEnv,
          create_group = True
        }
      `catchAny` ( \exn ->
                     error $
                       unlines
                         [ "Failed to spawn postgres-agent process:",
                           show exn
                         ]
                 )

  cleanLogThread1 <- dcPgLogRelayThread logger dcPgStdOut
  cleanLogThread2 <- dcPgStdErrRelayThread logger dcPgStdErr
  cleanHeartbeatThread <- heartbeatThread (emitHeartbeatHandle dcPgStdIn) 10

  -- We do cleanup asynchronously, because we have observed that waiting for the
  -- spawned process to finish can take an inordinate amount of time.
  --
  -- This does risk leaking processes, especially when doing the final cleanup.
  -- The only real solution to this is to have the process listen for heartbeats
  -- which we send, and the process stopping when the heartbeats do.
  let cleanup = void $ do
        cleanHeartbeatThread
        cleanLogThread1
        cleanLogThread2
        void $ forkIO $ cleanupProcess process
        testLogMessage testEnv $ DcPgInstanceShutdownMessage port

  let server = DcPgAgentInstance "127.0.0.1" port
  result <- Http.healthCheck' (T.unpack $ getDcPgAgentInstanceUrl server <> "health")
  case result of
    Http.Healthy -> pure (server, cleanup)
    Http.Unhealthy failures -> do
      runLogger logger $ DcPgInstanceFailedHealthcheckMessage failures
      cleanup
      error "postgres-agent failed http healthcheck."

-- | Log message type used to indicate a postgres-agent server instance has started.
data DcPgInstanceStartMessage = DcPgInstanceStartMessage {hiStartPort :: Int}

instance LoggableMessage DcPgInstanceStartMessage where
  fromLoggableMessage DcPgInstanceStartMessage {..} =
    J.object
      [ ("type", J.String "DcPgInstanceStartMessage"),
        ("port", J.Number (fromIntegral hiStartPort))
      ]

-- | Log message type used to indicate a postgres-agent instance failed the
-- healthcheck.
data DcPgInstanceFailedHealthcheckMessage = DcPgInstanceFailedHealthcheckMessage
  {hisfFailures :: [Http.HttpException]}

instance LoggableMessage DcPgInstanceFailedHealthcheckMessage where
  fromLoggableMessage DcPgInstanceFailedHealthcheckMessage {..} =
    J.object
      [ ("type", J.String "DcPgInstanceFailedHealthcheckMessage"),
        ("failures", J.Array (fromList (map (J.String . tshow) hisfFailures)))
      ]

-- | Log message type used to indicate a postgres-agent server instance has shutdown.
data DcPgInstanceShutdownMessage = DcPgInstanceShutdownMessage
  { hiShutdownPort :: Int
  }

instance LoggableMessage DcPgInstanceShutdownMessage where
  fromLoggableMessage DcPgInstanceShutdownMessage {..} =
    J.object
      [ ("type", J.String "DcPgInstanceShutdownMessage"),
        ("port", J.Number (fromIntegral hiShutdownPort))
      ]

-- | Log message type used to indicate a single line output by a postgres-agent
-- server instance (on StdOut).
data DcPgStdOutLogMessage = DcPgStdOutLogMessage {dcPgStdOutLogMessage :: Text}

instance LoggableMessage DcPgStdOutLogMessage where
  fromLoggableMessage DcPgStdOutLogMessage {..} =
    J.object
      [ ("type", J.String "DcPgStdOutLogMessage"),
        ("message", J.String dcPgStdOutLogMessage)
      ]

-- | Log message type used to indicate a single line output by a postgres-agent server
-- instance on StdErr.
data DcPgStdErrLogMessage = DcPgStdErrLogMessage {dcPgStdErrLogMessage :: Text}

instance LoggableMessage DcPgStdErrLogMessage where
  fromLoggableMessage DcPgStdErrLogMessage {..} =
    J.object
      [ ("type", J.String "DcPgStdErrLogMessage"),
        ("message", J.String dcPgStdErrLogMessage)
      ]

-- | A thread that reads from the engine's StdErr handle and makes one test-log
-- message per line.
dcPgStdErrRelayThread :: Logger -> Handle -> IO (IO ())
dcPgStdErrRelayThread logger dcPgOutput = do
  threadHandle <- Async.async $ forever $ do
    nextChunk <- BS.hGetLine dcPgOutput
    runLogger logger $ DcPgStdErrLogMessage (decodeUtf8 nextChunk)
  return (Async.cancel threadHandle)

-- | A thread that reads from the engine's StdOut handle and makes one test-log
-- message per line.
dcPgLogRelayThread :: Logger -> Handle -> IO (IO ())
dcPgLogRelayThread logger dcPgOutput = do
  threadHandle <- Async.async $ forever $ do
    nextChunk <- BS.hGetLine dcPgOutput
    runLogger logger $ DcPgStdOutLogMessage (decodeUtf8 nextChunk)
  return (Async.cancel threadHandle)
