{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module houses low-level functions and types to help access and work
-- with Graphql Engine.
module Harness.Services.ExternalProcess.GraphqlEngine
  ( -- * Types
    HgeBinPath (..),
    HgeConfig (..),
    emptyHgeConfig,
    PassthroughEnvVars (..),

    -- * Direct spawning
    spawnServer,

    -- * Pooled access
    HgePool (hgePoolDestroy),
    mkHgeInstancePool,
    drawFromPool,
  )
where

import Control.Concurrent (forkIO)
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.MVar
import Control.Lens
import Control.Monad.Managed
import Data.Aeson qualified as J
import Data.Attoparsec.ByteString as Atto
import Data.ByteString qualified as BS
import Data.Has
import Data.HashMap.Strict qualified as HashMap
import Data.IORef
import Data.Pool
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Text.IO qualified as T
import Data.Vector (fromList)
import Harness.Exceptions
import Harness.Http qualified as Http
import Harness.Logging
import Harness.Services.Database.Postgres
import Harness.Services.GraphqlEngine.API
import Hasura.Prelude
import Network.HTTP.Simple qualified as Http
import Network.Socket qualified as Socket
import Network.Wai.Handler.Warp qualified as Warp
import System.IO
import System.Monitor.Heartbeat (emitHeartbeatHandle, heartbeatThread)
import System.Process

-- | The path to the 'graphql-engine' executable.
newtype HgeBinPath = HgeBinPath FilePath
  deriving (Eq, Generic)

instance Hashable HgeBinPath

data HgeConfig = HgeConfig
  { hgeConfigEnvironmentVars :: [(String, String)]
  }
  deriving (Eq, Generic)

instance Hashable HgeConfig

-- | When spawning new HGE instances from a binary, we may want to pass through
-- some environment variables (for database credentials, for instance).
newtype PassthroughEnvVars
  = PassthroughEnvVars [(String, String)]
  deriving (Eq, Generic)

instance Hashable PassthroughEnvVars

emptyHgeConfig :: HgeConfig
emptyHgeConfig = HgeConfig []

data HgePoolArguments = HgePoolArguments
  { _hpaConfig :: HgeConfig,
    _hpaPostgresServerUrl :: PostgresServerUrl,
    _hpaBinPath :: HgeBinPath,
    _hpaPassthroughEnvVars :: PassthroughEnvVars
  }
  deriving (Eq, Generic)

instance Hashable HgePoolArguments

$(makeLenses ''HgePoolArguments)

instance Has HgeConfig HgePoolArguments where
  hasLens = hpaConfig

instance Has PostgresServerUrl HgePoolArguments where
  hasLens = hpaPostgresServerUrl

instance Has HgeBinPath HgePoolArguments where
  hasLens = hpaBinPath

instance Has PassthroughEnvVars HgePoolArguments where
  hasLens = hpaPassthroughEnvVars

data HgeServerHandle = HgeServerHandle
  { hshInstance :: HgeServerInstance,
    hshDestroy :: IO ()
  }

data HgePool = HgePool
  { hgePoolGet :: HgePoolArguments -> IO (Pool HgeServerHandle),
    hgePoolDestroy :: IO ()
  }

mkHgeInstancePool :: Logger -> IO HgePool
mkHgeInstancePool logger = do
  poolsMVar <- newMVar (mempty :: HashMap.HashMap HgePoolArguments (Pool HgeServerHandle))
  let hgePoolGet = \args -> do
        pools <- takeMVar poolsMVar
        case HashMap.lookup args pools of
          Nothing -> do
            pool <- createPool (serverHandle args) hshDestroy 1 60.0 5
            putMVar poolsMVar (HashMap.insert args pool pools)
            return pool
          Just pool -> do
            putMVar poolsMVar pools
            return pool

      hgePoolDestroy = do
        pools <- takeMVar poolsMVar
        traverse_ destroyAllResources pools

  return $ HgePool {..}
  where
    serverHandle :: HgePoolArguments -> IO HgeServerHandle
    serverHandle args = do
      (hge, cleanup) <- spawnServer (logger, args) (_hpaConfig args)
      return $ HgeServerHandle hge cleanup

withPool ::
  (Has Logger testEnvironment) =>
  testEnvironment ->
  HgePool ->
  HgePoolArguments ->
  (HgeServerInstance -> IO a) ->
  IO a
withPool env mkPool args k = do
  pool <- hgePoolGet mkPool args
  withResource
    pool
    ( \h -> do
        metadata <- export_metadata (hshInstance h, env)
        k (hshInstance h)
          `finally` void (replace_metadata (hshInstance h, env) metadata)
    )

drawFromPool ::
  ( Has PostgresServerUrl testEnvironment,
    Has Logger testEnvironment,
    Has HgeBinPath testEnvironment,
    Has PassthroughEnvVars testEnvironment,
    Has HgePool testEnvironment
  ) =>
  testEnvironment ->
  HgeConfig ->
  Managed HgeServerInstance
drawFromPool env config =
  managed
    ( withPool
        env
        (getter env)
        HgePoolArguments
          { _hpaConfig = config,
            _hpaPostgresServerUrl = getter env,
            _hpaBinPath = getter env,
            _hpaPassthroughEnvVars = getter env
          }
    )

-- | spin up a Manager HGE instance and check it is healthy
spawnServer ::
  ( Has PostgresServerUrl testEnvironment,
    Has Logger testEnvironment,
    Has HgeBinPath testEnvironment,
    Has PassthroughEnvVars testEnvironment
  ) =>
  testEnvironment ->
  HgeConfig ->
  IO (HgeServerInstance, IO ())
spawnServer testEnv (HgeConfig {hgeConfigEnvironmentVars}) = do
  let (HgeBinPath hgeBinPath) = getter testEnv
      pgUrl = getter testEnv
      logger = getter @Logger testEnv
      (PassthroughEnvVars envVars) = getter testEnv
      allEnv = hgeConfigEnvironmentVars <> envVars
  (freshDb, cleanupDb) <- mkFreshPostgresDbIO testEnv
  let metadataDbUrl = mkFreshDbConnectionString pgUrl freshDb
  port <- bracket (Warp.openFreePort) (Socket.close . snd) (pure . fst)
  testLogMessage testEnv $ HgeInstanceStartMessage port

  process@(Just hgeStdIn, Just hgeStdOut, Just hgeStdErr, _) <-
    createProcess
      ( proc
          hgeBinPath
          [ "+Heartbeat",
            "--enable-monitoring",
            "-Heartbeat",
            "serve",
            "--enable-console",
            "--console-assets-dir",
            "frontend/dist/apps/server-assets-console-ce",
            "--server-port",
            show port,
            "--metadata-database-url",
            T.unpack (getPostgresServerUrl metadataDbUrl)
          ]
      )
        { env =
            Just
              $ ("HASURA_GRAPHQL_GRACEFUL_SHUTDOWN_TIMEOUT", "0")
              : ("HASURA_GRAPHQL_ADMIN_SECRET", T.unpack adminSecret)
              : allEnv,
          std_in = CreatePipe,
          std_out = CreatePipe,
          std_err = CreatePipe,
          create_group = True
        }
      `catchAny` ( \exn ->
                     error
                       $ unlines
                         [ "Failed to spawn Graphql-Engine process:",
                           show exn
                         ]
                 )

  cleanLogThread1 <- hgeLogRelayThread logger hgeStdOut
  cleanLogThread2 <- hgeStdErrRelayThread logger hgeStdErr
  cleanHeartbeatThread <- heartbeatThread (emitHeartbeatHandle hgeStdIn) 10

  -- We do cleanup asynchronously, because we have observed that waiting for the
  -- spawned process to finish can take an inordinate amount of time.
  --
  -- This does risk leaking processes, especially when doing the final cleanup.
  -- The only real solution to this is to have the process listen for heartbeats
  -- which we send, and the process stopping when the heartbeats do.
  let cleanup = void $ do
        cleanupDb
        cleanHeartbeatThread
        cleanLogThread1
        cleanLogThread2
        void $ forkIO $ cleanupProcess process
        testLogMessage testEnv $ HgeInstanceShutdownMessage port

  let server = HgeServerInstance "127.0.0.1" port adminSecret
  result <- Http.healthCheck' (T.unpack $ getHgeServerInstanceUrl server <> "/healthz")
  case result of
    Http.Healthy -> pure (server, cleanup)
    Http.Unhealthy failures -> do
      runLogger logger $ HgeInstanceFailedHealthcheckMessage failures
      cleanup
      error "Graphql-Engine failed http healthcheck."
  where
    adminSecret :: Text
    adminSecret = "top-secret"

-- | Log message type used to indicate a HGE server instance has started.
data HgeInstanceStartMessage = HgeInstanceStartMessage {hiStartPort :: Int}

instance LoggableMessage HgeInstanceStartMessage where
  fromLoggableMessage HgeInstanceStartMessage {..} =
    J.object
      [ ("type", J.String "HgeInstanceStartMessage"),
        ("port", J.Number (fromIntegral hiStartPort))
      ]

-- | Log message type used to indicate a HGE server instance failed the
-- healthcheck.
data HgeInstanceFailedHealthcheckMessage = HgeInstanceFailedHealthcheckMessage
  {hisfFailures :: [Http.HttpException]}

instance LoggableMessage HgeInstanceFailedHealthcheckMessage where
  fromLoggableMessage HgeInstanceFailedHealthcheckMessage {..} =
    J.object
      [ ("type", J.String "HgeInstanceFailedHealthcheckMessage"),
        ("failures", J.Array (fromList (map (J.String . tshow) hisfFailures)))
      ]

-- | Log message type used to indicate a HGE server instance has shutdown.
data HgeInstanceShutdownMessage = HgeInstanceShutdownMessage
  { hiShutdownPort :: Int
  }

instance LoggableMessage HgeInstanceShutdownMessage where
  fromLoggableMessage HgeInstanceShutdownMessage {..} =
    J.object
      [ ("type", J.String "HgeInstanceShutdownMessage"),
        ("port", J.Number (fromIntegral hiShutdownPort))
      ]

-- | Log message type used to indicate a single log-J.object output by a HGE
-- server instance (on StdOut).
data HgeLogMessage = HgeLogMessage {hgeLogMessage :: J.Value}

instance LoggableMessage HgeLogMessage where
  fromLoggableMessage HgeLogMessage {..} =
    J.object
      [ ("type", J.String "HgeLogMessage"),
        ("message", hgeLogMessage)
      ]

-- | Log message type used to indicate a chunk of log output text by a HGE
-- server instance which could not be parsed as a json J.object.
data HgeUnparsableLogMessage = HgeUnparsableLogMessage {hgeUnparsableLogMessage :: Text}

instance LoggableMessage HgeUnparsableLogMessage where
  fromLoggableMessage HgeUnparsableLogMessage {..} =
    J.object
      [ ("type", J.String "HgeUnparsableLogMessage"),
        ("message", J.String hgeUnparsableLogMessage)
      ]

-- | Log message type used to indicate a single line output by a HGE server
-- instance on StdErr.
data HgeStdErrLogMessage = HgeStdErrLogMessage {hgeStdErrLogMessage :: Text}

instance LoggableMessage HgeStdErrLogMessage where
  fromLoggableMessage HgeStdErrLogMessage {..} =
    J.object
      [ ("type", J.String "HgeStdErrLogMessage"),
        ("message", J.String hgeStdErrLogMessage)
      ]

-- | A thread that reads from the engine's StdErr handle and makes one test-log
-- message per line.
hgeStdErrRelayThread :: Logger -> Handle -> IO (IO ())
hgeStdErrRelayThread logger hgeOutput = do
  async <- Async.async $ forever $ do
    nextChunk <- decodeUtf8 <$> BS.hGetLine hgeOutput
    T.putStrLn nextChunk
    runLogger logger $ HgeStdErrLogMessage nextChunk
  return $ Async.cancel async

-- | A thread that reads from the engine's StdOut handle and makes one test-log
-- message per json-J.object, on a best-effort basis.
hgeLogRelayThread :: Logger -> Handle -> IO (IO ())
hgeLogRelayThread logger hgeOutput = do
  resultRef <- newIORef (Atto.parse logParser "")
  threadHandle <- Async.async $ forever $ do
    nextChunk <- (<> "\n") <$> BS.hGetLine hgeOutput
    processChunk resultRef nextChunk
  return
    ( do
        Async.cancel threadHandle
        processChunk resultRef ""
        processChunk resultRef ""
    )
  where
    processChunk :: IORef (Atto.Result J.Value) -> BS.ByteString -> IO ()
    processChunk ref nextChunk = do
      result <- readIORef ref
      result' <- processDone result
      case result' of
        Atto.Fail {} -> do
          runLogger logger $ HgeUnparsableLogMessage (tshow result)
          writeIORef ref (Atto.parse logParser "")
          processChunk ref nextChunk
        Atto.Partial k -> do
          result'' <- processDone (k nextChunk)
          writeIORef ref result''
        Atto.Done {} ->
          runLogger logger $ HgeUnparsableLogMessage "Impossible: Done{}-case in 'processChunk'"

    processDone :: Atto.Result J.Value -> IO (Atto.Result J.Value)
    processDone result =
      case result of
        Atto.Done rest parsed -> do
          runLogger logger $ HgeLogMessage parsed
          if BS.empty == rest
            then return $ Atto.parse logParser ""
            else processDone $ Atto.parse logParser rest
        _ -> return result

    logParser :: Atto.Parser J.Value
    logParser = J.json' <* (option () (void (string "\n")) <|> endOfInput)
