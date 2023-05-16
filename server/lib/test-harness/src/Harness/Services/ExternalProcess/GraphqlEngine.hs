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
    HgePool,
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
import Data.Time (NominalDiffTime, diffUTCTime, getCurrentTime)
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
import System.Exit (ExitCode)
import System.IO
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

newtype HgePool = HgePool {getHgePool :: HgePoolArguments -> IO (Pool HgeServerHandle)}

mkHgeInstancePool :: Logger -> IO HgePool
mkHgeInstancePool logger = do
  poolsMVar <- newMVar (mempty :: HashMap.HashMap HgePoolArguments (Pool HgeServerHandle))
  return $ HgePool \args -> do
    pools <- takeMVar poolsMVar
    case HashMap.lookup args pools of
      Nothing -> do
        pool <- createPool (serverHandle args) hshDestroy 1 60.0 5
        putMVar poolsMVar (HashMap.insert args pool pools)
        return pool
      Just pool -> do
        putMVar poolsMVar pools
        return pool
  where
    serverHandle :: HgePoolArguments -> IO HgeServerHandle
    serverHandle args = do
      (hge, cleanup) <- unmanage (spawnServer (logger, args) (_hpaConfig args))
      return $ HgeServerHandle hge cleanup

unmanage :: Managed a -> IO (a, IO ())
unmanage act = do
  resMVar <- newEmptyMVar
  cleanupMVar <- newEmptyMVar
  _ <- forkIO $ runManaged $ do
    res <- act
    liftIO $ do
      putMVar resMVar res
      takeMVar cleanupMVar

  res <- takeMVar resMVar
  return (res, putMVar cleanupMVar ())

withPool ::
  Has Logger testEnvironment =>
  testEnvironment ->
  HgePool ->
  HgePoolArguments ->
  (HgeServerInstance -> IO a) ->
  IO a
withPool env mkPool args k = do
  pool <- getHgePool mkPool args
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
  Managed HgeServerInstance
spawnServer testEnv (HgeConfig {hgeConfigEnvironmentVars}) = do
  let (HgeBinPath hgeBinPath) = getter testEnv
      pgUrl = getter testEnv
      (PassthroughEnvVars envVars) = getter testEnv
  freshDb <- mkFreshPostgresDb testEnv
  let allEnv = hgeConfigEnvironmentVars <> envVars
      metadataDbUrl = mkFreshDbConnectionString pgUrl freshDb
  ((_, Just hgeStdOut, Just hgeStdErr, _), port) <-
    managed
      ( bracket
          ( do
              port <- bracket (Warp.openFreePort) (Socket.close . snd) (pure . fst)
              testLogMessage testEnv $ HgeInstanceStartMessage port

              process <-
                createProcess
                  ( proc
                      hgeBinPath
                      [ "serve",
                        "--enable-console",
                        "--server-port",
                        show port,
                        "--metadata-database-url",
                        T.unpack (getPostgresServerUrl metadataDbUrl)
                      ]
                  )
                    { env =
                        Just $
                          ("HASURA_GRAPHQL_GRACEFUL_SHUTDOWN_TIMEOUT", "0")
                            : ("HASURA_GRAPHQL_ADMIN_SECRET", T.unpack adminSecret)
                            : allEnv,
                      std_out = CreatePipe,
                      std_err = CreatePipe,
                      create_group = True
                    }
                  `catchAny` ( \exn ->
                                 error $
                                   unlines
                                     [ "Failed to spawn Graphql-Engine process:",
                                       show exn
                                     ]
                             )
              return $ (process, port)
          )
          ( \(process@(_, _, _, ph), port) -> forkIO $ do
              startTime <- getCurrentTime
              interruptProcessGroupOf ph
              exitCode <- waitForProcess ph
              cleanupProcess process
              endTime <- getCurrentTime
              testLogMessage testEnv $ HgeInstanceShutdownMessage port exitCode (diffUTCTime endTime startTime)
          )
      )
  let logger = getter @Logger testEnv
  hgeLogRelayThread logger hgeStdOut
  hgeStdErrRelayThread logger hgeStdErr
  liftIO do
    let server = HgeServerInstance "127.0.0.1" port adminSecret
    result <- Http.healthCheck' (T.unpack $ getHgeServerInstanceUrl server <> "/healthz")
    case result of
      Http.Healthy -> pure server
      Http.Unhealthy failures -> do
        runLogger logger $ HgeInstanceFailedHealthcheckMessage failures
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
  { hiShutdownPort :: Int,
    hiShutdownExitCode :: ExitCode,
    hiShutdownDuration :: NominalDiffTime
  }

instance LoggableMessage HgeInstanceShutdownMessage where
  fromLoggableMessage HgeInstanceShutdownMessage {..} =
    J.object
      [ ("type", J.String "HgeInstanceShutdownMessage"),
        ("port", J.Number (fromIntegral hiShutdownPort)),
        ("duration", J.Number (realToFrac hiShutdownDuration)),
        ("exit-code", J.String (tshow hiShutdownExitCode))
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
hgeStdErrRelayThread :: Logger -> Handle -> Managed ()
hgeStdErrRelayThread logger hgeOutput = do
  _ <-
    managed
      ( bracket
          ( Async.async $ forever $ do
              nextChunk <- BS.hGetLine hgeOutput
              runLogger logger $ HgeStdErrLogMessage (decodeUtf8 nextChunk)
          )
          Async.cancel
      )
  return ()

-- | A thread that reads from the engine's StdOut handle and makes one test-log
-- message per json-J.object, on a best-effort basis.
hgeLogRelayThread :: Logger -> Handle -> Managed ()
hgeLogRelayThread logger hgeOutput = do
  resultRef <- liftIO $ newIORef (Atto.parse logParser "")
  _ <-
    managed
      ( bracket
          ( Async.async $ forever $ do
              nextChunk <- (<> "\n") <$> BS.hGetLine hgeOutput
              processChunk resultRef nextChunk
          )
          ( \threadHandle -> do
              Async.cancel threadHandle
              processChunk resultRef ""
              processChunk resultRef ""
          )
      )
  return ()
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
